"use strict";
// =============================================================================
// Encantis Compiler
// Main entry point: exports parse(), check(), and compile()
// =============================================================================
Object.defineProperty(exports, "__esModule", { value: true });
exports.getLineAndColumn = exports.formatDiagnostic = exports.tokenize = void 0;
exports.parse = parse;
exports.check = check;
exports.analyze = analyze;
exports.compile = compile;
exports.compileWithSourceMap = compileWithSourceMap;
const checker_1 = require("./checker");
const lexer_1 = require("./lexer");
Object.defineProperty(exports, "formatDiagnostic", { enumerable: true, get: function () { return lexer_1.formatDiagnostic; } });
Object.defineProperty(exports, "getLineAndColumn", { enumerable: true, get: function () { return lexer_1.getLineAndColumn; } });
Object.defineProperty(exports, "tokenize", { enumerable: true, get: function () { return lexer_1.tokenize; } });
const parser_1 = require("./parser");
// -----------------------------------------------------------------------------
// Parse API
// -----------------------------------------------------------------------------
/**
 * Parse Encantis source code into an AST.
 * Returns the AST and any syntax errors encountered.
 * The parser is error-tolerant and will return a partial AST even with errors.
 */
function parse(src) {
    return (0, parser_1.parse)(src);
}
// -----------------------------------------------------------------------------
// Check API
// -----------------------------------------------------------------------------
/**
 * Perform semantic analysis on a parsed AST.
 * Checks for undefined variables, type mismatches, etc.
 * Returns the combined errors from parsing and checking.
 */
function check(parseResult, src) {
    return (0, checker_1.check)(parseResult, src);
}
/**
 * Parse and check source code in one step.
 * Convenience function that combines parse() and check().
 */
function analyze(src) {
    const parseResult = parse(src);
    return check(parseResult, src);
}
// -----------------------------------------------------------------------------
// Compile API (WAT generation - placeholder for now)
// -----------------------------------------------------------------------------
/**
 * Compile Encantis source code to WebAssembly Text Format (WAT).
 * Currently a placeholder - full codegen will be added later.
 */
function compile(src) {
    const result = compileWithSourceMap(src);
    return result.wat;
}
/**
 * Compile Encantis source code to WAT with source mapping.
 * Used by the WAT preview feature for click-to-navigate.
 */
function compileWithSourceMap(src) {
    const parseResult = parse(src);
    const checkResult = check(parseResult, src);
    if (checkResult.errors.some(e => e.severity === 'error')) {
        // Return error summary as WAT comment with no source map
        const errorLines = checkResult.errors
            .filter(e => e.severity === 'error')
            .map(e => `;; ERROR: ${(0, lexer_1.formatDiagnostic)(src, e).replace(/\n/g, '\n;; ')}`);
        return {
            wat: `;; Compilation failed\n${errorLines.join('\n')}`,
            sourceMap: [],
        };
    }
    return generateWatWithSourceMap(parseResult.ast, checkResult.symbols, src);
}
function emit(ctx, line) {
    ctx.output.push('  '.repeat(ctx.indent) + line);
}
function emitWithSpan(ctx, line, span) {
    const watLine = ctx.output.length;
    ctx.output.push('  '.repeat(ctx.indent) + line);
    if (span) {
        ctx.sourceMap.push({ watLine, sourceSpan: span });
    }
}
// -----------------------------------------------------------------------------
// Type Inference Helpers
// -----------------------------------------------------------------------------
/**
 * Infer the type of an expression using the symbol table.
 */
function inferExprType(ctx, expr) {
    switch (expr.kind) {
        case 'Identifier': {
            // Look up in current function scope first, then global
            const funcScope = ctx.currentFunc ? ctx.symbols.scopes.get(ctx.currentFunc) : undefined;
            let sym = funcScope?.symbols.get(expr.name);
            if (!sym)
                sym = ctx.symbols.global.symbols.get(expr.name);
            return sym?.type;
        }
        case 'NumberLiteral':
            if (expr.suffix) {
                return { kind: 'PrimitiveType', name: expr.suffix, span: expr.span };
            }
            if (expr.value.includes('.') || expr.value.includes('e') || expr.value.includes('E')) {
                return { kind: 'PrimitiveType', name: 'f64', span: expr.span };
            }
            return { kind: 'PrimitiveType', name: 'i32', span: expr.span };
        case 'BinaryExpr':
            // Result type matches left operand type
            return inferExprType(ctx, expr.left);
        case 'CallExpr': {
            if (expr.callee.kind === 'Identifier') {
                const sym = ctx.symbols.global.symbols.get(expr.callee.name);
                if (sym?.type?.kind === 'FunctionType') {
                    return sym.type.returns;
                }
            }
            return undefined;
        }
        case 'TupleExpr':
            return {
                kind: 'TupleType',
                elements: expr.elements.map(e => inferExprType(ctx, e)).filter((t) => t !== undefined),
                span: expr.span,
            };
        default:
            return undefined;
    }
}
/**
 * Get the WAT type prefix (i32, i64, f32, f64) for a given type.
 */
function getWatPrefix(type) {
    if (!type || type.kind !== 'PrimitiveType')
        return 'i32';
    switch (type.name) {
        case 'f64': return 'f64';
        case 'f32': return 'f32';
        case 'i64':
        case 'u64': return 'i64';
        default: return 'i32'; // i32, u32, i16, u16, i8, u8
    }
}
function generateGlobalInit(expr) {
    // For global initialization, we need a constant expression
    if (expr.kind === 'NumberLiteral') {
        return expr.value;
    }
    // TODO: handle other constant expressions
    return '0';
}
function generateWatWithSourceMap(module, symbols, src) {
    const ctx = {
        output: [],
        indent: 0,
        strings: new Map(),
        stringOffset: 0,
        src,
        sourceMap: [],
        globals: new Set(),
        symbols,
    };
    // Collect global names first
    for (const global of module.globals) {
        ctx.globals.add(global.name);
    }
    emit(ctx, '(module');
    ctx.indent++;
    // Generate imports
    for (const imp of module.imports) {
        if (imp.kind === 'ImportGroup') {
            for (const item of imp.items) {
                const localName = item.localName || item.exportName;
                const params = item.params.map(p => `(param ${typeToWat(p.type)})`).join(' ');
                const result = item.returnType ? `(result ${typeToWat(item.returnType)})` : '';
                emitWithSpan(ctx, `(func $${localName} (import "${imp.module}" "${item.exportName}") ${params} ${result})`, imp.span);
            }
        }
        else {
            const localName = imp.localName || imp.exportName;
            const params = imp.funcType.params.map(t => typeToWat(t)).join(' ');
            const paramsStr = params ? `(param ${params})` : '';
            const returns = imp.funcType.returns;
            const result = returns.kind !== 'TupleType' || (returns.kind === 'TupleType' && returns.elements.length > 0)
                ? `(result ${typeToWat(imp.funcType.returns)})`
                : '';
            emitWithSpan(ctx, `(func $${localName} (import "${imp.module}" "${imp.exportName}") ${paramsStr} ${result})`, imp.span);
        }
    }
    // Generate globals
    for (const global of module.globals) {
        const watType = global.type ? typeToWat(global.type) : 'i32';
        const mutability = global.mutable ? `(mut ${watType})` : watType;
        const initValue = generateGlobalInit(global.init);
        emit(ctx, `(global $${global.name} ${mutability} (${watType}.const ${initValue}))`);
    }
    // Generate exports
    for (const exp of module.exports) {
        if (exp.decl.kind === 'FuncDecl') {
            generateFunction(ctx, exp.decl, exp.exportName);
        }
        else if (exp.decl.kind === 'MemoryDecl') {
            emitWithSpan(ctx, `(memory (export "${exp.exportName}") ${exp.decl.pages})`, exp.decl.span);
        }
    }
    // Generate non-exported functions
    for (const func of module.functions) {
        if (func.name) {
            generateFunction(ctx, func);
        }
    }
    // Generate memory if not already exported
    const hasMemory = module.exports.some(e => e.decl.kind === 'MemoryDecl') || module.memories.length > 0;
    if (!hasMemory && ctx.strings.size > 0) {
        emit(ctx, '(memory 1)');
    }
    // Generate data section for strings
    if (ctx.strings.size > 0) {
        const sortedStrings = Array.from(ctx.strings.entries()).sort((a, b) => a[1] - b[1]);
        const allStrings = sortedStrings.map(([str]) => escapeWatString(str)).join('');
        emit(ctx, `(data (i32.const 0) "${allStrings}")`);
    }
    ctx.indent--;
    emit(ctx, ')');
    return {
        wat: ctx.output.join('\n'),
        sourceMap: ctx.sourceMap,
    };
}
function generateFunction(ctx, func, exportName) {
    ctx.currentFunc = func; // Set for type lookups in this function's scope
    const name = func.name || exportName || 'anonymous';
    const exportClause = exportName ? `(export "${exportName}")` : '';
    // Build params
    const params = func.params.map(p => `(param $${p.name} ${typeToWat(p.type)})`).join(' ');
    // Build results
    let results = '';
    if (func.returnType) {
        if ('params' in func.returnType) {
            // Named returns
            const namedReturn = func.returnType;
            results = `(result ${namedReturn.params.map(p => typeToWat(p.type)).join(' ')})`;
        }
        else {
            results = `(result ${typeToWat(func.returnType)})`;
        }
    }
    emitWithSpan(ctx, `(func $${name} ${exportClause} ${params} ${results}`.trim(), func.span);
    ctx.indent++;
    // Collect locals from body
    if (func.body?.kind === 'BlockBody') {
        for (const stmt of func.body.stmts) {
            if (stmt.kind === 'LocalDecl' && stmt.type) {
                emit(ctx, `(local $${stmt.name} ${typeToWat(stmt.type)})`);
            }
        }
    }
    // Named returns are also locals
    if (func.returnType && 'params' in func.returnType) {
        const namedReturn = func.returnType;
        for (const ret of namedReturn.params) {
            emit(ctx, `(local $${ret.name} ${typeToWat(ret.type)})`);
        }
    }
    // Generate body
    if (func.body) {
        generateFuncBody(ctx, func.body, func);
    }
    ctx.indent--;
    emit(ctx, ')');
    ctx.currentFunc = undefined; // Clear after function generation
}
function generateFuncBody(ctx, body, func) {
    if (body.kind === 'ArrowBody') {
        // Arrow body: just the expressions
        for (const expr of body.exprs) {
            generateExpr(ctx, expr);
        }
    }
    else {
        // Block body: statements
        for (const stmt of body.stmts) {
            generateStmt(ctx, stmt);
        }
        // Return named return values
        if (func.returnType && 'params' in func.returnType) {
            const namedReturn = func.returnType;
            for (const ret of namedReturn.params) {
                emit(ctx, `(local.get $${ret.name})`);
            }
        }
    }
}
function generateStmt(ctx, stmt) {
    switch (stmt.kind) {
        case 'LocalDecl':
            if (stmt.init) {
                generateExpr(ctx, stmt.init);
                emit(ctx, `(local.set $${stmt.name})`);
            }
            break;
        case 'Assignment':
            if (stmt.op) {
                // Compound assignment: target op= value  ->  target = target op value
                // Only works for single target
                const target = stmt.targets[0];
                const isGlobal = ctx.globals.has(target.name);
                const targetType = inferExprType(ctx, target);
                const prefix = getWatPrefix(targetType);
                const isFloat = prefix === 'f64' || prefix === 'f32';
                emit(ctx, `(${isGlobal ? 'global' : 'local'}.get $${target.name})`);
                generateExpr(ctx, stmt.value);
                // Map compound op to WAT instruction with correct type prefix
                const opMap = {
                    '+=': `${prefix}.add`,
                    '-=': `${prefix}.sub`,
                    '*=': `${prefix}.mul`,
                    '/=': isFloat ? `${prefix}.div` : `${prefix}.div_s`,
                    '%=': `${prefix}.rem_s`, // integers only
                    '&=': `${prefix}.and`, // integers only
                    '|=': `${prefix}.or`, // integers only
                    '^=': `${prefix}.xor`, // integers only
                    '<<=': `${prefix}.shl`, // integers only
                    '>>=': `${prefix}.shr_s`, // integers only
                    '<<<=': `${prefix}.rotl`, // integers only
                    '>>>=': `${prefix}.rotr`, // integers only
                };
                const watOp = opMap[stmt.op] || `${prefix}.add`;
                emit(ctx, `(${watOp})`);
                emit(ctx, `(${isGlobal ? 'global' : 'local'}.set $${target.name})`);
            }
            else {
                // Simple assignment
                generateExpr(ctx, stmt.value);
                // For multiple targets, WAT pops in reverse order
                for (let i = stmt.targets.length - 1; i >= 0; i--) {
                    const t = stmt.targets[i];
                    const isGlobal = ctx.globals.has(t.name);
                    emit(ctx, `(${isGlobal ? 'global' : 'local'}.set $${t.name})`);
                }
            }
            break;
        case 'ExprStmt':
            generateExpr(ctx, stmt.expr);
            // Drop result if not used
            emit(ctx, '(drop)');
            break;
        case 'ReturnStmt':
            if (stmt.value) {
                generateExpr(ctx, stmt.value);
                emit(ctx, '(return)');
            }
            break;
        // TODO: Implement other statements (if, while, for, etc.)
        default:
            emit(ctx, `;; TODO: ${stmt.kind}`);
    }
}
function generateExpr(ctx, expr) {
    switch (expr.kind) {
        case 'NumberLiteral': {
            const literalType = inferExprType(ctx, expr);
            const prefix = getWatPrefix(literalType);
            emit(ctx, `(${prefix}.const ${expr.value})`);
            break;
        }
        case 'StringLiteral': {
            // Register string and emit ptr/len
            const offset = registerString(ctx, expr.value);
            emit(ctx, `(i32.const ${offset})`);
            emit(ctx, `(i32.const ${expr.value.length})`);
            break;
        }
        case 'Identifier':
            if (ctx.globals.has(expr.name)) {
                emit(ctx, `(global.get $${expr.name})`);
            }
            else {
                emit(ctx, `(local.get $${expr.name})`);
            }
            break;
        case 'BinaryExpr': {
            const exprType = inferExprType(ctx, expr);
            generateExpr(ctx, expr.left);
            generateExpr(ctx, expr.right);
            emit(ctx, `(${getBinaryOp(expr.op, exprType)})`);
            break;
        }
        case 'CallExpr': {
            // Check if this is a builtin function (WASM instruction)
            const builtinOps = {
                sqrt: 'f64.sqrt',
                abs: 'f64.abs',
                ceil: 'f64.ceil',
                floor: 'f64.floor',
                trunc: 'f64.trunc',
                nearest: 'f64.nearest',
                min: 'f64.min',
                max: 'f64.max',
                copysign: 'f64.copysign',
            };
            for (const arg of expr.args) {
                generateExpr(ctx, arg);
            }
            if (expr.callee.kind === 'Identifier') {
                const builtinOp = builtinOps[expr.callee.name];
                if (builtinOp) {
                    emit(ctx, `(${builtinOp})`);
                }
                else {
                    emit(ctx, `(call $${expr.callee.name})`);
                }
            }
            break;
        }
        case 'TupleExpr':
            for (const elem of expr.elements) {
                generateExpr(ctx, elem);
            }
            break;
        default:
            emit(ctx, `;; TODO: ${expr.kind}`);
    }
}
function getBinaryOp(op, type) {
    const prefix = getWatPrefix(type);
    const isFloat = prefix === 'f64' || prefix === 'f32';
    // Float types don't have bitwise ops, rem, or rotate
    // Comparison ops use different suffixes for float vs int
    const opMap = {
        '+': `${prefix}.add`,
        '-': `${prefix}.sub`,
        '*': `${prefix}.mul`,
        '/': isFloat ? `${prefix}.div` : `${prefix}.div_s`,
        '%': `${prefix}.rem_s`, // integers only
        '&': `${prefix}.and`, // integers only
        '|': `${prefix}.or`, // integers only
        '^': `${prefix}.xor`, // integers only
        '<<': `${prefix}.shl`, // integers only
        '>>': `${prefix}.shr_s`, // integers only
        '<<<': `${prefix}.rotl`, // integers only
        '>>>': `${prefix}.rotr`, // integers only
        '<': isFloat ? `${prefix}.lt` : `${prefix}.lt_s`,
        '>': isFloat ? `${prefix}.gt` : `${prefix}.gt_s`,
        '<=': isFloat ? `${prefix}.le` : `${prefix}.le_s`,
        '>=': isFloat ? `${prefix}.ge` : `${prefix}.ge_s`,
        '==': `${prefix}.eq`,
        '!=': `${prefix}.ne`,
    };
    return opMap[op] || `${prefix}.${op}`;
}
function typeToWat(type) {
    switch (type.kind) {
        case 'PrimitiveType': {
            const name = type.name;
            if (name === 'f64')
                return 'f64';
            if (name === 'f32')
                return 'f32';
            if (name === 'i64' || name === 'u64')
                return 'i64';
            return 'i32';
        }
        case 'SliceType':
            return 'i32 i32'; // ptr, len
        case 'PointerType':
            return 'i32';
        case 'TupleType':
            return type.elements.map(typeToWat).join(' ');
        default:
            return 'i32';
    }
}
function registerString(ctx, str) {
    const existing = ctx.strings.get(str);
    if (existing !== undefined) {
        return existing;
    }
    const offset = ctx.stringOffset;
    ctx.strings.set(str, offset);
    ctx.stringOffset += str.length;
    return offset;
}
function escapeWatString(s) {
    return s
        .replace(/\\/g, '\\\\')
        .replace(/"/g, '\\"')
        .replace(/\n/g, '\\n')
        .replace(/\t/g, '\\t')
        .replace(/\r/g, '\\r')
        .replace(/\0/g, '\\00');
}
//# sourceMappingURL=compile.js.map