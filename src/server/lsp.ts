// =============================================================================
// Encantis Language Server
// Provides diagnostics, hover, and other LSP features for .ents files
// =============================================================================

import {
  createConnection,
  TextDocuments,
  Diagnostic as LSPDiagnostic,
  DiagnosticSeverity,
  ProposedFeatures,
  InitializeParams,
  InitializeResult,
  TextDocumentSyncKind,
  DidChangeConfigurationNotification,
  Range,
  TextDocumentPositionParams,
  Hover,
  MarkupKind,
  RenameParams,
  WorkspaceEdit,
  TextEdit,
  PrepareRenameParams,
  SemanticTokensParams,
  SemanticTokens,
  SemanticTokensBuilder,
  SemanticTokensLegend,
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';

import { check, getLineAndColumn, parse } from './compile';
import type { CheckResult, Diagnostic, Type, Symbol as EncantisSymbol, Span, FuncDecl, Module, Stmt, Expr } from './types';

// Cache analysis results per document for hover and rename support
const analysisCache = new Map<string, { text: string; result: CheckResult; ast: Module }>();

// -----------------------------------------------------------------------------
// Connection Setup
// -----------------------------------------------------------------------------

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

let hasConfigurationCapability = false;
let hasWorkspaceFolderCapability = false;

// -----------------------------------------------------------------------------
// Semantic Tokens Legend
// -----------------------------------------------------------------------------

// Token types - order matters, these are indices
const tokenTypes = [
  'parameter',    // 0 - function parameters
  'variable',     // 1 - local variables and globals (distinguished by modifier)
  'function',     // 2 - function names
];

// Token modifiers - can be combined as bitflags
const tokenModifiers = [
  'declaration',  // 0 (bit 0 = 1) - where the symbol is declared
  'static',       // 1 (bit 1 = 2) - global/static variables
  'modification', // 2 (bit 2 = 4) - where a variable is being modified
  'readonly',     // 3 (bit 3 = 8) - immutable variables
];

const semanticTokensLegend: SemanticTokensLegend = {
  tokenTypes,
  tokenModifiers,
};

// -----------------------------------------------------------------------------
// Initialization
// -----------------------------------------------------------------------------

connection.onInitialize((params: InitializeParams): InitializeResult => {
  const capabilities = params.capabilities;

  hasConfigurationCapability = !!(
    capabilities.workspace && !!capabilities.workspace.configuration
  );
  hasWorkspaceFolderCapability = !!(
    capabilities.workspace && !!capabilities.workspace.workspaceFolders
  );

  const result: InitializeResult = {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      // Enable hover
      hoverProvider: true,
      // Enable rename
      renameProvider: {
        prepareProvider: true,
      },
      // Enable semantic tokens for rich highlighting
      semanticTokensProvider: {
        legend: semanticTokensLegend,
        full: true,
      },
      // Future: Enable completions
      // completionProvider: {
      //   resolveProvider: true,
      //   triggerCharacters: ['.', ':'],
      // },
    },
  };

  if (hasWorkspaceFolderCapability) {
    result.capabilities.workspace = {
      workspaceFolders: {
        supported: true,
      },
    };
  }

  return result;
});

connection.onInitialized(() => {
  if (hasConfigurationCapability) {
    connection.client.register(DidChangeConfigurationNotification.type, undefined);
  }
});

// -----------------------------------------------------------------------------
// Document Change Handling
// -----------------------------------------------------------------------------

documents.onDidChangeContent(change => {
  validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
  const text = textDocument.getText();
  const diagnostics: LSPDiagnostic[] = [];

  // Run the Encantis parser and checker (use same parseResult so FuncDecl refs match)
  const parseResult = parse(text);
  const result = check(parseResult, text);

  // Cache the result for hover and rename support
  analysisCache.set(textDocument.uri, { text, result, ast: parseResult.ast });

  // Convert Encantis diagnostics to LSP diagnostics
  for (const diag of result.errors) {
    diagnostics.push(convertDiagnostic(text, diag));
  }

  // Send diagnostics to the client
  connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

// -----------------------------------------------------------------------------
// Diagnostic Conversion
// -----------------------------------------------------------------------------

function convertDiagnostic(src: string, diag: Diagnostic): LSPDiagnostic {
  const startPos = getLineAndColumn(src, diag.span.start);
  const endPos = getLineAndColumn(src, diag.span.end);

  let severity: DiagnosticSeverity;
  switch (diag.severity) {
    case 'error':
      severity = DiagnosticSeverity.Error;
      break;
    case 'warning':
      severity = DiagnosticSeverity.Warning;
      break;
    case 'info':
      severity = DiagnosticSeverity.Information;
      break;
    default:
      severity = DiagnosticSeverity.Error;
  }

  return {
    severity,
    range: {
      start: { line: startPos.line - 1, character: startPos.column - 1 },
      end: { line: endPos.line - 1, character: endPos.column - 1 },
    },
    message: diag.message,
    source: 'encantis',
  };
}

// -----------------------------------------------------------------------------
// Hover Provider
// -----------------------------------------------------------------------------

connection.onHover((params: TextDocumentPositionParams): Hover | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const text = document.getText();
  const offset = document.offsetAt(params.position);

  // Get the word at the position
  const word = getWordAtOffset(text, offset);
  if (!word) return null;

  // Check for builtins
  const builtinDocs: Record<string, string> = {
    sqrt: '```encantis\nfunc sqrt(x: f64) -> f64\n```\nReturns the square root of x. Maps to `f64.sqrt` WASM instruction.',
    abs: '```encantis\nfunc abs(x: f64) -> f64\n```\nReturns the absolute value of x. Maps to `f64.abs` WASM instruction.',
    ceil: '```encantis\nfunc ceil(x: f64) -> f64\n```\nRounds x up to the nearest integer. Maps to `f64.ceil` WASM instruction.',
    floor: '```encantis\nfunc floor(x: f64) -> f64\n```\nRounds x down to the nearest integer. Maps to `f64.floor` WASM instruction.',
    trunc: '```encantis\nfunc trunc(x: f64) -> f64\n```\nTruncates x toward zero. Maps to `f64.trunc` WASM instruction.',
    nearest: '```encantis\nfunc nearest(x: f64) -> f64\n```\nRounds x to the nearest integer. Maps to `f64.nearest` WASM instruction.',
    min: '```encantis\nfunc min(a: f64, b: f64) -> f64\n```\nReturns the minimum of a and b. Maps to `f64.min` WASM instruction.',
    max: '```encantis\nfunc max(a: f64, b: f64) -> f64\n```\nReturns the maximum of a and b. Maps to `f64.max` WASM instruction.',
    copysign: '```encantis\nfunc copysign(x: f64, y: f64) -> f64\n```\nReturns x with the sign of y. Maps to `f64.copysign` WASM instruction.',
  };

  // Check for keywords
  const keywordDocs: Record<string, string> = {
    func: 'Declares a function.\n\n```encantis\nfunc name(params) -> ReturnType\n  body\nend\n```',
    local: 'Declares a local variable.\n\n```encantis\nlocal x: i32 = 42\nlocal y = inferred_value\n```',
    import: 'Imports functions from an external module.\n\n```encantis\nimport "module" (\n  "name" func (params) -> Return\n)\n```',
    export: 'Exports a function or memory for external use.\n\n```encantis\nexport "name"\nfunc (params) -> Return => body\n```',
    if: 'Conditional statement.\n\n```encantis\nif condition then\n  body\nelse\n  body\nend\n```',
    while: 'Loop while condition is true.\n\n```encantis\nwhile condition do\n  body\nend\n```',
    for: 'Iterate over a range or collection.\n\n```encantis\nfor i in 10 do\n  body\nend\n```',
    return: 'Returns a value from a function.\n\n```encantis\nreturn value\nreturn when condition\n```',
    end: 'Ends a block (function, if, while, for, etc.).',
  };

  // Type docs
  const typeDocs: Record<string, string> = {
    i32: '32-bit signed integer',
    u32: '32-bit unsigned integer',
    i64: '64-bit signed integer',
    u64: '64-bit unsigned integer',
    f32: '32-bit floating point',
    f64: '64-bit floating point',
    u8: '8-bit unsigned integer (byte)',
    i8: '8-bit signed integer',
    u16: '16-bit unsigned integer',
    i16: '16-bit signed integer',
  };

  const doc = builtinDocs[word] || keywordDocs[word] || typeDocs[word];
  if (doc) {
    return {
      contents: {
        kind: MarkupKind.Markdown,
        value: doc,
      },
    };
  }

  // Look up identifier in symbol table
  const cached = analysisCache.get(params.textDocument.uri);
  if (cached) {
    const symbol = findSymbol(cached.result, word);
    if (symbol) {
      const typeStr = symbol.type ? typeToString(symbol.type) : 'unknown';
      const kindLabel = symbol.kind === 'param' ? 'parameter' : symbol.kind;
      return {
        contents: {
          kind: MarkupKind.Markdown,
          value: `\`\`\`encantis\n(${kindLabel}) ${word}: ${typeStr}\n\`\`\``,
        },
      };
    }
  }

  return null;
});

function findSymbol(result: CheckResult, name: string): EncantisSymbol | undefined {
  // Check global scope first
  const globalSym = result.symbols.global.symbols.get(name);
  if (globalSym) return globalSym;

  // Check all function scopes
  for (const [, scope] of result.symbols.scopes) {
    const sym = scope.symbols.get(name);
    if (sym) return sym;
  }

  return undefined;
}

function typeToString(type: Type): string {
  switch (type.kind) {
    case 'PrimitiveType':
      return type.name;
    case 'SliceType':
      return `[${typeToString(type.element)}]`;
    case 'PointerType':
      return `*${typeToString(type.target)}`;
    case 'TupleType':
      if (type.elements.length === 0) return '()';
      return `(${type.elements.map(typeToString).join(', ')})`;
    case 'FunctionType': {
      const params = type.params.map(typeToString).join(', ');
      const ret = typeToString(type.returns);
      return `(${params}) -> ${ret}`;
    }
    default:
      return '?';
  }
}

function getWordAtOffset(text: string, offset: number): string | null {
  // Find word boundaries
  let start = offset;
  let end = offset;

  while (start > 0 && isWordChar(text[start - 1])) {
    start--;
  }
  while (end < text.length && isWordChar(text[end])) {
    end++;
  }

  if (start === end) return null;
  return text.slice(start, end);
}

function isWordChar(ch: string): boolean {
  return /[a-zA-Z0-9_-]/.test(ch);
}

// -----------------------------------------------------------------------------
// Rename Provider
// -----------------------------------------------------------------------------

connection.onPrepareRename((params: PrepareRenameParams): Range | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const text = document.getText();
  const offset = document.offsetAt(params.position);

  // Get the word boundaries at the position
  const wordRange = getWordRangeAtOffset(text, offset);
  if (!wordRange) return null;

  const word = text.slice(wordRange.start, wordRange.end);

  // Check if it's a renameable symbol (not a keyword, builtin, or type)
  const keywords = new Set([
    'import', 'export', 'func', 'local', 'global', 'end',
    'if', 'then', 'elif', 'else', 'while', 'do', 'for', 'in',
    'loop', 'return', 'when', 'and', 'or', 'not', 'as',
    'memory', 'define', 'interface', 'type', 'break', 'br', 'mut',
  ]);

  const builtins = new Set([
    'sqrt', 'abs', 'ceil', 'floor', 'trunc', 'nearest', 'min', 'max', 'copysign',
  ]);

  const types = new Set([
    'i32', 'u32', 'i64', 'u64', 'f32', 'f64', 'u8', 'i8', 'u16', 'i16',
  ]);

  if (keywords.has(word) || builtins.has(word) || types.has(word)) {
    return null; // Cannot rename keywords, builtins, or types
  }

  // Check if this symbol exists in our analysis
  const cached = analysisCache.get(params.textDocument.uri);
  if (!cached) return null;

  const symbol = findSymbol(cached.result, word);
  if (!symbol) return null;

  // Return the range of the word
  const startPos = getLineAndColumn(text, wordRange.start);
  const endPos = getLineAndColumn(text, wordRange.end);

  return {
    start: { line: startPos.line - 1, character: startPos.column - 1 },
    end: { line: endPos.line - 1, character: endPos.column - 1 },
  };
});

connection.onRenameRequest((params: RenameParams): WorkspaceEdit | null => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return null;

  const text = document.getText();
  const offset = document.offsetAt(params.position);

  // Get the word at the position
  const word = getWordAtOffset(text, offset);
  if (!word) return null;

  // Find the symbol to determine its scope
  const cached = analysisCache.get(params.textDocument.uri);
  if (!cached) return null;

  // Find which function contains the cursor (if any)
  const containingFunc = findFunctionAtOffset(cached.ast, offset);

  // Find the symbol and its scope
  const symbolInfo = findSymbolWithScope(cached.result, word, containingFunc);
  if (!symbolInfo) return null;

  // Determine the rename boundaries based on symbol scope
  let scopeStart = 0;
  let scopeEnd = text.length;

  if (symbolInfo.funcSpan) {
    // Local/param symbol - only rename within this function
    scopeStart = symbolInfo.funcSpan.start;
    scopeEnd = symbolInfo.funcSpan.end;
  }
  // Global symbols rename everywhere (scopeStart/scopeEnd remain as full document)

  // Find all occurrences of this identifier within the scope
  const edits: TextEdit[] = [];
  const regex = new RegExp(`\\b${escapeRegex(word)}\\b`, 'g');
  const matches = text.matchAll(regex);

  for (const match of matches) {
    if (match.index === undefined) continue;
    const matchStart = match.index;
    const matchEnd = matchStart + word.length;

    // Skip matches outside the scope
    if (matchStart < scopeStart || matchEnd > scopeEnd) continue;

    // Convert to LSP positions
    const startPos = getLineAndColumn(text, matchStart);
    const endPos = getLineAndColumn(text, matchEnd);

    edits.push({
      range: {
        start: { line: startPos.line - 1, character: startPos.column - 1 },
        end: { line: endPos.line - 1, character: endPos.column - 1 },
      },
      newText: params.newName,
    });
  }

  if (edits.length === 0) return null;

  return {
    changes: {
      [params.textDocument.uri]: edits,
    },
  };
});

function getWordRangeAtOffset(text: string, offset: number): { start: number; end: number } | null {
  let start = offset;
  let end = offset;

  while (start > 0 && isWordChar(text[start - 1])) {
    start--;
  }
  while (end < text.length && isWordChar(text[end])) {
    end++;
  }

  if (start === end) return null;
  return { start, end };
}

function findSymbolWithScope(
  result: CheckResult,
  name: string,
  containingFunc: FuncDecl | undefined
): { symbol: EncantisSymbol; isGlobal: boolean; funcSpan?: Span } | undefined {
  // If we're inside a function, check that function's scope first
  if (containingFunc) {
    const funcScope = result.symbols.scopes.get(containingFunc);
    if (funcScope) {
      const sym = funcScope.symbols.get(name);
      if (sym) {
        return { symbol: sym, isGlobal: false, funcSpan: containingFunc.span };
      }
    }
  }

  // Check global scope
  const globalSym = result.symbols.global.symbols.get(name);
  if (globalSym) return { symbol: globalSym, isGlobal: true };

  // Check all function scopes (fallback for edge cases)
  for (const [func, scope] of result.symbols.scopes) {
    const sym = scope.symbols.get(name);
    if (sym) return { symbol: sym, isGlobal: false, funcSpan: func.span };
  }

  return undefined;
}

function findFunctionAtOffset(ast: Module, offset: number): FuncDecl | undefined {
  // Check exported functions
  for (const exp of ast.exports) {
    if (exp.decl.kind === 'FuncDecl') {
      if (offset >= exp.decl.span.start && offset <= exp.decl.span.end) {
        return exp.decl;
      }
    }
  }

  // Check non-exported functions
  for (const func of ast.functions) {
    if (offset >= func.span.start && offset <= func.span.end) {
      return func;
    }
  }

  return undefined;
}

function escapeRegex(str: string): string {
  return str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
}

// -----------------------------------------------------------------------------
// Semantic Tokens Provider
// -----------------------------------------------------------------------------

connection.languages.semanticTokens.on((params: SemanticTokensParams): SemanticTokens => {
  const document = documents.get(params.textDocument.uri);
  if (!document) return { data: [] };

  const cached = analysisCache.get(params.textDocument.uri);
  if (!cached) return { data: [] };

  const text = document.getText();
  const builder = new SemanticTokensBuilder();
  const ast = cached.ast;
  const symbols = cached.result.symbols;

  // Collect all tokens to emit, sorted by position
  const tokens: Array<{
    span: Span;
    type: number;
    modifiers: number;
  }> = [];

  // Helper to add a token
  const addToken = (span: Span, type: number, modifiers: number = 0) => {
    tokens.push({ span, type, modifiers });
  };

  // Process global declarations
  for (const global of ast.globals) {
    // Find the name span within the global declaration
    // Format: "global name = value" or "global name: type = value"
    const declText = text.slice(global.span.start, global.span.end);
    const match = declText.match(/^global\s+([a-zA-Z_][a-zA-Z0-9_-]*)/);
    if (match) {
      const nameStart = global.span.start + declText.indexOf(match[1]);
      const nameEnd = nameStart + match[1].length;
      addToken({ start: nameStart, end: nameEnd }, 1, 2 | 1); // variable + static + declaration
    }
  }

  // Process exports and their function declarations
  for (const exp of ast.exports) {
    if (exp.decl.kind === 'FuncDecl') {
      processFunction(exp.decl);
    }
  }

  // Process non-exported functions
  for (const func of ast.functions) {
    processFunction(func);
  }

  function processFunction(func: FuncDecl) {
    // Function name (if named)
    if (func.name) {
      const funcText = text.slice(func.span.start, func.span.end);
      const nameMatch = funcText.match(/^func\s+([a-zA-Z_][a-zA-Z0-9_-]*)/);
      if (nameMatch) {
        const nameStart = func.span.start + funcText.indexOf(nameMatch[1]);
        const nameEnd = nameStart + nameMatch[1].length;
        addToken({ start: nameStart, end: nameEnd }, 2, 1); // function + declaration
      }
    }

    // Parameters - use their spans from the AST
    for (const param of func.params) {
      if (param.span) {
        // The param span includes the type annotation, we need just the name
        const paramText = text.slice(param.span.start, param.span.end);
        const colonIdx = paramText.indexOf(':');
        if (colonIdx > 0) {
          const nameEnd = param.span.start + colonIdx;
          addToken({ start: param.span.start, end: nameEnd }, 0, 1); // parameter + declaration
        } else {
          // No type annotation, whole span is the name
          addToken(param.span, 0, 1); // parameter + declaration
        }
      }
    }

    // Named return values like -> (h64: u64)
    // If a named return has the same name as a parameter, tag it as parameter
    // Otherwise tag it as variable (it's like a local)
    if (func.returnType && 'params' in func.returnType) {
      const paramNames = new Set(func.params.map(p => p.name));
      const namedReturn = func.returnType as { params: Array<{ name: string; span?: Span }> };
      for (const ret of namedReturn.params) {
        if (ret.span) {
          // The span includes the type annotation, we need just the name
          const retText = text.slice(ret.span.start, ret.span.end);
          const colonIdx = retText.indexOf(':');
          if (colonIdx > 0) {
            const nameEnd = ret.span.start + colonIdx;
            // If same name as param, tag as parameter; otherwise as variable
            const tokenType = paramNames.has(ret.name) ? 0 : 1;
            addToken({ start: ret.span.start, end: nameEnd }, tokenType, 1); // + declaration
          }
        }
      }
    }

    // Get function's scope to identify locals vs params
    const funcScope = symbols.scopes.get(func);
    const localSymbols = funcScope?.symbols || new Map();

    // Process body - this is where identifiers live with accurate spans
    if (func.body) {
      if (func.body.kind === 'ArrowBody') {
        for (const expr of func.body.exprs) {
          processExpr(expr, localSymbols);
        }
      } else {
        for (const stmt of func.body.stmts) {
          processStmt(stmt, localSymbols);
        }
      }
    }
  }

  function processStmt(stmt: Stmt, localSymbols: Map<string, EncantisSymbol>) {
    switch (stmt.kind) {
      case 'LocalDecl': {
        // Tag the local variable name
        const localText = text.slice(stmt.span.start, stmt.span.end);
        const localMatch = localText.match(/^local\s+([a-zA-Z_][a-zA-Z0-9_-]*)/);
        if (localMatch) {
          const nameStart = stmt.span.start + localText.indexOf(localMatch[1]);
          const nameEnd = nameStart + localMatch[1].length;
          addToken({ start: nameStart, end: nameEnd }, 1, 1); // variable + declaration
        }
        if (stmt.init) {
          processExpr(stmt.init, localSymbols);
        }
        break;
      }

      case 'Assignment':
        // Assignment targets are Identifier nodes with spans
        for (const target of stmt.targets) {
          if (target.span) {
            const sym = localSymbols.get(target.name);
            const tokenType = sym?.kind === 'param' ? 0 : 1;
            addToken(target.span, tokenType, 4); // modification modifier (bit 2)
          }
        }
        processExpr(stmt.value, localSymbols);
        break;

      case 'ExprStmt':
        processExpr(stmt.expr, localSymbols);
        break;

      case 'ReturnStmt':
        if (stmt.value) {
          processExpr(stmt.value, localSymbols);
        }
        break;

      case 'IfStmt':
        processExpr(stmt.condition, localSymbols);
        for (const s of stmt.thenBody) {
          processStmt(s, localSymbols);
        }
        if (stmt.elseBody) {
          for (const s of stmt.elseBody) {
            processStmt(s, localSymbols);
          }
        }
        break;

      case 'WhileStmt':
        processExpr(stmt.condition, localSymbols);
        for (const s of stmt.body) {
          processStmt(s, localSymbols);
        }
        break;

      case 'LoopStmt':
        for (const s of stmt.body) {
          processStmt(s, localSymbols);
        }
        break;

      case 'ForStmt':
        processExpr(stmt.iterable, localSymbols);
        for (const s of stmt.body) {
          processStmt(s, localSymbols);
        }
        break;
    }
  }

  function processExpr(expr: Expr, localSymbols: Map<string, EncantisSymbol>) {
    switch (expr.kind) {
      case 'Identifier':
        if (expr.span) {
          const sym = localSymbols.get(expr.name);
          if (sym) {
            // Local variable or parameter
            const tokenType = sym.kind === 'param' ? 0 : 1;
            addToken(expr.span, tokenType, 0);
          } else {
            // Check global scope
            const globalSym = symbols.global.symbols.get(expr.name);
            if (globalSym) {
              if (globalSym.kind === 'function' || globalSym.kind === 'builtin') {
                addToken(expr.span, 2, 0); // function
              } else {
                // Global variable: use 'variable' type with 'static' modifier
                addToken(expr.span, 1, 2); // variable + static (bit 1)
              }
            }
          }
        }
        break;

      case 'BinaryExpr':
        processExpr(expr.left, localSymbols);
        processExpr(expr.right, localSymbols);
        break;

      case 'UnaryExpr':
        processExpr(expr.operand, localSymbols);
        break;

      case 'CallExpr':
        processExpr(expr.callee, localSymbols);
        for (const arg of expr.args) {
          processExpr(arg, localSymbols);
        }
        break;

      case 'MemberExpr':
        processExpr(expr.object, localSymbols);
        break;

      case 'IndexExpr':
        processExpr(expr.object, localSymbols);
        processExpr(expr.index, localSymbols);
        break;

      case 'TupleExpr':
        for (const elem of expr.elements) {
          processExpr(elem, localSymbols);
        }
        break;

      case 'CastExpr':
        processExpr(expr.expr, localSymbols);
        break;
    }
  }

  // Sort tokens by position (required by LSP)
  tokens.sort((a, b) => a.span.start - b.span.start);

  // Emit tokens using the builder
  for (const token of tokens) {
    const startPos = getLineAndColumn(text, token.span.start);
    const length = token.span.end - token.span.start;

    builder.push(
      startPos.line - 1,    // 0-based line
      startPos.column - 1,  // 0-based character
      length,
      token.type,
      token.modifiers
    );
  }

  return builder.build();
});

// -----------------------------------------------------------------------------
// Document Events
// -----------------------------------------------------------------------------

documents.onDidClose(e => {
  // Clear diagnostics when document is closed
  connection.sendDiagnostics({ uri: e.document.uri, diagnostics: [] });
});

// -----------------------------------------------------------------------------
// Start Server
// -----------------------------------------------------------------------------

documents.listen(connection);
connection.listen();

console.error('Encantis Language Server started');
