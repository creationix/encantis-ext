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
  Position,
  Range,
  TextDocumentPositionParams,
  Hover,
  MarkupKind,
  RenameParams,
  WorkspaceEdit,
  TextEdit,
  PrepareRenameParams,
} from 'vscode-languageserver/node';

import { TextDocument } from 'vscode-languageserver-textdocument';

import { analyze, getLineAndColumn, parse } from './compile';
import type { CheckResult, Diagnostic, Type, Symbol as EncantisSymbol, Span, FuncDecl, Module } from './types';

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

  // Run the Encantis parser and analyzer
  const parseResult = parse(text);
  const result = analyze(text);

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
