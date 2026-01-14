"use strict";
// =============================================================================
// Encantis Compiler Types
// Shared type definitions for lexer, parser, checker, and LSP
// =============================================================================
Object.defineProperty(exports, "__esModule", { value: true });
exports.spanUnion = spanUnion;
exports.spanFrom = spanFrom;
// -----------------------------------------------------------------------------
// Utility Functions
// -----------------------------------------------------------------------------
function spanUnion(a, b) {
    return {
        start: Math.min(a.start, b.start),
        end: Math.max(a.end, b.end),
    };
}
function spanFrom(start, end) {
    return {
        start: start.span.start,
        end: end.span.end,
    };
}
//# sourceMappingURL=types.js.map