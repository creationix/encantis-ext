"use strict";
// =============================================================================
// Encantis VS Code Extension
// Provides language support for .ents files via LSP
// =============================================================================
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || (function () {
    var ownKeys = function(o) {
        ownKeys = Object.getOwnPropertyNames || function (o) {
            var ar = [];
            for (var k in o) if (Object.prototype.hasOwnProperty.call(o, k)) ar[ar.length] = k;
            return ar;
        };
        return ownKeys(o);
    };
    return function (mod) {
        if (mod && mod.__esModule) return mod;
        var result = {};
        if (mod != null) for (var k = ownKeys(mod), i = 0; i < k.length; i++) if (k[i] !== "default") __createBinding(result, mod, k[i]);
        __setModuleDefault(result, mod);
        return result;
    };
})();
Object.defineProperty(exports, "__esModule", { value: true });
exports.activate = activate;
exports.deactivate = deactivate;
const path = __importStar(require("path"));
const node_1 = require("vscode-languageclient/node");
let client;
function activate(context) {
    // Path to the server module
    const serverModule = context.asAbsolutePath(path.join('out', 'server', 'lsp.js'));
    // Server options - run the server as a separate Node process
    const serverOptions = {
        run: {
            module: serverModule,
            transport: node_1.TransportKind.stdio,
        },
        debug: {
            module: serverModule,
            transport: node_1.TransportKind.stdio,
            options: { execArgv: ['--nolazy', '--inspect=6009'] },
        },
    };
    // Client options - register for .ents files
    const clientOptions = {
        documentSelector: [
            { scheme: 'file', language: 'encantis' },
            { scheme: 'untitled', language: 'encantis' },
        ],
        synchronize: {
            // Notify the server about file changes to .ents files
            fileEvents: undefined, // We'll rely on document sync for now
        },
    };
    // Create and start the client
    client = new node_1.LanguageClient('encantis', 'Encantis Language Server', serverOptions, clientOptions);
    // Start the client (also starts the server)
    client.start();
}
function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
//# sourceMappingURL=extension.js.map