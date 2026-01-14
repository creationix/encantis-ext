// =============================================================================
// Encantis VS Code Extension
// Provides language support for .ents files via LSP
// =============================================================================

import * as path from 'path';
import { ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext): void {
  // Path to the server module
  const serverModule = context.asAbsolutePath(path.join('out', 'server', 'lsp.js'));

  // Server options - run the server as a separate Node process
  const serverOptions: ServerOptions = {
    run: {
      module: serverModule,
      transport: TransportKind.stdio,
    },
    debug: {
      module: serverModule,
      transport: TransportKind.stdio,
      options: { execArgv: ['--nolazy', '--inspect=6009'] },
    },
  };

  // Client options - register for .ents files
  const clientOptions: LanguageClientOptions = {
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
  client = new LanguageClient(
    'encantis',
    'Encantis Language Server',
    serverOptions,
    clientOptions
  );

  // Start the client (also starts the server)
  client.start();
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
