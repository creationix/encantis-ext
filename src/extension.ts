// =============================================================================
// Encantis VS Code Extension
// Provides language support for .ents files via LSP
// =============================================================================

import * as path from 'path';
import * as vscode from 'vscode';
import { ExtensionContext } from 'vscode';
import {
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from 'vscode-languageclient/node';
import { WatPreviewProvider, WAT_SCHEME } from './watPreviewProvider';

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

  // -------------------------------------------------------------------------
  // WAT Preview Feature
  // -------------------------------------------------------------------------

  const watProvider = new WatPreviewProvider();

  // Register content provider for 'encantis-wat' scheme
  const providerRegistration = vscode.workspace.registerTextDocumentContentProvider(
    WAT_SCHEME,
    watProvider
  );

  // Command: Open WAT Preview (in new tab)
  const openPreviewCommand = vscode.commands.registerCommand(
    'encantis.openWatPreview',
    () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor || editor.document.languageId !== 'encantis') {
        vscode.window.showErrorMessage('Open an Encantis (.ents) file first');
        return;
      }

      const watUri = WatPreviewProvider.encodeUri(editor.document.uri);
      vscode.workspace.openTextDocument(watUri).then(doc => {
        vscode.window.showTextDocument(doc, { preview: false });
      });
    }
  );

  // Command: Open WAT Preview to Side (split view)
  const openPreviewSideCommand = vscode.commands.registerCommand(
    'encantis.openWatPreviewSide',
    () => {
      const editor = vscode.window.activeTextEditor;
      if (!editor || editor.document.languageId !== 'encantis') {
        vscode.window.showErrorMessage('Open an Encantis (.ents) file first');
        return;
      }

      const watUri = WatPreviewProvider.encodeUri(editor.document.uri);
      vscode.workspace.openTextDocument(watUri).then(doc => {
        vscode.window.showTextDocument(doc, {
          viewColumn: vscode.ViewColumn.Beside,
          preview: false,
          preserveFocus: true,
        });
      });
    }
  );

  // Auto-refresh preview when source changes
  const changeSubscription = vscode.workspace.onDidChangeTextDocument(e => {
    if (e.document.languageId === 'encantis') {
      watProvider.refresh(e.document.uri);
    }
  });

  context.subscriptions.push(
    watProvider,
    providerRegistration,
    openPreviewCommand,
    openPreviewSideCommand,
    changeSubscription
  );
}

export function deactivate(): Thenable<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
