program Test.Oplls.Helpers;
{
  unit tests for Oplls.Helpers unit
}
uses
  Oplls.Helpers, SysUtils, Classes;

var
  TestRequest: AnsiString;

begin

  TestRequest := '{"params":{"rootPath":null,"processId":166217,"clientInfo":{"version":"0.9.5","name":"Neovim"},"capabilities":{"workspace":{"symbol":{"hierarchicalWorkspaceSymbolSupport":true,"dynamicRegistration":false,"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]}},"workspaceEdit":{"resourceOperations":["rename","create","delete"]},"workspaceFolders":true,"didChangeWatchedFiles":{"dynamicRegistration":false,"relativePatternSupport":true},"semanticTokens":{"refreshSupport":true},"applyEdit":true,"configuration":true},"window":{"showMessage":{"messageActionItem":{"additionalPropertiesSupport":false}},"showDocument":{"support":true},"workDoneProgress":true},"textDocument":{"codeAction":{"codeActionLiteralSupport":{"codeActionKind":{"valueSet":["","quickfix","refactor","refactor.extract","refactor.inline","refactor.rewrite","source","source.organizeImports"]}},"dynamicRegistration":false,"isPreferredSupport":true,"dataSupport":true,"resolveSupport":{"properties":["edit"]}},"publishDiagnostics":{"relatedInformation":true,"tagSupport":{"valueSet":[1,2]}},"callHierarchy":{"dynamicRegistration":false},"implementation":{"linkSupport":true},"typeDefinition":{"linkSupport":true},"semanticTokens":{"requests":{"range":false,"full":{"delta":true}},"tokenTypes":["namespace","type","class","enum","interface","struct","typeParameter","parameter","variable","property","enumMember","event","function","method","macro","keyword","modifier","comment","string","number","regexp","operator","decorator"],"formats":["relative"],"overlappingTokenSupport":true,"multilineTokenSupport":false,"serverCancelSupport":false,"augmentsSyntaxTokens":true,"dynamicRegistration":false,"tokenModifiers":["declaration","definition","readonly","static","deprecated","abstract","async","modification","documentation","defaultLibrary"]},"signatureHelp":{"dynamicRegistration":false,"signatureInformation":{"activeParameterSupport":true,"parameterInformation":{"labelOffsetSupport":true},"documentationFormat":["markdown","plaintext"]}},"rename":{"dynamicRegistration":false,"prepareSupport":true},"documentSymbol":{"symbolKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26]},"dynamicRegistration":false,"hierarchicalDocumentSymbolSupport":true},"completion":{"completionItemKind":{"valueSet":[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]},"contextSupport":false,"dynamicRegistration":false,"completionItem":{"snippetSupport":false,"commitCharactersSupport":false,"preselectSupport":false,"deprecatedSupport":false,"documentationFormat":["markdown","plaintext"]}},"synchronization":{"didSave":true,"willSaveWaitUntil":true,"dynamicRegistration":false,"willSave":true},"references":{"dynamicRegistration":false},"documentHighlight":{"dynamicRegistration":false},"declaration":{"linkSupport":true},"definition":{"linkSupport":true},"hover":{"dynamicRegistration":false,"contentFormat":["markdown","plaintext"]}}},"trace":"off","workspaceFolders":null,"rootUri":null},"id":1,"method":"initialize","jsonrpc":"2.0"}';

  WriteLn('*** UNIT TEST: Oplls.Helpers ***');
  WriteLn('********************************');
  WriteLn('*** START: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

  WriteLn('Testing GetConfigPath function..');
  WriteLn(GetConfigPath('config/'));
  WriteLn('Testing StripMessage function...');
  WriteLn(StripMessage('      ' + #13#10 + '  {some-json: value}'));
  WriteLn('Testing RequestHandler function...');
  WriteLn(RequestHandler(TestRequest));

  WriteLn('*** END: ' + FormatDateTime('YYYY-MM-DD_hh.mm.ss',Now));

end.