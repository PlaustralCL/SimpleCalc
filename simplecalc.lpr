program simplecalc;

uses
  Classes,
  SysUtils,
  ShuntingYardParser,
  Generics.Collections,
  PostFixCalculator,
  HelpFile,
  StringQueue,
  Tokenizer,
  Tui;

var
  TerminalInterface: TTui;

  {$R *.res}

begin // main program block
  TerminalInterface := TTui.Create;
  TerminalInterface.Run;
  FreeAndNil(TerminalInterface);
end.
