program calc;

uses Classes, SysUtils, Tokenizer;

const
  Digits = ['0'..'9'];
type
  TokenizerState = (NewTokenState, IntegerState, DashState, RealNumState);


var
  CurrentState: TokenizerState;
  //TokenList: array[1..80] of string;
  ch: char;
  TestString, token, x: string;
  i, j: integer;
  TokenCount: integer;
  TokenParser: TTokenizer;
  TokenList: TStringList;


begin
  CurrentState := NewTokenState;
  TokenCount := 0;
  token := '';
  TestString := '111 + 22+3';

  TokenParser := TTokenizer.Create(TestString);
  TokenList := TokenParser.ParseTokens;
  for x in TokenList do
      writeln(x);




  readln();


end.
