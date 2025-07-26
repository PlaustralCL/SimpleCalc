program calc;

uses Tokenizer;

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
  TokenList: TTokenizer;


begin
  CurrentState := NewTokenState;
  TokenCount := 0;
  token := '';
  TestString := '111 + 22+3';

  TokenList := TTokenizer.Create(TestString);
  writeln(TokenList.ToString);




  readln();


end.
