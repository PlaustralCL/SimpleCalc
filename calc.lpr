program calc;

uses Classes, SysUtils, Tokenizer,
ShuntingYardParser, Generics.Collections, MathOperations;

Type
  TStringQueue = specialize TQueue<string>;


var
  InputString, token, TestString, x: string;
  i, j: integer;
  TokenParser: TTokenizer;
  TokenList: TStringList;
  IsDone: boolean;
  ShuntingYard: TShuntingYardParser;
  PostFix: TStringQueue;


begin
  IsDone := False;

  TestString := '111 aBc - -2.2/-.3';

  repeat
    write('> ');
    Readln(InputString);
    TokenParser := TTokenizer.Create(InputString);
    TokenList := TokenParser.ParseTokens;
    for token in TokenList do
    begin
      if token = 'quit' then
      begin
         IsDone := True;
      end;
    end;
    if not IsDone then
       begin
         ShuntingYard := TShuntingYardParser.Create(TokenList);
         PostFix := ShuntingYard.ConvertToPostfix;
         write('PostFix: ');
         for x in PostFix do
         begin
           write(x, ' ');
         end;
       end;

    writeln('');

  until IsDone;


  //TokenParser := TTokenizer.Create(TestString);
  //TokenList := TokenParser.ParseTokens;
  //writeln('TestString: ', TestString);
  //for x in TokenList do
  //    writeln(x);
  //
  //
  //
  //
  //readln();


end.
