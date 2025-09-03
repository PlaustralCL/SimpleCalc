program simplecalc;

uses Classes, SysUtils, Tokenizer,
ShuntingYardParser, Generics.Collections, PostFixCalculator;

Type
  TStringQueue = specialize TQueue<string>;


var
  InputString, token: string;
  TokenParser: TTokenizer;
  TokenList: TStringList;
  IsDone: boolean;
  ShuntingYard: TShuntingYardParser;
  PostFixExpression: TStringQueue;
  Calculator: TPostFixCalculator;
  answer: double;


begin
  IsDone := False;
  Calculator := TPostFixCalculator.Create;

  writeln('Simple Calculator');
  writeln('Type "quit" to exit, "@" to use the result of the previous calculation.');

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
         PostFixExpression := ShuntingYard.ConvertToPostfix;
         try
           answer := Calculator.Calculate(PostFixExpression);
           writeln(FloatToStr(answer));
         except
           on e: Exception do writeln('Error. ', e.Message);
         end;

       end;

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
