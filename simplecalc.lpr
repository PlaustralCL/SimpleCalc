program simplecalc;

uses Classes, SysUtils, Tokenizer,
ShuntingYardParser, Generics.Collections, PostFixCalculator, HelpFile;

Type
  TStringQueue = specialize TQueue<string>;

var
  InputString, token: string;
  TokenParser: TTokenizer;
  TokenList: TStringList;
  IsDone, IsCalculation: boolean;
  ShuntingYard: TShuntingYardParser;
  PostFixExpression: TStringQueue;
  Calculator: TPostFixCalculator;
  answer: double;

begin // main program block
  IsDone := False;

  Calculator := TPostFixCalculator.Create;

  writeln('Simple Calculator');
  writeln('Type "quit" to exit or "help" for more information.');

  repeat
    IsCalculation := True;
    write('> ');
    Readln(InputString);
    if Trim(InputString) = '' then continue;
    TokenParser := TTokenizer.Create(InputString);
    TokenList := TokenParser.ParseTokens;
    for token in TokenList do
    begin
      if token = 'quit' then
      begin
         IsDone := True;
         IsCalculation := False;
         Break;
      end
      else if token = 'help' then
      begin
         PrintHelp;
         IsCalculation := False;
      end;
    end;
    if IsCalculation then
       begin
         ShuntingYard := TShuntingYardParser.Create(TokenList);
         PostFixExpression := ShuntingYard.ConvertToPostfix;

         try
           answer := Calculator.Calculate(PostFixExpression);
           writeln(FloatToStr(answer));
           FreeAndNil(ShuntingYard); // Also frees PostFixExpression
           FreeAndNil(TokenParser);
         except
           on e: Exception do writeln('Error. ', e.Message);
         end;
       end;

  until IsDone;

  // TokenList and TokenParser freed here if the program is exited with the
  // quit command.
  FreeAndNil(TokenList);
  FreeAndNil(TokenParser);
  FreeAndNil(Calculator);






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
