program simplecalc;

uses Classes, SysUtils, Tokenizer,
ShuntingYardParser, Generics.Collections, PostFixCalculator, HelpFile, StringQueue;

//Type
//  TStringQueue = specialize TQueue<string>;

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
    TokenList := TokenParser.ParseTokens; // Freed as part of TokenParser
    for token in TokenList do
    begin
      if token = 'quit' then
      begin
         IsDone := True;
         IsCalculation := False;
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
        try
          answer := Calculator.Calculate(PostFixExpression);
          writeln(FloatToStr(answer));
        except
          on e: Exception do
          begin
            writeln('Error. ', e.Message);
          end;
        end;
      finally
        FreeAndNil(ShuntingYard);
      end;
    end;
    FreeAndNil(TokenParser);

  until IsDone;

  FreeAndNil(Calculator);

end.
