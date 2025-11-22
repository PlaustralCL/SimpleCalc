program simplecalc;

uses
  Classes,
  SysUtils,
  ShuntingYardParser,
  Generics.Collections,
  PostFixCalculator,
  HelpFile,
  StringQueue,
  Tokenizer;

  //Type
  //  TStringQueue = specialize TQueue<string>;

var
  InputString, token: string;
  TokenParser: TTokenizer;
  TokenList: TStringList;
  IsDone, IsCalculation, HasValidTokens: boolean;
  ShuntingYard: TShuntingYardParser;
  PostFixExpression: TStringQueue;
  Calculator: TPostFixCalculator;
  answer: double;

{$R *.res}

begin // main program block
  IsDone := False;

  Calculator := TPostFixCalculator.Create;

  writeln('Simple Calculator');
  writeln('Type "quit" to exit or "help" for more information.');

  repeat
    HasValidTokens := True;
    IsCalculation := False;
    Write('> ');
    Readln(InputString);
    if Trim(InputString) = '' then continue;
    TokenParser := TTokenizer.Create(InputString);
  try
    TokenList := TokenParser.ParseTokens; // Freed as part of TokenParser
  except
    on e: Exception do
    begin
      writeln('Error. ', e.Message);
      HasValidTokens := False;
    end;
  end;

    if HasValidTokens then
    begin
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
        end
        else
        begin
          IsCalculation := True;
        end;
      end;
    end;

    if IsCalculation then
    begin
      PostFixExpression := TStringQueue.Create;
      ShuntingYard := TShuntingYardParser.Create(TokenList, PostFixExpression);
      ShuntingYard.ConvertToPostfix;
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
        FreeAndNil(PostFixExpression);
        FreeAndNil(ShuntingYard);
      end;
    end;
    FreeAndNil(TokenParser);

  until IsDone;

  FreeAndNil(Calculator);

end.
