program simplecalc;

uses
  Classes,
  SysUtils,
  ShuntingYardParser,
  Generics.Collections,
  PostFixCalculator,
  HelpFile,
  StringQueue,
  Tokenizer, Tui;

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
    // Get input and convert it to tokens
    Write('> ');
    Readln(InputString);
    if Trim(InputString) = '' then continue;
    TokenList := TStringList.Create;
    TokenParser := TTokenizer.Create(InputString, TokenList);
    try
      TokenParser.ParseTokens;
    except
      on e: Exception do
      begin
        writeln('Error. ', e.Message);
        HasValidTokens := False;
      end;
    end;

    // Seach for special commands
    if HasValidTokens then
    begin
      for token in TokenList do
      begin
        if token = 'quit' then
        begin
          IsDone := True;
        end
        else if token = 'help' then
        begin
          PrintHelp;
        end
        else
        begin
          IsCalculation := True;
        end;
      end;
    end;

    // Perform calculations
    if IsCalculation then
    begin
      PostFixExpression := TStringQueue.Create;
      ShuntingYard := TShuntingYardParser.Create(TokenList, PostFixExpression);
      ShuntingYard.ConvertToPostfix;
      try
        // write answer
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
    FreeAndNil(TokenList);

  until IsDone;

  FreeAndNil(Calculator);

end.
