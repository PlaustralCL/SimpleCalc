unit Tui;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  HelpFile,
  Tokenizer,
  StringQueue,
  ShuntingYardParser,
  PostFixCalculator;

type

  { TTui }

  TTui = class
  private
    FInputString: string;
    FIsDone: boolean;
    FTokenList: TStringList;
    procedure PrintHeader;
    procedure GetInput;
    procedure PerformCalculation;
    procedure PrintResults;
    function ConvertToTokens: boolean;
    function CalculateResult: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

implementation

{ TTui }




procedure TTui.PrintHeader;
begin
  writeln('Simple Calculator');
  writeln('Type "quit" to exit or "help" for more information.');
end;

procedure TTui.GetInput;
var
  InputString: string;
  IsValidInput: boolean;
begin

  repeat
    IsValidInput := True;
    Write('> ');
    Readln(InputString);
    FInputString := Trim(InputString);
    FInputString := LowerCase(InputString);
    if FInputString = '' then
      isValidInput := False
    else
    if FInputString = 'help' then
    begin
      PrintHelp;
      isValidInput := False;
    end;

  until IsValidInput;

  if LowerCase(InputString) = 'quit' then
    FIsDone := True;
end;

procedure TTui.PerformCalculation;
begin

end;

procedure TTui.PrintResults;
begin

end;

function TTui.ConvertToTokens: boolean;
var
  TokenParser: TTokenizer;
begin
  Result := True;
  try
    TokenParser := TTokenizer.Create(FInputString, FTokenList);
    try
      TokenParser.ParseTokens;
    except
      on e: Exception do
      begin
        writeln('Error. ', e.Message);
        Result := False;
      end;
    end;
  finally
    FreeAndNil(TokenParser);
  end;
end;

function TTui.CalculateResult: string;
var
  PostFixExpression: TSTringQueue;
  ShuntingYard: TShuntingYardParser;
  Answer: double;
  Calculator: TPostFixCalculator;
begin
  Calculator := TPostFixCalculator.Create;
  PostFixExpression := TStringQueue.Create;
  ShuntingYard := TShuntingYardParser.Create(FTokenList, PostFixExpression);
  ShuntingYard.ConvertToPostfix;
  try
    try
      answer := Calculator.Calculate(PostFixExpression);
      Result := FloatToStr(answer);
    except
      on e: Exception do
      begin
        Result := 'Error. ' + e.Message;
      end;
    end;
  finally
    FreeAndNil(Calculator);
    FreeAndNil(PostFixExpression);
    FreeAndNil(ShuntingYard);
  end;

end;

procedure TTui.Run;
var
  IsValidTokens: boolean;
  Answer: string;
begin
  PrintHeader;
  repeat
    GetInput;
    if not FIsDone then
    begin
      IsValidTokens := ConvertToTokens;
      if IsValidTokens then
      begin
        Answer := CalculateResult;
        writeln(Answer);
      end;
    end;
  until FIsDone;
end;

constructor TTui.Create;
begin
  FInputString := '';
  FIsDone := False;
  FTokenList := TStringList.Create;
  inherited Create;
end;

destructor TTui.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FTokenList);
end;

end.
