unit ShuntingYardParser;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Generics.Collections, StringQueue;

Type
  TStringStack = specialize TStack<string>;
  //TStringQueue = specialize TQueue<string>;


  { TShuntingYardParser }

  TShuntingYardParser = class
    public
      constructor Create(TokenList: TStringList);
      destructor Destroy; override;
      function ConvertToPostfix: TStringQueue;


    private
      FTokenList: TStringList;
      FOperatorStack: TStringStack;
      FOutputQueue: TStringQueue;
      function IsOperator(token: string): boolean;
      function IsNumber(token: string): boolean;
      function IsValidToken(token: string): boolean;
      function OperatorPrecedence(token: string): integer;
      procedure ProcessLowPriorityOperator(const token: string);
      procedure ProcessRightParenthesis;
  end;

implementation


{ TShuntingYardParser }

constructor TShuntingYardParser.Create(TokenList: TStringList);
begin
  FOperatorStack := TStringStack.Create;
  FOutputQueue := TStringQueue.Create;
  FTokenList := TokenList;
end;

destructor TShuntingYardParser.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FOperatorStack);
  FreeAndNil(FOutputQueue);
  FreeAndNil(FTokenList);
end;

function TShuntingYardParser.IsOperator(token: string): boolean;
const
  Operators: array[1..5] of string = ('+', '-', '*', '/', '^');
var
  i: integer;
begin
  IsOperator := False;
  for i := low(Operators) to high(operators) do
    if Operators[i] = token then
       IsOperator := True;
end;

function TShuntingYardParser.IsNumber(token: string): boolean;
var
  f: double;
begin
  IsNumber := (TryStrToFloat(token, f)) or (token = '@');
end;

function TShuntingYardParser.IsValidToken(token: string): boolean;
begin
  Result := False;
  if (OperatorPrecedence(token) > 0) or
     IsNumber(token) or
     (token = '(') or
     (token = ')') then
    begin
      Result := True;
    end;
end;

function TShuntingYardParser.OperatorPrecedence(token: string): integer;
begin
  case token of
    '+', '-': OperatorPrecedence := 10;
    '*', '/', '\', '%': OperatorPrecedence := 20;
    '^': OperatorPrecedence := 30;
    else
      OperatorPrecedence := -1;
  end;
end;

procedure TShuntingYardParser.ProcessLowPriorityOperator(const token: string);
begin
  begin
    while (FOperatorStack.Count > 0) and (OperatorPrecedence(token) <=
      OperatorPrecedence(FOperatorStack.Peek)) do
    begin
      FOutputQueue.Enqueue(FOperatorStack.Pop);
    end;
    FOperatorStack.Push(token);
  end;
end;

procedure TShuntingYardParser.ProcessRightParenthesis;
begin
  while (FOperatorStack.Count > 0) and (FOperatorStack.Peek <> '(') do
  begin
    FOutputQueue.Enqueue(FOperatorStack.Pop);
  end;
  // Remove the left parenthesis
  if (FOperatorStack.Count > 0) and (FOperatorStack.Peek = '(') then
  begin
    FOperatorStack.Pop;
  end;
end;

function TShuntingYardParser.ConvertToPostfix: TStringQueue;
var
  token: string;
begin
  for token in FTokenList do
  begin
    // Source: https://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
    // TODO: Need to ensure it is a valid token (number or valid operator or parenthesis)

    // An empty string was being passed, which caused problems. This check
    // prevents an empty string from being processed.
    if token <> '' then
    begin
      if IsNumber(token) then
      begin
        FOutputQueue.Enqueue(token);
      end
      else if token = '(' then
      begin
        FOperatorStack.Push(token)
      end
      else if token = ')' then
      begin
        ProcessRightParenthesis;
      end
      else if (FOperatorStack.Count = 0) or
              (FOperatorStack.Peek = '(') or
              (OperatorPrecedence(token) > OperatorPrecedence(FOperatorStack.Peek)) then
      { If the incoming symbol is an operator and has either higher precedence than
      the operator on the top of the stack, or if the stack is empty, or if the top
      of the stack is "(" (a floor) -- push it on the stack.}
      begin
        FOperatorStack.Push(token)
      end
      else ProcessLowPriorityOperator(token);
    end;
  end;

  ProcessRightParenthesis;

  {Pop any remaining operators and add them to the output queue.
   No parentheses should remain.}
  while FOperatorStack.Count > 0 do
  begin
    FOutputQueue.Enqueue(FOperatorStack.Pop);
  end;

  ConvertToPostfix := FOutputQueue;
end;

end.

