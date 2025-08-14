unit shuntingyard;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Generics.Collections;

Type
  TStringStack = specialize TStack<string>;
  TStringQueue = specialize TQueue<string>;


  { TShuntingYard }

  TShuntingYard = class
    public
      constructor Create(TokenList: TStringList);
      function ConvertToPostfix: TStringQueue;

    private
      FTokenList: TStringList;
      FOperatorStack: TStringStack;
      FOutputQueue: TStringQueue;
      function IsOperator(token: string): boolean;
      function IsNumber(token: string): boolean;
      function IsValidToken(token: string): boolean;
      function OperatorPrecedence(token: string): integer;
  end;

implementation


{ TShuntingYard }

constructor TShuntingYard.Create(TokenList: TStringList);
begin
  FOperatorStack := TStringStack.Create;
  FOutputQueue := TStringQueue.Create;
  FTokenList := TokenList;
end;

function TShuntingYard.IsOperator(token: string): boolean;
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

function TShuntingYard.IsNumber(token: string): boolean;
var
  f: double;
begin
  IsNumber := TryStrToFloat(token, f);
end;

function TShuntingYard.IsValidToken(token: string): boolean;
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

function TShuntingYard.OperatorPrecedence(token: string): integer;
begin
  case token of
    '+': OperatorPrecedence := 10;
    '-': OperatorPrecedence := 10;
    '*': OperatorPrecedence := 20;
    '/': OperatorPrecedence := 20;
    '^': OperatorPrecedence := 30;
    else
      OperatorPrecedence := -1;
  end;
end;

function TShuntingYard.ConvertToPostfix: TStringQueue;
var
  token: string;
begin
  for token in FTokenList do
  begin
    // Source: https://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/
    // TODO: Need to ensure it is a valid token (number or valid operator or parenthesis)
    if IsNumber(token) then
    begin
      FOutputQueue.Enqueue(token);
    end
    else if token = '(' then
      FOperatorStack.Push(token)
    else if token = ')' then
    { If the incoming symbol is a right parenthesis: discard the right parenthesis,
    pop stack symbols and add them to the output queue until there is a left parenthesis
    in the operator stack. Pop the left parenthesis and discard it.}
    begin
      while (FOperatorStack.Count > 0) and (FOperatorStack.Peek <> '(') do
      begin
        FOutputQueue.Enqueue(FOperatorStack.Pop);
      end;
      if (FOperatorStack.Count > 0) and (FOperatorStack.Peek = '(') then
      begin
        FOperatorStack.Pop;
      end;
    end
    else if (FOperatorStack.Count = 0) or
            (FOperatorStack.Peek = '(') or
            (OperatorPrecedence(token) > OperatorPrecedence(FOperatorStack.Peek)) then
    { If the incoming symbol is an operator and has either higher precedence than
    the operator on the top of the stack, or if the stack is empty, or if the top
    of the stack is "(" (a floor) -- push it on the stack.}
      FOperatorStack.Push(token)
    else
    { If the incoming symbol is an operator and has either lower precedence than
    the operator on the top of the stack, or has the same precedence as the operator
    on the top of the stack and is left associative -- continue to pop the stack
    until this is not true. Then, push the incoming operator.
    }
    begin
      while (FOperatorStack.Count > 0) and (OperatorPrecedence(token) <= OperatorPrecedence(FOperatorStack.Peek)) do
      begin
        FOutputQueue.Enqueue(FOperatorStack.Pop);
      end;
      FOperatorStack.Push(token);
    end;
  end;

  {Pop any remaining operators and add them to the output queue.
   No parentheses should remain.}
  while FOperatorStack.Count > 0 do
  begin
    FOutputQueue.Enqueue(FOperatorStack.Pop);
  end;

  ConvertToPostfix := FOutputQueue;
end;

end.

