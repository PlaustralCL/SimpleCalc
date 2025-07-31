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
      function ConvertToPostfix: TStringList;

    private
      //FOperatorStack: TStack;
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
    '+', '-': OperatorPrecedence := 1;
    '*', '/': OperatorPrecedence := 2;
    '^': OperatorPrecedence := 2;
    else
      OperatorPrecedence := -1;
  end;
end;

function TShuntingYard.ConvertToPostfix: TStringList;
var
  token: string;
  PostFix: TStringList;
  x: string;
begin
  PostFix := TStringList.Create;
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
    begin
      while (FOperatorStack.Count > 0) and (FOperatorStack.Peek <> '(') do
      begin
        FOutputQueue.Enqueue(FOperatorStack.Pop);
      end;
    end
    else if (FOperatorStack.Count = 0) or
            (FOperatorStack.Peek = '(') or
            (OperatorPrecedence(token) >= OperatorPrecedence(FOperatorStack.Peek)) then
      FOperatorStack.Push(token)
    else
    begin
      while (FOperatorStack.Count > 0) and (OperatorPrecedence(token) < OperatorPrecedence(FOperatorStack.Peek)) do
      begin
        FOutputQueue.Enqueue(FOperatorStack.Pop);
      end;
      FOperatorStack.Push(token);
    end;
  end;

  while FOperatorStack.Count > 0 do
  begin
    if FOperatorStack.Peek <> '(' then
       FOutputQueue.Enqueue(FOperatorStack.Pop)
    else
      FOperatorStack.Pop;

  end;

  while FOutputQueue.Count > 0 do
  begin
    PostFix.Add(FOutputQueue.Dequeue);
  end;

  ConvertToPostfix := PostFix;

end;

end.

