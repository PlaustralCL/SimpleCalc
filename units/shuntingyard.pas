unit shuntingyard;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, Generics.Collections;



Type
  TStringStack = specialize TStack<string>;


  { TShuntingYard }

  TShuntingYard = class
    private
      //FOperatorStack: TStack;
      FTokenList: TStringList;
      FOperatorStack: TStringStack;
    public
      constructor Create(TokenList: TStringList);
      function ConvertToPostfix: TStringList;
      function IsOperator(token: string): boolean;
  end;

implementation
const
  Operators: array[1..4] of string = ('+', '-', '*', '/');




{ TShuntingYard }

constructor TShuntingYard.Create(TokenList: TStringList);
begin
  FOperatorStack := TStringStack.Create;
  FTokenList := TokenList;
end;

function TShuntingYard.IsOperator(token: string): boolean;
var
  i: integer;
begin
  IsOperator := False;
  for i := low(Operators) to high(operators) do
    if Operators[i] = token then
       IsOperator := True;
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
    if IsOperator(token) then
       FOperatorStack.Push(token);
    //case token of
    //'+', '-', '*', '/': FOperatorStack.Push(token);
    //end;
  end;
  while FOperatorStack.Count > 0 do
  begin
    PostFix.Add(FOperatorStack.Pop);

  end;

  ConvertToPostfix := PostFix;
  FOperatorStack.Free;

end;

end.

