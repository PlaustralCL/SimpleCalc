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
  end;

implementation




{ TShuntingYard }

constructor TShuntingYard.Create(TokenList: TStringList);
begin
  FOperatorStack := TStringStack.Create;
  FTokenList := TokenList;
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
    case token of
    '+', '-', '*', '/': FOperatorStack.Push(token);
    end;
  end;
  while FOperatorStack.Count > 0 do
  begin
    PostFix.Add(FOperatorStack.Pop);

  end;

  ConvertToPostfix := PostFix;
  FOperatorStack.Free;

end;

end.

