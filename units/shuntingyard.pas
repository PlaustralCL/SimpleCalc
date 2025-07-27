unit shuntingyard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

Type

  { TShuntingYard }

  TShuntingYard = class
    private
      FOperatorStack: TStack;
      FTokenList: TStringList;
    public
      constructor Create(TokenList: TStringList);
      function ConvertToPostfix: TStringList;
  end;

implementation




{ TShuntingYard }

constructor TShuntingYard.Create(TokenList: TStringList);
begin
  FOperatorStack := TStack.Create;
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
      '+', '-', '*', '/':  FOperatorStack.Push(TObject(token));
    end;
  end;
  while FOperatorStack.Count > 0 do
  begin
    PostFix.Add(String(FOperatorStack.pop));

  end;

  ConvertToPostfix := PostFix;
  FOperatorStack.Free;

end;

end.

