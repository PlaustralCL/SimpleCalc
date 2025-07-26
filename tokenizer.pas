unit Tokenizer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils;

type
  TTokenizer = class
    private
      FInputString: string;
      FTokenList: TStringList;
    public
      constructor Create(InputString: string);
      function ParseTokens: TStringList;
  end;




implementation

constructor TTokenizer.Create(InputString: string);
begin
  FInputString := InputString;
  FTokenList := TStringList.Create;
end;

function TTokenizer.ParseTokens: TStringList;
begin
  FTokenList := TStringList.Create;
  FTokenList.Add('Hello ');
  FTokenList.Add('World');
  ParseTokens := FTokenList;
end;





end.

