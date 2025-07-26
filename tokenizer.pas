unit Tokenizer;

{$mode ObjFPC}{$H+}

interface
type
  TTokenizer = class
    private
      FTokenString: string;
    public
      constructor Create(InputString: string);
      function ToString: string;
  end;


implementation
uses
  Classes, SysUtils;

constructor TTokenizer.Create(InputString: string);
begin
  FTokenString := InputString;
end;

function TTokenizer.ToString: string;
begin
  ToString := FTokenSTring + 'zzz';
end;


end.

