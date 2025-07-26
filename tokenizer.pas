unit Tokenizer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils;

type
  TokenizerState = (NewToken, IntegerToken, DashToken, RealNumToken);

  { TTokenizer }

  TTokenizer = class
    private
      FInputString, FToken: string;
      FTokenList: TStringList;
      FCurrentState: TokenizerState;
      procedure ParseNewToken(ch: char);
      procedure ParseIntegerToken(ch: char);
      procedure ParseDashToken(ch: char);
      procedure ParseRealNumToken(ch: char);

    public
      constructor Create(InputString: string);
      function ParseTokens: TStringList;
  end;




implementation

procedure TTokenizer.ParseNewToken(ch: char);
begin
  case ch of
    '0'..'9':
      begin
        FToken := FToken + ch;
        FCurrentState := IntegerToken;
      end;
    '.':
      begin
        FToken := FToken + ch;
        FCurrentState := RealNumToken
      end;
    '-':
      begin
        FToken := FToken + ch;
        FCurrentState := DashToken;
      end;
  end;
end;

procedure TTokenizer.ParseIntegerToken(ch: char);
begin

end;

procedure TTokenizer.ParseDashToken(ch: char);
begin

end;

procedure TTokenizer.ParseRealNumToken(ch: char);
begin

end;

constructor TTokenizer.Create(InputString: string);
begin
  FInputString := InputString;
  FTokenList := TStringList.Create;
  FToken := '';
  FCurrentState := NewToken;
end;

function TTokenizer.ParseTokens: TStringList;
var
  ch: char;
begin
  for ch in FInputString do
  begin
    case FCurrentState of
      NewToken: ParseNewToken(ch);
      IntegerToken: ParseIntegerToken(ch);
      DashToken: ParseDashToken(ch);
      RealNumToken: ParseRealNumToken(ch);
      else
        FTokenList.Add('unknown token');
    end;
  end;


  ParseTokens := FTokenList;
end;





end.

