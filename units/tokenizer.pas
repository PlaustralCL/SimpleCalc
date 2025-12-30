unit Tokenizer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils;

type
  TokenizerState = (NewToken, IntegerToken, DashToken, RealNumToken,
    AlphaToken, AtToken);

  ETokenError = class(Exception)
  end;

  { TTokenizer }

  // TODO: Convert states to separate classes instead of procedures within
  // TTokenizer.
  TTokenizer = class
  private
    FInputString, FToken: string;
    FTokenList: TStringList;
    FCurrentState: TokenizerState;
    procedure ParseNewToken(ch: char);
    procedure ParseIntegerToken(ch: char);
    procedure ParseDashToken(ch: char);
    procedure ParseRealNumToken(ch: char);
    procedure ParseAlphaToken(ch: char);
    procedure ParseError;
    procedure ProcessWhitespace;
    procedure ParseAtToken(ch: char);
  public
    constructor Create(InputString: string; TokenList: TStringList);
    destructor Destroy; override;
    procedure ParseTokens;
  end;


implementation


constructor TTokenizer.Create(InputString: string; TokenList: TStringList);
begin
  FInputString := InputString;
  FTokenList := TokenList;
  FToken := '';
  FCurrentState := NewToken;

end;

destructor TTokenizer.Destroy;
begin
  //FreeAndNil(FTokenList);
  //inherited Destroy;
end;

procedure TTokenizer.ParseTokens;
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
      AlphaToken: ParseAlphaToken(ch);
      AtToken: ParseAtToken(ch);
      else
        FTokenList.Add('unknown token');
    end;
  end;
  FTokenList.Add(FToken);
end;


procedure TTokenizer.ParseError;
begin
  FTokenList.Add('Error');
  FTokenList.Add(FToken);
  FToken := '';
  FCurrentState := NewToken;
end;

procedure TTokenizer.ProcessWhitespace;
begin
  FTokenList.Add(FToken);
  FToken := '';
  FCurrentState := NewToken;
end;

procedure TTokenizer.ParseAtToken(ch: char);
begin
  case ch of
    '@', '0'..'9', 'A'..'Z', 'a'..'z', '.':
    begin
      raise ETokenError.Create('@ must be followed by white space or an operator');
    end;
    ' ':
    begin
      FToken := '';
      FCurrentState := NewToken;
    end;
    else
    begin
      FTokenList.Add(FToken);
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := NewToken;
    end;
  end;
end;

procedure TTokenizer.ParseNewToken(ch: char);
begin
  case ch of
    '0'..'9':
    begin
      FToken := FToken + ch;
      FCurrentState := IntegerToken;
    end;
    ' ': FToken := '';
    '.':
    begin
      FToken := FToken + ch;
      FCurrentState := RealNumToken;
    end;
    '-':
    begin
      FToken := FToken + ch;
      FCurrentState := DashToken;
    end;
    'A'..'Z', 'a'..'z':
    begin
      FCurrentState := AlphaToken;
      ParseAlphaToken(ch);
    end;
    '@':
    begin
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := AtToken;
      //raise ETokenError.Create('@ must be proceeded by whitespace or an operator');
    end
    else
    begin
      // This is where non-dash operators are handled
      FTokenList.Add(ch);
      FToken := '';
    end;
  end;
end;

procedure TTokenizer.ParseIntegerToken(ch: char);
begin
  case ch of
    '0'..'9': FToken := FToken + ch;
    ' ': ProcessWhitespace;
    '.':
    begin
      FToken := FToken + ch;
      FCurrentState := RealNumToken;
    end;
    'A'..'Z', 'a'..'z':
    begin
      FTokenList.Add(FToken);
      FToken := '';
      FCurrentState := AlphaToken;
      ParseAlphaToken(ch);
    end;
    '@':
    begin
      raise ETokenError.Create('@ must be proceeded by an operator');
    end
    else
    begin
      FTokenList.Add(FToken);
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := NewToken;
    end;
  end;
end;

{ ParseRealNumToken only occurs after a decimal point has been identified.}
procedure TTokenizer.ParseRealNumToken(ch: char);
begin
  case ch of
    '0'..'9': FToken := FToken + ch;
    ' ': ProcessWhitespace;
    'A'..'Z', 'a'..'z':
    begin
      FTokenList.Add(FToken);
      FToken := '';
      FCurrentState := AlphaToken;
      ParseAlphaToken(ch);
    end;
    '@':
    begin
      raise ETokenError.Create('@ must be proceeded by an operator');
    end;
    '.':
    begin
      raise ETokenError.Create('Too many decimal points');
    end
    else
    begin
      FTokenList.Add(FToken);
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := NewToken;
    end;
  end;
end;

procedure TTokenizer.ParseDashToken(ch: char);
begin
  case ch of
    '0'..'9':
    begin
      FToken := FToken + ch;
      FCurrentState := IntegerToken;
    end;
    ' ': ProcessWhitespace;
    '.':
    begin
      FToken := FToken + ch;
      FCurrentState := RealNumToken;
    end;
    '-':
    begin
      FTokenList.Add(FToken);
      FToken := ch;
      FCurrentState := IntegerToken;
    end;
    'A'..'Z', 'a'..'z':
    begin
      FTokenList.Add(FToken);
      FToken := '';
      FCurrentState := AlphaToken;
      ParseAlphaToken(ch);
    end;
    '@':
    begin
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := AtToken;
    end
    else
    begin
      FTokenList.Add(FToken);
      FTokenList.Add(ch);
      FToken := '';
      FCurrentState := NewToken;
    end;
  end;
end;

procedure TTokenizer.ParseAlphaToken(ch: char);
begin
  ch := LowerCase(ch);
  case ch of
    'a'..'z':
    begin
      FToken := FToken + ch;
      if FToken = 'pi' then
      begin
        FToken := FloatToStr(Pi);
        ProcessWhitespace;
      end
      else if FToken = 'exp' then
      begin
        FToken := FloatToStr(Exp(1));
        ProcessWhitespace;
      end
    end;
    '@':
    begin
      raise ETokenError.Create('@ must be proceeded by an operator');
    end
    else
    begin
      FTokenList.Add(FToken);
      FToken := '';
      FCurrentState := NewToken;
      ParseNewToken(ch);
    end;
  end;
end;




end.
