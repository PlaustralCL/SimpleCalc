unit Tokenizer;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils;

type
  TokenizerState = (NewToken, IntegerToken, DashToken, RealNumToken, LetterToken);

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
      procedure ParseError;
      procedure ProcessWhitespace;


    public
      constructor Create(InputString: string);
      function ParseTokens: TStringList;
  end;




implementation

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
  FTokenList.Add(FToken);
  ParseTokens := FTokenList;
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
    else
      begin
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
    else
      begin
        FTokenList.Add(FToken);
        FTokenList.Add(ch);
        FToken := '';
        FCurrentState := NewToken;
      end;
  end;
end;

procedure TTokenizer.ParseRealNumToken(ch: char);
begin
  case ch of
    '0'..'9': FToken := FToken + ch;
    ' ': ProcessWhitespace;
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
    else
      begin
        FTokenList.Add(FToken);
        FTokenList.Add(ch);
        FToken := '';
        FCurrentState := NewToken;
      end;
  end;
end;









end.

