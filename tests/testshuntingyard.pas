unit TestShuntingYard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ShuntingYardParser;

type

  { TTestShuntingYard }

  TTestShuntingYard= class(TTestCase)
  published
    procedure TestHookUp;
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure Test4;
    procedure Test5;
    procedure Test6;
  end;

implementation
{ Tests based on the examples at https://mathcenter.oxford.emory.edu/site/cs171/shuntingYardAlgorithm/}

procedure TTestShuntingYard.TestHookUp;
begin
  //Fail('Write your own test');
  AssertEquals(1, 1);
end;

procedure TTestShuntingYard.Test1;
var
  InputString: String = '1 * 2 + 3';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..4] of string = ('1', '2', '*', '3', '+');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;

  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;

procedure TTestShuntingYard.Test2;
var
  InputString: String = '1 + 2 * 3';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..4] of string = ('1', '2', '3', '*', '+');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;

  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;

procedure TTestShuntingYard.Test3;
var
  InputString: String = '1 * ( 2 + 3 )';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..4] of string = ('1', '2', '3', '+', '*');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;

  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;

procedure TTestShuntingYard.Test4;
var
  InputString: String = '1 - 2 + 3';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..4] of string = ('1', '2', '-', '3', '+');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;

  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;

procedure TTestShuntingYard.Test5;
var
  InputString: String = '1 * 2 ^ 3 + 4';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..6] of string = ('1', '2', '3', '^', '*', '4', '+');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;

  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;

procedure TTestShuntingYard.Test6;
var
  InputString: String = '1 * ( 2 + 3 * 4 ) + 5';
  Actual: TStringQueue;
  TokenList: TStringList;
  ShuntingYard: TShuntingYardParser;
  Expected: array[0..8] of string = ('1', '2', '3', '4', '*', '+', '*', '5', '+');
  Token: string;
begin
  TokenList := TStringList.Create;
  TokenList.Delimiter := ' ';
  TokenList.DelimitedText := InputString;
  ShuntingYard := TShuntingYardParser.Create(TokenList);
  Actual := ShuntingYard.ConvertToPostfix;


  AssertEquals(Length(Expected), Actual.Count);
  for Token in Expected do
  begin
    AssertEquals(Token, Actual.Dequeue);
  end;
  AssertEquals(0, Actual.Count);

  FreeAndNil(Actual);
  FreeAndNil(TokenList);
  FreeAndNil(ShuntingYard);
end;



initialization

  RegisterTest(TTestShuntingYard);
end.

