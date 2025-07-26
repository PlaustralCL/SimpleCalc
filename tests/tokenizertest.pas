// fpc .\tokenizertest.pas -FuC:\coding\calc\units; .\tokenizertest.exe
program TokenizerTest;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Tokenizer;

var
  PassedTestCount, FailedTestCount: integer;


procedure AssertEqualsInt(expected, actual: integer; msg: string);
begin
  if expected = actual then
  begin
    inc(PassedTestCount);
  end
  else
  begin
    inc(FailedTestCount);
    writeln('Failed ', msg);
    writeln('    Expected: ', expected, ' but actual was: ', actual);
  end;
end;

procedure AssertEqualsString(expected, actual, msg: string);
begin
  if expected = actual then
  begin
    inc(PassedTestCount);
  end
  else
  begin
    inc(FailedTestCount);
    writeln('Failed ', msg);
    writeln('    Expected: ', expected, ' but actual was: ', actual);
  end;
end;

procedure TestBasicAdd;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('111');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('22');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('3');

  TestString := '111 + 22+3';
  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

procedure TestBasicSubtraction;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('111');
  ExpectedTokenList.Add('-');
  ExpectedTokenList.Add('22');
  ExpectedTokenList.Add('-');
  ExpectedTokenList.Add('3');

  TestString := '111 - 22-3';
  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

procedure TestAddNegativeNumberWithSpace;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  TestString := '111 + 22+ -3';

  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('111');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('22');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('-3');

  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

procedure TestAddNegativeNumberNoSpace;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  TestString := '111 + 22+-3';
  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('111');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('22');
  ExpectedTokenList.Add('+');
  ExpectedTokenList.Add('-3');

  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

procedure TestRealNum;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('.111');
  ExpectedTokenList.Add('*');
  ExpectedTokenList.Add('22');
  ExpectedTokenList.Add('/');
  ExpectedTokenList.Add('.3');

  TestString := '.111 * 22/.3';
  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

procedure TestRealNegNum;
var
  TestString, msg: string;
  TokenParser: TTokenizer;
  ActualTokenList: TStringList;
  ExpectedTokenList: TStringList;
  i: integer;
begin
  ExpectedTokenList := TStringList.Create;
  ExpectedTokenList.Add('-.111');
  ExpectedTokenList.Add('-');
  ExpectedTokenList.Add('-22');
  ExpectedTokenList.Add('/');
  ExpectedTokenList.Add('-.3');

  TestString := '-.111 - -22/-.3';
  TokenParser := TTokenizer.Create(TestString);
  ActualTokenList := TokenParser.ParseTokens;
  msg := 'Token count for ' + TestString;
  AssertEqualsInt(ExpectedTokenList.Count, ActualTokenList.Count, msg );

  for i := 0 to ExpectedTokenList.Count - 1 do
  begin
    msg := 'Element verification for ' + TestString;
    AssertEqualsString(ExpectedTokenList[i], ActualTokenList[i], msg);
  end;
end;

begin
  writeln('');
  writeln('Beginning tests...');

  TestBasicAdd;
  TestBasicSubtraction;
  TestAddNegativeNumberWithSpace;
  TestRealNum;
  TestRealNegNum;

  writeln('Tests complete');
  writeln('Number of failed tests: ', FailedTestCount);
  writeln('Number of passed tests: ', PassedTestCount);
end.

