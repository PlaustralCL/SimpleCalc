unit TestStringQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, StringQueue;

type

  { TestStringQueue }

  { TTestStringQueue }

  TTestStringQueue= class(TTestCase)
  published
    procedure TestInitializeQueue;
    procedure AddOneNode;
    procedure PeekWithOneNode;
    procedure DequeAfterOneNode;
    procedure MultipleEnqueues;
    procedure MultipleDequeues;
    procedure EnqueueAndDeque;
  end;

implementation



procedure TTestStringQueue.TestInitializeQueue;
var
  Queue: TStringQueue;
  ExpectedSize, ActualSize: integer;
  ExpectedEmpty, ActualEmpty: boolean;
begin
  Queue := TStringQueue.Create;
  ExpectedSize := 0;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := True;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);
end;

procedure TTestStringQueue.AddOneNode;
var
  Queue: TStringQueue;
  Expected, Actual: integer;
  ExpectedEmpty, ActualEmpty: boolean;
begin
  Queue := TStringQueue.Create;
  Queue.Enqueue('a');
  Expected := 1;
  Actual := Queue.Size;
  AssertEquals(Expected, Actual);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);
end;

procedure TTestStringQueue.PeekWithOneNode;
var
  Queue: TStringQueue;
  Expected: string;
  Actual: string;
begin
  Queue := TStringQueue.Create;
  Expected := 'a';
  Queue.Enqueue('a');
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);
end;

procedure TTestStringQueue.DequeAfterOneNode;
var
  Queue: TStringQueue;
  Expected, Actual: string;
  ExpectedSize, ActualSize: integer;
  ExpectedEmpty, ActualEmpty: boolean;
begin
  Queue := TStringQueue.Create;
  Expected := 'a';
  Queue.Enqueue('a');
  Actual := Queue.Dequeue;
  AssertEquals(Expected, Actual);

  ExpectedSize := 0;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := True;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);
end;

procedure TTestStringQueue.MultipleEnqueues;
var
  Queue: TStringQueue;
  Expected, Actual, token: string;
  ExpectedSize, ActualSize: integer;
  ExpectedEmpty, ActualEmpty: boolean;
  InputTokens: array[0..3] of string = ('a', 'b', 'c', 'd');
begin
  Queue := TStringQueue.Create;
  for token in InputTokens do
  begin
    Queue.Enqueue(token);
  end;

  ExpectedSize := 4;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Expected := 'a';
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);
end;

procedure TTestStringQueue.MultipleDequeues;
var
  Queue: TStringQueue;
  Expected, Actual, token: string;
  ExpectedSize, ActualSize: integer;
  ExpectedEmpty, ActualEmpty: boolean;
  InputTokens: array[0..3] of string = ('a', 'b', 'c', 'd');
begin
  Queue := TStringQueue.Create;
  for token in InputTokens do
  begin
    Queue.Enqueue(token);
  end;

  Expected := 'a';
  Actual := Queue.Dequeue;
  AssertEquals(Expected, Actual);

  Expected := 'b';
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);

  ExpectedSize := 3;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Expected := 'b';
  Actual := Queue.Dequeue;
  AssertEquals(Expected, Actual);

  Expected := 'c';
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);

  ExpectedSize := 2;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Expected := 'c';
  Actual := Queue.Dequeue;
  AssertEquals(Expected, Actual);

  Expected := 'd';
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);

  ExpectedSize := 1;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Expected := 'd';
  Actual := Queue.Dequeue;
  AssertEquals(Expected, Actual);

  try
    Queue.Peek;
    Fail('Should have raised in EIndexError');
  except
    on EIndexError do AssertEquals(1, 1);
  end;
  ExpectedSize := 0;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := True;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);
end;

procedure TTestStringQueue.EnqueueAndDeque;
var
  Queue: TStringQueue;
  Expected, Actual, token: string;
  ExpectedSize, ActualSize: integer;
  ExpectedEmpty, ActualEmpty: boolean;
  InputTokens: array[0..3] of string = ('a', 'b', 'c', 'd');
begin
  Queue := TStringQueue.Create;
  Queue.Enqueue('a');
  Queue.Enqueue('b');
  Queue.Dequeue;
  Queue.Dequeue;

  ExpectedSize := 0;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := True;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Queue.Enqueue('c');
  Queue.Enqueue('d');

  ExpectedSize := 2;
  ActualSize := Queue.Size;
  AssertEquals(ExpectedSize, ActualSize);

  ExpectedEmpty := False;
  ActualEmpty := Queue.IsEmpty;
  AssertEquals(ExpectedEmpty, ActualEmpty);

  Expected := 'c';
  Actual := Queue.Peek;
  AssertEquals(Expected, Actual);
end;





initialization

  RegisterTest(TTestStringQueue);
end.

