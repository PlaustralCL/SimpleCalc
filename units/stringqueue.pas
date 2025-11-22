unit StringQueue;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  PNode = ^TNode;

  TNode = record
    Data: string;
    Next: PNode;
  end;

  EIndexError = class(Exception)
  end;

  { TStringQueue }

  TStringQueue = class
  private
    FSize: integer;
    FHead: PNode;
    FTail: PNode;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(token: string);
    function Dequeue: string;
    function Peek: string;
    function IsEmpty: boolean;
    property Size: integer read FSize;
  end;

implementation

{ TStringQueue }

constructor TStringQueue.Create;
begin
  FSize := 0;
  New(FTail);
  New(FHead);
  FTail^.Data := '';
  FTail^.Next := nil;
  FHead^.Data := '';
  FHead^.Next := FTail;

end;

destructor TStringQueue.Destroy;
begin
  while FSize > 0 do
  begin
    Dequeue;
  end;
  Dispose(FHead);
  Dispose(FTail);
  inherited Destroy;
end;

procedure TStringQueue.Enqueue(token: string);
var
  NewNode: PNode;
begin
  if FSize = 0 then
    FTail^.Data := token
  else
  begin
    New(NewNode);
    NewNode^.Data := token;
    NewNode^.Next := nil;
    FTail^.Next := NewNode;
    FTail := NewNode;
  end;
  Inc(FSize);
end;

function TStringQueue.Dequeue: string;
var
  FirstNode: PNode;
begin
  if FSize = 0 then raise EIndexError.Create('Empty Queue');

  FirstNode := FHead^.Next;
  Result := FirstNode^.Data;
  if FSize = 1 then
  begin
    FTail^.Data := '';
    FTail^.Next := nil;
  end
  else
  begin
    FHead^.Next := FirstNode^.Next;
    Dispose(FirstNode);
  end;
  Dec(FSize);
end;

function TStringQueue.Peek: string;
var
  FirstNode: PNode;
begin
  if FSize = 0 then raise EIndexError.Create('Empty Queue');
  FirstNode := FHead^.Next;
  Result := FirstNode^.Data;
end;

function TStringQueue.IsEmpty: boolean;
begin
  if FSize = 0 then
    Result := True
  else
    Result := False;
end;


end.
