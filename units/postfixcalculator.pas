unit PostFixCalculator;

{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, Generics.Collections;

Type
  TStringStack = specialize TStack<string>;
  TStringQueue = specialize TQueue<string>;

type

  EUnknownOperatorError = class(Exception) end;

  { TFloatStack }

  TFloatStack = class
    public
      constructor Create;
      function Size: integer;
      function Pop: double;
      procedure Push(token: double);
      function Peek: double;
      function IsEmpty: boolean;

    private
      FCount: integer;
      FLength: integer;
      FStackArray: array of double;
      procedure GrowArray;
  end;

  { TPostFixCalculator }

  TPostFixCalculator = class
    public
      constructor Create;
      function Calculate(PostFixExpression: TStringQueue): double;

    private
      FOperandStack: TFloatStack;
      FAns: double;
      procedure Add;
      procedure Multiply;

  end;

implementation

{ TFloatStack }

constructor TFloatStack.Create;
begin
  FLength := 2;
  SetLength(FStackArray, FLength);
  FCount := 0;
end;

function TFloatStack.Size: integer;
begin
  Size := FCount;
end;

function TFloatStack.Pop: double;
begin
  Pop := FStackArray[FCount - 1];
  Dec(FCount);
end;

procedure TFloatStack.Push(token: double);
begin
  if FCount = FLength then GrowArray;
  FStackArray[FCount] := token;
  Inc(FCount);
end;

function TFloatStack.Peek: double;
begin
  Peek := FStackArray[FCount - 1];
end;

function TFloatStack.IsEmpty: boolean;
begin
  IsEmpty := FCount = 0;
end;

procedure TFloatStack.GrowArray;
begin
  FLength := 2 * FLength;
  SetLength(FStackArray, FLength);
end;

{ TPostFixCalculator }

constructor TPostFixCalculator.Create;
begin
  FOperandStack := TFloatStack.Create;
  FAns := 0;
end;

function TPostFixCalculator.Calculate(PostFixExpression: TStringQueue): double;
var
  token: string;
  NumberToken: double;
begin
  while PostFixExpression.Count > 0 do
  begin
    token := PostFixExpression.Dequeue;
    if TryStrToFloat(token, NumberToken) then
    begin
      FOperandStack.Push(NumberToken);
    end
    else
    begin
      case token of
      '+': Add;
      else
        raise EUnknownOperatorError.Create('Unknown operator: ' + token);
      end;
    end;
  end;
  FAns := FOperandStack.Pop;
  Calculate := FAns;
end;

procedure TPostFixCalculator.Add;
var
  right, left: double;
begin
  right := FOperandStack.Pop;
  left := FOperandStack.Pop;
  FOperandStack.Push(left + right);
end;

procedure TPostFixCalculator.Multiply;
begin

end;

end.

