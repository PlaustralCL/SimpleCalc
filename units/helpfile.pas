unit HelpFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure PrintHelp;

implementation

const
  RT_RCDATA = PChar(10);

var
  ResStream: TResourceStream;
  TextContent: string;

procedure PrintHelp;
begin
  //writeln('*********************************************************************');
  //writeln('Simple Calculator');
  //writeln();
  //writeln('help - read this file');
  //writeln('quit - exit the program');
  //writeln();
  //writeln('Math operations:');
  //writeln('  @ The result of the previous calculation. Initially set to 0.');
  //writeln('  + Addition');
  //writeln('  - Subtraction');
  //writeln('  / Real division. 10 / = 3.33333333333333');
  //writeln('  * Multiplication');
  //writeln('  ^ Exponentiation');
  //writeln('  % Modulo. Returns the remainder after division. 10 % 3 = 1');
  //writeln('  \ Integer division. 10 \ 3 = 3');
  //writeln();
  //writeln('Order of operations follows the normal order in math or Python:');
  //writeln('  parentheses');
  //writeln('  exponents');
  //writeln('  multiplication, division, integer division, and modulo');
  //writeln('  addition and subtraction');
  //writeln();
  //writeln('Where operators have the same precedence, they are evaluated from');
  //writeln('left to right.');
  //writeln('Whitespace does not impact the calculation.');
  //writeln('Negative number are processed as expected. However, you cannot negate');
  //writeln('a negative number back to postive. For example, --3 or -(-3) will');
  //writeln('throw an exception. Instead, multiply it by negative one: -1 * -3 = 3.');
  //writeln('*********************************************************************');

  try
    ResStream := TResourceStream.Create(HInstance, 'HELPTEXT', RT_RCDATA);
    SetLength(TextContent, ResStream.Size);
    ResStream.Read(TextContent[1], ResStream.Size);
    Write(TextContent);
  finally
    ResStream.Free;
  end;
end;

end.
