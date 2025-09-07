unit help;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure PrintHelp;

implementation

procedure PrintHelp;
begin
  writeln('*********************************************************************')
  writeln('Simple Calculator')
  writeln()
  writeln('help - read this file')
  writeln('quit - exit the program')
  writeln()
  writeln('Math operations:')
  writeln('  @ The result of the previous calculation. Initially set to 0.')
  writeln('  + Addition')
  writeln('  - Subtraction')
  writeln('  / Real division. 10 / = 3.33333333333333')
  writeln('  * Multiplication')
  writeln('  ^ Exponentiation')
  writeln('  % Modulo. Returns the remainder after division. 10 % 3 = 1')
  writeln('  \ Integer division. 10 \ 3 = 3')
  writeln()
  writeln('Order of operations follows the normal order in math or Python:')
  writeln('  parentheses')
  writeln('  exponents')
  writeln('  multiplication, division, integer division, and modulo')
  writeln('  addition and subtraction')
  writeln()
  writeln('Where operators have the same precedence, they are evaluated from')
  writeln('left to right.')
  writeln('********************************************************************* ')
end;

end.

