unit MathOperations;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function add(n1, n2: integer): integer;

implementation
function add(n1, n2: integer): integer;
begin
  add := n1 + n2;
end;



end.

