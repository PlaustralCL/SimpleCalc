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
