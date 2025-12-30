unit HelpFile;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure PrintHelp;


implementation
const
  { Define the resource type that will be used when creating the resourse stream.
    From https://forum.lazarus.freepascal.org/index.php/topic,41317.msg426767.html#msg426767
    This should be cross-platform. However, if not RT_RCDATA is not defined here,
    using LCLType might work but it will not be cross platform.}
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
