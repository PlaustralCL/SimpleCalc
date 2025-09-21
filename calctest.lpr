program calctest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestShuntingYard, TestStringQueue;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

