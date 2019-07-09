program example;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufrmmain
  { you can add units after this };

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

