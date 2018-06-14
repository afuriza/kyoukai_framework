program kyoukai_framework;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fphttpapp, fpwebfile, Kyoukai.Standard.HTTPApplication;

{$R *.res}

begin

end.

