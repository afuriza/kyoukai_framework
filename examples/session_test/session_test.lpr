program session_test;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Classes,
  kyoukai.standard.HTTPApplication,
  {You must place your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };


begin
  KyoukaiApp.Port := 80;
  KyoukaiApp.Active := True;
end.

