program simple_server;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Classes,
  kyoukai.standard.HTTPApplication,
  {You must place your module units here or Kyoukai can't register anything!}
  main_controller,
  hello_controller
  { you can add units after this };


begin
  KyoukaiApp.Port := 30;
  KyoukaiApp.Threaded := True;
  KyoukaiApp.Active := True;
end.

