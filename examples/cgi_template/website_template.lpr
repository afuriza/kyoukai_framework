program website_template;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  // New CGI Support
  kyoukai.standard.CGIApplication,
  Kyoukai.Standard.WebRouter,
  {You must placed your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };

{$R *.res}

begin
  KyoukaiApp.Run;
end.
