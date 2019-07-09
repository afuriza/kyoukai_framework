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
  {You must place your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };


begin
  KyoukaiApp.Run;
end.
