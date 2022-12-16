program kyoukai_framework;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { Utility And Non-Sense Units }
  Classes, fphttpapp, fpwebfile,
  Kyoukai.Core.Base64Util,
  Kyoukai.Core.CommonUtil,
  Kyoukai.Core.KTemplate,
  Kyoukai.Core.RengeMessages,
  Kyoukai.Core.STLUtil,
  { Standard Framework Units }
  Kyoukai.Base.CGIUtil,
  Kyoukai.Base.DefaultHTML,
  Kyoukai.Base.HTTPApplication,
  Kyoukai.Base.HTTPServer,
  Kyoukai.Base.Controller,
  Kyoukai.Base.WebRouter,
  Kyoukai.Base.WebView,
  Kyoukai.Base.CGIApplication
  { Kyoukai IDEIntf Units }
  ;

{$R *.res}

begin

end.

