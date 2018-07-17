program kyoukai_framework;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  { Utility And Non-Sense Units }
  Classes, fphttpapp, fpwebfile,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil,
  Kyoukai.Other.KTemplate,
  Kyoukai.Other.RengeMessages,
  Kyoukai.Other.STLUtil,
  { Standard Framework Units }
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Standard.HTTPApplication,
  Kyoukai.Standard.HTTPServer,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebView
  { Kyoukai IDEIntf Units }
  ;

{$R *.res}

begin

end.

