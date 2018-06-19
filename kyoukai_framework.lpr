program kyoukai_framework;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fphttpapp, fpwebfile,
  Kyoukai.Standard.HTTPApplication,
  Kyoukai.Standard.HTTPServer,
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebView,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil,
  Kyoukai.Other.KTemplate,
  Kyoukai.Other.RengeMessages,
  Kyoukai.Other.STLUtil;

{$R *.res}

begin

end.

