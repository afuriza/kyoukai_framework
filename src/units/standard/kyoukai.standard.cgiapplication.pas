unit Kyoukai.Standard.CGIApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.CGIUtil;

type

  TKyoukaiApp = TKyCustCGIHandler;

var
  KyoukaiApp: TKyoukaiApp;
implementation

initialization
  Routes := TKyRoutes.Create;
  KyoukaiApp := TKyoukaiApp.Create(nil);
  KyoukaiApp.Router := Routes;

finalization
  FreeAndNil(Routes);
  FreeAndNil(KyoukaiApp);

end.

