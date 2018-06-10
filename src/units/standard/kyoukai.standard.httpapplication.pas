unit Kyoukai.Standard.HTTPApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.HTTPServer,
  Kyoukai.Standard.WebRouter;

type
  TKyoukaiApp = class(TKyServer)

  end;

var
  KyoukaiApp: TKyoukaiApp;

implementation

initialization
  Routes := TKyRoutes.Create;
  KyoukaiApp := TKyoukaiApp.Create(nil);
  WriteLn('Registered into port: ', KyoukaiApp.Port);

finalization
  FreeAndNil(Routes);
  FreeAndNil(KyoukaiApp);

end.

