{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
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
  KyoukaiApp.Router := Routes;
  WriteLn('Registered into port: ', KyoukaiApp.Port);

finalization
  FreeAndNil(Routes);
  FreeAndNil(KyoukaiApp);

end.

