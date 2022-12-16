unit Kyoukai.Standard.CGIApplication;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.Controller,
  Kyoukai.Standard.CGIUtil;

type

  TKyoukaiApp = TKyoukaiCGIHandler;

var
  KyoukaiApp: TKyoukaiApp;
implementation

initialization
  KyoukaiApp := TKyoukaiApp.Create(nil);
  KyoukaiApp.ControllerList := Routes;
  KyoukaiApp.FileRouter := FileRoutes;

finalization

end.

