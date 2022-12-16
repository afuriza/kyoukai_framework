unit Kyoukai.Base.CGIApplication;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Base.WebRouter,
  Kyoukai.Base.Controller,
  Kyoukai.Base.CGIUtil;

type

  TKyoukaiApp = TKyoukaiCGIHandler;

var
  KyoukaiApp: TKyoukaiApp;
implementation

initialization
  KyoukaiApp := TKyoukaiApp.Create(nil);
  KyoukaiApp.ControllerList := Controllers;
  KyoukaiApp.Router := Route;
  KyoukaiApp.FileRouter := FileRoute;

finalization

end.

