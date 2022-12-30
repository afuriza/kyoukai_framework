{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.HTTPApplication;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, custapp,
  Kyoukai.Base.HTTPServer,
  Kyoukai.Base.Controller,
  Kyoukai.Base.WebRouter;

type

  TKyoukaiApp = class(TCustomApplication)
  private
    fServer: TKyoukaiHTTPServer;
    fMimeTypesFile: string;
    procedure WriteControllerList(AControllerList: TControllerList);
    function ReadControllerList: TControllerList;
    procedure WriteRouter(ARouter: TKyoukaiRouteHandler);
    function ReadRouter: TKyoukaiRouteHandler;

    function ReadFileRouter: TKyFileRoutes;
    procedure WriteFileRouter(AFileRoute: TKyFileRoutes);
    function ReadIsActive: boolean;
    procedure WriteToActive(AState: boolean);
    function ReadPort: word;
    procedure WritePort(APort: word);
    function ReadThreaded: boolean;
    procedure WriteThreaded(IsThreaded: boolean);

    procedure WriteAutoRouting(AValue: boolean);
    function ReadAutoRouting: boolean;
  public
    property AutoRouting: boolean read ReadAutoRouting write WriteAutoRouting;
    property Threaded: boolean read ReadThreaded write WriteThreaded; experimental;
    property Port: word read ReadPort write WritePort;
    property ControllerList: TControllerList read ReadControllerList write WriteControllerList;
    property FileRouter: TKyFileRoutes read ReadFileRouter write WriteFileRouter;
    property Router: TKyoukaiRouteHandler read ReadRouter write WriteRouter;
    property Active: boolean read ReadIsActive write WriteToActive; deprecated;
    property MimeTypesFile: string read fMimeTypesFile write fMimeTypesFile;
    procedure Run;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  KyoukaiApp: TKyoukaiApp;

implementation

function TKyoukaiApp.ReadThreaded: boolean;
begin
  Result := fServer.Threaded;
end;

procedure TKyoukaiApp.WriteThreaded(IsThreaded: boolean);
begin
  fServer.Threaded := IsThreaded;
end;

procedure TKyoukaiApp.WriteControllerList(AControllerList: TControllerList);
begin
  fServer.ControllerList := AControllerList;
end;

function TKyoukaiApp.ReadControllerList: TControllerList;
begin
  Result := fServer.ControllerList;
end;

procedure TKyoukaiApp.WriteRouter(ARouter: TKyoukaiRouteHandler);
begin
  fServer.Router := ARouter;
end;

function TKyoukaiApp.ReadRouter: TKyoukaiRouteHandler;
begin
  Result := fServer.Router;
end;

function TKyoukaiApp.ReadFileRouter: TKyFileRoutes;
begin
  Result := fServer.FileRouter;
end;

procedure TKyoukaiApp.WriteFileRouter(AFileRoute: TKyFileRoutes);
begin
  fServer.FileRouter := AFileRoute;
end;

procedure TKyoukaiApp.WriteAutoRouting(AValue: boolean);
begin
  fServer.AutoRouting := AValue;
end;

function TKyoukaiApp.ReadAutoRouting: boolean;
begin
  Result := fServer.AutoRouting;
end;

procedure TKyoukaiApp.Run;
begin
  fServer.MimeTypesFile := fMimeTypesFile;
  WriteLn('Registered into port: ', KyoukaiApp.Port);
  fServer.Active := True;
end;

function TKyoukaiApp.ReadIsActive: boolean;
begin
  Result := fServer.Active;
end;

function TKyoukaiApp.ReadPort: word;
begin
  Result := fServer.Port;
end;

procedure TKyoukaiApp.WritePort(APort: word);
begin
  fServer.Port := APort;
end;

procedure TKyoukaiApp.WriteToActive(AState: boolean);
begin
  fServer.MimeTypesFile := fMimeTypesFile;
  WriteLn('Registered into port: ', KyoukaiApp.Port);
  fServer.Active := AState;
end;

constructor TKyoukaiApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fServer := TKyoukaiHTTPServer.Create(Self);
end;

destructor TKyoukaiApp.Destroy;
begin
  FreeAndNil(fServer);
  inherited Destroy;
end;

initialization
  KyoukaiApp := TKyoukaiApp.Create(nil);
  KyoukaiApp.ControllerList := Controllers;
  KyoukaiApp.FileRouter := FileRoute;

finalization
  FreeAndNil(KyoukaiApp);

end.

