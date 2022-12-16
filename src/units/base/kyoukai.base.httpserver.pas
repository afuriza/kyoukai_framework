{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.HTTPServer;

{$mode objfpc}{$H+}

interface

uses
  cmem,
  Classes, SysUtils, fphttp, fphttpserver, httpdefs,
  fpmimetypes, fphttpclient,
  Kyoukai.Base.WebRouter,
  Kyoukai.Base.Controller,
  Kyoukai.Base.WebHandler;

type

  TKyoukaiHTTPServer = class(TFPHTTPServer)
  private
    fWebHandler: TKyoukaiHTTPHandler;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
     procedure WriteControllerList(AControllerList: TControllerList);
    function ReadControllerList: TControllerList;
    procedure WriteRouter(ARouter: TKyoukaiRouteHandler);
    function ReadRouter: TKyoukaiRouteHandler;

    procedure WriteFileRouter(ARoutes: TKyFileRoutes);
    function ReadFileRouter: TKyFileRoutes;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property ControllerList: TControllerList read ReadControllerList write WriteControllerList;
    property FileRouter: TKyFileRoutes read ReadFileRouter write WriteFileRouter;
    property Router: TKyoukaiRouteHandler read ReadRouter write WriteRouter;
  end;

  TKyoukaiHTTPServerClass = class of TKyoukaiHTTPServer;

  TKyoukaiHTTPServerThread = class(TThread)
  private
    _Error: string;

  protected
    procedure Execute; override;
  public
    fServer: TKyoukaiHTTPServer;
    constructor Create(APort: word; ARouter: TKyoukaiRouteHandler; AControllerList: TControllerList; AMimeFileName: string;
      var AFileRouter: TKyFileRoutes);
    destructor Destroy; override;
    property Error: string read _Error;
    //property Server: TKyoukaiHTTPServer read fServer write fServer;
  end;

  TKyHTTPServer = class(TComponent)
  private
    fServerThread: TKyoukaiHTTPServerThread;
    fMimeTypesFile: string;
    fRouter: TKyoukaiRouteHandler;
    fControllerList: TControllerList;
    fFileRouter: TKyFileRoutes;
    fPort: word;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Please write all properties before you're starting server
    procedure Start;
    // Just stop the server
    procedure Stop;
  published
    property Port: word read fPort write fPort;
    property MimeTypesFile: string read fMimeTypesFile write fMimeTypesFile;
    property Router: TKyoukaiRouteHandler read fRouter write fRouter;
    property ControllerList: TControllerList read fControllerList write fControllerList;
    property FileRouter: TKyFileRoutes read fFileRouter write fFileRouter;
  end;

implementation

procedure TKyoukaiHTTPServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent.ClassParent.ClassName = 'TKyModule') then
  begin
    WriteLn('someone free: ' + AComponent.ClassName + ', parent: ' +
      AComponent.ClassParent.ClassName);
  end;

end;

procedure TKyoukaiHTTPServer.WriteControllerList(AControllerList: TControllerList);
begin
  fWebHandler.ControllerList := AControllerList;
end;

function TKyoukaiHTTPServer.ReadControllerList: TControllerList;
begin
  Result := fWebHandler.ControllerList;
end;

procedure TKyoukaiHTTPServer.WriteRouter(ARouter: TKyoukaiRouteHandler);
begin
  fWebHandler.Router := ARouter;
end;

function TKyoukaiHTTPServer.ReadRouter: TKyoukaiRouteHandler;
begin
  Result := fWebHandler.Router;
end;

procedure TKyoukaiHTTPServer.WriteFileRouter(ARoutes: TKyFileRoutes);
begin
  fWebHandler.FileRouter := ARoutes;
end;

function TKyoukaiHTTPServer.ReadFileRouter: TKyFileRoutes;
begin
  Result := fWebHandler.FileRouter;
end;

procedure TKyoukaiHTTPServer.WriteMimeTypesFile(AFileName: string);
begin
  fWebHandler.MimeTypesFile := AFileName;
end;

function TKyoukaiHTTPServer.ReadMimeTypesFile: string;
begin
  Result := fWebHandler.MimeTypesFile;
end;

procedure TKyoukaiHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  fWebHandler.DoHandleRequest(ARequest, AResponse);
end;

constructor TKyoukaiHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWebHandler := TKyoukaiHTTPHandler.Create;
end;

destructor TKyoukaiHTTPServer.Destroy;
begin
  FreeAndNil(fWebHandler);
  inherited Destroy;
end;

// KyServerThread
procedure TKyoukaiHTTPServerThread.Execute;
begin
  try
    try
      fServer.Active := True;
    finally
      FreeAndNil(fServer);
    end;
  except
    on E: Exception do
    begin
      _Error := E.Message;
    end;
  end;
end;

constructor TKyoukaiHTTPServerThread.Create(APort: word; ARouter: TKyoukaiRouteHandler;
  AControllerList: TControllerList; AMimeFileName: string; var AFileRouter: TKyFileRoutes);
begin
  inherited Create(False);
  fServer := TKyoukaiHTTPServer.Create(nil);
  fServer.Port := APort;
  fServer.ControllerList := AControllerList;
  fServer.Router := ARouter;
  fServer.MimeTypesFile := AMimeFileName;
  fServer.FileRouter := AFileRouter;
  Self.FreeOnTerminate := True;
  _Error := 'nil';
end;

destructor TKyoukaiHTTPServerThread.Destroy;
begin
  inherited Destroy;
end;

// Server component

procedure TKyHTTPServer.Start;
begin

  fServerThread := TKyoukaiHTTPServerThread.Create(fPort, fRouter, fControllerList,
    fMimeTypesFile, fFileRouter);
end;

procedure TKyHTTPServer.Stop;
begin
  // need to create a fake request to stop this?
  fServerThread.fServer.Active := False;

  with TFPHTTPClient.Create(nil) do
  begin
    try
      Get('http://localhost:' + IntToStr(fPort) + '/kyoukai_info');
    except
      // silently ignore an error
    end;
  end;
end;

constructor TKyHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fPort := 80;
end;

destructor TKyHTTPServer.Destroy;
begin
  inherited Destroy;
end;

initialization

end.
