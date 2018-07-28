{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Standard.HTTPServer;

{$mode objfpc}{$H+}

interface

uses
  cmem,
  Classes, SysUtils, fphttp, fphttpserver, httpdefs,
  fpmimetypes, fphttpclient,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebHandler;

type

  TKyCustHTTPServer = class(TFPHTTPServer)
  private
    fWebHandler: TKyCustHTTPHandler;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
    procedure WriteRouter(ARoutes: TKyRoutes);
    function ReadRouter: TKyRoutes;
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
    property Router: TKyRoutes read ReadRouter write WriteRouter;
    property FileRouter: TKyFileRoutes read ReadFileRouter write WriteFileRouter;
  end;

  TKyCustHTTPServerClass = class of TKyCustHTTPServer;

  TKyCustHTTPServerThread = class(TThread)
  private
    _Error: string;

  protected
    procedure Execute; override;
  public
    fServer: TKyCustHTTPServer;
    constructor Create(APort: word; ARouter: TKyRoutes; AMimeFileName: string;
      var AFileRouter: TKyFileRoutes);
    destructor Destroy; override;
    property Error: string read _Error;
    //property Server: TKyCustHTTPServer read fServer write fServer;
  end;

  TKyHTTPServer = class(TComponent)
  private
    fServerThread: TKyCustHTTPServerThread;
    fMimeTypesFile: string;
    fRouter: TKyRoutes;
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
    property Router: TKyRoutes read fRouter write fRouter;
    property FileRouter: TKyFileRoutes read fFileRouter write fFileRouter;
  end;

implementation

procedure TKyCustHTTPServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent.ClassParent.ClassName = 'TKyModule') then
  begin
    WriteLn('someone free: ' + AComponent.ClassName + ', parent: ' +
      AComponent.ClassParent.ClassName);
  end;

end;

procedure TKyCustHTTPServer.WriteRouter(ARoutes: TKyRoutes);
begin
  fWebHandler.Router := ARoutes;
end;

function TKyCustHTTPServer.ReadRouter: TKyRoutes;
begin
  Result := fWebHandler.Router;
end;

procedure TKyCustHTTPServer.WriteFileRouter(ARoutes: TKyFileRoutes);
begin
  fWebHandler.FileRouter := ARoutes;
end;

function TKyCustHTTPServer.ReadFileRouter: TKyFileRoutes;
begin
  Result := fWebHandler.FileRouter;
end;

procedure TKyCustHTTPServer.WriteMimeTypesFile(AFileName: string);
begin
  fWebHandler.MimeTypesFile := AFileName;
end;

function TKyCustHTTPServer.ReadMimeTypesFile: string;
begin
  Result := fWebHandler.MimeTypesFile;
end;

procedure TKyCustHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  fWebHandler.DoHandleRequest(ARequest, AResponse);
end;

constructor TKyCustHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWebHandler := TKyCustHTTPHandler.Create;
end;

destructor TKyCustHTTPServer.Destroy;
begin
  FreeAndNil(fWebHandler);
  inherited Destroy;
end;

// KyServerThread
procedure TKyCustHTTPServerThread.Execute;
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

constructor TKyCustHTTPServerThread.Create(APort: word; ARouter: TKyRoutes;
  AMimeFileName: string; var AFileRouter: TKyFileRoutes);
begin
  inherited Create(False);
  fServer := TKyCustHTTPServer.Create(nil);
  fServer.Port := APort;
  fServer.Router := ARouter;
  fServer.MimeTypesFile := AMimeFileName;
  fServer.FileRouter := AFileRouter;
  Self.FreeOnTerminate := True;
  _Error := 'nil';
end;

destructor TKyCustHTTPServerThread.Destroy;
begin
  inherited Destroy;
end;

// Server component

procedure TKyHTTPServer.Start;
begin

  fServerThread := TKyCustHTTPServerThread.Create(fPort, fRouter,
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
  //fFileRouter := TKyFileRoutes.Create;
end;

destructor TKyHTTPServer.Destroy;
begin
  inherited Destroy;
  //FreeAndNil(fFileRouter);
end;

initialization

end.
