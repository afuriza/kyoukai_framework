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
  Classes, SysUtils, fphttp, fphttpserver, httpdefs,
  fpmimetypes, fphttpclient,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil;

type

  TKyCustHTTPServer = class(TFPHTTPServer)
  private
    type
    TURICallback =
    procedure of object;
  protected
    fMimeTypesFile: string;
    KMime: TFPMimeTypes;
    fRouter: TKyRoutes;
    fFileRouter: TKyFileRoutes;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WriteMimeTypesFile(AFileName: string);
    procedure Cust404Handle(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ReadMimeTypesFile: string;
    procedure SendFile(const AFileName: string;
      var AResponse: TFPHTTPConnectionResponse);

    procedure HandleRequest(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property Router: TKyRoutes read fRouter write fRouter;
    property FileRouter: TKyFileRoutes read fFileRouter write fFileRouter;
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
    property FileRoutes: TKyFileRoutes read fFileRouter write fFileRouter;
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

procedure TKyCustHTTPServer.WriteMimeTypesFile(AFileName: string);
begin
  if AFileName <> fMimeTypesFile then
  begin
    KMime.LoadFromFile(AFileName);
    fMimeTypesFile := AFileName;
  end;
end;

function TKyCustHTTPServer.ReadMimeTypesFile: string;
begin
  Result := fMimeTypesFile;
end;

procedure TKyCustHTTPServer.Cust404Handle(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  CallFunc: TURICallback;
  ModuleWorker: TKyModule;
begin
  ModuleWorker := TKyModuleClass(Router['404_override']).Create(nil,
    ARequest, AResponse);
  try
    if ModuleWorker.MethodAddress('MainHandle') <> nil then
    begin
      AResponse.Code := 404;
      TMethod(CallFunc).Code := ModuleWorker.MethodAddress('MainHandle');
      TMethod(CallFunc).Data := ModuleWorker;
      CallFunc;
    end
    else
    begin
      AResponse.Code := 404;
      AResponse.Content := GetNotFoundInformation(ARequest.Host,
        ARequest.URL, 'No main handle method found! Did you forget ' +
        'to create MainHandle to override 404 Module?', Now);
    end;

  finally
    FreeAndNil(ModuleWorker);
  end;

end;

procedure TKyCustHTTPServer.SendFile(const AFileName: string;
  var AResponse: TFPHTTPConnectionResponse);
var
  F: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    AResponse.ContentType := KMime.GetMimeType(ExtractFileExt(AFileName));
    if (AResponse.ContentType = '') then
      AResponse.ContentType := 'Application/octet-stream';
    F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      AResponse.ContentLength := F.Size;
      AResponse.ContentStream := F;
      AResponse.SendContent;
      AResponse.ContentStream := nil;
    finally
      FreeAndNil(F);
    end;
  end
  else
  begin
    raise Exception.Create('Can''t open file with this name: ' + AFileName);
  end;
end;

procedure TKyCustHTTPServer.HandleRequest(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  URIStr, URIStr2: string;
  ExplodedURI: TStringList;
  CallFunc: TURICallback;
  ModuleWorker: TKyModule;
  StartServeTime: TDateTime;
  DecodedStream: TStream;
  URIForFile: string;
  i: integer;
begin
  StartServeTime := Now;

  try
    ExplodedURI := TStringList.Create;
    Split('/', HTTPDecode(ARequest.PathInfo), ExplodedURI);
    if ExplodedURI.Count > 0 then
    begin
      if ExplodedURI.Count > 1 then
        URIStr := LowerCase(ExplodedURI[1]);
      if ExplodedURI.Count > 2 then
        URIStr2 := LowerCase(ExplodedURI[2]);
    end;
    if (URIStr = '') then
    begin
      URIStr := 'main';
    end;
    if ExplodedURI.Count > 2 then
    begin
      for i := 2 to ExplodedURI.Count - 1 do
      begin
        if i <> ExplodedURI.Count - 1 then
          URIForFile += ExplodedURI[i] + PathDelim
        else
          URIForFile += ExplodedURI[i];
      end;
    end;
    FreeAndNil(ExplodedURI);

    if Router.Contains(URIStr) then
    begin
      ModuleWorker := TKyModuleClass(Router[URIStr]).Create(nil,
        ARequest, AResponse);
      try
        if URIStr2 = '' then
        begin
          if ModuleWorker.MethodAddress('MainHandle') <> nil then
          begin
            TMethod(CallFunc).Code := ModuleWorker.MethodAddress('MainHandle');
            TMethod(CallFunc).Data := ModuleWorker;
            CallFunc;
          end
          else
          begin
            AResponse.Code := 404;
            AResponse.Content :=
              GetNotFoundInformation(ARequest.Host, ARequest.URL,
              'No main handle method found!', StartServeTime);
          end;
        end
        else
        begin
          if ModuleWorker.MethodAddress(URIStr2) <> nil then
          begin
            TMethod(CallFunc).Code := ModuleWorker.MethodAddress(URIStr2);
            TMethod(CallFunc).Data := ModuleWorker;
            CallFunc;
          end
          else
          begin
            AResponse.Code := 404;
            AResponse.Content :=
              GetNotFoundInformation(ARequest.Host, ARequest.URL,
              'There''s no handle method with this name: ' + URIStr2 +
              '!', StartServeTime);
          end;
        end;

      finally
        FreeAndNil(ModuleWorker);
      end;

    end

    else if URIStr = 'kyoukai_info' then
    begin
      AResponse.Content := GetKyoukaiInformation(ARequest.Host,
        ARequest.URL, StartServeTime);
    end
    else if URIStr = 'ky_icon_nyanpasu.png' then
    begin
      DecodedStream := DecodeBase64StrToStream(base64_nyanpasu_icon_35p);
      AResponse.ContentType := 'image/png';
      AResponse.ContentLength := DecodedStream.Size;
      AResponse.ContentStream := DecodedStream;
      AResponse.SendContent;
      FreeAndNil(DecodedStream);
    end
    else if Router.Contains('main') then
    begin
      ModuleWorker := TKyModuleClass(Router['main']).Create(nil,
        ARequest, AResponse);
      try
        if ModuleWorker.MethodAddress(URIStr) <> nil then
        begin
          TMethod(CallFunc).Code := ModuleWorker.MethodAddress(URIStr);
          TMethod(CallFunc).Data := ModuleWorker;
          CallFunc;
        end
        else
        begin
          if Assigned(fFileRouter) then
          begin
            if fFileRouter.size > 0 then
            begin
              if fFileRouter.Contains(URIStr) then
                sendfile(fFileRouter[URIStr] + URIForFile, AResponse)
              else
              begin
                AResponse.Code := 404;
                AResponse.Content :=
                  GetNotFoundInformation(ARequest.Host, ARequest.URL,
                  'There''s no module or main module method with this name: ' +
                  URIStr + '!', StartServeTime);
              end;
            end;
          end
          else
          begin
            AResponse.Code := 404;
            AResponse.Content :=
              GetNotFoundInformation(ARequest.Host, ARequest.URL,
              'There''s no module or main module method with this name: ' +
              URIStr + '!', StartServeTime);
          end;
        end;

      finally
        FreeAndNil(ModuleWorker);
      end;

    end
    else
    begin
      AResponse.Code := 404;
      AResponse.Content := GetNotFoundInformation(ARequest.Host,
        ARequest.URL, 'There''s no module or main module method with this name: ' +
        URIStr + '!', StartServeTime);
    end;

  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := GetErrorInformation(ARequest.Host, ARequest.URL,
        DumpExceptionCallStack(E), StartServeTime);
    end;
  end;

end;

constructor TKyCustHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  KMime := TFPMimeTypes.Create(Self);
end;

destructor TKyCustHTTPServer.Destroy;
begin
  FreeAndNil(KMime);
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
