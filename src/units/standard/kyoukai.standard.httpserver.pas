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
  Classes, SysUtils, fphttp, fphttpserver, httpdefs, fpmimetypes,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil;

type

  TKyHTTPServer = Class(TFPHTTPServer)
  private
    type
      TURICallback = procedure of object;
  private
    fMimeTypesFile: string;
    KMime: TFPMimeTypes;
    fRouter: TKyRoutes;
    fFileRouter: TFileRouteMap;
    procedure WriteMimeTypesFile(AFileName: string);
    procedure Cust404Handle(var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    function ReadMimeTypesFile: string;
    procedure KHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure SendFile(const AFileName: string; var AResponse: TFPHTTPConnectionResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property Router: TKyRoutes read fRouter write fRouter;
    property FileRoutes: TFileRouteMap read fFileRouter write fFileRouter;
  end;

  TKyHTTPServerClass = class of TKyHTTPServer;

  TKyHTTPServerThread = class(TThread)
  private
    fKyServer: TKyHTTPServerClass;
    fRouter: TKyRoutes;
  public
    constructor Create(AServer: TKyHTTPServerClass);
    destructor Destroy; override;
  published
    property Router: TKyRoutes read fRouter write fRouter;
  end;

  TKyHTTPServerComponents = class(TComponent)

  end;

implementation

procedure TKyHTTPServer.WriteMimeTypesFile(AFileName: string);
begin
  if AFileName <> fMimeTypesFile then
  begin
    KMime.LoadFromFile(AFileName);
    fMimeTypesFile := AFileName;
  end;
end;

function TKyHTTPServer.ReadMimeTypesFile: string;
begin
  Result := fMimeTypesFile;
end;

procedure TKyHTTPServer.Cust404Handle(var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
var
  CallFunc: TURICallback;
  ModuleWorker: TKyModule;
begin
  ModuleWorker := TKyModuleClass(Router['404_override']).Create(Self, ARequest, AResponse);
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
    AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
      'No main handle method found! Did you forget '+
      'to create MainHandle to override 404 Module?', Now);
  end;
  FreeAndNil(ModuleWorker);
end;

procedure TKyHTTPServer.SendFile(Const AFileName: String; var AResponse: TFPHTTPConnectionResponse);
var
  F: TFileStream;
begin
  if FileExists(AFileName) then
  begin
    AResponse.ContentType:=KMime.GetMimeType(ExtractFileExt(AFileName));
    if (AResponse.ContentType='') then
      AResponse.ContentType:='Application/octet-stream';
    F:=TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
    try
      AResponse.ContentLength:=F.Size;
      AResponse.ContentStream:=F;
      AResponse.SendContent;
      AResponse.ContentStream:=Nil;
    finally
      FreeAndNil(F);
    end;
  end
  else
  begin
    raise Exception.Create('Can''t open file with this name: '+AFileName);
  end;
end;

procedure TKyHTTPServer.KHandleRequest(Sender: TObject;var ARequest: TFPHTTPConnectionRequest;
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
    Split('/',  HTTPDecode(ARequest.PathInfo), ExplodedURI);
    if ExplodedURI.Count > 0 then
    begin
      if ExplodedURI.Count > 1 then
        URIStr := LowerCase(ExplodedURI[1]);
      if ExplodedURI.Count > 2 then
        URIStr2 := LowerCase(ExplodedURI[2]);
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
    ExplodedURI.Free;

    if Router.Contains(URIStr) then
    begin
      ModuleWorker := TKyModuleClass(Router[URIStr]).Create(Self, ARequest,
        AResponse);
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
          AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
            'No main handle method found!',
            StartServeTime);
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
          AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
            'There''s no handle method with this name: '+ URIStr2 +'!',
            StartServeTime);
        end;
      end;
      FreeAndNil(ModuleWorker);
    end
    else if URIStr = '' then
    begin
      if Router.Contains('main') then
      begin
        ModuleWorker := TKyModuleClass(Router['main']).Create(Self, ARequest,
          AResponse);
        if ModuleWorker.MethodAddress('MainHandle') <> nil then
        begin
          TMethod(CallFunc).Code := ModuleWorker.MethodAddress('MainHandle');
          TMethod(CallFunc).Data := ModuleWorker;
          CallFunc;
        end
        else
        begin
          AResponse.Code := 404;
          AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
            'No main module handle method found!', StartServeTime);
        end;
        FreeAndNil(ModuleWorker);
      end
      else
      begin
        AResponse.Code := 204;
        AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
          'The server successfully processed the request and is not returning any content!',
          StartServeTime);
      end;
    end
    else if URIStr = 'kyoukai_info' then
    begin
      AResponse.Content := GetKyoukaiInformation(ARequest.Host, ARequest.URL,
        StartServeTime);
    end
    else if URIStr = 'ky_icon_nyanpasu.png' then
    begin
      DecodedStream := DecodeBase64StrToStream(base64_nyanpasu_icon_35p);
      AResponse.ContentType := 'image/png';
      AResponse.ContentLength := DecodedStream.Size;
      AResponse.ContentStream := DecodedStream;
      AResponse.SendContent;
      DecodedStream.Free;
    end
    else if fFileRouter.Contains(URIStr) then
    begin
      sendfile(fFileRouter[URIStr] + URIForFile, AResponse);
    end
    else if Router.Contains('main') then
    begin
      ModuleWorker := TKyModuleClass(Router['main']).Create(Self, ARequest,
        AResponse);
      if ModuleWorker.MethodAddress(URIStr) <> nil then
      begin
        TMethod(CallFunc).Code := ModuleWorker.MethodAddress(URIStr);
        TMethod(CallFunc).Data := ModuleWorker;
        CallFunc;
      end
      else
      begin
        AResponse.Code := 404;
        AResponse.Content := GetNotFoundInformation(ARequest.Host, ARequest.URL,
          'There''s no module or main module method with this name: '+ URIStr +'!',
          StartServeTime);
      end;
      FreeAndNil(ModuleWorker);
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

constructor TKyHTTPServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fFileRouter := TFileRouteMap.create;
  KMime := TFPMimeTypes.Create(Self);
  OnRequest := @KHandleRequest;
end;

destructor TKyHTTPServer.Destroy;
begin
  FreeAndNil(KMime);
  FreeAndNil(fFileRouter);
  inherited Destroy;
end;

// KyServerThread

constructor TKyHTTPServerThread.Create(AServer: TKyHTTPServerClass);
begin

end;

destructor TKyHTTPServerThread.Destroy;
begin

end;

initialization

end.
