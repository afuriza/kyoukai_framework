unit Kyoukai.Standard.CGIUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, httpdefs, fpmimetypes, custcgi, httproute,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil;

type
  TKyoukaiCGIWrapper = TCustomCGIApplication;

  TKyCustCGIHandler = class(TComponent)
  private
    type
    TURICallback =
    procedure of object;
  private
    fKyoukaiCGI: TKyoukaiCGIWrapper;
  protected
    fMimeTypesFile: string;
    KMime: TFPMimeTypes;
    fRouter: TKyRoutes;
    fFileRouter: TKyFileRoutes;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WriteMimeTypesFile(AFileName: string);
    procedure Cust404Handle(var ARequest: TRequest;
      var AResponse: TResponse);
    function ReadMimeTypesFile: string;
    procedure SendFile(const AFileName: string;
      var AResponse: TResponse);

    procedure HandleRequest(ARequest: TRequest;
      AResponse: TResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
    procedure Terminate;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property Router: TKyRoutes read fRouter write fRouter;
    property FileRouter: TKyFileRoutes read fFileRouter write fFileRouter;
  end;

implementation
  //RegisterRoutes;

procedure TKyCustCGIHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent.ClassParent.ClassName = 'TKyModule') then
  begin
    WriteLn('someone free: ' + AComponent.ClassName + ', parent: ' +
      AComponent.ClassParent.ClassName);
  end;

end;

procedure TKyCustCGIHandler.WriteMimeTypesFile(AFileName: string);
begin
  if AFileName <> fMimeTypesFile then
  begin
    KMime.LoadFromFile(AFileName);
    fMimeTypesFile := AFileName;
  end;
end;

function TKyCustCGIHandler.ReadMimeTypesFile: string;
begin
  Result := fMimeTypesFile;
end;

procedure TKyCustCGIHandler.Cust404Handle(var ARequest: TRequest;
  var AResponse: TResponse);
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

procedure TKyCustCGIHandler.SendFile(const AFileName: string;
  var AResponse: TResponse);
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

procedure TKyCustCGIHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
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

procedure TKyCustCGIHandler.Run;
begin
  fKyoukaiCGI.Initialize;
  fKyoukaiCGI.Run;
end;

procedure TKyCustCGIHandler.Terminate;
begin
  fKyoukaiCGI.Terminate;
end;

constructor TKyCustCGIHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  KMime := TFPMimeTypes.Create(Self);
  fKyoukaiCGI := TKyoukaiCGIWrapper.Create(self);
  HTTPRouter.RegisterRoute('*path',rmall,@HandleRequest,True);
end;

destructor TKyCustCGIHandler.Destroy;
begin
  FreeAndNil(KMime);
  inherited Destroy;
end;

initialization

end.

