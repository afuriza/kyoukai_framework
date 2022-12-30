unit Kyoukai.Base.WebHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs, fpmimetypes,
  Kyoukai.Base.Controller,
  Kyoukai.Core.CommonUtil,
  Kyoukai.Core.Base64Util,
  Kyoukai.Base.DefaultHTML,
  Kyoukai.Base.WebRouter,
  Kyoukai.Base.CGIDefs;

type

  TKyoukaiHTTPHandler = class(TObject)
  private
    fMimeTypesFile: string;
    KMime: TFPMimeTypes;
    fControllers: TControllerList;
    fRouter: TKyoukaiRouteHandler;
    fFileRouter: TKyFileRoutes;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
    procedure Cust404Handle(var ARequest: TRequest;
      var AResponse: TResponse; var StartServeTime: TDateTime);
    procedure Cust500Handle(var ARequest: TRequest;
      var AResponse: TResponse; const ErrorStr: string);
    procedure SendFile(const AFileName: string;
      var AResponse: TResponse);
  public
    AutoRouting: boolean;
    procedure DoHandleRequest(ARequest: TRequest;
      AResponse: TResponse);
    constructor Create;
    destructor Destroy; override;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property ControllerList: TControllerList read fControllers write fControllers;
    property Router: TKyoukaiRouteHandler read FRouter write FRouter;
    property FileRouter: TKyFileRoutes read fFileRouter write fFileRouter;
  end;

implementation

{ HTTP Handler }

procedure TKyoukaiHTTPHandler.WriteMimeTypesFile(AFileName: string);
begin
  if AFileName <> fMimeTypesFile then
  begin
    KMime.LoadFromFile(AFileName);
    fMimeTypesFile := AFileName;
  end;
end;

function TKyoukaiHTTPHandler.ReadMimeTypesFile: string;
begin
  Result := fMimeTypesFile;
end;

procedure TKyoukaiHTTPHandler.Cust404Handle(var ARequest: TRequest;
  var AResponse: TResponse; var StartServeTime: TDateTime);
var
  CallProc: TProcCallback;
  ModuleWorker404: TController;
  SiteLink: string;
begin
  if CGIROOTPath = '' then
    SiteLink := ARequest.URL
  else
    SiteLink := CGIRootPath;
  if ControllerList.Contains('404_override') then
  begin
    ModuleWorker404 := TControllerClass(ControllerList['404_override']).Create(nil,
      ARequest, AResponse, 'There''s no module or method related with this URL: "' +
      ARequest.URL + '"!');
    try
      if ModuleWorker404.MethodAddress('BeforeHandle') <> nil then
      begin
        TMethod(CallProc).Code := ModuleWorker404.MethodAddress('BeforeHandle');
        TMethod(CallProc).Data := ModuleWorker404;
        CallProc;
      end;
      if ModuleWorker404.MethodAddress('MainHandle') <> nil then
      begin
        AResponse.Code := 404;
        TMethod(CallProc).Code := ModuleWorker404.MethodAddress('MainHandle');
        TMethod(CallProc).Data := ModuleWorker404;
        CallProc;
      end
      else
      begin
        AResponse.Code := 404;
        AResponse.Content := GetNotFoundInformation(ARequest.Host,
          SiteLink, 'No main handle method found! Did you forget ' +
          'to create MainHandle to override 404 Module?', Now);
      end;

    finally
      if ModuleWorker404.MethodAddress('AfterHandle') <> nil then
      begin
        TMethod(CallProc).Code := ModuleWorker404.MethodAddress('AfterHandle');
        TMethod(CallProc).Data := ModuleWorker404;
        CallProc;
      end;
      FreeAndNil(ModuleWorker404);
    end;
  end
  else
  begin
    AResponse.Code := 404;
    AResponse.Content := GetNotFoundInformation(ARequest.Host,
      SiteLink, 'There''s no module or method related with this URL: "' +
      ARequest.URL + '"!', StartServeTime);
  end;

end;

procedure TKyoukaiHTTPHandler.Cust500Handle(var ARequest: TRequest;
  var AResponse: TResponse; const ErrorStr: string);
var
  CallProc: TProcCallback;
  ModuleWorker: TController;
  SiteLink: string;
begin
  if CGIROOTPath = '' then
    SiteLink := ARequest.URL
  else
    SiteLink := CGIRootPath;
  ModuleWorker := TControllerClass(ControllerList['500_override']).Create(nil,
    ARequest, AResponse, ErrorStr);
  try
    if ModuleWorker.MethodAddress('BeforeHandle') <> nil then
    begin
      TMethod(CallProc).Code := ModuleWorker.MethodAddress('BeforeHandle');
      TMethod(CallProc).Data := ModuleWorker;
      CallProc;
    end;
    if ModuleWorker.MethodAddress('MainHandle') <> nil then
    begin
      AResponse.Code := 500;
      TMethod(CallProc).Code := ModuleWorker.MethodAddress('MainHandle');
      TMethod(CallProc).Data := ModuleWorker;
      CallProc;
    end
    else
    begin
      AResponse.Code := 500;
      AResponse.Content := GetNotFoundInformation(ARequest.Host,
        SiteLink, 'No main handle method found! Did you forget ' +
        'to create MainHandle to override 500 Module?', Now);
    end;

  finally
    if ModuleWorker.MethodAddress('AfterHandle') <> nil then
    begin
      TMethod(CallProc).Code := ModuleWorker.MethodAddress('AfterHandle');
      TMethod(CallProc).Data := ModuleWorker;
      CallProc;
    end;
    FreeAndNil(ModuleWorker);
  end;

end;

procedure TKyoukaiHTTPHandler.SendFile(const AFileName: string;
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

procedure TKyoukaiHTTPHandler.DoHandleRequest(ARequest: TRequest;
      AResponse: TResponse);
var
  URIStr, URIStr2: string;
  ExplodedURI: TStringList;
  CallProc: TProcCallback;
  ModuleWorker: TController;
  StartServeTime: TDateTime;
  DecodedStream: TStream;
  URIForFile: string = '';
  i: integer;
  SiteLink: string;

  // dynamic routing
  s, key, val: string;
  ARouteSplit, CurrentRouteSplit: TStringArray;
  DynRouteValues: TRouteValues;
  DynHandler: TStringList;
begin
  if CGIROOTPath = '' then
    SiteLink := ARequest.URL
  else
    SiteLink := CGIRootPath;
  StartServeTime := Now;
  if AutoRouting then
  begin
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

      if ControllerList.Contains(URIStr) then
      begin
        ModuleWorker := TControllerClass(ControllerList[URIStr]).Create(nil);
        ModuleWorker.Request := ARequest;
        ModuleWorker.Response := AResponse;
        ModuleWorker.Handled := True;
        try
          if ModuleWorker.MethodAddress('BeforeHandle') <> nil then
          begin
            TMethod(CallProc).Code := ModuleWorker.MethodAddress('BeforeHandle');
            TMethod(CallProc).Data := ModuleWorker;
            CallProc;
          end;
          if URIStr2 = '' then
          begin
            if ModuleWorker.MethodAddress('MainHandle') <> nil then
            begin
              if ModuleWorker.Handled then
              begin
                TMethod(CallProc).Code := ModuleWorker.MethodAddress('MainHandle');
                TMethod(CallProc).Data := ModuleWorker;
                CallProc;
              end;
            end
            else
            begin
              Cust404Handle(ARequest, AResponse, StartServeTime);
            end;
          end
          else
          begin
            if ModuleWorker.MethodAddress(URIStr2) <> nil then
            begin
              if ModuleWorker.Handled then
              begin
                TMethod(CallProc).Code := ModuleWorker.MethodAddress(URIStr2);
                TMethod(CallProc).Data := ModuleWorker;
                CallProc;
              end;
            end
            else
            begin
              Cust404Handle(ARequest, AResponse, StartServeTime);
            end;
          end;

        finally
          if ModuleWorker.MethodAddress('AfterHandle') <> nil then
          begin
            TMethod(CallProc).Code := ModuleWorker.MethodAddress('AfterHandle');
            TMethod(CallProc).Data := ModuleWorker;
            CallProc;
          end;
          FreeAndNil(ModuleWorker);
        end;

      end

      else if URIStr = 'kyoukai_info' then
      begin
        AResponse.Content := GetKyoukaiInformation(ARequest.Host,
          SiteLink, StartServeTime);
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
      else if ControllerList.Contains('main') then
      begin
        ModuleWorker := TControllerClass(ControllerList['main']).Create(nil);
        ModuleWorker.Request := ARequest;
        ModuleWorker.Response := AResponse;
        ModuleWorker.Handled := True;
        try
          if ModuleWorker.MethodAddress('BeforeHandle') <> nil then
          begin
            TMethod(CallProc).Code := ModuleWorker.MethodAddress('BeforeHandle');
            TMethod(CallProc).Data := ModuleWorker;
            CallProc;
          end;
          if ModuleWorker.MethodAddress(URIStr) <> nil then
          begin
            if ModuleWorker.Handled then
            begin
              TMethod(CallProc).Code := ModuleWorker.MethodAddress(URIStr);
              TMethod(CallProc).Data := ModuleWorker;
              CallProc;
            end;
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
                  Cust404Handle(ARequest, AResponse, StartServeTime);
                end;
              end
              else
              begin
                Cust404Handle(ARequest, AResponse, StartServeTime);
              end;
            end
            else
            begin
              Cust404Handle(ARequest, AResponse, StartServeTime);
            end;
          end;

        finally
          if ModuleWorker.MethodAddress('AfterHandle') <> nil then
          begin
            TMethod(CallProc).Code := ModuleWorker.MethodAddress('AfterHandle');
            TMethod(CallProc).Data := ModuleWorker;
            CallProc;
          end;
          FreeAndNil(ModuleWorker);
        end;

      end
      else
      begin
        Cust404Handle(ARequest, AResponse, StartServeTime);
      end;

    except
      on E: Exception do
      begin
        if ControllerList.Contains('500_override') then
        begin
          Cust500Handle(ARequest, AResponse, DumpExceptionCallStack(E));
        end
        else
        begin
          AResponse.Code := 500;
          AResponse.Content := GetErrorInformation(ARequest.Host, SiteLink,
            DumpExceptionCallStack(E), StartServeTime);
        end;
      end;
    end;
  end
  else
  begin
    if ARequest.PathInfo = '' then
      DynRouteValues := Router.GetRouteMatch('/')
    else
      DynRouteValues := Router.GetRouteMatch(HTTPDecode(ARequest.PathInfo));

    if DynRouteValues = nil then
      AResponse.Content := 'nil';

    if (DynRouteValues <> nil) and
      (ARequest.Method in DynRouteValues.Methods) then
    begin

      // method@controller
      DynHandler := TStringList.Create;
      Split('@', DynRouteValues.Handler.ToLower, DynHandler);
      if ControllerList.Contains(DynHandler[1]) then
      begin

        ModuleWorker := TControllerClass(ControllerList[DynHandler[1]]).Create(nil);
        ModuleWorker.Request := ARequest;
        ModuleWorker.Response := AResponse;
        ModuleWorker.Handled := True;
        //ModuleWorker.Params := ;
        i := 0;
        ARouteSplit := DynRouteValues.RouteStr.Split('/');
        CurrentRouteSplit := HTTPDecode(ARequest.PathInfo).ToLower.Split('/');
        for s in ARouteSplit do
        begin
          if s.StartsWith('<') and s.EndsWith('>') then
          begin
            key := s.Replace('<', '').Replace('>', '').Trim;
            val := CurrentRouteSplit[i];
            ModuleWorker.Params.Values[key] := val;
          end;
          Inc(i);
        end;
        if ModuleWorker.MethodAddress(DynHandler[0]) <> nil then
        begin
          if ModuleWorker.Handled then
          begin
            TMethod(CallProc).Code := ModuleWorker.MethodAddress(DynHandler[0]);
            TMethod(CallProc).Data := ModuleWorker;
            CallProc;
          end;
        end
        else
        begin
          Cust404Handle(ARequest, AResponse, StartServeTime);
        end;
      end;
      FreeAndNil(DynHandler);
    end
    else
    begin
      Cust404Handle(ARequest, AResponse, StartServeTime);
    end;
  end;
end;

constructor TKyoukaiHTTPHandler.Create;
begin
  inherited Create;
  KMime := TFPMimeTypes.Create(nil);
  AutoRouting := True;
end;

destructor TKyoukaiHTTPHandler.Destroy;
begin
  FreeAndNil(KMime);
  inherited Destroy;
end;

end.
