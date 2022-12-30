unit Kyoukai.Base.CGIUtil;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, httpdefs, fpmimetypes, custcgi, httproute,
  cgiprotocol,
  Kyoukai.Base.WebRouter,
  Kyoukai.Base.WebHandler,
  Kyoukai.Base.Controller,
  Kyoukai.Base.CGIDefs;

type
  TKyoukaiCGIWrapper = TCGIHandler;

  TKyoukaiCGIHandler = class(TComponent)
  private
    fKyoukaiCGI: TKyoukaiCGIWrapper;
    fWebHandler: TKyoukaiHTTPHandler;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
    procedure WriteControllerList(AControllerList: TControllerList);
    function ReadControllerList: TControllerList;
    procedure WriteRouter(ARouter: TKyoukaiRouteHandler);
    function ReadRouter: TKyoukaiRouteHandler;

    procedure WriteFileRouter(AControllerList: TKyFileRoutes);
    function ReadFileRouter: TKyFileRoutes;

    procedure WriteAutoRouting(AValue: boolean);
    function ReadAutoRouting: boolean;

    procedure HandleRequest(ARequest: TRequest;
      AResponse: TResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
    procedure Terminate;
  published
    property AutoRouting: boolean read ReadAutoRouting write WriteAutoRouting;
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property ControllerList: TControllerList read ReadControllerList write WriteControllerList;
    property FileRouter: TKyFileRoutes read ReadFileRouter write WriteFileRouter;
    property Router: TKyoukaiRouteHandler read ReadRouter write WriteRouter;
  end;

implementation

procedure TKyoukaiCGIHandler.WriteControllerList(AControllerList: TControllerList);
begin
  fWebHandler.ControllerList := AControllerList;
end;

function TKyoukaiCGIHandler.ReadControllerList: TControllerList;
begin
  Result := fWebHandler.ControllerList;
end;

procedure TKyoukaiCGIHandler.WriteRouter(ARouter: TKyoukaiRouteHandler);
begin
  fWebHandler.Router := ARouter;
end;

function TKyoukaiCGIHandler.ReadRouter: TKyoukaiRouteHandler;
begin
  Result := fWebHandler.Router;
end;

procedure TKyoukaiCGIHandler.WriteAutoRouting(AValue: boolean);
begin
  fWebHandler.AutoRouting := AValue;
end;

function TKyoukaiCGIHandler.ReadAutoRouting: boolean;
begin
  Result := fWebHandler.AutoRouting;
end;

procedure TKyoukaiCGIHandler.WriteFileRouter(AControllerList: TKyFileRoutes);
begin
  fWebHandler.FileRouter := AControllerList;
end;

function TKyoukaiCGIHandler.ReadFileRouter: TKyFileRoutes;
begin
  Result := fWebHandler.FileRouter;
end;

procedure TKyoukaiCGIHandler.WriteMimeTypesFile(AFileName: string);
begin
  fWebHandler.MimeTypesFile := AFileName;
end;

function TKyoukaiCGIHandler.ReadMimeTypesFile: string;
begin
  Result := fWebHandler.MimeTypesFile;
end;

procedure TKyoukaiCGIHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
var
  APPROOT: string;
  URISTR: string;
begin
  APPROOT := ExtractFilePath(StringReplace(GetEnvironmentVariable('SCRIPT_FILENAME'),
    GetEnvironmentVariable('DOCUMENT_ROOT'), '', [rfReplaceAll]));
  URISTR := StringReplace('/'+ StringReplace(GetEnvironmentVariable('REQUEST_URI'),
    APPROOT, '', [rfIgnoreCase]), '//', '/', [rfIgnoreCase,rfReplaceAll]);
  if (ARequest.URI = '') or (ARequest.URL = '') then
  begin
    ARequest.URI := URISTR;
    ARequest.URL := GetEnvironmentVariable('REQUEST_URI');
  end;
  CGIROOTPath := APPROOT;
  fWebHandler.DoHandleRequest(ARequest, AResponse);
end;

procedure TKyoukaiCGIHandler.Run;
var
  UploadPath: string;
begin
  //fKyoukaiCGI.Initialize;
  if not DirectoryExists(ExtractFilePath(ParamStr(0)) +
    'kyoukai_temp\uploadedfiles') then
    ForceDirectories(ExtractFilePath(ParamStr(0)) +
    'kyoukai_temp\uploadedfiles');
  UploadPath := ExtractFilePath(ParamStr(0)) +
    'kyoukai_temp\uploadedfiles';
  fKyoukaiCGI.Request.DefaultRequestUploadDir := UploadPath;
  fKyoukaiCGI.Run;
end;

procedure TKyoukaiCGIHandler.Terminate;
begin
  //fKyoukaiCGI.Terminate;
end;

constructor TKyoukaiCGIHandler.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWebHandler := TKyoukaiHTTPHandler.Create;
  fKyoukaiCGI := TKyoukaiCGIWrapper.Create(self);
  HTTPRouter.RegisterRoute('*',rmall,@HandleRequest,True);
end;

destructor TKyoukaiCGIHandler.Destroy;
begin
  FreeAndNil(fWebHandler);
  FreeAndNil(fKyoukaiCGI);
  inherited Destroy;
end;

initialization

end.

