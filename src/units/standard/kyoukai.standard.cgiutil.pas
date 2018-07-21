unit Kyoukai.Standard.CGIUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcgi, httpdefs, fpmimetypes, custcgi, httproute,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.DefaultHTML,
  Kyoukai.Standard.WebHandler,
  Kyoukai.Other.Base64Util,
  Kyoukai.Other.CommonUtil;

type
  TKyoukaiCGIWrapper = TCustomCGIApplication;

  TKyCustCGIHandler = class(TComponent)
  private
    fKyoukaiCGI: TKyoukaiCGIWrapper;
    fWebHandler: TKyCustHTTPHandler;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
    procedure WriteRouter(ARoutes: TKyRoutes);
    function ReadRouter: TKyRoutes;
    procedure WriteFileRouter(ARoutes: TKyFileRoutes);
    function ReadFileRouter: TKyFileRoutes;
    procedure HandleRequest(ARequest: TRequest;
      AResponse: TResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
    procedure Terminate;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property Router: TKyRoutes read ReadRouter write WriteRouter;
    property FileRouter: TKyFileRoutes read ReadFileRouter write WriteFileRouter;
  end;

implementation

procedure TKyCustCGIHandler.WriteRouter(ARoutes: TKyRoutes);
begin
  fWebHandler.Router := ARoutes;
end;

function TKyCustCGIHandler.ReadRouter: TKyRoutes;
begin
  Result := fWebHandler.Router;
end;

procedure TKyCustCGIHandler.WriteFileRouter(ARoutes: TKyFileRoutes);
begin
  fWebHandler.FileRouter := ARoutes;
end;

function TKyCustCGIHandler.ReadFileRouter: TKyFileRoutes;
begin
  Result := fWebHandler.FileRouter;
end;

procedure TKyCustCGIHandler.WriteMimeTypesFile(AFileName: string);
begin
  fWebHandler.MimeTypesFile := AFileName;
end;

function TKyCustCGIHandler.ReadMimeTypesFile: string;
begin
  Result := fWebHandler.MimeTypesFile;
end;

procedure TKyCustCGIHandler.HandleRequest(ARequest: TRequest;
  AResponse: TResponse);
begin
  fWebHandler.DoHandleRequest(ARequest, AResponse);
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
  fWebHandler := TKyCustHTTPHandler.Create;
  fKyoukaiCGI := TKyoukaiCGIWrapper.Create(self);
  HTTPRouter.RegisterRoute('*',rmall,@HandleRequest,True);
end;

destructor TKyCustCGIHandler.Destroy;
begin
  FreeAndNil(fWebHandler);
  FreeAndNil(fKyoukaiCGI);
  inherited Destroy;
end;

initialization

end.

