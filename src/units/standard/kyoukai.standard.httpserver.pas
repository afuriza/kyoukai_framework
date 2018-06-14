{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
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
  Kyoukai.Other.Base64Util;

type

  TKyServer = Class(TFPHTTPServer)
  private
    type
      TURICallback = procedure of object;
  private
    fMimeTypesFile: string;
    KMime: TFPMimeTypes;
    fRouter: TKyRoutes;
    fFileRouter: TFileRouteMap;
    procedure WriteMimeTypesFile(AFileName: string);
    function ReadMimeTypesFile: string;
    procedure KHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure SendFile(Const AFileName : String; var AResponse : TFPHTTPConnectionResponse);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property MimeTypesFile: string read ReadMimeTypesFile write WriteMimeTypesFile;
    property Router: TKyRoutes read fRouter write fRouter;
    property FileRoutes: TFileRouteMap read fFileRouter write fFileRouter;
  end;

  TKyServerClass = class of TKyServer;

  TKyServerThread = class(TThread)
  private
    fKyServer: TKyServerClass;
    fRouter: TKyRoutes;
  public
    constructor Create(AServer: TKyServerClass);
    destructor Destroy; override;
  published
    property Router: TKyRoutes read fRouter write fRouter;
  end;

  TKyServerComponents = class(TComponent)

  end;

implementation

procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

function DumpExceptionCallStack(E: Exception): string;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'EXCEPTION ERROR ' + LineEnding +
    'Stacktrace' + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result := Report + LineEnding +
    'Stacktrace';
  WriteLn(Report);
  //Halt; // End of program execution
end;

procedure TKyServer.WriteMimeTypesFile(AFileName: string);
begin
  if AFileName <> fMimeTypesFile then
  begin
    KMime.LoadFromFile(AFileName);
    fMimeTypesFile := AFileName;
  end;
end;

function TKyServer.ReadMimeTypesFile: string;
begin
  Result := fMimeTypesFile;
end;

procedure TKyServer.SendFile(Const AFileName : String; var AResponse : TFPHTTPConnectionResponse);
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

procedure TKyServer.KHandleRequest(Sender: TObject;var ARequest: TFPHTTPConnectionRequest;
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
      ModuleWorker := TKyModuleClass(Router[URIStr]).Create(Self, arequest, aResponse);
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
      AResponse := ModuleWorker.Response;
      FreeAndNil(ModuleWorker);
    end
    else if URIStr = '' then
    begin
      if Router.Contains('main') then
      begin
        ModuleWorker := TKyModuleClass(Router['main']).Create(Self, arequest, aResponse);

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
        AResponse := ModuleWorker.Response;
        FreeAndNil(ModuleWorker);
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
      ModuleWorker := TKyModuleClass(Router['main']).Create(Self, arequest, aResponse);
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
      AResponse := ModuleWorker.Response;
      FreeAndNil(ModuleWorker);
    end;
  except
    on E: Exception do
    begin
      AResponse.Code := 500;
      AResponse.Content := GetErrorInformation(ARequest.Host, ARequest.URL,
          DumpExceptionCallStack(E),
          StartServeTime);
    end;
  end;

end;

constructor TKyServer.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  fFileRouter := TFileRouteMap.create;
  KMime := TFPMimeTypes.Create(Self);
  OnRequest := @KHandleRequest;
end;

destructor TKyServer.Destroy;
begin
  FreeAndNil(KMime);
  FreeAndNil(fFileRouter);
  inherited Destroy;
end;

// KyServerThread

constructor TKyServerThread.Create(AServer: TKyServerClass);
begin

end;

destructor TKyServerThread.Destroy;
begin

end;

initialization

end.
