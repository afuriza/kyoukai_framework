{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.Controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fphttp, TypInfo,
  Kyoukai.Core.STLUtil,
  Kyoukai.Base.WebSession,
  Kyoukai.Base.WebView,
  Kyoukai.Core.KTemplate,
  Kyoukai.Base.CGIDefs;

type

  TController = class(TComponent)
  private
    HandleMethod: Boolean;
    fcgirootpath: string;

    fParams: TStringList;
    fRequest: TRequest;
    fResponse: TResponse;
    fSession: TSessionController;
    fWebWritings: string;
    fErrorMsg: string;
    function ReadGetVar(const AVarName: string): string;
    function ReadPostVar(const AVarName: string): string;
    function ReadSessionVar(const AVarName: string): string;
    procedure WriteSessionVar(const AVarName, AVarValue: string);
  public
    procedure Redirect(const ALocation: string);
    destructor Destroy; override;
    constructor Create(AOwner: TComponent); override;
    Constructor Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse); reintroduce; overload;
    Constructor Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse; ErrorStr: string); reintroduce;
    procedure echo(const AMessage: String);
    procedure Render(ATemplate: TKTemplate);
    procedure Render(AView: TWebView);
    procedure Render(AView: TWebView; AGlobalViewName: String);
    procedure StartSession;
    procedure TerminateSession;
    property InputGet[const AVarName: string]: string read ReadGetVar;
    property InputPost[const AVarName: string]: string read ReadPostVar;
    property SessionVar[const ASessionName: string]: string read ReadSessionVar
      write WriteSessionVar;
    property _get[const AVarName: string]: string read ReadGetVar;
    property _post[const AVarName: string]: string read ReadPostVar;
    property _session[const ASessionName: string]: string read ReadSessionVar
      write WriteSessionVar;
  published
    property ErrorString: string read fErrorMsg;
    property Session: TSessionController read fSession write fSession;
    property Params: TStringList read fParams write fParams;
    property WebWritings: string read fWebWritings write fWebWritings;
    property Request: TRequest read fRequest write fRequest;
    property Response: TResponse read fResponse write fResponse;
    property Handled: Boolean read HandleMethod write HandleMethod;
    property MainPath: string read fCGIROOTPath;
  end;

  TControllerClass = class of TController;

  TControllerList = class(TObject)
  private
    type
      TControllerClassMap = specialize TStringHashMap<TControllerClass>;
  private
    ControllerMap: TControllerClassMap;
    function GetURL(const AURL: string): TControllerClass;
    procedure SetURL(const AURL: string; AValue: TControllerClass);
  public
    class function MakeOther: TControllerList;
    function GetRouteAliasList: TStringList;
    function Contains(AKey: string): Boolean;
    constructor Create;
    destructor Destroy; override;
    property Routes[const AURL: String]: TControllerClass read GetURL write SetURL;default;
  end;

  TControllerListClass = class of TControllerList;

var
  Controllers: TControllerList;
implementation

procedure TController.StartSession;
begin
  if not Assigned(fSession) then
    fSession := TSessionController.Create(fRequest);
  fSession.StartSession;
end;

procedure TController.TerminateSession;
begin
  fSession.Terminate;
end;

function TController.ReadSessionVar(const AVarName: string): string;
begin
  if Assigned(fSession) then
  Result := fSession.Values[AVarName];
end;

procedure TController.WriteSessionVar(const AVarName, AVarValue: string);
begin
  if Assigned(fSession) then
  fSession.Values[AVarName] := AVarValue;
end;

procedure TController.Redirect(const ALocation: string);
begin
  Response.SendRedirect(ALocation);
end;

procedure TController.Render(ATemplate: TKTemplate);
begin
  Response.Contents.Text := ATemplate.GetContent;
end;

procedure TController.Render(AView: TWebView; AGlobalViewName: String);
begin
  Response.Content := Response.Content + AView.GetContent(AGlobalViewName);
end;

procedure TController.Render(AView: TWebView);
begin
  Response.Content := Response.Content + AView.GetContent;
end;

procedure TController.echo(const AMessage: String);
begin
  Response.Content := Response.Content + AMessage;
end;

function TController.ReadGetVar(const AVarName: string): string;
begin
  Result := Request.QueryFields.Values[AVarName];
end;

function TController.ReadPostVar(const AVarName: string): string;
begin
  Result := Request.ContentFields.Values[AVarName];
end;

Constructor TController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fcgirootpath := CGIROOTPath;
  fParams := TStringList.Create;
end;

Constructor TController.Create(AOwner: TComponent;
  aRequest: TRequest; aResponse: TResponse);
begin
  inherited Create(AOwner);
  fRequest := ARequest;
  fResponse := AResponse;
  fcgirootpath := CGIROOTPath;
  fParams := TStringList.Create;
end;

Constructor TController.Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse; ErrorStr: string);
begin
  inherited Create(AOwner);
  fRequest := ARequest;
  fResponse := AResponse;
  fErrorMsg := ErrorStr;
  fcgirootpath := CGIROOTPath;
  fParams := TStringList.Create;
end;

destructor TController.Destroy;
begin
  if Assigned(fSession) then
    FreeAndNil(fSession);
  FreeAndNil(fParams);
  inherited Destroy;
end;


{ Controller List }

class function TControllerList.MakeOther: TControllerList;
begin
  Result := Create;
end;

function TControllerList.GetRouteAliasList: TStringList;
begin
  //Result := TStringList.Create;
end;

function TControllerList.Contains(AKey: string): Boolean;
begin
  {$if fpc_fullversion >= 20701}
  Result := ControllerMap.contains(AKey);
  {$else fpc_fullversion >= 20701}
  Result := ControllerMap.IndexOf('main') >= 0;
  {$endif fpc_fullversion >= 20701}
end;

function TControllerList.GetURL(const AURL: string): TControllerClass;
begin
  Result := ControllerMap[AURL];
end;

procedure TControllerList.SetURL(const AURL: string; AValue: TControllerClass);
begin
  ControllerMap[AURL] := AValue;
end;

constructor TControllerList.Create;
begin
  ControllerMap := TControllerClassMap.create;
end;

destructor TControllerList.Destroy;
begin
  FreeAndNil(ControllerMap);
  inherited;
end;

initialization

Controllers := TControllerList.Create;

finalization

FreeAndNil(Controllers);

end.
