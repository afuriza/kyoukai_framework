{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Standard.WebModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fphttp, TypInfo,
  Kyoukai.Standard.WebSession,
  Kyoukai.Standard.WebView,
  Kyoukai.Other.KTemplate,
  Kyoukai.Standard.CGIDefs;

type

  TKyModule = class(TComponent)
  private
    HandleMethod: Boolean;
    fcgirootpath: string;

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
    procedure redirect(const ALocation: string);
    destructor destroy; override;
    Constructor Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse); reintroduce; overload;
    Constructor Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse; ErrorStr: string); reintroduce; overload;
    procedure echo(const AMessage: String);
    procedure Render(ATemplate: TKTemplate);
    procedure Render(AView: TKyView);
    procedure Render(AView: TKyView; AGlobalViewName: String);
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
    property WebWritings: string read fWebWritings write fWebWritings;
    property Request: TRequest read fRequest write fRequest;
    property Response: TResponse read fResponse write fResponse;
    property Handled: Boolean read HandleMethod write HandleMethod;
    property MainPath: string read fCGIROOTPath;
  end;

  TKyModuleClass = class of TKyModule;

implementation

procedure TKyModule.StartSession;
begin
  fSession := TSessionController.Create(fRequest);
  fSession.StartSession;
end;

procedure TKyModule.TerminateSession;
begin
  fSession.Terminate;
end;

function TKyModule.ReadSessionVar(const AVarName: string): string;
begin
  if Assigned(fSession) then
  Result := fSession.Values[AVarName];
end;

procedure TKyModule.WriteSessionVar(const AVarName, AVarValue: string);
begin
  if Assigned(fSession) then
  fSession.Values[AVarName] := AVarValue;
end;

procedure TKyModule.redirect(const ALocation: string);
begin
  Response.SendRedirect(ALocation);
end;

procedure TKyModule.Render(ATemplate: TKTemplate);
begin
  Response.Contents.Text := ATemplate.GetContent;
end;

procedure TKyModule.Render(AView: TKyView; AGlobalViewName: String);
begin
  Response.Content := Response.Content + AView.GetContent(AGlobalViewName);
end;

procedure TKyModule.Render(AView: TKyView);
begin
  Response.Content := Response.Content + AView.GetContent;
end;

procedure TKyModule.echo(const AMessage: String);
begin
  Response.Content := Response.Content + AMessage;
end;

function TKyModule.ReadGetVar(const AVarName: string): string;
begin
  Result := Request.QueryFields.Values[AVarName];
end;

function TKyModule.ReadPostVar(const AVarName: string): string;
begin
  Result := Request.ContentFields.Values[AVarName];
end;

Constructor TKyModule.Create(AOwner: TComponent;
  aRequest: TRequest; aResponse: TResponse);
begin
  inherited Create(AOwner);
  fRequest := ARequest;
  fResponse := AResponse;
  fcgirootpath := CGIROOTPath;
end;

Constructor TKyModule.Create(AOwner: TComponent; aRequest: TRequest;
      aResponse: TResponse; ErrorStr: string);
begin
  inherited Create(AOwner);
  fRequest := ARequest;
  fResponse := AResponse;
  fErrorMsg := ErrorStr;
end;

destructor TKyModule.destroy;
begin
  if Assigned(fSession) then
    FreeAndNil(fSession);
  inherited Destroy;
end;

end.
