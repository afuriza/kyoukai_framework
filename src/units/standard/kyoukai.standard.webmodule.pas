{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Standard.WebModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpserver, httpdefs, fphttp,
  Kyoukai.Standard.WebSession,
  Kyoukai.Standard.WebView,
  Kyoukai.Other.KTemplate;

type

  TKyModule = class(TComponent)
  private
    type
      TConstructCallback = procedure of object;
  var
    fRequest: TFPHTTPConnectionRequest;
    fResponse: TFPHTTPConnectionResponse;
  private
    fSession: TSessionController;
    fWebWritings: string;
    function ReadGetVar(const AVarName: string): string;
    function ReadPostVar(const AVarName: string): string;
    function ReadSessionVar(const AVarName: string): string;
    procedure WriteSessionVar(const AVarName, AVarValue: string);
  public
    procedure redirect(const ALocation: string);
    destructor destroy; override;
    Constructor Create(AOwner: TComponent; aRequest: TFPHTTPConnectionRequest;
      aResponse: TFPHTTPConnectionResponse);
    procedure _echo(AMessage: String);
    procedure Render(ATemplate: TKTemplate);
    procedure Render(AView: TKyView);
    procedure StartSession;
    procedure TerminateSession;
    property _get[const AVarName: string]: string read ReadGetVar;
    property _post[const AVarName: string]: string read ReadPostVar;
    property _session[const ASessionName: string]: string read ReadSessionVar
      write WriteSessionVar;
  published
    property Session: TSessionController read fSession write fSession;
    property WebWritings: string read fWebWritings write fWebWritings;
    property Request: TFPHTTPConnectionRequest read fRequest write fRequest;
    property Response: TFPHTTPConnectionResponse read fResponse write fResponse;
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

procedure TKyModule.Render(AView: TKyView);
begin
  Response.Contents.Text := AView.GetContent;
end;

procedure TKyModule._echo(AMessage: String);
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
  aRequest: TFPHTTPConnectionRequest; aResponse: TFPHTTPConnectionResponse);
var
  CallFunc: TConstructCallback;
begin
  inherited Create(AOwner);
  fRequest := ARequest;
  fResponse := AResponse;
  if Self.MethodAddress('_prepare') <> nil then
  begin
    TMethod(CallFunc).Code := Self.MethodAddress('_prepare');
    TMethod(CallFunc).Data := Self;
    CallFunc;
  end;
end;

destructor TKyModule.destroy;
var
  CallFunc: TConstructCallback;
begin
  if Self.MethodAddress('_done') <> nil then
  begin
    TMethod(CallFunc).Code := Self.MethodAddress('_done');
    TMethod(CallFunc).Data := Self;
    CallFunc;
  end;
  if Assigned(fSession) then
  FreeAndNil(fSession);
  inherited Destroy;
end;

end.

