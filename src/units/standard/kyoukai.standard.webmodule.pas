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
  Classes, SysUtils, fphttpserver;

type

  TKyModule = class(TComponent)
  private
  var
    fRequest: TFPHTTPConnectionRequest;
    fResponse: TFPHTTPConnectionResponse;
  private
    fWebWritings: string;
  public
    destructor destroy; override;
    Constructor Create(AOwner: TComponent); override;
    procedure _echo(AMessage: String);
  published
    property WebWritings: string read fWebWritings write fWebWritings;
    property Request: TFPHTTPConnectionRequest read fRequest write fRequest;
    property Response: TFPHTTPConnectionResponse read fResponse write fResponse;
  end;

  TKyModuleClass = class of TKyModule;

implementation

procedure TKyModule._echo(AMessage: String);
begin
  Response.Content := Response.Content + AMessage;
end;

Constructor TKyModule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TKyModule.destroy;
begin
  inherited Destroy;
end;

end.

