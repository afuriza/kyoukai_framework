{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.WebRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, fgl,
  Kyoukai.Core.STLUtil,
  Kyoukai.Core.CommonUtil,
  Kyoukai.Base.Controller;

type

  TKyFileRoutes = specialize TStringHashMap<string>;

  TRouteVarMap = specialize TFPGMap<string, string>;

  TRouteValues = class(TObject)
    Methods: TStringList;
    Handler: string;
    RouteStr: string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TRoutesValuesMap = specialize TFPGMap<string, TRouteValues>;

  TKyoukaiRouteHandler = class(TObject)
  private
    RouteValuesMap: TRoutesValuesMap;
  public
    RouteVarMap: TRouteVarMap;
    procedure Match(ARoute: string; AMethods: array of string;
      AHandler: string);
    function GetRouteMatch(AURL: string): TRouteValues;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Route: TKyoukaiRouteHandler;
  FileRoute: TKyFileRoutes;
implementation

constructor TRouteValues.Create;
begin
  inherited Create;
  Methods := TStringList.Create;
end;

destructor TRouteValues.Destroy;
begin
  FreeAndNil(Methods);
  inherited Destroy;
end;

{ Route Handler }

function RouteToPattern(ARoute: string): string;
begin
  Result := Format('^(%s)(\?[a-zA-Z0-9]+=[a-zA-Z0-9]+)?$', [ARoute]);
  Result := ReplaceRegExpr('<[a-zA-Z0-9_]+>', Result, '[a-zA-Z0-9_]+', True);
end;

function RoutePatternMatches(CurrentRoute: string;
  DefinedRoute: string): boolean;
var
  regexobj: TRegExpr;
  RoutePattern: string;
begin
  RoutePattern := RouteToPattern(DefinedRoute);
  regexobj := TRegExpr.Create(RoutePattern);
  Result := regexobj.Exec(CurrentRoute);
  regexobj.Free;
end;

function TKyoukaiRouteHandler.GetRouteMatch(AURL: string): TRouteValues;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to RouteValuesMap.Count -1 do
  begin
    if RoutePatternMatches(AURL, RouteValuesMap.Keys[i]) then
    begin
      Result := RouteValuesMap.Data[i];
    end;
  end;
end;

constructor TKyoukaiRouteHandler.Create;
begin
  inherited Create;
  RouteValuesMap := TRoutesValuesMap.Create;
end;

destructor TKyoukaiRouteHandler.Destroy;
begin
  FreeAndNil(RouteValuesMap);
  inherited Destroy;
end;

procedure TKyoukaiRouteHandler.Match(ARoute: string; AMethods: array of string;
  AHandler: string);
var
  RouteValues: TRouteValues;
  Str: String;
begin
  RouteValues := TRouteValues.Create;
  for Str in AMethods do
  begin
    RouteValues.Methods.Add(Str);
  end;
  RouteValues.Handler := AHandler;
  RouteValues.RouteStr := ARoute;
  if ARoute = '' then
    RouteValuesMap.Add('/', RouteValues)
  else
    RouteValuesMap.Add(ARoute, RouteValues);
end;

initialization
  Route := TKyoukaiRouteHandler.Create;
  FileRoute := TKyFileRoutes.create;

finalization
  FreeAndNil(Route);
  FreeAndNil(FileRoute);

end.

