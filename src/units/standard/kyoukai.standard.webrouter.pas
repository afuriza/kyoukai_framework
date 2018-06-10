{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Standard.WebRouter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Kyoukai.Other.STLUtil, Kyoukai.Standard.WebModule;

type

  TKyRoutes = class(TObject)
  private
    type
      TKyModuleClassMap = specialize TStringHashMap<TKyModuleClass>;
  private
    fKyModuleMap: TKyModuleClassMap;
    function GetURL(const AURL: string): TKyModuleClass;
    procedure SetURL(const AURL: string; AValue: TKyModuleClass);
  public
    function GetRouteAliasList: TStringList;
    function Contains(AKey: string): Boolean;
    constructor Create;
    destructor Destroy; override;
    property Routes[const AURL: String]: TKyModuleClass read GetURL write SetURL;default;
  end;

var
  Routes: TKyRoutes;
implementation

function TKyRoutes.GetRouteAliasList: TStringList;
begin
  //Result := TStringList.Create;
end;

function TKyRoutes.Contains(AKey: string): Boolean;
begin
  {$if fpc_fullversion >= 20701}
  Result := fKyModuleMap.contains(AKey);
  {$else fpc_fullversion >= 20701}
  Result := fKyModuleMap.IndexOf('main') >= 0;
  {$endif fpc_fullversion >= 20701}
end;

function TKyRoutes.GetURL(const AURL: string): TKyModuleClass;
begin
  Result := fKyModuleMap[AURL];
end;

procedure TKyRoutes.SetURL(const AURL: string; AValue: TKyModuleClass);
begin
  fKyModuleMap[AURL] := AValue;
end;

constructor TKyRoutes.Create;
begin
  fKyModuleMap := TKyModuleClassMap.create;
end;

destructor TKyRoutes.Destroy;
begin
  FreeAndNil(fKyModuleMap);
  inherited;
end;

end.

