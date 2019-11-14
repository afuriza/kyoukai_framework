{*******************************************************************************
                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal
See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*******************************************************************************}
unit Kyoukai.DBLib.SQLBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TQueryBuilderType = (dqSelect, dqFrom, dqWhere, dqAnd, dqOr,
    dqLimit, dqOffset);

  TQueryBuilder = class(TComponent)
  public
    PropVal: string;
    WorkerParent: TQueryBuilder;
    WorkerChild: TQueryBuilder;
    WorkerType: TQueryBuilderType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function SELECT(AValue: string): TQueryBuilder;
    function FROM(AValue: string): TQueryBuilder;
    function WHERE(AValue: string): TQueryBuilder;
    function WHERE(AExpr: string; AValue: string): TQueryBuilder;
    function WHERE(AExpr: string; AValue: integer): TQueryBuilder;
    function AND_(AValue: string): TQueryBuilder;
    function AND_(AExpr: string; AValue: string): TQueryBuilder;
    function AND_(AExpr: string; AValue: integer): TQueryBuilder;
    function AND_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
    function AND_(AExpr: string; ADate: TDate): TQueryBuilder;
    function OR_(AValue: string): TQueryBuilder;
    function OR_(AExpr: string; AValue: string): TQueryBuilder;
    function OR_(AExpr: string; AValue: integer): TQueryBuilder;
    function OR_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
    function OR_(AExpr: string; ADate: TDate): TQueryBuilder;
    function LIMIT(AValue: integer): TQueryBuilder;
    function OFFSET(AValue: integer): TQueryBuilder;
    function AsSQL: string;
  end;

  function QueryBuilder: TQueryBuilder;

implementation
var
  fQueryBuilder: TQueryBuilder;

function SetBracket(AExpr, AValue: string; IsNotStr: boolean = True): string;
begin
  if IsNotStr then
    Result :=  ' (' + AExpr + ' ' + AValue + ')'
  else
    Result :=  ' (' + AExpr + ' ''' + AValue + ''')';
end;

function QueryBuilder: TQueryBuilder;
begin
  if Assigned(fQueryBuilder) then
    fQueryBuilder.Free;
  fQueryBuilder := TQueryBuilder.Create(nil);
  Result := fQueryBuilder;
end;

function TQueryBuilder.Select(AValue: string): TQueryBuilder;
begin
  PropVal := AValue;
  WorkerType := dqSelect;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
  Self.WorkerChild := Result;
end;

function TQueryBuilder.From(AValue: string): TQueryBuilder;
begin
  WorkerType := dqFrom;
  PropVal := AValue;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
  Self.WorkerChild := Result;
end;

function TQueryBuilder.Where(AValue: string): TQueryBuilder;
begin
  PropVal := AValue;
  WorkerType := dqWhere;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.Where(AExpr: string; AValue: string): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue, False);
  WorkerType := dqWhere;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.Where(AExpr: string; AValue: integer): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue.ToString);
  WorkerType := dqWhere;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

{%region /fold 'and section'}

function TQueryBuilder.and_(AValue: string): TQueryBuilder;
begin
  PropVal := AValue;
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; AValue: string): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue, False);
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
begin
  DummyObj := nil;
  PropVal := SetBracket(AExpr, 'NULL');
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; AValue: integer): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue.ToString);
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; ADate: TDate): TQueryBuilder;
var
  fFM: TFormatSettings;
begin
  fFM := FormatSettings;
  fFM.DateSeparator := '-';
  fFM.ShortDateFormat := 'YYYY-MM-DD';
  PropVal := SetBracket(AExpr, DateToStr(ADate, fFM), False);
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

{%endregion}

{%region /fold 'or section'}

function TQueryBuilder.LIMIT(AValue: integer): TQueryBuilder;
begin
  PropVal := ' LIMIT '+AValue.ToString;
  WorkerType := dqLimit;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.OFFSET(AValue: integer): TQueryBuilder;
begin
  PropVal := ' OFFSET '+AValue.ToString;
  WorkerType := dqOffset;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.or_(AValue: string): TQueryBuilder;
begin
  PropVal := AValue;
  WorkerType := dqOr;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.or_(AExpr: string; AValue: string): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue, False);
  WorkerType := dqOr;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.or_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
begin
  DummyObj := nil;
  PropVal := SetBracket(AExpr, 'NULL');
  WorkerType := dqOr;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.or_(AExpr: string; AValue: integer): TQueryBuilder;
begin
  PropVal := SetBracket(AExpr, AValue.ToString);
  WorkerType := dqOr;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.or_(AExpr: string; ADate: TDate): TQueryBuilder;
var
  fFM: TFormatSettings;
begin
  fFM := FormatSettings;
  fFM.DateSeparator := '-';
  fFM.ShortDateFormat := 'YYYY-MM-DD';
  PropVal := SetBracket(AExpr, DateToStr(ADate, fFM), False);
  WorkerType := dqOr;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

{%endregion}

function TQueryBuilder.AsSQL: string;
var
  CurrentWorker: TQueryBuilder;
begin
  Result := '';
  CurrentWorker := Self;
  while CurrentWorker.WorkerParent <> nil do
  begin
    CurrentWorker := CurrentWorker.WorkerParent;
    CASE CurrentWorker.WorkerType OF
      dqSelect:
      begin
        if CurrentWorker.WorkerParent = nil then
          Result := 'SELECT ' + CurrentWorker.PropVal + Result
        else
          Result := ' SELECT ' + CurrentWorker.PropVal + Result;
      end;
      dqFrom: Result := ' FROM' + CurrentWorker.PropVal + Result;
      dqWhere: Result := ' WHERE' + CurrentWorker.PropVal + Result;
      dqAnd: Result := ' AND' + CurrentWorker.PropVal + Result;
      dqOr: Result := ' OR' + CurrentWorker.PropVal + Result;
      dqOffset: Result := CurrentWorker.PropVal + Result;
      dqLimit: Result := CurrentWorker.PropVal + Result;
    end;

  end;
end;



constructor TQueryBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TQueryBuilder.Destroy;
begin
  inherited Destroy;
end;

end.

