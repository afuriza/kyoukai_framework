unit Kyoukai.DBLib.SQLBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TQueryBuilderClass = class of TQueryBuilder;

  TQueryBuilderType = (dqSelect, dqFrom, dqWhere, dqAnd);

  TQueryBuilder = class(TComponent)
  public
    PropVal: string;
    WorkerParent: TQueryBuilder;
    WorkerChild: TQueryBuilder;
    WorkerType: TQueryBuilderType;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Select(AValue: string): TQueryBuilder;
    function From(AValue: string): TQueryBuilder;
    function Where(AValue: string): TQueryBuilder;
    function Where(AExpr: string; AValue: string): TQueryBuilder;
    function Where(AExpr: string; AValue: integer): TQueryBuilder;
    function and_(AValue: string): TQueryBuilder;
    function and_(AExpr: string; AValue: string): TQueryBuilder;
    function and_(AExpr: string; AValue: integer): TQueryBuilder;
    function and_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
    function and_(AExpr: string; ADate: TDate): TQueryBuilder;
    function AsSQL: string;
  end;

implementation
var
  fQueryBuilder: TQueryBuilder;

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
  PropVal := AExpr + ' ''' + AValue + '''';
  WorkerType := dqWhere;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.Where(AExpr: string; AValue: integer): TQueryBuilder;
begin
  PropVal := AExpr + ' ' + AValue.ToString;
  WorkerType := dqWhere;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AValue: string): TQueryBuilder;
begin
  PropVal := AValue;
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; AValue: string): TQueryBuilder;
begin
  PropVal := AExpr + ' ''' + AValue + '''';
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; DummyObj: Tobject): TQueryBuilder;
begin
  DummyObj := nil;
  PropVal := AExpr + ' NULL';
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

function TQueryBuilder.and_(AExpr: string; AValue: integer): TQueryBuilder;
begin
  PropVal := AExpr + ' ' + AValue.ToString;
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
  PropVal := AExpr + ' ''' + DateToStr(ADate, fFM) + '''';
  WorkerType := dqAnd;
  Result := TQueryBuilder.Create(Self);
  Result.WorkerParent := Self;
end;

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
      dqSelect: Result := 'SELECT ' + CurrentWorker.PropVal + Result;
      dqFrom: Result := ' FROM ' + CurrentWorker.PropVal + Result;
      dqWhere: Result := ' WHERE ' + CurrentWorker.PropVal + Result;
      dqAnd: Result := ' AND ' + CurrentWorker.PropVal + Result;
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

