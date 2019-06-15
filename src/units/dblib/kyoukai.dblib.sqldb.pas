{*******************************************************************************
                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal
See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.
This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*******************************************************************************}
unit Kyoukai.DBLib.SQLDB; 

{$mode objfpc}{$H+}
//{$LinkLib libmysqlclient.so}

interface

uses
  Classes, SysUtils, sqldb, db,
  {connectors}
  mysql50conn, mysql51conn, mysql55conn, mysql56conn, mysql57conn, mssqlconn,
  sqlite3conn
  {/connectors};

type
  TKySQLConnection = record
    hostname: string;
    username: string;
    password: string;
    database: string;
  end;

  { Wrapper Class }

  TKySQL = class(TObject)
     SQLQuery: TSQLQuery;
     SQLConnection: TSQLConnection;
  private
     SQLTransaction: TSQLTransaction;
     fSQL: string;
     function GetEOF: Boolean;
     function GetFields: TFields;
     function GetFieldValues(FieldName: string): Variant;
  public
     constructor Create(const ConnectionType: string);
     destructor Destroy; override;
     procedure Connect(const hostname, uname, passwd, DatabaseName: string);
     procedure Connect(AKySQLConnection: TKySQLConnection);
     procedure Open;
     procedure Close;
     procedure Next;
     procedure ExecSQL(const AQuery: string);
     property EOF: Boolean read GetEOF;
     property Fields : TFields read GetFields;
     property Field[FieldName : string]: Variant read GetFieldValues;
     property SQL: string read fSQL write fSQL;
  end;

implementation

//procedure InitSQL(const AConnectionType, ALibName: string);
//begin
//  dblib.LibraryName:= ALibName;
//  dblib.ConnectionType:= AConnectionType;
//  dblib.Enabled:= true;
//  dblib.LoadLibrary;
//end;

constructor TKySQL.Create(const ConnectionType: string);
begin
  inherited Create;
  SQLQuery := TSQLQuery.Create(nil);
  case ConnectionType of
    'mysql50': SQLConnection := TMySQL50Connection.Create(nil);
    'mysql51': SQLConnection := TMySQL51Connection.Create(nil);
    'mysql55': SQLConnection := TMySQL55Connection.Create(nil);
    'mysql56': SQLConnection := TMySQL56Connection.Create(nil);
    'mysql57': SQLConnection := TMySQL57Connection.Create(nil);
    'mysql': SQLConnection := TMySQL56Connection.Create(nil);
    'sqlserver': SQLConnection := TMSSQLConnection.Create(nil);
    'sqlite3': SQLConnection := TSQLite3Connection.Create(nil);
  end;
  {$IFDEF ANDROID}
  TConnectionName(SQLConnection).SkipLibraryVersionCheck := True;
  {$ENDIF}
  SQLTransaction := TSQLTransaction.Create(nil);
  SQLQuery.SQLConnection := SQLConnection;
  SQLTransaction.DataBase := SQLConnection;
  SQLQuery.Transaction := SQLTransaction;
end;

destructor TKySQL.Destroy;
begin

  SQLConnection.Close();
  SQLQuery.Free;
  SQLConnection.Free;
  SQLTransaction.Free;

  inherited Destroy;
end;

procedure TKySQL.ExecSQL(const AQuery: string);
begin
  SQLQuery.SQL.Text := AQuery;
  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
end;

procedure TKySQL.Open;
begin
  SQLQuery.SQL.Text := fSQL;
  SQLQuery.Open;
end;

procedure TKySQL.Close;
begin
  SQLQuery.Close;
end;

function TKySQL.GetEOF: Boolean;
begin
  Result := SQLQuery.EOF;
end;

procedure TKySQL.Next;
begin
  SQLQuery.Next;
end;

function TKySQL.GetFields: TFields;
begin
  Result := SQLQuery.Fields;
end;

function TKySQL.GetFieldValues(FieldName: string): Variant;
begin
  if not SQLQuery.Fields.FieldByName(FieldName).IsNull then
    Result := SQLQuery.Fields.FieldByName(FieldName).AsString;
end;

procedure TKySQL.Connect(const hostname, uname, passwd, DatabaseName: string);
var
  IsChanged: Boolean = False;
begin
  if (Hostname <> SQLConnection.HostName) or
    (DatabaseName <> SQLConnection.DatabaseName) then
    IsChanged := True;
  SQLConnection.HostName := hostname;
  SQLConnection.UserName := uname;
  SQLConnection.Password := passwd;
  SQLConnection.DatabaseName := DatabaseName;
  if not SQLConnection.Connected then
    SQLConnection.Connected:=True
  else
  begin
    if isChanged then
    begin
      SQLConnection.Connected := False;
      SQLConnection.Connected := True;
    end;
  end;
end;

procedure TKySQL.Connect(AKySQLConnection: TKySQLConnection);
var
  IsChanged: Boolean = False;
begin
  if (AKySQLConnection.hostname <> SQLConnection.HostName) or
    (AKySQLConnection.database <> SQLConnection.DatabaseName) then
    IsChanged := True;
  SQLConnection.HostName := AKySQLConnection.hostname;
  SQLConnection.UserName := AKySQLConnection.username;
  SQLConnection.Password := AKySQLConnection.password;
  SQLConnection.DatabaseName := AKySQLConnection.database;
  if not SQLConnection.Connected then
    SQLConnection.Connected:=True
  else
  begin
    if isChanged then
    begin
      SQLConnection.Connected := False;
      SQLConnection.Connected := True;
    end;
  end;
end;

end.

                                        
