unit Kyoukai.DBLib.SQLDB;

{$mode objfpc}{$H+}
//{$LinkLib libmysqlclient.so}

interface

uses
  Classes, SysUtils, sqldb, db, sqldblib, mysql50conn, mysql51conn, mysql55conn,
  mysql56conn, mysql57conn, mssqlconn, sqlite3conn;

type
  TKySQLConnection = record
    hostname: string;
    username: string;
    password: string;
    database: string;
  end;

  TKySQL = class(TObject)
     SQLQuery: TSQLQuery;
  private
     MySQLConnection: TSQLConnection;
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
    'mysql50': MySQLConnection := TMySQL50Connection.Create(nil);
    'mysql51': MySQLConnection := TMySQL51Connection.Create(nil);
    'mysql55': MySQLConnection := TMySQL55Connection.Create(nil);
    'mysql56': MySQLConnection := TMySQL56Connection.Create(nil);
    'mysql57': MySQLConnection := TMySQL57Connection.Create(nil);
    'mysql': MySQLConnection := TMySQL56Connection.Create(nil);
    'sqlserver': MySQLConnection := TMSSQLConnection.Create(nil);
    'sqlite3': MySQLConnection := TSQLite3Connection.Create(nil);
  end;

  SQLTransaction := TSQLTransaction.Create(nil);
  SQLQuery.SQLConnection := MySQLConnection;
  SQLTransaction.DataBase := MySQLConnection;
  SQLQuery.Transaction := SQLTransaction;
end;

destructor TKySQL.Destroy;
begin
  
  MySQLConnection.Close();
  SQLQuery.Free;
  MySQLConnection.Free;
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
  Result := SQLQuery.Fields.FieldByName(FieldName).AsString;
end;

procedure TKySQL.Connect(const hostname, uname, passwd, DatabaseName: string);
begin
  MySQLConnection.HostName := hostname;
  MySQLConnection.UserName := uname;
  MySQLConnection.Password := passwd;
  MySQLConnection.DatabaseName := DatabaseName;
  if not MySQLConnection.Connected then
    MySQLConnection.Connected:=True;
end;

procedure TKySQL.Connect(AKySQLConnection: TKySQLConnection);
begin
  MySQLConnection.HostName := AKySQLConnection.hostname;
  MySQLConnection.UserName := AKySQLConnection.username;
  MySQLConnection.Password := AKySQLConnection.password;
  MySQLConnection.DatabaseName := AKySQLConnection.database;
  if not MySQLConnection.Connected then
    MySQLConnection.Connected:=True;
end;

end.


