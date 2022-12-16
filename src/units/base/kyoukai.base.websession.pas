{*******************************************************************************

                          This is Part of Kyoukai units
                       The Simple Web Template for Pascal

Based on Session_Controller from Luri Darmawan

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.WebSession;

{$mode objfpc}{$H+}

interface

uses
  fpcgi, md5, IniFiles, fpjson, fphttpserver, httpdefs,
  Classes, SysUtils;

const
  MaxIniCreate = 5;
  _SESSION_SESSION = 'session';
  _SESSION_DATA = 'data';
  _SESSION_ACTIVE = 'active';         // Start time of session
  _SESSION_KEYSTART = 'start';         // Start time of session
  _SESSION_KEYLAST = 'last';          // Last seen time of session
  _SESSION_IPADDRESS = 'ipaddr';
  _SESSION_FLASHMESSAGE = 'flash';
  _SESSION_TIMEOUT_DEFAULT = 3600;
  TDateTimeEpsilon = 2.2204460493e-16;

type

  { TSessionController }

  TSessionController = class(TObject)
  private
    fRequest: TRequest;
    fResponse: TResponse;
    FIniFile: TMemInifile;
    FLastAccess: TDateTime;
    FSessionTimeout: integer;
    FSessionStarted, FSessionTerminated, FCached: boolean;
    FSessionPrefix, FSessionSuffix, FSessionExtension, FHttpCookie,
    FCookieID, FSessionID: string;
    FSessionDir: string;
    FSessionVars: TStringList;
    function GenerateSesionID: string;
    function CreateIniFile(const FileName: string): TMemIniFile;
    procedure DeleteIniFile;
    function GetInterval: double;
    function GetIsExpired: boolean;
    function GetTimeOut: integer;
    function GetValue(variable: string): string;
    procedure SetSessionDir(AValue: string);
    procedure SetTimeOut(AValue: integer);
    procedure SetValue(variable: string; AValue: string);
    procedure UpdateIniFile;

    function c(SourceString: string): string;
    function d(SourceString: string): string;

    function StringVarToJson(Source: string): string;
    function JsonToStringVar(JSON: string): string;

    function StartSessionWithFile: boolean;
  public
    Storage: integer;
    constructor Create(ARequest: TRequest);
    destructor Destroy; override;
    property Values[variable: string]: string read GetValue write SetValue; default;
    property CookieID: string read FCookieID;
    property SessionID: string read FSessionID;
    property SessionDir: string read FSessionDir write SetSessionDir;
    property TimeOut: integer read GetTimeOut write SetTimeOut;
    property LastAccess: TDateTime read FLastAccess;

    property Interval: double read GetInterval;
    property IsExpired: boolean read GetIsExpired;
    property IsStarted: boolean read FSessionStarted;
    property IsTerminated: boolean read FSessionTerminated;

    function StartSession: boolean;
    procedure Clear;
    procedure DeleteKey(const Key: string);
    procedure EndSession(const Force: boolean = True);
    procedure Terminate;
    procedure ForceUpdate;

    function ReadDateTime(const variable: string): TDateTime;
    function ReadInteger(const variable: string): integer;

    function _DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
    function GetData(): string;
  public
    property Request: TRequest read fRequest;
    property Response: TResponse read fResponse;
  end;



implementation

function AppendPathDelim(const Path: string): string;
begin
  if (Path <> '') and not (Path[length(Path)] in AllowDirectorySeparators) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

function DirectoryIsWritable(const DirectoryName: string): boolean;
var
  TempFilename: string;
  s: string;
  fHandle: THANDLE;
begin
  Result := False;
  TempFilename :=
    SysUtils.GetTempFilename(AppendPathDelim(DirectoryName), 'ztstperm');
  fHandle := FileCreate(TempFilename);
  if (THandle(fHandle) <> feInvalidHandle) then
  begin
    s := 'WriteTest';
    if FileWrite(fHandle, S[1], Length(S)) > 0 then
      Result := True;
    FileClose(fHandle);
    DeleteFile(TempFilename);
  end;
end;

{ TSessionController }

function TSessionController.GenerateSesionID: string;
begin
  Result := fRequest.RemoteAddr + '-' +
    FCookieID + '-' + fRequest.UserAgent;
  Result := FSessionPrefix + MD5Print(MD5String(Result)) + '-' +
    FCookieID + FSessionSuffix;
end;

function TSessionController.CreateIniFile(const FileName: string): TMemIniFile;
var
  Count: integer;
begin
  Count := 0;
  Result := nil;
  repeat
    Inc(Count);
    try
      Result := TMemIniFile.Create(FileName, False);
    except
      On E: EFCreateError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until (Result <> nil);
end;

procedure TSessionController.DeleteIniFile;
begin
  try
    if DeleteFile(FSessionDir + FSessionID + FSessionExtension) then
    begin
    end;
  except
    on e: Exception do
    begin
    end;
  end;
end;

function TSessionController.GetInterval: double;
begin
  Result := (((Now - LastAccess) + TDateTimeEpsilon) * SecsPerDay);
  if Result < 0 then
    Result := 0;
end;


function TSessionController.GetIsExpired: boolean;
begin
  Result := False;
  if FSessionTimeout = 0 then
    Exit;
  if (Interval > FSessionTimeout) then
  begin
    Clear;
    Result := True;
  end;
end;

function TSessionController.GetTimeOut: integer;
begin
  Result := FSessionTimeout;
end;

function TSessionController.GetValue(variable: string): string;
begin
  Result := '';
  if (not FSessionStarted) or (FSessionTerminated) then
    Exit;


  if (FIniFile = nil) then
    Exit;
  Result := FIniFile.ReadString(_SESSION_DATA, variable, '');

end;

procedure TSessionController.SetSessionDir(AValue: string);
begin
  if FSessionDir = AValue then
    Exit;
  //if not DirectoryExists(AValue) then Exit;
  FSessionDir := IncludeTrailingPathDelimiter(AValue);
  try
    ForceDirectories(FSessionDir);
  except
  end;
end;

procedure TSessionController.SetTimeOut(AValue: integer);
begin
  FSessionTimeout := AValue;
end;

procedure TSessionController.SetValue(variable: string; AValue: string);
begin
  if (not FSessionStarted) or (FSessionTerminated) then
    Exit;

  if (FIniFile = nil) then
    Exit;
  try
    FIniFile.WriteString(_SESSION_DATA, variable, AValue);
    UpdateIniFile;
  except
    on E: Exception do
    begin
      //pr('session:' + e.Message);
      //die;
    end;
  end;

end;

procedure TSessionController.UpdateIniFile;
var
  Count: integer;
  OK: boolean;
begin
  Count := 0;
  OK := False;
  repeat
    Inc(Count);
    try
      TMemIniFile(FIniFile).UpdateFile;
      OK := True;
    except
      On E: EFCreateError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: EFOpenError do
      begin
        if Count > MaxIniCreate then
          raise;
        Sleep(20);
      end;
      On E: Exception do
        raise;
    end;
  until OK;
end;

// prepare for encode var session
function TSessionController.c(SourceString: string): string;
begin
  Result := SourceString;
end;

// prepare for decode var session
function TSessionController.d(SourceString: string): string;
begin
  Result := SourceString;
end;

function TSessionController.StringVarToJson(Source: string): string;
var
  str: TStringList;
  i: integer;
  sessObj: TJSONObject;
  items: TJSONObject;
begin
  sessObj := TJSONObject.Create;
  items := TJSONObject.Create;
  str := TStringList.Create;
  str.Text := Source;

  for i := 0 to str.Count - 1 do
  begin
    items.Add(str.Names[i], str.ValueFromIndex[i]);
  end;
  sessObj.Add('data', items);
  Result := (sessObj.AsJSON);
  FreeAndNil(sessObj);
  FreeAndNil(str);
end;

function TSessionController.JsonToStringVar(JSON: string): string;
begin
  Result := '';

end;

function TSessionController._DateTimeDiff(const ANow, AThen: TDateTime): TDateTime;
begin
  Result := ANow - AThen;
  if (ANow > 0) and (AThen < 0) then
    Result := Result - 0.5
  else if (ANow < -1.0) and (AThen > -1.0) then
    Result := Result + 0.5;
end;

function TSessionController.GetData: string;
var
  lst: TStringList;
begin

  lst := TStringList.Create;
  FIniFile.GetStrings(lst);
  Result := lst.Text;
  lst.Free;

end;

function Explode(Str, Delimiter: string): TStrings;
var
  i: integer;
  p: string;
begin
  Result := TStringList.Create;
  while Pos(Delimiter, Str) <> 0 do
  begin
    p := '';
    for i := 1 to Pos(Delimiter, Str) - 1 do
      p := p + Str[i];
    Result.Add(p);
    //Delete(s,1,Pos(Delimiter,Str));
    Delete(Str, 1, Pos(Delimiter, Str) + Length(Delimiter) - 1);
  end;
  //result.Add(s);
  if (Length(Str) <> 0) then
    Result.Add(Str);
end;

constructor TSessionController.Create(ARequest: TRequest);
var
  lstr: TStrings;
begin
  inherited Create();
  fRequest := ARequest;
  FSessionVars := TStringList.Create;
  FLastAccess := 0;
  FHttpCookie := fRequest.Cookie;
  FHttpCookie := StringReplace(FHttpCookie, ' ', '', [rfReplaceAll]);
  //FCookieID := Copy(FHttpCookie, Pos('__cfduid=', FHttpCookie) + 9,
  //  Length(FHttpCookie) - Pos('__cfduid=', FSessionID) - 9);
  lstr := Explode(FHttpCookie, ';');
  FCookieID := lstr.Values['__cfduid'];
  if FCookieID = '' then
    FCookieID := MD5Print(MD5String(FHttpCookie));
  FreeAndNil(lstr);
  FSessionID := GenerateSesionID();
  FSessionDir := ExtractFilePath(ParamStr(0)) + 'kyoukai_temp/session';
  FSessionExtension := '.ses';
  FSessionStarted := False;
  FSessionTerminated := False;
  FCached := False;
  FSessionTimeout := _SESSION_TIMEOUT_DEFAULT;
end;

destructor TSessionController.Destroy;
begin
  inherited Destroy;
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  FreeAndNil(FSessionVars);
end;

function TSessionController.StartSession: boolean;
begin
  Result := False;
  if FSessionStarted then
    Exit;
  StartSessionWithFile;
  Result := True;
end;

function TSessionController.StartSessionWithFile: boolean;
begin
  Result := False;

  // check if directory is writeable
  if FSessionDir <> '' then
  begin
    if not DirectoryIsWritable(FSessionDir) then
      FSessionDir := '';
  end;
  if FSessionDir = '' then
  begin
    FSessionDir := ExtractFilePath(ParamStr(0)) + 'kyoukai_temp/sessions/';
    try
      if not DirectoryExists(FSessionDir) then
        ForceDirectories(FSessionDir);
    except
    end;
  end;
  FSessionDir := IncludeTrailingPathDelimiter(FSessionDir);

  //-- create session file
  FIniFile := CreateIniFile(FSessionDir + FSessionID + FSessionExtension);
  if FIniFile = nil then
    Exit;

  if not DirectoryIsWritable(FSessionDir) then
  begin
    //LogUtil.Add('Can''t write session', 'sessions', True);
  end;

  // init session file
  if not FIniFile.ReadBool(_SESSION_SESSION, _SESSION_ACTIVE, False) then
  begin
    FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, True);
    FIniFile.WriteString(_SESSION_SESSION, _SESSION_IPADDRESS,
      fRequest.RemoteAddress);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYSTART, now);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, now);
  end;

  FLastAccess := FIniFile.ReadDateTime(_SESSION_SESSION, _SESSION_KEYLAST, 0);
  FSessionStarted := True;

  // check if expired
  if GetIsExpired then
  begin
    {
    DeleteIniFile;
    FreeAndNil(FIniFile);
    FSessionTerminated:=True;
    }
    Exit;
  end;

  FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, now);

  if not FCached then
    UpdateIniFile;
  Result := True;
end;

procedure TSessionController.Clear;
begin
  try
    FIniFile.EraseSection(_SESSION_DATA);
    FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, True);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYSTART, now);
    FIniFile.WriteDateTime(_SESSION_SESSION, _SESSION_KEYLAST, now);
    ForceUpdate;
  except;
  end;
end;

procedure TSessionController.DeleteKey(const Key: string);
begin
  FIniFile.DeleteKey(_SESSION_DATA, Key);
end;

procedure TSessionController.EndSession(const Force: boolean);
begin
  try
    FIniFile.WriteBool(_SESSION_SESSION, _SESSION_ACTIVE, False);
    FIniFile.EraseSection(_SESSION_DATA);
    if Force then
    begin
      DeleteIniFile;
      FreeAndNil(FIniFile);
      FSessionTerminated := True;
    end;
  except
  end;
end;

procedure TSessionController.Terminate;
begin
  EndSession;
end;

procedure TSessionController.ForceUpdate;
begin
  UpdateIniFile;
end;

function TSessionController.ReadDateTime(const variable: string): TDateTime;
begin
  Result := 0;
  if (not FSessionStarted) or (FSessionTerminated) or (FIniFile = nil) then
    Exit;
  Result := FIniFile.ReadDateTime(_SESSION_DATA, variable, 0);
end;

function TSessionController.ReadInteger(const variable: string): integer;
begin
  Result := 0;
  if (not FSessionStarted) or (FSessionTerminated) or (FIniFile = nil) then
    Exit;
  Result := FIniFile.ReadInteger(_SESSION_DATA, variable, 0);
end;

end.
