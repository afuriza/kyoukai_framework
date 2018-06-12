unit main_controller;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebModule;

type
  THome = class(TKyModule)
  published
    procedure _prepare;
    procedure MainHandle;
  end;

implementation

procedure THome._prepare;
begin
  Self.StartSession;
end;

procedure THome.MainHandle;
var
  i : integer;
begin
  _echo('Session Example');

  if _SESSION['exist'] = 'yes' then
  begin
    i := strtoint(_SESSION['count']) + 1;
    _SESSION['count'] := inttostr(i);
    _echo('<br>you visit this page ' + IntToStr(i) + ' times');
  end
  else
  begin
    _echo('<br><b>THIS IS FIRST TIME</b>');
    _SESSION['exist'] := 'yes';
    _SESSION['count'] := inttostr(1);
  end;

  if (_GET['op'] = 'delete') then
  begin
    Session.EndSession;
    Redirect('./');
  end;

  _echo('<br><br><br>');
  _echo('<a href="./?op=delete">End Session</a>');
end;

initialization
Routes['main'] := THome;

end.

