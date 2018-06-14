program file_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  kyoukai.standard.HTTPApplication,
  {You must placed your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };

{$R *.res}

begin
  KyoukaiApp.MimeTypesFile := ExtractFilePath(ParamStr(0)) + 'mime.types';

  // FileRoutes['assets'] := 'D:\htdocs\assets\';

  // http://localhost:80/files/<path and filename>
  KyoukaiApp.FileRoutes['files'] :=
    ExtractFilePath(ParamStr(0)) + // Extract Executable path
    'files' + // enter folder name "files"
    PathDelim; // delimiter, windows \, unix /

  KyoukaiApp.Port := 80;
  // Initialize everything before you make server.active = true
  KyoukaiApp.Active := True;
end.
