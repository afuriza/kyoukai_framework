program file_server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  kyoukai.standard.HTTPApplication,
  Kyoukai.Standard.WebRouter,
  {You must placed your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };

{$R *.res}

begin

  KyoukaiApp.Port := 80;
  KyoukaiApp.MimeTypesFile := ExtractFilePath(ParamStr(0)) + 'mime.types';
  // http://localhost:80/files/<path and filename>
  FileRoutes['files'] :=
    ExtractFilePath(ParamStr(0)) + // Extract Executable path
    'files' + // enter folder name "files"
    PathDelim; // delimiter, windows \, unix /
  // FileRoutes['assets'] := 'D:\htdocs\assets\';
  KyoukaiApp.Active := True;
end.
