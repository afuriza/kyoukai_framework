program website_template;

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
  // localhost/vendor/*
  KyoukaiApp.FileRoutes['vendor'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'vendor' + PathDelim;
  // localhost/css/*
  KyoukaiApp.FileRoutes['css'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'css' + PathDelim;
  // localhost/img/*
  KyoukaiApp.FileRoutes['img'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'img' + PathDelim;
  // localhost/js/*
  KyoukaiApp.FileRoutes['js'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'js' + PathDelim;
  KyoukaiApp.Port := 80;
   KyoukaiApp.Threaded := True;
  // Threading somehow causes memleak on webview
  KyoukaiApp.Active := True;
end.
