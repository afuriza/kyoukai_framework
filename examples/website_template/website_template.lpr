program website_template;

{$mode objfpc}{$H+}

uses
  {$IFNDEF WINDOWS}
  cthreads,
  {$ENDIF}
  Classes, SysUtils,
  kyoukai.standard.HTTPApplication,
  Kyoukai.Standard.WebRouter,
  {You must place your module units here or Kyoukai can't register anything!}
  main_controller
  { you can add units after this };

begin
  KyoukaiApp.MimeTypesFile := ExtractFilePath(ParamStr(0)) + 'mime.types';
  // localhost/vendor/*
  FileRoutes['vendor'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'vendor' + PathDelim;
  // localhost/css/*
  FileRoutes['css'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'css' + PathDelim;
  // localhost/img/*
  FileRoutes['img'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'img' + PathDelim;
  // localhost/js/*
  FileRoutes['js'] :=
    ExtractFilePath(ParamStr(0)) +
    'assets' + PathDelim + 'js' + PathDelim;
  KyoukaiApp.Port := 9000;
   KyoukaiApp.Threaded := True;
  // Threading somehow causes memleak on webview or webmodule
  KyoukaiApp.Run;
end.
