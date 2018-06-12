unit hello_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebModule,
  Kyoukai.Standard.WebRouter;

type
  THello = class(TKyModule)
  published
    //must be published your you get an error!
    procedure MainHandle;
    procedure SayHelloWorld;
  end;

implementation

// http://localhost:80/hello/
procedure THello.MainHandle;
begin
  _echo('Hello, from "'+ClassName+'" module!');
end;

// http://localhost:80/hello/sayhelloworld
procedure THello.SayHelloWorld;
begin
  _echo('Hello, world!');
  _echo('<br>');
  _echo('- From "'+ClassName+'" module with URI: "'+Request.URI+'"');
  _echo('<br>');
  _echo('Let''s try to get uriparam: '+_get['what']);
end;

initialization

// add a new route for the module Thello
Routes['hello'] := THello;

end.

