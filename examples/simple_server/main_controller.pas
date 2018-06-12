unit main_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebModule;

type
  THome = class(TKyModule)
  published
    //must be published or Kyoukai unable to call your method!
    procedure MainHandle;
    procedure FunctionFromMainModule;
    procedure RaiseError;
  end;

implementation

// http://localhost:80/
// Please be remember,
// MainHandle is an index method of the module
procedure THome.MainHandle;
begin
  Response.SendRedirect('/kyoukai_info');
end;

// if there's no module name like this,
// Kyoukai will trying to find it in the main module methods
procedure THome.FunctionFromMainModule;
begin
  _echo('Hello, from "'+ClassName+'" module with '+Request.URI+'!');
end;

procedure THome.RaiseError;
begin
  raise Exception.Create('Error test...');
end;

initialization

// Add a new route for the module THome
Routes['main'] := THome;
{ *********************** information *****************************************
Please be remember, the class module must be added to the new route by accessing
"Routes['module name'] := TClassModule;" with using "Kyoukai.Standard.WebRouter"
unit in the initialization section, or Kyoukai can't register any route!

- To route
Kyoukai.Standard.WebRouter -> Routes['module name']
- To make a module
Kyoukai.Standard.WebModule -> class of TKyModule or Class of Kyoukai Module

Except the function, it can be called via class method already registered,
but it should be declared in published section.
 ***************************************************************************** }

end.

