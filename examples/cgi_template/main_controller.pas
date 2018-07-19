unit main_controller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebView,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebModule;

type
  THome = class(TKyModule)
  protected
    View: TKyView;
  private
    procedure DoParseShortAbout;
  published
    procedure _prepare;
    procedure _done;
    procedure MainHandle;
  end;

implementation

procedure THome._prepare;
begin
  // load a template
  View := TKyView.Create(ExtractFilePath(ParamStr(0)) +
    'views' + PathDelim + 'index.html');
end;

procedure THome.DoParseShortAbout;
var
  Data: TDataItem;
begin
  View.DataItems['shortabout'] := TDataItems.Create; // create items container
  // you can do loop here
  Data := TDataItem.Create; // create an item
  Data['firstname'] := 'dio';
  Data['lastname'] := 'affriza';
  Data['address'] := 'Bukit Cemara Tidar';
  Data['city'] := 'Malang';
  Data['phonenumber'] := '+6285785851539';
  Data['email'] := 'dioaffriza@gmail.com';
  Data['shortinfo'] := 'Linux + Lazarus Free Pascal Rocks!';
  View.DataItems['shortabout'].Add(Data); // insert into view
  // to here
end;

procedure THome.MainHandle;
begin
  // single data item
  View.DataItem['title'] := 'Halo';
  // muliple data item
  DoParseShortAbout;
  // Then render the view to the web page
  Self.Render(View);
end;

procedure THome._done;
begin
  FreeAndNil(View);
end;

initialization
Routes['main'] := THome;

end.

