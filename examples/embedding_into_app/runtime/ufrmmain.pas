unit ufrmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  hello_controller,
  Kyoukai.Standard.HTTPServer,
  Kyoukai.Standard.WebRouter;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnActivateServer: TButton;
    procedure btnActivateServerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    MyServer: TKyHTTPServer;
    MyRouter: TKyRoutes;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnActivateServerClick(Sender: TObject);
begin
  if btnActivateServer.Caption = 'Start' then
  begin
    btnActivateServer.Caption := 'Stop';
    // Server started
    MyServer.Start;
  end
  else
  begin
    btnActivateServer.Caption := 'Start';
    // Unable to stop server
    MyServer.Stop;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MyServer := TKyHTTPServer.Create(Self);
  MyRouter := TKyRoutes.Create;
  MyRouter['hello'] := THello;
  MyServer.Router := MyRouter;
  MyServer.Port := 80;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MyRouter.Free;
  MyServer.Free;
end;

end.

