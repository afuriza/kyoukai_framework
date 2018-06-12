<div align="center">
  <img src="src/images/kyoukai_framework.png"/>
</div>
<div align="center">
  <strong>The Boundary, A Simple Framework for A Small Server Application</strong>
</div>
<br/>

[![GitHub version](https://badge.fury.io/gh/afuriza%2Fkyoukai_framework.svg)](https://badge.fury.io/gh/afuriza%2Fkyoukai_framework)
<a href="https://gitter.im/kyoukai_framework?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/Join Chat.svg" alt="Gitter"></a>

The Kyoukai (境界), A Simplified Pascal Web Framework.

In Japanese, Kyoukai means boundary, because the Pascal, it's bounded me as my favorite programming language, no, not like that. The concept is, Kyoukai can be embedded into your desktop application or just leave it as a separated single program, so you can decide which is better, bound it into your existing application or not.

I make it for Pascal because I can not find any Pascal web-framework which I really comfortable with. Some are too complicated, while some others have too many dependencies. Basically, I expect an easy-to-use framework like CodeIgniter in Pascal, even simpler than CodeIgniter.

This project is too far from a complete framework, it still in the experimental stage, please help me if you have any idea, code, and feature request. And, Don't too silly to open an issue.

Requirements
---
* fpc / free pascal compiler, version 3.0.4
* fcl-web
* kyoukai_framework
* lazarus (optional)

Dependencies
---
* kyoukai_framework
* -> kyoukai_standard.lpk

<br/>
<div align="center">
  <img src="src/icons/nyanpasu_125p.png"/>
</div>
<div align="center">
  <h2>Let's getting started!</h2>
</div>

Create two files:

* `project_kyoukai.lpr`;
* `unit1.pas`;

In `project_kyoukai.lpr`, type:

```pascal
program project_kyoukai;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  kyoukai.standard.HTTPApplication,
  {You must placed your module units here or Kyoukai can't register anything!}
  unit1
  { you can add units after this };

{$R *.res}

begin
  KyoukaiApp.Port := 80;
  KyoukaiApp.Active := True;
end.
```

In `unit1.pas`, type:

```pascal
unit uni1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Kyoukai.Standard.WebRouter,
  Kyoukai.Standard.WebModule;

type
  THome = class(TKyModule)
  published
    procedure MainHandle;
  end;

implementation

procedure THome.MainHandle;
begin
  _echo('Hello world!');
end;

initialization
Routes['main'] := THome;

end.
```

Compile and run the project `project_kyoukai.lpr`. Now, in your web browser, access the following URL:

```
http://localhost/
```


<h3 align="center">Screenshots</h3>
<h4 align="center">Hello World Demo</h4>
<div align="center">
  <img src="src/images/helloworld_demo.png" />
</div>

<h4 align="center">Kyoukai Information Page</h4>
<div align="center">
  <img src="src/images/information_page.png" />
</div>

<h4 align="center">Not Found Default Handler</h4>
<div align="center">
  <img src="src/images/notfound_page.png" />
</div>

<h4 align="center">Routing A Module</h4>
<div align="center">
  <img src="src/images/registering_module.png" />
</div>

<h4 align="center">Section Code To Run HTTP Server Application</h4>
<div align="center">
  <img src="src/images/running_application.png" />
</div>
