unit kyoukai_projectconsts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
const
  Kyoukai = 'Kyoukai';
  _APP_SLOGAN = 'A Simplified Pascal Web Framework';
  {$ifdef windows}
  _APP_EXTENSION = '.exe';
  {$else}
  _APP_EXTENSION = '.bin';
  {$endif}
  _APP_EXTENSION_CGI = '.cgi';

implementation

end.

