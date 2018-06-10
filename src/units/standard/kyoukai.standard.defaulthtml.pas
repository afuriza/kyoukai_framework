{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Standard.DefaultHTML;

{$mode objfpc}{$H+}

interface
uses
  Classes, SysUtils, DateUtils, Kyoukai.Other.RengeMessages;

function GetDefaultHTML(SiteName, SiteLink, Title, HeadingText, Content: string;
  const ServeStartTime: TDateTime): string;
function GetKyoukaiInformation(SiteName, SiteLink: string;
  const ServeStartTime: TDateTime): string;
function GetNotFoundInformation(SiteName, SiteLink, AMessage: string;
  const ServeStartTime: TDateTime): string;
function GetErrorInformation(SiteName, SiteLink, AMessage: string;
  const ServeStartTime: TDateTime): string;

const
  base64_nyanpasu_icon_35p = {$I 'nyanpasu_icon_35p.inc'};
implementation


function DefaultCSS: String;
begin
  Result :=
  'h3 {'+ LineEnding +
  '  font-size: 12pt;'+ LineEnding +
  '  font-weight: bold;'+ LineEnding +
  '  margin: 0 0 12px 0;'+ LineEnding +
  '}'+ LineEnding +

  'body {'+ LineEnding +
  '  font-size: 10pt;'+ LineEnding +
  '  font-family: Helvetica;'+ LineEnding +
  '  margin: 20px;'+ LineEnding +
  '}'+ LineEnding +

  '@media handheld,'+ LineEnding +
  '  (max-width: 640px),'+ LineEnding +
  '  (max-device-width: 480px) {'+ LineEnding +
  '    body {'+ LineEnding +
  '      margin: 10px;'+ LineEnding +
  '      width: 90%'+ LineEnding +
  '    }'+ LineEnding +
  '  }'+ LineEnding +

  'input[type="text"] {'+ LineEnding +
  '  font-size: 10pt;'+ LineEnding +
  '  padding: 2px;'+ LineEnding +
  '  margin: 0 2px;'+ LineEnding +
  '  width: 160px;'+ LineEnding +
  '}'+ LineEnding +

  'input[type="submit"] {'+ LineEnding +
  '  font-size: 12pt;'+ LineEnding +
  '  margin: 0 2px;'+ LineEnding +
  '}'+ LineEnding +

  'input[type="radio"] {'+ LineEnding +
  '  margin: 2px 2px 2px 2px;'+ LineEnding +
  '}'+ LineEnding +

  'textarea {'+ LineEnding +
  '  width: calc(100% - 14px);'+ LineEnding +
  '  padding: 4px 6px;'+ LineEnding +
  '  color: #eeeeee;'+ LineEnding +
  '  border-color: #000;'+ LineEnding +
  '  background-color: #222222;'+ LineEnding +
  '  font-size: 10pt;'+ LineEnding +
  '	font-family: "Menlo","Monaco","Consolas","Inconsolata","Courier New",monospace;'+ LineEnding +
  '}'+ LineEnding +

  'select {'+ LineEnding +
  '  font-size: 12pt;'+ LineEnding +
  '  margin: 4px 4px;'+ LineEnding +
  '  width: 200px;'+ LineEnding +
  '}'+ LineEnding +

  'pre {'+ LineEnding +
  '  font-size: 11pt;'+ LineEnding +
  '  margin: 0 4px;'+ LineEnding +
  '  overflow: auto;'+ LineEnding +
  '}'+ LineEnding +

  'code {'+ LineEnding +
  '  font-size: 11pt;'+ LineEnding +
  '  margin: 0 4px;'+ LineEnding +
  '}'+ LineEnding +

  'label {'+ LineEnding +
  '  cursor: pointer;'+ LineEnding +
  '}'+ LineEnding +

  'table, th, td, tr {'+ LineEnding +
  '  font-size: 10pt;'+ LineEnding +
  '  border: 1px solid;'+ LineEnding +
  '  padding: 2px 4px;'+ LineEnding +
  '  border-collapse: collapse;'+ LineEnding +
  '}'+ LineEnding +

  'li {'+ LineEnding +
  '  margin-left: -10px;'+ LineEnding +
  '}'+ LineEnding +

  'th {'+ LineEnding +
  '  background-color: #666;'+ LineEnding +
  '  border-color: #000;'+ LineEnding +
  '  color: #fff;'+ LineEnding +
  '}'+ LineEnding +

  'tr:nth-of-type(odd) {'+ LineEnding +
  '  background-color: #eee;'+ LineEnding +
  '}'+ LineEnding +

  'tr:nth-of-type(even) {'+ LineEnding +
  '  background-color: #fff;'+ LineEnding +
  '}'+ LineEnding +

  'hr {'+ LineEnding +
  '  border: none;'+ LineEnding +
  '  height: 1px;'+ LineEnding +
  '  background-color: #999;'+ LineEnding +
  '}'+ LineEnding +

  'mark {'+ LineEnding +
  '  margin: 0 4px;'+ LineEnding +
  '}'+ LineEnding +
  '.content-outer {'+ LineEnding +
  '  background-color: white;'+ LineEnding +
  '  padding: 10px;'+ LineEnding +
  '  margin: auto;'+ LineEnding +
  '/*   min-width: 400px; */'+ LineEnding +
  '   max-width: 600px;'+ LineEnding +
  '  overflow: auto;'+ LineEnding +
  '}'+ LineEnding +
  '.content, .box {'+ LineEnding +
  '  border: 1px;'+ LineEnding +
  '  border-style: solid;'+ LineEnding +
  '  border-color: gray;'+ LineEnding +
  '  border-radius: 6px;'+ LineEnding +
  '  background-color: white;'+ LineEnding +
  '  padding: 10px;'+ LineEnding +
  '  margin: 6px 0;'+ LineEnding +
  '/*   min-width: 400px; */'+ LineEnding +
  '  overflow: auto;'+ LineEnding +
  '}'+ LineEnding +

  '.caption {'+ LineEnding +
  '  display: inline-block;'+ LineEnding +
  '  width: 60px;'+ LineEnding +
  '  margin: 0 0 12px 0px;'+ LineEnding +
  '}'+ LineEnding +

  '.separator {'+ LineEnding +
  '  display: inline-block;'+ LineEnding +
  '  margin: 0 2px;'+ LineEnding +
  '  width: 40px;'+ LineEnding +
  '  text-align: right;'+ LineEnding +
  '}'+ LineEnding +

  '.footer {'+ LineEnding +
  '  font-size: 8pt;'+ LineEnding +
  '  font-style: italic;'+ LineEnding +
  '}'+ LineEnding +

  '.smaller {'+ LineEnding +
  '  font-size: 9pt;'+ LineEnding +
  '  font-style: italic;'+ LineEnding +
  '}'+ LineEnding +

  '.bigger {'+ LineEnding +
  '  font-size: 14pt;'+ LineEnding +
  '}'+ LineEnding +

  '.error {'+ LineEnding +
  '  color: red;'+ LineEnding +
  '  background-color: yellow;'+ LineEnding +
  '  font-weight: bold;'+ LineEnding +
  '  text-transform: uppercase;'+ LineEnding +
  '  padding: 0 2pt;'+ LineEnding +
  '}'+ LineEnding +

  '.letters {'+ LineEnding +
  '  color: blue;'+ LineEnding +
  '  background-color: yellow;'+ LineEnding +
  '  font-weight: bold;'+ LineEnding +
  '  font-size: 11pt;'+ LineEnding +
  '  text-transform: uppercase;'+ LineEnding +
  '  letter-spacing: 4pt;'+ LineEnding +
  '  padding: 0 0 0 4pt;'+ LineEnding +
  '}'+ LineEnding +

  '.tighter {'+ LineEnding +
  '  margin-top: 5px;'+ LineEnding +
  '}'+ LineEnding +

  // tooltip
  '.tooltip {'+ LineEnding +
  '    position: relative;'+ LineEnding +
  '    display: inline-block;'+ LineEnding +
  '    border-bottom: 1px dotted black;'+ LineEnding +
  '}'+ LineEnding +

  '.tooltip .tooltiptext {'+ LineEnding +
  '    visibility: hidden;'+ LineEnding +
  '    width: 120px;'+ LineEnding +
  '    background-color: #555;'+ LineEnding +
  '    color: #fff;'+ LineEnding +
  '    text-align: center;'+ LineEnding +
  '    border-radius: 6px;'+ LineEnding +
  '    padding: 5px 0;'+ LineEnding +
  '    position: absolute;'+ LineEnding +
  '    z-index: 1;'+ LineEnding +
  '    bottom: 125%;'+ LineEnding +
  '    left: 50%;'+ LineEnding +
  '    margin-left: -60px;'+ LineEnding +
  '    opacity: 0;'+ LineEnding +
  '    transition: opacity 0.3s;'+ LineEnding +
  '}'+ LineEnding +

  '.tooltip .tooltiptext::after {'+ LineEnding +
  '    content: "";'+ LineEnding +
  '    position: absolute;'+ LineEnding +
  '    top: 100%;'+ LineEnding +
  '    left: 50%;'+ LineEnding +
  '    margin-left: -5px;'+ LineEnding +
  '    border-width: 5px;'+ LineEnding +
  '    border-style: solid;'+ LineEnding +
  '    border-color: #555 transparent transparent transparent;'+ LineEnding +
  '}'+ LineEnding +

  '.tooltip:hover .tooltiptext {'+ LineEnding +
  '    visibility: visible;'+ LineEnding +
  '    opacity: 1;'+ LineEnding +
  '}'+ LineEnding;
end;

function GetDefaultHTML(SiteName, SiteLink, Title, HeadingText, Content: string;
  const ServeStartTime: TDateTime): string;
begin
  Result :=
  '<!DOCTYPE html>'+ LineEnding +
  '<html><head>'+ LineEnding +
  '  <meta charset="UTF-8" lang="id">'+ LineEnding +
  '  '+ LineEnding +
  '  <style type="text/css">'+ LineEnding +
  DefaultCSS + LineEnding +
  '  </style>'+ LineEnding +
  '  <title>$title</title>'+ LineEnding +
  '</head><body>'+ LineEnding +
  '  <div class="content-outer">'+ LineEnding +
  '<img src="/ky_icon_nyanpasu.png" alt="Nyanpasu!" align="right" '+ LineEnding +
  'title="Renge found a something interesting!"> <h3>$heading_text</h3> '+ LineEnding +
  '<strong>Rechon says: </strong><i>'+KataRenge+'</i>'+ LineEnding +
  '  <div class="header"></div>'+ LineEnding +
  '  <div class="content">'+ LineEnding +
  '<!--- ### generated content start here ### --->'+ LineEnding +

  '$content'+ LineEnding +

  '<!--- ### generated content end here ### --->'+ LineEnding +
  '  </div>'+ LineEnding +
  '  <div class="footer">'+ LineEnding +
  '    This page is served in $serving_time ms by '+ LineEnding +
  '    <strong><a href="http://freepascal.org" target="_blank">Free Pascal</a>'+
  '</strong>.<br>'+ LineEnding +
  '    Courtesy of '+ LineEnding +
  '    <a href="$site_link">$site_name</a><br>'+ LineEnding +
  '  </div>'+ LineEnding +
  '  </div>'+ LineEnding +

  '</body></html>'+ LineEnding;
  Result := StringReplace(Result, '$site_name', SiteName, [rfReplaceAll]);
  Result := StringReplace(Result, '$site_link', SiteLink, [rfReplaceAll]);
  Result := StringReplace(Result, '$title', Title, [rfReplaceAll]);
  Result := StringReplace(Result, '$heading_text', HeadingText, [rfReplaceAll]);
  Result := StringReplace(Result, '$content', Content, [rfReplaceAll]);
  Result := StringReplace(Result, '$serving_time',
    IntToStr(MilliSecondsBetween(Now, ServeStartTime)), [rfReplaceAll]);

end;

function GetNotFoundInformation(SiteName, SiteLink, AMessage: string;
  const ServeStartTime: TDateTime): string;
begin
  Result := GetDefaultHTML(SiteName, SiteLink,
      'Kyoukai HTTP Server - Not Found', ':''( | 404 - PAGE NOT FOUND',
      AMessage, Now);
end;

function GetKyoukaiInformation(SiteName, SiteLink: string;
  const ServeStartTime: TDateTime): string;
begin
  Result := GetDefaultHTML(SiteName, SiteLink,
      'Kyoukai HTTP Server', 'KYOUKAI HTTP SERVER INFORMATION',
      '<strong>Kyoukai HTTP Application Framework '+{$I 'kyoukai_version.inc'}+
      '</strong><br>'+
      'Build Date: '+{$I %date%}+'<br>'+
      'Free Pascal Version: FPC '+{$I %FPCVERSION%}+'<br>'+
      'Platform: '+{$I %FPCTARGETCPU%}+'-'+{$I %FPCTARGETOS%}, Now);
end;

function GetErrorInformation(SiteName, SiteLink, AMessage: string;
  const ServeStartTime: TDateTime): string;
var
  ReportStr: string;
begin
  ReportStr := StringReplace(AMessage, LineEnding, '<br>', [rfReplaceAll]);
  ReportStr := StringReplace(ReportStr, 'EXCEPTION ERROR',
    '<b>EXCEPTION ERROR!</b><br>', [rfReplaceAll]);
  ReportStr := StringReplace(ReportStr, 'Stacktrace',
    '<i><b>------ stacktrace ------</b></i>', [rfReplaceAll]);
  ReportStr := StringReplace(ReportStr, 'Exception class:',
    '<b>Exception class:</b>', [rfReplaceAll]);
  ReportStr := StringReplace(ReportStr, 'Message:',
    '<b>Message:</b>', [rfReplaceAll]);
  Result := GetDefaultHTML(SiteName, SiteLink,
      'Kyoukai HTTP Server - Error', ':''( | SOMETHING WENT WRONG',
      ReportStr, Now);
end;

end.

