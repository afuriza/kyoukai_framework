{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Core.CommonUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TProcCallback = procedure of object;


procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings);
function DumpExceptionCallStack(E: Exception): string;
operator in(str: string; strlist: TStringList): boolean;
operator in(str: string; strarr: array of string): boolean;


implementation

operator in(str: string; strarr: array of string): boolean;
var
  s: string;
begin
  Result := False;
  for s in strarr do
  begin
    Result := s = str;
    if Result then Break;
  end;
end;

operator in(str: string; strlist: TStringList): boolean;
var
  s: string;
begin
  Result := False;
  for s in strlist do
  begin
    Result := s = str;
    if Result then Break;
  end;
end;

procedure Split (const Delimiter: Char; Input: string; const Strings: TStrings);
begin
   Assert(Assigned(Strings)) ;
   Strings.Clear;
   Strings.StrictDelimiter := true;
   Strings.Delimiter := Delimiter;
   Strings.DelimitedText := Input;
end;

function DumpExceptionCallStack(E: Exception): string;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'EXCEPTION ERROR ' + LineEnding +
    'Stacktrace' + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Result := Report + LineEnding +
    'Stacktrace';
  //WriteLn(Report);
end;

end.
