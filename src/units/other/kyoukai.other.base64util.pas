{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file LICENSE.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Other.Base64Util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Base64;

function EncodeBase64Str(const AStr: string): string;
function DecodeBase64Str(const AStr: string): string;

implementation

function EncodeBase64Str(const AStr: string): string;
var
  DecodedStream: TStringStream;
  EncodedStream: TStringStream;
  Decoder: TBase64EncodingStream;
begin
  EncodedStream := TStringStream.Create(AStr);
  DecodedStream := TStringStream.Create('');
  Decoder       := TBase64EncodingStream.Create(EncodedStream);
  DecodedStream.CopyFrom(Decoder, Decoder.Size);
  Result := DecodedStream.DataString;
  DecodedStream.Free;
  EncodedStream.Free;
  Decoder.Free;
end;

function DecodeBase64Str(const AStr: string): string;
var
  DecodedStream: TStringStream;
  EncodedStream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  EncodedStream := TStringStream.Create(AStr);
  DecodedStream := TStringStream.Create('');
  Decoder       := TBase64DecodingStream.Create(EncodedStream);
  DecodedStream.CopyFrom(Decoder, Decoder.Size);
  Result := DecodedStream.DataString;
  DecodedStream.Free;
  EncodedStream.Free;
  Decoder.Free;
end;

end.

