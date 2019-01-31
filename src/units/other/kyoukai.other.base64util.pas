{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
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

//function EncodeBase64StreamToStr(var AStream: TStream): string;
function DecodeBase64StrToStream(const AStr: string): TMemoryStream;
{ Still thinking to write this...
function DecodeBase64StreamToStr(var AStream: TStream): string;
function EncodeBase64StrToStream(const AStr: string): TStream;
function EncodeBase64FileToStream(const AFileName: string): TStream;
function EncodeBase64FileToStr(const AFileName: string): string;
procedure EncodeBase64StrToFile(const AStr, AFileName: string);
procedure EncodeBase64StreamToFile(const AStr, AFileName: string);
function DecodeBase64FileToStream(const AFileName: string): TStream;
function DecodeBase64FileToStr(const AFileName: string): string;
procedure DecodeBase64StrToFile(const AStr, AFileName: string);
procedure DecodeBase64StreamToFile(const AStr, AFileName: string);
}

implementation

function EncodeBase64Str(const AStr: string): string;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream);
    try
      Encoder.Write(AStr[1],Length(AStr));
    finally
      Encoder.Free;
      end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

function DecodeBase64Str(const AStr: string): string;
var
  Instream,
  Outstream : TStringStream;
  Decoder   : TBase64DecodingStream;
begin
  Instream:=TStringStream.Create(AStr);
  try
    Outstream:=TStringStream.Create('');
    try
      Decoder:=TBase64DecodingStream.Create(Instream,bdmMIME);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
         Result:=Outstream.DataString;
      finally
        Decoder.Free;
      end;
    finally
     Outstream.Free;
     end;
  finally
    Instream.Free;
  end;
end;

function DecodeBase64StrToStream(const AStr: string): TMemoryStream;
var
  EncodedStream: TStringStream;
  Decoder: TBase64DecodingStream;
begin
  EncodedStream := TStringStream.Create(AStr);
  Result := TMemoryStream.Create;
  Decoder := TBase64DecodingStream.Create(EncodedStream);
  Result.CopyFrom(Decoder, Decoder.Size);
  FreeAndNil(EncodedStream);
  FreeAndNil(Decoder);
end;

end.

