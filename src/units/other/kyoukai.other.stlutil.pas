unit Kyoukai.Other.STLUtil;

{$mode objfpc}{$H+}

interface

uses
{$if fpc_fullversion >= 20701}
  ghashmap,
{$else fpc_fullversion >= 20701}
  fgl,
{$endif fpc_fullversion >= 20701}
  Classes, SysUtils;

type
{$if fpc_fullversion >= 20701}
  { TStringHash }

  TStringHash = class
    class function hash(s: String; n: Integer): Integer;
  end;

  generic TStringHashMap<T> = class(specialize THashMap<String,T,TStringHash>) end;
{$else fpc_fullversion >= 20701}
  generic TStringHashMap<T> = class(specialize TFPGMap<String,T>) end;
{$endif fpc_fullversion >= 20701}

implementation

{$if fpc_fullversion >= 20701}
class function TStringHash.hash(s: String; n: Integer): Integer;
var
  c: Char;
begin
  Result := 0;
  for c in s do
    Inc(Result,Ord(c));
  Result := Result mod n;
end;
{$endif fpc_fullversion >= 20701}

end.
