unit Kyoukai.Other.RengeMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function KataRenge: string;
function RandomRange(from_number, onto_number: integer): integer;

const
  RengeNoKataru: array [0..10] of string = (
    'Do we live in the country?',
    'I''m going to call him Salt.',
    'Yeah, I''m laughing, all right.',
    'What is haute couture?',
    'The dekopon is two-digit subtraction, so maybe it''s too hard.',
    'An eight-legged Martian with glowing eyes is lying on the ground!',
    'Nee-nee told me that if you drive a car at fifty kilometers per hour, '+
    'youl''ll get to place fifty kilometers away in one hour, and that''s '+
    'really amazing.',
    'Nyanpasu!',
    'You forgot your cellphone!',
    'Who cares about that? I want candy!',
    'I know! I''ll do a Swallow Reversal!');

implementation

function RandomRange(from_number, onto_number: integer): integer;
begin
  Result := Random(onto_number - from_number) + from_number;
end;

function KataRenge: string;
begin
  Result := RengeNoKataru[RandomRange(0, Length(RengeNoKataru) - 1)];
end;

initialization
{ generate a new random sequence when the program being initialized }
Randomize;
end.

