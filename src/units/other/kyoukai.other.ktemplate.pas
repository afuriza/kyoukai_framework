{*******************************************************************************

                          This is Part of Kyoukai units
                       The Simple Web Template for Pascal

Based on QTemplate by Leledumbo

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Other.KTemplate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fptemplate, dateutils, Kyoukai.Other.STLUtil;

function SQLParse(const SQLStr: string; AItemNames, AItemValues: array of string): string;

type

  { HashMap }

  TTagCallback = function (const ATag: String; AParams: TStringList): String of object;
  TCallbackMap = specialize TStringHashMap<TTagCallback>;

  { TKItems class }

  TKItems = class(TObject)
  private
    fItemTemplate: string;
    fItemList: TStringList;
    function GetText: string;
  public
    constructor Create(const ItemTemplate: string); overload; virtual;
    destructor Destroy; override;
    {* multiply for each html tag usage
{+ transactionlist
  #echo[
   <th>$customer</th>
   <th>$product</th>
   <th>$msisdn</th>
   <th>$price</th>
   <th>$status</th>
   <th>$serialnumber</th>
   <th>
     <div class="dataTables_buttons hidden-sm-down actions">
       <span class="actions__item zmdi zmdi-print" data-table-action="print"></span>
     </div>
   </th>
  ]#
+}

MyTemplate['transactionlist'] := @ParseTransactions;
...
  Transaksi := TTransactions.Create;
  try
    Transaksi.OpenTransactions(_Session['user']);
    with TKItems.Create(AParams.Values['echo']) do
      try
        while not Transaksi.EOF do
        begin
          Add(
            ['date', 'time', 'productname', 'productcat', 'msisdn', 'qty', 'price', 'status', 'sn'],
            [
              Transaksi.Field['date'],
              Transaksi.Field['time'],
              Transaksi.Field['product_name'],
              Transaksi.Field['category_name'],
              Transaksi.Field['msisdn'],
              Transaksi.Field['qty'],
              Transaksi.Field['price'],
              Transaksi.Field['status'],
              Transaksi.Field['sn']
            ]);
          Transaksi.Next;
        end;
        Result += Text;
      finally
        Free;
      end;
  except
    on E: Exception do
    begin
      Result :=
        '{ "name": "error", "message": "' +
          StringReplace(e.Message, '"', '''', [rfReplaceAll]) +
        '" }';
    end;
  end;
  Transaksi.Free;

*}
    procedure Add(const AItemNamesAsFormat, AItemValues: array of string);
    property Text: string read GetText;
  end;

  { KTemplate class }

  TKTemplate = class(TFPTemplate)
  private
    fTagMap: TCallbackMap;
    fCaseSensitive: Boolean;
    function GetTagName(const TagString: String): String;
    procedure ReplaceTags(Sender: TObject; const TagString: String;
      TagParams: TStringList; out ReplaceText: String);
    function GetTag(const TagString: String): TTagCallback;
    procedure SetTag(const TagString: String; AValue: TTagCallback);
  public
    constructor Create(const AFileName: String);overload; virtual;
    constructor Create(const AStrings: TStrings);overload; virtual;
    destructor Destroy; override;
    property Tags[const TagString: String]: TTagCallback read GetTag write SetTag; default;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
  end;


implementation

function SQLParse(const SQLStr: string; AItemNames, AItemValues: array of string): string;
var
  fItems: array of string;
  i: integer;
begin
  SetLength(fItems, Length(AItemNames));
  for i := 0 to Length(AItemNames) - 1 do
  begin
    fItems[i] := '$'+AItemNames[i];
  end;
  Result := StringsReplace(SQLStr, fItems, AItemValues,[rfReplaceAll]);
end;

{ TKItems }

constructor TKItems.Create(const ItemTemplate: string);
begin
  inherited Create;
  fItemList := TStringList.Create;
  fItemTemplate := ItemTemplate;
end;

destructor TKItems.Destroy;
begin
  FreeAndNil(fItemList);
  inherited Destroy;
end;

function TKItems.GetText: string;
begin
  Result := fItemList.Text;
end;

procedure TKItems.Add(const AItemNamesAsFormat, AItemValues: array of string);
var
  fItems: array of string;
  i: integer;
begin
  SetLength(fItems, Length(AItemNamesAsFormat));
  for i := 0 to Length(AItemNamesAsFormat) - 1 do
  begin
    fItems[i] := '$'+AItemNamesAsFormat[i];
  end;
  fItemList.Add(StringsReplace(fItemTemplate, fItems, AItemValues,[rfReplaceAll]));
end;

{ TKTemplate }

function TKTemplate.GetTagName(const TagString: String): String;
begin
  if FCaseSensitive then
    Result := TagString
  else
    Result := LowerCase(TagString);
end;

procedure TKTemplate.ReplaceTags(Sender: TObject; const TagString: String;
  TagParams: TStringList; out ReplaceText: String);
var
  TagName: String;
begin
  TagName := GetTagName(TagString);
  {$if fpc_fullversion >= 20701}
  if fTagMap.Contains(TagName) then begin
  {$else fpc_fullversion >= 20701}
  if fTagMap.IndexOf(TagName) >= 0 then begin
  {$endif fpc_fullversion >= 20701}
    ReplaceText := fTagMap[TagName](TagName,TagParams);
  end
  else
  begin
    //Not found value for tag -> TagString
    ReplaceText := #13 + '<!-- Warning: Did you forget to assign ' +
      StartDelimiter + TagString + EndDelimiter + ' tag? -->'
      + #13;
  end;
end;

function TKTemplate.GetTag(const TagString: String): TTagCallback;
var
  TagName: String;
begin
  TagName := GetTagName(TagString);
  Result := FTagMap[TagName];
end;

procedure TKTemplate.SetTag(const TagString: String; AValue: TTagCallback);
var
  TagName: String;
begin
  TagName := GetTagName(TagString);
  fTagMap[TagName] := AValue;
end;

constructor TKTemplate.Create(const AFileName: String);
begin
  inherited Create;
  FileName := AFileName;

  fCaseSensitive := false;
  fTagMap := TCallbackMap.Create;

  StartDelimiter := '{+';
  EndDelimiter := '+}';
  ParamStartDelimiter := '#';
  ParamValueSeparator := '[';
  ParamEndDelimiter := ']#';

  AllowTagParams := true;
  OnReplaceTag := @ReplaceTags;
end;

constructor TKTemplate.Create(const AStrings: TStrings);
begin
  inherited Create;
  Template := AStrings.Text;
  fCaseSensitive := false;
  fTagMap := TCallbackMap.Create;

  StartDelimiter := '{+';
  EndDelimiter := '+}';
  ParamStartDelimiter := '#';
  ParamValueSeparator := '[';
  ParamEndDelimiter := ']#';

  AllowTagParams := true;
  OnReplaceTag := @ReplaceTags;
end;

destructor TKTemplate.Destroy;
begin
  FreeAndNil(fTagMap);
  inherited Destroy;
end;

end.

