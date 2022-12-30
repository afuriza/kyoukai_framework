{*******************************************************************************

                          This is Part of Kyoukai units
                        A Simple Web Framework for Pascal

See the file COPYING.LGPL.txt, included in this distribution,
for details about the copyright.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

*******************************************************************************}
unit Kyoukai.Base.WebView;

{$mode objfpc}{$H+}
{ To do
  * Loop syntax
  Single tag DataItem[tagname: string] := string
  Iterable tag DataItems[tagname: string] := TDataItem

  This code is too messy, I need help for this
}
interface

uses
  Classes, SysUtils, fptemplate, fgl,
  Kyoukai.Core.KTemplate,
  Kyoukai.Core.STLUtil;

type

  TDataItem = class(TObject)
  private
    fIterateData: TStrings;
    function ReadIterateData(AName: string): string;
    procedure WriteIterateData(AName, AValue: string);
    function ReadCount: integer;
    function ReadNames(Position: integer): string;
    function ReadValue(Position: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    property Count: integer read ReadCount;
    property Names[Position: integer]: string read ReadNames;
    property Values[Position: integer]: string read ReadValue;
    property NameValues[AName: string]: string read ReadIterateData write
     WriteIterateData; default;
  end;

  TDataItems = specialize TFPGObjectList<TDataItem>;

  TDataItemMap = specialize TStringHashMap<TDataItems>;

  TWebView = class(TObject)
  private
    fKTemplate: TFPTemplate;
    fDataItem: TStringList;
    fDataItemsTagList: TStringList;
    fDataItems: TDataItemMap;
    function ReadDataItem(AName: string): string;
    procedure WriteDataItem(AName, AValue: string);
    function ReadDataItems(AName: string): TDataItems;
    procedure WriteDataItems(AName: string; AValues: TDataItems);
    procedure ReplaceTag(Sender : TObject; Const TagString : String;
      TagParams:TStringList; Out ReplaceText : String);
  public
    property DataItem[AName: string]: string read ReadDataItem write
      WriteDataItem;
    property DataItems[AName: string]: TDataItems read ReadDataItems write
      WriteDataItems;
    function GetContent: string;
    function GetContent(GlobalViewName: string): string;
    constructor Create(const AFileName: String);
    destructor Destroy; override;
  end;

  TKyGlobalView = class(TObject)
  private
    fContent: string;
    fKTemplate: TFPTemplate;
    fDataItem: TStringList;
    fDataItemsTagList: TStringList;
    fDataItems: TDataItemMap;
    function ReadDataItem(AName: string): string;
    procedure WriteDataItem(AName, AValue: string);
    function ReadDataItems(AName: string): TDataItems;
    procedure WriteDataItems(AName: string; AValues: TDataItems);
    procedure ReplaceTag(Sender : TObject; Const TagString : String;
      TagParams:TStringList; Out ReplaceText : String);
  public
    property DataItem[AName: string]: string read ReadDataItem write
      WriteDataItem;
    property DataItems[AName: string]: TDataItems read ReadDataItems write
      WriteDataItems;
    function GetContent(AContent: string): string;
    constructor Create(const AFileName: String);
    destructor Destroy; override;
  end;

  TGlobalViewMap = specialize TStringHashMap<TKyGlobalView>;

var
  GlobalView: TGlobalViewMap;
implementation

function TDataItem.ReadCount: integer;
begin
  Result := fIterateData.Count;
end;

function TDataItem.ReadNames(Position: integer): string;
begin
  Result := fIterateData.Names[Position];
end;

function TDataItem.ReadValue(Position: integer): string;
begin
  Result := fIterateData.Values[fIterateData.Names[position]];
end;

function TDataItem.ReadIterateData(AName: string): string;
begin
  Result := fIterateData.Values[AName];
end;

procedure TDataItem.WriteIterateData(AName, AValue: string);
begin
  fIterateData.Values[AName] := AValue;
end;

constructor TDataItem.Create;
begin
  fIterateData := TStringList.Create;
end;

destructor TDataItem.Destroy;
begin
  FreeAndNil(fIterateData);
  inherited Destroy;
end;

{view}

function TWebView.ReadDataItem(AName: string): string;
begin
  Result := fDataItem.Values[AName];
end;

procedure TWebView.WriteDataItem(AName, AValue: string);
begin
  fDataItem.Values[AName] := AValue;
end;

procedure TWebView.ReplaceTag(Sender : TObject; Const TagString : String;
      TagParams:TStringList; Out ReplaceText : String);
var
  ParamNames, ParamValues: array of string;
  i, j: integer;
begin
  if fDataItems.contains(TagString) then
  begin
    fDataItemsTagList.Add(TagString);
    if fDataItems[TagString].count > 0 then
    for i:= 0 to fDataItems[TagString].items[0].count - 1 do
    begin
      SetLength(ParamNames, i+1);
      SetLength(ParamValues, i+1);
      ParamNames[i] := fDataItems[TagString].items[0].Names[i];
    end;
    with TKItems.Create(TagParams.Values['iterable']) do
    begin
      for i := 0 to fDataItems[TagString].count - 1 do
      begin
        for j := 0 to Length(ParamValues)-1 do
        begin
          try
          ParamValues[j] := fDataItems[TagString].items[i].Values[j];

          except
          end;
        end;
        Add(ParamNames, ParamValues);
        ReplaceText := Text;
      end;
    end;
  end
  else
  begin
    ReplaceText := fDataItem.Values[TagString];
  end;
end;

function TWebView.ReadDataItems(AName: string): TDataItems;
var
  ffDataItems: TDataItems;
begin
  if fDataItems.contains(AName) then
    Result := fDataItems[AName]
  else
  begin
    ffDataItems := TDataItems.Create;
    fDataItems[AName] := ffDataItems;
    Result := fDataItems[AName];
  end;
end;

procedure TWebView.WriteDataItems(AName: string; AValues: TDataItems);
var
  ffDataItems: TDataItems;
begin
  if fDataItems.contains(AName) then
    fDataItems[AName] := AValues
  else
  begin
    ffDataItems := TDataItems.Create;
    fDataItems[AName] := ffDataItems;
  end;
end;

function TWebView.GetContent: string;
begin
  Result := fKTemplate.GetContent;
end;

function TWebView.GetContent(GlobalViewName: string): string;
begin
  Result := GlobalView[GlobalViewName].GetContent(fKTemplate.GetContent);
end;

constructor TWebView.Create(const AFileName: String);
begin
  fKTemplate := TFPTemplate.Create;
  fDataItem := TStringList.Create;
  fDataItems := TDataItemMap.Create;
  fDataItemsTagList := TStringList.Create;

  fKTemplate.StartDelimiter := '{+';
  fKTemplate.EndDelimiter := '+}';
  fKTemplate.ParamStartDelimiter := '#';
  fKTemplate.ParamValueSeparator := '[';
  fKTemplate.ParamEndDelimiter := ']#';
  fKTemplate.FileName := AFileName;

  fKTemplate.AllowTagParams := true;
  fKTemplate.OnReplaceTag := @ReplaceTag;
end;

destructor TWebView.Destroy;
var
  i, j: integer;
begin

  FreeAndNil(fDataItemsTagList);
  FreeAndNil(fDataItems);
  FreeAndNil(fKTemplate);
  FreeAndNil(fDataItem);
  inherited Destroy;
end;

{ globalview }

{view}

function TKyGlobalView.ReadDataItem(AName: string): string;
begin
  Result := fDataItem.Values[AName];
end;

procedure TKyGlobalView.WriteDataItem(AName, AValue: string);
begin
  fDataItem.Values[AName] := AValue;
end;

procedure TKyGlobalView.ReplaceTag(Sender : TObject; Const TagString : String;
      TagParams:TStringList; Out ReplaceText : String);
var
  ParamNames, ParamValues: array of string;
  i, j: integer;
begin
  if fDataItems.contains(TagString) then
  begin
    fDataItemsTagList.Add(TagString);
    if fDataItems[TagString].count > 0 then
    for i:= 0 to fDataItems[TagString].items[0].count - 1 do
    begin
      SetLength(ParamNames, i+1);
      SetLength(ParamValues, i+1);
      ParamNames[i] := fDataItems[TagString].items[0].Names[i];
    end;
    with TKItems.Create(TagParams.Values['iterable']) do
    begin
      for i := 0 to fDataItems[TagString].count - 1 do
      begin
        for j := 0 to Length(ParamValues)-1 do
        begin
          ParamValues[j] := fDataItems[TagString].items[i].Values[j];
        end;
        Add(ParamNames, ParamValues);
        ReplaceText := Text;
      end;
    end;
  end
  else
  begin
    ReplaceText := fDataItem.Values[TagString];
  end;
  if TagString = 'view_container' then
  begin
    ReplaceText := fContent;
  end;
end;

function TKyGlobalView.ReadDataItems(AName: string): TDataItems;
begin
  Result := fDataItems[AName];
end;

procedure TKyGlobalView.WriteDataItems(AName: string; AValues: TDataItems);
begin
  fDataItems[AName] := AValues;
end;

function TKyGlobalView.GetContent(AContent: string): string;
begin
  fContent := AContent;
  Result := fKTemplate.GetContent;
end;

constructor TKyGlobalView.Create(const AFileName: String);
begin
  fKTemplate := TFPTemplate.Create;
  fDataItem := TStringList.Create;
  fDataItems := TDataItemMap.Create;
  fDataItemsTagList := TStringList.Create;

  fKTemplate.StartDelimiter := '{+';
  fKTemplate.EndDelimiter := '+}';
  fKTemplate.ParamStartDelimiter := '#';
  fKTemplate.ParamValueSeparator := '[';
  fKTemplate.ParamEndDelimiter := ']#';
  fKTemplate.FileName := AFileName;

  fKTemplate.AllowTagParams := true;
  fKTemplate.OnReplaceTag := @ReplaceTag;
end;

destructor TKyGlobalView.Destroy;
var
  i, j: integer;
begin
  for i := 0 to fDataItemsTagList.Count-1 do
  begin
    for j := 0 to fDataItems[fDataItemsTagList[i]].count-1 do
    begin
      fDataItems[fDataItemsTagList[i]].items[j].Free;
    end;

  end;
  FreeAndNil(fDataItemsTagList);
  FreeAndNil(fDataItems);
  FreeAndNil(fKTemplate);
  FreeAndNil(fDataItem);
  inherited Destroy;
end;

initialization
  GlobalView := TGlobalViewMap.create;
finalization
  FreeAndNil(GlobalView);
end.

