unit DataSetToJsonConverter;

interface

uses DB, DBClient, SuperObject, System.Variants;

type

  TDataSetToJsonConverter = class
  private
    class procedure CreateJsonValueByField(AJson: ISuperObject; AField: TField);
  public
    class procedure UnMarshalAllToJson(out AJson: string; ADataSet: TDataSet); overload;
    class procedure UnMarshalAllToJson(out AObject: ISuperObject; ADataSet: TDataSet); overload;

    class procedure UnMarshalCurrentToJson(out AJson: string; ADataSet: TDataSet); overload;
    class procedure UnMarshalCurrentToJson(out AObject: ISuperObject; ADataSet: TDataSet); overload;
  end;

implementation

{ TDataSetToJsonConverter }

class procedure TDataSetToJsonConverter.CreateJsonValueByField(AJson: ISuperObject; AField: TField);
var
  vNestedDataSet: TDataSet;
  vJson: string;
begin
  case AField.DataType of
    ftSmallint, ftInteger, ftWord, ftLargeint:
      AJson.O[AField.FieldName] := SO(AField.AsInteger);
    ftFloat, ftCurrency, ftBCD, ftFMTBcd:
      AJson.O[AField.FieldName] := SO(AField.AsFloat);
    ftBoolean:
      AJson.O[AField.FieldName] := SO(AField.AsBoolean);
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      AJson.O[AField.FieldName] := SO(AField.AsDateTime);
    ftDataSet:
      begin
        vNestedDataSet := TDataSetField(AField).NestedDataSet;

        UnMarshalAllToJson(vJson, vNestedDataSet);

        AJson.O[AField.FieldName] := SO(vJson);
      end;
  else
    AJson.O[AField.FieldName] := SO(AField.AsString);
  end;
end;

class procedure TDataSetToJsonConverter.UnMarshalAllToJson(out AJson: string; ADataSet: TDataSet);
var
  AJo: ISuperObject;
begin
  UnMarshalAllToJson(AJo, ADataSet);
  AJson := AJo.AsString;
end;

class procedure TDataSetToJsonConverter.UnMarshalAllToJson(out AObject: ISuperObject; ADataSet: TDataSet);
var
  vJa, vJo: ISuperObject;
  vBookMark: TBookmark;
  i: Integer;
begin
  try
    ADataSet.DisableControls;
    vBookMark := ADataSet.Bookmark;

    vJa := SA([]);

    ADataSet.First;
    while not ADataSet.Eof do
    begin
      vJo := SO();
      for i := 0 to Pred(ADataSet.FieldCount) do
      begin
        if VarIsNull(ADataSet.Fields[i].Value) then
          vJo.O[ADataSet.Fields[i].FieldName] := SO(Null)
        else
        begin
          CreateJsonValueByField(vJo, ADataSet.Fields[i]);
        end;
      end;
      vJa.AsArray.Add(vJo);
      ADataSet.Next;
    end;

    AObject := vJa;
  finally
    if ADataSet.BookmarkValid(vBookMark) then
      ADataSet.GotoBookmark(vBookMark);
    ADataSet.FreeBookmark(vBookMark);
    ADataSet.EnableControls;
  end;
end;

class procedure TDataSetToJsonConverter.UnMarshalCurrentToJson(out AJson: string; ADataSet: TDataSet);
var
  AJo: ISuperObject;
begin
  UnMarshalCurrentToJson(AJo, ADataSet);
  AJson := AJo.AsString;
end;

class procedure TDataSetToJsonConverter.UnMarshalCurrentToJson(out AObject: ISuperObject; ADataSet: TDataSet);
var
  vJo: ISuperObject;
  i: Integer;
begin
  try
    ADataSet.DisableControls;

    vJo := SO();

    if not ADataSet.IsEmpty then
    begin
      for i := 0 to Pred(ADataSet.FieldCount) do
      begin
        if VarIsNull(ADataSet.Fields[i].Value) then
          vJo.O[ADataSet.Fields[i].FieldName] := SO(Null)
        else
        begin
          CreateJsonValueByField(vJo, ADataSet.Fields[i]);
        end;
      end;
    end;

    AObject := vJo;
  finally
    ADataSet.EnableControls;
  end;
end;

end.
