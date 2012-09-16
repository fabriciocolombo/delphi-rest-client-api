unit JsonToDataSetConverter;

interface

uses DB, DBClient, SuperObject;

type
  TJsonToDataSetConverter = class
  private
    class procedure AppendRecord(ADataSet: TDataSet; AObject: ISuperObject);
    class procedure SetFieldValue(AField: TField; AValue: ISuperObject);
  public
    class procedure UnMarshalToDataSet(ADataSet: TDataSet; AJson: string);overload;
    class procedure UnMarshalToDataSet(ADataSet: TDataSet; AObject: ISuperObject);overload;
  end;

implementation

{ TJsonToDataSetConverter }

class procedure TJsonToDataSetConverter.AppendRecord(ADataSet: TDataSet;AObject: ISuperObject);
var
  vField: TField;
  vIterator: TSuperObjectIter;
begin
  ADataSet.Append;

  if SuperObject.ObjectFindFirst(AObject, vIterator) then
  begin
    try
      repeat
        vField := ADataSet.FindField(vIterator.key);

        if Assigned(vField) then
        begin
          SetFieldValue(vField, vIterator.val);
        end;
      until not SuperObject.ObjectFindNext(vIterator);
    finally
      SuperObject.ObjectFindClose(vIterator);
    end;
  end;

  ADataSet.Post;
end;

class procedure TJsonToDataSetConverter.SetFieldValue(AField: TField;AValue: ISuperObject);
var
  vFieldName: string;
  vDate: TDateTime;
  vNestedDataSet: TDataSet;
begin
  vFieldName := AField.FieldName;
  case AField.DataType of
    ftSmallint, ftInteger, ftWord, ftLargeint: AField.AsInteger := AValue.AsInteger;
    ftFloat, ftCurrency, ftBCD, ftFMTBcd: AField.AsFloat := AValue.AsDouble;
    ftBoolean: AField.AsBoolean := AValue.AsBoolean;
    ftDate, ftTime, ftDateTime, ftTimeStamp:  begin
                                                if SuperObject.ISO8601DateToDelphiDateTime(AValue.AsString, vDate) then
                                                begin
                                                  AField.AsDateTime := vDate;
                                                end;
                                              end;
    ftDataSet:  begin
                  vNestedDataSet := TDataSetField(AField).NestedDataSet;

                  UnMarshalToDataSet(vNestedDataSet, AValue);
                end;
  else
    AField.AsString := AValue.AsString;
  end;
end;

class procedure TJsonToDataSetConverter.UnMarshalToDataSet(ADataSet: TDataSet;AObject: ISuperObject);
var
  i: Integer;
  vArray: TSuperArray;
begin
  ADataSet.DisableControls;
  try
    if AObject.IsType(stArray) then
    begin
      vArray := AObject.AsArray;

      for I := 0 to vArray.Length-1 do
      begin
        AppendRecord(ADataSet, vArray.O[i]);
      end;
    end
    else
    begin
      AppendRecord(ADataSet, AObject);
    end;
  finally
    ADataSet.EnableControls;
  end;

  ADataSet.First;
end;

class procedure TJsonToDataSetConverter.UnMarshalToDataSet(ADataSet: TDataSet; AJson: string);
var
  AObject: ISuperObject;
begin
  AObject := SuperObject.SO(AJson);

  UnMarshalToDataSet(ADataSet, AObject);
end;

end.
