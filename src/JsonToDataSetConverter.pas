unit JsonToDataSetConverter;

interface

uses DB, DBClient, SuperObject;

type
  TJsonToDataSetConverter = class
  private
    class procedure AppendRecord(ADataSet: TDataSet; AObject: ISuperObject);
  public
    class procedure UnMarshalToDataSet(ADataSet: TDataSet; AJson: string);
  end;

implementation

{ TJsonToDataSetConverter }

class procedure TJsonToDataSetConverter.AppendRecord(ADataSet: TDataSet;AObject: ISuperObject);
var
  i: Integer;
  vField: TField;
  vDate: TDateTime;
begin
  ADataSet.Append;
  for i := 0 to ADataSet.FieldCount-1 do
  begin
    vField := ADataSet.Fields[i];

    case vField.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint: vField.AsInteger := AObject.I[vField.FieldName];
      ftFloat, ftCurrency, ftBCD, ftFMTBcd: vField.AsFloat := AObject.D[vField.FieldName];
      ftBoolean: vField.AsBoolean := AObject.B[vField.FieldName];
      ftDate, ftTime, ftDateTime, ftTimeStamp:  begin
                                                  if SuperObject.ISO8601DateToDelphiDateTime(AObject.S[vField.FieldName], vDate) then
                                                  begin
                                                    vField.AsDateTime := vDate;
                                                  end;
                                                end;
      ftDataSet: ;
    else
      vField.AsString := AObject.S[vField.FieldName];
    end;
  end;
  ADataSet.Post;
end;

class procedure TJsonToDataSetConverter.UnMarshalToDataSet(ADataSet: TDataSet; AJson: string);
var
  i: Integer;
  obj: ISuperObject;
  vArray: TSuperArray;
begin
  obj := SuperObject.SO(AJson);

  if obj.IsType(stArray) then
  begin
    vArray := obj.AsArray;

    for I := 0 to vArray.Length-1 do
    begin
      AppendRecord(ADataSet, vArray.O[i]);
    end;
  end
  else
  begin
    AppendRecord(ADataSet, obj);
  end;
end;

end.
