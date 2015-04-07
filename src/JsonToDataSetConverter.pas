unit JsonToDataSetConverter;

interface

uses DB, DBClient, SuperObject;

type
  TJsonToDataSetConverter = class
  private
    class procedure AppendRecord(ADataSet: TDataSet; AObject: ISuperObject);
    class procedure SetFieldValue(AField: TField; AValue: ISuperObject);

    class procedure ExtractFields(ADataSet: TDataSet; AObject: ISuperObject);
    class procedure ExtractStructure(ADataSet: TDataSet; AObject: ISuperObject; tableNode, titleNode: string);

    class function SuperTypeToFieldType(ASuperType: TSuperType): TFieldType;
    class function SuperTypeToFieldSize(ASuperType: TSuperType): Integer;
  public
    class procedure UnMarshalToDataSet(ADataSet: TClientDataSet; AJson: string);
    class procedure ToDataSet(ADataSet: TClientDataSet; AObject: ISuperObject);

    class function CreateDataSetMetadata(AJson: string): TClientDataSet; overload;
    class function CreateDataSetMetadata(AObject: ISuperObject): TClientDataSet; overload;
    class function CreateDataSetMetadata(AObject: ISuperObject; table, structure: string): TClientDataSet; overload;
  end;

implementation

{ TJsonToDataSetConverter }

uses DataSetUtils;

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

class function TJsonToDataSetConverter.CreateDataSetMetadata(AJson: string): TClientDataSet;
var
  AObject: ISuperObject;
begin
  AObject := SuperObject.SO(AJson);

  Result := CreateDataSetMetadata(AObject);
end;

class function TJsonToDataSetConverter.CreateDataSetMetadata(AObject: ISuperObject): TClientDataSet;
var
  vArray: TSuperArray;
begin
  Result := TClientDataSet.Create(nil);

  if AObject.IsType(stArray) then
  begin
    vArray := AObject.AsArray;

    ExtractFields(Result, vArray.O[0]);
  end
  else
  begin
    ExtractFields(Result, AObject);
  end;

  Result.CreateDataSet;
end;

class function TJsonToDataSetConverter.CreateDataSetMetadata(AObject: ISuperObject; table, structure: string): TClientDataSet;
var
  vArray: TSuperArray;
begin
  Result := TClientDataSet.Create(nil);

  if AObject.IsType(stArray) then
  begin
    vArray := AObject.O[table].AsArray;

    ExtractStructure(Result, AObject, table, structure);
  end
  else
  begin
    ExtractStructure(Result, AObject, table, structure);
  end;
  if Result.Fields.Count > 0 then
    Result.CreateDataSet;
end;

class procedure TJsonToDataSetConverter.ExtractFields(ADataSet: TDataSet;AObject: ISuperObject);
var
  vIterator: TSuperObjectIter;
  vNestedField: TDataSetField;
  vArray: TSuperArray;
begin
  if SuperObject.ObjectFindFirst(AObject, vIterator) then
  begin
    try
      repeat
        if (vIterator.val.IsType(stArray)) then
        begin
          vNestedField := TDataSetUtils.CreateDataSetField(ADataSet, vIterator.key);

          vArray := vIterator.val.AsArray;
          if (vArray.Length > 0) then
          begin
            ExtractFields(vNestedField.NestedDataSet, vArray[0]);
          end;
        end
        else
        begin
          TDataSetUtils.CreateField(ADataSet, SuperTypeToFieldType(vIterator.val.DataType), vIterator.key, SuperTypeToFieldSize(vIterator.val.DataType));
        end;
      until not SuperObject.ObjectFindNext(vIterator);
    finally
      SuperObject.ObjectFindClose(vIterator);
    end;
  end;
end;

class procedure TJsonToDataSetConverter.ExtractStructure(ADataSet: TDataSet; AObject: ISuperObject; tableNode, titleNode: string);

var
  vIterator: TSuperObjectIter;
  vNestedField: TDataSetField;
  vArray: TSuperArray;
  table: TSuperArray;
  I: Integer;
begin
  i:= 0;
  table:= AObject.o[tableNode].AsArray;
  if SuperObject.ObjectFindFirst(table[0], vIterator) then
  begin
    try
      repeat
        if (vIterator.val.IsType(stArray)) then
        begin
          vNestedField := TDataSetUtils.CreateDataSetField(ADataSet, vIterator.key);

          vArray := vIterator.val.AsArray;
          if (vArray.Length > 0) then
          begin
            ExtractFields(vNestedField.NestedDataSet, vArray[0]);
          end;
        end
        else
        begin
          TDataSetUtils.CreateField(ADataSet, SuperTypeToFieldType(vIterator.val.DataType), vIterator.key,
          SuperTypeToFieldSize(vIterator.val.DataType), AObject.o[titleNode].S[vIterator.key]);
        end;
      until not SuperObject.ObjectFindNext(vIterator);
    finally
      SuperObject.ObjectFindClose(vIterator);
    end;
  end;

end;

class procedure TJsonToDataSetConverter.SetFieldValue(AField: TField;AValue: ISuperObject);
var
  vFieldName: string;
  vNestedDataSet: TClientDataSet;
begin
    vFieldName := AField.FieldName;
    if (AValue.IsType(stNull)) then
      begin
        vNestedDataSet := nil;
        exit;
      end;
    case AField.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint: AField.AsInteger := AValue.AsInteger;
      ftFloat, ftCurrency, ftBCD, ftFMTBcd: AField.AsFloat := AValue.AsDouble;
      ftBoolean: AField.AsBoolean := AValue.AsBoolean;
      ftDate, ftTime, ftDateTime, ftTimeStamp: AField.AsDateTime := AValue.AsDouble;
      ftDataSet:  begin
                    vNestedDataSet := TClientDataSet( TDataSetField(AField).NestedDataSet);

                    ToDataSet(vNestedDataSet, AValue);
                  end;
    else
      AField.AsString := AValue.AsString;
    end;
end;

class function TJsonToDataSetConverter.SuperTypeToFieldSize(ASuperType: TSuperType): Integer;
begin
  Result := 0;

  if (ASuperType = stNull) or (ASuperType = stString) then	// Some fields return as null
  begin
    Result := 255;
  end;
end;

class function TJsonToDataSetConverter.SuperTypeToFieldType(ASuperType: TSuperType): TFieldType;
begin
  case ASuperType of
    stBoolean: Result := ftBoolean;
    stDouble: Result := ftFloat;
    stCurrency: Result := ftCurrency;
    stInt: Result := ftInteger;
    stObject: Result := ftDataSet;
    stArray: Result := ftDataSet;
    stString: Result := ftString;
    stNull: Result:= ftstring;	// Rather than fail with an unknown type
  else
    Result := ftUnknown;
  end;
end;

class procedure TJsonToDataSetConverter.ToDataSet(ADataSet: TClientDataSet; AObject: ISuperObject);
var
  i: Integer;
  vArray: TSuperArray;
begin
  if (ADataSet.FieldDefs.Count = 0) then // Check for whether any data is returned
    EXIT;
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

class procedure TJsonToDataSetConverter.UnMarshalToDataSet(ADataSet: TClientDataSet; AJson: string);
var
  AObject: ISuperObject;
begin
  AObject := SuperObject.SO(AJson);

  ToDataSet(ADataSet, AObject);
end;

end.
