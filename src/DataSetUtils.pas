unit DataSetUtils;

interface

uses DB, SysUtils;

type
  TDataSetUtils = class
  public
    class function CreateField(DataSet: TDataSet; FieldType: TFieldType; const FieldName: string = ''; ASize: Integer=0; ADisplayWidth: Integer = 30): TField;
    class function CreateDataSetField(DataSet: TDataSet; const FieldName: string): TDataSetField;
    class function RetFieldName(DataSet: TDataSet; FieldName: string): string;
  end;

implementation

{ TDataSetUtils }

class function TDataSetUtils.CreateDataSetField(DataSet: TDataSet;const FieldName: string): TDataSetField;
begin
  Result := TDataSetField(CreateField(DataSet, ftDataSet, RetFieldName(DataSet, FieldName)));
end;

class function TDataSetUtils.CreateField(DataSet: TDataSet;
  FieldType: TFieldType; const FieldName: string; ASize: Integer; ADisplayWidth: Integer): TField;
var
  fieldClass: TFieldClass;
begin
    fieldClass := DefaultFieldClasses[FieldType];
    if (fieldClass = nil) then
    begin
      fieldClass := TStringField;
      ASize := 255;
    end;

    Result:= fieldClass.Create(DataSet);
    Result.FieldName:= RetFieldName(DataSet, FieldName);

    if Result.FieldName = '' then
      Result.FieldName:= 'Field' + IntToStr(DataSet.FieldCount +1);
    Result.FieldKind := fkData;
    Result.DataSet:= DataSet;
    Result.Name:= DataSet.Name + Result.FieldName;
    Result.Size := ASize;
    if (FieldType = ftString) then
      Result.DisplayWidth := ADisplayWidth;

    if (FieldType = ftString) and (ASize <= 0) then
      raise Exception.CreateFmt('Size não definido para o campo "%s".',[FieldName]);
end;

class function TDataSetUtils.RetFieldName(DataSet: TDataSet; FieldName: string): string;

  function RetAlfanum(str: string): string;
  var
    i: integer;
  begin
    Result := '';
    for i:=1 to Length(str) do
    begin
      case str[i] of
        'A'..'Z', 'a'..'z', '0'..'9', '_': Result := Result + str[i];
      end;
    end;
  end;

begin
  Result := RetAlfanum( FieldName );

  if Length(Result) > 31 then
    Result := Copy(Result,1,28) + Format('%3.3d',[DataSet.FieldCount + 1])
end;

end.
