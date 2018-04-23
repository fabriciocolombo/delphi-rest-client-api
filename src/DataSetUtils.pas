unit DataSetUtils;

interface

uses DB, SysUtils;

type
  TDataSetUtils = class
  public
    class function CreateField(DataSet: TDataSet; FieldType: TFieldType; const FieldName: string = ''; ASize: Integer=0; ADisplayWidth: Integer = 30): TField;
    class function CreateDataSetField(DataSet: TDataSet; const FieldName: string): TDataSetField;
  end;

implementation

{ TDataSetUtils }

class function TDataSetUtils.CreateDataSetField(DataSet: TDataSet;const FieldName: string): TDataSetField;
begin
  Result := TDataSetField(CreateField(DataSet, ftDataSet, FieldName));
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

    Result:= fieldClass.Create(DataSet);    Result:= DefaultFieldClasses[FieldType].Create(DataSet);
    Result.FieldName:= FieldName;
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

end.
