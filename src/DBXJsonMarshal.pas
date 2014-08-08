unit DBXJsonMarshal;

interface

uses Rtti, TypInfo, DBXJson, DbxJsonUtils, DBXJsonHelpers;

type
  TDBXJsonMarshal = class
  private
    FContext: TRttiContext;

    function IsList(ATypeInfo: PTypeInfo): Boolean;

    function ToJson(field: TRttiField; var AValue: TValue): TJSONValue;overload;

    function ToClass(AValue: TValue): TJSONValue;
    function ToRecord(AValue: TValue): TJSONValue;
    function ToInteger(AValue: TValue): TJSONValue;
    function ToInt64(AValue: TValue): TJSONValue;
    function ToChar(AValue: TValue): TJSONValue;
    function ToFloat(field: TRttiField; AValue: TValue): TJSONValue;
    function ToJsonString(AValue: TValue): TJSONValue;
    function ToWideChar(AValue: TValue): TJSONValue;
    function ToDynArray(field: TRttiField; AValue: TValue): TJSONValue;
  public
    constructor Create;
    destructor Destroy; override;

    class function ToJson(AObject: TObject): TJSONValue;overload;
    class function ToJsonText(AObject: TObject): string;
  end;

implementation

uses StrUtils;

{ TDBXJsonMarshal }

constructor TDBXJsonMarshal.Create;
begin
  FContext := TRttiContext.Create;
end;

destructor TDBXJsonMarshal.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TDBXJsonMarshal.IsList(ATypeInfo: PTypeInfo): Boolean;
var
  method: TRttiMethod;
begin
  method := FContext.GetType(ATypeInfo).GetMethod('Add');

  Result := (method <> nil) and
            (method.MethodKind = mkFunction) and
            (Length(method.GetParameters) = 1)
end;

function TDBXJsonMarshal.ToJson(field: TRttiField; var AValue: TValue): TJSONValue;
begin
  case AValue.Kind of
    tkInt64: Result := ToInt64(AValue);
    tkChar: Result := ToChar(AValue);
    tkSet, tkInteger, tkEnumeration: Result := ToInteger(AValue);
    tkFloat: Result := ToFloat(field, AValue);
    tkString, tkLString, tkUString, tkWString: Result := ToJsonString(AValue);
    tkClass: Result := ToClass(AValue);
    tkWChar: Result := ToWideChar(AValue);
//    tkVariant: ToVariant;
    tkRecord: Result := ToRecord(AValue);
//    tkArray: ToArray;
    tkDynArray: Result := ToDynArray(field, AValue);
//    tkClassRef: ToClassRef;
//    tkInterface: ToInterface;
  else
    result := nil;
  end;
end;

function TDBXJsonMarshal.ToChar(AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(String(AValue.AsType<Char>));
end;

function TDBXJsonMarshal.ToClass(AValue: TValue): TJSONValue;
var
  f: TRttiField;
  fieldValue: TValue;
  vJsonObject: TJSONObject;
  vJsonValue: TJSONValue;
  vIsList: Boolean;
begin
  Result := nil;

  if AValue.IsObject and (AValue.AsObject <> nil) then
  begin
    vIsList := IsList(AValue.TypeInfo);

    vJsonObject := nil;
    if not vIsList then
    begin
      vJsonObject := TJSONObject.Create;
    end;

    for f in FContext.GetType(AValue.AsObject.ClassType).GetFields do
    begin
      if (f.FieldType <> nil) (* and (f.Visibility in [mvPublic, mvPublished])*) then
      begin
        fieldValue := f.GetValue(AValue.AsObject);

        if fieldValue.IsObject and (fieldValue.AsObject = nil) then
        begin
          Continue;          
        end;
        
        if vIsList then
        begin
          if (f.Name = 'FItems') then
          begin
            Exit(ToJson(f, fieldValue));
          end;
          Continue;
        end;
        
        vJsonValue := ToJson(f, fieldValue);

        if vJsonValue <> nil then
        begin
          vJsonObject.AddPair(TJSONPair.Create(f.GetFieldName, vJsonValue));
        end;
      end;
    end;
    Result := vJsonObject;
  end;
end;

function TDBXJsonMarshal.ToDynArray(field: TRttiField; AValue: TValue): TJSONValue;
var
  i: Integer;
  v: TValue;
begin
  Result := TJSONArray.Create;
  for i := 0 to AValue.GetArrayLength - 1 do
  begin
    v := AValue.GetArrayElement(i);
    if not v.IsEmpty then
    begin
    
      Result.AsJsonArray.AddElement(toJSon(field, v));
    end;
  end;
end;

function TDBXJsonMarshal.ToFloat(field: TRttiField; AValue: TValue): TJSONValue;
begin
  Result := nil;
  if AValue.TypeInfo = TypeInfo(TDateTime) then
  begin
    if TValueData(AValue).FAsDouble > 0 then
    begin
      if field.FormatUsingISO8601 then
        Result := TJSONString.Create(DelphiDateTimeToISO8601Date(AValue.AsType<TDateTime>))
      else
        Result := TJSONNumber.Create(DelphiToJavaDateTime(AValue.AsType<TDateTime>));
    end;
  end
  else
  begin 
    case AValue.TypeData.FloatType of
      ftSingle: Result := TJSONNumber.Create(TValueData(AValue).FAsSingle);
      ftDouble: Result := TJSONNumber.Create(TValueData(AValue).FAsDouble);
      ftExtended: Result := TJSONNumber.Create(TValueData(AValue).FAsExtended);
      ftComp: Result := TJSONNumber.Create(TValueData(AValue).FAsSInt64);
      ftCurr: Result := TJSONNumber.Create(TValueData(AValue).FAsCurr);    
    end;
  end;
end;

function TDBXJsonMarshal.ToInt64(AValue: TValue): TJSONValue;
begin
  Result := TJSONNumber.Create(AValue.AsInt64);
end;

function TDBXJsonMarshal.ToInteger(AValue: TValue): TJSONValue;
begin
  if AValue.TypeInfo = TypeInfo(Boolean) then
  begin
    if AValue.AsBoolean then
      Result := TJSONTrue.Create
    else
      Result := TJSONFalse.Create;
  end
  else
  begin
    Result := TJSONNumber.Create(TValueData(AValue).FAsSLong);
  end;
end;

class function TDBXJsonMarshal.ToJson(AObject: TObject): TJSONValue;
var
  vMarshal: TDBXJsonMarshal;
  v: TValue;
begin
  vMarshal := TDBXJsonMarshal.Create;
  try
    v := AObject;
    
    Result := vMarshal.ToJson(nil, v);
  finally
    vMarshal.Free;
  end;
end;

class function TDBXJsonMarshal.ToJsonText(AObject: TObject): string;
var
  vJson: TJSONValue;
begin
  vJson := ToJson(AObject);
  try
    Result := vJson.ToString;
  finally
    vJson.Free;
  end;
end;

function TDBXJsonMarshal.ToRecord(AValue: TValue): TJSONValue;
var
  f: TRttiField;
  fieldValue: TValue;
  vJsonObject: TJSONObject;
  vJsonValue: TJSONValue;
begin
  Result := nil;

  if AValue.Kind = tkRecord then
  begin
    vJsonObject := TJSONObject.Create;

    for f in FContext.GetType(AValue.TypeInfo).GetFields do
    begin
      if (f.FieldType <> nil) then
      begin
        {$IFDEF VER210}
          fieldValue := f.GetValue(IValueData(TValueData(AValue).FHeapData).GetReferenceToRawData);
        {$ELSE}
          fieldValue := f.GetValue(TValueData(AValue).FValueData.GetReferenceToRawData);
        {$ENDIF}

        if fieldValue.IsObject and (fieldValue.AsObject = nil) then
        begin
          Continue;
        end;

        vJsonValue := ToJson(f, fieldValue);

        if vJsonValue <> nil then
        begin
          vJsonObject.AddPair(TJSONPair.Create(f.GetFieldName, vJsonValue));
        end;
      end;
    end;
    Result := vJsonObject;
  end;
end;

function TDBXJsonMarshal.ToJsonString(AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsString);
end;

function TDBXJsonMarshal.ToWideChar(AValue: TValue): TJSONValue;
begin
  Result := TJSONString.Create(AValue.AsType<Char>);
end;

end.
