unit JsonUnMarshal;

interface

uses System.Rtti, System.TypInfo, Data.DBXJson;

type
  TDBXJsonUnmarshal = class
  private
    FContext: TRttiContext;

    function CreateInstance(ATypeInfo: PTypeInfo): TValue;
    function IsList(ATypeInfo: PTypeInfo): Boolean;
    function IsParameterizedType(ATypeInfo: PTypeInfo): Boolean;
    function GetParameterizedType(ATypeInfo: PTypeInfo): TRttiType;

    function FromJson(ATypeInfo: PTypeInfo; AJSONValue: TJSONValue): TValue;overload;

    function FromClass(ATypeInfo: PTypeInfo; AJSONValue: TJSONValue): TValue;
    function FromList(ATypeInfo: PTypeInfo; AJSONValue: TJSONValue): TValue;
    function FromString(const value: TJSONValue): TValue;
    function FromInt(ATypeInfo: PTypeInfo; const value: TJSONValue): TValue;
    function FromInt64(ATypeInfo: PTypeInfo; const value: TJSONValue): TValue;
    function FromFloat(ATypeInfo: PTypeInfo; const value: TJSONValue): TValue;
  public
    constructor Create;
    destructor Destroy; override;

    class function FromJson<T>(AJSONValue: TJSONValue): T;overload;
  end;

implementation

{ TDBXJsonUnmarshal }

constructor TDBXJsonUnmarshal.Create;
begin
  FContext := TRttiContext.Create;
end;

function TDBXJsonUnmarshal.CreateInstance(ATypeInfo: PTypeInfo): TValue;
var
  rType: TRttiType;
  mType: TRTTIMethod;
  metaClass: TClass;
begin
  rType := FContext.GetType(ATypeInfo);
  if ( rType <> nil ) then
  begin
    for mType in rType.GetMethods do
    begin
      if mType.HasExtendedInfo and mType.IsConstructor and (Length(mType.GetParameters) = 0) then
      begin
        metaClass := rType.AsInstance.MetaclassType;
        exit(mType.Invoke(metaClass, []).AsObject);
      end;
    end;
  end;

  raise Exception.CreateFmt('No default constructor found for clas "%s".', [GetTypeData(ATypeInfo).ClassType.ClassName]);
end;

destructor TDBXJsonUnmarshal.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TDBXJsonUnmarshal.FromClass(ATypeInfo: PTypeInfo;AJSONValue: TJSONValue): TValue;
var
  f: TRttiField;
  v: TValue;
  vJsonObject: TJSONObject;
  vFieldPair: TJSONPair;
  vOldValue: TValue;
begin
  if AJSONValue is TJSONObject then
  begin
    vJsonObject := TJSONObject(AJSONValue);

    Result := CreateInstance(ATypeInfo);

    for f in FContext.GetType(Result.AsObject.ClassType).GetFields do
    begin
      if f.FieldType <> nil then
      begin
        vFieldPair := vJsonObject.Get(f.Name);

        v := FromJson(f.FieldType.Handle, vFieldPair.JsonValue);

        if not v.IsEmpty then
        begin
          vOldValue := f.GetValue(Result.AsObject);

          if not vOldValue.IsEmpty and vOldValue.IsObject then
          begin
            vOldValue.AsObject.Free;
          end;

          f.SetValue(Result.AsObject, v);
        end;
      end;
    end;
  end
  else if AJsonValue is TJsonArray then
  begin
    if IsList(ATypeInfo) and IsParameterizedType(ATypeInfo) then
    begin
      Result := FromList(ATypeInfo, AJSONValue);
    end;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TDBXJsonUnmarshal.FromFloat(ATypeInfo: PTypeInfo; const value: TJSONValue): TValue;
var
  vNumber: TJSONNumber;
  vTemp: TJSONValue;
begin
  if value is TJSONNumber then
  begin
    vNumber := TJSONNumber(value);

    if ATypeInfo = TypeInfo(TDateTime) then
    begin
      Result := JavaToDelphiDateTime(vNumber.AsInt64);
    end
    else
    begin
      TValue.Make(nil, ATypeInfo, Result);

      case GetTypeData(ATypeInfo).FloatType of
        ftSingle: TValueData(Result).FAsSingle := vNumber.AsDouble;
        ftDouble: TValueData(Result).FAsDouble := vNumber.AsDouble;
        ftExtended: TValueData(Result).FAsExtended := vNumber.AsDouble;
        ftComp: TValueData(Result).FAsSInt64 := vNumber.AsInt;
        ftCurr: TValueData(Result).FAsCurr := vNumber.AsDouble;
      end;
    end;
  end
  else if value is TJSONString then
  begin
    vTemp := TJSONObject.ParseJSONValue(TJSONString(value).Value);

    if vTemp is TJSONNumber then
      Result := FromFloat(ATypeInfo, vTemp)
    else
      Result := TValue.Empty;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TDBXJsonUnmarshal.FromInt(ATypeInfo: PTypeInfo;const value: TJSONValue): TValue;
  var
    TypeData: PTypeData;
    i: Integer;
    vIsValid: Boolean;
    vTemp: TJSONValue;
  begin
    if (value is TJSONNumber) then
    begin
      i := TJSONNumber(value).AsInt;

      TypeData := GetTypeData(ATypeInfo);
      if TypeData.MaxValue > TypeData.MinValue then
        vIsValid := (i >= TypeData.MinValue) and (i <= TypeData.MaxValue)
      else
        vIsValid := (i >= TypeData.MinValue) and (i <= Int64(PCardinal(@TypeData.MaxValue)^));

      if vIsValid then
        TValue.Make(@i, ATypeInfo, Result);
    end
    else if value is TJSONTrue then
    begin
      Result := Ord(True);
    end
    else if value is TJSONFalse then
    begin
      Result := Ord(False);
    end
    else if value is TJSONString then
    begin
      vTemp := TJSONObject.ParseJSONValue(TJSONString(value).Value);
      if vTemp is TJSONNumber then
        Result := FromInt(ATypeInfo, value)
      else
        Result := TValue.Empty;
    end;
end;

function TDBXJsonUnmarshal.FromInt64(ATypeInfo: PTypeInfo;const value: TJSONValue): TValue;
var
  i: Int64;
begin
  if value is TJSONNumber then
  begin
    TValue.Make(nil, ATypeInfo, Result);
    TValueData(Result).FAsSInt64 := TJSONNumber(value).AsInt64;
  end
  else if (value is TJSONString) and TryStrToInt64(TJSONString(value).Value, i) then
  begin
    TValue.Make(nil, ATypeInfo, Result);
    TValueData(Result).FAsSInt64 := i;
  end
  else
  begin
    Result := TValue.Empty;
  end;
end;

function TDBXJsonUnmarshal.FromJson(ATypeInfo: PTypeInfo; AJSONValue: TJSONValue): TValue;
begin
  if AJSONValue is TJSONNull then
  begin
    Result := TValue.Empty;
  end
  else
  begin
    case ATypeInfo.Kind of
  //    tkChar: Result := FromChar;
      tkInt64: Result := FromInt64(ATypeInfo, AJSONValue);
      tkEnumeration, tkInteger: Result := FromInt(ATypeInfo, AJSONValue);
  //    tkSet: Result := fromSet;
      tkFloat: Result := FromFloat(ATypeInfo, AJSONValue);
      tkString, tkLString, tkUString, tkWString: Result := FromString(AJSONValue);
      tkClass: Result := FromClass(ATypeInfo, AJSONValue);
  //    tkMethod: ;
  //    tkWChar: Result := FromWideChar;
  //    tkRecord: Result := FromRecord;
  //    tkPointer: ;
  //    tkInterface: Result := FromInterface;
  //    tkArray: Result := FromArray;
  //    tkDynArray: Result := FromDynArray;
  //    tkClassRef: Result := FromClassRef;
  //  else
  //    Result := FromUnknown;
    else
      Result := TValue.Empty;
    end;
  end;
end;

class function TDBXJsonUnmarshal.FromJson<T>(AJSONValue: TJSONValue): T;
var
  vUnmarshal: TDBXJsonUnmarshal;
begin
  vUnmarshal := TDBXJsonUnmarshal.Create;
  try
    Result := vUnmarshal.FromJson(TypeInfo(T), AJSONValue).AsType<T>;
  finally
    vUnmarshal.Free;
  end;
end;

function TDBXJsonUnmarshal.FromList(ATypeInfo: PTypeInfo; AJSONValue: TJSONValue): TValue;
var
  method: TRttiMethod;
  vJsonArray: TJSONArray;
  vJsonValue: TJSONValue;
  vItem: TValue;
begin
  Result := CreateInstance(ATypeInfo);

  method := FContext.GetType(ATypeInfo).GetMethod('Add');

  vJsonArray := TJSONArray(AJSONValue);

  for vJsonValue in vJsonArray do
  begin
    vItem := FromJson(GetParameterizedType(ATypeInfo).Handle, vJsonValue);

    if not vItem.IsEmpty then
    begin
      method.Invoke(Result.AsObject, [vItem])
    end;
  end;
end;

function TDBXJsonUnmarshal.FromString(const value: TJSONValue): TValue;
begin
  if value is TJSONNull then
  begin
    Result := ''
  end
  else
  begin
    Result := TJSONString(value).Value;
  end;
end;

function TDBXJsonUnmarshal.GetParameterizedType(ATypeInfo: PTypeInfo): TRttiType;
var
  startPos,
  endPos: Integer;
  vTypeName: String;
begin
  Result := nil;

  startPos := AnsiPos('<', String(ATypeInfo.Name));

  if startPos > 0 then
  begin
    endPos := Pos('>', String(ATypeInfo.Name));

    vTypeName := Copy(String(ATypeInfo.Name), startPos + 1, endPos - Succ(startPos));

    Result := FContext.FindType(vTypeName);
  end;
end;

function TDBXJsonUnmarshal.IsList(ATypeInfo: PTypeInfo): Boolean;
var
  method: TRttiMethod;
begin
  method := FContext.GetType(ATypeInfo).GetMethod('Add');

  Result := (method <> nil) and
            (method.MethodKind = mkFunction) and
            (Length(method.GetParameters) = 1)
end;

function TDBXJsonUnmarshal.IsParameterizedType(ATypeInfo: PTypeInfo): Boolean;
var
  vStartPos: Integer;
begin
  vStartPos := Pos('<', String(ATypeInfo.Name));
  Result := (vStartPos > 0) and (PosEx('>', String(ATypeInfo.Name), vStartPos) > 0);
end;

end.
