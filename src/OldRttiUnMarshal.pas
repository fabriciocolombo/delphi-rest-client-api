unit OldRttiUnMarshal;

interface

uses TypInfo, SuperObject, Variants, SysUtils, Math, DbxJsonUtils, Classes;

type
  TOldRttiUnMarshal = class
  private
    function FromClass(AClassType: TClass; AJSONValue: ISuperObject): TObject;
    function FromList(AClassType: TClass; APropInfo: PPropInfo; const AJSONValue: ISuperObject): TList;
    function FromInt(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
    function FromInt64(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
    function FromFloat(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
    function FromString(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
    function FromChar(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
    function FromWideChar(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
  public
    class function FromJson(AClassType: TClass; const AJson: string): TObject;
  end;

implementation

{ TOldRttiUnMarshal }

function TOldRttiUnMarshal.FromChar(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
begin
  Result := Null;
  if ObjectIsType(AJSONValue, stString) and (Length(AJSONValue.AsString) = 1) then
  begin
    Result := Ord(AJSONValue.AsString[1]);
  end;
end;

function TOldRttiUnMarshal.FromClass(AClassType: TClass; AJSONValue: ISuperObject): TObject;
var
  i: Integer;
  vTypeInfo: PTypeInfo;
  vTypeData: PTypeData;
  vPropList: PPropList;
  vPropInfo: PPropInfo;
  value: Variant;
  vPropName: string;
  vInt64Value: Int64;
  vObjProp: TObject;
  vObjClass: TClass;
begin
  Result := nil;

  case ObjectGetType(AJSONValue) of
    stObject: begin
                Result := AClassType.Create;
                try
                  vTypeInfo := AClassType.ClassInfo;
                  vTypeData := GetTypeData(vTypeInfo);

                  New(vPropList);
                  try
                    GetPropList(vTypeInfo, tkProperties, vPropList);

                    for i := 0 to vTypeData^.PropCount-1 do
                    begin
                      vPropInfo := vPropList^[i];
                      vPropName := String(vPropInfo^.Name);

                      value := Null;
                      try
                        case vPropInfo^.PropType^.Kind of
                          tkMethod: ;
                          tkSet,
                          tkInteger,
                          tkEnumeration: begin
                                           value := FromInt(vPropInfo, AJSONValue.O[vPropName]);
                                           if not VarIsNull(value) then
                                           begin
                                             vInt64Value := value;
                                             SetOrdProp(Result, vPropInfo, vInt64Value);
                                             value := Null;
                                           end;
                                         end;
                          tkInt64: begin
                                     value := FromInt64(vPropInfo, AJSONValue.O[vPropName]);
                                     if not VarIsNull(value) then
                                     begin
                                       vInt64Value := value;
                                       SetInt64Prop(Result, vPropInfo, vInt64Value);
                                       value := Null;
                                     end;
                                   end;
                          tkFloat: value := FromFloat(vPropInfo, AJSONValue.O[vPropName]);
                          tkChar: value := FromChar(vPropInfo, AJSONValue.O[vPropName]);
                          tkWChar: value := FromWideChar(vPropInfo, AJSONValue.O[vPropName]);
                          tkString, tkLString,
                          {$IFDEF UNICODE}
                          tkUString,
                          {$ENDIF}
                          tkWString: value := FromString(vPropInfo, AJSONValue.O[vPropName]);
                          tkClass: begin
                                      value := Null;

                                      vObjClass := GetObjectPropClass(Result, vPropInfo);

                                      if vObjClass.InheritsFrom(TList) then
                                      begin
                                        vObjProp := FromList(vObjClass, vPropInfo, AJSONValue.O[vPropName])
                                      end
                                      else
                                      begin
                                        vObjProp := FromClass(vObjClass, AJSONValue.O[vPropName]);
                                      end;

                                      if Assigned(vObjProp) then
                                      begin
                                        SetObjectProp(Result, vPropInfo, vObjProp);
                                      end;
                                   end;
                        end;
                      except
                        on E: Exception do
                        begin
                          raise EJsonInvalidValueForField.CreateFmt('UnMarshalling error for field "%s.%s" : %s',
                                                                    [Result.ClassName, vPropName, E.Message]);
                        end;
                      end;

                      if not VarIsNull(value) then
                      begin
                        SetPropValue(Result, vPropName, value);
                      end;
                    end;
                  finally
                    Dispose(vPropList);
                  end;
                  except
                  Result.Free;
                  raise;
                end;
              end;
  end;
end;

function TOldRttiUnMarshal.FromFloat(APropInfo: PPropInfo;const AJSONValue: ISuperObject): Variant;
var
  o: ISuperObject;
begin
  Result := Null;
  case ObjectGetType(AJSONValue) of
    stInt, stDouble, stCurrency:
      begin
        if APropInfo^.PropType^ = System.TypeInfo(TDateTime) then
        begin
          Result := JavaToDelphiDateTime(AJSONValue.AsInteger);
        end
        else
        begin
          case GetTypeData(APropInfo^.PropType^).FloatType of
            ftSingle: Result := AJSONValue.AsDouble;
            ftDouble: Result := AJSONValue.AsDouble;
            ftExtended: Result := AJSONValue.AsDouble;
            ftCurr: Result := AJSONValue.AsCurrency;
          end;
        end;
      end;
    stString:
      begin
        o := SO(AJSONValue.AsString);
        if not ObjectIsType(o, stString) then
        begin
          Result := FromFloat(APropInfo, o);
        end;
      end
  end;
end;

function TOldRttiUnMarshal.FromInt(APropInfo: PPropInfo; const AJSONValue: ISuperObject): Variant;
var
   o: ISuperObject;
begin
  Result := Null;
  case ObjectGetType(AJSONValue) of
    stInt, stBoolean:
      begin
        Result := AJSONValue.AsInteger;
      end;
    stString:
      begin
        o := SO(AJSONValue.AsString);
        if not ObjectIsType(o, stString) then
        begin
          Result := FromInt(APropInfo, o);
        end;
      end;
  end;
end;

function TOldRttiUnMarshal.FromInt64(APropInfo: PPropInfo;const AJSONValue: ISuperObject): Variant;
var
  i: Int64;
begin
  Result := Null;
  case ObjectGetType(AJSONValue) of
    stInt:
      begin
        Result := AJSONValue.AsInteger;
      end;
    stString:
      begin
        if TryStrToInt64(AJSONValue.AsString, i) then
        begin
          Result := i;
        end;
      end;
  end;
end;

class function TOldRttiUnMarshal.FromJson(AClassType: TClass;const AJson: string): TObject;
var
  vUnMarshal: TOldRttiUnMarshal;
  vJsonObject: ISuperObject;
begin
  Result := nil;

  vUnMarshal := TOldRttiUnMarshal.Create;
  try
    vJsonObject := SO(AJson);

    if vJsonObject = nil then
    begin
      raise EJsonInvalidSyntax.CreateFmt('Invalid json: "%s"', [AJson]);
    end;

    Result := vUnMarshal.FromClass(AClassType, vJsonObject);
  finally
    vUnMarshal.Free;
  end;
end;

function TOldRttiUnMarshal.FromList(AClassType: TClass; APropInfo: PPropInfo; const AJSONValue: ISuperObject): TList;
var
  i: Integer;
  vPosList: Integer;
  vItemClassName: String;
  vItemClass: TClass;
  vItem: TObject;
begin
  Result := nil;
  if ObjectIsType(AJSONValue, stArray) then
  begin
    if AClassType.InheritsFrom(TList) and (AJSONValue.AsArray.Length > 0) then
    begin
      vPosList := Pos('ObjectList', APropInfo^.Name);
      if (vPosList = 0) then
      begin
        vPosList := Pos('List', APropInfo^.Name);
      end;

      vItemClassName := 'T' + Copy(APropInfo^.Name, 1, vPosList-1);

      vItemClass := FindClass(vItemClassName);

      Result := TList(AClassType.Create);
      try
        for i := 0 to AJSONValue.AsArray.Length-1 do
        begin
          vItem := FromClass(vItemClass, AJSONValue.AsArray.O[i]);

          Result.Add(vItem);
        end;
      except
        Result.Free;
        raise;
      end;
    end;
  end;
end;

function TOldRttiUnMarshal.FromString(APropInfo: PPropInfo;const AJSONValue: ISuperObject): Variant;
begin
  case ObjectGetType(AJSONValue) of
    stNull: Result := '';
    stString: Result := AJSONValue.AsString;
  else
    raise EJsonInvalidValue.CreateFmt('Invalid value "%s".', [AJSONValue.AsJSon]);
  end;
end;

function TOldRttiUnMarshal.FromWideChar(APropInfo: PPropInfo;const AJSONValue: ISuperObject): Variant;
begin
  Result := Null;
  if ObjectIsType(AJSONValue, stString) and (Length(AJSONValue.AsString) = 1) then
  begin
    Result := Ord(AJSONValue.AsString[1]);
  end;
end;

end.
