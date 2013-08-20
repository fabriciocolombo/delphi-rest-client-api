unit OldRttiMarshal;

interface

uses TypInfo, SuperObject, DbxJsonUtils, Classes;

type
  TOldRttiMarshal = class
  private
    function ToClass(AObject: TObject): ISuperObject;
    function ToList(AList: TList): ISuperObject;
    function ToInteger(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
    function ToInt64(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
    function ToFloat(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
    function ToJsonString(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
    function ToChar(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
  public
    class function ToJson(AObject: TObject): ISuperObject;
  end;

implementation

uses Variants, JsonListAdapter, SysUtils;

{ TOldRttiMarshal }

function TOldRttiMarshal.ToChar(AObject: TObject;APropInfo: PPropInfo): ISuperObject;
begin
  Result := TSuperObject.Create(Char(GetOrdProp(AObject, APropInfo)));
end;

function TOldRttiMarshal.ToClass(AObject: TObject): ISuperObject;
var
  i: Integer;
  vTypeInfo: PTypeInfo;
  vTypeData: PTypeData;
  vPropList: PPropList;
  vPropInfo: PPropInfo;
  value: ISuperObject;
  vObjProp: TObject;
  vAdapter: IJsonListAdapter;
begin
  if AObject = nil then
  begin
    Result := SuperObject.SO();
    Exit;
  end;

  if (AObject is TList) then
  begin
    Result := ToList(TList(AObject));
  end
  else if Supports(AObject, IJsonListAdapter, vAdapter) then
  begin
    Result := ToList(TList(vAdapter.UnWrapList));
  end
  else
  begin
    vTypeInfo := AObject.ClassInfo;

    if vTypeInfo = nil then
    begin
      raise ENoSerializableClass.Create(AObject.ClassType);
    end;

    vTypeData := GetTypeData(vTypeInfo);

    Result := SO();

    New(vPropList);
    try
      GetPropList(vTypeInfo, tkProperties, vPropList);

      for i := 0 to vTypeData^.PropCount-1 do
      begin
        vPropInfo := vPropList^[i];

        value := nil;

        case vPropInfo^.PropType^.Kind of
          tkMethod: ;
          tkSet, tkInteger, tkEnumeration: value := ToInteger(AObject, vPropInfo);
          tkInt64: value := ToInt64(AObject, vPropInfo);
          tkFloat: value := ToFloat(AObject, vPropInfo);
          tkChar, tkWChar: value := ToChar(AObject, vPropInfo); 
          tkString, tkLString,
          {$IFDEF UNICODE}
          tkUString,
          {$ENDIF}
          tkWString: value := ToJsonString(AObject, vPropInfo);
          tkClass: begin
                      value := nil;
                      vObjProp := GetObjectProp(AObject, vPropInfo);
                      if Assigned(vObjProp) then
                      begin
                        if vObjProp is TList then
                          value := ToList(TList(vObjProp))
                        else
                          value := ToClass(vObjProp);
                      end;
                   end;
        end;

        if Assigned(value) then
        begin
          Result.O[String(vPropInfo^.Name)] := value;
        end;
      end;
    finally
      Dispose(vPropList);
    end;
  end;
end;

function TOldRttiMarshal.ToFloat(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
begin
  if APropInfo^.PropType^ = System.TypeInfo(TDateTime) then
    Result := TSuperObject.Create(DelphiToJavaDateTime(GetFloatProp(AObject, APropInfo)))
  else
    Result := TSuperObject.Create(GetFloatProp(AObject, APropInfo));
end;

function TOldRttiMarshal.ToInt64(AObject: TObject;APropInfo: PPropInfo): ISuperObject;
begin
  Result := TSuperObject.Create(GetInt64Prop(AObject, APropInfo));
end;

function TOldRttiMarshal.ToInteger(AObject: TObject; APropInfo: PPropInfo): ISuperObject;
var
  vIntValue: Integer;
begin
  vIntValue := GetOrdProp(AObject, APropInfo);

  if APropInfo^.PropType^ = TypeInfo(Boolean) then
    Result := TSuperObject.Create(Boolean(vIntValue))
  else
    Result := TSuperObject.Create(vIntValue);
end;

class function TOldRttiMarshal.ToJson(AObject: TObject): ISuperObject;
var
  vMarshal: TOldRttiMarshal;
begin
  vMarshal := TOldRttiMarshal.Create;
  try
    Result := vMarshal.ToClass(AObject);
  finally
    vMarshal.Free;
  end;
end;

function TOldRttiMarshal.ToJsonString(AObject: TObject;APropInfo: PPropInfo): ISuperObject;
begin
  Result := TSuperObject.Create(GetWideStrProp(AObject, APropInfo));
end;

function TOldRttiMarshal.ToList(AList: TList): ISuperObject;
var
  i: Integer;
begin
  Result := TSuperObject.Create(stArray);

  for i := 0 to AList.Count-1 do
  begin
    Result.AsArray.Add(ToClass(TObject(AList.Items[i])));
  end;
end;

end.
