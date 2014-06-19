unit RestJsonUtils;

interface

{$I DelphiRest.inc}

uses {$IFDEF USE_GENERICS}RestJsonGenerics, {$ENDIF}
     {$IFDEF USE_SUPER_OBJECT}RestJsonOldRTTI, {$ENDIF}
     SysUtils, DateUtils, TypInfo;

type
  TJsonUtil = class
  public
    class function Marshal(entity: TObject): string;

    {$IFDEF USE_GENERICS}
    class function UnMarshal<T>(AJsonText: String): T;overload;
    {$ENDIF}
    class function UnMarshal(AClassType: TClass; AJsonText: String): TObject;overload;
  end;

implementation

{ TJsonUtil }

class function TJsonUtil.Marshal(entity: TObject): string;
begin
{$IFDEF USE_GENERICS}
  Result := TJsonUtilGenerics.Marshal(entity);
{$ELSE}
  Result := TJsonUtilOldRTTI.Marshal(entity);
{$ENDIF}
end;

class function TJsonUtil.UnMarshal(AClassType: TClass;AJsonText: String): TObject;
begin
{$IFDEF USE_GENERICS}
  Result := TJsonUtilGenerics.UnMarshal(AClassType, AJsonText);
{$ELSE}
  Result := TJsonUtilOldRTTI.UnMarshal(AClassType, AJsonText);
{$ENDIF}
end;

{$IFDEF USE_GENERICS}
class function TJsonUtil.UnMarshal<T>(AJsonText: String): T;
begin
  Result := TJsonUtilGenerics.UnMarshal<T>(AJsonText);
end;
{$ENDIF}

end.
