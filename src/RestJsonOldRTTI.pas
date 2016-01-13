unit RestJsonOldRTTI;

{.$I DelphiRest.inc}

interface

uses OldRttiMarshal, OldRttiUnMarshal;

type
  TJsonUtilOldRTTI = class
  public
    class function Marshal(entity: TObject): string;

    class function UnMarshal(AClassType: TClass; AJsonText: String): TObject;
  end;

implementation

{ TJsonUtilOldRTTI }

class function TJsonUtilOldRTTI.Marshal(entity: TObject): string;
begin
  Result := TOldRttiMarshal.ToJson(entity).AsJSon();
end;

class function TJsonUtilOldRTTI.UnMarshal(AClassType: TClass; AJsonText: String): TObject;
begin
  Result := TOldRttiUnMarshal.FromJson(AClassType, AJsonText);
end;

end.
