unit RestJsonOldRTTI;

interface

{$I DelphiRest.inc}

uses SuperObject;

type
  TJsonUtilOldRTTI = class
  public
    class function Marshal(entity: TObject): string;

    class function UnMarshal(text: String): TObject;
  end;

implementation

{ TJsonUtilOldRTTI }

class function TJsonUtilOldRTTI.Marshal(entity: TObject): string;
begin
  Result := entity.ClassName;
end;

class function TJsonUtilOldRTTI.UnMarshal(text: String): TObject;
begin
  Result := TObject.Create;
end;

end.
