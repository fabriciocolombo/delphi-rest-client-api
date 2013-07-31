unit RestJsonGenerics;

interface

{$I DelphiRest.inc}

uses Generics.Collections,
     {$IFDEF USE_SUPER_OBJECT}
     SuperObject,
     {$ENDIF}
     DbxJsonUnMarshal, DbxJsonMarshal, Rtti;

type
  TJsonUtilGenerics = class
  public
    class function Marshal(entity: TObject): string;

    class function UnMarshal<T>(text: String): T;
  end;

implementation

{ TJsonUtilGenerics }

class function TJsonUtilGenerics.Marshal(entity: TObject): string;
begin
  {$IFDEF USE_SUPER_OBJECT}
  Result := entity.ToJson().AsJSon();
  {$ELSE}
  Result := TDBXJsonMarshal.ToJsonText(entity);
  {$ENDIF}
end;

class function TJsonUtilGenerics.UnMarshal<T>(text: String): T;
{$IFDEF USE_SUPER_OBJECT}
var
  ctx: TSuperRttiContext;
begin
  ctx := TSuperRttiContext.Create;
  try
    Result := ctx.AsType<T>(SuperObject.SO(text));
  finally
    ctx.Free;
  end;
{$ELSE}
begin
  Result := TDBXJsonUnmarshal.FromJson<T>(text);
{$ENDIF}
end;

end.
