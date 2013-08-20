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

    class function UnMarshal<T>(text: String): T;overload;
    class function UnMarshal(AClassType: TClass; text: String): TObject;overload;
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

class function TJsonUtilGenerics.UnMarshal(AClassType: TClass;text: String): TObject;
{$IFDEF USE_SUPER_OBJECT}
var
  ctx: TSuperRttiContext;
  v: TValue;
begin
  Result := nil;
  if text <> '' then
  begin
    ctx := TSuperRttiContext.Create;
    try
      if ctx.FromJson(AClassType.ClassInfo, SuperObject.SO(text), v) then
      begin
        Result := v.Cast(AClassType.ClassInfo).AsObject;
      end;
    finally
      ctx.Free;
    end;
  end;
{$ELSE}
begin
  Result := TDBXJsonUnmarshal.FromJson(AClassType, text);
{$ENDIF}
end;

class function TJsonUtilGenerics.UnMarshal<T>(text: String): T;
{$IFDEF USE_SUPER_OBJECT}
var
  ctx: TSuperRttiContext;
  v: TValue;
begin
  Result := T(nil);
  if text <> '' then
  begin
    ctx := TSuperRttiContext.Create;
    try
      Result := ctx.AsType<T>(SuperObject.SO(text));
    finally
      ctx.Free;
    end;
  end;
{$ELSE}
begin
  Result := TDBXJsonUnmarshal.FromJson<T>(text);
{$ENDIF}
end;

end.
