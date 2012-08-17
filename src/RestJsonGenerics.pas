unit RestJsonGenerics;

interface

{$I DelphiRest.inc}

uses Generics.Collections, Rtti,
     SuperObject;

type
  TJsonUtilGenerics = class
  public
    class function Marshal(entity: TObject): string;

    class function UnMarshal<T>(text: String): T;
    class function UnMarshalList<T>(text: String): TList<T>;overload;
    class function UnMarshalList<T>(obj: ISuperObject): TList<T>;overload;
  end;

implementation

{ TJsonUtilGenerics }

class function TJsonUtilGenerics.Marshal(entity: TObject): string;
begin
  Result := entity.ToJson().AsJSon();
end;

class function TJsonUtilGenerics.UnMarshal<T>(text: String): T;
var
  ctx: TSuperRttiContext;
begin
  ctx := TSuperRttiContext.Create;
  try
    Result := ctx.AsType<T>(SuperObject.SO(text));
  finally
    ctx.Free;
  end;
end;

class function TJsonUtilGenerics.UnMarshalList<T>(text: String): TList<T>;
begin
  Result := Self.UnMarshalList<T>(SuperObject.SO(text));
end;

class function TJsonUtilGenerics.UnMarshalList<T>(obj: ISuperObject): TList<T>;
var
  ctx: TSuperRttiContext;
  ret: TValue;
  vArray: TSuperArray;
  i: Integer;
begin
  ctx := TSuperRttiContext.Create;
  try
    Result := TList<T>.Create;

    if obj.IsType(stArray) then
    begin
      vArray := obj.AsArray;

      for i := 0 to vArray.Length-1 do
      begin
        Result.Add(ctx.AsType<T>(vArray.O[i]));
      end;
    end
    else
    begin
      Result.Add(ctx.AsType<T>(obj));
    end;
  finally
    ctx.Free;
  end;
end;

end.
