unit TestSuperobject;

interface

uses
  SysUtils, TestFramework, SuperObject, RestJsonUtils, rtti,
  Generics.Collections;

type

  TTestSuperObjectUnMarshal = class(TTestCase)
  private
    class function UnMarshal<T>(text: String): T;
  protected
    procedure TearDown; override;        
    procedure SetUp; override;
  published
    procedure TestNullInt;
    procedure TestList;
  end;

  TTestInt = class(TObject)
  public
    a: integer;
    b: int64;
    c: integer;
    d: int64;
  end;

implementation

{ TTestDbxJsonUnMarshal }

class function TTestSuperObjectUnMarshal.UnMarshal<T>(text: String): T;
var
  ctx: TSuperRttiContext;
  v: TValue;
begin
  Result := Default(T);
  if text <> '' then
  begin
    ctx := TSuperRttiContext.Create;
    try
      if ctx.FromJson(TypeInfo(T), SuperObject.SO(text), v) then
        Result := v.AsType<T>
      else
        raise Exception.Create('Marshalling error');
    finally
      ctx.Free;
    end;
  end;
end;

procedure TTestSuperObjectUnMarshal.SetUp;
begin
  inherited;
end;

procedure TTestSuperObjectUnMarshal.TearDown;
begin
  inherited;
end;

procedure TTestSuperObjectUnMarshal.TestList;
var
  list: TList<integer>;
  s: string;
begin
  list := TList<integer>.create;
  try
    list.Add(1);
    list.Add(2);
    list.Add(3);
    s := list.ToJson().AsJSon();
    checkEquals('[1,2,3]', s);
  finally
    list.Free;
  end;
end;

procedure TTestSuperObjectUnMarshal.TestNullInt;
var
  json: string;
  t: TTESTInt;
begin
  json := '{"a": null, "b": null, "c":999, "d": 999999999}';

  SODefaultIntNull := 0;
  t := UnMarshal<TTestInt>(json);
  try
    checkEquals(t.a, 0);
    checkEquals(t.b, 0);
  finally
    t.free;
  end;
  SODefaultIntNull := -1;
  t := UnMarshal<TTestInt>(json);
  try
    checkEquals(t.a, -1);
    checkEquals(t.b, -1);
  finally
    t.free;
  end;
  t := UnMarshal<TTestInt>(json);
  try
    checkEquals(t.c, 999);
    checkEquals(t.d, 999999999);
  finally
    t.free;
  end;
end;

initialization
  RegisterTest(TTestSuperObjectUnMarshal.Suite);

end.
