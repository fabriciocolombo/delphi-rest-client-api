unit TestSerializer;

interface

uses BaseTestRest, Generics.Collections, SuperObject, ContNrs, Classes, SysUtils;

type
  TPhone = class(TObject)
  public
    number: Int64;
    code: Integer;
  end;

  TPerson = class(TObject)
  public
    age: Integer;
    name: String;
    father: TPerson;
    phones : TObjectList<TPhone>;

    function ToString: string; override;
    destructor Destroy; override;
  end;
  
  TTestDesserializer = class(TBaseTestRest)
  published
    procedure person;
    procedure personWithFather;
    procedure personWithFatherAndPhones;
  end;

  TTestSerializer = class(TBaseTestRest)
  published
    procedure person;
  end;

implementation

{ TTestDesserializer }

procedure TTestDesserializer.personWithFather;
const
  sJson = '{' +
          '    "name": "Helbert",' +
          '    "age": 30,' +
          '    "father": {' +
          '        "name": "Luiz",' +
          '        "age": 60,' +
          '        "father": null,' +
          '        "phones": null' +
          '    }' +
          '}';
var
  vPerson: TPerson;
begin
  vPerson := TPerson.FromJson(sJson);
  try
    CheckNotNull(vPerson);
    CheckEquals('Helbert', vPerson.name);
    CheckEquals(30, vPerson.age);

    CheckNotNull(vPerson.father);
    CheckEquals('Luiz', vPerson.father.name);
    CheckEquals(60, vPerson.father.age);
  finally
    vPerson.Free;
  end;
end;

procedure TTestDesserializer.personWithFatherAndPhones;
const
  sJson = '{' +
          '    "name": "Helbert",' +
          '    "age": 30,' +
          '    "father": {' +
          '        "name": "Luiz",' +
          '        "age": 60,' +
          '        "father": null,' +
          '        "phones": null' +
          '    },' +
          '    "phones": [' +
          '        {' +
          '            "number": 33083518,' +
          '            "code": 61' +
          '        },' +
          '        {' +
          '            "number": 99744165,' +
          '            "code": 61' +
          '        }' +
          '    ]' +
          '}';
var
  vPerson: TPerson;
begin
  vPerson := TPerson.FromJson(sJson);
  try
    CheckNotNull(vPerson);
    CheckEquals('Helbert', vPerson.name);
    CheckEquals(30, vPerson.age);

    CheckNotNull(vPerson.father);
    CheckEquals('Luiz', vPerson.father.name);
    CheckEquals(60, vPerson.father.age);

    CheckNotNull(vPerson.phones);
    CheckEquals(2, vPerson.phones.Count);
    CheckEquals(33083518, vPerson.phones[0].number);
    CheckEquals(61, vPerson.phones[0].code);

    CheckEquals(99744165, vPerson.phones[1].number);
    CheckEquals(61, vPerson.phones[1].code);
  finally
    vPerson.Free;
  end;
end;

procedure TTestDesserializer.person;
const
  sJson = '{' +
          '    "name": "Helbert",' +
          '    "age": 30' +
          '}';
var
  vPerson: TPerson;
begin
  vPerson := TPerson.FromJson(sJson);
  try
    CheckNotNull(vPerson);
    CheckEquals('Helbert', vPerson.name);
    CheckEquals(30, vPerson.age);
  finally
    vPerson.Free;
  end;
end;

{ TTestSerializer }

procedure TTestSerializer.person;
const
  sJson = '{"age":30,"name":"Helbert"}';
var
  vPerson: TPerson;
begin
  vPerson := TPerson.Create;
  try
    vPerson.name := 'Helbert';
    vPerson.age := 30;

    CheckEquals(sJson, vPerson.ToJson().AsJSon());
  finally
    vPerson.Free;
  end;
end;

{ TPerson }

destructor TPerson.Destroy;
begin
  if Assigned(phones) then
    phones.OwnsObjects := True;
  phones.Free;
  father.Free;
  inherited;
end;

function TPerson.ToString: string;
begin
  Result := 'name:' + name + ' age:' + IntToStr(age);
end;

initialization
  TTestDesserializer.RegisterTest;
  TTestSerializer.RegisterTest;

end.
