unit TestSerializer;

interface

uses BaseTestRest, Generics.Collections, SuperObject, ContNrs, Classes;

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
    phones : TList<TPhone>;
  end;
  
  TTestDesserializer = class(TBaseTestRest)
  published
    procedure person;
    procedure personWithFather;
    procedure personWithFatherAndPhones;
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

  CheckNotNull(vPerson);
  CheckEquals('Helbert', vPerson.name);
  CheckEquals(30, vPerson.age);

  CheckNotNull(vPerson.father);
  CheckEquals('Luiz', vPerson.father.name);
  CheckEquals(60, vPerson.father.age);
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

  CheckNotNull(vPerson);
  CheckEquals('Helbert', vPerson.name);
  CheckEquals(30, vPerson.age);
end;

initialization
  TTestDesserializer.RegisterTest;

end.
