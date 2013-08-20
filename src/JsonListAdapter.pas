unit JsonListAdapter;

interface

uses Classes;

type
  IJsonListAdapter = interface
  ['{8E288A8D-E3F5-4945-9DF2-6CD6B60ADEA8}']
    function UnWrapList: TList;
    function ItemClass: TClass;
  end;

implementation

end.
