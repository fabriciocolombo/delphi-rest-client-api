unit TestRegister;

interface

{$I DelphiRest.inc}

{$IFDEF USE_GENERICS}
uses TestDBXJson, TestSerializer, TestDataSetHandler,
     TestDbxJsonUnMarshal, TestDbxJsonMarshal,
     DbxJsonUnMarshal, DbxJsonMarshal;
{$ELSE}
uses TestRequestSerialization;
{$ENDIF}

implementation

end.
