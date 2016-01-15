unit TestRegister;

interface

{$I DelphiRest.inc}

{$IFDEF SUPPORTS_GENERICS}
uses TestDBXJson, TestSerializer, TestDataSetHandler,
     TestDbxJsonUnMarshal, TestDbxJsonMarshal,
     DbxJsonUnMarshal, DbxJsonMarshal
     {$IFDEF SUPPORTS_ANONYMOUS_METHODS}
     ,TestAsync
     {$ENDIF}
     ;
{$ELSE}
uses TestRequestSerialization;
{$ENDIF}

implementation

end.
