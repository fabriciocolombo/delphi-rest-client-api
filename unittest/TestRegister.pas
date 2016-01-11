unit TestRegister;

interface

{$I DelphiRest.inc}

{$IFDEF USE_GENERICS}
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
