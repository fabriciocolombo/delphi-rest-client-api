unit RestException;

{$I DelphiRest.inc}

interface

uses
  SysUtils;

type
  ERestClientException = class(Exception);
  EInvalidHttpConnectionConfiguration = class(ERestClientException);
  ECustomCreateConnectionException = class(ERestClientException);
  EInactiveConnection = class(ERestClientException);

{$IFNDEF DELPHI_2010_UP}
  ENotSupportedException = class(Exception);
{$ENDIF}

{$IFNDEF DELPHI_XE_UP}
  ENotImplemented = class(Exception);
{$ENDIF}

implementation

end.
