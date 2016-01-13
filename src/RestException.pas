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

{$IFNDEF HAS_ENOTSUPPORTED}
  ENotSupportedException = class(Exception);
{$ENDIF}

{$IFNDEF HAS_ENOTIMPLEMENTED}
  ENotImplemented = class(Exception);
{$ENDIF}

implementation

end.
