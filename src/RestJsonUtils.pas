unit RestJsonUtils;

interface

{$I DelphiRest.inc}

uses {$IFDEF USE_GENERICS}RestJsonGenerics, {$ENDIF}RestJsonOldRTTI, SysUtils,
     DateUtils;

type
  {$IFDEF USE_GENERICS}
  TJsonUtil = TJsonUtilGenerics;
  {$ELSE}
  TJsonUtil = TJsonUtilOldRTTI;
  {$ENDIF}

implementation

end.
