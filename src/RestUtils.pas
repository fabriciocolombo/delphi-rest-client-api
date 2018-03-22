unit RestUtils;

interface

{$I DelphiRest.inc}

const
  MediaType_Json = 'application/json';
  MediaType_Xml = 'text/xml';

  LOCALE_PORTUGUESE_BRAZILIAN = 'pt-BR';
  LOCALE_US = 'en-US';

type
  TReponseCode = record
    StatusCode: Integer;
    Reason: string;
  end;

  (* See Status Code Definitions at http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html *)
  TStatusCode = class
  public
    (* 100 CONTINUE *)
    class function CONTINUE: TReponseCode;

    (* 101 Switching Protocols *)
    class function SWITCHING_PROTOCOLS: TReponseCode;

    (* 200 OK *)
    class function OK: TReponseCode;

    (* 201 Created *)
    class function CREATED: TReponseCode;

    (* 202 Accepted *)
    class function ACCEPTED: TReponseCode;

    (* 203 Non-Authoritative Information *)
    class function NON_AUTHORITATIVE_INFORMATION: TReponseCode;

    (* 204 No Content *)
    class function NO_CONTENT: TReponseCode;

    (* 205 Reset Content *)
    class function RESET_CONTENT: TReponseCode;

    (* 206 Partial Content *)
    class function PARTIAL_CONTENT: TReponseCode;

    (* 300 Multiple Choices *)
    class function MULTIPLE_CHOICES: TReponseCode;

    (* 301 Moved Permanently *)
    class function MOVED_PERMANENTLY: TReponseCode;

    (* 302 Found *)
    class function FOUND: TReponseCode;

    (* 303 See Other *)
    class function SEE_OTHER: TReponseCode;

    (* 304 Not Modified *)
    class function NOT_MODIFIED: TReponseCode;

    (* 305 Use Proxy *)
    class function USE_PROXY: TReponseCode;

    (* 307 Temporary Redirect *)
    class function TEMPORARY_REDIRECT: TReponseCode;

    (* 400 Bad Request *)
    class function BAD_REQUEST: TReponseCode;

    (* 401 Unauthorized *)
    class function UNAUTHORIZED: TReponseCode;

    (* 402 Payment Required *)
    class function PAYMENT_REQUIRED: TReponseCode;

    (* 403 Forbidden *)
    class function FORBIDDEN: TReponseCode;

    (* 404 Not Found *)
    class function NOT_FOUND: TReponseCode;

    (* 405 Method Not Allowed *)
    class function METHOD_NOT_ALLOWED: TReponseCode;

    (* 406 Not Acceptable *)
    class function NOT_ACCEPTABLE: TReponseCode;

    (* 407 Proxy Authentication Required *)
    class function PROXY_AUTHENTICATION_REQUIRED: TReponseCode;

    (* 408 Request Timeout *)
    class function REQUEST_TIMEOUT: TReponseCode;

    (* 409 OK *)
    class function CONFLICT: TReponseCode;

    (* 410 Gone *)
    class function GONE: TReponseCode;

    (* 411 Length Required *)
    class function LENGTH_REQUIRED: TReponseCode;

    (* 412 Precondition Failed *)
    class function PRECONDITION_FAILED: TReponseCode;

    (* 413 Request Entity Too Large *)
    class function REQUEST_ENTITY_TOO_LARGE: TReponseCode;

    (* 414 Request-URI Too Long *)
    class function REQUEST_URI_TOO_LONG: TReponseCode;

    (* 415 Unsupported Media Type *)
    class function UNSUPPORTED_MEDIA_TYPE: TReponseCode;

    (* 416 Requested Range Not Satisfiable *)
    class function REQUESTED_RANGE_NOT_SATISFIABLE: TReponseCode;

    (* 417 Expectation Failed *)
    class function EXPECTATION_FAILED: TReponseCode;

    (* 422 Unprocessable Entity *)
    class function UNPROCESSABLE_ENTITY: TReponseCode;

    (* 500 Internal Server Error *)
    class function INTERNAL_SERVER_ERROR: TReponseCode;

    (* 501 Not Implemented *)
    class function NOT_IMPLEMENTED: TReponseCode;

    (* 502 Bad Gateway *)
    class function BAD_GATEWAY: TReponseCode;

    (* 503 Service Unavailable *)
    class function SERVICE_UNAVAILABLE: TReponseCode;

    (* 504 Gateway Timeout *)
    class function GATEWAY_TIMEOUT: TReponseCode;

    (* 505 HTTP Version Not Supported *)
    class function HTTP_VERSION_NOT_SUPPORTED: TReponseCode;

  end;

  TRestUtils = class
  public
    class function Base64Encode(const AValue: String): String;
    class function Base64Decode(const AValue: String): String;
  end;

implementation

uses
  {$IFDEF HAS_UNIT_NETENCODING}
  System.NetEncoding, //allows inlining of EncodeString, DecodeString
  {$ENDIF}
  EncdDecd;

{ TStatusCode }

class function TStatusCode.ACCEPTED: TReponseCode;
begin
  Result.StatusCode := 202;
  Result.Reason := 'Accepted';
end;

class function TStatusCode.BAD_GATEWAY: TReponseCode;
begin
  Result.StatusCode := 502;
  Result.Reason := 'Bad Gateway';
end;

class function TStatusCode.BAD_REQUEST: TReponseCode;
begin
  Result.StatusCode := 400;
  Result.Reason := 'Bad Request';
end;

class function TStatusCode.CONFLICT: TReponseCode;
begin
  Result.StatusCode := 409;
  Result.Reason := 'Conflict';
end;

class function TStatusCode.CONTINUE: TReponseCode;
begin
  Result.StatusCode := 100;
  Result.Reason := 'Continue';
end;

class function TStatusCode.Created: TReponseCode;
begin
  Result.StatusCode := 201;
  Result.Reason := 'Created';
end;

class function TStatusCode.EXPECTATION_FAILED: TReponseCode;
begin
  Result.StatusCode := 417;
  Result.Reason := 'Expectation Failed';
end;

class function TStatusCode.FORBIDDEN: TReponseCode;
begin
  Result.StatusCode := 403;
  Result.Reason := 'Forbidden';
end;

class function TStatusCode.FOUND: TReponseCode;
begin
  Result.StatusCode := 302;
  Result.Reason := 'Found';
end;

class function TStatusCode.GATEWAY_TIMEOUT: TReponseCode;
begin
  Result.StatusCode := 504;
  Result.Reason := 'Gateway Timeout';
end;

class function TStatusCode.GONE: TReponseCode;
begin
  Result.StatusCode := 410;
  Result.Reason := 'Gone';
end;

class function TStatusCode.HTTP_VERSION_NOT_SUPPORTED: TReponseCode;
begin
  Result.StatusCode := 505;
  Result.Reason := 'HTTP Version Not Supported';
end;

class function TStatusCode.INTERNAL_SERVER_ERROR: TReponseCode;
begin
  Result.StatusCode := 500;
  Result.Reason := 'Internal Server Error';
end;

class function TStatusCode.LENGTH_REQUIRED: TReponseCode;
begin
  Result.StatusCode := 411;
  Result.Reason := 'Length Required';
end;

class function TStatusCode.METHOD_NOT_ALLOWED: TReponseCode;
begin
  Result.StatusCode := 405;
  Result.Reason := 'Method Not Allowed';
end;

class function TStatusCode.MOVED_PERMANENTLY: TReponseCode;
begin
  Result.StatusCode := 301;
  Result.Reason := 'Moved Permanently';
end;

class function TStatusCode.MULTIPLE_CHOICES: TReponseCode;
begin
  Result.StatusCode := 300;
  Result.Reason := 'Multiple Choices';
end;

class function TStatusCode.NON_AUTHORITATIVE_INFORMATION: TReponseCode;
begin
  Result.StatusCode := 203;
  Result.Reason := 'Non-Authoritative Information';
end;

class function TStatusCode.NOT_ACCEPTABLE: TReponseCode;
begin
  Result.StatusCode := 406;
  Result.Reason := 'Not Acceptable';
end;

class function TStatusCode.NOT_FOUND: TReponseCode;
begin
  Result.StatusCode := 404;
  Result.Reason := 'Not Found';
end;

class function TStatusCode.NOT_IMPLEMENTED: TReponseCode;
begin
  Result.StatusCode := 501;
  Result.Reason := 'Not Implemented';
end;

class function TStatusCode.NOT_MODIFIED: TReponseCode;
begin
  Result.StatusCode := 304;
  Result.Reason := 'Not Modified';
end;

class function TStatusCode.NO_CONTENT: TReponseCode;
begin
  Result.StatusCode := 204;
  Result.Reason := 'No Content';
end;

class function TStatusCode.OK: TReponseCode;
begin
  Result.StatusCode := 200;
  Result.Reason := 'OK';
end;

class function TStatusCode.PARTIAL_CONTENT: TReponseCode;
begin
  Result.StatusCode := 206;
  Result.Reason := 'Partial Content';
end;

class function TStatusCode.PAYMENT_REQUIRED: TReponseCode;
begin
  Result.StatusCode := 402;
  Result.Reason := 'Payment Required';
end;

class function TStatusCode.PRECONDITION_FAILED: TReponseCode;
begin
  Result.StatusCode := 412;
  Result.Reason := 'Precondition Failed';
end;

class function TStatusCode.PROXY_AUTHENTICATION_REQUIRED: TReponseCode;
begin
  Result.StatusCode := 407;
  Result.Reason := 'Proxy Authentication Required';
end;

class function TStatusCode.REQUESTED_RANGE_NOT_SATISFIABLE: TReponseCode;
begin
  Result.StatusCode := 416;
  Result.Reason := 'Requested Range Not Satisfiable';
end;

class function TStatusCode.REQUEST_ENTITY_TOO_LARGE: TReponseCode;
begin
  Result.StatusCode := 413;
  Result.Reason := 'Request Entity Too Large';
end;

class function TStatusCode.REQUEST_TIMEOUT: TReponseCode;
begin
  Result.StatusCode := 408;
  Result.Reason := 'Request Timeout';
end;

class function TStatusCode.REQUEST_URI_TOO_LONG: TReponseCode;
begin
  Result.StatusCode := 414;
  Result.Reason := 'Request-URI Too Long';
end;

class function TStatusCode.RESET_CONTENT: TReponseCode;
begin
  Result.StatusCode := 205;
  Result.Reason := 'Reset Content';
end;

class function TStatusCode.SEE_OTHER: TReponseCode;
begin
  Result.StatusCode := 303;
  Result.Reason := 'See Other';
end;

class function TStatusCode.SERVICE_UNAVAILABLE: TReponseCode;
begin
  Result.StatusCode := 503;
  Result.Reason := 'Service Unavailable';
end;

class function TStatusCode.SWITCHING_PROTOCOLS: TReponseCode;
begin
  Result.StatusCode := 101;
  Result.Reason := 'Switching Protocols';
end;

class function TStatusCode.TEMPORARY_REDIRECT: TReponseCode;
begin
  Result.StatusCode := 307;
  Result.Reason := 'Temporary Redirect';
end;

class function TStatusCode.UNAUTHORIZED: TReponseCode;
begin
  Result.StatusCode := 401;
  Result.Reason := 'Unauthorized';
end;

class function TStatusCode.UNPROCESSABLE_ENTITY: TReponseCode;
begin
  Result.StatusCode := 422;
  Result.Reason := 'Unprocessable Entity';
end;

class function TStatusCode.UNSUPPORTED_MEDIA_TYPE: TReponseCode;
begin
  Result.StatusCode := 415;
  Result.Reason := 'Unsupported Media Type';
end;

class function TStatusCode.USE_PROXY: TReponseCode;
begin
  Result.StatusCode := 305;
  Result.Reason := 'Use Proxy';
end;

{ TRestUtils }

class function TRestUtils.Base64Decode(const AValue: String): String;
begin
  Result := EncdDecd.DecodeString(AValue);
end;

class function TRestUtils.Base64Encode(const AValue: String): String;
begin
  Result := EncdDecd.EncodeString(AValue);
end;

end.
