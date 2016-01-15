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

  TStatusCode = class
  public
    (* 200 OK, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.1">HTTP/1.1 documentation</a>}.*)
    class function OK(): TReponseCode;

    (* 201 Created, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.2">HTTP/1.1 documentation</a>}. *)
    class function CREATED(): TReponseCode;

    (* 202 Accepted, see {@l ink <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.3">HTTP/1.1 documentation</a>}.*)
    class function ACCEPTED: TReponseCode;

    (* 204 No Content, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.2.5">HTTP/1.1 documentation</a>}. *)
    class function NO_CONTENT: TReponseCode;

    (* 404 Not Found, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.5">HTTP/1.1 documentation</a>}*)
    class function NOT_FOUND: TReponseCode;

    (* 400 Bad Request, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.1">HTTP/1.1 documentation</a>}*)
    class function BAD_REQUEST: TReponseCode;

    (* 401 Unauthorized, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.2">HTTP/1.1 documentation</a>}*)
    class function UNAUTHORIZED: TReponseCode;

    (* 407 Proxy Authentication Required, see {@link <a href="http://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html#sec10.4.8">HTTP/1.1 documentation</a>}*)
    class function PROXY_AUTHENTICATION_REQUIRED: TReponseCode;
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

class function TStatusCode.BAD_REQUEST: TReponseCode;
begin
  Result.StatusCode := 400;
  Result.Reason := 'Bad Request';
end;

class function TStatusCode.Created: TReponseCode;
begin
  Result.StatusCode := 201;
  Result.Reason := 'Created';
end;

class function TStatusCode.NOT_FOUND: TReponseCode;
begin
  Result.StatusCode := 404;
  Result.Reason := 'Not Found';
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

class function TStatusCode.PROXY_AUTHENTICATION_REQUIRED: TReponseCode;
begin
  Result.StatusCode := 407;
  Result.Reason := 'Proxy Authentication Required';
end;

class function TStatusCode.UNAUTHORIZED: TReponseCode;
begin
  Result.StatusCode := 401;
  Result.Reason := 'Unauthorized';
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
