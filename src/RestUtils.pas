unit RestUtils;

interface

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
  end;

implementation


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

end.
