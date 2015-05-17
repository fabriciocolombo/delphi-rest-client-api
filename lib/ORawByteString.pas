unit ORawByteString;

{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  Version
    0.2 2014-04-12

  License:
    MPL 1.1: http://www.mozilla.org/MPL/1.1/

}

{
  ORawByteString.pas

  TORawByteString
    - a replacement of RawByteString for NextGen (iOS/Android) for use
      as a plain byte buffer (no explicit string data inside)
    - please note that all indices used in TORawByteString are 0-based if not stated differently.
      this is necessary to keep things simple

  TORawByteChar
    - a replacement for AnsiChar

  TORawSingleByteEncoding
    - Encoding that converts the byte representation of chars (only within $FF) directly to/from byte
      (no character codepage involved).

}

{.$DEFINE ORAW_IMPLICIT}//define to allow implicit conversions (a little less secure)

{$BOOLEVAL OFF}

interface

uses
  SysUtils, Classes, Generics.Collections;

type
  TORawSingleByteEncoding = class(TEncoding)
  strict private
    class var
      FSavedInstance: TORawSingleByteEncoding;
    class destructor Destroy;
  strict protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; override;
  public
    constructor Create;
  public
    class function Use: TORawSingleByteEncoding;
    destructor Destroy; override;

    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

  TEncodingHelper = class helper for TEncoding
  public
    class function RawByte: TEncoding;
  end;

  TORawByteChar = packed record
  strict private
    FChar: Byte;
  public
    //operators
    class operator Equal(const a1, a2: TORawByteChar): Boolean;
    class operator Equal(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator Equal(const a1: Char; const a2: TORawByteChar): Boolean;

    class operator NotEqual(const a1, a2: TORawByteChar): Boolean;
    class operator NotEqual(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator NotEqual(const a1: Char; const a2: TORawByteChar): Boolean;

    class operator GreaterThan(const a1, a2: TORawByteChar): Boolean;
    class operator GreaterThan(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator GreaterThan(const a1: Char; const a2: TORawByteChar): Boolean;

    class operator GreaterThanOrEqual(const a1, a2: TORawByteChar): Boolean;
    class operator GreaterThanOrEqual(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator GreaterThanOrEqual(const a1: Char; const a2: TORawByteChar): Boolean;

    class operator LessThan(const a1, a2: TORawByteChar): Boolean;
    class operator LessThan(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator LessThan(const a1: Char; const a2: TORawByteChar): Boolean;

    class operator LessThanOrEqual(const a1, a2: TORawByteChar): Boolean;
    class operator LessThanOrEqual(const a1: TORawByteChar; const a2: Char): Boolean;
    class operator LessThanOrEqual(const a1: Char; const a2: TORawByteChar): Boolean;

    {$IFDEF ORAW_IMPLICIT}
    class operator Implicit(const a: Char): TORawByteChar;
    class operator Implicit(const a: TORawByteChar): Char;
    class operator Implicit(const a: Byte): TORawByteChar;
    class operator Implicit(const a: TORawByteChar): Byte;
    {$ELSE}
    class operator Explicit(const a: Char): TORawByteChar;
    class operator Explicit(const a: TORawByteChar): Char;
    class operator Explicit(const a: Byte): TORawByteChar;
    class operator Explicit(const a: TORawByteChar): Byte;
    {$ENDIF}
  public
    //ANSI functions
    function ToUpper: TORawByteChar;
    function ToLower: TORawByteChar;
  end;
  TORawByteCharArray = array[0..High(Integer)-1] of TORawByteChar;
  PORawByteChar = ^TORawByteCharArray;

  TORawByteString = packed record
  strict private class var
    FDefaultEncoding: TEncoding;//default encoding for AnsiString operations, use TORawSingleByteEncoding
  strict private type
    PORawByteString = ^TORawByteString;
    TORawByteStringCharEnumerator = class
    strict private
      FIndex: Integer;
      FString: PORawByteString;
    public
      constructor Create(aBuffer: PORawByteString);
      function GetCurrent: TORawByteChar; inline;
      function MoveNext: Boolean;
      property Current: TORawByteChar read GetCurrent;
    end;
  strict private
    FStrBuffer: String;//We store the real buffer length in the first two char positions!

    function GetChar(const aIndex: Integer): TORawByteChar;
    procedure SetChar(const aIndex: Integer; const aChar: TORawByteChar);
    function GetDefChar(const aIndex: Integer): TORawByteChar;
    procedure SetDefChar(const aIndex: Integer; const aChar: TORawByteChar);
    function GetLength: Integer;

    procedure CopyOnWriteCheck;
  public
    //operators

    function GetBuffer: PByteArray;
    procedure SetLength(const aLength: Integer);
    class operator Add(const a1, a2: TORawByteString): TORawByteString;
    class operator Add(const a1: TORawByteString; const a2: TBytes): TORawByteString;
    class operator Add(const a1: TBytes; const a2: TORawByteString): TORawByteString;
    class operator Add(const a1: TORawByteString; const a2: TORawByteChar): TORawByteString;
    class operator Add(const a1: TORawByteChar; const a2: TORawByteString): TORawByteString;
    class operator Equal(const a1, a2: TORawByteString): Boolean;
    class operator NotEqual(const a1, a2: TORawByteString): Boolean;

    {$IFDEF ORAW_IMPLICIT}
    class operator Implicit(const a: string): TORawByteString;
    class operator Implicit(const a: TORawByteString): string;
    class operator Implicit(const a: TBytes): TORawByteString;
    class operator Implicit(const a: TORawByteString): TBytes;
    class operator Implicit(const a: TORawByteString): PORawByteChar;
    class operator Implicit(const a: PORawByteChar): TORawByteString;
    {$ELSE}
    class operator Explicit(const a: string): TORawByteString;
    class operator Explicit(const a: TORawByteString): string;
    class operator Explicit(const a: TBytes): TORawByteString;
    class operator Explicit(const a: TORawByteString): TBytes;
    class operator Explicit(const a: TORawByteString): PORawByteChar;
    class operator Explicit(const a: PORawByteChar): TORawByteString;
    {$ENDIF}

    function GetEnumerator: TORawByteStringCharEnumerator;
  public
    //constructors

    constructor Create(const aBytes: TBytes); overload;
    constructor Create(const aString: string; aEncoding: TEncoding = nil); overload;
  public
    //byte functions -> they act on raw byte string as "Array of Byte"

    //clear
    procedure Clear;
    //get the byte position of string, result is 0-based!
    function Pos(const aSubStr: TORawByteString): Integer;
    function PosEx(const aSubStr: TORawByteString; aOffset: Integer): Integer;

    //Append
    procedure Append(const aBytes: TBytes); overload;
    procedure Append(const aBuffer: TORawByteString); overload;
    procedure Append(const aBuffer; aBufferLength: Integer); overload;
    //Prepend
    procedure Prepend(const aBytes: TBytes); overload;
    procedure Prepend(const aBuffer: TORawByteString); overload;
    procedure Prepend(const aBuffer; aBufferLength: Integer); overload;
    //insert into a byte index, 0-based!!!
    procedure Insert(const aSubBytes: TBytes; aIndex: Integer); overload;
    procedure Insert(const aSubBuffer: TORawByteString; aIndex: Integer); overload;
    procedure Insert(const aSubBuffer; aSubBufferLength, aIndex: Integer); overload;

    //copy, 0-based!!!
    function Copy(aIndex, aCount: Integer): TORawByteString;
    //delete, 0-based!!!
    procedure Delete(aIndex, aCount: Integer);

    procedure ToUpper(aEncoding: TEncoding = nil);
    procedure ToLower(aEncoding: TEncoding = nil);
  public
    //String & TBytes conversion utilities
    function GetString(aEncoding: TEncoding = nil): string;
    procedure SetString(const aString: string; aEncoding: TEncoding = nil);
    function GetBytes: TBytes;
    procedure SetBytes(const aBytes: TBytes);
  public
    //stream & file functions
    procedure LoadFromStream(const aStream: TStream);
    procedure LoadFromFile(const aFileName: string);
    procedure SaveToStream(const aStream: TStream; const aWriteBOM: Boolean = True);
    procedure SaveToFile(const aFileName: string; const aWriteBOM: Boolean = True);
  public
    class function GetDefaultEncoding: TEncoding; static;
    class procedure SetDefaultEncoding(aEncoding: TEncoding); static;//default is TORawSingleByteEncoding = RawByteString, if you want to simulate AnsiString, use TEncoding.ANSI
  public
    //properties

    property Length: Integer read GetLength write SetLength;
    //get/set raw bytes by index, 0-based !!!
    property Chars[const aIndex: Integer]: TORawByteChar read GetChar write SetChar;
    //get/set raw bytes by index, 1-based !!!
    property DefChars[const aIndex: Integer]: TORawByteChar read GetDefChar write SetDefChar; default;
  end;

//Ansi* definitions and helper function

{$IFDEF NEXTGEN}
  AnsiString = TORawByteString;
  AnsiChar = TORawByteChar;
  RawByteString = TORawByteString;
  PAnsiChar = PORawByteChar;
  PRawByteChar = PORawByteChar;
{$ENDIF}

//All following string functions are 1-based
function UTF8ToString(const aUTF8String: TORawByteString): String; inline; overload;
function UTF8Decode(const aUTF8String: TORawByteString): String; inline; overload;
function UTF8Encode(const aString: String): TORawByteString; inline; overload;
function Pos(const aSubStr: String; const aString: TORawByteString): Integer; inline; overload;
function Pos(const aSubStr, aString: TORawByteString): Integer; inline; overload;
function PosEx(const aSubStr: String; const aString: TORawByteString; aOffset: Integer): Integer; inline; overload;
function PosEx(const aSubStr, aString: TORawByteString; aOffset: Integer): Integer; inline; overload;
function LowerCase(const aString: TORawByteString): TORawByteString; inline; overload;
function UpperCase(const aString: TORawByteString): TORawByteString; inline; overload;
function CompareStr(const S1, S2: TORawByteString): Integer; inline; overload;
function CompareText(const S1, S2: TORawByteString): Integer; inline; overload;

//the following functions can't be declared without Ansi prefix because they are compiler magic
function AnsiCopy(const aString: TORawByteString; aIndex, aCount: Integer): TORawByteString; inline; overload;
procedure AnsiInsert(const aSubString: TORawByteString; var aString: TORawByteString; aIndex: Integer); inline; overload;
procedure AnsiDelete(var aString: TORawByteString; aIndex, aCount: Integer); inline; overload;
function AnsiLength(const aString: TORawByteString): Integer; inline; overload;
function AnsiLow(const aString: TORawByteString): Integer; inline; overload;
function AnsiHigh(const aString: TORawByteString): Integer; inline; overload;
{$IFNDEF NEXTGEN}
function AnsiCopy(const aString: RawByteString; aIndex, aCount: Integer): RawByteString; inline; overload;
procedure AnsiInsert(const aSubString: RawByteString; var aString: RawByteString; aIndex: Integer); inline; overload;
procedure AnsiDelete(var aString: RawByteString; aIndex, aCount: Integer); inline; overload;
function AnsiLength(const aString: RawByteString): Integer; inline; overload;
function AnsiLow(const aString: RawByteString): Integer; inline; overload;
function AnsiHigh(const aString: RawByteString): Integer; inline; overload;
{$ENDIF}

implementation

function UTF8ToString(const aUTF8String: TORawByteString): String;
begin
  Result := aUTF8String.GetString(TEncoding.UTF8);
end;

function UTF8Decode(const aUTF8String: TORawByteString): String;
begin
  Result := aUTF8String.GetString(TEncoding.UTF8);
end;

function UTF8Encode(const aString: String): TORawByteString;
begin
  Result.SetString(aString, TEncoding.UTF8);
end;

function Pos(const aSubStr: String; const aString: TORawByteString): Integer;
begin
  Result := aString.Pos(TORawByteString(aSubStr)) + 1;
end;

function Pos(const aSubStr, aString: TORawByteString): Integer;
begin
  Result := aString.Pos(aSubStr) + 1;
end;

function PosEx(const aSubStr: String; const aString: TORawByteString; aOffset: Integer): Integer; inline; overload;
begin
  Result := aString.PosEx(TORawByteString(aSubStr), aOffset - 1) + 1;
end;

function PosEx(const aSubStr, aString: TORawByteString; aOffset: Integer): Integer; inline; overload;
begin
  Result := aString.PosEx(aSubStr, aOffset - 1) + 1;
end;

function LowerCase(const aString: TORawByteString): TORawByteString;
begin
  Result := aString;
  Result.ToLower;
end;

function UpperCase(const aString: TORawByteString): TORawByteString;
begin
  Result := aString;
  Result.ToUpper;
end;

function CompareStr(const S1, S2: TORawByteString): Integer;
begin
  Result := SysUtils.CompareStr(S1.GetString, S2.GetString);
end;

function CompareText(const S1, S2: TORawByteString): Integer;
begin
  Result := SysUtils.CompareText(S1.GetString, S2.GetString);
end;

function AnsiCopy(const aString: TORawByteString; aIndex, aCount: Integer): TORawByteString;
begin
  Result := aString.Copy(aIndex - 1, aCount);
end;

procedure AnsiInsert(const aSubString: TORawByteString; var aString: TORawByteString; aIndex: Integer);
begin
  aString.Insert(aSubString, aIndex-1);
end;

procedure AnsiDelete(var aString: TORawByteString; aIndex, aCount: Integer);
begin
  aString.Delete(aIndex-1, aCount);
end;

function AnsiLength(const aString: TORawByteString): Integer;
begin
  Result := aString.Length;
end;

function AnsiLow(const aString: TORawByteString): Integer;
begin
  Result := 1;
end;

function AnsiHigh(const aString: TORawByteString): Integer;
begin
  Result := aString.Length;;
end;

{$IFNDEF NEXTGEN}
function AnsiCopy(const aString: RawByteString; aIndex, aCount: Integer): RawByteString;
begin
  Result := System.Copy(aString, aIndex, aCount);
end;

procedure AnsiInsert(const aSubString: RawByteString; var aString: RawByteString; aIndex: Integer);
begin
  System.Insert(aSubString, aString, aIndex);
end;

procedure AnsiDelete(var aString: RawByteString; aIndex, aCount: Integer);
begin
  System.Delete(aString, aIndex, aCount);
end;

function AnsiLength(const aString: RawByteString): Integer;
begin
  Result := System.Length(aString);
end;

function AnsiLow(const aString: RawByteString): Integer;
begin
  Result := 1;
end;

function AnsiHigh(const aString: RawByteString): Integer;
begin
  Result := Length(aString);
end;
{$ENDIF}

{ TORawByteString }

class operator TORawByteString.Add(const a1, a2: TORawByteString): TORawByteString;
begin
  Result := a1;
  Result.Append(a2);
end;

class operator TORawByteString.Add(const a1: TORawByteString;
  const a2: TBytes): TORawByteString;
begin
  Result := a1;
  Result.Append(a2);
end;

class operator TORawByteString.Add(const a1: TBytes;
  const a2: TORawByteString): TORawByteString;
begin
  Result := a2;
  Result.Prepend(a1);
end;

class operator TORawByteString.Add(const a1: TORawByteString;
  const a2: TORawByteChar): TORawByteString;
begin
  Result.Length := a1.Length + 1;
  if a1.Length > 0 then
    System.Move(a1.GetBuffer[0], Result.GetBuffer[0], a1.Length);//we don't need COW check, writing to result
  Result[Result.Length-1] := a2;
end;

class operator TORawByteString.Add(const a1: TORawByteChar;
  const a2: TORawByteString): TORawByteString;
begin
  Result := a2 + a1;
end;

procedure TORawByteString.Append(const aBuffer;
  aBufferLength: Integer);
var
  xOldLength: Integer;
begin
  if aBufferLength > 0 then
  begin
    xOldLength := Length;
    Length := Length + aBufferLength;

    System.Move(aBuffer, GetBuffer[xOldLength], aBufferLength);//we don't need COW check, SetLength does it
  end;
end;

procedure TORawByteString.Append(
  const aBuffer: TORawByteString);
begin
  if aBuffer.Length > 0 then
    Append(aBuffer.GetBuffer[0], aBuffer.Length)
end;

procedure TORawByteString.Append(const aBytes: TBytes);
begin
  if System.Length(aBytes) > 0 then
    Append(aBytes[0], System.Length(aBytes))
end;

procedure TORawByteString.Clear;
begin
  SetLength(0);
end;

function TORawByteString.Copy(aIndex, aCount: Integer): TORawByteString;
begin
  Assert((aIndex >= 0) and (aIndex + aCount <= Length));

  Result.Length := aCount;
  if aCount > 0 then
    System.Move(GetBuffer[aIndex], Result.GetBuffer[0], aCount);//we don't need COW check, writing to result
end;

procedure TORawByteString.CopyOnWriteCheck;
  function RefCount: Integer;
  var
    xStrPtr: ^Integer;
  begin
    //get reference count of FStrBuffer, correct results on 32bit, 64bit and also mobile
    xStrPtr := Pointer(PChar(FStrBuffer));
    Dec(xStrPtr, 2);
    Result := xStrPtr^;
  end;
begin
  if (FStrBuffer <> '') and (RefCount > 1) then//if RefCount is only 1, we don't need CopyOnWrite
    FStrBuffer := System.Copy(FStrBuffer, 1, System.Length(FStrBuffer));//Simulate COW
end;

constructor TORawByteString.Create(const aString: string;
  aEncoding: TEncoding);
begin
  SetString(aString, aEncoding);
end;

procedure TORawByteString.Delete(aIndex, aCount: Integer);
begin
  Assert((aIndex >= 0) and (aIndex + aCount <= Length));

  if aCount <= 0 then
    Exit;

  CopyOnWriteCheck;//we need copy on write check here because Move() is performed on Self before SetLength!

  if aIndex+aCount < Length then
    System.Move(GetBuffer[aIndex+aCount], GetBuffer[aIndex], Length-(aIndex+aCount));

  Length := Length-aCount;
end;

constructor TORawByteString.Create(const aBytes: TBytes);
begin
  SetBytes(aBytes);
end;

class operator TORawByteString.Equal(const a1, a2: TORawByteString): Boolean;
var
  xL1, xL2: Integer;
begin
  //optimization -> compare memory
  xL1 := a1.Length;
  xL2 := a2.Length;

  Result :=
    (a1.GetBuffer = a2.GetBuffer) or (//same string reference
      (xL1 = xL2) and
      ((xL1 = 0) or (CompareMem(a1.GetBuffer, a2.GetBuffer, xL1))));
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: TORawByteString): PORawByteChar;
{$ELSE}
class operator TORawByteString.Explicit(const a: TORawByteString): PORawByteChar;
{$ENDIF}
begin
  if a.Length > 0 then
  begin
    Result := PORawByteChar(a.GetBuffer);
  end else
    Result := nil;
end;

function TORawByteString.GetBuffer: PByteArray;
begin
  if FStrBuffer <> '' then
    Result := PByteArray(@PChar(FStrBuffer)[2])//real buffer length is stored in the first 2 char positions
  else
    Result := nil;
end;

function TORawByteString.GetBytes: TBytes;
begin
  System.SetLength(Result, Length);
  if Length > 0 then
    System.Move(GetBuffer[0], Result[0], Length);//we don't need COW check, writing to result
end;

function TORawByteString.GetEnumerator: TORawByteStringCharEnumerator;
begin
  Result := TORawByteStringCharEnumerator.Create(@Self);
end;

function TORawByteString.GetLength: Integer;
begin
  if FStrBuffer = '' then//must be here -> FBufferByteLength may be uninitialized
    Result := 0
  else
    Result := PInteger(PChar(FStrBuffer))^;//real buffer length is stored in the first 2 char positions
end;

function TORawByteString.GetChar(const aIndex: Integer): TORawByteChar;
begin
  Result := TORawByteChar(GetBuffer[aIndex]);
end;

class function TORawByteString.GetDefaultEncoding: TEncoding;
begin
  if not Assigned(FDefaultEncoding) then
    FDefaultEncoding := TEncoding.RawByte;

  Result := FDefaultEncoding;
end;

function TORawByteString.GetDefChar(const aIndex: Integer): TORawByteChar;
begin
  Result := GetChar(aIndex-1);
end;

type
  TEncodingProt = class(TEncoding)
  public
    function GetCharCountProt(Bytes: PByte; ByteCount: Integer): Integer;
    function GetCharsProt(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
    function GetByteCountProt(Chars: PChar; CharCount: Integer): Integer;
    function GetBytesProt(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
  end;

function TEncodingProt.GetCharCountProt(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetCharCount(Bytes, ByteCount);
end;

function TEncodingProt.GetCharsProt(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetChars(Bytes, ByteCount, Chars, CharCount);
end;

function TEncodingProt.GetByteCountProt(Chars: PChar; CharCount: Integer): Integer;
begin
  Result := GetByteCount(Chars, CharCount);
end;

function TEncodingProt.GetBytesProt(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetBytes(Chars, CharCount, Bytes, ByteCount);
end;

function TORawByteString.GetString(aEncoding: TEncoding): string;
var
  xCharCount: Integer;
begin
  if not Assigned(aEncoding) then
    aEncoding := GetDefaultEncoding;

  xCharCount := TEncodingProt(aEncoding).GetCharCountProt(PByte(GetBuffer), Length);
  System.SetLength(Result, xCharCount);
  if xCharCount = 0 then
    Exit('');
  TEncodingProt(aEncoding).GetCharsProt(PByte(GetBuffer), Length, PChar(Result), xCharCount);//we don't need COW check, writing to result
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: TBytes): TORawByteString;
{$ELSE}
class operator TORawByteString.Explicit(const a: TBytes): TORawByteString;
{$ENDIF}
begin
  Result.SetBytes(a);
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: TORawByteString): TBytes;
{$ELSE}
class operator TORawByteString.Explicit(const a: TORawByteString): TBytes;
{$ENDIF}
begin
  Result := a.GetBytes;
end;

procedure TORawByteString.Insert(const aSubBuffer: TORawByteString;
  aIndex: Integer);
begin
  if aSubBuffer.Length > 0 then
    Insert(aSubBuffer.GetBuffer[0], aSubBuffer.Length, aIndex)
end;

procedure TORawByteString.Insert(const aSubBytes: TBytes;
  aIndex: Integer);
begin
  if System.Length(aSubBytes) > 0 then
    Insert(aSubBytes[0], System.Length(aSubBytes), aIndex)
end;

procedure TORawByteString.Insert(const aSubBuffer;
  aSubBufferLength, aIndex: Integer);
var
  xOldLength: Integer;
begin
  if aSubBufferLength > 0 then
  begin
    Assert((aIndex >= 0) and (aIndex <= Length));

    xOldLength := Length;
    Length := Length + aSubBufferLength;
    if aIndex < xOldLength then
      System.Move(GetBuffer[aIndex], GetBuffer[aIndex+aSubBufferLength], xOldLength-aIndex);//we don't need COW check, SetLength does it
    System.Move(aSubBuffer, GetBuffer[aIndex], aSubBufferLength);
  end;
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: string): TORawByteString;
{$ELSE}
class operator TORawByteString.Explicit(const a: string): TORawByteString;
{$ENDIF}
begin
  Result.SetString(a);
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: TORawByteString): string;
{$ELSE}
class operator TORawByteString.Explicit(const a: TORawByteString): string;
{$ENDIF}
begin
  Result := a.GetString;
end;

procedure TORawByteString.LoadFromFile(const aFileName: string);
var
  xS: TStream;
begin
  xS := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(xS);
  finally
    xS.Free;
  end;
end;

procedure TORawByteString.LoadFromStream(const aStream: TStream);
begin
  Length := aStream.Size-aStream.Position;
  if Length > 0 then
  begin
    CopyOnWriteCheck;//we need COW check, Length may stay unchanged!!!
    aStream.ReadBuffer(GetBuffer[0], Length);
  end;
end;

class operator TORawByteString.NotEqual(const a1, a2: TORawByteString): Boolean;
begin
  Result := not(a1 = a2);
end;

function TORawByteString.Pos(const aSubStr: TORawByteString): Integer;
begin
  Result := PosEx(aSubStr, 0);
end;

function TORawByteString.PosEx(const aSubStr: TORawByteString; aOffset: Integer): Integer;
var
  xSubBuffer: PByteArray;
  xSubLength: Integer;
  xSelfBuffer: PByteArray;
  xSelfLength: Integer;
begin
  Assert((aSubStr.Length > 0) and (aOffset >= 0));

  xSelfBuffer := GetBuffer;
  xSelfLength := Length;
  xSubBuffer := aSubStr.GetBuffer;
  xSubLength := aSubStr.Length;
  while aOffset <= (xSelfLength-xSubLength) do
  begin
    if CompareMem(@xSubBuffer[0], @xSelfBuffer[aOffset], xSubLength) then
    begin
      Result := aOffset;
      Exit;
    end;
    Inc(aOffset);
  end;
  Result := -1;
end;

procedure TORawByteString.Prepend(const aBuffer;
  aBufferLength: Integer);
var
  xOldLength: Integer;
begin
  if aBufferLength > 0 then
  begin
    xOldLength := Length;
    Length := Length + aBufferLength;

    if xOldLength > 0 then
      System.Move(GetBuffer[0], GetBuffer[aBufferLength], xOldLength);//we don't need COW check, SetLength does it
    if aBufferLength > 0 then
      System.Move(aBuffer, GetBuffer[0], aBufferLength);
  end;
end;

procedure TORawByteString.Prepend(
  const aBuffer: TORawByteString);
begin
  if aBuffer.Length > 0 then
    Prepend(aBuffer.GetBuffer[0], aBuffer.Length)
end;

procedure TORawByteString.Prepend(const aBytes: TBytes);
begin
  if System.Length(aBytes) > 0 then
    Prepend(aBytes[0], System.Length(aBytes))
end;

procedure TORawByteString.SaveToFile(const aFileName: string;
  const aWriteBOM: Boolean);
var
  xS: TStream;
begin
  xS := TFileStream.Create(aFileName, fmCreate);
  try
    SaveToStream(xS, aWriteBOM);
  finally
    xS.Free;
  end;
end;

procedure TORawByteString.SaveToStream(const aStream: TStream;
  const aWriteBOM: Boolean);
begin
  if Length > 0 then
    aStream.WriteBuffer(GetBuffer[0], Length);
end;

procedure TORawByteString.SetBytes(const aBytes: TBytes);
begin
  Length := System.Length(aBytes);
  if Length > 0 then
  begin
    CopyOnWriteCheck;//we need COW check, the length may stay unchanged
    System.Move(aBytes[0], GetBuffer[0], Length);
  end;
end;

procedure TORawByteString.SetLength(const aLength: Integer);
var
  xNewLength: Integer;
begin
  if aLength = 0 then//empty string, just empty the buffer
    FStrBuffer := ''
  else
  if Length <> aLength then
  begin
    xNewLength := (aLength div 2) + 3;//we need max 3 extra characters (2 for integer length + 1/2 for trailing zero).

    if xNewLength = System.Length(FStrBuffer) then
      CopyOnWriteCheck//byte length is different, but FStrBuffer remained the same -> we need COW
    else
      System.SetLength(FStrBuffer, xNewLength);//different lengths -> Delphi handles COW

    PInteger(PChar(FStrBuffer))^ := aLength;//real buffer length is stored in the first 2 char positions
    GetBuffer[Length] := 0;//write trailing zero (for PAnsiChar support)!
  end;
end;

procedure TORawByteString.SetChar(const aIndex: Integer; const aChar: TORawByteChar);
begin
  Assert((aIndex >= 0) and (aIndex < Length));

  CopyOnWriteCheck;//we need COW check here because we change buffer value of the string
  GetBuffer[aIndex] := Byte(aChar);
end;

class procedure TORawByteString.SetDefaultEncoding(aEncoding: TEncoding);
begin
  FDefaultEncoding := aEncoding;
end;

procedure TORawByteString.SetDefChar(const aIndex: Integer;
  const aChar: TORawByteChar);
begin
  SetChar(aIndex-1, aChar);
end;

procedure TORawByteString.SetString(const aString: string; aEncoding: TEncoding);
begin
  if not Assigned(aEncoding) then
    aEncoding := GetDefaultEncoding;

  Length := TEncodingProt(aEncoding).GetByteCountProt(PChar(aString), System.Length(aString));
  if Length > 0 then
  begin
    CopyOnWriteCheck;//we need COW check here because the length may stay unchanged!
    TEncodingProt(aEncoding).GetBytesProt(PChar(aString), System.Length(aString), PByte(GetBuffer), Length);
  end;
end;

procedure TORawByteString.ToLower(aEncoding: TEncoding);
var
  I: Integer;
  xByte: PByte;
begin
  if not Assigned(aEncoding) then
    aEncoding := GetDefaultEncoding;

  if aEncoding is TORawSingleByteEncoding then
  begin//RawByteString
    CopyOnWriteCheck;//we need COW check here because we modify buffer directly
    xByte := PByte(GetBuffer);
    for I := 0 to Length-1 do
    begin
      case xByte^ of
        Ord('A')..Ord('Z'): xByte^ := xByte^ + $20;
      end;
      Inc(xByte);
    end;
  end else
    SetString(SysUtils.LowerCase(GetString(aEncoding)), aEncoding);
end;

procedure TORawByteString.ToUpper(aEncoding: TEncoding);
var
  I: Integer;
  xByte: PByte;
begin
  if not Assigned(aEncoding) then
    aEncoding := GetDefaultEncoding;

  if aEncoding is TORawSingleByteEncoding then
  begin//RawByteString
    CopyOnWriteCheck;//we need COW check here because we modify buffer directly
    xByte := PByte(GetBuffer);
    for I := 0 to Length-1 do
    begin
      case xByte^ of
        Ord('a')..Ord('z'): xByte^ := xByte^ - $20;
      end;
      Inc(xByte);
    end;
  end else
    SetString(SysUtils.UpperCase(GetString(aEncoding)), aEncoding);
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteString.Implicit(const a: PORawByteChar): TORawByteString;
{$ELSE}
class operator TORawByteString.Explicit(const a: PORawByteChar): TORawByteString;
{$ENDIF}
var
  xC: PByte;
begin
  xC := PByte(a);
  while xC^ <> 0 do
    Inc(xC);

  Result.Length := 0;
  Result.Append(a^, NativeInt(xC)-NativeInt(a));
end;

{ TORawByteString.TORawByteStringCharEnumerator }

constructor TORawByteString.TORawByteStringCharEnumerator.Create(
  aBuffer: PORawByteString);
begin
  inherited Create;

  FIndex := -1;
  FString := aBuffer;
end;

function TORawByteString.TORawByteStringCharEnumerator.GetCurrent: TORawByteChar;
begin
  Result := FString^.GetChar(FIndex);
end;

function TORawByteString.TORawByteStringCharEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FString.Length - 1;
  if Result then
    Inc(FIndex);
end;

{ TORawSingleByteEncoding }

constructor TORawSingleByteEncoding.Create;
begin
  inherited;

  FIsSingleByte := True;
  FMaxCharSize := 1;
end;

destructor TORawSingleByteEncoding.Destroy;
begin
  if Self = FSavedInstance then
    FSavedInstance := nil;

  inherited;
end;

class destructor TORawSingleByteEncoding.Destroy;
begin
  //no need to initialize FSavedInstance in class constructor to nil
  //  (global variables are automatically initialized).
  //the FSavedInstance is set to nil in the destructor
  FSavedInstance.Free;

  inherited;
end;

function TORawSingleByteEncoding.GetByteCount(Chars: PChar;
  CharCount: Integer): Integer;
begin
  Result := CharCount;
end;

function TORawSingleByteEncoding.GetBytes(Chars: PChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
  C: PWord;
begin
  Result := ByteCount;
  if Result > CharCount then
    Result := CharCount;

  if Result > 0 then
  begin
    C := PWord(Chars);
    for I := 0 to Result-1 do
    begin
      Bytes[I] := (C^ and $FF);
      Inc(C);
    end;
  end;
end;

function TORawSingleByteEncoding.GetCharCount(Bytes: PByte;
  ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

function TORawSingleByteEncoding.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
var
  I: Integer;
  C: PWord;
begin
  Result := ByteCount;
  if Result > CharCount then
    Result := CharCount;

  if Result > 0 then
  begin
    C := PWord(Chars);
    for I := 0 to Result-1 do
    begin
      C^ := Bytes[I];
      Inc(C);
    end;
  end;
end;

function TORawSingleByteEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := CharCount;
end;

function TORawSingleByteEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

function TORawSingleByteEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

class function TORawSingleByteEncoding.Use: TORawSingleByteEncoding;
begin
  if not Assigned(FSavedInstance) then
    FSavedInstance := TORawSingleByteEncoding.Create;

  Result := FSavedInstance;
end;

{ TORawByteChar }

class operator TORawByteChar.Equal(const a1, a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar = a2.FChar;
end;

class operator TORawByteChar.Equal(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar = (Ord(a2) and $FF);
end;

class operator TORawByteChar.Equal(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) = a2.FChar;
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteChar.Implicit(const a: Char): TORawByteChar;
{$ELSE}
class operator TORawByteChar.Explicit(const a: Char): TORawByteChar;
{$ENDIF}
begin
  Result.FChar := (Ord(a) and $FF);
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteChar.Implicit(const a: TORawByteChar): Char;
{$ELSE}
class operator TORawByteChar.Explicit(const a: TORawByteChar): Char;
{$ENDIF}
begin
  Result := Char(a.FChar);
end;

class operator TORawByteChar.GreaterThan(const a1, a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar > a2.FChar;
end;

class operator TORawByteChar.GreaterThan(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar > (Ord(a2) and $FF);
end;

class operator TORawByteChar.GreaterThan(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) > a2.FChar;
end;

class operator TORawByteChar.GreaterThanOrEqual(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) >= a2.FChar;
end;

class operator TORawByteChar.GreaterThanOrEqual(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar >= (Ord(a2) and $FF);
end;

class operator TORawByteChar.GreaterThanOrEqual(const a1,
  a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar >= a2.FChar;
end;

class operator TORawByteChar.LessThan(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) < a2.FChar;
end;

class operator TORawByteChar.LessThan(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar < (Ord(a2) and $FF);
end;

class operator TORawByteChar.LessThan(const a1, a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar < a2.FChar;
end;

class operator TORawByteChar.LessThanOrEqual(const a1,
  a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar <= a2.FChar;
end;

class operator TORawByteChar.LessThanOrEqual(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar <= (Ord(a2) and $FF);
end;

class operator TORawByteChar.LessThanOrEqual(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) <= a2.FChar;
end;

class operator TORawByteChar.NotEqual(const a1: TORawByteChar;
  const a2: Char): Boolean;
begin
  Result := a1.FChar = (Ord(a2) and $FF);
end;

class operator TORawByteChar.NotEqual(const a1: Char;
  const a2: TORawByteChar): Boolean;
begin
  Result := (Ord(a1) and $FF) = a2.FChar;
end;

function TORawByteChar.ToLower: TORawByteChar;
begin
  case FChar of
    Ord('A')..Ord('Z'): Result.FChar := FChar + $20;
  else
    Result := Self;
  end;
end;

function TORawByteChar.ToUpper: TORawByteChar;
begin
  case FChar of
    Ord('a')..Ord('z'): Result.FChar := FChar - $20;
  else
    Result := Self;
  end;
end;

class operator TORawByteChar.NotEqual(const a1, a2: TORawByteChar): Boolean;
begin
  Result := a1.FChar <> a2.FChar;
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteChar.Implicit(const a: Byte): TORawByteChar;
{$ELSE}
class operator TORawByteChar.Explicit(const a: Byte): TORawByteChar;
{$ENDIF}
begin
  Result.FChar := a;
end;

{$IFDEF ORAW_IMPLICIT}
class operator TORawByteChar.Implicit(const a: TORawByteChar): Byte;
{$ELSE}
class operator TORawByteChar.Explicit(const a: TORawByteChar): Byte;
{$ENDIF}
begin
  Result := a.FChar;
end;

{ TEncodingHelper }

class function TEncodingHelper.RawByte: TEncoding;
begin
  Result := TORawSingleByteEncoding.Use;
end;

end.
