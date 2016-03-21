unit ProxyUtils;

interface

  function ProxyActive: Boolean;
  function GetProxyServer: string;
  function GetProxyOverride: string;
  function GetProxyServerIP: string;
  function GetProxyServerPort: Integer;

implementation

uses
  SysUtils,
  Registry,
  Windows;

const
  INTERNET_SETTINGS = 'Software\Microsoft\Windows\CurrentVersion\Internet Settings';
  PROXY_ENABLED = 'ProxyEnable';
  PROXY_SERVER = 'ProxyServer';
  PROXY_OVERRIDE = 'ProxyOverride';


function ReadStringRegistryValue(const Value: string): string;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      if Reg.ValueExists(Value) then
        Result := Reg.ReadString(Value);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do
      Result := '';
  end;
end;

function ReadIntegerRegistryValue(const Value: string): Integer;
var
  Reg: TRegistry;
begin
  Result := 0;
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      if Reg.ValueExists(Value) then
        Result := Reg.ReadInteger(Value);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do;
  end;
end;

function ProxyActive: Boolean;
begin
  Result := ReadIntegerRegistryValue(PROXY_ENABLED) = 1;
end;

function GetProxyServer: string;
begin
  Result := ReadStringRegistryValue(PROXY_SERVER);
end;

function GetProxyOverride: string;
begin
  Result := ReadStringRegistryValue(PROXY_OVERRIDE);
end;

function GetProxyServerIP: string;
var
  ProxyServer: string;
begin
  ProxyServer := GetProxyServer;
  if ProxyServer = '' then
  begin
    Result := '';
    Exit;
  end;

  Result := Copy(ProxyServer, 1, Pos(':', ProxyServer)-1);
end;

function GetProxyServerPort: Integer;
var
  ProxyServer: string;
begin
  ProxyServer := GetProxyServer;
  Result := StrToIntDef(Copy(ProxyServer,  Pos(':', ProxyServer)+1, length(ProxyServer)-1), 0);
end;

end.
