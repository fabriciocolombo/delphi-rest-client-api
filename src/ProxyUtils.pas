unit ProxyUtils;

interface

  function ProxyActive: Boolean;
  function GetProxyServer: string;
  function GetProxyOverride: string;

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
      Result := Reg.ReadString(Value);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do
      Result := '';
  end;
end;

function ReadIntegerRegistryValue(const Value: string): integer;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      Result := Reg.ReadInteger(Value);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do
      Result := 0;
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

end.
