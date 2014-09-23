unit ProxyUtils;

interface

const
  HTTPREQUEST_PROXYSETTING_PROXY = 2;

  function ProxyActive: Boolean;
  function GetProxyServer: string;
  function GetProxyOverride: string;

implementation

uses
  System.SysUtils,
  System.Win.Registry,
  Winapi.Windows;

const
  INTERNET_SETTINGS = 'Software\Microsoft\Windows\CurrentVersion\Internet Settings';
  PROXY_ENABLED = 'ProxyEnable';
  PROXY_SERVER = 'ProxyServer';
  PROXY_OVERRIDE = 'ProxyOverride';


function ProxyActive: Boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      Result := Reg.ReadInteger(PROXY_ENABLED) = 1;
    finally
      Reg.Free;
    end;
  except
    on E: ERegistryException do
      Result := false;
  end;
end;

function GetProxyServer: string;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      Result := Reg.ReadString(PROXY_SERVER);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do
      Result := '';
  end;
end;

function GetProxyOverride: string;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.OpenKey(INTERNET_SETTINGS, false);
      Result := Reg.ReadString(PROXY_OVERRIDE);
    finally
      Reg.Free;
    end
  except
    on E: ERegistryException do
      Result := '';
  end;
end;

end.
