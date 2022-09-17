{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit alpacaserver;

{$warn 5023 off : no warning about unused units}
interface

uses
  cu_serial, cu_tcpserver, cu_alpacadevice, cu_alpacafocuser, cu_alpacaserver, cu_alpacarotator,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('alpacaserver', @Register);
end.
