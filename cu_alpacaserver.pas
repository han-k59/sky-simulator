unit cu_alpacaserver;

{$mode objfpc}{$H+}
{
Copyright (C) 2020 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

interface

uses cu_alpacadevice, cu_tcpserver, synautil, math,
  contnrs, Classes, SysUtils;

type

  T_AlpacaDeviceElement = class(TObject)
    public
      devtype: TAlpacaDeviceType;
      device: T_AlpacaDevice;
      devicenum: integer;
      devicename: string;
      devicepath: string;
      setuppath: string;
  end;

  T_AlpacaDeviceList = class(TObjectList)
  private
    function GetItem(Index: integer): T_AlpacaDeviceElement;
    procedure SetItem(Index: integer; AValue: T_AlpacaDeviceElement);
  public
    property Items[Index: integer]: T_AlpacaDeviceElement read GetItem write SetItem; default;
  end;


  T_AlpacaServer = class(TComponent)
    protected
      TCPDaemon: TTCPDaemon;
      Discovery: TDiscoveryDaemon;
      DeviceList: T_AlpacaDeviceList;
      ServerTransactionID: LongWord;
      FIPAddr, FIPPort : string;
      FDiscoveryPort : string;
      FServerName, FManufacturer, FLocation : string;
      FShowMsg: TStringProc;
      FShowError: TStringProc;
      FPortMsg: TStringProc;
      FDiscoveryPortMsg: TStringProc;
      procedure ShowError(var msg:string);
      procedure ShowMsg(var msg:string);
      procedure ShowSocket(var msg:string);
      procedure ShowDiscoverySocket(var msg:string);
      function ProcessGet(HttpRequest: string): string;
      function ProcessGetImageBytes(HttpRequest: string; var s: TMemoryStream): string;
      function ProcessPut(HttpRequest,arg: string): string;
      function ProcessSetup(HttpRequest: string): string;
      function ProcessManagement(HttpRequest: string): string;
      function DecodeManagementRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      procedure StartServer;
      procedure StopServer;
      procedure AddDevice(devtype:TAlpacaDeviceType; device:T_AlpacaDevice);
      property IPAddr:string read FIPAddr write FIPAddr;
      property IPPort:string read FIPPort write FIPPort;
      property DiscoveryPort:string read FDiscoveryPort write FDiscoveryPort;
      property ServerName:string read FServerName write FServerName;
      property Manufacturer:string read FManufacturer write FManufacturer;
      property Location:string read FLocation write FLocation;
      property onShowError: TStringProc read FShowError write FShowError;
      property onShowMsg: TStringProc read FShowMsg write FShowMsg;
      property onPortMsg: TStringProc read FPortMsg write FPortMsg;
      property onDiscoveryPortMsg: TStringProc read FDiscoveryPortMsg write FDiscoveryPortMsg;
  end;

  const
    server_version='1.0.1';

implementation

//////////////////////// T_AlpacaDeviceList ////////////////////////
function T_AlpacaDeviceList.GetItem(Index: integer): T_AlpacaDeviceElement;
begin
  Result := T_AlpacaDeviceElement(inherited Items[Index]);
end;

procedure T_AlpacaDeviceList.SetItem(Index: integer; AValue: T_AlpacaDeviceElement);
begin
  inherited Items[Index] := AValue;
end;

//////////////////////// T_AlpacaServer ////////////////////////

constructor T_AlpacaServer.Create(AOwner: TComponent);
begin
  inherited Create(aOwner);
  FIPAddr := '0.0.0.0';
  FIPPort := '11111';
  FDiscoveryPort := '32227';
  FServerName := 'ASCOM Alpaca Server - Freepascal-Synapse';
  FManufacturer:='Alpaca Server project, https://github.com/pchev/alpaca_server';
  FLocation:='sky_simulator';
  DefaultFormatSettings.DecimalSeparator := '.';
  DefaultFormatSettings.ThousandSeparator := ',';
  DefaultFormatSettings.DateSeparator := '/';
  DefaultFormatSettings.TimeSeparator := ':';
  TCPDaemon:=TTCPDaemon.Create;
  DeviceList:=T_AlpacaDeviceList.Create;
  TCPDaemon.onShowError := @ShowError;
  TCPDaemon.onShowMsg := @ShowMsg;
  TCPDaemon.onShowSocket := @ShowSocket;
  TCPDaemon.onProcessGet:=@ProcessGet;
  TCPDaemon.onProcessGetImageBytes:=@ProcessGetImageBytes;
  TCPDaemon.onProcessPut:=@ProcessPut;
  ServerTransactionID := 0;
  Discovery:=TDiscoveryDaemon.Create;
  Discovery.onShowError := @ShowError;
  Discovery.onShowMsg := @ShowMsg;
  Discovery.onShowSocket := @ShowDiscoverySocket;

end;

destructor  T_AlpacaServer.Destroy;
var i: integer;
begin
  if DeviceList<>nil then begin
    for i:=0 to DeviceList.Count-1 do begin
       T_AlpacaDeviceElement(DeviceList.Items[i]).device.Free;
    end;
    DeviceList.Free;
  end;
end;

procedure T_AlpacaServer.AddDevice(devtype:TAlpacaDeviceType; device:T_AlpacaDevice);
var elem: T_AlpacaDeviceElement;
    i,n: integer;
begin
  n:=0;
  for i:=0 to DeviceList.Count-1 do begin
    if DeviceList[i].devtype=devtype then
      n:=max(n,DeviceList[i].devicenum+1);
  end;
  elem:=T_AlpacaDeviceElement.Create;
  elem.device:=device;
  elem.devtype:=devtype;
  elem.devicenum:=n;
  elem.devicename:=AlpacaDeviceName[ord(devtype)];
  elem.devicepath:='/api/'+ApiVersion+'/'+elem.devicename+'/'+IntToStr(elem.devicenum)+'/';
  elem.setuppath:='/setup/'+ApiVersion+'/'+elem.devicename+'/'+IntToStr(elem.devicenum)+'/';
  elem.device.Path:=elem.devicepath;
  elem.device.SetupPath:=elem.setuppath;
  DeviceList.Add(elem);
end;

procedure T_AlpacaServer.StartServer;
begin
  TCPDaemon.IPaddr := FIPAddr;
  TCPDaemon.IPport := FIPPort;
  TCPDaemon.Start;
  Discovery.IPaddr := FIPAddr;
  Discovery.IPport := FDiscoveryPort;
  Discovery.AlpacaPort := FIPPort;
  Discovery.Start;
end;

procedure T_AlpacaServer.StopServer;
begin
  TCPDaemon.Terminate;
  Discovery.Terminate;
end;

procedure T_AlpacaServer.ShowError(var msg:string);
var buf:string;
begin
   buf:=copy(msg,1,400);
   if assigned(FShowError) then FShowError(buf);
end;

procedure T_AlpacaServer.ShowMsg(var msg:string);
var buf:string;
begin
  buf:=copy(msg,1,400);
  if assigned(FShowMsg) then FShowMsg(buf);
end;

procedure T_AlpacaServer.ShowSocket(var msg:string);
var buf:string;
begin
   buf:=copy(msg,1,400);
   if assigned(FPortMsg) then FPortMsg(buf);
end;

procedure T_AlpacaServer.ShowDiscoverySocket(var msg:string);
var buf:string;
begin
   buf:=copy(msg,1,400);
   if assigned(FDiscoveryPortMsg) then FDiscoveryPortMsg(buf);
end;

function T_AlpacaServer.ProcessGet(HttpRequest: string): string;
var req,doc, total: string;
    i,p,n,httpstatus: integer;
begin
  try
  if copy(HttpRequest,1,6)='/setup' then begin
    result:=ProcessSetup(HttpRequest);
  end
  else if copy(HttpRequest,1,11)='/management' then begin
    result:=ProcessManagement(HttpRequest);
  end
  else begin
    req:=HttpRequest;
    n:=-1;
    for i:=0 to DeviceList.Count-1 do begin
      p:=pos(DeviceList[i].devicepath,req);
      if p>0 then begin
        n:=i;
        Delete(req,1,p-1);
        break;
      end;
    end;
    if n>=0 then begin
      inc(ServerTransactionID);
      doc:=DeviceList[n].device.ProcessGetRequest(req,ServerTransactionID,httpstatus) + CRLF;

      total:='GET '+req +' => '+doc ;
      ShowMsg(total);
      if httpstatus=200 then begin
      result:='HTTP/1.1 200' + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: application/json; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc;
      end
      else begin
        result:='HTTP/1.1 '+inttostr(httpstatus) + CRLF
               +'Connection: close' + CRLF
               +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
               +'Content-type: text/html; charset=utf-8' + CRLF
               +'Date: ' + Rfc822DateTime(now) + CRLF
               +'Server: ' + FServerName + CRLF
               +'' + CRLF
               +doc;
      end;
    end
    else begin
      doc:='400 - Not found.';
      ShowMsg(doc); {allow the instruction to be logged}
      result:='HTTP/1.1 400' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc + CRLF;
    end;

  end;
  except
    on E: Exception do begin
      doc:='500 - '+E.Message;
      ShowMsg(doc);{allow the instruction to be logged}
      result:='HTTP/1.1 500' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc + CRLF;
    end;
  end;
end;

function T_AlpacaServer.ProcessGetImageBytes(HttpRequest: string; var s: TMemoryStream): string;
var req,doc: string;
    i,p,n,httpstatus: integer;
begin
  try
  doc:='GET '+HttpRequest+' format: ImageBytes';
  ShowMsg(doc); {allow the instruction to be logged}
  req:=HttpRequest;
  n:=-1;
  for i:=0 to DeviceList.Count-1 do begin
    p:=pos(DeviceList[i].devicepath,req);
    if p>0 then begin
      n:=i;
      Delete(req,1,p-1);
      break;
    end;
  end;
  if n>=0 then begin
  inc(ServerTransactionID);
  doc:=DeviceList[n].device.ProcessGetImageBytesRequest(req,ServerTransactionID,httpstatus,s) + CRLF;
  if httpstatus=200 then begin
  result:='HTTP/1.1 200' + CRLF
         +'Connection: close' + CRLF
         +'Content-length: ' + IntTostr(s.Size) + CRLF
         +'Content-type: application/imagebytes' + CRLF
         +'Date: ' + Rfc822DateTime(now) + CRLF
         +'Server: ' + FServerName + CRLF
         +'' + CRLF;
  end
  else begin
    result:='HTTP/1.1 '+inttostr(httpstatus) + CRLF
           +'Connection: close' + CRLF
           +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ' + FServerName + CRLF
           +'' + CRLF
           +doc;
  end;
end
else begin
  doc:='400 - Not found.';
  ShowMsg(doc); {allow the instruction to be logged}
  result:='HTTP/1.1 400' + CRLF
         +'Connection: close' + CRLF
         +'Content-type: text/html; charset=utf-8' + CRLF
         +'Date: ' + Rfc822DateTime(now) + CRLF
         +'Server: ' + FServerName + CRLF
         +'' + CRLF
         +doc + CRLF;
end;
except
  on E: Exception do begin
    doc:='500 - '+E.Message;
    ShowMsg(doc); {allow the instruction to be logged}
    result:='HTTP/1.1 500' + CRLF
           +'Connection: close' + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ' + FServerName + CRLF
           +'' + CRLF
           +doc + CRLF;
  end;
end;
end;

function T_AlpacaServer.ProcessPut(HttpRequest,arg: string): string;
var req,doc,total: string;
    i,p,n,httpstatus: integer;
begin
  try
  req:=HttpRequest;

  n:=-1;
  for i:=0 to DeviceList.Count-1 do begin
    p:=pos(DeviceList[i].devicepath,req);
    if p>0 then begin
      n:=i;
      Delete(req,1,p-1);
      break;
    end;
  end;
  if n>=0 then begin
    inc(ServerTransactionID);
    doc:=DeviceList[n].device.ProcessPutRequest(req,arg,ServerTransactionID,httpstatus) + CRLF;
    total:='PUT'+req+'   '+arg + '   =>   '+doc  ;
    ShowMsg(total); {allow the instruction to be logged}
    if httpstatus=200 then begin
      result:='HTTP/1.1 200' + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: application/json; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc;
    end
    else begin
      result:='HTTP/1.1 '+inttostr(httpstatus) + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc;
    end;
  end
  else begin
    doc:='400 - Not found.';
    ShowMsg(doc); {allow the instruction to be logged}
    result:='HTTP/1.1 400' + CRLF
           +'Connection: close' + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ' + FServerName + CRLF
           +'' + CRLF
           +doc + CRLF;
  end;
  except
    on E: Exception do begin
      doc:='500 - '+E.Message;
      ShowMsg(doc); {allow the instruction to be logged}
      result:='HTTP/1.1 500' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc + CRLF;
    end;
  end;
end;

function T_AlpacaServer.ProcessSetup(HttpRequest: string): string;
var doc,req: string;
    n,i,p,httpstatus: integer;
begin
  if HttpRequest='/setup' then begin
    doc:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
         '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
         '<title>Alpaca Server</title></head><body text>'+
         '<H1>Alpaca Server Setup</H1><br/>'+
         'To setup this Alpaca server you must use the GUI of the main program at the tab Alpaca<br/><br/>'+
         'You can change the server listen adress and port.<br/>'+
         'The program must be restarted to apply the changes.'+
         '</body></html>';
    result:='HTTP/1.1 200' + CRLF
           +'Connection: close' + CRLF
           +'Content-type: text/html; charset=utf-8' + CRLF
           +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ' + FServerName + CRLF
           +'' + CRLF
           +doc;
  end else begin
    req:=HttpRequest;
    n:=-1;
    for i:=0 to DeviceList.Count-1 do begin
      p:=pos(DeviceList[i].setuppath,req);
      if p>0 then begin
        n:=i;
        Delete(req,1,p-1);
        break;
      end;
    end;
    if (n>=0) then begin
      doc:=DeviceList[n].device.ProcessSetup(req,httpstatus);
      if httpstatus=200 then begin
        result:='HTTP/1.1 200' + CRLF
               +'Connection: close' + CRLF
               +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
               +'Content-type: text/html; charset=utf-8' + CRLF
               +'Date: ' + Rfc822DateTime(now) + CRLF
               +'Server: ' + FServerName + CRLF
               +'' + CRLF
               +doc;
      end
      else begin
        result:='HTTP/1.1 '+inttostr(httpstatus) + CRLF
               +'Connection: close' + CRLF
               +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
               +'Content-type: text/html; charset=utf-8' + CRLF
               +'Date: ' + Rfc822DateTime(now) + CRLF
               +'Server: ' + FServerName + CRLF
               +'' + CRLF
               +doc;
      end;
    end
    else begin
      doc:='400 - Not found.';
      result:='HTTP/1.1 400' + CRLF
             +'Connection: close' + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc + CRLF;
    end;
  end;
end;

function T_AlpacaServer.DecodeManagementRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
var i,p: integer;
    buf,MPath:string;
begin
result:=false;
MPath:='/management/';
buf:=copy(req,1,length(MPath));
if copy(req,1,length(MPath))<>MPath then exit;
Delete(req,1,length(MPath));
p:=pos('?',req);
if p<=0 then begin
  method:=req;
  params.Clear;
  ClientID:=0;
  ClientTransactionID:=0;
end
else begin
  method:=copy(req,1,p-1);
  delete(req,1,p);
  params.Clear;
  ClientID:=0;
  ClientTransactionID:=0;
  SplitRec(req,'&',params);
  for i:=0 to params.Count-1 do begin
    if uppercase(copy(params[i],1,9))='CLIENTID=' then begin
      buf:=params[i];
      delete(buf,1,9);
      ClientID:=StrToIntDef(buf,0);
      params.Delete(i);
      break;
    end;
  end;
  for i:=0 to params.Count-1 do begin
    if uppercase(copy(params[i],1,20))='CLIENTTRANSACTIONID=' then begin
      buf:=params[i];
      delete(buf,1,20);
      ClientTransactionID:=StrToIntDef(buf,0);
      params.Delete(i);
      break;
    end;
  end;
end;
result:=true;
end;

function T_AlpacaServer.ProcessManagement(HttpRequest: string): string;
var doc: string;
    method,value: string;
    apiversions: array of integer;
    params: TStringlist;
    i,status: integer;
    ClientID,ClientTransactionID:Longword;
    ErrorNumber: integer;
    ErrorMessage:string;
begin
  params:=TStringlist.Create;
  try
  DecodeManagementRequest(HttpRequest,method,params,ClientID,ClientTransactionID);
  status:=400;
  doc:='400 - Not found.';
  ErrorNumber:=0;
  ErrorMessage:='';
  if method='apiversions' then begin
    SetLength(apiversions,1);
    apiversions[0]:=1;
    doc:=DeviceList[0].device.FormatIntArrayResp(apiversions,ClientTransactionID,ServerTransactionID,ErrorNumber,ErrorMessage);
    status:=200;
  end
  else if method='v1/description' then begin
    value:='{"ServerName": "' + FServerName + '",'+
            '"Manufacturer": "' + FManufacturer + '",'+
            '"ManufacturerVersion": "v'+server_version+'",'+
            '"Location": "'+ FLocation +'"}';
    doc:=DeviceList[0].device.FormatRawResp(value,ClientTransactionID,ServerTransactionID,ErrorNumber,ErrorMessage);
    status:=200;
  end
  else if method='v1/configureddevices' then begin
    value:='[';
    for i:=0 to DeviceList.Count-1 do begin
      value:=value+'{"DeviceName": "'+DeviceList[i].device.Name+'",'+
                    '"DeviceType": "'+DeviceList[i].devicename+'",'+
                    '"DeviceNumber": '+IntToStr(DeviceList[i].devicenum)+','+
                    '"UniqueID": "'+DeviceList[i].device.GetGuid+'"},';
    end;
    SetLength(value,Length(value)-1);
    value:=value+']';
    doc:=DeviceList[0].device.FormatRawResp(value,ClientTransactionID,ServerTransactionID,ErrorNumber,ErrorMessage);
    status:=200;
  end;
  if status=200 then begin
    result:='HTTP/1.1 200' + CRLF
           +'Connection: close' + CRLF
           +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
           +'Content-type: application/json; charset=utf-8' + CRLF
           +'Date: ' + Rfc822DateTime(now) + CRLF
           +'Server: ' + FServerName + CRLF
           +'' + CRLF
           +doc;
    end
    else begin
      result:='HTTP/1.1 '+inttostr(status) + CRLF
             +'Connection: close' + CRLF
             +'Content-length: ' + IntTostr(Length(Doc)) + CRLF
             +'Content-type: text/html; charset=utf-8' + CRLF
             +'Date: ' + Rfc822DateTime(now) + CRLF
             +'Server: ' + FServerName + CRLF
             +'' + CRLF
             +doc;
    end;
  finally
   params.free;
  end;
end;

end.

