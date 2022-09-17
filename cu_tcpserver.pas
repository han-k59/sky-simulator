unit cu_tcpserver;
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

{ TCP/IP Connexion, based on Synapse Echo demo }

{ April 2022, modified to persistent connection by Han Kleijn}

{$MODE objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Variants, comobj, ActiveX,
  {$else}
  process,
  {$ENDIF}
  blcksock, synsock, synautil, synaip, cu_alpacadevice,
  fpjson, jsonparser, SysUtils, Classes,strutils;

const
  msgFailed='Failed!';

type

  TTCPThrd = class(TThread)
  private
    FSock: TTCPBlockSocket;
    CSock: TSocket;
    FHttpRequest: string;
    FBody: string;
    FHttpResult: string;
    FImageBytes: TMemoryStream;
    FConnectTime: double;
    FTerminate: TIntProc;
    FProcessGet: TGetCmd;
    FProcessGetImageBytes: TGetImageBytes;
    FProcessPut: TPutCmd;
  public
    abort, stoping: boolean;
    remoteip, remoteport: string;
    constructor Create(hsock: tSocket);
    destructor Destroy; override;
    procedure Execute; override;
    procedure SendData(str: string);
    procedure ProcessGet;
    procedure ProcessGetImageBytes;
    procedure ProcessPut;
    property sock: TTCPBlockSocket read FSock;
    property ConnectTime: double read FConnectTime;
    property Terminated;
    property onTerminate: TIntProc read FTerminate write FTerminate;

    property onProcessGet: TGetCmd read FProcessGet write FProcessGet;
    property onProcessGetImageBytes: TGetImageBytes read FProcessGetImageBytes write FProcessGetImageBytes;
    property onProcessPut: TPutCmd read FProcessPut write FProcessPut;
  end;

  TTCPDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
    FShowError: TStringProc;
    FShowMsg: TStringProc;
    FShowSocket: TStringProc;
    FIPaddr, FIPport: string;
    FProcessGet: TGetCmd;
    FProcessGetImageBytes: TGetImageBytes;
    FProcessPut: TPutCmd;
    procedure ShowError;
    function GetIPport: string;
  public
    stoping: boolean;
    TCPThrd: TTCPThrd;
    constructor Create;
    procedure Execute; override;
    procedure ShowSocket;
    property IPaddr: string read FIPaddr write FIPaddr;
    property IPport: string read GetIPport write FIPport;
    property onShowError: TStringProc read FShowError write FShowError;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
    property onShowSocket: TStringProc read FShowSocket write FShowSocket;
    property onProcessGet: TGetCmd read FProcessGet write FProcessGet;
    property onProcessGetImageBytes: TGetImageBytes read FProcessGetImageBytes write FProcessGetImageBytes;
    property onProcessPut: TPutCmd read FProcessPut write FProcessPut;
  end;

  TDiscoveryDaemon = class(TThread)
  private
    Sock,ReplySock: TUDPBlockSocket;
    FShowSocket: TStringProc;
    FShowError: TStringProc;
    FShowMsg: TStringProc;
    FIPaddr, FIPport, FAlpacaPort,FDiscoveryStr: string;
    servermask,serverip: dword;
    procedure SetIPaddr(value:string);
    procedure ShowError;
    function GetIPport: string;
    function GetMask(addr:string): dword;
  public
    stoping: boolean;
    constructor Create;
    procedure Execute; override;
    procedure ShowSocket;
    property IPaddr: string read FIPaddr write SetIPaddr;
    property IPport: string read GetIPport write FIPport;
    property AlpacaPort: string read FAlpacaPort write FAlpacaPort;
    property DiscoveryStr: string read FDiscoveryStr write FDiscoveryStr;
    property onShowError: TStringProc read FShowError write FShowError;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
    property onShowSocket: TStringProc read FShowSocket write FShowSocket;
  end;


implementation

{$ifdef darwin}
{$ifdef CPU32}
 uses BaseUnix;       //  to catch SIGPIPE

 var
   NewSigRec, OldSigRec: SigActionRec;
   res: integer;
{$endif}
{$endif}

constructor TTCPDaemon.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

procedure TTCPDaemon.ShowError;
var
  msg: string;
begin
  msg := IntToStr(sock.lasterror) + ' ' + sock.GetErrorDesc(sock.lasterror);
  if assigned(FShowError) then FShowError(msg);
end;

function TTCPDaemon.GetIPport: string;
begin
  if sock=nil then
    result:=FIPport
  else begin
    sock.GetSins;
    result := IntToStr(sock.GetLocalSinPort);
  end;
end;

procedure TTCPDaemon.ShowSocket;
var
  locport: string;
begin
  sock.GetSins;
  locport := IntToStr(sock.GetLocalSinPort);
  if assigned(FShowSocket) then
    FShowSocket(locport);
end;


procedure TTCPDaemon.Execute;
var
  ClientSock: TSocket;
begin
  //writetrace('start tcp deamon');
  stoping := False;
  sock := TTCPBlockSocket.Create;
  //writetrace('blocksocked created');
  try
    with sock do
    begin
      //writetrace('create socket');
      CreateSocket;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      MaxLineLength := 1024;
      {$ifdef linux}
      //writetrace('setlinger');
      setLinger(True, 0);
      {$endif}
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('bind to '+fipaddr+' '+fipport);
      bind(FIPaddr, FIPport);
      if lasterror <> 0 then
        Synchronize(@ShowError);
      //writetrace('listen');
      listen;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      Synchronize(@ShowSocket);
      //writetrace('start main loop');
      repeat
        if stoping or terminated then
          break;
        if canread(500) and (not terminated) and (not stoping) then
        begin
          ClientSock := accept;
          if lastError = 0 then
          begin {ready one HTTP header plus optional body}
            TCPThrd := TTCPThrd.Create(ClientSock);
            TCPThrd.onProcessGet := FProcessGet;
            TCPThrd.onProcessGetImageBytes := FProcessGetImageBytes;
            TCPThrd.onProcessPut := FProcessPut;
            TCPThrd.Start;

          end
          else if lasterror <> 0 then
            Synchronize(@ShowError);
        end;
      until False;
    end;
  finally
    //  Suspended:=true;
    Sock.CloseSocket;
    Sock.Free;
    //  terminate;
  end;
end;

constructor TTCPThrd.Create(Hsock: TSocket);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Csock := Hsock;
  abort := False;
  FImageBytes:=TMemoryStream.Create;
end;

destructor TTCPThrd.Destroy;
begin
  if FSock<>nil then begin
    FSock.AbortSocket;
    Fsock.Free;
  end;
  if FImageBytes<>nil then
    FImageBytes.Free;
  inherited Destroy;
end;

procedure TTCPThrd.Execute; {reads one HTTP header plus optional body}
var
  body,method,buf,upbuf: string;
  cl,i,j,k: integer;
  imagebytes: boolean;
begin
  try
    Fsock := TTCPBlockSocket.Create;
    FConnectTime := now;
    stoping := False;
    try
      Fsock.socket := CSock;
      Fsock.GetSins;
      Fsock.MaxLineLength := 1024;
      FSock.SetLinger(true,1000);
      remoteip := Fsock.GetRemoteSinIP;
      remoteport := IntToStr(Fsock.GetRemoteSinPort);
      with Fsock do
      begin
        if stoping or terminated then
          exit;
        repeat
          buf:=RecvTerminated(100,#13+#10+#13+#10);// receive the full header
          if LastError=WSAETIMEDOUT then continue;// Timeout, try to read next
          if lastError<>0 then break;             // Other error, maybe client disconnect

          //GET /api/v1/camera/0/driverversion?ClientID=3200023&ClientTransactionID=373 HTTP/1.1\r\n
          //Host: 127.0.0.1:11111\r\n
          //Keep-Alive: 300\r\n
          //Connection: keep-alive\r\n
          //\r\n

          method:=uppercase(copy(buf,1,3));{either GET or PUT}
          k:=posex(' ',buf,6);// find end of Request-URI
          FHttpRequest:=copy(buf,5,k-5);//extract Request-URI, something like '/api/v1/camera/0/ccdtemperature?ClientID=3200089&ClientTransactionID=120'
          k:=k+length(' HTTP/'+#13+#10); //pointer to the fields (second line and further}

          cl:=-1; {assume no body behind header}
          upbuf:=UpperCase(copy(buf,k,500));   //field headers should be treated as case insensitive. Assume total field size 500 max
          i:=Pos('CONTENT-LENGTH:',Upbuf);
          if i>0 then {there is a body behind the header}
          begin
             i:=i+length('CONTENT-LENGTH:');
             j:=PosEx(#13,UpBuf,i);
             if j=0 then j:=999; {content lenght was the last line}
             cl:=StrToIntDef(trim(copy(Upbuf,i,j-i)),0); {length of the body behind header}
          end;
          imagebytes:=pos('APPLICATION/IMAGEBYTES',Upbuf,7)>0; {found ACCEPT: APPLICATION/IMAGEBYTES. Only requested when an image is made}

         //     PUT /api/v1/camera/0/connected HTTP/1.1\r\n                   header
         //     Host: 127.0.0.1:11111\r\n                                     header
         //     Keep-Alive: 300\r\n                                           header
         //     Connection: keep-alive\r\n                                    header
         //     Content-Type: application/x-www-form-urlencoded\r\n           header
         //     Content-Length: 55\r\n                                        header
         //     \r\n                                                          end header marker
         //     Connected=True&ClientID=3200023&ClientTransactionID=370       body

          if cl>0 then {there is a body behind the header}
            body:=RecvBufferStr(cl,100) {receive the body, something like: Connected=True&ClientID=3200023&ClientTransactionID=370}
          else
            body:='';

          if method='GET' then begin
             if imagebytes then begin
               // imagearray imagebytes request
               Synchronize(@ProcessGetImageBytes);
               SendString(FHttpResult);
               SendStreamRaw(FImageBytes);
               FImageBytes.Clear;
             end
             else begin
               // all other request
               Synchronize(@ProcessGet);
               SendString(FHttpResult);
             end;
          end
          else if method='PUT' then begin
             FBody:=body;
             Synchronize(@ProcessPut);
             SendString(FHttpResult);
          end;

        until false;{repeat}
      end;
    finally
    end;
  except
  end;
end;

procedure TTCPThrd.Senddata(str: string);
begin
  try
    if Fsock <> nil then
      with Fsock do
      begin
        if terminated then
          exit;
        SendString(str + CRLF);
        if LastError <> 0 then
          terminate;
      end;
  except
    terminate;
  end;
end;

procedure TTCPThrd.ProcessGet;
begin
  try
    FHttpResult:='';
    if Assigned(FProcessGet) then
      FHttpResult := FProcessGet(FHttpRequest);
  except
    FHttpResult := msgFailed;
  end;
end;

procedure TTCPThrd.ProcessGetImageBytes;
begin
  try
    FHttpResult:='';
    if Assigned(FProcessGetImageBytes) then
      FHttpResult := FProcessGetImageBytes(FHttpRequest,FImageBytes);
  except
    FHttpResult := msgFailed;
  end;
end;

procedure TTCPThrd.ProcessPut;
begin
  try
    FHttpResult:='';
    if Assigned(FProcessPut) then
      FHttpResult := FProcessPut(FHttpRequest,FBody);
  except
    FHttpResult := msgFailed;
  end;
end;

///////////////// UPD for discovery /////////////////////

constructor TDiscoveryDaemon.Create;
begin
  inherited Create(True);
  ReplySock:=TUDPBlockSocket.Create;
  FreeOnTerminate := True;
  FDiscoveryStr:='alpacadiscovery1';
end;

procedure TDiscoveryDaemon.ShowError;
var
  msg: string;
begin
  msg := IntToStr(sock.lasterror) + ' ' + sock.GetErrorDesc(sock.lasterror);
  if assigned(FShowError) then FShowError(msg);
end;

function TDiscoveryDaemon.GetIPport: string;
begin
  if sock=nil then
    result:=FIPport
  else begin
    sock.GetSins;
    result := IntToStr(sock.GetLocalSinPort);
  end;
end;

procedure TDiscoveryDaemon.ShowSocket;
var
  locport: string;
begin
  sock.GetSins;
  locport := IntToStr(sock.GetLocalSinPort);
  if assigned(FShowSocket) then
    FShowSocket(locport);
end;

function TDiscoveryDaemon.GetMask(addr:string): dword;
var
  ip1,ip2:dword;
  sl: TStringList;
  i: integer;
  {$IFDEF UNIX}
  AProcess: TProcess;
  processok: boolean;
  s,buf: string;
  jl,ja: TJSONData;
  j,k,n,l: integer;
  pl:dword;
  {$ENDIF}
  {$IFDEF WINDOWS}
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
  ip,mask: string;
  hasIP, hasMask: boolean;
const
  wbemFlagForwardOnly = $00000020;
  {$ENDIF}
begin
  result:=$0;
  if addr=cAnyHost then begin
    exit;
  end;
  if copy(addr,1,3)='127' then begin
    result:=$ff000000;
    exit;
  end;
  ip1:=StrToIp(addr);
  sl:=TStringList.Create();
  {$IFDEF WINDOWS}
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT IPAddress,IPSubnet FROM Win32_NetworkAdapterConfiguration','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  // loop all interfaces
  while oEnum.Next(1, FWbemObject, iValue) = 0 do
  begin
    hasIP:=false;
    hasMask:=false;
    if not VarIsClear(FWbemObject.IPAddress) and not VarIsNull(FWbemObject.IPAddress) then begin
     // this interface address is assigned
     for i := VarArrayLowBound(FWbemObject.IPAddress, 1) to VarArrayHighBound(FWbemObject.IPAddress, 1) do begin
       ip:=String(FWbemObject.IPAddress[i]);
       if pos(':',ip)>0 then continue; // TODO: IPv6
       hasIP:=true;
       break;
     end;
     if not VarIsClear(FWbemObject.IPSubnet) and not VarIsNull(FWbemObject.IPSubnet) then begin
      // mask assigned
      for i := VarArrayLowBound(FWbemObject.IPSubnet, 1) to VarArrayHighBound(FWbemObject.IPSubnet, 1) do begin
        mask:=String(FWbemObject.IPSubnet[i]);
        if pos('.',mask)=0 then continue; // TODO: IPv6
        hasMask:=true;
        break;
      end;
     end;
     if hasIP and hasMask then begin
       ip2:=StrToIp(ip);
       if ip2=ip1 then begin
         result:=StrToIp(mask);
         break;
       end;
     end;
    end;
    FWbemObject:=Unassigned;
  end;
  {$ENDIF}
  {$IFDEF UNIX}
  // Try to use ip
  AProcess:=TProcess.Create(nil);
  AProcess.Executable := '/bin/ip';
  AProcess.Parameters.Add('-json');
  AProcess.Parameters.Add('address');
  AProcess.Options := AProcess.Options + [poUsePipes, poWaitOnExit];
  try
    try
    AProcess.Execute();
    processok:=(AProcess.ExitStatus=0);
    except
      processok:=false;
    end;
    sl.LoadFromStream(AProcess.Output);
  finally
    AProcess.Free();
  end;
  if processok then begin
    buf:='';
    for i:=0 to sl.Count-1 do
      buf:=buf+sl[i];
    jl:=GetJSON(buf);
    try
    for i:=0 to jl.Count-1 do begin
      ja:=jl.Items[i].FindPath('addr_info');
      if ja<>nil then for j:=0 to ja.Count-1 do begin
        if ja.Items[j].FindPath('family')=nil then Continue;
        buf:=ja.Items[j].FindPath('family').AsString;
        if buf='inet' then begin
          if ja.Items[j].FindPath('local')=nil then Continue;
          buf:=ja.Items[j].FindPath('local').AsString;
          ip2:=StrToIp(buf);
          if ip2=ip1 then begin
            if ja.Items[j].FindPath('prefixlen')=nil then Continue;
            buf:=ja.Items[j].FindPath('prefixlen').AsString;
            pl:=strtoint(buf);
            k:=32-pl;
            result:=($ffffffff shr k) shl k;
          end;
        end;
      end;
    end;
    finally
      jl.Free;
    end;
  end
  else begin
    // try to use ifconfig
    AProcess:=TProcess.Create(nil);
    AProcess.Executable := '/sbin/ifconfig';
    AProcess.Parameters.Add('-a');
    AProcess.Options := AProcess.Options + [poUsePipes, poWaitOnExit];
    try
      try
      AProcess.Execute();
      processok:=(AProcess.ExitStatus=0);
      except
        processok:=false;
      end;
      sl.LoadFromStream(AProcess.Output);
    finally
      AProcess.Free();
    end;
    for i:=0 to sl.Count-1 do
    begin
      l:=10;
      n:=Pos('inet addr:', sl[i]);
      if n=0 then begin n:=Pos('inet ', sl[i]); l:=5; end;
      if n=0 then Continue;
      s:=sl[i];
      buf:=Copy(s, n+l, 999);
      n:=Pos(' ', buf);
      if n>0 then buf:=Copy(buf, 1, n);
      ip2:=StrToIp(buf);
      if ip2=ip1 then begin
        l:=8;
        n:=Pos('netmask ', s);
        if n=0 then begin n:=Pos('Mask:', s); l:=5; end;
        if n=0 then Continue;
        buf:=Copy(s, n+l, 999);
        n:=Pos(' ', buf);
        if n>0 then buf:=Copy(buf, 1, n);
        buf:=trim(buf);
        if copy(buf,1,2)='0x' then begin
          buf:=copy(buf,3,99);
          result:=StrToInt('$'+trim(buf));
        end
        else begin
          result:=StrToIp(buf);
        end;
        break;
      end;
    end;
  end;
  {$ENDIF}
  sl.Free();
end;

procedure TDiscoveryDaemon.SetIPaddr(value:string);
begin
  FIPaddr:=value;
  serverip:=StrToIp(FIPaddr);
  servermask:=GetMask(FIPaddr);
end;

procedure TDiscoveryDaemon.Execute;
var
  i: integer;
  remoteip, remoteport,req, reply: string;
  data: array[0..1024] of char;
  p: pointer;
  testip: dword;
begin
  //writetrace('start udp deamon');
  stoping := False;
  sock := TUDPBlockSocket.Create;
  //writetrace('blocksocked created');
  p:=@data;
  try
    with sock do
    begin
      //writetrace('create socket');
      CreateSocket;
      if lasterror <> 0 then
        Synchronize(@ShowError);
      MaxLineLength := 1024;
      {$ifdef linux}
      //writetrace('setlinger');
      setLinger(True, 0);
      {$endif}
      if lasterror <> 0 then
        Synchronize(@ShowError);
      EnableReuse(true);
      bind(cAnyHost, FIPport);
      if lasterror <> 0 then
        Synchronize(@ShowError);
      Synchronize(@ShowSocket);
      //writetrace('start main loop');
      repeat
        if stoping or terminated then
          break;
        if canread(500) and (not terminated) and (not stoping) then
        begin
          if lastError = 0 then
          begin
            FillByte(data,1024,0);
            i:=RecvBuffer(p,1024);
            if i>0 then begin
              req:=trim(data);
              if req=FDiscoveryStr then begin
                remoteip := GetRemoteSinIP;
                testip:=StrToIp(remoteip);
                if (serverip and servermask)=(testip and servermask) then begin
                  // reply only if client is on right subnet for this interface
                  remoteport := IntToStr(GetRemoteSinPort);
                  ReplySock.Connect(remoteip,remoteport);
                  if lastError = 0 then begin
                    reply:='{"AlpacaPort":'+FAlpacaPort+'}';
                    FillByte(data,1024,0);
                    data:=reply;
                    ReplySock.SendBuffer(p,length(reply));
                  end;
                end;
              end;
            end;
          end
          else
            Synchronize(@ShowError);
        end;
      until False;
    end;
  finally
    //  Suspended:=true;
    Sock.CloseSocket;
    Sock.Free;
    ReplySock.CloseSocket;
    ReplySock.Free;
    //  terminate;
  end;
end;


initialization

 {$ifdef darwin}//  ignore SIGPIPE
 {$ifdef CPU32}
  with NewSigRec do
  begin
    integer(Sa_Handler) := SIG_IGN; // ignore signal
    Sa_Mask[0] := 0;
    Sa_Flags := 0;
  end;
  res := fpsigaction(SIGPIPE, @NewSigRec, @OldSigRec);
 {$endif}
 {$endif}
end.
