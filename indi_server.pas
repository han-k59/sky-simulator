unit indi_server;

{$mode objfpc}{$H+}

{
INDI protocol transport for Sky Simulator.

Copyright (C) 2026 Han Kleijn
https://sourceforge.net/projects/sky-simulator

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

--------------------------------------------------------------------------
This unit contains NO device logic.  It is the INDI counterpart of
cu_tcpserver.pas + cu_alpacaserver.pas:

   TIndiSplitter - cuts the endless XML stream into top level elements
   TIndiElement  - one parsed element with attributes and children
   TIndiDevice   - abstract base class, one descendant per device
   TIndiClient   - one thread per connected client
   TIndiDaemon   - listening thread
   T_IndiServer  - component owned by the main form

Device units (indi_mount.pas, later indi_camera.pas) register themselves
at startup:

   RegisterIndiDevice(T_Indi_Mount);

Every client connection creates its OWN instance of every registered
device class, so per client state - which properties were already sent,
which values were last pushed - can never get mixed up between clients.

We are a server, not a driver: we listen on 7624 ourselves instead of
being launched by indiserver.  Clients cannot tell the difference.
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, strutils, dateutils, math, blcksock, synsock, cu_alpacadevice;

const
  IndiDefaultPort = '7624';
  CRLF2           = #13#10;
  TraceMaxLen     = 2000;   {longer trace lines are truncated, see Execute}
//  VecSep = #1;   {marks the end of one vector in FOut, see EmitToBuffer}


var
  {When true every XML element in and out of the server is passed to
   onShowMsg, prefixed with <<< or >>>.  Set it from the form, e.g.
   indi_log_traffic:=form1.indi_log1.checked;  It is off by default: the
   trace costs a Synchronize per element and a completed exposure produces
   a very large outgoing element.}
  indi_log_traffic : boolean = false;

type
  TIndiPerm  = (ipRO, ipWO, ipRW);
  TIndiState = (isIdle, isOk, isBusy, isAlert);

const
  StateStr: array[TIndiState] of string = ('Idle','Ok','Busy','Alert');
  PermStr : array[TIndiPerm]  of string = ('ro','wo','rw');

type
{ ---------------------------------------------------------------------- }
{ Incremental stream splitter.                                            }
{ The INDI stream is NOT a well formed document: it is an endless         }
{ sequence of top level elements with no root.  A DOM parser would block  }
{ forever waiting for a root close tag, so we cut the stream into         }
{ complete top level elements here and parse them one at a time.          }
{ ---------------------------------------------------------------------- }
  TIndiSplitter = class
  private
    FBuf: string;
    FDepth: integer;
  public
    procedure Add(const data: string);
    function  NextElement(out elem: string): boolean;
    procedure Reset;
  end;

{ ---------------------------------------------------------------------- }
{ One parsed element.  INDI never nests deeper than two levels            }
{ (newXxxVector -> oneXxx), so this is a flat container.                  }
{ ---------------------------------------------------------------------- }
  TIndiElement = class
  private
    FChildren: TList;
  public
    Tag: string;
    Attr: TStringList;          {name=value}
    Content: string;
    constructor Create;
    destructor  Destroy; override;
    function  ChildCount: integer;
    function  Child(i: integer): TIndiElement;
    function  A(const n: string): string;              {attribute, '' if absent}
    function  ChildValue(const nm: string): string;    {content of child name=nm}
    function  ChildNum(const nm: string; def: double): double;
    function  HasChild(const nm: string): boolean;
    function  SwitchOn: string;      {name of the first child whose value is On}
    class function Parse(const s: string): TIndiElement;   {nil on error}
  end;

  TIndiEmitProc = procedure(const s: string) of object;

{ ---------------------------------------------------------------------- }
{ Abstract device.                                                        }
{ Descendants live in their own unit and only ever build XML through the  }
{ Def/Set helpers below; they never touch sockets or threads.             }
{                                                                         }
{ IMPORTANT: DefineAll, HandleElement and Poll are always called from the }
{ MAIN thread (through Synchronize), so they may touch form1 and the      }
{ simulation globals freely - the same rule as the Alpaca handlers.       }
{ ---------------------------------------------------------------------- }
  TIndiDevice = class
  private
    FEmit: TIndiEmitProc;
  protected
    procedure Emit(const s: string);
    {vector builders - kind is 'Switch', 'Number' or 'Text'}
    procedure StartDef(const kind, name, alabel, group: string;
                       st: TIndiState; perm: TIndiPerm; const extra: string='');
    procedure AddDef(const kind, name, alabel, value: string;
                     const extra: string='');
    procedure EndDef(const kind: string);
    procedure StartSet(const kind, name: string; st: TIndiState);
    procedure AddOne(const kind, name, value: string);
    procedure EndSet(const kind: string);
    procedure LogMessage(const txt: string);
    function  OnOff(b: boolean): string;
  public
    DeviceName: string;
    constructor Create; virtual;
    procedure SetEmitProc(p: TIndiEmitProc);
    {called when a client sends getProperties}
    procedure DefineAll; virtual; abstract;
    {called for every incoming element addressed to this device}
    procedure HandleElement(e: TIndiElement); virtual; abstract;
    {called a few times per second, push whatever changed}
    procedure Poll; virtual; abstract;
  end;

  TIndiDeviceClass = class of TIndiDevice;

{ ---------------------------------------------------------------------- }
  TIndiClient = class(TThread)
  private
    FSock: TTCPBlockSocket;
    CSock: TSocket;
    FConnList: TThreadList;
    FSplit: TIndiSplitter;
    FDevices: TList;            {one TIndiDevice per registered class}
    FOut: string;               {built in main thread, flushed in Execute}
    FElem: TIndiElement;        {element currently handled by Synchronize}
    FDefined: boolean;
    FMsg: string;
    FTerminate: TIntProc;
    FShowMsg: TStringProc;
    stoping: boolean;
    remoteip, remoteport: string;
    procedure EmitToBuffer(const s: string);
    procedure HandleElementSync;      {main thread}
    procedure PollDevicesSync;        {main thread}
    procedure CreateDevicesSync;      {main thread}
    procedure DoShowMsg;
  public
    constructor Create(hsock: TSocket; AConnList: TThreadList);
    destructor  Destroy; override;
    procedure Execute; override;
    property Terminated;
    property onTerminate: TIntProc read FTerminate write FTerminate;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
  end;

{ ---------------------------------------------------------------------- }
  TIndiDaemon = class(TThread)
  private
    Sock: TTCPBlockSocket;
    FIPaddr, FIPport: string;
    FConnList: TThreadList;
    FErr: string;
    FShowError: TStringProc;
    FShowMsg: TStringProc;
    FPortMsg: TStringProc;
    procedure ShowError;
    procedure ShowPort;
    procedure TerminateChildren;
  public
    stoping: boolean;
    constructor Create;
    destructor  Destroy; override;
    procedure Execute; override;
    property ConnList: TThreadList read FConnList;
    property IPaddr: string read FIPaddr write FIPaddr;
    property IPport: string read FIPport write FIPport;
    property onShowError: TStringProc read FShowError write FShowError;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
    property onPortMsg: TStringProc read FPortMsg write FPortMsg;
  end;

{ ---------------------------------------------------------------------- }
  T_IndiServer = class(TComponent)
  private
    FDaemon: TIndiDaemon;
    FIPaddr, FIPport: string;
    FShowError: TStringProc;
    FShowMsg: TStringProc;
    FPortMsg: TStringProc;
    function GetActiveConnections: integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure StartServer;
    procedure StopServer;
    property ActiveConnections: integer read GetActiveConnections;
    property IPaddr: string read FIPaddr write FIPaddr;
    property IPport: string read FIPport write FIPport;
    property onShowError: TStringProc read FShowError write FShowError;
    property onShowMsg: TStringProc read FShowMsg write FShowMsg;
    property onPortMsg: TStringProc read FPortMsg write FPortMsg;
  end;

procedure RegisterIndiDevice(cls: TIndiDeviceClass);
procedure ClearIndiDevices;

function IndiTimeStamp: string;
function XmlEsc(const s: string): string;
function XmlUnesc(const s: string): string;
function F(v: double; dec: integer=6): string;   {locale independent float}
function StrToF(const s: string; def: double): double;

implementation

var
  IndiDeviceClasses: TList = nil;

{ ====================================================================== }
{ helpers                                                                 }
{ ====================================================================== }

function F(v: double; dec: integer=6): string;
var fs: TFormatSettings;
begin
  fs:=DefaultFormatSettings;
  fs.DecimalSeparator:='.';          {INDI is always dot, never comma}
  Result:=FloatToStrF(v,ffFixed,15,dec,fs);
end;

function StrToF(const s: string; def: double): double;
var t: string;
begin
  t:=StringReplace(Trim(s),',','.',[rfReplaceAll]);
  Result:=StrToFloatDef(t,def);
end;

function XmlEsc(const s: string): string;
begin
  Result:=StringReplace(s,'&','&amp;',[rfReplaceAll]);
  Result:=StringReplace(Result,'<','&lt;',[rfReplaceAll]);
  Result:=StringReplace(Result,'>','&gt;',[rfReplaceAll]);
  Result:=StringReplace(Result,'"','&quot;',[rfReplaceAll]);
  Result:=StringReplace(Result,#39,'&apos;',[rfReplaceAll]);
end;

function XmlUnesc(const s: string): string;
begin
  Result:=StringReplace(s,'&lt;','<',[rfReplaceAll]);
  Result:=StringReplace(Result,'&gt;','>',[rfReplaceAll]);
  Result:=StringReplace(Result,'&quot;','"',[rfReplaceAll]);
  Result:=StringReplace(Result,'&apos;',#39,[rfReplaceAll]);
  Result:=StringReplace(Result,'&amp;','&',[rfReplaceAll]);
end;

function IndiTimeStamp: string;
var
  y,m,d,hh,mm,ss,ms: word;
  dt: TDateTime;
begin
  dt:=LocalTimeToUniversal(Now);
  DecodeDate(dt,y,m,d);
  DecodeTime(dt,hh,mm,ss,ms);
  Result:=Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2d',[y,m,d,hh,mm,ss]);
end;

procedure RegisterIndiDevice(cls: TIndiDeviceClass);
begin
  if IndiDeviceClasses=nil then IndiDeviceClasses:=TList.Create;
  if IndiDeviceClasses.IndexOf(Pointer(cls))<0 then
    IndiDeviceClasses.Add(Pointer(cls));
end;

procedure ClearIndiDevices;
begin
  if IndiDeviceClasses<>nil then IndiDeviceClasses.Clear;
end;

{ ====================================================================== }
{ TIndiSplitter                                                           }
{ ====================================================================== }

procedure TIndiSplitter.Reset;
begin
  FBuf:=''; FDepth:=0;
end;

procedure TIndiSplitter.Add(const data: string);
begin
  FBuf:=FBuf+data;
  if Length(FBuf)>8*1024*1024 then begin FBuf:=''; FDepth:=0; end;  {runaway}
end;

function TIndiSplitter.NextElement(out elem: string): boolean;
var
  i, start, p: integer;
  inTag, inQuote, selfClose: boolean;
  q: char;
begin
  Result:=false; elem:='';
  if FBuf='' then exit;
  {FDepth must be recomputed from scratch on every call: a client may
   deliver one element in many small TCP segments, and this routine
   rescans FBuf from the start each time it is called.  Keeping the
   depth across calls counts the same opening tags repeatedly, so it
   never returns to zero and no element is ever emitted.}
  FDepth:=0;

  i:=1; start:=0; inTag:=false; inQuote:=false; q:=#0; selfClose:=false;
  while i<=Length(FBuf) do
  begin
    if inQuote then
    begin
      if FBuf[i]=q then inQuote:=false;
      inc(i); continue;
    end;

    if not inTag then
    begin
      if FBuf[i]='<' then
      begin
        {skip comments and processing instructions}
        if (i+3<=Length(FBuf)) and (Copy(FBuf,i,4)='<!--') then
        begin
          p:=PosEx('-->',FBuf,i);
          if p=0 then exit;                     {incomplete, wait for more}
          if FDepth=0 then FBuf:=Copy(FBuf,p+3,MaxInt);
          i:=1; start:=0; continue;
        end;
        if (i+1<=Length(FBuf)) and (FBuf[i+1]='?') then
        begin
          p:=PosEx('?>',FBuf,i);
          if p=0 then exit;
          FBuf:=Copy(FBuf,p+2,MaxInt);
          i:=1; start:=0; continue;
        end;

        if (FDepth=0) and (start=0) then start:=i;
        inTag:=true; selfClose:=false;

        if (i+1<=Length(FBuf)) and (FBuf[i+1]='/') then   {closing tag}
        begin
          dec(FDepth);
          if FDepth<0 then FDepth:=0;
          inc(i);
          while (i<=Length(FBuf)) and (FBuf[i]<>'>') do inc(i);
          if i>Length(FBuf) then exit;          {incomplete}
          inTag:=false;
          if (FDepth=0) and (start>0) then
          begin
            elem:=Copy(FBuf,start,i-start+1);
            FBuf:=Copy(FBuf,i+1,MaxInt);
            Result:=true; exit;
          end;
          inc(i); continue;
        end;

        inc(FDepth);
        inc(i); continue;
      end;
      inc(i); continue;
    end;

    {inside an opening tag}
    case FBuf[i] of
      '"',#39 : begin inQuote:=true; q:=FBuf[i]; end;
      '/'     : if (i<Length(FBuf)) and (FBuf[i+1]='>') then selfClose:=true;
      '>'     : begin
                  inTag:=false;
                  if selfClose then
                  begin
                    dec(FDepth);
                    if FDepth<0 then FDepth:=0;
                    if (FDepth=0) and (start>0) then
                    begin
                      elem:=Copy(FBuf,start,i-start+1);
                      FBuf:=Copy(FBuf,i+1,MaxInt);
                      Result:=true; exit;
                    end;
                  end;
                end;
    end;
    inc(i);
  end;

  if (FDepth=0) and (start=0) then FBuf:='';   {drop leading rubbish}
end;

{ ====================================================================== }
{ TIndiElement                                                            }
{ ====================================================================== }

constructor TIndiElement.Create;
begin
  inherited Create;
  Attr:=TStringList.Create;
  Attr.NameValueSeparator:='=';
  FChildren:=TList.Create;
end;

destructor TIndiElement.Destroy;
var i: integer;
begin
  for i:=0 to FChildren.Count-1 do
    if FChildren[i]<>nil then TIndiElement(FChildren[i]).Free;
  FChildren.Free;
  Attr.Free;
  inherited Destroy;
end;

function TIndiElement.ChildCount: integer;
begin Result:=FChildren.Count; end;

function TIndiElement.Child(i: integer): TIndiElement;
begin Result:=TIndiElement(FChildren[i]); end;

function TIndiElement.A(const n: string): string;
begin Result:=Attr.Values[n]; end;

function TIndiElement.ChildValue(const nm: string): string;
var i: integer; c: TIndiElement;
begin
  Result:='';
  for i:=0 to FChildren.Count-1 do
  begin
    c:=Child(i);
    if (c<>nil) and (c.A('name')=nm) then begin Result:=c.Content; exit; end;
  end;
end;

function TIndiElement.HasChild(const nm: string): boolean;
var i: integer; c: TIndiElement;
begin
  Result:=false;
  for i:=0 to FChildren.Count-1 do
  begin
    c:=Child(i);
    if (c<>nil) and (c.A('name')=nm) then begin Result:=true; exit; end;
  end;
end;

function TIndiElement.ChildNum(const nm: string; def: double): double;
var i: integer; c: TIndiElement;
begin
  Result:=def;
  for i:=0 to FChildren.Count-1 do
  begin
    c:=Child(i);
    if (c<>nil) and (c.A('name')=nm) then
      begin Result:=StrToF(c.Content,def); exit; end;
  end;
end;

function TIndiElement.SwitchOn: string;
var i: integer; c: TIndiElement;
begin
  Result:='';
  for i:=0 to FChildren.Count-1 do
  begin
    c:=Child(i);
    if (c<>nil) and (UpperCase(Trim(c.Content))='ON') then
      begin Result:=c.A('name'); exit; end;
  end;
end;

{ Flat two level parser.  INDI never nests deeper than
    <newXxxVector ...> <oneXxx name="..">value</oneXxx> ... </newXxxVector>
  so no recursion is needed.  Every loop is guaranteed to advance pos_, and
  both loops carry a hard guard, so malformed input can never hang. }
class function TIndiElement.Parse(const s: string): TIndiElement;
var
  pos_: integer;
  root, kid: TIndiElement;
  sc, kidSc: boolean;
  guard: integer;

  procedure SkipWs;
  begin
    while (pos_<=Length(s)) and (s[pos_] in [' ',#9,#10,#13]) do inc(pos_);
  end;

  function ReadOpenTag(out selfClose: boolean): TIndiElement;
  var
    e: TIndiElement;
    n, v: string;
    q: char;
    g: integer;
  begin
    Result:=nil; selfClose:=false;
    SkipWs;
    if (pos_>Length(s)) or (s[pos_]<>'<') then exit;
    if (pos_<Length(s)) and (s[pos_+1]='/') then exit;      {a closing tag}
    inc(pos_);

    e:=TIndiElement.Create;
    try
      n:='';
      while (pos_<=Length(s)) and not (s[pos_] in [' ',#9,#10,#13,'>','/']) do
      begin n:=n+s[pos_]; inc(pos_); end;
      e.Tag:=n;

      g:=0;
      repeat
        inc(g);
        if g>256 then break;                {malformed tag, stop safely}
        SkipWs;
        if pos_>Length(s) then break;

        if s[pos_]='/' then
        begin
          selfClose:=true;
          inc(pos_);
          SkipWs;
          if (pos_<=Length(s)) and (s[pos_]='>') then inc(pos_);
          break;
        end;

        if s[pos_]='>' then begin inc(pos_); break; end;

        n:='';
        while (pos_<=Length(s)) and not (s[pos_] in ['=',' ',#9,#10,#13,'>','/']) do
        begin n:=n+s[pos_]; inc(pos_); end;

        SkipWs;
        v:='';
        if (pos_<=Length(s)) and (s[pos_]='=') then
        begin
          inc(pos_); SkipWs;
          if (pos_<=Length(s)) and ((s[pos_]='"') or (s[pos_]=#39)) then
          begin
            q:=s[pos_]; inc(pos_);
            while (pos_<=Length(s)) and (s[pos_]<>q) do
            begin v:=v+s[pos_]; inc(pos_); end;
            if pos_<=Length(s) then inc(pos_);          {closing quote}
          end
          else                                          {unquoted, tolerate}
            while (pos_<=Length(s)) and not (s[pos_] in [' ',#9,#10,#13,'>','/']) do
            begin v:=v+s[pos_]; inc(pos_); end;
        end;

        if n='' then inc(pos_)              {never stand still: forces progress}
                else e.Attr.Values[n]:=XmlUnesc(v);
      until false;

      Result:=e;
    except
      FreeAndNil(e);
      Result:=nil;
    end;
  end;

  function ReadText: string;
  var st: integer;
  begin
    st:=pos_;
    while (pos_<=Length(s)) and (s[pos_]<>'<') do inc(pos_);
    Result:=Trim(XmlUnesc(Copy(s,st,pos_-st)));
  end;

  procedure SkipCloseTag;
  begin
    SkipWs;
    if (pos_<Length(s)) and (s[pos_]='<') and (s[pos_+1]='/') then
    begin
      while (pos_<=Length(s)) and (s[pos_]<>'>') do inc(pos_);
      if pos_<=Length(s) then inc(pos_);
    end;
  end;

begin
  Result:=nil;
  pos_:=1;

  root:=ReadOpenTag(sc);
  if root=nil then exit;
  if sc then begin Result:=root; exit; end;      {e.g. <getProperties .../>}

  try
    root.Content:=ReadText;

    guard:=0;
    while pos_<=Length(s) do
    begin
      inc(guard);
      if guard>4096 then break;                  {hard stop, never hang}

      SkipWs;
      if pos_>Length(s) then break;

      if s[pos_]<>'<' then begin ReadText; continue; end;

      if (pos_<Length(s)) and (s[pos_+1]='/') then
      begin
        SkipCloseTag;                            {end of the root element}
        break;
      end;

      kid:=ReadOpenTag(kidSc);
      if kid=nil then break;
      root.FChildren.Add(kid);
      if not kidSc then
      begin
        kid.Content:=ReadText;
        SkipCloseTag;
      end;
    end;

    Result:=root;
  except
    FreeAndNil(root);
    Result:=nil;
  end;
end;

{ ====================================================================== }
{ TIndiDevice                                                             }
{ ====================================================================== }

constructor TIndiDevice.Create;
begin
  inherited Create;
  DeviceName:='Unnamed';
end;

procedure TIndiDevice.SetEmitProc(p: TIndiEmitProc);
begin
  FEmit:=p;
end;

procedure TIndiDevice.Emit(const s: string);
begin
  if Assigned(FEmit) then FEmit(s);
end;

function TIndiDevice.OnOff(b: boolean): string;
begin
  if b then Result:='On' else Result:='Off';
end;

procedure TIndiDevice.StartDef(const kind, name, alabel, group: string;
                               st: TIndiState; perm: TIndiPerm;
                               const extra: string='');
begin
  Emit('<def'+kind+'Vector device="'+XmlEsc(DeviceName)+'" name="'+name+
       '" label="'+XmlEsc(alabel)+'" group="'+XmlEsc(group)+
       '" state="'+StateStr[st]+'" perm="'+PermStr[perm]+'" timeout="60"'+
       extra+' timestamp="'+IndiTimeStamp+'">'+CRLF2);
end;

procedure TIndiDevice.AddDef(const kind, name, alabel, value: string;
                             const extra: string='');
begin
  Emit('  <def'+kind+' name="'+name+'" label="'+XmlEsc(alabel)+'"'+extra+'>'+
       XmlEsc(value)+'</def'+kind+'>'+CRLF2);
end;

procedure TIndiDevice.EndDef(const kind: string);
begin
  Emit('</def'+kind+'Vector>'+CRLF2);
end;

procedure TIndiDevice.StartSet(const kind, name: string; st: TIndiState);
begin
  Emit('<set'+kind+'Vector device="'+XmlEsc(DeviceName)+'" name="'+name+
       '" state="'+StateStr[st]+'" timestamp="'+IndiTimeStamp+'">'+CRLF2);
end;

procedure TIndiDevice.AddOne(const kind, name, value: string);
begin
  Emit('  <one'+kind+' name="'+name+'">'+XmlEsc(value)+'</one'+kind+'>'+CRLF2);
end;

procedure TIndiDevice.EndSet(const kind: string);
begin
  Emit('</set'+kind+'Vector>'+CRLF2);
end;

procedure TIndiDevice.LogMessage(const txt: string);
begin
  Emit('<message device="'+XmlEsc(DeviceName)+'" timestamp="'+IndiTimeStamp+
       '" message="'+XmlEsc(txt)+'"/>'+CRLF2);
end;

{ ====================================================================== }
{ TIndiClient                                                             }
{ ====================================================================== }

constructor TIndiClient.Create(hsock: TSocket; AConnList: TThreadList);
begin
  CSock:=hsock;
  FConnList:=AConnList;
  FSplit:=TIndiSplitter.Create;
  FDevices:=TList.Create;
  FreeOnTerminate:=true;
  FDefined:=false;
  inherited Create(false);
end;

destructor TIndiClient.Destroy;
var i: integer;
begin
  if FDevices<>nil then
  begin
    for i:=0 to FDevices.Count-1 do TIndiDevice(FDevices[i]).Free;
    FDevices.Free;
  end;
  FSplit.Free;
  if FSock<>nil then FSock.Free;
  inherited Destroy;
end;

procedure TIndiClient.EmitToBuffer(const s: string);
begin
  FOut:=FOut+s;
// Not used. Is for splitting each vector
//  if (Copy(s,1,5)='</def') or (Copy(s,1,5)='</set') then FOut:=FOut+VecSep;//send per vector. More like drivers
end;

procedure TIndiClient.DoShowMsg;
begin
  if Assigned(FShowMsg) then FShowMsg(FMsg);
end;

procedure TIndiClient.CreateDevicesSync;
var
  i: integer;
  d: TIndiDevice;
begin
  if IndiDeviceClasses=nil then exit;
  for i:=0 to IndiDeviceClasses.Count-1 do
  begin
    d:=TIndiDeviceClass(IndiDeviceClasses[i]).Create;
    d.SetEmitProc(@EmitToBuffer);
    FDevices.Add(d);
  end;
end;

procedure TIndiClient.HandleElementSync;
var
  i: integer;
  d: TIndiDevice;
  dev: string;
begin
  if FElem=nil then exit;
  dev:=FElem.A('device');

  {getProperties with no device attribute means "everything you have"}
  if FElem.Tag='getProperties' then
  begin
    for i:=0 to FDevices.Count-1 do
    begin
      d:=TIndiDevice(FDevices[i]);
      if (dev='') or (dev=d.DeviceName) then d.DefineAll;
    end;
    FDefined:=true;
    exit;
  end;

  for i:=0 to FDevices.Count-1 do
  begin
    d:=TIndiDevice(FDevices[i]);
    if (dev='') or (dev=d.DeviceName) then d.HandleElement(FElem);
  end;
end;

procedure TIndiClient.PollDevicesSync;
var i: integer;
begin
  if not FDefined then exit;
  for i:=0 to FDevices.Count-1 do
    TIndiDevice(FDevices[i]).Poll;
end;


procedure TIndiClient.Execute;
const
  RecvTimeout = 100;     {ms - short so Terminated is noticed quickly}
  PollEvery   = 3;       {poll the devices every 3 * RecvTimeout}
var
  buf, elem: string;
  tick: integer;
begin
  try
    FSock:=TTCPBlockSocket.Create;
    FSock.socket:=CSock;
    FSock.GetSins;
    FSock.SetLinger(true,1000);
    remoteip  :=FSock.GetRemoteSinIP;
    remoteport:=IntToStr(FSock.GetRemoteSinPort);
    stoping:=false;
    tick:=0;

    FConnList.Add(Self);
    Synchronize(@CreateDevicesSync);

    FMsg:='INDI client connected from '+remoteip;
    Synchronize(@DoShowMsg);


    repeat
      if stoping or Terminated then break;

      buf:=FSock.RecvPacket(RecvTimeout);
      if FSock.LastError=WSAETIMEDOUT then
        {nothing received, fall through to the poll}
      else if FSock.LastError<>0 then
        break                                   {client gone}
      else if buf<>'' then
      begin
        FSplit.Add(buf);
        while FSplit.NextElement(elem) do
        begin
          if indi_log_traffic then
          begin
            FMsg:='<<< '+elem;
            Synchronize(@DoShowMsg);

          end;

          FElem:=TIndiElement.Parse(elem);
          if FElem<>nil then
          try
            Synchronize(@HandleElementSync);
          finally
            FreeAndNil(FElem);
          end;
        end;
      end;

      inc(tick);
      if tick>=PollEvery then
      begin
        tick:=0;
        Synchronize(@PollDevicesSync);
      end;

      if FOut<>'' then
      begin
        if indi_log_traffic then //logging in simulator
        begin
          {a completed exposure puts a whole base64 image in FOut, so the
           trace is capped - the interesting part is always the header}
          if Length(FOut)>TraceMaxLen then
            FMsg:='>>> '+Copy(FOut,1,TraceMaxLen)+' ...['+
                  IntToStr(Length(FOut))+' bytes]'
          else
            FMsg:='>>> '+FOut;
          Synchronize(@DoShowMsg);
        end;

        FSock.SendString(FOut);
        FOut:='';
        if FSock.LastError<>0 then break;
      end;
    until false;

  except
    on E: Exception do
    begin
      FMsg:='INDI client error: '+E.Message;
      try Synchronize(@DoShowMsg); except end;
    end;
  end;

  try
    FConnList.Remove(Self);
    FMsg:='INDI client disconnected '+remoteip;
    Synchronize(@DoShowMsg);
    if FSock<>nil then FSock.CloseSocket;
  except
  end;
end;


//next routine is not used. It sends each vector seperately
{procedure TIndiClient.Execute;
const
  RecvTimeout = 100;     {ms - short so Terminated is noticed quickly}
  PollEvery   = 3;       {poll the devices every 3 * RecvTimeout}
var
  buf, elem, part: string;
  tick, j: integer;
begin
  try
    FSock:=TTCPBlockSocket.Create;
    FSock.socket:=CSock;
    FSock.GetSins;
    FSock.SetLinger(true,1000);
    remoteip  :=FSock.GetRemoteSinIP;
    remoteport:=IntToStr(FSock.GetRemoteSinPort);
    stoping:=false;
    tick:=0;

    FConnList.Add(Self);
    Synchronize(@CreateDevicesSync);

    FMsg:='INDI client connected from '+remoteip;
    Synchronize(@DoShowMsg);

    repeat
      if stoping or Terminated then break;

      buf:=FSock.RecvPacket(RecvTimeout);
      if FSock.LastError=WSAETIMEDOUT then
        {nothing received, fall through to the poll}
      else if FSock.LastError<>0 then
        break                                   {client gone}
      else if buf<>'' then
      begin
        FSplit.Add(buf);
        while FSplit.NextElement(elem) do
        begin
          if indi_log_traffic then
          begin
            FMsg:='<<< '+elem;
            Synchronize(@DoShowMsg);
          end;

          FElem:=TIndiElement.Parse(elem);
          if FElem<>nil then
          try
            Synchronize(@HandleElementSync);
          finally
            FreeAndNil(FElem);
          end;
        end;
      end;

      inc(tick);
      if tick>=PollEvery then
      begin
        tick:=0;
        Synchronize(@PollDevicesSync);
      end;

      if FOut<>'' then
      begin
        {one vector per send, see EmitToBuffer}
        while FOut<>'' do
        begin
          j:=Pos(VecSep,FOut);//VecSep=#1 is used as a marker to send each vector seperate. Not required. But more like other drivers
          if j>0 then
          begin
            part:=Copy(FOut,1,j-1);
            Delete(FOut,1,j);           {drop the separator too}
          end
          else
          begin
            part:=FOut;                 {tail without a marker, e.g. a message}
            FOut:='';
          end;
          if part='' then continue;

          if indi_log_traffic then
          begin
            {a completed exposure is a whole base64 image, so the trace is
             capped - the interesting part is always the header}
            if Length(part)>TraceMaxLen then
              FMsg:='>>> '+Copy(part,1,TraceMaxLen)+' ...['+
                    IntToStr(Length(part))+' bytes]'
            else
              FMsg:='>>> '+part;
            Synchronize(@DoShowMsg);
          end;

          FSock.SendString(part);
          if FSock.LastError<>0 then break;
        end;
        if FSock.LastError<>0 then break;
      end;
    until false;

  except
    on E: Exception do
    begin
      FMsg:='INDI client error: '+E.Message;
      try Synchronize(@DoShowMsg); except end;
    end;
  end;

  try
    FConnList.Remove(Self);
    FMsg:='INDI client disconnected '+remoteip;
    Synchronize(@DoShowMsg);
    if FSock<>nil then FSock.CloseSocket;
  except
  end;
end;
}

{ ====================================================================== }
{ TIndiDaemon                                                             }
{ ====================================================================== }

constructor TIndiDaemon.Create;
begin
  FConnList:=TThreadList.Create;
  FIPaddr:='0.0.0.0';
  FIPport:=IndiDefaultPort;
  FreeOnTerminate:=false;
  inherited Create(true);
end;

destructor TIndiDaemon.Destroy;
begin
  FConnList.Free;
  inherited Destroy;
end;

procedure TIndiDaemon.ShowError;
begin
  if Assigned(FShowError) then FShowError(FErr);
end;

procedure TIndiDaemon.ShowPort;
begin
  if Assigned(FPortMsg) then FPortMsg(FIPport);
end;

procedure TIndiDaemon.TerminateChildren;
var
  l: TList;
  i: integer;
begin
  l:=FConnList.LockList;
  try
    for i:=l.Count-1 downto 0 do
      TIndiClient(l[i]).stoping:=true;
  finally
    FConnList.UnlockList;
  end;
end;

procedure TIndiDaemon.Execute;
var
  ClientSock: TSocket;
  c: TIndiClient;
begin
  stoping:=false;
  Sock:=TTCPBlockSocket.Create;
  try
    Sock.CreateSocket;
    Sock.SetLinger(true,10000);
    Sock.Bind(FIPaddr,FIPport);
    if Sock.LastError<>0 then
    begin
      FErr:='INDI server cannot bind port '+FIPport+': '+Sock.LastErrorDesc;
      Synchronize(@ShowError);
      exit;
    end;
    Sock.Listen;
    if Sock.LastError<>0 then
    begin
      FErr:='INDI server listen failed: '+Sock.LastErrorDesc;
      Synchronize(@ShowError);
      exit;
    end;
    Synchronize(@ShowPort);

    repeat
      if stoping or Terminated then break;
      if Sock.CanRead(500) then
      begin
        ClientSock:=Sock.Accept;
        if Sock.LastError=0 then
        begin
          c:=TIndiClient.Create(ClientSock,FConnList);
          c.onShowMsg:=FShowMsg;
        end;
      end;
    until false;
  finally
    TerminateChildren;
    Sock.CloseSocket;
    Sock.Free;
  end;
end;

{ ====================================================================== }
{ T_IndiServer                                                            }
{ ====================================================================== }

constructor T_IndiServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDaemon:=nil;
  FIPaddr:='0.0.0.0';
  FIPport:=IndiDefaultPort;
end;

destructor T_IndiServer.Destroy;
begin
  StopServer;
  inherited Destroy;
end;

function T_IndiServer.GetActiveConnections: integer;
var l: TList;
begin
  Result:=0;
  if FDaemon=nil then exit;
  l:=FDaemon.ConnList.LockList;
  try
    Result:=l.Count;
  finally
    FDaemon.ConnList.UnlockList;
  end;
end;

procedure T_IndiServer.StartServer;
begin
  if FDaemon<>nil then exit;
  FDaemon:=TIndiDaemon.Create;
  FDaemon.IPaddr:=FIPaddr;
  FDaemon.IPport:=FIPport;
  FDaemon.onShowError:=FShowError;
  FDaemon.onShowMsg:=FShowMsg;
  FDaemon.onPortMsg:=FPortMsg;
  FDaemon.Start;
end;

procedure T_IndiServer.StopServer;
begin
  if FDaemon=nil then exit;
  FDaemon.stoping:=true;
  FDaemon.Terminate;
  FDaemon.WaitFor;
  FreeAndNil(FDaemon);
end;

finalization
  if IndiDeviceClasses<>nil then FreeAndNil(IndiDeviceClasses);

end.
