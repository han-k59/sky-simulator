unit cu_alpacadevice;

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

uses  ctypes,
  Classes, SysUtils;

type

  TAlpacaDeviceType=(switch,safetymonitor,dome,camera,observingconditions,filterwheel,focuser,rotator,telescope);

  TAxisRate = class(TObject)
    public
      maximum,minimum: double;
  end;
  TAxisRates = array of TAxisRate;
  TTrackingRates = array of integer;
  Timg           = array of array of word;

  // ImageBytes support, from Alpaca documentation
  TArrayUInt16 = array of cuint16;
  TImageBytesInfo = record
    MetadataVersion: cint;         // Bytes 0..3 - Metadata version = 1
    ErrorNumber: cint;             // Bytes 4..7 - Alpaca error number or zero for success
    ClientTransactionID: cuint;    // Bytes 8..11 - Client's transaction ID
    ServerTransactionID: cuint;    // Bytes 12..15 - Device's transaction ID
    DataStart: cint;               // Bytes 16..19 - Offset of the start of the data bytes = 36 for version 1
    ImageElementType: cint;        // Bytes 20..23 - Element type of the source image array
    TransmissionElementType: cint; // Bytes 24..27 - Element type as sent over the network
    Rank: cint;                    // Bytes 28..31 - Image array rank
    Dimension1: cint;              // Bytes 32..35 - Length of image array first dimension
    Dimension2: cint;              // Bytes 36..39 - Length of image array second dimension
    Dimension3: cint;              // Bytes 40..43 - Length of image array third dimension (0 for 2D array)
  end;

  TStringProc = procedure(var S: string) of object;
  TIntProc = procedure(var i: integer) of object;
  TGetCmd = function(cmd: string): string of object;
  TGetImageBytes = function(cmd: string; var s: TMemoryStream):string  of object;
  TPutCmd = function(cmd,arg: string): string of object;


const
  ApiVersion='v1';
  AlpacaDeviceName: array[0..ord(high(TAlpacaDeviceType))] of string =
    ('switch','safetymonitor','dome','camera','observingconditions','filterwheel','focuser','rotator','telescope');
  NullCoord=-9999;
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  ERR_NOT_IMPLEMENTED = $400;{1024}
  ERR_INVALID_VALUE = $401; {1025}
  ERR_VALUE_NOT_SET = $402;
  ERR_NOT_CONNECTED = $407;
  ERR_INVALID_WHILE_PARKED = $408;
  ERR_INVALID_WHILE_SLAVED = $409;
  ERR_INVALID_OPERATION = $40B;
  ERR_ACTION_NOT_IMPLEMENTED = $40C;
  ERR_DRIVER_ERROR = $500;
  MSG_NOT_IMPLEMENTED = 'Property or method not implemented';
  MSG_INVALID_VALUE = 'Invalid value';
  MSG_VALUE_NOT_SET = 'Value not set';
  MSG_NOT_CONNECTED = 'Not connected';
  MSG_DRIVER_ERROR = 'Driver error';

  MSG_OUT_OF_RANGE = 'Set value outside range. Will be clamped within range';

type
  T_AlpacaDevice = class(TComponent)
    protected
      FConnected: boolean;
      FErrorNumber: integer;
      FErrorMessage: String;
      FPath,FSetupPath: string;

    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  FormatEmptyResp(ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatRawResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatStringResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatStringListResp(value:TStringList; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatBoolResp(value:boolean; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatIntResp(value:integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatIntArrayResp(value:array of integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;

      function  FormatIntArrayofArrayResp(value:Timg; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatImageBytesResp(value:Timg; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string; var s:TMemoryStream):string;

      function  FormatFloatResp(value:double; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  FormatAxisRateResp(value:TAxisRates; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
      function  DecodeRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
      function  DecodeSetupRequest(req: string; out method: string; var params: TStringlist):boolean;
      function  GetParamString(params: Tstringlist; key: string; out value: string):boolean;
      function  GetParamBool(params: Tstringlist; key: string; out value: boolean):boolean;
      function  GetParamFloat(params: Tstringlist; key: string; out value: double):boolean;
      function  GetParamInt(params: Tstringlist; key: string; out value: integer):boolean;
      function  ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string; virtual; abstract;
      function  ProcessGetImageBytesRequest(req: string; ServerTransactionID:LongWord; out status: integer; var s:TMemoryStream):string; virtual;
      function  ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string; virtual; abstract;
      function  ProcessSetup(req: string; out status: integer):string; virtual; abstract;
      function  GetGuid: string; virtual; abstract;

      // Common properties and methods.
      function  Action( actionName, actionParameters: string):string; virtual; abstract;
      procedure CommandBlind( command: string;  raw: boolean = false); virtual; abstract;
      function  CommandBool(command: string;  raw: boolean = false):boolean; virtual; abstract;
      function  CommandString(command: string;  raw: boolean = false):string; virtual; abstract;
      function  Connected:boolean; virtual; abstract;
      procedure SetConnected(value:boolean); virtual; abstract;
      function  Description:string; virtual; abstract;
      function  DriverInfo:string; virtual; abstract;
      function  DriverVersion:string; virtual; abstract;
      function  InterfaceVersion: integer; virtual; abstract;
      function  Name:string; virtual; abstract;
      function  SupportedActions:TStringList; virtual; abstract;

      property  Path: string read FPath write FPath;
      property  SetupPath: string read FSetupPath write FSetupPath;
  end;

procedure SplitRec(buf,sep:string; var arg: TStringList);


implementation

//uses sky_simulator_main;

constructor T_AlpacaDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnected:=False;
end;

destructor  T_AlpacaDevice.Destroy;
begin
  inherited Destroy;
end;

function T_AlpacaDevice.DecodeRequest(req: string; out method: string; var params: TStringlist; out ClientID,ClientTransactionID: Longword):boolean;
var i,p: integer;
    buf:string;
begin
  result:=false;
  buf:=copy(req,1,length(FPath));
  if copy(req,1,length(FPath))<>FPath then exit;
  Delete(req,1,length(FPath));
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

function T_AlpacaDevice.DecodeSetupRequest(req: string; out method: string; var params: TStringlist):boolean;
var p: integer;
begin
result:=false;
if copy(req,1,length(FSetupPath))<>FSetupPath then exit;
Delete(req,1,length(FSetupPath));
p:=pos('?',req);
if p<=0 then begin
  method:=req;
  params.Clear;
end
else begin
  method:=copy(req,1,p-1);
  delete(req,1,p);
  params.Clear;
  SplitRec(req,'&',params);
end;
result:=true;
end;

function  T_AlpacaDevice.GetParamString(params: Tstringlist; key: string; out value: string):boolean;
var i: integer;
    buf,dummy: string;
begin
result:=false;
buf:=UpperCase(key)+'=';
for i:=0 to params.Count-1 do begin
  if uppercase(copy(params[i],1,length(buf)))=buf then begin
     value:=params[i];
     Delete(value,1,length(buf));
     result:=true;
     break;
  end;
end;
if not result then begin
  FErrorMessage:=MSG_VALUE_NOT_SET+' '+key;
  FErrorNumber:=ERR_VALUE_NOT_SET;
end;
end;

function  T_AlpacaDevice.GetParamBool(params: Tstringlist; key: string; out value: boolean):boolean;
var buf: string;
begin
//dummy:=params.text;
//Form1.memo1.lines.add('parameter: '+dummy);
result:=GetParamString(params,key,buf);
if result then begin
   buf:=UpperCase(buf);
   if buf='TRUE' then
     value:=true
   else if buf='FALSE' then
     value:=false
   else begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.GetParamFloat(params: Tstringlist; key: string; out value: double):boolean;
var buf: string;
begin
result:=GetParamString(params,key,buf);
if result then begin
   value:=StrToFloatDef(buf,NullCoord);
   if value=NullCoord then begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.GetParamInt(params: Tstringlist; key: string; out value: integer):boolean;
var buf: string;
    n: integer;
begin
result:=GetParamString(params,key,buf);
if result then begin
   val(buf,value,n);
   if n<>0 then begin
     FErrorMessage:=MSG_INVALID_VALUE+' '+key;
     FErrorNumber:=ERR_INVALID_VALUE;
     result:=false;
   end;
end;
end;

function  T_AlpacaDevice.FormatEmptyResp(ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatStringResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +'"'+value+'",'
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatRawResp(value:string; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +value+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatStringListResp(value:TStringList; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    i: integer;
begin
  if value.Count=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to value.Count-1 do begin
      buf:=buf+'"'+value[i]+'",';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatBoolResp(value:boolean; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +BoolToStr(value,'true','false')+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatIntResp(value:integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +IntToStr(value)+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatIntArrayResp(value:array of integer; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    n,i: integer;
begin
  n:=length(value);
  if n=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to n-1 do begin
        buf:=buf+IntToStr(value[i])+',';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;


function  T_AlpacaDevice.FormatIntArrayofArrayResp(value:timg; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    w,h,i,j: integer;
begin
  w:=length(value);
  h:=length(value[0]);
  if w=0 then
    buf:='[]'
  else begin
    buf:='[';  {begin mark image}
    for i:=0 to w-1 do
    begin
      buf:=buf+'['; {begin mark each row}

      for j:=0 to h-1 do
//      for j:=h-1 downto 0 do
      begin
        buf:=buf+IntToStr(value[i,j]);
        if j<h-1 then buf:=buf+',';
      end;
      buf:=buf+']'; {end mark each row}
      if i<w-1 then buf:=buf+',';
    end;
    buf:=buf+']'; {end mark image}
  end;
  result:='{"Type":2,' {int32}    {0 = Unknown, 1 = Short(int16), 2 = Integer (int32), 3 = Double (Double precision real number).}
         +'"Rank":2,'  {single plane monochrome} { The array's rank, will be 2 (single plane image (monochrome)) or 3 (multi-plane image)}
         +'"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;


function  T_AlpacaDevice.FormatImageBytesResp(value:Timg; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string; var s:TMemoryStream):string;
var metadata: TImageBytesInfo;
    data: TArrayUInt16;
    i,j,n: integer;
begin
  // Fill metadata
  metadata.MetadataVersion:=1;
  metadata.ErrorNumber:=0;
  metadata.ClientTransactionID:=ClientTransactionID;
  metadata.ServerTransactionID:=ServerTransactionID;
  metadata.DataStart:=sizeof(metadata);
  metadata.ImageElementType:=2;
  metadata.TransmissionElementType:=8;  {Alpaca standard 2021-12-7:  Int16 = 1, Int32 = 2, Double = 3, Single = 4, Decimal = 5, Byte = 6, Int64 = 7, UInt16 = 8, UInt32 = 9  }
  metadata.Rank:=2;
  metadata.Dimension1:=length(value);  //width !!!
  metadata.Dimension2:=length(value[0]); //height !!!
  metadata.Dimension3:=0;
  // write metadata to stream
  s.Position:=0;
  s.Write(metadata,sizeof(metadata));
  SetLength(data,metadata.Dimension1*metadata.Dimension2);
  // write image to sequential array
  n:=0;
  for i:=0 to metadata.Dimension1-1 do //cycle in width.
    //for j:=0 to metadata.Dimension2-1 do  //cycle in heighth Swapped :(
    for j:=metadata.Dimension2-1 downto 0 do  //cycle in heighth Swapped :(
    begin
      data[n]:=value[i,j];
      inc(n);
    end;
  // write image to stream
  s.Write(data[0],metadata.Dimension1*metadata.Dimension2*sizeof(cuint16));
  s.Position:=0;
  // empty result text
  result:='';
end;

function  T_AlpacaDevice.FormatFloatResp(value:double; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
begin
  result:='{"Value":'
         +FormatFloat('0.0000000000',value)+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

function  T_AlpacaDevice.FormatAxisRateResp(value:TAxisRates; ClientTransactionID, ServerTransactionID: LongWord; ErrorNumber: integer; ErrorMessage:string):string;
var buf: string;
    n,i: integer;
begin
  n:=length(value);
  if n=0 then
    buf:='[]'
  else begin
    buf:='[';
    for i:=0 to n-1 do begin
        buf:=buf+'{"Maximum":'+FormatFloat('0.0000000000',value[i].maximum)+',';
        buf:=buf+'"Minimum":'+FormatFloat('0.0000000000',value[i].minimum)+'},';
    end;
    SetLength(buf,Length(buf)-1);
    buf:=buf+']';
  end;
  result:='{"Value":'
         +buf+','
         +'"ClientTransactionID":'+inttostr(ClientTransactionID)+','
         +'"ServerTransactionID":'+inttostr(ServerTransactionID)+','
         +'"ErrorNumber":'+inttostr(ErrorNumber)+','
         +'"ErrorMessage":"'+ErrorMessage+'"}'
end;

procedure SplitRec(buf,sep:string; var arg: TStringList);
var i,l:integer;
begin
arg.clear;
l:=length(sep);
while pos(sep,buf)<>0 do begin
 for i:=1 to length(buf) do begin
  if copy(buf,i,l) = sep then begin
      arg.add(copy(buf,1,i-1));
      delete(buf,1,i-1+l);
      break;
  end;
 end;
end;
arg.add(buf);
end;

function T_AlpacaDevice.ProcessGetImageBytesRequest(req: string; ServerTransactionID:LongWord; out status: integer; var s:TMemoryStream):string;
begin
 // default for devices other than camera
 result:='GET - ImageBytes not implemented';
 status:=400;
end;

end.

