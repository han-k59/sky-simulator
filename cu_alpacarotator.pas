unit cu_alpacarotator;

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

uses  cu_alpacadevice, Classes, SysUtils;

type

  T_Alpacarotator = class(T_AlpacaDevice)
    protected
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessSetup(req: string; out status: integer):string; override;
      function  GetSetupPage: string; virtual; abstract;

      function fabsolute: boolean; virtual; abstract;
      function ismoving: boolean; virtual; abstract;

      function  position: double; virtual; abstract;
      function  targetposition: double; virtual; abstract;
      function  MechanicalPosition: double; virtual; abstract;

      function  stepsize: double; virtual; abstract;
//      function  maxstep: integer; virtual; abstract;
      function  temperature: double; virtual; abstract;
      function  reverse: boolean; virtual; abstract;
      function  canreverse: boolean; virtual; abstract;
      procedure setReverse(ok: boolean); virtual; abstract;
      procedure rotator_move(i: integer); virtual; abstract;
      procedure rotator_moveabsolute(x: double); virtual; abstract;
      procedure rotator_movemechanical(x: double); virtual; abstract;
      procedure rotator_sync(x: double); virtual; abstract;

      procedure rot_halt; virtual; abstract;
  end;

implementation

constructor T_Alpacarotator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_Alpacarotator.Destroy;
begin
  inherited Destroy;
end;

function  T_Alpacarotator.ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string;
var method,value: string;
    ok    : boolean;
    lst   : TStringList;
    x     : double;
    params: TStringlist;
    i: integer;
    img: timg;

    ClientID,ClientTransactionID:Longword;
begin
  params:=TStringlist.Create;
  DecodeRequest(req,method,params,ClientID,ClientTransactionID);
  status:=200;
  FErrorNumber:=0;
  FErrorMessage:='';
  ok:=false; i:=0; {setlength(axr,0);} value:='';
  if method='connected' then begin
    ok:=Connected;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='description' then begin
    value:=Description;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='driverinfo' then begin
    value:=DriverInfo;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='driverversion' then begin
    value:=DriverVersion;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='interfaceversion' then begin
    i:=InterfaceVersion;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='name' then begin
    value:=Name;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='supportedactions' then begin
    lst:=SupportedActions;
    result:=FormatStringListResp(lst,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
    lst.Free;
  end
  else if method='absolute' then begin
    ok:=fabsolute;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='ismoving' then begin
    ok:=ismoving;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='position' then begin
    x:=position;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='targetposition' then begin
    x:=targetposition;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='mechanicalposition' then begin
    x:=mechanicalposition;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='stepsize' then begin
    x:=stepsize;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='temperature' then begin
    x:=temperature;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='reverse' then begin
    ok:=reverse;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canreverse' then begin
    ok:=canreverse;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else begin
    result:='GET - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function  T_Alpacarotator.ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string;
var method,p1,p2,value: string;
    ok,bvalue: boolean;
    x        : double;
    params   : TStringlist;
    i        : integer;
    ClientID,ClientTransactionID: LongWord;
begin
  if pos('?',req)>0 then
    req:=req+'&'+arg
  else
    req:=req+'?'+arg;
  params:=TStringlist.Create;
  DecodeRequest(req,method,params,ClientID,ClientTransactionID);
  status:=200;
  FErrorNumber:=0;
  FErrorMessage:='';
  bvalue:=false; value:='';
  if method='action' then begin
    if GetParamString(params,'Action',p1) and GetParamString(params,'Parameters',p2) then
      value:=Action(p1,p2);
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='commandblind' then begin
    if GetParamString(params,'Command',p1) and GetParamBool(params,'Raw',ok) then
      CommandBlind(p1,ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='commandbool' then begin
    if GetParamString(params,'Command',p1) and GetParamBool(params,'Raw',ok) then
      bvalue:=CommandBool(p1,ok);
    result:=FormatBoolResp(bvalue,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='commandstring' then begin
    if GetParamString(params,'Command',p1) and GetParamBool(params,'Raw',ok) then
      value:=CommandString(p1,ok);
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='connected' then begin
    if GetParamBool(params,'connected',ok) then
      SetConnected(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='halt' then begin
    rot_halt;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='reverse' then begin
    if GetParamBool(params,'reverse',ok) then
      setReverse(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='move' then begin
    if GetParamInt(params,'position',i) then {parameter is position, integer}
    rotator_move(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='moveabsolute' then begin
    if GetParamFloat(params,'position',x) then {parameter is Position, double}
    rotator_moveabsolute(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='movemechanical' then begin
    if GetParamFloat(params,'position',x) then {parameter is Position, double}
    rotator_movemechanical(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sync' then begin
    if GetParamFloat(params,'position',x) then {parameter is Position, double}
    rotator_sync(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else begin
    result:='PUT - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function  T_Alpacarotator.ProcessSetup(req: string; out status: integer):string;
var method: string;
    params: TStringlist;
begin
  params:=TStringlist.Create;
  DecodeSetupRequest(req,method,params);
  status:=200;
  FErrorNumber:=0;
  FErrorMessage:='';
  if method='setup' then begin
    result:=GetSetupPage;
  end
  else begin
    result:='GET - Unknown setup method: '+method;
    status:=400;
  end;
  params.Free;
end;

end.

