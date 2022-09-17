unit cu_alpacacamera;

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

  T_AlpacaCamera = class(T_AlpacaDevice)
    protected
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessGetImageBytesRequest(req: string; ServerTransactionID:LongWord; out status: integer; var s:TMemoryStream):string; override;
      function  ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessSetup(req: string; out status: integer):string; override;
      function  GetSetupPage: string; virtual; abstract;
      function  cameraXsize: integer; virtual; abstract;
      function  cameraYsize: integer; virtual; abstract;
      function  maxbinx: integer; virtual; abstract;
      function  maxbiny: integer; virtual; abstract;
      function  binx: integer; virtual; abstract;
      function  biny: integer; virtual; abstract;
      function  pixelsizex: integer; virtual; abstract;
      function  pixelsizey: integer; virtual; abstract;
      function  sensorname: string; virtual; abstract;
      function  maxadu: integer; virtual; abstract;
      function  camerastate: integer; virtual; abstract;
      function  startx: integer; virtual; abstract;
      function  starty: integer; virtual; abstract;
      function  numx: integer; virtual; abstract;
      function  numy: integer; virtual; abstract;
      function  coolpower: integer; virtual; abstract;
      function  setccdtemperature: integer; virtual; abstract;
      function  sensortype: integer; virtual; abstract;

      procedure setstartx(x: integer); virtual; abstract;
      procedure setstarty(x: integer); virtual; abstract;
      procedure setnumx(x: integer); virtual; abstract;
      procedure setnumy(x: integer); virtual; abstract;
      procedure setcooler(x: boolean); virtual; abstract;
      procedure fastreadout(x: boolean); virtual; abstract;
      procedure setbinX(x: integer; out ok :boolean); virtual; abstract;
      procedure setbinY(x: integer; out ok :boolean); virtual; abstract;

      function  cangetcoolerpower: boolean; virtual; abstract;
      function  cansetccdtemperature: boolean; virtual; abstract;
      function  canpulseguide: boolean; virtual; abstract;
      function  canabortexposure: boolean; virtual; abstract;
      function  canstopexposure: boolean; virtual; abstract;
      function  canasymmetricbin: boolean; virtual; abstract;
      function  cooleron: boolean; virtual; abstract;
      function  ispulseguiding: boolean; virtual; abstract;
      function  hasshutter: boolean; virtual; abstract;
      function  imageready: boolean; virtual; abstract;
      function  fastreadout: boolean; virtual; abstract;{get}

      function  ccdtemperature: double; virtual; abstract;
      function  exposuremax: double; virtual; abstract;
      function  exposuremin: double; virtual; abstract;
      function  fullwellcapacity: double; virtual; abstract;
      function  electronsperadu: double; virtual; abstract;
      function  gain: integer; virtual; abstract;
      function  gainmax: integer; virtual; abstract;
      function  gainmin: integer; virtual; abstract;
      function  coolerpower: double; virtual; abstract;
      function  heatSinkTemperature: double; virtual; abstract;
      function  exposureresolution: double; virtual; abstract; {get}
      function  lastexposureduration: double; virtual; abstract;
      function  percentcompleted: integer; virtual; abstract;


      procedure abortexposure(out exception:boolean); virtual; abstract;
      procedure stopexposure(out exception:boolean); virtual; abstract;
      procedure startexposure(duration: double; out errortype : integer); virtual; abstract;
      procedure setCcdTemperature(duration: double ;out error:double); virtual; abstract;
      procedure setGain(x: integer; out error :integer); virtual; abstract;
      function  imagearray(out ok: boolean) : Timg; virtual; abstract;

  end;

implementation

constructor T_AlpacaCamera.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_AlpacaCamera.Destroy;
begin
  inherited Destroy;
end;

function  T_AlpacaCamera.ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string;
var method,value: string;
    ok: boolean;
    lst:TStringList;
    x,ra,dec: double;
    params: TStringlist;
    i: integer;
    img: timg;
    ClientID,ClientTransactionID:Longword;

    procedure set_not_implemented;
    begin
      FErrorNumber:=ERR_NOT_IMPLEMENTED;
      FErrorMessage:=MSG_NOT_IMPLEMENTED;
    end;

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
  else if ((method='gains') or (method='offsets')) then begin
    lst:=TStringList.Create;
    lst.Clear;
    set_not_implemented;
    result:=FormatStringListResp(lst,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
    lst.Free;
  end
  else if method='readoutmodes' then begin
    lst:=TStringList.Create;
    lst.Clear;
    lst.add('normal');
    result:=FormatStringListResp(lst,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
    lst.Free;
  end
  else if method='readoutmodes' then begin
    lst:=TStringList.Create;
    lst.Clear;
    lst.add('normal');
    result:=FormatStringListResp(lst,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
    lst.Free;
  end
  else if ((method='readoutmode') ) then begin
    i:=0;{index to readoutmodes=normal}
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if ((method='offsetmin') or (method='offsetmax') or (method='offset') or (method='subexposureduration')) then begin {get integers not implemented}
    set_not_implemented;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cameraxsize' then begin
    i:=cameraXsize;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cameraysize' then begin
    i:=cameraYsize;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end

  else if method='maxbinx' then begin
    i:=maxbinX;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='maxbiny' then begin
    i:=maxbinY;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='binx' then begin
    i:=binX;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='biny' then begin
    i:=binY;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='pixelsizex' then begin
    i:=pixelsizex;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='pixelsizey' then begin
    i:=pixelsizeY;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sensorname' then begin
    value:=sensorname;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sensorname' then begin
    value:=sensorname;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='lastexposurestarttime' then begin {get string not implemented}
    set_not_implemented;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='bayeroffsetx' then begin
    set_not_implemented;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='bayeroffsety' then begin
    set_not_implemented;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='maxadu' then begin
    i:=maxadu;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='camerastate' then begin
    i:=camerastate;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='startx' then begin
    i:=startx;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='starty' then begin
    i:=starty;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='numx' then begin
    i:=numx;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='numy' then begin
    i:=numy;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='setccdtemperature' then begin
    i:=setccdtemperature;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sensortype' then begin
    i:=sensortype;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cangetcoolerpower' then begin
    ok:=cangetcoolerpower;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetccdtemperature' then begin
    ok:=cansetccdtemperature;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canfastreadout' then begin
    ok:=false;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cooleron' then begin
    ok:=cooleron;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='ispulseguiding' then begin
    ok:=ispulseguiding;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end

  else if method='hasshutter' then begin
    ok:=hasshutter;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='imageready' then begin
    ok:=imageready;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='fastreadout' then begin
    set_not_implemented;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
    else if method='ccdtemperature' then begin
    x:=ccdtemperature;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='exposuremax' then begin
    x:=exposuremax;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='exposuremin' then begin
    x:=exposuremin;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='fullwellcapacity' then begin
    x:=fullwellcapacity;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='electronsperadu' then begin
    x:=electronsperadu;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='gain' then begin
    i:=gain;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='gainmax' then begin
    i:=gainmax;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='gainmin' then begin
    i:=gainmin;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='coolerpower' then begin
    x:=coolerpower;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='heatsinktemperature' then begin
    x:=heatsinktemperature;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='exposureresolution' then begin
    x:=exposureresolution;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='lastexposureduration' then begin
    x:=lastexposureduration;
    if x<0 then begin FErrorNumber:=ERR_INVALID_VALUE; FErrorMessage:='No exposure was taken';  end;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='percentcompleted' then begin
    x:=percentcompleted;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='imagearray' then begin
    img:=imagearray(ok);
    if ok=false then begin FErrorNumber:=ERR_INVALID_VALUE; FErrorMessage:='Image is not ready since no startexposure was given';  end;
    result:= FormatIntArrayofArrayResp(img,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='imagearrayvariant' then begin {process the same as ethod='imagearray'}
    img:=imagearray(ok);
    if ok=false then begin FErrorNumber:=ERR_INVALID_VALUE; FErrorMessage:='Image is not ready since no startexposure was given';  end;
    result:= FormatIntArrayofArrayResp(img,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canpulseguide' then begin
    ok:=canpulseguide;
    result:= FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canabortexposure' then begin
    ok:=canabortexposure;
    result:= FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canstopexposure' then begin
    ok:=canstopexposure;
    result:= FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canasymmetricbin' then begin
    ok:=canasymmetricbin;
    result:= FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else begin
    result:='GET - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function T_AlpacaCamera.ProcessGetImageBytesRequest(req: string; ServerTransactionID:LongWord; out status: integer; var s:TMemoryStream):string;
var method   : string;
    ok: boolean;
    lst:TStringList;
    params: TStringlist;
    img: timg;
    ClientID,ClientTransactionID:Longword;
begin
  params:=TStringlist.Create;
  DecodeRequest(req,method,params,ClientID,ClientTransactionID);
  status:=200;
  FErrorNumber:=0;
  FErrorMessage:='';
  // Only valid method is imagearray
  if method='imagearray' then begin
    img:=imagearray(ok);
    if ok=false then begin FErrorNumber:=ERR_INVALID_VALUE; FErrorMessage:='Image is not ready since no startexposure was given';  end;
    result:= FormatImageBytesResp(img,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage,s);
  end
  else begin
    result:='GET - Method invalid for ImageBytes: '+method;
    status:=400;
  end;
  params.Free;
end;


function  T_AlpacaCamera.ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string;
var method,p1,p2,value: string;
    ok,bvalue: boolean;
    x,y: double;
    params: TStringlist;
    i,error: integer;
    error_float : double;
    ClientID,ClientTransactionID: LongWord;

    procedure set_invalid_range(i: double);
    begin
      fErrorNumber:=ERR_INVALID_VALUE; {1025}
      FErrorMessage:=MSG_OUT_OF_RANGE+' Set value: '+floattostrF(i,ffGeneral,5,2);
    end;

    procedure set_not_implemented;
    begin
      FErrorNumber:=ERR_NOT_IMPLEMENTED;
      FErrorMessage:=MSG_NOT_IMPLEMENTED;
    end;
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
    if GetParamBool(params,'Connected',ok) then
      SetConnected(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='abortexposure' then begin
    abortexposure(ok);
    if ok=false then   set_not_implemented;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='stopexposure' then begin
    stopexposure(ok);
    if ok=false then   set_not_implemented;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='startx' then begin
    if GetParamInt(params,'startx',i) then setstartx(i); {no check since it is binning dependend and clamped later. Check is done in startExposure}
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='starty' then begin
    if GetParamInt(params,'starty',i) then setstarty(i); {no check since it is binning dependend and clamped later. Check is done in startExposure}
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='numx' then begin
    if GetParamInt(params,'numx',i) then  setnumx(i);  {no check since it is binning dependend and clamped later. Check is done in startExposure}
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='numy' then begin
    if GetParamInt(params,'numy',i) then setnumy(i);  {no check since it is binning dependend and clamped later. Check is done in startExposure}
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cooleron' then begin
    if GetParamBool(params,'cooleron',ok) then
      setcooler(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='binx' then begin
    if GetParamInt(params,'binx',i) then
      setbinX(i,ok);
      if ok=false then set_invalid_range(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='biny' then begin
    if GetParamInt(params,'biny',i) then
      setbinY(i,ok);
    if ok=false then set_invalid_range(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if ((method='offset') or (method='fastreadout') or (method='subexposureduration')) then begin
    set_not_implemented;  {not implemented}
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end



  else if method='startexposure' then begin
    if GetParamFloat(params,'Duration',x)  then startexposure(x,{out} i);
    if i=1 then set_invalid_range(x) else
    if i=2 then
    begin
      FErrorNumber:=1025;
      FErrorMessage:=('Error, subframe is outside main frame! Check frame settings');
    end;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end

  else if method='setccdtemperature' then begin
    if GetParamFloat(params,'SetCCDTemperature',x)  then
      SetCCDTemperature(x,error_float);
    if x-error_float<>0 then {out of range}
      set_invalid_range(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='gain' then begin
    if GetParamInt(params,'gain',i)  then
    SetGain(i,error);
    if error<>0 then {out of range}
      set_invalid_range(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end

  else begin
    result:='PUT - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function  T_AlpacaCamera.ProcessSetup(req: string; out status: integer):string;
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

