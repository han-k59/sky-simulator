unit cu_alpacatelescope;

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

  T_AlpacaTelescope = class(T_AlpacaDevice)
    protected
      TargetRA, TargetDEC: double;
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string; override;
      function  ProcessSetup(req: string; out status: integer):string; override;
      function  GetSetupPage: string; virtual; abstract;
      function  alignmentmode: integer; virtual; abstract;
      function  altitude: double; virtual; abstract;
      function  aperturearea: double; virtual; abstract;
      function  aperturediameter: double; virtual; abstract;
      function  athome: boolean; virtual; abstract;
      function  atpark: boolean; virtual; abstract;
      function  azimuth: double; virtual; abstract;
      function  canpark: boolean; virtual; abstract;
      function  canunpark: boolean; virtual; abstract;
      function  canpulseguide: boolean; virtual; abstract;
      function  cansetdeclinationrate: boolean; virtual; abstract;
      function  cansetguiderates: boolean; virtual; abstract;
      function  cansetpark: boolean; virtual; abstract;
      function  cansetpierside: boolean; virtual; abstract;
      function  cansetrightascensionrate: boolean; virtual; abstract;
      function  cansettracking: boolean; virtual; abstract;
      function  canslew: boolean; virtual; abstract;
      function  canslewaltaz: boolean; virtual; abstract;
      function  canslewaltazasync: boolean; virtual; abstract;
      function  canslewasync: boolean; virtual; abstract;
      function  cansync: boolean; virtual; abstract;
      function  canfindhome: boolean; virtual; abstract;
      function  cansyncaltaz: boolean; virtual; abstract;
      function  declination: double; virtual; abstract;
      function  declinationrate: double; virtual; abstract;
      procedure setdeclinationrate(value: double); virtual; abstract;
      function  doesrefraction: boolean; virtual; abstract;
      procedure setdoesrefraction(value: boolean); virtual; abstract;
      function  equatorialsystem: integer; virtual; abstract;
      function  focallength: double; virtual; abstract;
      function  guideratedeclination: double; virtual; abstract;
      procedure setguideratedeclination(value: double); virtual; abstract;
      function  guideraterightascension: double; virtual; abstract;
      procedure setguideraterightascension(value: double); virtual; abstract;
      function  ispulseguiding: boolean; virtual; abstract;
      function  rightascension: double; virtual; abstract;
      function  rightascensionrate: double; virtual; abstract;
      procedure setrightascensionrate(value: double); virtual; abstract;
      function  siderealtime: double; virtual; abstract;
      function  sideofpier: integer; virtual; abstract;
      procedure setsideofpier(value: integer); virtual; abstract;
      function  siteelevation: double; virtual; abstract;
      procedure setsiteelevation(value: double); virtual; abstract;
      function  sitelatitude: double; virtual; abstract;
      procedure setsitelatitude(value: double); virtual; abstract;
      function  sitelongitude: double; virtual; abstract;
      procedure setsitelongitude(value: double); virtual; abstract;
      function  is_slewing: boolean; virtual; abstract;

      function  slewsettletime: integer; virtual; abstract;
      procedure setslewsettletime(value: integer; out ok : boolean); virtual; abstract;
      function  targetdeclination: double; virtual; abstract;
      procedure settargetdeclination(value: double; out ok:boolean); virtual; abstract;
      function  targetrightascension: double; virtual; abstract;
      procedure settargetrightascension(value: double; out ok:boolean); virtual; abstract;
      function  tracking: boolean; virtual; abstract;
      procedure settracking(value: boolean); virtual; abstract;
      function  trackingrate: integer; virtual; abstract;
      procedure settrackingrate(value: integer); virtual; abstract;
      function  trackingrates: TTrackingRates; virtual; abstract;
      function  utcdate: string; virtual; abstract;
      procedure setutcdate(value: string); virtual; abstract;
      procedure abortslew; virtual; abstract;
      function  axisrates(axis:integer): TAxisRates; virtual; abstract;
      function  canmoveaxis(axis:integer): boolean; virtual; abstract;

      procedure axis_rates; virtual; abstract;
      function  destinationsideofpier(ra,dec: double):integer; virtual; abstract;
      procedure findhome; virtual; abstract;
      procedure moveaxis(axis:integer;rate:double); virtual; abstract;
      procedure park; virtual; abstract;
      procedure pulseguide(direction,duration: integer); virtual; abstract;
      procedure setpark; virtual; abstract;
      procedure slewtoaltaz(az,alt: double); virtual; abstract;
      procedure slewtoaltazasync(az,alt: double); virtual; abstract;
      procedure slewtocoordinates(ra,dec: double; out ok:boolean); virtual; abstract;
      procedure slewtocoordinatesasync(ra,dec: double; out ok:boolean); virtual; abstract;
      procedure slewtotarget; virtual; abstract;
      procedure slewtotargetasync; virtual; abstract;
      procedure synctoaltaz(az,alt: double); virtual; abstract;
      procedure synctocoordinates(ra,dec: double; out ok: boolean ); virtual; abstract;
      procedure synctotarget; virtual; abstract;
      procedure unpark; virtual; abstract;

  end;

implementation

constructor T_AlpacaTelescope.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TargetRA:=NullCoord;
  TargetDEC:=NullCoord;
end;

destructor  T_AlpacaTelescope.Destroy;
begin
  inherited Destroy;
end;

function  T_AlpacaTelescope.ProcessGetRequest(req: string; ServerTransactionID:LongWord; out status: integer):string;
var method,value: string;
    ok: boolean;
    lst:TStringList;
    x,ra,dec: double;
    params: TStringlist;
    i: integer;
    axr:TAxisRates;
    trr:TTrackingRates;
    ClientID,ClientTransactionID:Longword;
begin
  params:=TStringlist.Create;
  DecodeRequest(req,method,params,ClientID,ClientTransactionID);
  status:=200;
  FErrorNumber:=0;
  FErrorMessage:='';
  ok:=false; i:=0; setlength(axr,0); value:='';
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
  else if method='alignmentmode' then begin
    i:=alignmentmode;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='equatorialsystem' then begin
    i:=equatorialsystem;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sideofpier' then begin
    i:=sideofpier;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='trackingrate' then begin
    i:=trackingrate;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='trackingrates' then begin
    trr:=trackingrates;
    result:=FormatIntArrayResp(trr,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewsettletime' then begin
    i:=slewsettletime;
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='destinationsideofpier' then begin
    if GetParamFloat(params,'RightAscension',ra) and GetParamFloat(params,'Declination',dec) then
      i:=destinationsideofpier(ra,dec);
    result:=FormatIntResp(i,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='altitude' then begin
    x:=altitude;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='aperturearea' then begin
    x:=aperturearea;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='aperturediameter' then begin
    x:=aperturediameter;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='azimuth' then begin
    x:=azimuth;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='declination' then begin
    x:=declination;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='declinationrate' then begin
    x:=declinationrate;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='focallength' then begin
    x:=focallength;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='guideratedeclination' then begin
    x:=guideratedeclination;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='guideraterightascension' then begin
    x:=guideraterightascension;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='rightascension' then begin
    x:=rightascension;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='rightascensionrate' then begin
    x:=rightascensionrate;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='siderealtime' then begin
    x:=siderealtime;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='siteelevation' then begin
    x:=siteelevation;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sitelatitude' then begin
    x:=sitelatitude;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sitelongitude' then begin
    x:=sitelongitude;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='targetdeclination' then begin
    x:=targetdeclination;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='targetrightascension' then begin
    x:=targetrightascension;
    result:=FormatFloatResp(x,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='utcdate' then begin
    value:=utcdate;
    result:=FormatStringResp(value,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='athome' then begin
    ok:=athome;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='atpark' then begin
    ok:=atpark;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canpark' then begin
    ok:=canpark;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canunpark' then begin
    ok:=canunpark;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canpulseguide' then begin
    ok:=canpulseguide;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetdeclinationrate' then begin
    ok:=cansetdeclinationrate;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetguiderates' then begin
    ok:=cansetguiderates;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetpark' then begin
    ok:=cansetpark;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetpierside' then begin
    ok:=cansetpierside;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansetrightascensionrate' then begin
    ok:=cansetrightascensionrate;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansettracking' then begin
    ok:=cansettracking;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canslew' then begin
    ok:=canslew;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canslewaltaz' then begin
    ok:=canslewaltaz;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canslewaltazasync' then begin
    ok:=canslewaltazasync;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canslewasync' then begin
    ok:=canslewasync;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansync' then begin
    ok:=cansync;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='cansyncaltaz' then begin
    ok:=cansyncaltaz;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canfindhome' then begin
    ok:=canfindhome;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='doesrefraction' then begin
    ok:=doesrefraction;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='ispulseguiding' then begin
    ok:=ispulseguiding;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewing' then begin
    ok:=is_slewing;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='tracking' then begin
    ok:=tracking;
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='canmoveaxis' then begin
    if GetParamInt(params,'Axis',i) then
      ok:=canmoveaxis(i);
    result:=FormatBoolResp(ok,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='axisrates' then begin
    if GetParamInt(params,'Axis',i) then
       axr:=axisrates(i);
    result:=FormatAxisRateResp(axr,ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else begin
    result:='GET - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function  T_AlpacaTelescope.ProcessPutRequest(req,arg: string; ServerTransactionID:LongWord; out status: integer):string;
var method,p1,p2,value: string;
    ok,bvalue: boolean;
    x,y: double;
    params: TStringlist;
    i,j: integer;
    ClientID,ClientTransactionID: LongWord;

    procedure set_invalid_range2(i,j : double);
    begin
      fErrorNumber:=ERR_INVALID_VALUE; {1025}
      FErrorMessage:=MSG_OUT_OF_RANGE+' Set values: '+floattostrF(i,ffGeneral,5,2)+', '+floattostrF(j,ffGeneral,5,2);
    end;
    procedure set_invalid_range(i : double);
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
    if GetParamBool(params,'connected',ok) then
      SetConnected(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='abortslew' then begin
    abortslew;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='findhome' then begin
    findhome;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='park' then begin
    park;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='unpark' then begin
    unpark;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='setpark' then begin
    setpark;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtotarget' then begin
    slewtotarget;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtotargetasync' then begin
    slewtotargetasync;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='synctotarget' then begin
    synctotarget;
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='doesrefraction' then begin
    if GetParamBool(params,'DoesRefraction',ok) then
      setdoesrefraction(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='tracking' then begin
    if GetParamBool(params,'Tracking',ok) then
      settracking(ok);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='declinationrate' then begin
    if GetParamFloat(params,'DeclinationRate',x) then
      setdeclinationrate(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='guideratedeclination' then begin
    if GetParamFloat(params,'GuideRateDeclination',x) then
      setguideratedeclination(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='guideraterightascension' then begin
    if GetParamFloat(params,'GuideRateRightAscension',x) then
      setguideraterightascension(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='rightascensionrate' then begin
    if GetParamFloat(params,'RightAscensionRate',x) then
      setrightascensionrate(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='siteelevation' then begin
    if GetParamFloat(params,'SiteElevation',x) then
      setsiteelevation(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sitelatitude' then begin
    if GetParamFloat(params,'SiteLatitude',x) then
      setsitelatitude(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sitelongitude' then begin
    if GetParamFloat(params,'SiteLongitude',x) then
      setsitelongitude(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='targetdeclination' then begin
    if GetParamFloat(params,'TargetDeclination',x) then
      settargetdeclination(x,ok);
    if ok=false then set_invalid_range(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='targetrightascension' then begin
    if GetParamFloat(params,'TargetRightAscension',x) then
      settargetrightascension(x,ok);
    if ok=false then set_invalid_range(x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='utcdate' then begin
    if GetParamString(params,'UTCDate',value) then
      setutcdate(value);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='trackingrate' then begin
    if GetParamInt(params,'TrackingRate',i) then
      settrackingrate(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='sideofpier' then begin
    if GetParamInt(params,'SideOfPier',i) then
      setsideofpier(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewsettletime' then begin
    if GetParamInt(params,'SlewSettleTime',i) then
      setslewsettletime(i, ok);
    if ok=false then set_invalid_range(i);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='moveaxis' then begin
    if GetParamInt(params,'Axis',i) and GetParamFloat(params,'Rate',x) then
      moveaxis(i,x);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='pulseguide' then begin
    if GetParamInt(params,'Direction',i) and GetParamInt(params,'Duration',j) then
      pulseguide(i,j);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtoaltaz' then begin
    if GetParamFloat(params,'Azimuth',x) and GetParamFloat(params,'Altitude',y) then
      slewtoaltaz(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtoaltazasync' then begin
    if GetParamFloat(params,'Azimuth',x) and GetParamFloat(params,'Altitude',y) then
      slewtoaltazasync(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtocoordinates' then begin
    if GetParamFloat(params,'RightAscension',x) and GetParamFloat(params,'Declination',y) then
      slewtocoordinates(x,y, ok);
    if ok=false then set_invalid_range2(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='slewtocoordinatesasync' then begin
    if GetParamFloat(params,'RightAscension',x) and GetParamFloat(params,'Declination',y) then
      slewtocoordinatesasync(x,y, ok);
    if ok=false then set_invalid_range2(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='synctocoordinates' then begin
    if GetParamFloat(params,'RightAscension',x) and GetParamFloat(params,'Declination',y) then
      synctocoordinates(x,y,ok);
    if ok=false then set_invalid_range2(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else if method='synctoaltaz' then begin
    if GetParamFloat(params,'Azimuth',x) and GetParamFloat(params,'Altitude',y) then
      synctoaltaz(x,y);
    result:=FormatEmptyResp(ClientTransactionID,ServerTransactionID,FErrorNumber,FErrorMessage);
  end
  else begin
    result:='PUT - Unknown device method: '+method;
    status:=400;
  end;
  params.Free;
end;

function  T_AlpacaTelescope.ProcessSetup(req: string; out status: integer):string;
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

