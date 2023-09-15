unit alpaca_mount_protocol;

{$mode objfpc}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
{
Copyright (C) 2021 Patrick Chevalley & Han Kleijn

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

{
This unit implement the specific telescope protocol.
For the detail of the requirement for every function see:
https://www.ascom-standards.org/Help/Developer/html/T_ASCOM_DeviceInterface_ITelescopeV3.htm

Beware that many unimplemented result in this example do not respect the ASCOM protocol.
Be sure to test the driver using the ASCOM Conformance Checker Tool to validate the result.
}

interface

uses  cu_alpacamount, cu_alpacadevice, Classes, SysUtils, math,
      LazSysUtils; {nowUtc}


function alpaca_ra : double;{RA position mount}
function alpaca_dec : double;{DEC position mount}
procedure mount_simulation;{called by the timer every second}
function crosses_meridian(meridian :double) : boolean;
var
  ra_encoder: double=3.5*15;{RA encoder position, degrees}
  ra_corr: double=0;{mount sync correction}
  dec_encoder: double=50;{DEC encoder position}
  dec_corr: double=0;{mount sync correction}

  alpaca_ra_target: double=3.5;{hours}
  alpaca_dec_target: double=50;{degree}

  alpaca_ra_target2: double=3.5;{hours. For slew to target}
  alpaca_dec_target2: double=50;{degree}

  alpaca_tracking: boolean=true;
  alpaca_mount_slewing: boolean=false;
  slew_settletime: integer=1;
  sideofpier_alpaca:integer=99;  //0 = pierEast, 1 = pierWest, -1= pierUnknown. Start with 99 to initialise a setting. Else it is done when slewing
  DecPulseReverses :boolean=true;//normal situation. Mount N/S pulsing is swapped after meridian flip, same with camera
  NSswapped: integer=1;//for pulse guiding

  pulseNorth : integer=0;
  pulseSouth : integer=0;
  pulseEast  : integer=0;
  pulseWest  : integer=0;

  backslash  : integer=0;
  pulsedirection:  integer=0;

  meridian          : double=2; //hours
  equatorial_mount  : boolean=true;

  guiderateRa: double=0.5*360/(24*60*60);// 0.5x and 1.5x rate
  guiderateDec: double=0.5*360/(24*60*60);// 0.5x and 1.5x rate

  theaxisrates : array[0..2]  of double=(0,0,0);

type

  T_Alpaca_Mount = class(T_AlpacaTelescope)
    protected
    public
      constructor Create(AOwner: TComponent);override;
      destructor  Destroy; override;
      function  GetGuid: string; override;
      function  GetSetupPage: string; override;
      function  Action( actionName, actionParameters: string):string; override;
      procedure CommandBlind( command: string;  raw: boolean = false); override;
      function  CommandBool(command: string;  raw: boolean = false):boolean; override;
      function  CommandString(command: string;  raw: boolean = false):string; override;
      function  Connected:boolean; override;
      procedure SetConnected(value:boolean); override;
      function  Description:string; override;
      function  DriverInfo:string; override;
      function  DriverVersion:string; override;
      function  Name:string; override;
      function  InterfaceVersion: integer; override;
      function  SupportedActions:TStringList; override;
      function  alignmentmode: integer; override;
      function  altitude: double; override;
      function  aperturearea: double; override;
      function  aperturediameter: double; override;
      function  athome: boolean; override;
      function  atpark: boolean; override;
      function  azimuth: double; override;
      function  canpark: boolean; override;
      function  canunpark: boolean; override;
      function  canpulseguide: boolean; override;
      function  cansetdeclinationrate: boolean; override;
      function  cansetrightascensionrate: boolean; override;
      function  cansetguiderates: boolean; override;
      function  cansetpark: boolean; override;
      function  cansetpierside: boolean; override;
      function  cansettracking: boolean; override;
      function  canslew: boolean; override;
      function  canslewaltaz: boolean; override;
      function  canslewaltazasync: boolean; override;
      function  canslewasync: boolean; override;
      function  cansync: boolean; override;
      function  canfindhome: boolean; override;
      function  cansyncaltaz: boolean; override;
      function  declination: double; override;
      function  declinationrate: double; override;
      procedure setdeclinationrate(value: double); override;
      function  doesrefraction: boolean; override;
      procedure setdoesrefraction(value: boolean); override;
      function  equatorialsystem: integer; override;
      function  focallength: double; override;

      function  guideratedeclination: double; override;
      function  guideraterightascension: double; override;

      procedure setguideratedeclination(value: double); override;
      procedure setguideraterightascension(value: double); override;
      function  ispulseguiding: boolean; override;

      function  rightascension: double; override;
      function  rightascensionrate: double; override;
      procedure setrightascensionrate(value: double); override;
      function  sideofpier: integer; override;
      procedure setsideofpier(value: integer); override;
      function  siderealtime: double; override;
      function  siteelevation: double; override;
      procedure setsiteelevation(value: double); override;
      function  sitelatitude: double; override;
      procedure setsitelatitude(value: double); override;
      function  sitelongitude: double; override;
      procedure setsitelongitude(value: double); override;
      function  is_slewing: boolean; override;
      function  slewsettletime: integer; override;
      procedure setslewsettletime(value: integer; out ok : boolean); override;
      function  targetdeclination: double; override;
      procedure settargetdeclination(value: double;out ok:boolean); override;
      function  targetrightascension: double; override;
      procedure settargetrightascension(value: double;out ok :boolean); override;
      function  tracking: boolean; override;
      procedure settracking(value: boolean); override;
      function  utcdate: string; override;
      function  trackingrate: integer; override;
      procedure settrackingrate(value: integer); override;
      function  trackingrates: TTrackingRates; override;
      procedure setutcdate(value: string); override;
      procedure abortslew; override;
      function  axisrates(axis:integer): TAxisRates; override;

      function  canmoveaxis(axis:integer): boolean; override;
      function  destinationsideofpier(ra,dec: double):integer; override;
      procedure findhome; override;
      procedure moveaxis(axis:integer;rate:double); override;
      procedure park; override;
      procedure pulseguide(direction,duration: integer); override;
      procedure setpark; override;
      procedure slewtoaltaz(az,alt: double); override;
      procedure slewtoaltazasync(az,alt: double); override;
      procedure slewtocoordinates(ra,dec: double; out ok : boolean); override;
      procedure slewtocoordinatesasync(ra,dec: double; out ok : boolean); override;
      procedure slewtotarget; override;
      procedure slewtotargetasync; override;
      procedure synctoaltaz(az,alt: double); override;
      procedure synctocoordinates(ra,dec: double; out ok:boolean); override;
      procedure synctotarget; override;
      procedure unpark; override;
  end;


implementation

uses sky_annotation,sky_simulator_main; {for siderealtime}

function inc_angle(angle, adjustment :double): double; {make range -180..+180, simulate encoder behaviour}
begin
  result:=angle+adjustment;
  while result>180 do result:=result-360;
  while result<-180 do result:=result+360;
end;


function alpaca_dec : double;{DEC position mount}
begin
  result:=inc_angle(dec_encoder,dec_corr);{calculate reported DEC position}

  if result>+90 then {crossing celestial pole}
    result:=180-result;
  if result<-90 then {crossing celestial pole}
    result:=-180-result;
end;


function alpaca_ra : double;{RA position mount}
var de :double;
begin
  de:=inc_angle(dec_encoder,dec_corr);{calculate reported DEC position}

  if de>+90 then {crossing celestial pole, add 180 degrees to ra}
    result:=inc_angle(ra_encoder,ra_corr+180)
  else
  if de<-90 then {crossing celestial pole, add 180 degrees to ra}
    result:=inc_angle(ra_encoder,ra_corr+180)
  else
    result:=inc_angle(ra_encoder,ra_corr);


  if result<0 then result:=result+360; {make range 0..360}
  result:=result/15; {make hours}
end;


function angular_distance(r1,r2 :double) : double;//in hours
begin
  result:=(r2-r1);
  if result>12 then result:=result-24
  else
  if result<-12 then result:=result+24;
end;


function crosses_meridian(meridian :double) : boolean;
var
  angular_distance_mount, angular_distance_target : double;
begin
  angular_distance_mount:=angular_distance(meridian,alpaca_ra);//from meridian
  angular_distance_target:=angular_distance(meridian,alpaca_ra_target);//meridian

  result:=( (angular_distance_mount>0) <> (angular_distance_target>0) );//both positive or both negative. So at same side

  if angular_distance_mount>=0 then sideofpier_alpaca:=1 else sideofpier_alpaca:=0; // 0=pierEast(pointing West), 1=pierWest(Pointing East), -1=pierUnknown

  if abs(abs(angular_distance_mount)-12)<=0.00001 then result:=false; //Exactly at north. Meridian crossing will no longer occur. Mount will stop briefly at North. Simplified simulation
end;


procedure mount_simulation;{called every second by the timer}
var
  deltaRa,deltaDec,stepRa,stepDec,ra_target_north : double;
  reverseDecPulse: integer;
begin
  if alpaca_mount_slewing then
  begin

    if equatorial_mount=false then sideofpier_alpaca:=-1; // 0=pierEast(pointing West), 1=pierWest(Pointing East), -1=pierUnknown
    if ((equatorial_mount) and (crosses_meridian(meridian) )) then
    begin //go first to north to avoid crossing meridian
      ra_target_north:=inc_angle((meridian)*15,180)/15;
    end
    else
    begin //no meridian crossing
      ra_target_north:=alpaca_ra_target;
    end;

    deltaRa:=inc_angle((ra_target_north-alpaca_ra)*15,0); {calculate ra distance in degrees in range -180..+180 degrees}
    stepRA:=min(abs(deltaRA),10); {degrees, slew speed ten degree per second}
    if deltaRA<0 then
      ra_encoder:=inc_angle(ra_encoder,-stepRa)    //decrement
    else
      ra_encoder:=inc_angle(ra_encoder,+stepRa);   //increment

    deltaDec:=alpaca_dec_target-alpaca_Dec;{calculate dec distance}
    stepDec:=min(abs(deltaDec),10); {slew speed ten degree per second}
    if deltaDec<0 then
      dec_encoder:=inc_angle(dec_encoder,-stepDec)
    else
      dec_encoder:=inc_angle(dec_encoder,+stepDec);

    if ra_target_north=alpaca_ra_target then //do not stop at first stop north
      alpaca_mount_slewing:=((stepRa>0.00001) or (stepDec>0.00001)) {reached target?}
    else
      alpaca_mount_slewing:=true;

  end
  else
  begin
    if alpaca_tracking=false then
    begin
      ra_encoder:=inc_angle(ra_encoder,15/3600); //if no tracking increase RA with 15 arc seconds per second or one RA second
    end
    else
    begin //Normal tracking. Processed every second
      //pulse guiding preperation
      if ((equatorial_mount) and (DecPulseReverses)) then //normal situation. Mount N/S pulsing is swapped after meridian flip, same with camera
      begin
        if Sideofpier_alpaca=1 then  reverseDecPulse:=-1 //If east then N/S buttons are reversed,  0=pierEast(pointing West), 1=pierWest(Pointing East), -1=pierUnknown
        else
        if Sideofpier_alpaca=0 then reverseDecPulse:=+1 // If west then N/S buttons normal,  0=pierEast(pointing West), 1=pierWest(Pointing East), -1=pierUnknown, equatorial meridian flipped mount ,the declination motor has turned upside down.
        else
        reverseDecPulse:=+1;
      end
      else
        reverseDecPulse:=+1;//mount software prevents reversing

      if backslash<>0 then
      begin
        if ((pulsedirection=+1){north} and (pulseSouth<>0))  then begin pulseSouth:=max(pulseSouth-100,0);pulsedirection:=-1;{south} end;//100 ms backslash
        if ((pulsedirection=-1){south} and (pulseNorth<>0))  then begin pulseNorth:=max(pulseNorth-100,0);pulsedirection:=1;{North} end;//100 ms backslash
      end;
      ra_encoder:=inc_angle(ra_encoder, (pulseEast-pulseWest){ms}*guiderateRa/1000); //pulse duration is in milliseconds
      dec_encoder:=inc_angle(dec_encoder,NSswapped*reverseDecPulse*(pulseNorth-pulseSouth) {ms}*guiderateDec/1000);

      pulseNorth:=0;//processed, zero it.
      pulseSouth:=0;
      pulseEast:=0;
      pulseWest:=0;

      ra_encoder:=ra_encoder+theaxisrates[0];//[deg/sec] normal zero but axis rate can be adjusted by moveaxis
      dec_encoder:=dec_encoder+ NSswapped*reverseDecPulse*theaxisrates[1];

      //if alpaca_mount_slewing then memo2_message('reverse dec pulse '+inttostr(reverseDecPulse)+'  Side of pier '+ inttostr(Sideofpier_alpaca)+ ' slewing' )
      //else memo2_message('reverse dec pulse '+inttostr(reverseDecPulse)+'  Side of pier '+ inttostr(Sideofpier_alpaca)+ ' not slewing !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!' )
    end
  end;
end;


// Replace the following by the driver UniqueID
// On Linux this can be generated by the command uuidgen
// See the ASCOM Alpaca Management API configureddevices
// https://ascom-standards.org/api/?urls.primaryName=ASCOM%20Alpaca%20Management%20API#/Management%20Interface%20(JSON)/get_management_v1_configureddevices
const guid='mount_sky_simulator_unique-id';

constructor T_Alpaca_Mount.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_Alpaca_Mount.Destroy;
begin
  inherited Destroy;
end;

function  T_Alpaca_Mount.GetGuid: string;
begin
  result:=guid;
end;

function  T_Alpaca_Mount.Action( actionName, actionParameters: string):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_Alpaca_Mount.CommandBlind( command: string;  raw: boolean = false);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.CommandBool(command: string;  raw: boolean = false):boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_Mount.CommandString(command: string;  raw: boolean = false):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

function   T_Alpaca_Mount.GetSetupPage: string;
begin
  result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
       '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
       '<title>Mount driver</title></head><body text>'+
       '<H1>Driver Setup</H1><br/>'+
       'There is no Sky Simulator setup information.<br/><br/>'+
       '</body></html>';
end;

function  T_Alpaca_Mount.Connected:boolean;
begin
  result:=FConnected;
end;

procedure  T_Alpaca_Mount.SetConnected(value:boolean);
begin
  // code to make connection
  FConnected:=value;
end;

function  T_Alpaca_Mount.Description:string;
begin
  result:='Sky simulator mount.';
end;

function  T_Alpaca_Mount.DriverInfo:string;
begin
  result:='Sky simulator mount.';
end;

function  T_Alpaca_Mount.DriverVersion:string;
begin
  result:='v1';
end;

function  T_Alpaca_Mount.InterfaceVersion: integer;
begin
  result:=3; // ITelescope version implementation
end;

function  T_Alpaca_Mount.Name:string;
begin
  result:='Mount Sky Simulator for ALPACA';
end;

function  T_Alpaca_Mount.SupportedActions:TStringList;
begin
  result:=TStringList.Create;
  result.Clear;
  result.add('slewtocoordinates');
  result.add('Many actions are supported!');
end;

function  T_Alpaca_Mount.alignmentmode: integer;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.altitude: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Mount.aperturearea: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_Alpaca_Mount.aperturediameter: double;
begin
 FErrorNumber:=ERR_NOT_IMPLEMENTED;
 FErrorMessage:=MSG_NOT_IMPLEMENTED;
 result:=0;
end;

function  T_Alpaca_Mount.athome: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.atpark: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.azimuth: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Mount.canpark: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.canunpark: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.canpulseguide: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.cansetdeclinationrate: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.cansetguiderates: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.cansetpark: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.cansetpierside: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.cansetrightascensionrate: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.cansettracking: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.canslew: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.canslewaltaz: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.canslewaltazasync: boolean;
begin
  result:=false;
end;

function T_Alpaca_Mount.canslewasync: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.cansync: boolean;
begin
  result:=true;
end;

function  T_Alpaca_Mount.canfindhome: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.cansyncaltaz: boolean;
begin
  result:=false;
end;

function  T_Alpaca_Mount.declination: double;
begin
  result:=alpaca_dec;
end;

function  T_Alpaca_Mount.declinationrate: double;
begin
  result:= theaxisrates[1]*3600;//from [deg/sec] to ["/sec]
end;

procedure T_Alpaca_Mount.setdeclinationrate(value: double);
begin
  theaxisrates[1]:=value/3600;//from ["/sec] to [deg/sec]
end;

function  T_Alpaca_Mount.doesrefraction: boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=true;
end;

procedure T_Alpaca_Mount.setdoesrefraction(value: boolean);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.equatorialsystem: integer;
begin
  result:=2; {J2000 for simulation}
  {equOther	0	Custom or unknown equinox and/or reference frame.
  equTopocentric	1	Topocentric coordinates. Coordinates of the object at the current date having allowed for annual aberration, precession and nutation. This is the most common coordinate type for amateur telescopes.
  equJ2000	2	J2000 equator/equinox. Coordinates of the object at mid-day on 1st January 2000, ICRS reference frame.
  equJ2050	3	J2050 equator/equinox, ICRS reference frame. }
end;

function  T_Alpaca_Mount.focallength: double;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

function  T_Alpaca_Mount.guideratedeclination: double;
begin
  result:=guiderateDec;
end;

procedure T_Alpaca_Mount.setguideratedeclination(value: double);
begin
  guiderateDec:=value;
end;

function  T_Alpaca_Mount.guideraterightascension: double;
begin
  result:=guiderateRa;
end;

procedure T_Alpaca_Mount.setguideraterightascension(value: double);
begin
  guiderateRa:=value;
end;

function  T_Alpaca_Mount.ispulseguiding: boolean;
begin
  result:=((pulseNorth>0) or  (pulseSouth>0) or (pulseEast>0) or (pulseWest>0));
end;

function  T_Alpaca_Mount.rightascension: double;
begin
  result:=alpaca_ra;
end;

function  T_Alpaca_Mount.rightascensionrate: double;
begin
  result:= theaxisrates[0]*(24/(360*0.9972695677))*3600;//from [deg/sec] to [ra_sec/sec]
  //To convert a given rate in (the more common) units of sidereal seconds per UTC (clock) second, multiply the value by 0.9972695677 (the number of UTC seconds in a sidereal second) then set the property. Please note that these units were chosen for the Telescope V1 standard, and in retrospect, this was an unfortunate choice. However, to maintain backwards compatibility, the units cannot be changed. A simple multiplication is all that's needed, as noted.
end;

procedure T_Alpaca_Mount.setrightascensionrate(value: double);
begin
  theaxisrates[0]:=value*(360/24)*0.9972695677/(3600);//from [ra_/sec] to [deg/sec]
  //To convert a given rate in (the more common) units of sidereal seconds per UTC (clock) second, multiply the value by 0.9972695677 (the number of UTC seconds in a sidereal second) then set the property. Please note that these units were chosen for the Telescope V1 standard, and in retrospect, this was an unfortunate choice. However, to maintain backwards compatibility, the units cannot be changed. A simple multiplication is all that's needed, as noted.
end;

function  T_Alpaca_Mount.SiderealTime: double;
begin
  calc_sidereal_time(longitude);
  result:=sidereal_time*12/pi
end;


function  T_Alpaca_Mount.sideofpier: integer;
begin //0 = pierEast, 1 = pierWest, -1= pierUnknown
  result:=sideofpier_alpaca;
end;

procedure T_Alpaca_Mount.setsideofpier(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.siteelevation: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.setsiteelevation(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.sitelatitude: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.setsitelatitude(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.sitelongitude: double;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.setsitelongitude(value: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.is_slewing: boolean;
begin
  result:=alpaca_mount_slewing;
end;

function  T_Alpaca_Mount.slewsettletime: integer;
begin
  result:=slew_settletime; {1}
end;

procedure T_Alpaca_Mount.setslewsettletime(value: integer; out ok : boolean);
begin
  slew_settletime:=max(0,value);
  ok:=(value=slew_settletime);
end;

function  T_Alpaca_Mount.targetdeclination: double;
begin
  result:=alpaca_dec_target2; {temp value till execute slewtotarget}
end;

procedure T_Alpaca_Mount.settargetdeclination(value: double; out ok: boolean);
begin
  alpaca_dec_target2:=min(90,max(-90,value));
  ok:=(abs(alpaca_dec_target2-value)<0.00001); {within range?}
end;

function  T_Alpaca_Mount.targetrightascension: double;
begin
  result:=alpaca_ra_target2; {temp value till execute slewtotarget}
end;

procedure T_Alpaca_Mount.settargetrightascension(value: double; out ok :boolean);
begin
  alpaca_ra_target2:=min(24,max(0,value));
  ok:=(abs(alpaca_ra_target2-value)<0.00001); {within range?}
end;

function  T_Alpaca_Mount.tracking: boolean;
begin
  result:=((alpaca_tracking) and (alpaca_mount_slewing=false));
end;

procedure T_Alpaca_Mount.settracking(value: boolean);
begin
  alpaca_tracking:=value;
end;

function  T_Alpaca_Mount.trackingrate: integer;
begin
  result:=0;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.settrackingrate(value: integer);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.trackingrates: TTrackingRates;
begin
  result:=nil;
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_Mount.utcdate: string;
var
  yy,mm,dd :word;
  hour,min, ss,ms: Word;
  dt         :tdatetime;
  function fl(i:integer):string;
  begin
    result:=inttostr(i);
    if length(result)<=1 then result:='0'+result;
  end;

begin
  dt:=LazSysUtils.NowUTC;
  DeCodeDate(dt,YY,MM,DD);
  DecodeTime(dt,hour,min,ss,ms);
  //2016-03-04T17:45:31.1234567Z or 2016-11-14T07:03:08.1234567Z Please note the compulsary trailing Z indicating the 'Zulu', UTC time zone.
  result:=inttostr(YY)+'-'+fl(MM)+'-'+fl(DD)+'T'+fl(hour)+':'+fl(min)+':'+fl(ss)+'Z';
end;

procedure T_Alpaca_Mount.setutcdate(value: string);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.abortslew;
begin
  alpaca_mount_slewing:=false;


  //alpaca_tracking:=false;
end;

function  T_Alpaca_Mount.axisrates(axis:integer): TAxisRates;
var
  newItem: TAxisRate;
begin
  newitem.create;
  newitem.minimum:=1.5;
  newitem.maximum:=1.5;
  setlength(axisrates,1);
  axisrates[0]:=newitem;
  result:=AxisRates;

end;


function  T_Alpaca_Mount.canmoveaxis(axis:integer): boolean;
begin
  result:=axis<2; //(axis<=axis);  // 0 = axisPrimary, 1 = axisSecondary, 2 = axisTertiary.
end;


function  T_Alpaca_Mount.destinationsideofpier(ra,dec: double):integer;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=0;
end;

procedure T_Alpaca_Mount.findhome;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.moveaxis(axis:integer;rate:double);
begin
  if axis=0 then
    theaxisrates[axis]:=0.004178075-rate //[deg/sec] absolute so subtract sidereal rate. Positive rate goes east
  else
    theaxisrates[axis]:=rate;//[deg/sec] declination
end;

procedure T_Alpaca_Mount.park;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.pulseguide(direction,duration: integer);
begin
  {0=north, 1=south, 2 East, 3 West}
  case direction of  0 : pulseNorth:=pulseNorth+duration;
                     1 : pulseSouth:=pulseSouth+duration;
                     2 : pulseEast:=pulseEast+duration;
                     3 : pulseWest:=pulseWest+duration;
                 end;

end;

procedure T_Alpaca_Mount.setpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.slewtoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.slewtoaltazasync(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.slewtocoordinates(ra,dec: double; out ok : boolean);
begin
  alpaca_ra_target:=min(24,max(0,ra));
  alpaca_dec_target:=min(90,max(-90,dec));
  alpaca_mount_slewing:=true;
  ok:=((abs(alpaca_ra_target-ra)<0.00001) and (abs(alpaca_dec_target-dec)<0.00001)); {within range?}
end;

procedure T_Alpaca_Mount.slewtocoordinatesasync(ra,dec: double; out ok : boolean);
begin
  alpaca_ra_target:=min(24,max(0,ra));
  alpaca_dec_target:=min(90,max(-90,dec));
  alpaca_mount_slewing:=true;
  ok:=((abs(alpaca_ra_target-ra)<0.00001) and (abs(alpaca_dec_target-dec)<0.00001)); {within range?}
end;

procedure T_Alpaca_Mount.slewtotarget;
begin
  alpaca_ra_target:=alpaca_ra_target2; {copy temp value to final value}
  alpaca_dec_target:=alpaca_dec_target2; {copy temp value to final value}
  alpaca_mount_slewing:=true;
end;

procedure T_Alpaca_Mount.slewtotargetasync;
begin
  alpaca_ra_target:=alpaca_ra_target2; {copy temp value to final value}
  alpaca_dec_target:=alpaca_dec_target2; {copy temp value to final value}
  alpaca_mount_slewing:=true;
end;

procedure T_Alpaca_Mount.synctoaltaz(az,alt: double);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

procedure T_Alpaca_Mount.synctocoordinates(ra,dec: double; out ok: boolean);
begin
  if ((ra>=0) and (ra<24) and (dec>=-90) and (dec<=90)) then
  begin
    ra_corr:=ra_corr+15*(ra-alpaca_ra);{Unit is degrees. Sync mount by setting and offset ra_corr}
    dec_corr:=dec_corr+(dec-alpaca_dec);{degrees}
    ok:=true;
  end
  else
  ok:=false
end;

procedure T_Alpaca_Mount.synctotarget;
begin
  ra_corr:=ra_corr+15*(alpaca_ra_target2-alpaca_ra);{Unit is degrees. Sync mount by setting and offset ra_corr}
  dec_corr:=dec_corr+(alpaca_dec_target2-alpaca_dec);{degrees}
end;

procedure T_Alpaca_Mount.unpark;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;


end.

