unit indi_mount;

{$mode objfpc}{$H+}

{
INDI mount simulation and protocol for Sky Simulator.

Copyright (C) 2026 Han Kleijn
https://sourceforge.net/projects/sky-simulator

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

--------------------------------------------------------------------------
THIRD MOUNT OPTION

sky_simulator_main offers the user a choice of drive:

   1) external ASCOM mount over COM        (Windows only)
   2) internal Alpaca drive                alpaca_mount_protocol.pas
   3) internal INDI drive                  THIS UNIT

Only one is active at a time, selected by radio button, so this unit
carries its OWN complete mount simulation: its own encoders, its own
targets, its own tracking and park state, and its own copy of the
simulation step.  Nothing is shared with alpaca_mount_protocol, which
means the two drives can be changed independently without any risk of
one breaking the other.

The simulation code below is a copy of mount_simulation /
crosses_meridian / inc_angle / angular_distance from
alpaca_mount_protocol.pas, with the variables renamed to the indi_
prefix.  Behaviour is identical: slew at 20 deg/s, meridian flip via the
pole, pulse guiding with backlash and N/S reversal, park by slewing just
above the pole.

sky_simulator_main must call indi_mount_simulation from Timer1Timer when
the INDI drive is selected, in the same place where mount_simulation is
called for the Alpaca drive.

EQUINOX
-------
INDI EQUATORIAL_EOD_COORD is by definition equinox of date (JNow), so
this unit works in JNow throughout and has no equinox setting.  The
Alpaca equinox_communication option does not apply here.

TRANSPORT
---------
Sockets, threads and XML live in indi_server.pas.  This unit only
implements the device.
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, math, indi_server;

const
  IndiMountName = 'Sky Simulator Mount';   {the device name a client will show}

var
  {---- mount state, the simulated hardware ----}
  indi_ra_encoder : double = 256.741389196031;  {RA encoder position, degrees}
  indi_ra_corr    : double = 0;                 {mount sync correction, degrees}
  indi_dec_encoder: double = 89.7900991439244;  {DEC encoder position, degrees}
  indi_dec_corr   : double = 0;                 {mount sync correction, degrees}

  indi_ra_target  : double = 3.5;               {hours}
  indi_dec_target : double = 50;                {degrees}

  indi_tracking   : boolean = true;
  {ON_COORD_SET was TRACK, so tracking must be switched on when the slew
   completes.  INDI expects the DRIVER to do this; unlike ASCOM, the client
   sends no separate track command after a goto.  Device state, not per
   client state, so it lives here and not in a private field.}
  indi_track_on_arrival : boolean = false;
  indi_slewing    : boolean = false;
  indi_parked     : integer = 0;   {2 parked, 1 going to park, 0 not parked}
  indi_sideofpier : integer = 99;  {0 pierEast, 1 pierWest, -1 unknown,
                                    99 not yet initialised}

  {---- pulse guiding, milliseconds waiting to be applied ----}
  indi_pulseNorth : integer = 0;
  indi_pulseSouth : integer = 0;
  indi_pulseEast  : integer = 0;
  indi_pulseWest  : integer = 0;

  {---- settings, normally filled from the UI by sky_simulator_main ----}
  indi_meridian         : double  = 2;      {hours}
  indi_equatorial_mount : boolean = true;
  indi_DecPulseReverses : boolean = true;   {N/S pulsing swaps after a flip}
  indi_NSswapped        : integer = 1;
  indi_backslash        : integer = 0;
  indi_pulsedirection   : integer = 0;

  indi_guiderateRa  : double = 0.5*360/(24*60*60);
  indi_guiderateDec : double = 0.5*360/(24*60*60);

  indi_axisrates : array[0..2] of double =
      (360/86164.098903691 {RA deg/sec, sidereal}, 0 {DEC}, 0);

  indi_slew_settletime : integer = 1;
  indi_site_elevation  : double  = 10;

{ ---- simulation ------------------------------------------------------- }
procedure indi_mount_simulation;   {call from Timer1Timer, once per second}
function  indi_crosses_meridian(meridian_: double): boolean;

{ ---- position, JNow --------------------------------------------------- }
procedure indi_get_position(out ra, de: double);  {ra hours, de degrees}
function  indi_ra: double;                        {hours}
function  indi_dec: double;                       {degrees}

{ ---- commands --------------------------------------------------------- }
procedure indi_slew(ra, de: double; out ok: boolean);   {ra hours, de degrees}
procedure indi_sync(ra, de: double; out ok: boolean);
procedure indi_abort;
procedure indi_park;
procedure indi_unpark;
procedure indi_set_tracking(value: boolean);
procedure indi_pulseguide(north, south, east, west: integer);  {milliseconds}

type
{ The INDI device.  One instance per connected client, created by
  indi_server; all three methods run in the main thread. }
  T_Indi_Mount = class(TIndiDevice)
  private
    FCoordAction: string;       {TRACK / SLEW / SYNC from ON_COORD_SET}
    {cache of what was last pushed to this client}
    FLastRa, FLastDec: double;
    FLastLat, FLastLon, FLastElev: double;
    FLastTrack: boolean;
    FLastPark, FLastPier: integer;
    FLastEqState: TIndiState;

    FConnected : boolean;   {protocol state, not hardware state}

    procedure DefineDeviceProperties;   {libindi updateProperties() equivalent}
    procedure SendConnection(def: boolean);
    procedure SendDriverInfo;
    procedure SendEqCoord(def: boolean; st: TIndiState);
    procedure SendOnCoordSet(def: boolean);
    procedure SendTrackState(def: boolean);
    procedure SendAbort(def: boolean; st: TIndiState);
    procedure SendPark(def: boolean; st: TIndiState);
    procedure SendPierSide(def: boolean);
    procedure SendGeographic(def: boolean; st: TIndiState);
    procedure SendTimeUtc(def: boolean);
    procedure SendTelescopeInfo(def: boolean);
    procedure SendGuideNS(def: boolean; st: TIndiState);
    procedure SendGuideWE(def: boolean; st: TIndiState);

    procedure DoSwitch(e: TIndiElement);
    procedure DoNumber(e: TIndiElement);
    procedure DoText(e: TIndiElement);
  public
    constructor Create; override;
    procedure DefineAll; override;
    procedure HandleElement(e: TIndiElement); override;
    procedure Poll; override;
  end;

implementation

uses
  sky_annotation,               {latitude}
  sky_simulator_main;           {form1, sidereal_time, memo2_message, jd}

const
  GroupMain   = 'Main Control';
  GroupSite   = 'Site Management';
  GroupMotion = 'Motion Control';
  GroupOpt    = 'Options';
  GroupInfo   = 'General Info';

{ ====================================================================== }
{ Mount simulation.                                                       }
{ Copy of the Alpaca simulation with indi_ variables, so this drive is    }
{ fully independent of alpaca_mount_protocol.                             }
{ ====================================================================== }

function indi_inc_angle(angle, adjustment: double): double;
{add two angles and keep the result in -180..+180 degrees}
begin
  Result:=angle+adjustment;
  while Result>180 do Result:=Result-360;
  while Result<-180 do Result:=Result+360;
end;

function indi_angular_distance(r1, r2: double): double;  {in hours}
begin
  Result:=r1-r2;
  while Result>12 do Result:=Result-24;
  while Result<-12 do Result:=Result+24;
end;

procedure indi_get_position(out ra, de: double);
{Mount position in JNow.  ra in hours, de in degrees.
 No precession: EQUATORIAL_EOD_COORD is equinox of date by definition.}
begin
  de:=indi_inc_angle(indi_dec_encoder,indi_dec_corr);

  if (de>90) or (de<-90) then     {crossing the pole, ra flips by 12 hours}
    ra:=indi_inc_angle(indi_ra_encoder,indi_ra_corr+180)
  else
    ra:=indi_inc_angle(indi_ra_encoder,indi_ra_corr);

  if ra<0 then ra:=ra+360;
  ra:=ra/15;                      {degrees to hours}
end;

function indi_ra: double;
var de: double;
begin
  indi_get_position(Result,de);
end;

function indi_dec: double;
var ra: double;
begin
  indi_get_position(ra,Result);
end;

function indi_crosses_meridian(meridian_: double): boolean;
var
  dist_mount, dist_target: double;
begin
  dist_mount :=indi_angular_distance(meridian_,indi_ra);
  dist_target:=indi_angular_distance(meridian_,indi_ra_target);

  {true when mount and target are on opposite sides of the meridian}
  Result:=((dist_mount>0) <> (dist_target>0));

  {0 = pierEast (pointing west), 1 = pierWest (pointing east)}
  if dist_mount>=0 then indi_sideofpier:=1 else indi_sideofpier:=0;

  if abs(abs(dist_mount)-12)<=0.000003 then
    Result:=false;   {exactly at north, no flip needed. Simplified simulation}
end;

procedure indi_mount_simulation;
{Called every second by Timer1 while the INDI drive is selected.}
var
  deltaRa, deltaDec, stepRa, stepDec, ra_tgt, dec_tgt : double;
  reverseDecPulse, pulseE2, pulseW2, pulseN2, pulseS2 : integer;
begin
  {slew to the park position has finished}
  if (indi_parked=1) and (indi_slewing=false) then
  begin
    indi_parked:=2;
    indi_tracking:=false;
    exit;
  end;

  if indi_slewing then
  begin
    if indi_equatorial_mount=false then indi_sideofpier:=-1;

    if indi_equatorial_mount and indi_crosses_meridian(indi_meridian) then
    begin
      {go to the celestial pole first so the meridian is not crossed directly}
      ra_tgt:=indi_inc_angle(indi_meridian*15,180)/15;
      if latitude>0 then
        dec_tgt:=85          {near the pole; 85 keeps RA movement visible}
      else
        dec_tgt:=-85;
    end
    else
    begin
      ra_tgt :=indi_ra_target;
      dec_tgt:=indi_dec_target;
    end;

    {RA distance in degrees, range -180..+180. Factor 15 for hours to degrees}
    deltaRa:=indi_inc_angle((ra_tgt-indi_ra)*15,0);
    stepRa :=min(abs(deltaRa),20);          {slew speed 20 degrees per second}
    if deltaRa<0 then
      indi_ra_encoder:=indi_inc_angle(indi_ra_encoder,-stepRa)
    else
      indi_ra_encoder:=indi_inc_angle(indi_ra_encoder,+stepRa);

    deltaDec:=dec_tgt-indi_dec;
    stepDec :=min(abs(deltaDec),20);
    if deltaDec<0 then
      indi_dec_encoder:=indi_inc_angle(indi_dec_encoder,-stepDec)
    else
      indi_dec_encoder:=indi_inc_angle(indi_dec_encoder,+stepDec);


    if ra_tgt=indi_ra_target then      {do not stop at the intermediate pole}
      indi_slewing:=((stepRa>0.000003) or (stepDec>0.000003))
    else
      indi_slewing:=true;

    {arrived: honour ON_COORD_SET=TRACK.  Poll pushes the new
     TELESCOPE_TRACK_STATE to every client on the next tick.}
    if (indi_slewing=false) and (indi_track_on_arrival) then
    begin
      indi_track_on_arrival:=false;
      indi_tracking:=true;
    end;

  end
  else
  begin
    if indi_tracking=false then
    begin
      {no tracking: RA increases by 15 arcsec per second, one RA second}
      indi_ra_encoder:=indi_inc_angle(indi_ra_encoder,15/3600);
    end
    else
    begin
      {normal tracking, processed once per second}

      {N/S pulsing is swapped after a meridian flip, same as the camera}
      if indi_equatorial_mount and indi_DecPulseReverses then
      begin
        if indi_sideofpier=1 then reverseDecPulse:=-1
        else
        if indi_sideofpier=0 then reverseDecPulse:=+1
        else
          reverseDecPulse:=+1;
      end
      else
        reverseDecPulse:=+1;     {mount software prevents reversing}

      if indi_backslash<>0 then
      begin
        if (indi_pulsedirection=+1) and (indi_pulseSouth<>0) then
        begin
          indi_pulseSouth:=max(indi_pulseSouth-100,0);   {100 ms backlash}
          indi_pulsedirection:=-1;
        end;
        if (indi_pulsedirection=-1) and (indi_pulseNorth<>0) then
        begin
          indi_pulseNorth:=max(indi_pulseNorth-100,0);
          indi_pulsedirection:=+1;
        end;
      end;

      {process at most 1000 ms of pulse per cycle}
      pulseE2:=min(indi_pulseEast ,1000); indi_pulseEast :=indi_pulseEast -pulseE2;
      pulseW2:=min(indi_pulseWest ,1000); indi_pulseWest :=indi_pulseWest -pulseW2;
      pulseN2:=min(indi_pulseNorth,1000); indi_pulseNorth:=indi_pulseNorth-pulseN2;
      pulseS2:=min(indi_pulseSouth,1000); indi_pulseSouth:=indi_pulseSouth-pulseS2;

      indi_ra_encoder:=indi_inc_angle(indi_ra_encoder,
                          (pulseE2-pulseW2)*indi_guiderateRa/1000);
      indi_dec_encoder:=indi_inc_angle(indi_dec_encoder,
                          indi_NSswapped*reverseDecPulse*
                          (pulseN2-pulseS2)*indi_guiderateDec/1000);

      {axis rates: normally zero net, but adjustable through moveaxis}
      indi_ra_encoder :=indi_ra_encoder+indi_axisrates[0]-15.04107/3600;
      indi_dec_encoder:=indi_dec_encoder+
                          indi_NSswapped*reverseDecPulse*indi_axisrates[1];
    end;
  end;
end;

{ ====================================================================== }
{ Commands                                                                }
{ ====================================================================== }

procedure indi_slew(ra, de: double; out ok: boolean);
begin
  ok:=false;
  if indi_parked<>0 then
  begin
    memo2_message('INDI: slew refused, mount is parked.');
    exit;
  end;
  if (ra<0) or (ra>=24) or (de<-90) or (de>90) then
  begin
    memo2_message('INDI: slew refused, coordinates out of range.');
    exit;
  end;
  indi_ra_target :=ra;
  indi_dec_target:=de;
  indi_slewing:=true;
  memo2_message('INDI slew to [JNow] '+prepare_ra(ra*pi/12)+'  '+
                prepare_dec(de*pi/180));
  ok:=true;
end;

procedure indi_sync(ra, de: double; out ok: boolean);
{Adjust the encoder corrections so the mount reports the given position at
 its current mechanical position.}
var
  ra0, de0: double;
begin
  ok:=false;
  if (ra<0) or (ra>=24) or (de<-90) or (de>90) then exit;
  indi_get_position(ra0,de0);
  indi_ra_corr :=indi_ra_corr +15*(ra-ra0);    {both in degrees}
  indi_dec_corr:=indi_dec_corr+(de-de0);
  memo2_message('INDI sync at [JNow] '+prepare_ra(ra*pi/12)+'  '+
                prepare_dec(de*pi/180));
  ok:=true;
end;


procedure indi_abort;
{Stop any motion at once.  Called from TELESCOPE_ABORT_MOTION and from the
 abort path of the park handling.}
begin
  indi_slewing:=false;

  {An interrupted park never reached the park position, so the mount is not
   parked.  State 1 means "going to park"; leaving it there would make the
   next Poll report Ok on a park that never completed.}
  if indi_parked=1 then indi_parked:=0;

  {ON_COORD_SET=TRACK promises tracking at the END of a slew.  An aborted
   slew has no end, so the promise is void.  Without this, the flag survives
   into the next goto and would switch tracking on even for a client that
   asked for ON_COORD_SET=SLEW.}
  indi_track_on_arrival:=false;

  memo2_message('INDI abort motion.');
end;


procedure indi_park;
{Slew to just above the pole; indi_mount_simulation completes the park and
 stops tracking.}
var
  dect: double;
  ok  : boolean;
begin
  if latitude>0 then dect:=89 else dect:=-89;
  indi_track_on_arrival:=false;  {a park must end with tracking off}
  indi_slew(sidereal_time*12/pi,dect,ok);
  if ok then
  begin
    indi_parked:=1;                        {going to the park position}
    memo2_message('INDI park requested.');
  end;
end;

procedure indi_unpark;
begin
  indi_parked:=0;
  indi_tracking:=true;
  memo2_message('INDI unpark.');
end;

procedure indi_set_tracking(value: boolean);
begin
  indi_tracking:=value;
end;

procedure indi_pulseguide(north, south, east, west: integer);
begin
  if north>0 then indi_pulseNorth:=indi_pulseNorth+north;
  if south>0 then indi_pulseSouth:=indi_pulseSouth+south;
  if east >0 then indi_pulseEast :=indi_pulseEast +east;
  if west >0 then indi_pulseWest :=indi_pulseWest +west;
end;

{ ====================================================================== }
{ T_Indi_Mount                                                            }
{ ====================================================================== }

constructor T_Indi_Mount.Create;
begin
  inherited Create;
  DeviceName:=IndiMountName;
  FCoordAction:='TRACK';             {the INDI default}
  FLastRa:=-999; FLastDec:=-999;
  FLastLat:=-999; FLastLon:=-999; FLastElev:=-999;
  FLastTrack:=not indi_tracking;     {force a first push}
  FLastPark:=-1; FLastPier:=-99;
  FLastEqState:=isIdle;
  FConnected:=false;
end;

{ ---------------- property senders ------------------------------------- }

procedure T_Indi_Mount.SendConnection(def: boolean);
{A driver must come up DISCONNECTED even though the simulator is always
 available.  Clients treat CONNECTION as a handshake: they send CONNECT=On
 and only then start waiting for the device properties.  A device that
 already reports On gives them nothing to do, and they stall.}
var st: TIndiState;
begin
  if FConnected then st:=isOk else st:=isIdle;
  if def then
  begin
    StartDef('Switch','CONNECTION','Connection',GroupMain,st,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','CONNECT','Connect',OnOff(FConnected));
    AddDef('Switch','DISCONNECT','Disconnect',OnOff(not FConnected));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CONNECTION',st);
    AddOne('Switch','CONNECT',OnOff(FConnected));
    AddOne('Switch','DISCONNECT',OnOff(not FConnected));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Mount.SendDriverInfo;
begin
  StartDef('Text','DRIVER_INFO','Driver Info',GroupInfo,isIdle,ipRO);
  AddDef('Text','DRIVER_NAME','Name',DeviceName);
  AddDef('Text','DRIVER_EXEC','Exec','indi_skysimulator');
  AddDef('Text','DRIVER_VERSION','Version','1.0');
  AddDef('Text','DRIVER_INTERFACE','Interface','1');  {1 = TELESCOPE_INTERFACE}
  EndDef('Text');
end;


procedure T_Indi_Mount.SendEqCoord(def: boolean; st: TIndiState);
var ra, de: double;
begin
  indi_get_position(ra,de);
  if def then
  begin
    StartDef('Number','EQUATORIAL_EOD_COORD','Eq. Coordinates',
             GroupMain,st,ipRW);
    AddDef('Number','RA','RA (hh:mm:ss)',F(ra),
           ' format="%010.6m" min="0" max="24" step="0"');
    AddDef('Number','DEC','DEC (dd:mm:ss)',F(de),
           ' format="%010.6m" min="-90" max="90" step="0"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','EQUATORIAL_EOD_COORD',st);
    AddOne('Number','RA',F(ra));
    AddOne('Number','DEC',F(de));
    EndSet('Number');
  end;
end;

procedure T_Indi_Mount.SendOnCoordSet(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','ON_COORD_SET','On coord set',GroupMain,isIdle,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','TRACK','Track',OnOff(FCoordAction='TRACK'));
    AddDef('Switch','SLEW','Slew',OnOff(FCoordAction='SLEW'));
    AddDef('Switch','SYNC','Sync',OnOff(FCoordAction='SYNC'));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','ON_COORD_SET',isOk);
    AddOne('Switch','TRACK',OnOff(FCoordAction='TRACK'));
    AddOne('Switch','SLEW',OnOff(FCoordAction='SLEW'));
    AddOne('Switch','SYNC',OnOff(FCoordAction='SYNC'));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Mount.SendTrackState(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','TELESCOPE_TRACK_STATE','Tracking',GroupMain,
             isOk,ipRW,' rule="OneOfMany"');
    AddDef('Switch','TRACK_ON','On',OnOff(indi_tracking));
    AddDef('Switch','TRACK_OFF','Off',OnOff(not indi_tracking));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','TELESCOPE_TRACK_STATE',isOk);
    AddOne('Switch','TRACK_ON',OnOff(indi_tracking));
    AddOne('Switch','TRACK_OFF',OnOff(not indi_tracking));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Mount.SendAbort(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Switch','TELESCOPE_ABORT_MOTION','Abort Motion',GroupMain,
             isIdle,ipRW,' rule="AtMostOne"');
    AddDef('Switch','ABORT','Abort','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','TELESCOPE_ABORT_MOTION',st);
    AddOne('Switch','ABORT','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_Mount.SendPark(def: boolean; st: TIndiState);
var pk: boolean;
begin
  pk:=(indi_parked=2);
  if def then
  begin
    StartDef('Switch','TELESCOPE_PARK','Parking',GroupMain,st,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','PARK','Park',OnOff(pk));
    AddDef('Switch','UNPARK','UnPark',OnOff(not pk));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','TELESCOPE_PARK',st);
    AddOne('Switch','PARK',OnOff(pk));
    AddOne('Switch','UNPARK',OnOff(not pk));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Mount.SendPierSide(def: boolean);
var
  east, west, known: boolean;
  st: TIndiState;
begin
  {INDI has no pierUnknown.  An alt-az mount, or the state before the first
   meridian calculation, must therefore be sent with BOTH elements Off and
   the vector Idle - a client reads that as unknown.  Sending PIER_WEST for
   an unknown side makes CCDciel believe a meridian flip is due on every
   target, and its abort path then also switches tracking off.}
  known:=(indi_sideofpier=0) or (indi_sideofpier=1);
  east :=(indi_sideofpier=0);        {0 = pierEast, pointing west}
  west :=(indi_sideofpier=1);
  if known then st:=isOk else st:=isIdle;
  if def then
  begin
    StartDef('Switch','TELESCOPE_PIER_SIDE','Pier Side',GroupMain,
             st,ipRO,' rule="OneOfMany"');
    AddDef('Switch','PIER_EAST','East (pointing west)',OnOff(east));
    AddDef('Switch','PIER_WEST','West (pointing east)',OnOff(west));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','TELESCOPE_PIER_SIDE',st);
    AddOne('Switch','PIER_EAST',OnOff(east));
    AddOne('Switch','PIER_WEST',OnOff(west));
    EndSet('Switch');
  end;
end;


procedure T_Indi_Mount.SendGeographic(def: boolean; st: TIndiState);
var la, lo, el: double;
begin
  {the site is a property of the simulated observatory, shared by all
   drives, so it comes from the form as it does for Alpaca}
  la:=strtofloat2(form1.latitude1.text);
  lo:=strtofloat2(form1.longitude1.text);
  if lo<0 then lo:=lo+360;          {INDI wants 0..360 degrees east}
  el:=indi_site_elevation;

  if def then
  begin
    StartDef('Number','GEOGRAPHIC_COORD','Scope Location',GroupSite,st,ipRW);
    AddDef('Number','LAT','Lat (dd:mm:ss)',F(la),
           ' format="%010.6m" min="-90" max="90" step="0"');
    AddDef('Number','LONG','Lon (dd:mm:ss)',F(lo),
           ' format="%010.6m" min="0" max="360" step="0"');
    AddDef('Number','ELEV','Elevation (m)',F(el,1),
           ' format="%g" min="-200" max="10000" step="0"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','GEOGRAPHIC_COORD',st);
    AddOne('Number','LAT',F(la));
    AddOne('Number','LONG',F(lo));
    AddOne('Number','ELEV',F(el,1));
    EndSet('Number');
  end;
end;

procedure T_Indi_Mount.SendTimeUtc(def: boolean);
begin
  if def then
  begin
    StartDef('Text','TIME_UTC','UTC',GroupSite,isOk,ipRW);
    AddDef('Text','UTC','UTC Time',IndiTimeStamp);
    AddDef('Text','OFFSET','UTC Offset','0');
    EndDef('Text');
  end
  else
  begin
    StartSet('Text','TIME_UTC',isOk);
    AddOne('Text','UTC',IndiTimeStamp);
    AddOne('Text','OFFSET','0');
    EndSet('Text');
  end;
end;

procedure T_Indi_Mount.SendTelescopeInfo(def: boolean);
begin
  {focal length is an optical property of the simulated telescope, shared
   with the camera, so it stays in sky_simulator_main's world}
  if def then
  begin
    StartDef('Number','TELESCOPE_INFO','Scope Properties',GroupOpt,isOk,ipRW);
    AddDef('Number','TELESCOPE_APERTURE','Aperture (mm)','100',
           ' format="%g" min="10" max="5000" step="0"');
    AddDef('Number','TELESCOPE_FOCAL_LENGTH','Focal Length (mm)',
           F(strtofloat2(form1.focallength1.caption),1),
           ' format="%g" min="10" max="10000" step="0"');
    AddDef('Number','GUIDER_APERTURE','Guider Aperture (mm)','50',
           ' format="%g" min="10" max="5000" step="0"');
    AddDef('Number','GUIDER_FOCAL_LENGTH','Guider Focal Length (mm)',
            F(strtofloat2(form1.focallength1.caption),1),     //same as main telescope
            ' format="%g" min="10" max="10000" step="0"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','TELESCOPE_INFO',isOk);
    AddOne('Number','TELESCOPE_APERTURE','100');
    AddOne('Number','TELESCOPE_FOCAL_LENGTH',
           F(strtofloat2(form1.focallength1.caption),1));
    AddOne('Number','GUIDER_APERTURE','50');
    AddOne('Number','GUIDER_FOCAL_LENGTH',F(strtofloat2(form1.focallength1.caption),1));
    EndSet('Number');
  end;
end;

procedure T_Indi_Mount.SendGuideNS(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','TELESCOPE_TIMED_GUIDE_NS','Guide N/S',
             GroupMotion,st,ipRW);
    AddDef('Number','TIMED_GUIDE_N','North (ms)','0',
           ' format="%g" min="0" max="60000" step="10"');
    AddDef('Number','TIMED_GUIDE_S','South (ms)','0',
           ' format="%g" min="0" max="60000" step="10"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','TELESCOPE_TIMED_GUIDE_NS',st);
    AddOne('Number','TIMED_GUIDE_N','0');
    AddOne('Number','TIMED_GUIDE_S','0');
    EndSet('Number');
  end;
end;

procedure T_Indi_Mount.SendGuideWE(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','TELESCOPE_TIMED_GUIDE_WE','Guide E/W',
             GroupMotion,st,ipRW);
    AddDef('Number','TIMED_GUIDE_W','West (ms)','0',
           ' format="%g" min="0" max="60000" step="10"');
    AddDef('Number','TIMED_GUIDE_E','East (ms)','0',
           ' format="%g" min="0" max="60000" step="10"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','TELESCOPE_TIMED_GUIDE_WE',st);
    AddOne('Number','TIMED_GUIDE_W','0');
    AddOne('Number','TIMED_GUIDE_E','0');
    EndSet('Number');
  end;
end;

{ ---------------- definition dump --------------------------------------- }

procedure T_Indi_Mount.DefineDeviceProperties;
{Everything except CONNECTION and DRIVER_INFO.  A libindi driver defines
 these from updateProperties(), i.e. only AFTER the client has connected.
 Called on getProperties and again on CONNECT; re-defining a property is
 legal INDI, the client simply replaces its copy.}
var st: TIndiState;
begin
  if indi_slewing then st:=isBusy else st:=isOk;

  SendEqCoord(true,st);
  SendOnCoordSet(true);
  SendTrackState(true);
  SendAbort(true,isIdle);
  SendPark(true,isIdle);
  SendPierSide(true);
  SendGeographic(true,isOk);
  SendTimeUtc(true);
  SendTelescopeInfo(true);
  SendGuideNS(true,isIdle);
  SendGuideWE(true,isIdle);
end;

procedure T_Indi_Mount.DefineAll;
begin
  SendConnection(true);
  SendDriverInfo;
  DefineDeviceProperties;
end;

{ ---------------- command handling -------------------------------------- }

procedure T_Indi_Mount.HandleElement(e: TIndiElement);
begin
  if e=nil then exit;
  if e.Tag='newSwitchVector' then DoSwitch(e)
  else if e.Tag='newNumberVector' then DoNumber(e)
  else if e.Tag='newTextVector' then DoText(e);
  {enableBLOB is accepted and ignored: a mount sends no BLOBs}
end;


procedure T_Indi_Mount.DoSwitch(e: TIndiElement);
var
  nm, sw: string;
begin
  nm:=e.A('name');
  sw:=e.SwitchOn;

  if nm='CONNECTION' then
  begin
    FConnected:=(sw='CONNECT');
    SendConnection(false);              {Ok + CONNECT On, or Idle + Off}
    if FConnected then
      DefineDeviceProperties            {as libindi updateProperties()}
    else
      LogMessage('Sky Simulator stays available while the INDI server runs');
    exit;
  end;

  if nm='ON_COORD_SET' then
  begin
    if sw<>'' then FCoordAction:=sw;
    SendOnCoordSet(false);
    exit;
  end;

  if nm='TELESCOPE_TRACK_STATE' then
  begin
    if sw='TRACK_ON'  then indi_set_tracking(true);
    if sw='TRACK_OFF' then indi_set_tracking(false);
    SendTrackState(false);
    FLastTrack:=indi_tracking;
    exit;
  end;

  if nm='TELESCOPE_ABORT_MOTION' then
  begin
    indi_abort;
    SendAbort(false,isOk);
    SendEqCoord(false,isIdle);
    FLastEqState:=isIdle;
    LogMessage('Motion aborted');
    exit;
  end;

  if nm='TELESCOPE_PARK' then
  begin
    if sw='PARK' then
    begin
      indi_park;
      SendPark(false,isBusy);
      LogMessage('Parking');
    end
    else if sw='UNPARK' then
    begin
      indi_unpark;
      SendPark(false,isOk);
      LogMessage('Unparked');
    end;
    FLastPark:=indi_parked;
    exit;
  end;
end;


procedure T_Indi_Mount.DoNumber(e: TIndiElement);
var
  nm: string;
  ra, de, la, lo: double;
  gN, gS, gE, gW: integer;
  ok: boolean;
begin
  nm:=e.A('name');

  if nm='EQUATORIAL_EOD_COORD' then
  begin
    if not (e.HasChild('RA') and e.HasChild('DEC')) then exit;
    ra:=e.ChildNum('RA',0);
    de:=e.ChildNum('DEC',0);

    if FCoordAction='SYNC' then
    begin
      indi_sync(ra,de,ok);
      if ok then
      begin
        SendEqCoord(false,isOk);
        FLastEqState:=isOk;
        LogMessage('Synced');
      end
      else
      begin
        SendEqCoord(false,isAlert);
        FLastEqState:=isAlert;
      end;
    end
    else
    begin
      indi_track_on_arrival:=(FCoordAction='TRACK');
      indi_slew(ra,de,ok);
      if ok then
      begin
        SendEqCoord(false,isBusy);
        FLastEqState:=isBusy;
      end
      else
      begin
        SendEqCoord(false,isAlert);
        FLastEqState:=isAlert;
        LogMessage('Slew refused');
      end;
    end;
    exit;
  end;

  if nm='GEOGRAPHIC_COORD' then
  begin
    la:=e.ChildNum('LAT',strtofloat2(form1.latitude1.text));
    lo:=e.ChildNum('LONG',strtofloat2(form1.longitude1.text));
    indi_site_elevation:=e.ChildNum('ELEV',indi_site_elevation);
    if lo>180 then lo:=lo-360;            {back to the ASCOM convention}

    form1.latitude1.text :=FloatToStrF(la,ffFixed,0,4);
    form1.longitude1.text:=FloatToStrF(lo,ffFixed,0,4);
    form1.latitude1Exit(nil);             {same recalculation as the UI does}

    SendGeographic(false,isOk);
    exit;
  end;

  if (nm='TELESCOPE_TIMED_GUIDE_NS') or (nm='TELESCOPE_TIMED_GUIDE_WE') then
  begin
    gN:=round(e.ChildNum('TIMED_GUIDE_N',0));
    gS:=round(e.ChildNum('TIMED_GUIDE_S',0));
    gE:=round(e.ChildNum('TIMED_GUIDE_E',0));
    gW:=round(e.ChildNum('TIMED_GUIDE_W',0));
    indi_pulseguide(gN,gS,gE,gW);
    if nm='TELESCOPE_TIMED_GUIDE_NS' then SendGuideNS(false,isOk)
                                     else SendGuideWE(false,isOk);
    exit;
  end;

  if nm='TELESCOPE_INFO' then
  begin
    if e.HasChild('TELESCOPE_FOCAL_LENGTH') then
      form1.focallength1.caption:=
        F(e.ChildNum('TELESCOPE_FOCAL_LENGTH',
                     strtofloat2(form1.focallength1.caption)),1);
    SendTelescopeInfo(false);
    exit;
  end;
end;

procedure T_Indi_Mount.DoText(e: TIndiElement);
begin
  if e.A('name')='TIME_UTC' then
  begin
    {the simulator runs its own clock, acknowledge but do not apply}
    SendTimeUtc(false);
    LogMessage('Simulator uses its own time base, TIME_UTC ignored');
  end;
end;

{ ---------------- periodic push ----------------------------------------- }

procedure T_Indi_Mount.Poll;
var
  ra, de, la, lo: double;
  st: TIndiState;
begin
  indi_get_position(ra,de);
  if indi_slewing then st:=isBusy else st:=isOk;

  if (abs(ra-FLastRa)>1.0e-6) or (abs(de-FLastDec)>1.0e-5) or
     (st<>FLastEqState) then
  begin
    SendEqCoord(false,st);
    FLastRa:=ra; FLastDec:=de; FLastEqState:=st;
  end;

  if indi_tracking<>FLastTrack then
  begin
    SendTrackState(false);
    FLastTrack:=indi_tracking;
  end;

  if indi_parked<>FLastPark then
  begin
    case indi_parked of
      2: begin SendPark(false,isOk); LogMessage('Parked'); end;
      1: SendPark(false,isBusy);
    else SendPark(false,isIdle);
    end;
    FLastPark:=indi_parked;
  end;

  if indi_sideofpier<>FLastPier then
  begin
    SendPierSide(false);
    FLastPier:=indi_sideofpier;
  end;

  la:=strtofloat2(form1.latitude1.text);
  lo:=strtofloat2(form1.longitude1.text);
  if (abs(la-FLastLat)>1.0e-6) or (abs(lo-FLastLon)>1.0e-6) or
     (abs(indi_site_elevation-FLastElev)>0.01) then
  begin
    SendGeographic(false,isOk);
    FLastLat:=la; FLastLon:=lo; FLastElev:=indi_site_elevation;
  end;
end;

end.
