unit indi_rotator;

{$mode objfpc}{$H+}

{
INDI rotator simulation and protocol for Sky Simulator.

Copyright (C) 2026 Han Kleijn
https://sourceforge.net/projects/sky-simulator

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

--------------------------------------------------------------------------
THIRD ROTATOR OPTION

sky_simulator_main offers the user a choice of rotator:

   1) external ASCOM rotator over COM        (Windows only)
   2) internal Alpaca rotator                alpaca_rotator_protocol.pas
   3) internal INDI rotator                  THIS UNIT

Only one is active at a time, so this unit carries its OWN complete
rotator state and its own copy of the simulation step, renamed to the
indi_ prefix.  Nothing is shared with alpaca_rotator_protocol.  Same
arrangement as indi_mount.pas next to alpaca_mount_protocol.pas.

sky_simulator_main must call indi_rotator_simulation from Timer1Timer
when the INDI rotator is selected, and must read
indi_rot_position_synced / indi_rot_position_target_synced /
indi_rot_reverse in the rotator block instead of the alpaca_ variables,
otherwise the simulated field never rotates.

MECHANICAL VERSUS SYNCED  (the trap)
------------------------------------
The Alpaca unit keeps two angles and so does this one:

  indi_rot_position         mechanical angle of the hardware
  indi_rot_position_synced  what the sky sees = mechanical + sync offset

INDI's ABS_ROTATOR_ANGLE is the SKY angle, i.e. the synced one - the
same thing ASCOM calls Position, not MechanicalPosition.  Only the
simulation and the abort work on the mechanical angle.  Collapsing the
two into one value would make SYNC_ROTATOR_ANGLE silently do nothing,
so the distinction is copied here rather than simplified away.

All angles are integer degrees 0..360, as in the Alpaca unit.  INDI
sends and expects floating point degrees, so the conversion happens at
the property boundary only.

TRANSPORT
---------
Sockets, threads and XML live in indi_server.pas.  This unit only
implements the device and needs no change there: registration is one
line, RegisterIndiDevice(T_Indi_Rotator).
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, math, indi_server;

const
  IndiRotatorName = 'Sky Simulator Rotator';  {device name a client shows}

var
  {---- rotator state, the simulated hardware ----}
  indi_rot_position               : integer = 0;  {mechanical}
  indi_rot_position_synced        : integer = 0;  {synced, what the sky sees}
  indi_rot_position_target        : integer = 0;  {mechanical}
  indi_rot_position_target_synced : integer = 0;  {synced}
  indi_rot_temperature            : double  = 20;
  indi_rot_reverse                : boolean = false;
  indi_rot_sync_offset            : integer = 0;

{ ---- simulation ------------------------------------------------------- }
procedure indi_rotator_simulation;            {call from Timer1Timer, 1 Hz}

{ ---- commands --------------------------------------------------------- }
function  indi_rotator_position: integer;     {synced / sky angle, degrees}
procedure indi_rotator_moveabsolute(thepos: double);  {synced angle}
procedure indi_rotator_sync(thepos: double);          {synced angle}
procedure indi_rotator_abort;
function  indi_rotator_moving: boolean;

type
{ The INDI device.  One instance per connected client, created by
  indi_server; all three methods run in the main thread. }
  T_Indi_Rotator = class(TIndiDevice)
  private
    {cache of what was last pushed to THIS client}
    FLastAngle  : integer;
    FLastState  : TIndiState;
    FLastReverse: boolean;

    FConnected : boolean;   {protocol state, not hardware state}

    procedure DefineDeviceProperties;   {libindi updateProperties() equivalent}
    procedure SendConnection(def: boolean);
    procedure SendDriverInfo;
    procedure SendConfigProcess(def: boolean);
    procedure SendAngle(def: boolean; st: TIndiState);
    procedure SendSyncAngle(def: boolean; st: TIndiState);
    procedure SendAbort(def: boolean; st: TIndiState);
    procedure SendReverse(def: boolean);

    procedure DoSwitch(e: TIndiElement);
    procedure DoNumber(e: TIndiElement);
  public
    constructor Create; override;
    procedure DefineAll; override;
    procedure HandleElement(e: TIndiElement); override;
    procedure Poll; override;
  end;

implementation

uses
  sky_simulator_main;          {memo2_message}

const
  GroupMain = 'Main Control';
  GroupOpt  = 'Options';
  GroupInfo = 'General Info';

{ ====================================================================== }
{ Rotator simulation.                                                     }
{ Copy of the Alpaca simulation with indi_ variables, so this drive is    }
{ fully independent of alpaca_rotator_protocol.  Note that the movement   }
{ is linear in the mechanical angle and does NOT take the short way round }
{ through 0/360 - the Alpaca unit behaves the same way and the two must   }
{ stay comparable.                                                        }
{ ====================================================================== }

procedure indi_rotator_simulation;
{Called every second by Timer1 while the INDI rotator is selected.}
var
  deltaPos, step_size: integer;
begin
  {mechanical angle first}
  deltaPos:=indi_rot_position_target-indi_rot_position;

  step_size:=min(abs(deltaPos),30);           {30 degrees per second}
  if deltaPos<0 then
    indi_rot_position:=indi_rot_position-step_size
  else
    indi_rot_position:=indi_rot_position+step_size;

  {keep the synced angle in step, this is what the rest of the program
   and every INDI client reads}
  indi_rot_position_synced:=indi_rot_position+indi_rot_sync_offset;
  while indi_rot_position_synced>=360 do
    indi_rot_position_synced:=indi_rot_position_synced-360;
  while indi_rot_position_synced<0 do
    indi_rot_position_synced:=indi_rot_position_synced+360;
end;

function indi_rotator_position: integer;
{synced (sky) angle, 0..359}
begin
  Result:=indi_rot_position+indi_rot_sync_offset;
  while Result>=360 do Result:=Result-360;
  while Result<0    do Result:=Result+360;
  indi_rot_position_synced:=Result;
end;

function indi_rotator_moving: boolean;
begin
  Result:=abs(indi_rot_position_target-indi_rot_position)>=1;
end;

procedure indi_rotator_moveabsolute(thepos: double);
{thepos is the SKY angle; convert to a mechanical target.}
begin
  indi_rot_position_target_synced:=round(thepos);
  while indi_rot_position_target_synced>=360 do
    indi_rot_position_target_synced:=indi_rot_position_target_synced-360;
  while indi_rot_position_target_synced<0 do
    indi_rot_position_target_synced:=indi_rot_position_target_synced+360;

  indi_rot_position_target:=indi_rot_position_target_synced-
                            indi_rot_sync_offset;
  while indi_rot_position_target>=360 do
    indi_rot_position_target:=indi_rot_position_target-360;
  while indi_rot_position_target<0 do
    indi_rot_position_target:=indi_rot_position_target+360;

  memo2_message('INDI rotator to '+IntToStr(indi_rot_position_target_synced)+
                ' degrees.');
end;

procedure indi_rotator_sync(thepos: double);
{Redefine the sky angle at the current mechanical angle.}
begin
  indi_rot_sync_offset:=round(thepos)-indi_rot_position;
  indi_rot_position_synced:=indi_rotator_position;
  {the target must follow, otherwise the rotator would start moving
   just because it was synced}
  indi_rot_position_target_synced:=indi_rot_position_target+
                                   indi_rot_sync_offset;
  memo2_message('INDI rotator sync to '+IntToStr(round(thepos))+' degrees.');
end;

procedure indi_rotator_abort;
begin
  indi_rot_position_target:=indi_rot_position;
  indi_rot_position_target_synced:=indi_rotator_position;
  memo2_message('INDI rotator abort.');
end;

{ ====================================================================== }
{ T_Indi_Rotator                                                          }
{ ====================================================================== }

constructor T_Indi_Rotator.Create;
begin
  inherited Create;
  DeviceName:=IndiRotatorName;
  FLastAngle:=-999;                   {force a first push}
  FLastState:=isIdle;
  FLastReverse:=not indi_rot_reverse;
  FConnected:=false;
end;

{ ---------------- property senders ------------------------------------- }

procedure T_Indi_Rotator.SendConnection(def: boolean);
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

procedure T_Indi_Rotator.SendDriverInfo;
begin
  StartDef('Text','DRIVER_INFO','Driver Info',GroupInfo,isIdle,ipRO);
  AddDef('Text','DRIVER_NAME','Name',DeviceName);
  AddDef('Text','DRIVER_EXEC','Exec','indi_skysimulator');
  AddDef('Text','DRIVER_VERSION','Version','1.0');
  AddDef('Text','DRIVER_INTERFACE','Interface','4096'); {4096 = ROTATOR_INTERFACE, 1 shl 12}
  EndDef('Text');
end;

procedure T_Indi_Rotator.SendConfigProcess(def: boolean);
{See the long note in indi_camera.pas: CCDciel times out with "Missing
 property CONFIG_PROCESS" on any device that does not define it, and no
 documentation says so because libindi drivers inherit it from
 INDI::DefaultDevice.  Acknowledged, does nothing.}
begin
  if def then
  begin
    StartDef('Switch','CONFIG_PROCESS','Configuration',GroupOpt,isIdle,ipRW,
             ' rule="AtMostOne"');
    AddDef('Switch','CONFIG_LOAD','Load','Off');
    AddDef('Switch','CONFIG_SAVE','Save','Off');
    AddDef('Switch','CONFIG_DEFAULT','Default','Off');
    AddDef('Switch','CONFIG_PURGE','Purge','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CONFIG_PROCESS',isOk);
    AddOne('Switch','CONFIG_LOAD','Off');
    AddOne('Switch','CONFIG_SAVE','Off');
    AddOne('Switch','CONFIG_DEFAULT','Off');
    AddOne('Switch','CONFIG_PURGE','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_Rotator.SendAngle(def: boolean; st: TIndiState);
{ABS_ROTATOR_ANGLE is the SKY angle, see the header note.}
begin
  if def then
  begin
    StartDef('Number','ABS_ROTATOR_ANGLE','Angle',GroupMain,st,ipRW);
    AddDef('Number','ANGLE','Degrees',F(indi_rotator_position,2),
           ' format="%.2f" min="0" max="360" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','ABS_ROTATOR_ANGLE',st);
    AddOne('Number','ANGLE',F(indi_rotator_position,2));
    EndSet('Number');
  end;
end;

procedure T_Indi_Rotator.SendSyncAngle(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','SYNC_ROTATOR_ANGLE','Sync',GroupOpt,st,ipRW);
    AddDef('Number','ANGLE','Degrees',F(indi_rotator_position,2),
           ' format="%.2f" min="0" max="360" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','SYNC_ROTATOR_ANGLE',st);
    AddOne('Number','ANGLE',F(indi_rotator_position,2));
    EndSet('Number');
  end;
end;

procedure T_Indi_Rotator.SendAbort(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Switch','ROTATOR_ABORT_MOTION','Abort Motion',GroupMain,
             isIdle,ipRW,' rule="AtMostOne"');
    AddDef('Switch','ABORT','Abort','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','ROTATOR_ABORT_MOTION',st);
    AddOne('Switch','ABORT','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_Rotator.SendReverse(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','ROTATOR_REVERSE','Reverse',GroupOpt,isOk,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','INDI_ENABLED','Enabled',OnOff(indi_rot_reverse));
    AddDef('Switch','INDI_DISABLED','Disabled',OnOff(not indi_rot_reverse));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','ROTATOR_REVERSE',isOk);
    AddOne('Switch','INDI_ENABLED',OnOff(indi_rot_reverse));
    AddOne('Switch','INDI_DISABLED',OnOff(not indi_rot_reverse));
    EndSet('Switch');
  end;
end;

{ ---------------- definition dump --------------------------------------- }

procedure T_Indi_Rotator.DefineDeviceProperties;
{Everything except CONNECTION and DRIVER_INFO.  A libindi driver defines
 these from updateProperties(), i.e. only AFTER the client has connected.
 Called on getProperties and again on CONNECT; re-defining a property is
 legal INDI, the client simply replaces its copy.}
var st: TIndiState;
begin
  if indi_rotator_moving then st:=isBusy else st:=isOk;

  SendConfigProcess(true);
  SendAngle(true,st);
  SendSyncAngle(true,isOk);
  SendAbort(true,isIdle);
  SendReverse(true);
end;

procedure T_Indi_Rotator.DefineAll;
begin
  SendConnection(true);
  SendDriverInfo;
  DefineDeviceProperties;
end;

{ ---------------- command handling -------------------------------------- }

procedure T_Indi_Rotator.HandleElement(e: TIndiElement);
begin
  if e=nil then exit;
  if e.Tag='newSwitchVector' then DoSwitch(e)
  else if e.Tag='newNumberVector' then DoNumber(e);
  {enableBLOB is accepted and ignored: a rotator sends no BLOBs}
end;

procedure T_Indi_Rotator.DoSwitch(e: TIndiElement);
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

  if nm='CONFIG_PROCESS' then
  begin
    SendConfigProcess(false);
    if sw='CONFIG_SAVE' then
      LogMessage('Settings are kept in the Sky Simulator ini file');
    exit;
  end;

  if nm='ROTATOR_ABORT_MOTION' then
  begin
    indi_rotator_abort;
    SendAbort(false,isOk);
    SendAngle(false,isOk);          {report the stop at once}
    FLastAngle:=indi_rotator_position; FLastState:=isOk;
    LogMessage('Rotator motion aborted');
    exit;
  end;

  if nm='ROTATOR_REVERSE' then
  begin
    indi_rot_reverse:=(sw='INDI_ENABLED');
    SendReverse(false);
    FLastReverse:=indi_rot_reverse;
    exit;
  end;
end;

procedure T_Indi_Rotator.DoNumber(e: TIndiElement);
var
  nm: string;
  v : double;
begin
  nm:=e.A('name');

  if nm='ABS_ROTATOR_ANGLE' then
  begin
    if not e.HasChild('ANGLE') then exit;
    v:=e.ChildNum('ANGLE',indi_rotator_position);
    indi_rotator_moveabsolute(v);
    {Busy now, Ok on arrival from Poll: this is what drives the client's
     progress display}
    SendAngle(false,isBusy);
    FLastState:=isBusy;
    exit;
  end;

  if nm='SYNC_ROTATOR_ANGLE' then
  begin
    if not e.HasChild('ANGLE') then exit;
    v:=e.ChildNum('ANGLE',indi_rotator_position);
    indi_rotator_sync(v);
    SendSyncAngle(false,isOk);
    SendAngle(false,isOk);          {the reported angle changed at once}
    FLastAngle:=indi_rotator_position; FLastState:=isOk;
    exit;
  end;
end;

{ ---------------- periodic push ----------------------------------------- }

procedure T_Indi_Rotator.Poll;
{Runs a few times per second on EVERY connection, so only push what
 actually changed.}
var
  st : TIndiState;
  ang: integer;
begin
  ang:=indi_rotator_position;
  if indi_rotator_moving then st:=isBusy else st:=isOk;

  if (ang<>FLastAngle) or (st<>FLastState) then
  begin
    SendAngle(false,st);
    FLastAngle:=ang;
    FLastState:=st;
  end;

  {reverse can also be changed from the Sky Simulator form or by another
   client, so mirror it here}
  if indi_rot_reverse<>FLastReverse then
  begin
    SendReverse(false);
    FLastReverse:=indi_rot_reverse;
  end;
end;

end.
