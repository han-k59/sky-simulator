unit indi_focuser;

{$mode objfpc}{$H+}

{
INDI focuser simulation and protocol for Sky Simulator.

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
THIRD FOCUSER OPTION

sky_simulator_main offers the user a choice of focuser:

   1) external ASCOM focuser over COM        (Windows only)
   2) internal Alpaca focuser                alpaca_focuser_protocol.pas
   3) internal INDI focuser                  THIS UNIT

Only one is active at a time, so this unit carries its OWN complete
focuser state: its own position, its own setpoint, its own temperature
and its own copy of the simulation step.  Nothing is shared with
alpaca_focuser_protocol, which means the two drives can be changed
independently without any risk of one breaking the other.  This is
exactly the arrangement used by indi_mount.pas next to
alpaca_mount_protocol.pas, and by indi_camera.pas next to
alpaca_camera_protocol.pas.

The simulation below is a copy of focuser_simulation from
alpaca_focuser_protocol.pas with the variables renamed to the indi_
prefix.  Behaviour is identical: 100 steps per timer tick.

sky_simulator_main must call indi_focuser_simulation from Timer1Timer
when the INDI focuser is selected, in the same place where
focuser_simulation is called for the Alpaca focuser.  It must also read
indi_foc_position instead of alpaca_foc_position in the FOCUSER
PROCESSING block, otherwise the simulated star image never notices that
the INDI focuser moved.

TEMPERATURE  (small deliberate difference)
------------------------------------------
The Alpaca unit ramps alpaca_foc_temperature inside its temperature
GETTER, so the ramp speed depends on how often the client polls.  That
cannot be copied literally here: Poll runs a few times per second on
every connection, so the same trick would race up and down with the
number of clients.  The ramp therefore lives in the simulation step,
which runs exactly once per second regardless of how many clients are
attached.  The visible behaviour (5..7 degrees, sawtooth) is the same.

TRANSPORT
---------
Sockets, threads and XML live in indi_server.pas.  This unit only
implements the device and needs no change there: registration is one
line, RegisterIndiDevice(T_Indi_Focuser).
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, math, indi_server;

const
  IndiFocuserName = 'Sky Simulator Focuser';  {device name a client shows}

var
  {---- focuser state, the simulated hardware ----}
  indi_foc_position    : integer = 25000;   {current position, steps}
  indi_foc_setpoint    : integer = 25000;   {where it is heading}
  indi_foc_temperature : double  = 5;       {degrees C}
  indi_foc_max         : integer = 65535;   {FOCUS_MAX, writable}

  {FOCUS_MOTION is device state, not per client state: every client must
   see the same direction, so it lives here and not in a private field.}
  indi_foc_inward      : boolean = false;   {true = FOCUS_INWARD selected}
  indi_foc_reverse     : boolean = false;   {FOCUS_REVERSE_MOTION}

{ ---- simulation ------------------------------------------------------- }
procedure indi_focuser_simulation;          {call from Timer1Timer, 1 Hz}

{ ---- commands --------------------------------------------------------- }
procedure indi_focuser_moveto(x: integer);  {absolute}
procedure indi_focuser_moverel(steps: integer); {relative, sign from motion}
procedure indi_focuser_sync(x: integer);
procedure indi_focuser_abort;
function  indi_focuser_moving: boolean;

type
{ The INDI device.  One instance per connected client, created by
  indi_server; all three methods run in the main thread. }
  T_Indi_Focuser = class(TIndiDevice)
  private
    {cache of what was last pushed to THIS client}
    FLastPos    : integer;
    FLastTemp   : double;
    FLastMax    : integer;
    FLastState  : TIndiState;
    FLastInward : boolean;
    FLastReverse: boolean;

    FConnected : boolean;   {protocol state, not hardware state}

    procedure DefineDeviceProperties;   {libindi updateProperties() equivalent}
    procedure SendConnection(def: boolean);
    procedure SendDriverInfo;
    procedure SendConfigProcess(def: boolean);
    procedure SendAbsPosition(def: boolean; st: TIndiState);
    procedure SendRelPosition(def: boolean; st: TIndiState);
    procedure SendMotion(def: boolean);
    procedure SendAbort(def: boolean; st: TIndiState);
    procedure SendMax(def: boolean);
    procedure SendReverse(def: boolean);
    procedure SendTemperature(def: boolean);
    procedure SendSync(def: boolean; st: TIndiState);

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
{ Focuser simulation.                                                     }
{ Copy of the Alpaca simulation with indi_ variables, so this drive is    }
{ fully independent of alpaca_focuser_protocol.                           }
{ ====================================================================== }

procedure indi_focuser_simulation;
{Called every second by Timer1 while the INDI focuser is selected.}
var
  deltaPos, stepsize: integer;
begin
  deltaPos:=indi_foc_setpoint-indi_foc_position;

  stepsize:=min(abs(deltaPos),100);            {100 steps per second}
  if deltaPos<0 then
    indi_foc_position:=indi_foc_position-stepsize
  else
    indi_foc_position:=indi_foc_position+stepsize;

  {temperature sawtooth, see the note in the unit header}
  indi_foc_temperature:=indi_foc_temperature+0.5;
  if indi_foc_temperature>7 then indi_foc_temperature:=5;
end;

function indi_focuser_moving: boolean;
begin
  Result:=abs(indi_foc_setpoint-indi_foc_position)>=1;
end;

procedure indi_focuser_moveto(x: integer);
begin
  if x<0 then x:=0;
  if x>indi_foc_max then x:=indi_foc_max;
  indi_foc_setpoint:=x;
end;

procedure indi_focuser_moverel(steps: integer);
{REL_FOCUS_POSITION carries a magnitude only; the sign comes from the
 FOCUS_MOTION switch, and FOCUS_REVERSE_MOTION flips what inward means.
 Relative moves start from the SETPOINT, not from the current position:
 an autofocus run fires several relative moves in a row without waiting
 for each to finish, and measuring from the moving position would lose
 steps.}
var
  inward: boolean;
begin
  inward:=indi_foc_inward;
  if indi_foc_reverse then inward:=not inward;
  if inward then
    indi_focuser_moveto(indi_foc_setpoint-abs(steps))
  else
    indi_focuser_moveto(indi_foc_setpoint+abs(steps));
end;

procedure indi_focuser_sync(x: integer);
{Redefine the current position without moving anything.}
begin
  if x<0 then x:=0;
  if x>indi_foc_max then x:=indi_foc_max;
  indi_foc_position:=x;
  indi_foc_setpoint:=x;
  memo2_message('INDI focuser sync to '+IntToStr(x));
end;

procedure indi_focuser_abort;
begin
  indi_foc_setpoint:=indi_foc_position;
  memo2_message('INDI focuser abort.');
end;

{ ====================================================================== }
{ T_Indi_Focuser                                                          }
{ ====================================================================== }

constructor T_Indi_Focuser.Create;
begin
  inherited Create;
  DeviceName:=IndiFocuserName;
  FLastPos:=-1;                      {force a first push}
  FLastTemp:=-999;
  FLastMax:=-1;
  FLastState:=isIdle;
  FLastInward:=not indi_foc_inward;
  FLastReverse:=not indi_foc_reverse;
  FConnected:=false;
end;

{ ---------------- property senders ------------------------------------- }

procedure T_Indi_Focuser.SendConnection(def: boolean);
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

procedure T_Indi_Focuser.SendDriverInfo;
begin
  StartDef('Text','DRIVER_INFO','Driver Info',GroupInfo,isIdle,ipRO);
  AddDef('Text','DRIVER_NAME','Name',DeviceName);
  AddDef('Text','DRIVER_EXEC','Exec','indi_skysimulator');
  AddDef('Text','DRIVER_VERSION','Version','1.0');
  AddDef('Text','DRIVER_INTERFACE','Interface','8');  {8 = FOCUSER_INTERFACE}
  EndDef('Text');
end;

procedure T_Indi_Focuser.SendConfigProcess(def: boolean);
{CCDciel expects this vector on every device it drives, including the
 focuser, and simply times out after a minute with "Missing property
 CONFIG_PROCESS" if it never arrives.  Every real libindi driver gets it
 for free from INDI::DefaultDevice, so nothing advertises that a hand
 written driver has to supply it.  See the same note in indi_camera.pas.

 The simulator has no configuration file: all settings live in the Sky
 Simulator ini and in the form.  So the property exists to satisfy the
 client and the buttons are acknowledged without doing anything.}
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
    {always report all four Off again: this is a momentary vector}
    StartSet('Switch','CONFIG_PROCESS',isOk);
    AddOne('Switch','CONFIG_LOAD','Off');
    AddOne('Switch','CONFIG_SAVE','Off');
    AddOne('Switch','CONFIG_DEFAULT','Off');
    AddOne('Switch','CONFIG_PURGE','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_Focuser.SendAbsPosition(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','ABS_FOCUS_POSITION','Absolute Position',
             GroupMain,st,ipRW);
    AddDef('Number','FOCUS_ABSOLUTE_POSITION','Ticks',
           IntToStr(indi_foc_position),
           ' format="%.0f" min="0" max="'+IntToStr(indi_foc_max)+
           '" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','ABS_FOCUS_POSITION',st);
    AddOne('Number','FOCUS_ABSOLUTE_POSITION',IntToStr(indi_foc_position));
    EndSet('Number');
  end;
end;

procedure T_Indi_Focuser.SendRelPosition(def: boolean; st: TIndiState);
var rel: integer;
begin
  {report the remaining distance, so a client that watches the relative
   vector sees it run down to zero}
  rel:=abs(indi_foc_setpoint-indi_foc_position);
  if def then
  begin
    StartDef('Number','REL_FOCUS_POSITION','Relative Position',
             GroupMain,st,ipRW);
    AddDef('Number','FOCUS_RELATIVE_POSITION','Ticks',IntToStr(rel),
           ' format="%.0f" min="0" max="'+IntToStr(indi_foc_max)+
           '" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','REL_FOCUS_POSITION',st);
    AddOne('Number','FOCUS_RELATIVE_POSITION',IntToStr(rel));
    EndSet('Number');
  end;
end;

procedure T_Indi_Focuser.SendMotion(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','FOCUS_MOTION','Direction',GroupMain,isOk,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','FOCUS_INWARD','Focus In',OnOff(indi_foc_inward));
    AddDef('Switch','FOCUS_OUTWARD','Focus Out',OnOff(not indi_foc_inward));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','FOCUS_MOTION',isOk);
    AddOne('Switch','FOCUS_INWARD',OnOff(indi_foc_inward));
    AddOne('Switch','FOCUS_OUTWARD',OnOff(not indi_foc_inward));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Focuser.SendAbort(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Switch','FOCUS_ABORT_MOTION','Abort Motion',GroupMain,
             isIdle,ipRW,' rule="AtMostOne"');
    AddDef('Switch','ABORT','Abort','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','FOCUS_ABORT_MOTION',st);
    AddOne('Switch','ABORT','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_Focuser.SendMax(def: boolean);
begin
  if def then
  begin
    StartDef('Number','FOCUS_MAX','Max. Position',GroupOpt,isOk,ipRW);
    AddDef('Number','FOCUS_MAX_VALUE','Ticks',IntToStr(indi_foc_max),
           ' format="%.0f" min="1" max="1000000" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','FOCUS_MAX',isOk);
    AddOne('Number','FOCUS_MAX_VALUE',IntToStr(indi_foc_max));
    EndSet('Number');
  end;
end;

procedure T_Indi_Focuser.SendReverse(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','FOCUS_REVERSE_MOTION','Reverse',GroupOpt,isOk,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','INDI_ENABLED','Enabled',OnOff(indi_foc_reverse));
    AddDef('Switch','INDI_DISABLED','Disabled',OnOff(not indi_foc_reverse));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','FOCUS_REVERSE_MOTION',isOk);
    AddOne('Switch','INDI_ENABLED',OnOff(indi_foc_reverse));
    AddOne('Switch','INDI_DISABLED',OnOff(not indi_foc_reverse));
    EndSet('Switch');
  end;
end;

procedure T_Indi_Focuser.SendTemperature(def: boolean);
begin
  if def then
  begin
    StartDef('Number','FOCUS_TEMPERATURE','Temperature',GroupMain,isOk,ipRO);
    AddDef('Number','TEMPERATURE','Celsius',F(indi_foc_temperature,1),
           ' format="%.1f" min="-50" max="70" step="0"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','FOCUS_TEMPERATURE',isOk);
    AddOne('Number','TEMPERATURE',F(indi_foc_temperature,1));
    EndSet('Number');
  end;
end;

procedure T_Indi_Focuser.SendSync(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','FOCUS_SYNC','Sync',GroupOpt,st,ipRW);
    AddDef('Number','FOCUS_SYNC_VALUE','Ticks',IntToStr(indi_foc_position),
           ' format="%.0f" min="0" max="'+IntToStr(indi_foc_max)+
           '" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','FOCUS_SYNC',st);
    AddOne('Number','FOCUS_SYNC_VALUE',IntToStr(indi_foc_position));
    EndSet('Number');
  end;
end;

{ ---------------- definition dump --------------------------------------- }

procedure T_Indi_Focuser.DefineDeviceProperties;
{Everything except CONNECTION and DRIVER_INFO.  A libindi driver defines
 these from updateProperties(), i.e. only AFTER the client has connected.
 Called on getProperties and again on CONNECT; re-defining a property is
 legal INDI, the client simply replaces its copy.}
var st: TIndiState;
begin
  if indi_focuser_moving then st:=isBusy else st:=isOk;

  SendConfigProcess(true);
  SendAbsPosition(true,st);
  SendRelPosition(true,st);
  SendMotion(true);
  SendAbort(true,isIdle);
  SendMax(true);
  SendReverse(true);
  SendTemperature(true);
  SendSync(true,isOk);
end;

procedure T_Indi_Focuser.DefineAll;
begin
  SendConnection(true);
  SendDriverInfo;
  DefineDeviceProperties;
end;

{ ---------------- command handling -------------------------------------- }

procedure T_Indi_Focuser.HandleElement(e: TIndiElement);
begin
  if e=nil then exit;
  if e.Tag='newSwitchVector' then DoSwitch(e)
  else if e.Tag='newNumberVector' then DoNumber(e);
  {enableBLOB is accepted and ignored: a focuser sends no BLOBs}
end;

procedure T_Indi_Focuser.DoSwitch(e: TIndiElement);
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
    {nothing to load or save; acknowledge so the client does not hang}
    SendConfigProcess(false);
    if sw='CONFIG_SAVE' then
      LogMessage('Settings are kept in the Sky Simulator ini file');
    exit;
  end;

  if nm='FOCUS_MOTION' then
  begin
    if sw='FOCUS_INWARD'  then indi_foc_inward:=true;
    if sw='FOCUS_OUTWARD' then indi_foc_inward:=false;
    SendMotion(false);
    FLastInward:=indi_foc_inward;
    exit;
  end;

  if nm='FOCUS_ABORT_MOTION' then
  begin
    indi_focuser_abort;
    SendAbort(false,isOk);
    {report the stop immediately, do not wait for the next Poll}
    SendAbsPosition(false,isOk);
    SendRelPosition(false,isOk);
    FLastPos:=indi_foc_position; FLastState:=isOk;
    LogMessage('Focuser motion aborted');
    exit;
  end;

  if nm='FOCUS_REVERSE_MOTION' then
  begin
    indi_foc_reverse:=(sw='INDI_ENABLED');
    SendReverse(false);
    FLastReverse:=indi_foc_reverse;
    exit;
  end;
end;

procedure T_Indi_Focuser.DoNumber(e: TIndiElement);
var
  nm: string;
  v : integer;
begin
  nm:=e.A('name');

  if nm='ABS_FOCUS_POSITION' then
  begin
    if not e.HasChild('FOCUS_ABSOLUTE_POSITION') then exit;
    v:=round(e.ChildNum('FOCUS_ABSOLUTE_POSITION',indi_foc_position));
    indi_focuser_moveto(v);
    {Busy immediately: the client's progress display is driven by the
     Busy -> Ok transition, exactly as CCD_EXPOSURE is on the camera}
    SendAbsPosition(false,isBusy);
    SendRelPosition(false,isBusy);
    FLastState:=isBusy;
    exit;
  end;

  if nm='REL_FOCUS_POSITION' then
  begin
    if not e.HasChild('FOCUS_RELATIVE_POSITION') then exit;
    v:=round(e.ChildNum('FOCUS_RELATIVE_POSITION',0));
    indi_focuser_moverel(v);
    SendRelPosition(false,isBusy);
    SendAbsPosition(false,isBusy);
    FLastState:=isBusy;
    exit;
  end;

  if nm='FOCUS_MAX' then
  begin
    v:=round(e.ChildNum('FOCUS_MAX_VALUE',indi_foc_max));
    if v>0 then indi_foc_max:=v;
    if indi_foc_setpoint>indi_foc_max then
      indi_foc_setpoint:=indi_foc_max;
    SendMax(false);
    FLastMax:=indi_foc_max;
    exit;
  end;

  if nm='FOCUS_SYNC' then
  begin
    v:=round(e.ChildNum('FOCUS_SYNC_VALUE',indi_foc_position));
    indi_focuser_sync(v);
    SendSync(false,isOk);
    SendAbsPosition(false,isOk);
    FLastPos:=indi_foc_position; FLastState:=isOk;
    exit;
  end;
end;

{ ---------------- periodic push ----------------------------------------- }

procedure T_Indi_Focuser.Poll;
{Runs a few times per second on EVERY connection, so only push what
 actually changed: an unconditional setNumberVector floods the client.}
var
  st: TIndiState;
begin
  if indi_focuser_moving then st:=isBusy else st:=isOk;

  if (indi_foc_position<>FLastPos) or (st<>FLastState) then
  begin
    SendAbsPosition(false,st);
    SendRelPosition(false,st);
    FLastPos:=indi_foc_position;
    FLastState:=st;
  end;

  if abs(indi_foc_temperature-FLastTemp)>0.05 then
  begin
    SendTemperature(false);
    FLastTemp:=indi_foc_temperature;
  end;

  {these two can also be changed from the Sky Simulator side or by
   another client, so mirror them here as well}
  if indi_foc_max<>FLastMax then
  begin
    SendMax(false);
    FLastMax:=indi_foc_max;
  end;

  if indi_foc_inward<>FLastInward then
  begin
    SendMotion(false);
    FLastInward:=indi_foc_inward;
  end;

  if indi_foc_reverse<>FLastReverse then
  begin
    SendReverse(false);
    FLastReverse:=indi_foc_reverse;
  end;
end;

end.
