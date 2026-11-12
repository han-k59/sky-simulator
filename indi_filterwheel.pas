unit indi_filterwheel;

{$mode objfpc}{$H+}

{
INDI filter wheel protocol for Sky Simulator.

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
SECOND FILTER WHEEL DRIVE

   1) internal Alpaca filter wheel           alpaca_filterwheel_protocol.pas
   2) internal INDI filter wheel             THIS UNIT

As with the mount, the cameras, the focuser and the rotator, this unit
carries its OWN position variable, indi_wheel_position, so the two
protocols cannot disturb each other.  There is no simulation step: the
Alpaca wheel moves instantly and so does this one.

The filter NAMES and the focus offsets are not driver state, they
describe the simulated observatory, so they come from the form
(filter_1..filter_7 and updown_1..updown_7) exactly as
alpaca_filterwheel_protocol.filternames does.  Both protocols therefore
show the same seven filters.

sky_simulator_main must read indi_wheel_position instead of
alpaca_wheel_position for the bold filter indication when the INDI wheel
is selected.

OFF BY ONE  (the trap)
----------------------
INDI FILTER_SLOT is 1 based: slot 1 is the first filter.
alpaca_wheel_position is 0 based, and indi_wheel_position keeps that
same 0 based convention so the rest of the program does not have to
care which protocol is running.  The conversion happens in exactly two
places, both marked below.  Getting this wrong does not produce an
error, it silently returns the neighbouring filter.

BUSY THEN OK
------------
The move is instant, but a client that never sees Busy shows a filter
change as if the wheel were hung.  So FILTER_SLOT is answered Busy and
the following Poll pushes Ok, one tick later.

TRANSPORT
---------
Sockets, threads and XML live in indi_server.pas.  Registration is one
line, RegisterIndiDevice(T_Indi_FilterWheel).
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, indi_server;

const
  IndiWheelName  = 'Sky Simulator Filter Wheel';  {name a client shows}
  IndiWheelSlots = 7;             {the form has seven filters}

var
  {---- wheel state, the simulated hardware.  0 based, as Alpaca ----}
  indi_wheel_position : integer = 0;      {0 = luminance}

function indi_wheel_filtername(slot0: integer): string;   {0 based}

type
{ The INDI device.  One instance per connected client, created by
  indi_server; all three methods run in the main thread. }
  T_Indi_FilterWheel = class(TIndiDevice)
  private
    {cache of what was last pushed to THIS client}
    FLastSlot  : integer;
    FLastState : TIndiState;
    FLastNames : string;        {the seven names joined, cheap compare}
    FConnected : boolean;   {protocol state, not hardware state}

    function  NamesKey: string;
    procedure DefineDeviceProperties;   {libindi updateProperties() equivalent}
    procedure SendConnection(def: boolean);
    procedure SendDriverInfo;
    procedure SendConfigProcess(def: boolean);
    procedure SendSlot(def: boolean; st: TIndiState);
    procedure SendNames(def: boolean);

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
  sky_simulator_main;          {form1, memo2_message}

const
  GroupMain = 'Main Control';
  GroupOpt  = 'Options';
  GroupInfo = 'General Info';

function indi_wheel_filtername(slot0: integer): string;
{0 based.  Same source as alpaca_filterwheel_protocol.filternames: the
 filter name edit boxes on the form, so both protocols agree.}
begin
  case slot0 of
    0: Result:=form1.filter_1.text;
    1: Result:=form1.filter_2.text;
    2: Result:=form1.filter_3.text;
    3: Result:=form1.filter_4.text;
    4: Result:=form1.filter_5.text;
    5: Result:=form1.filter_6.text;
    6: Result:=form1.filter_7.text;
  else
    Result:='Filter'+IntToStr(slot0+1);
  end;
  if Trim(Result)='' then Result:='Filter'+IntToStr(slot0+1);
end;

{ ====================================================================== }
{ T_Indi_FilterWheel                                                      }
{ ====================================================================== }

constructor T_Indi_FilterWheel.Create;
begin
  inherited Create;
  DeviceName:=IndiWheelName;
  FLastSlot:=-1;                {force a first push}
  FLastState:=isIdle;
  FLastNames:='';
  FConnected:=false;
end;

function T_Indi_FilterWheel.NamesKey: string;
var i: integer;
begin
  Result:='';
  for i:=0 to IndiWheelSlots-1 do
    Result:=Result+indi_wheel_filtername(i)+'|';
end;

{ ---------------- property senders ------------------------------------- }

procedure T_Indi_FilterWheel.SendConnection(def: boolean);
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

procedure T_Indi_FilterWheel.SendDriverInfo;
begin
  StartDef('Text','DRIVER_INFO','Driver Info',GroupInfo,isIdle,ipRO);
  AddDef('Text','DRIVER_NAME','Name',DeviceName);
  AddDef('Text','DRIVER_EXEC','Exec','indi_skysimulator');
  AddDef('Text','DRIVER_VERSION','Version','1.0');
  AddDef('Text','DRIVER_INTERFACE','Interface','16'); {16 = FILTER_INTERFACE, 1 shl 4. NOT 4, that is GUIDER_INTERFACE}
  EndDef('Text');
end;

procedure T_Indi_FilterWheel.SendConfigProcess(def: boolean);
{See the long note in indi_camera.pas.  CCDciel asks for this on the
 filter wheel too and times out after a minute without it.  The
 simulator has no configuration file, so the buttons are acknowledged
 and do nothing.}
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

procedure T_Indi_FilterWheel.SendSlot(def: boolean; st: TIndiState);
{FILTER_SLOT is 1 BASED - see the OFF BY ONE note.  Conversion 1 of 2.}
begin
  if def then
  begin
    StartDef('Number','FILTER_SLOT','Filter Slot',GroupMain,st,ipRW);
    AddDef('Number','FILTER_SLOT_VALUE','Filter',
           IntToStr(indi_wheel_position+1),
           ' format="%.0f" min="1" max="'+IntToStr(IndiWheelSlots)+
           '" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','FILTER_SLOT',st);
    AddOne('Number','FILTER_SLOT_VALUE',IntToStr(indi_wheel_position+1));
    EndSet('Number');
  end;
end;

procedure T_Indi_FilterWheel.SendNames(def: boolean);
{FILTER_NAME_1 .. FILTER_NAME_n, also 1 based in the element names.}
var i: integer;
begin
  if def then
  begin
    StartDef('Text','FILTER_NAME','Filter Names',GroupMain,isOk,ipRW);
    for i:=0 to IndiWheelSlots-1 do
      AddDef('Text','FILTER_NAME_'+IntToStr(i+1),'Filter '+IntToStr(i+1),
             indi_wheel_filtername(i));
    EndDef('Text');
  end
  else
  begin
    StartSet('Text','FILTER_NAME',isOk);
    for i:=0 to IndiWheelSlots-1 do
      AddOne('Text','FILTER_NAME_'+IntToStr(i+1),indi_wheel_filtername(i));
    EndSet('Text');
  end;
end;

{ ---------------- definition dump --------------------------------------- }

procedure T_Indi_FilterWheel.DefineDeviceProperties;
{Everything except CONNECTION and DRIVER_INFO.  A libindi driver defines
 these from updateProperties(), i.e. only AFTER the client has connected.
 Called on getProperties and again on CONNECT; re-defining a property is
 legal INDI, the client simply replaces its copy.}
begin
  SendConfigProcess(true);
  SendSlot(true,isOk);
  SendNames(true);
end;

procedure T_Indi_FilterWheel.DefineAll;
begin
  SendConnection(true);
  SendDriverInfo;
  DefineDeviceProperties;
end;

{ ---------------- command handling -------------------------------------- }

procedure T_Indi_FilterWheel.HandleElement(e: TIndiElement);
begin
  if e=nil then exit;
  if e.Tag='newSwitchVector' then DoSwitch(e)
  else if e.Tag='newNumberVector' then DoNumber(e)
  else if e.Tag='newTextVector' then DoText(e);
  {enableBLOB is accepted and ignored: a wheel sends no BLOBs}
end;

procedure T_Indi_FilterWheel.DoSwitch(e: TIndiElement);
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
end;

procedure T_Indi_FilterWheel.DoNumber(e: TIndiElement);
var
  nm : string;
  slot: integer;
begin
  nm:=e.A('name');

  if nm='FILTER_SLOT' then
  begin
    if not e.HasChild('FILTER_SLOT_VALUE') then exit;
    {1 based on the wire, 0 based inside.  Conversion 2 of 2.}
    slot:=round(e.ChildNum('FILTER_SLOT_VALUE',indi_wheel_position+1))-1;

    if (slot<0) or (slot>IndiWheelSlots-1) then
    begin
      SendSlot(false,isAlert);
      FLastState:=isAlert;
      LogMessage('Filter position does not exist');
      exit;
    end;

    indi_wheel_position:=slot;
    memo2_message('INDI filter wheel to '+indi_wheel_filtername(slot)+'.');

    {the move is instant, but report Busy first so the client shows the
     change in progress; the next Poll turns it Ok}
    SendSlot(false,isBusy);
    FLastSlot:=indi_wheel_position;
    FLastState:=isBusy;
    exit;
  end;
end;

procedure T_Indi_FilterWheel.DoText(e: TIndiElement);
var
  i: integer;
  s: string;
begin
  if e.A('name')<>'FILTER_NAME' then exit;

  {Ekos likes to write the names back.  They live on the form and are
   shared with the Alpaca wheel, so apply them to the edit boxes - the
   same thing indi_mount does with the site coordinates.}
  for i:=0 to IndiWheelSlots-1 do
  begin
    s:=Trim(e.ChildValue('FILTER_NAME_'+IntToStr(i+1)));
    if s='' then continue;
    case i of
      0: form1.filter_1.text:=s;
      1: form1.filter_2.text:=s;
      2: form1.filter_3.text:=s;
      3: form1.filter_4.text:=s;
      4: form1.filter_5.text:=s;
      5: form1.filter_6.text:=s;
      6: form1.filter_7.text:=s;
    end;
  end;

  SendNames(false);
  FLastNames:=NamesKey;
end;

{ ---------------- periodic push ----------------------------------------- }

procedure T_Indi_FilterWheel.Poll;
{Runs a few times per second on EVERY connection, so only push what
 actually changed.}
var
  k: string;
begin
  if (indi_wheel_position<>FLastSlot) or (FLastState<>isOk) then
  begin
    SendSlot(false,isOk);           {Busy -> Ok, the move is complete}
    FLastSlot:=indi_wheel_position;
    FLastState:=isOk;
  end;

  {the user can rename a filter on the form at any moment}
  k:=NamesKey;
  if k<>FLastNames then
  begin
    SendNames(false);
    FLastNames:=k;
  end;
end;

end.
