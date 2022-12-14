unit alpaca_rotator_protocol;

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

uses cu_alpacarotator, cu_alpacadevice,  Classes, SysUtils, math;

var
    alpaca_rot_position       : integer=0;{mechanical}
    alpaca_rot_position_synced: integer=0;{synced}
    alpaca_rot_position_target: integer=0;{mechanical}
    alpaca_rot_position_target_synced: integer=0;{synced}
    alpaca_rot_temperature    : double=20;
    alpaca_rot_reverse        : boolean=false;
    alpaca_sync_offset        : integer;

procedure rotator_simulation; {called each second by a  timer}

type
  T_Alpaca_rotator = class(T_Alpacarotator)
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
      function  InterfaceVersion: integer; override;
      function  Name:string; override;
      function  position: double; override;
      function  targetposition: double; override;
      function  mechanicalposition: double; override;


      function  stepsize: double; override;
      function  SupportedActions:TStringList; override;

      function  fabsolute: boolean; override;
      function  ismoving: boolean; override;
      function  reverse: boolean; override;
      function  canreverse: boolean; override;
      function  temperature: double;  override;
      procedure rot_halt; override;
      procedure SetReverse(value:boolean); override;
      procedure rotator_move(x: integer); override;
      procedure rotator_moveabsolute(thepos: double); override;
      procedure rotator_movemechanical(thepos: double); override;
      procedure rotator_sync(thepos: double); override;
  end;


implementation

// Replace the following by the driver UniqueID
// On Linux this can be generated by the command uuidgen
// See the ASCOM Alpaca Management API configureddevices
// https://ascom-standards.org/api/?urls.primaryName=ASCOM%20Alpaca%20Management%20API#/Management%20Interface%20(JSON)/get_management_v1_configureddevices
const guid='rotator_sky_simulator_unique-id';


procedure rotator_simulation; {called by the timer}
var
  deltaPos,step_size : integer;
begin
  {calculate first mechanical}
  deltaPos:=alpaca_rot_position_target-alpaca_rot_position;

  step_size:=min(abs(deltaPos),30); {30 steps per second}
  if deltaPos<0 then
    alpaca_rot_position:=alpaca_rot_position-step_size
  else
    alpaca_rot_position:=alpaca_rot_position+step_size;

end;



constructor T_Alpaca_rotator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_Alpaca_rotator.Destroy;
begin
  inherited Destroy;
end;

function  T_Alpaca_rotator.GetGuid: string;
begin
  result:=guid;
end;

function  T_Alpaca_rotator.Action( actionName, actionParameters: string):string; {ASCOM Methods Common To All Devices}
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_Alpaca_rotator.CommandBlind( command: string;  raw: boolean = false); {ASCOM Methods Common To All Devices}
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_rotator.CommandBool(command: string;  raw: boolean = false):boolean;  {ASCOM Methods Common To All Devices}
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_rotator.CommandString(command: string;  raw: boolean = false):string;  {ASCOM Methods Common To All Devices}
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

function T_Alpaca_rotator.GetSetupPage: string;
begin
  result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
       '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
       '<title>Rotator driver</title></head><body text>'+
       '<H1>Driver Setup</H1><br/>'+
       'There is no Sky Simulator setup information.<br/><br/>'+
       '</body></html>';
end;

function  T_Alpaca_rotator.Connected:boolean;
begin
  result:=FConnected;
end;

procedure  T_Alpaca_rotator.SetConnected(value:boolean);
begin
  // code to make connection
  FConnected:=value;
end;

function  T_Alpaca_rotator.Description:string;
begin
  result:='Sky simulator rotator simulator';
end;

function  T_Alpaca_rotator.DriverInfo:string;
begin
  result:='Sky simulator rotator simulator';
end;

function  T_Alpaca_rotator.DriverVersion:string;
begin
  result:='v1';
end;

function  T_Alpaca_rotator.InterfaceVersion: integer;
begin
  result:=3; // ITelescope version implementation
end;

function  T_Alpaca_rotator.Name:string;
begin
  result:='Rotator Sky Simulator for ALPACA';
end;

function  T_Alpaca_rotator.SupportedActions:TStringList;
begin
  result:=TStringList.Create;
  result.Clear;
  result.add('moveabsolute');
  result.add('Many actions are supported!');
end;

function  T_Alpaca_rotator.fabsolute: boolean;
begin
  result:=true;
end;

function  T_Alpaca_rotator.ismoving: boolean;
begin
  result:=abs(alpaca_rot_position_target-alpaca_rot_position)>=1;
end;

function  T_Alpaca_rotator.temperature: double;
begin
  result:=alpaca_rot_temperature;
end;

function  T_Alpaca_rotator.position: double;
begin
  {convert to synced}
  alpaca_rot_position_synced:=alpaca_rot_position+alpaca_sync_offset;

  while alpaca_rot_position_synced>=360 do  alpaca_rot_position_synced:=alpaca_rot_position_synced-360;
  while alpaca_rot_position_synced< 0   do  alpaca_rot_position_synced:=alpaca_rot_position_synced+360;

  result:=alpaca_rot_position_synced;
end;

function  T_Alpaca_rotator.targetposition: double;
begin
  result:=alpaca_rot_position_target;
end;

function  T_Alpaca_rotator.mechanicalposition: double;
begin
  result:=alpaca_rot_position;
end;

function  T_Alpaca_rotator.reverse: boolean;
begin
  result:=alpaca_rot_reverse;
end;

function  T_Alpaca_rotator.canreverse: boolean;
begin
  result:=true; {0 to -360 degrees}
end;

function  T_Alpaca_rotator.stepsize: double;{returns minimum stepsize}
begin
  result:=1.0;
end;

procedure T_Alpaca_rotator.SetReverse(value:boolean);
begin
  alpaca_rot_reverse:=value;
end;

procedure T_Alpaca_rotator.rotator_move(x: integer);{Causes the rotator to move Position degrees relative to the current Position value.}
begin
  if alpaca_rot_reverse=false then
     alpaca_rot_position_target:=alpaca_rot_position_target+x
  else alpaca_rot_position_target:=alpaca_rot_position_target-x;

  while alpaca_rot_position_target>=360 do  alpaca_rot_position_target:=alpaca_rot_position_target-360;
  while alpaca_rot_position_target<   0 do  alpaca_rot_position_target:=alpaca_rot_position_target+360;

end;


procedure T_Alpaca_rotator.rotator_moveabsolute(thepos: double);{put}
begin
  alpaca_rot_position_target_synced:=round(thepos);
  alpaca_rot_position_target:= alpaca_rot_position_target_synced - alpaca_sync_offset; {synced to mechanical position}

  while alpaca_rot_position_target>=360 do  alpaca_rot_position_target:=alpaca_rot_position_target-360;
  while alpaca_rot_position_target< 0   do  alpaca_rot_position_target:=alpaca_rot_position_target+360;
end;


procedure T_Alpaca_rotator.rotator_movemechanical(thepos: double);{put}
begin
  alpaca_rot_position_target:=round(thepos);
end;

procedure T_Alpaca_rotator.rotator_sync(thepos: double);{put}
begin
  alpaca_sync_offset:= round(thepos-alpaca_rot_position);
end;



procedure T_Alpaca_rotator.rot_halt;
begin
  {halt rotator}
  alpaca_rot_position_target:=alpaca_rot_position;
end;


end.

