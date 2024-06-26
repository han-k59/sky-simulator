unit alpaca_guidecamera_protocol;

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

uses cu_alpacaCamera, cu_alpacadevice, Classes, SysUtils, math;

procedure guide_camera_simulation(fast:boolean);{called by the timer every second}

var
  img_width  : integer=300;
  img_height : integer=200;
  start_x     :  integer=0; {start x sub section}
  start_Y     :  integer=0; {start y sub section}
  num_x       :  integer=9999999;{width subsection}
  num_y       :  integer=9999999;{height sub section}
  sensor_temperature : double=20;
  set_temperature    : double=-40;
  camera_gain        : integer=100;
  camera_exposure: double=10;
  cooler_on  : boolean=true;
  bin_X      : integer=1;
  bin_Y      : integer=1;
  last_exposureduration:double=-1;
  Percent_Completed: integer=0; {%}
  sensor_type: integer=0;
   { 0 = Monochrome,
     1 = Colour not requiring Bayer decoding
     2 = RGGB Bayer encoding
     3 = CMYG Bayer encoding
     4 = CMYG2 Bayer encoding
     5 = LRGB TRUESENSE Bayer encoding.}
  bayeroffset_X : integer=0;
  bayeroffset_Y : integer=0;


  the_img    : Timg;
  camera_state : integer=3;
    {Returns one of the following status information:

        0 CameraIdle At idle state, available to start exposure
        1 CameraWaiting Exposure started but waiting (for shutter, trigger, filter wheel, etc.)
        2 CameraExposing Exposure currently in progress
        3 CameraReading CCD array is being read out (digitized)
        4 CameraDownload Downloading data to PC
        5 CameraError Camera error condition serious enough to prevent further operations (connection fail, etc.).
     }

const
  gain_max=1000;
  gain_min=100;


type
  T_Alpaca_guidecam = class(T_AlpacaCamera)
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
      function  SupportedActions:TStringList; override;
      function  Name:string; override;
      function  cameraXsize: integer; override;
      function  cameraYsize: integer; override;
      function  maxbinx : integer; override;
      function  maxbinY : integer; override;
      function  binx : integer; override;
      function  binY : integer; override;
      function  pixelsizex : integer; override;
      function  pixelsizeY : integer; override;
      function  sensorname : string; override;

      function  maxadu : integer; override;
      function  bayeroffsetX : integer; override;
      function  bayeroffsetY : integer; override;
      function  camerastate : integer; override;
      function  startx : integer; override;
      function  starty : integer; override;
      function  numx : integer; override;
      function  numy : integer; override;
      function  setccdtemperature : integer; override;{get}

      function  cangetcoolerpower: boolean; override;
      function  cansetccdtemperature: boolean; override;
      function  cooleron: boolean; override;
      function  hasshutter: boolean; override;
      function  imageready: boolean; override;

      function  ccdtemperature: double;  override;
      function  exposuremax: double;  override;
      function  exposuremin: double;  override;
      function  fullwellcapacity: double;  override;
      function  electronsperadu: double;  override;
      function  gain: integer;  override;
      function  gainmax: integer;  override;
      function  gainmin: integer;  override;

      function  coolerpower: double;  override;
      function  heatsinktemperature: double;  override;
      function  exposureresolution: double;  override;
      function  lastexposureduration: double;  override;
      function  percentcompleted: integer;  override;

      function  sensortype: integer;  override;
      function  ispulseguiding: boolean; override;


      procedure abortexposure(out ok:boolean); override;
      procedure stopexposure(out ok:boolean); override;
      procedure startexposure(x: double; out errortype : integer); override;
      procedure setCCDTemperature(x: double; out error:double); override;
      procedure setGain(x: integer; out error :integer); override;
      procedure setcooler(x: boolean); override;


      procedure setstartx(x: integer); override;
      procedure setstarty(x: integer); override;
      procedure setnumx(x: integer); override;
      procedure setnumy(x: integer); override;
      procedure setbinX(x: integer; out ok :boolean); override;
      procedure setbinY(x: integer; out ok :boolean); override;

      function  imagearray(out ok: boolean): Timg; override;
      function  canpulseguide: boolean; override;
      function  canabortexposure: boolean; override;
      function  canstopexposure: boolean; override;
      function  canasymmetricbin: boolean; override;



  end;


implementation

uses sky_annotation;{for img_array}

// Replace the following by the driver UniqueID
// On Linux this can be generated by the command uuidgen
// See the ASCOM Alpaca Management API configureddevices
// https://ascom-standards.org/api/?urls.primaryName=ASCOM%20Alpaca%20Management%20API#/Management%20Interface%20(JSON)/get_management_v1_configureddevices
const guid='Guide Camera_sky_simulator_unique-id';
var  exposure_remaining: double=0;


procedure guide_camera_simulation(fast:boolean);{called by the timer every second}
begin
  if fast=false then
     exposure_remaining:=exposure_remaining-1 // decrease remaining time with one second
  else
    exposure_remaining:=0;


  if exposure_remaining<=0 then //exposure finished
  begin
    camera_state:=0;
    exposure_remaining:=0;
  end;
  Percent_Completed:=round(100*(last_exposureduration-exposure_remaining)/last_exposureduration);

  {0 CameraIdle At idle state, available to start exposure
   1 CameraWaiting Exposure started but waiting (for shutter, trigger, filter wheel, etc.)
   2 CameraExposing Exposure currently in progress
   3 CameraReading CCD array is being read out (digitized)
   4 CameraDownload Downloading data to PC
   5 CameraError Camera error condition serious enough to prevent further operations (connection fail, etc.).
  }
end;

constructor T_Alpaca_GuideCam.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor  T_Alpaca_GuideCam.Destroy;
begin
  inherited Destroy;
end;

function  T_Alpaca_GuideCam.GetGuid: string;
begin
  result:=guid;
end;

function  T_Alpaca_GuideCam.Action( actionName, actionParameters: string):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

procedure T_Alpaca_GuideCam.CommandBlind( command: string;  raw: boolean = false);
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
end;

function  T_Alpaca_GuideCam.CommandBool(command: string;  raw: boolean = false):boolean;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:=false;
end;

function  T_Alpaca_GuideCam.CommandString(command: string;  raw: boolean = false):string;
begin
  FErrorNumber:=ERR_NOT_IMPLEMENTED;
  FErrorMessage:=MSG_NOT_IMPLEMENTED;
  result:='';
end;

function T_Alpaca_GuideCam.GetSetupPage: string;
begin
  result:='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">'+
       '<html><head><meta http-equiv="Content-Type" content="text/html; charset=utf-8">'+
       '<title>Camera setup</title></head><body text>'+
       '<H1>Driver Setup</H1><br/>'+
       'If your using ASCOM Remote, select Base64 for fast image transfer!<br/><br/>'+
       '</body></html>';
end;

function  T_Alpaca_GuideCam.Connected:boolean;
begin
  result:=FConnected;
end;

procedure  T_Alpaca_GuideCam.SetConnected(value:boolean);
begin
  // code to make connection
  FConnected:=value;
end;

function  T_Alpaca_GuideCam.Description:string;
begin
  result:='Guide Camera Sky Simulator.';
end;

function  T_Alpaca_GuideCam.DriverInfo:string;
begin
  result:='Guide Camera Sky Simulator.';
end;

function  T_Alpaca_GuideCam.DriverVersion:string;
begin
  result:='v1';
end;

function  T_Alpaca_GuideCam.InterfaceVersion: integer;
begin
  result:=3; // ICamera version implementation
end;

function  T_Alpaca_GuideCam.SupportedActions:TStringList;
begin

  result:=TStringList.Create;
  result.Clear;
  result.add('startexposure');
  result.add('and much more!');
end;

function  T_Alpaca_GuideCam.Name:string;
begin
  result:='Guide Camera Sky Simulator for ALPACA';
end;

function  T_Alpaca_GuideCam.cameraXsize: integer;
begin
  img_width:=length(img_array[0]);
  result:=img_width;
  if num_X>=9999 then num_X:=img_width; {initialise}
end;

function  T_Alpaca_GuideCam.cameraYsize: integer;
begin
  img_height:=length(img_array);
  result:=img_height;
  if num_Y>=9999 then num_Y:=img_height; {initialise}
end;

function  T_Alpaca_GuideCam.maxbinx: integer;
begin
  result:=1;
end;

function  T_Alpaca_GuideCam.maxbiny: integer;
begin
  result:=1;
end;


function  T_Alpaca_GuideCam.binx: integer;
begin
  result:=bin_X;
end;

function  T_Alpaca_GuideCam.biny: integer;
begin
  result:=bin_Y;
end;

function  T_Alpaca_GuideCam.pixelsizex: integer;
begin
  result:=5;
end;

function  T_Alpaca_GuideCam.pixelsizey: integer;
begin
  result:=5;
end;

function  T_Alpaca_GuideCam.sensorname: string;
begin
  result:='mono sensor';
end;

function  T_Alpaca_GuideCam.maxadu: integer;
begin
  result:=65535;
end;

function  T_Alpaca_Guidecam.bayeroffsetX: integer;
begin
  result:=bayeroffset_X;
end;

function  T_Alpaca_Guidecam.bayeroffsetY: integer;
begin
  result:=bayeroffset_Y;
end;

function  T_Alpaca_GuideCam.camerastate: integer;
begin
  result:=camera_state;

  {Returns one of the following status information:

      0 CameraIdle At idle state, available to start exposure
      1 CameraWaiting Exposure started but waiting (for shutter, trigger, filter wheel, etc.)
      2 CameraExposing Exposure currently in progress
      3 CameraReading CCD array is being read out (digitized)
      4 CameraDownload Downloading data to PC
      5 CameraError Camera error condition serious enough to prevent further operations (connection fail, etc.).
   }
end;

function  T_Alpaca_GuideCam.cangetcoolerpower: boolean;
begin
  result:=false;
end;
function  T_Alpaca_GuideCam.cansetccdtemperature: boolean;
begin
  result:=true;
end;
function  T_Alpaca_GuideCam.cooleron: boolean;
begin
  result:=cooler_on;
end;
function  T_Alpaca_GuideCam.hasshutter: boolean;
begin
  result:=false;
end;
function  T_Alpaca_GuideCam.imageready: boolean;
begin
  result:=((img_array<>nil) and (last_exposureduration>=0) and (Percent_Completed>=100)); {image in the buffer and startexposure was given}
end;

function  T_Alpaca_GuideCam.ccdtemperature: double;
begin
  result:=sensor_temperature;
end;

function  T_Alpaca_GuideCam.exposuremax: double;
begin
  result:=3600;
end;

function  T_Alpaca_GuideCam.exposuremin: double;
begin
  result:=0;
end;

function  T_Alpaca_GuideCam.fullwellcapacity: double;
begin
  result:=65535;
end;


function  T_Alpaca_GuideCam.electronsperadu: double;
begin
  result:=100/camera_gain; {100 is normal resulting in 1e/adu}
end;

function  T_Alpaca_GuideCam.gain: integer;
begin
  result:=camera_gain;
end;

function  T_Alpaca_GuideCam.gainmax: integer;
begin
  result:=gain_max;
end;

function  T_Alpaca_GuideCam.gainmin: integer;
begin
  result:=gain_min; {100 is factor 1}
end;

function  T_Alpaca_GuideCam.coolerpower: double;
begin
  result:=max(100,min(0,sqr((20 - set_temperature)/5)  ));
end;

function  T_Alpaca_GuideCam.heatsinktemperature: double;
begin
  result:=35;
end;
function  T_Alpaca_GuideCam.exposureresolution: double;
begin
  result:=0;
end;

function  T_Alpaca_GuideCam.lastexposureduration: double;
begin
  result:=last_exposureduration;
end;

function  T_Alpaca_GuideCam.percentcompleted: integer;
begin
  result:=Percent_Completed;
end;


function  T_Alpaca_GuideCam.sensortype: integer;
begin
  result:=0;{mono}
end;
function  T_Alpaca_GuideCam.ispulseguiding: boolean;
begin
  result:=false;
end;

procedure T_Alpaca_GuideCam.abortexposure(out ok:boolean);
begin
  {abortexposure;}
  ok:=true; {}
  Percent_Completed:=0;{%}
end;

procedure T_Alpaca_GuideCam.stopexposure(out ok:boolean);
begin
  {abortexposure;}
  ok:=true; {}
  Percent_Completed:=0;{%}
end;

procedure T_Alpaca_GuideCam.startexposure(x: double; out errortype : integer);
begin
  {startexposure}
  camera_exposure:=max(0,x);

  errortype:=0;
  if abs(camera_exposure-x)>0.00001 {check one} then
    errortype:=1; {invalid range}
  if ((start_X+num_X>img_width/bin_X) or (start_Y+num_Y>img_height/ bin_Y)) then {check two}
    errortype:=2;{subwindow out out range}

  camera_state:=2; {exposure ongoing}
  {0 CameraIdle At idle state, available to start exposure
   1 CameraWaiting Exposure started but waiting (for shutter, trigger, filter wheel, etc.)
   2 CameraExposing Exposure currently in progress
   3 CameraReading CCD array is being read out (digitized)
   4 CameraDownload Downloading data to PC
   5 CameraError Camera error condition serious enough to prevent further operations (connection fail, etc.).
  }


  exposure_remaining:=camera_exposure;
  last_exposureduration:=camera_exposure;
  Percent_Completed:=0;{%}
end;

procedure T_Alpaca_GuideCam.SetCCDTemperature(x: double;out error:double);
begin
  set_temperature:=min(max(-40,x),50);{limit setpoint range}
  error:=set_temperature-x;

  if cooler_on then
    sensor_temperature:=set_temperature
  else
    sensor_temperature:=20;

end;

procedure T_Alpaca_GuideCam.SetGain(x: integer;out error :integer);
begin
  camera_gain:=min(max(x,gain_min),gain_max); {minimum gain is 100%}
  error:=x-gain; {will be unequal if clamped}
end;

procedure T_Alpaca_GuideCam.setcooler(x: boolean);
begin
  cooler_on:=x;
end;


function  T_Alpaca_GuideCam.startx: integer;
begin
  result:=start_x;  {report sub section begin x}
end;

function  T_Alpaca_GuideCam.starty: integer;
begin
  result:=start_y; {report sub section begin y}
end;

function  T_Alpaca_GuideCam.numx: integer;
begin
  result:=num_x;{report sub section width}
end;
function  T_Alpaca_GuideCam.numy: integer;
begin
  result:=num_y; {report sub section height}
end;
function  T_Alpaca_GuideCam.setccdtemperature: integer;
begin
  result:=round(set_temperature);
end;

procedure T_Alpaca_GuideCam.setstartx(x: integer);
begin
  start_x:=x; {set sub section begin x}
  {no check since it is binning dependend and clamped later. Check is done in startExposure}
end;

procedure T_Alpaca_GuideCam.setstarty(x: integer);
begin
  start_Y:=x; {set sub section begin y}
  {no check since it is binning dependend and clamped later. Check is done in startExposure}

end;

procedure T_Alpaca_GuideCam.setnumx(x: integer);
begin
  num_x:=x;{set sub section width}
  {no check since it is binning dependend and clamped later. Check is done in startExposure}
end;

procedure T_Alpaca_GuideCam.setnumy(x: integer);
begin
  num_Y:=x;{set sub section heigth}
  {no check on NumY since it is binning dependend and clamped  Check is done in startExposure}
end;

procedure T_Alpaca_GuideCam.setbinX(x: integer; out ok :boolean);
begin
  bin_X:=max(1,min(1,x));{clamp range}
  ok:=(bin_X=x);
end;

procedure T_Alpaca_GuideCam.setbinY(x: integer; out ok :boolean);
begin
  bin_Y:=max(1,min(1,x));{clamp range}
  ok:=(bin_Y=x);
end;


function  T_Alpaca_GuideCam.imagearray(out ok: boolean): Timg;
var
  x,y, w, h,Ystart,Ystop,Xstart,Xstop : integer;
  noise,relative_gain : double;
begin

  relative_gain:=camera_gain/100; { gain of 1 is normal}
  noise:=(5/relative_gain {read noise} +15*sqrt(camera_exposure/5) {sky noise});{assume Sky Noise Dominated case. Gain reduces read-noise (5/gain)}
  img_width:=length(img_array[0]);
  img_height:=length(img_array);

  annotation_to_array('Guide cam',true{transparant},1000{gralevel},2, 10,img_height-25 {screen coord}, img_array);{string to image array as annotation}

  Ystop:= min(img_height,max(0,start_Y+num_Y));
  Ystart:=min(img_height,max(0,start_Y));

  Xstop:= min(img_width,max(0,start_X+num_X)); {left is 0}
  Xstart:=min(img_width,max(0,start_X));

  w:=Xstop-Xstart; {could be subsection}
  h:=Ystop-Ystart; {could be subsection}

  setlength(the_img,w,h);//width and height are swapped here !! Less efficient

   for x:=Xstart to Xstop-1 do
     for y:=Ystart to  Ystop-1 do
        the_img[x-Xstart,y-Ystart]:=min(65535, round(relative_gain*(camera_exposure)*img_array[y,x] + randg(4*noise {mean},noise {sd}) )) ;

  ok:=last_exposureduration>=0; {startexposure was given}


  result:=the_img;
end;

function T_Alpaca_GuideCam.canpulseguide: boolean;
begin
  result:=false;
end;

function T_Alpaca_GuideCam.canabortexposure: boolean;
begin
  result:=true;
end;

function T_Alpaca_GuideCam.canstopexposure: boolean;
begin
  result:=true;
end;

function T_Alpaca_GuideCam.canasymmetricbin: boolean;
begin
  result:=false;
end;

end.

