unit indi_guide_camera;

{$mode objfpc}{$H+}

{
INDI camera simulation and protocol for Sky Simulator.

Copyright (C) 2026 Han Kleijn
https://sourceforge.net/projects/sky-simulator

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

--------------------------------------------------------------------------
INDI GUIDE CAMERA

This is the guide camera counterpart of indi_camera.pas, in the same
relation to it as alpaca_guidecamera_protocol.pas is to
alpaca_camera_protocol.pas: a full independent copy rather than shared
code, so either camera can be changed without touching the other.

It is NOT an alternative drive.  The main camera and the guide camera are
both active at the same time, each a separate INDI device with its own
name, and a client may drive one, the other or both.  The guide camera
selection follows the main camera's protocol radio button.

Both cameras look at the same simulated sky (img_array), exactly as the
two Alpaca cameras do, but each applies its own gain, offset, subframe,
binning and noise to it, so the two frames are not identical.

This unit carries its OWN complete camera state: its own frame, binning, gain,
offset, temperature, exposure countdown and its own copy of the
simulation step.  Nothing is shared with alpaca_camera_protocol, which
means the two can be changed independently without any risk of one
breaking the other.  This is exactly the arrangement used by
indi_guide_mount.pas next to alpaca_mount_protocol.pas.

Shared, because they describe the observatory and not the driver:
img_array (the simulated sky), update_required, focal length and pixel
size from the form.

WHAT IS COPIED
--------------
indi_guide_camera_simulation is a copy of camera_simulation, and
indi_guide_camera_build_image is a copy of T_Alpaca_cam.imagearray with one
deliberate difference, see the AXIS ORDER note below.

AXIS ORDER  (the trap)
----------------------
img_array is indexed [y,x]: length(img_array)=height,
length(img_array[0])=width.  T_Alpaca_cam.imagearray transposes that
into the_img[x,y] because the Alpaca JSON image array wants it that
way.  FITS does NOT: FITS is row major with NAXIS1=width, so a
transposed frame would be silently wrong and only show up as a failed
plate solve.

Rather than transpose and then transpose back, indi_guide_the_img keeps the
SAME [y,x] layout as img_array and is written out row by row.  This is
the one place where this unit does not mirror the Alpaca code
literally, and it is on purpose.

Row order matches sky_simulator_unit_save_image.save_fits: row 0 of
img_array is written first, i.e. bottom-up, which is what the existing
BAYERPAT/ROWORDER handling already assumes.

BLOBS  (the other trap)
-----------------------
A BLOB may never be sent before the client has asked for it with
<enableBLOB>Also</enableBLOB> or Only.  Clients drop the connection on
an unrequested BLOB.  T_Indi_GuideCamera is instantiated once per
connection, so FBlobEnabled is a private field and can never leak from
one client to another.

TRANSPORT
---------
Sockets, threads and XML live in indi_server.pas.  This unit only
implements the device and needs no change there: registration is one
line, RegisterIndiDevice(T_Indi_GuideCamera).
--------------------------------------------------------------------------
}

interface

uses
  Classes, SysUtils, math, base64, indi_server;

const
  IndiGuideCameraName = 'Sky Simulator Guide Camera';  {device name a client will show}

  indi_guide_gain_max    = 1000;
  indi_guide_gain_min    = 50;
  indi_guide_offset_max  = 2000;
  indi_guide_offset_min  = 50;
  indi_guide_bin_maximum = 2;                     {2x2 is all bin_2X2 can do}

var
  {---- camera state, the simulated hardware ----}
  indi_guide_img_width  : integer = 300;          {unbinned, refreshed from img_array}
  indi_guide_img_height : integer = 200;

  indi_guide_start_x : integer = 0;               {subframe, UNBINNED pixels}
  indi_guide_start_y : integer = 0;
  indi_guide_num_x   : integer = 9999999;         {clamped on first use, as Alpaca}
  indi_guide_num_y   : integer = 9999999;

  indi_guide_bin_x : integer = 1;
  indi_guide_bin_y : integer = 1;

  indi_guide_camera_gain   : integer = 100;       {100 = factor 1}
  indi_guide_camera_offset : integer = 100;

  indi_guide_sensor_temperature : double = 20;
  indi_guide_set_temperature    : double = -40;
  indi_guide_cooler_on          : boolean = true;

  indi_guide_pixelsizemicrometer : double = 5;

  indi_guide_sensor_type   : integer = 0;   {0 mono, 2 Bayer, see alpaca camera}
  indi_guide_bayeroffset_X : integer = 0;
  indi_guide_bayeroffset_Y : integer = 0;

  indi_guide_camera_exposure       : double = 10;
  indi_guide_exposure_remaining    : double = 0;
  indi_guide_last_exposureduration : double = -1;   {<0 means no valid image}
  indi_guide_last_exposurestarttime: string = '2000-01-01T12:00:00.000';

  indi_guide_camera_state     : integer = 0;   {0 idle, 2 exposing, as ASCOM}
  indi_guide_Percent_Completed: integer = 0;

  indi_guide_frame_type : integer = 0;   {0 light, 1 bias, 2 dark, 3 flat}
  indi_guide_compress   : boolean = false;  {CCD_COMPRESSION, see note at the end}

{ ---- simulation ------------------------------------------------------- }
procedure indi_guide_camera_simulation(fast: boolean);  {call from Timer1Timer}

{ ---- commands --------------------------------------------------------- }
procedure indi_guide_camera_startexposure(x: double; out errortype: integer);
procedure indi_guide_camera_abort;
function  indi_guide_camera_imageready: boolean;
procedure indi_guide_camera_refresh_size;   {pick up the current sky image size}

type
{ The INDI device.  One instance per connected client, created by
  indi_server; all three methods run in the main thread. }
  T_Indi_GuideCamera = class(TIndiDevice)
  private
    FBlobEnabled  : boolean;   {per client: has it sent enableBLOB?}
    FLastSeq: integer;   {last exposure sequence this client delivered}
    FLastExpValue : double;    {cache of what was last pushed}
    FLastTemp     : double;
    FLastCooler   : boolean;
    FLastState    : TIndiState;
    FConnected    : boolean;   {protocol state, not hardware state}


    procedure DefineDeviceProperties;   {libindi updateProperties() equivalent}
    procedure SendConnection(def: boolean);
    procedure SendDriverInfo;
    procedure SendCcdInfo(def: boolean);
    procedure SendExposure(def: boolean; st: TIndiState);
    procedure SendAbort(def: boolean; st: TIndiState);
    procedure SendFrame(def: boolean; st: TIndiState);
    procedure SendBinning(def: boolean; st: TIndiState);
    procedure SendFrameType(def: boolean);
    procedure SendTemperature(def: boolean; st: TIndiState);
    procedure SendCooler(def: boolean);
    procedure SendCompression(def: boolean);
    procedure SendGain(def: boolean);
    procedure SendOffset(def: boolean);
    procedure SendCfa(def: boolean);
    procedure SendConfigProcess(def: boolean);
    procedure SendUploadMode(def: boolean);
    procedure DefineBlob;
    procedure SendBlob;

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
  sky_annotation,        {img_array, update_required, utc_date_time, ra0, dec0}
  sky_simulator_main,    {form1, memo2_message, strtofloat2}
  cu_alpacadevice;       {Timg}

const
  GroupMain  = 'Main Control';
  GroupImage = 'Image Settings';
  GroupInfo  = 'General Info';
  GroupOpt   = 'Options';


var
  indi_guide_the_img : Timg;   {the delivered frame, [y,x] - see AXIS ORDER above}

  {INDI is a broadcast protocol: a completed frame goes to every client
   that enabled BLOBs, not just the one that sent CCD_EXPOSURE.  CCDciel
   uses separate connections for the two, so keying delivery on the
   requesting client loses the image.  Each exposure gets a sequence
   number; each client delivers each sequence at most once.}
  indi_guide_exposure_seq  : integer = 0;
  indi_guide_fits_cache    : ansistring = '';
  indi_guide_fits_cache_seq: integer = -1;

{ ====================================================================== }
{ Camera simulation.                                                      }
{ Copy of camera_simulation with indi_guide_ variables, so this camera is fully }
{ independent of alpaca_camera_protocol.                                  }
{ ====================================================================== }

procedure indi_guide_camera_simulation(fast: boolean);
{Called every second by Timer1 while the INDI camera is selected.}
begin
  if fast=false then
    indi_guide_exposure_remaining:=indi_guide_exposure_remaining-1  {one second per tick}
  else
    indi_guide_exposure_remaining:=0;

  if indi_guide_exposure_remaining<=0 then          {exposure finished}
  begin
    indi_guide_camera_state:=0;
    indi_guide_exposure_remaining:=0;
    {only claim 100% if an exposure was actually started, otherwise an idle
     camera would look like it has a frame waiting}
    if indi_guide_last_exposureduration>=0 then indi_guide_Percent_Completed:=100;
  end
  else
    indi_guide_Percent_Completed:=round(100*(indi_guide_last_exposureduration-
                                       indi_guide_exposure_remaining)/
                                       indi_guide_last_exposureduration);
end;

procedure indi_guide_camera_refresh_size;
{The sky image is regenerated by the main simulation and can change size,
 so the sensor size follows it - same as T_Alpaca_cam.cameraXsize does.}
begin
  if img_array=nil then exit;
  indi_guide_img_height:=length(img_array);
  indi_guide_img_width :=length(img_array[0]);
  if indi_guide_num_x>=9999 then indi_guide_num_x:=indi_guide_img_width;    {initialise}
  if indi_guide_num_y>=9999 then indi_guide_num_y:=indi_guide_img_height;
end;

procedure indi_guide_camera_startexposure(x: double; out errortype: integer);
{errortype 0 ok, 1 exposure out of range, 2 subframe out of range}
begin
  {In simulation the image already exists if the mount moved, so
   startexposure only stamps a valid start time.}
  indi_guide_last_exposurestarttime:=utc_date_time;

  indi_guide_camera_exposure:=max(0,x);
  errortype:=0;
  if abs(indi_guide_camera_exposure-x)>0.00001 then errortype:=1;
  if ((indi_guide_start_x+indi_guide_num_x>indi_guide_img_width) or
      (indi_guide_start_y+indi_guide_num_y>indi_guide_img_height)) then errortype:=2;

  indi_guide_camera_state:=2;                       {exposure ongoing}
  indi_guide_exposure_remaining   :=indi_guide_camera_exposure;
  indi_guide_last_exposureduration:=indi_guide_camera_exposure;
  indi_guide_Percent_Completed:=0;

  inc(indi_guide_exposure_seq);
  indi_guide_fits_cache:='';           {drop the previous frame}
  indi_guide_fits_cache_seq:=-1;
end;

procedure indi_guide_camera_abort;
begin
  indi_guide_exposure_remaining:=0;
  indi_guide_camera_state:=0;
  indi_guide_Percent_Completed:=0;
  indi_guide_last_exposureduration:=-1;   {invalidate: no image after an abort}
end;

function indi_guide_camera_imageready: boolean;
begin
  Result:=((img_array<>nil) and (indi_guide_last_exposureduration>=0) and
           (indi_guide_Percent_Completed>=100) and (update_required=false));
end;

{ ====================================================================== }
{ Frame building                                                          }
{ ====================================================================== }

procedure indi_guide_bin_2X2(var img: Timg);
{2x2 bin, img is [y,x] here (the Alpaca copy works on a transposed array,
 which happens to give the same result there).}
var
  ix, iy, w, h: integer;
  tmp: Timg;
begin
  h:=length(img) div 2;
  if h=0 then exit;
  w:=length(img[0]) div 2;
  if w=0 then exit;

  setlength(tmp,h,w);
  for iy:=0 to h-1 do
    for ix:=0 to w-1 do
      tmp[iy,ix]:=(img[iy*2  ,ix*2  ]+
                   img[iy*2+1,ix*2  ]+
                   img[iy*2  ,ix*2+1]+
                   img[iy*2+1,ix*2+1]) div 4;
  img:=tmp;
end;

function indi_guide_camera_build_image(out ok: boolean): Timg;
{Copy of T_Alpaca_cam.imagearray, but WITHOUT the width/height swap:
 indi_guide_the_img[y,x] keeps the img_array layout so it can be streamed
 straight into a FITS file.  See AXIS ORDER in the unit header.}
var
  x, y, w, h, Ystart, Ystop, Xstart, Xstop: integer;
  noise, relative_gain: double;
begin
  ok:=false;
  Result:=nil;
  if img_array=nil then exit;

  relative_gain:=indi_guide_camera_gain/100;        {gain of 1 is normal}
  {sky noise dominated case; gain reduces the read noise}
  noise:=(5/relative_gain + 15*sqrt(indi_guide_camera_exposure/5));

  indi_guide_img_width :=length(img_array[0]);
  indi_guide_img_height:=length(img_array);

  Ystop :=min(indi_guide_img_height,max(0,indi_guide_start_y+indi_guide_num_y));
  Ystart:=min(indi_guide_img_height,max(0,indi_guide_start_y));
  Xstop :=min(indi_guide_img_width ,max(0,indi_guide_start_x+indi_guide_num_x));   {left is 0}
  Xstart:=min(indi_guide_img_width ,max(0,indi_guide_start_x));

  w:=Xstop-Xstart;
  h:=Ystop-Ystart;
  if (w<=0) or (h<=0) then exit;

  setlength(indi_guide_the_img,h,w);                {[y,x], NOT swapped}

  for y:=Ystart to Ystop-1 do
    for x:=Xstart to Xstop-1 do
      indi_guide_the_img[y-Ystart,x-Xstart]:=
        min(65535,round(relative_gain*indi_guide_camera_exposure*img_array[y,x]+
                        indi_guide_camera_offset+randg(4*noise,noise)));

  {bias and dark frames see no sky; flat is a flood, close enough for a
   simulator and it keeps CCDciel's sequence editor happy}
  if indi_guide_frame_type in [1,2] then
    for y:=0 to h-1 do
      for x:=0 to w-1 do
        indi_guide_the_img[y,x]:=min(65535,indi_guide_camera_offset+
                                     round(randg(4*noise,noise)));

  if indi_guide_bin_x=2 then indi_guide_bin_2X2(indi_guide_the_img);

  ok:=(indi_guide_last_exposureduration>=0);        {startexposure was given}
  Result:=indi_guide_the_img;
end;

{ ====================================================================== }
{ FITS                                                                    }
{ ====================================================================== }

function PadTo(const s: string; n: integer): string;
begin
  Result:=s;
  while length(Result)<n do Result:=Result+' ';
  Result:=copy(Result,1,n);
end;

function PadLeftTo(const s: string; n: integer): string;
begin
  Result:=s;
  while length(Result)<n do Result:=' '+Result;
end;

function FitsCard(const key, val, comment: string): string;
{numeric or logical card: value right justified up to column 30}
begin
  Result:=PadTo(key,8)+'= '+PadLeftTo(val,20);
  if comment<>'' then Result:=Result+' / '+comment;
  Result:=PadTo(Result,80);
end;

function FitsCardStr(const key, val, comment: string): string;
{String card: quoted, left justified, PADDED to at least 8 characters inside
 the quotes but never truncated to 8 - that would silently turn
 'Sky Simulator Camera' into 'Sky Simu'.  A card is 80 characters, so the
 value is clipped only if it really does not fit, and any embedded quote is
 doubled as the standard requires.}
var v: string;
begin
  v:=StringReplace(val,#39,#39#39,[rfReplaceAll]);
  while length(v)<8 do v:=v+' ';
  if length(v)>68 then v:=copy(v,1,68);      {8 for the key, 2 for '= ', 2 quotes}
  Result:=PadTo(key,8)+'= '+#39+v+#39;
  if (comment<>'') and (length(Result)+3+length(comment)<=80) then
    Result:=Result+' / '+comment;
  Result:=PadTo(Result,80);
end;

function FitsRa(rax: double): string;
{OBJCTRA wants "hh mm ss.ss".  prepare_ra in sky_simulator_main produces
 "12: 34 56.7" which is NOT accepted by most readers, so format here.}
var h, m: integer; s, r: double;
begin
  r:=rax*12/pi;                       {radians to hours}
  while r<0 do r:=r+24;
  while r>=24 do r:=r-24;
  h:=trunc(r);
  m:=trunc((r-h)*60);
  s:=(r-h-m/60)*3600;
  Result:=Format('%.2d %.2d ',[h,m])+F(s,2);
end;

function FitsDec(decx: double): string;
{OBJCTDEC wants "+dd mm ss.s"}
var g, m: integer; s, d: double; sign: char;
begin
  if decx<0 then sign:='-' else sign:='+';
  d:=abs(decx)*180/pi;
  g:=trunc(d);
  m:=trunc((d-g)*60);
  s:=(d-g-m/60)*3600;
  Result:=sign+Format('%.2d %.2d ',[g,m])+F(s,1);
end;

function indi_guide_bayer_pattern: string;
begin
  {both offsets 0 = RGGB, x=1 GRBG, y=1 GBRG, both 1 BGGR}
  if (indi_guide_bayeroffset_X=0) and (indi_guide_bayeroffset_Y=0) then Result:='RGGB'
  else if (indi_guide_bayeroffset_X=1) and (indi_guide_bayeroffset_Y=0) then Result:='GRBG'
  else if (indi_guide_bayeroffset_X=0) and (indi_guide_bayeroffset_Y=1) then Result:='GBRG'
  else Result:='BGGR';
end;

function indi_guide_frame_type_str: string;
begin
  case indi_guide_frame_type of
    1: Result:='Bias';
    2: Result:='Dark';
    3: Result:='Flat';
  else Result:='Light';
  end;
end;

function indi_guide_camera_fits(out ok: boolean): ansistring;
{Build a complete FITS file in memory.  16 bit unsigned stored in a signed
 container via BZERO=32768, big endian, header and data both padded to a
 multiple of 2880 bytes.}
var
  ms  : TMemoryStream;
  hdr : ansistring;
  w, h, x, y, i, dum, remain, bin: integer;
  row : array of word;
  pad : ansistring;
  focal, pixsz: double;
begin
  Result:='';
  indi_guide_camera_build_image(ok);
  if (not ok) or (indi_guide_the_img=nil) then begin ok:=false; exit; end;

  h:=length(indi_guide_the_img);
  w:=length(indi_guide_the_img[0]);
  bin:=indi_guide_bin_x;

  {Focal length is the telescope's and is shared.  Pixel size is NOT: the
   form field belongs to the main camera, and the Alpaca guide camera keeps
   its own pixel_size_guide_alpaca for the same reason.}
  focal:=strtofloat2(form1.focallength1.text);
  pixsz:=indi_guide_pixelsizemicrometer;

  hdr:=FitsCard('SIMPLE','T','FITS header')+
       FitsCard('BITPIX','16','Bits per entry')+
       FitsCard('NAXIS','2','Number of dimensions')+
       FitsCard('NAXIS1',IntToStr(w),'length of x axis')+
       FitsCard('NAXIS2',IntToStr(h),'length of y axis')+
       {16 bit unsigned in a signed container}
       FitsCard('BZERO','32768','physical = BZERO + BSCALE * array_value')+
       FitsCard('BSCALE','1','physical = BZERO + BSCALE * array_value')+
       FitsCard('DATAMIN','0','Minimum data value')+
       FitsCard('DATAMAX','65535','Maximum data value')+
       FitsCardStr('DATE-OBS',indi_guide_last_exposurestarttime,'UTC start of exposure')+
       FitsCard('EXPTIME',F(indi_guide_last_exposureduration,3),'[s] Exposure time')+
       FitsCard('EXPOSURE',F(indi_guide_last_exposureduration,3),'[s] Exposure time')+
       FitsCard('XBINNING',IntToStr(bin),'Binning factor in width')+
       FitsCard('YBINNING',IntToStr(bin),'Binning factor in height')+
       FitsCard('XPIXSZ',F(pixsz*bin,3),'[um] Pixel size including binning')+
       FitsCard('YPIXSZ',F(pixsz*bin,3),'[um] Pixel size including binning')+
       FitsCard('FOCALLEN',F(focal,1),'[mm] Telescope focal length')+
       FitsCard('GAIN',IntToStr(indi_guide_camera_gain),'Camera gain')+
       FitsCard('OFFSET',IntToStr(indi_guide_camera_offset),'Camera offset')+
       FitsCard('CCD-TEMP',F(indi_guide_sensor_temperature,1),'[C] Sensor temperature')+
       FitsCard('SET-TEMP',F(indi_guide_set_temperature,1),'[C] Temperature setpoint')+
       FitsCardStr('IMAGETYP',indi_guide_frame_type_str,'Frame type')+
       FitsCardStr('INSTRUME',IndiGuideCameraName,'Camera')+
       FitsCardStr('TELESCOP','Sky Simulator','Mount')+
       FitsCardStr('SWCREATE','Sky Simulator for Ascom and Alpaca','')+
       {position hint for the plate solver.  ra0/dec0 are the J2000 centre
        the sky image was generated for, in radians.}
       FitsCardStr('OBJCTRA',FitsRa(ra0),'[hms] Centre RA, J2000')+
       FitsCardStr('OBJCTDEC',FitsDec(dec0),'[dms] Centre DEC, J2000')+
       FitsCard('EQUINOX','2000.0','Equinox of the OBJCT coordinates');

  if indi_guide_sensor_type=2 then
  begin
    hdr:=hdr+FitsCardStr('BAYERPAT',indi_guide_bayer_pattern,'Bayer colour pattern')+
             FitsCard('XBAYROFF',IntToStr(indi_guide_bayeroffset_X),'Bayer X offset')+
             FitsCard('YBAYROFF',IntToStr(indi_guide_bayeroffset_Y),'Bayer Y offset')+
             {row 0 of img_array is written first, so bottom-up}
             FitsCardStr('ROWORDER','BOTTOM-UP','First row is the bottom row');
  end;

  hdr:=hdr+PadTo('END',80);
  {pad the header to a whole number of 2880 byte records}
  remain:=length(hdr) mod 2880;
  if remain<>0 then hdr:=hdr+StringOfChar(' ',2880-remain);

  ms:=TMemoryStream.Create;
  try
    ms.WriteBuffer(hdr[1],length(hdr));

    setlength(row,w);
    for y:=0 to h-1 do
    begin
      for x:=0 to w-1 do
      begin
        {shift 0..65535 down to -32768..32767, then swap to big endian}
        dum:=indi_guide_the_img[y,x]-32768;
        row[x]:=swap(word(dum));
      end;
      ms.WriteBuffer(row[0],w*2);
    end;

    {data records are multiples of 2880 too}
    remain:=ms.Size mod 2880;
    if remain<>0 then
    begin
      pad:=StringOfChar(#0,2880-remain);
      ms.WriteBuffer(pad[1],length(pad));
    end;

    setlength(Result,ms.Size);
    ms.Position:=0;
    ms.ReadBuffer(Result[1],ms.Size);
  finally
    ms.Free;
  end;

  {do not hold on to a possibly large frame}
  for i:=0 to length(indi_guide_the_img)-1 do indi_guide_the_img[i]:=nil;
  indi_guide_the_img:=nil;
  ok:=true;
end;

function indi_guide_base64(const s: ansistring): ansistring;
var
  src, dst: TStringStream;
  enc: TBase64EncodingStream;
begin
  Result:='';
  src:=TStringStream.Create(s);
  dst:=TStringStream.Create('');
  try
    enc:=TBase64EncodingStream.Create(dst);
    try
      enc.CopyFrom(src,src.Size);
    finally
      enc.Free;          {the Free flushes the last partial group - required}
    end;
    Result:=dst.DataString;
  finally
    dst.Free;
    src.Free;
  end;
end;

{ ====================================================================== }
{ T_Indi_GuideCamera                                                           }
{ ====================================================================== }

constructor T_Indi_GuideCamera.Create;
begin
  inherited Create;
  DeviceName:=IndiGuideCameraName;
  FBlobEnabled:=false;
  FLastSeq:=indi_guide_exposure_seq;
  FLastExpValue:=-1;
  FLastTemp:=-999;
  FLastCooler:=not indi_guide_cooler_on;   {force a first push}
  FLastState:=isIdle;
  indi_guide_camera_refresh_size;
  FConnected:=False;
end;

{ ---------------- property senders ------------------------------------- }

procedure T_Indi_GuideCamera.SendConnection(def: boolean);
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

procedure T_Indi_GuideCamera.SendDriverInfo;
begin
  StartDef('Text','DRIVER_INFO','Driver Info',GroupInfo,isIdle,ipRO);
  AddDef('Text','DRIVER_NAME','Name',DeviceName);
  AddDef('Text','DRIVER_EXEC','Exec','indi_skysimulator_guide');
  AddDef('Text','DRIVER_VERSION','Version','1.0');
  AddDef('Text','DRIVER_INTERFACE','Interface','2');   {2 = CCD_INTERFACE}
  EndDef('Text');
end;

procedure T_Indi_GuideCamera.SendCcdInfo(def: boolean);
begin
  indi_guide_camera_refresh_size;
  if def then
  begin
    StartDef('Number','CCD_INFO','CCD Information',GroupInfo,isOk,ipRO);
    AddDef('Number','CCD_MAX_X','Max. Width',F(indi_guide_img_width,0),
           ' format="%.f" min="1" max="100000" step="1"');
    AddDef('Number','CCD_MAX_Y','Max. Height',F(indi_guide_img_height,0),
           ' format="%.f" min="1" max="100000" step="1"');
    AddDef('Number','CCD_PIXEL_SIZE','Pixel size (um)',
           F(indi_guide_pixelsizemicrometer,2),
           ' format="%.2f" min="0" max="100" step="0"');
    AddDef('Number','CCD_PIXEL_SIZE_X','Pixel size X',
           F(indi_guide_pixelsizemicrometer,2),
           ' format="%.2f" min="0" max="100" step="0"');
    AddDef('Number','CCD_PIXEL_SIZE_Y','Pixel size Y',
           F(indi_guide_pixelsizemicrometer,2),
           ' format="%.2f" min="0" max="100" step="0"');
    AddDef('Number','CCD_BITSPERPIXEL','Bits per pixel','16',
           ' format="%.f" min="8" max="64" step="0"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_INFO',isOk);
    AddOne('Number','CCD_MAX_X',F(indi_guide_img_width,0));
    AddOne('Number','CCD_MAX_Y',F(indi_guide_img_height,0));
    AddOne('Number','CCD_PIXEL_SIZE',F(indi_guide_pixelsizemicrometer,2));
    AddOne('Number','CCD_PIXEL_SIZE_X',F(indi_guide_pixelsizemicrometer,2));
    AddOne('Number','CCD_PIXEL_SIZE_Y',F(indi_guide_pixelsizemicrometer,2));
    AddOne('Number','CCD_BITSPERPIXEL','16');
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendExposure(def: boolean; st: TIndiState);
{Ekos reads CCD_EXPOSURE_VALUE while the exposure runs to drive its
 progress bar, so this is pushed from Poll with the REMAINING seconds and
 state Busy, then once with 0 and Ok when the frame is delivered.  A camera
 that only ever reports 0 looks hung.}
begin
  if def then
  begin
    StartDef('Number','CCD_EXPOSURE','Expose',GroupMain,st,ipRW);
    AddDef('Number','CCD_EXPOSURE_VALUE','Duration (s)',
           F(indi_guide_exposure_remaining,3),
           ' format="%5.2f" min="0" max="3600" step="0.01"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_EXPOSURE',st);
    AddOne('Number','CCD_EXPOSURE_VALUE',F(indi_guide_exposure_remaining,3));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendAbort(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Switch','CCD_ABORT_EXPOSURE','Abort',GroupMain,isIdle,ipRW,
             ' rule="AtMostOne"');
    AddDef('Switch','ABORT','Abort','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CCD_ABORT_EXPOSURE',st);
    AddOne('Switch','ABORT','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_GuideCamera.SendFrame(def: boolean; st: TIndiState);
{X, Y, WIDTH and HEIGHT are UNBINNED pixels, the libindi convention, which
 happens to be exactly how indi_guide_start_x/indi_guide_num_x are stored.}
begin
  indi_guide_camera_refresh_size;
  if def then
  begin
    StartDef('Number','CCD_FRAME','Frame',GroupImage,st,ipRW);
    AddDef('Number','X','Left',F(indi_guide_start_x,0),
           ' format="%.f" min="0" max="'+F(indi_guide_img_width,0)+'" step="1"');
    AddDef('Number','Y','Top',F(indi_guide_start_y,0),
           ' format="%.f" min="0" max="'+F(indi_guide_img_height,0)+'" step="1"');
    AddDef('Number','WIDTH','Width',F(indi_guide_num_x,0),
           ' format="%.f" min="1" max="'+F(indi_guide_img_width,0)+'" step="1"');
    AddDef('Number','HEIGHT','Height',F(indi_guide_num_y,0),
           ' format="%.f" min="1" max="'+F(indi_guide_img_height,0)+'" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_FRAME',st);
    AddOne('Number','X',F(indi_guide_start_x,0));
    AddOne('Number','Y',F(indi_guide_start_y,0));
    AddOne('Number','WIDTH',F(indi_guide_num_x,0));
    AddOne('Number','HEIGHT',F(indi_guide_num_y,0));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendBinning(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','CCD_BINNING','Binning',GroupImage,st,ipRW);
    AddDef('Number','HOR_BIN','X',F(indi_guide_bin_x,0),
           ' format="%.f" min="1" max="'+IntToStr(indi_guide_bin_maximum)+'" step="1"');
    AddDef('Number','VER_BIN','Y',F(indi_guide_bin_y,0),
           ' format="%.f" min="1" max="'+IntToStr(indi_guide_bin_maximum)+'" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_BINNING',st);
    AddOne('Number','HOR_BIN',F(indi_guide_bin_x,0));
    AddOne('Number','VER_BIN',F(indi_guide_bin_y,0));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendFrameType(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','CCD_FRAME_TYPE','Frame Type',GroupImage,isIdle,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','FRAME_LIGHT','Light',OnOff(indi_guide_frame_type=0));
    AddDef('Switch','FRAME_BIAS','Bias',OnOff(indi_guide_frame_type=1));
    AddDef('Switch','FRAME_DARK','Dark',OnOff(indi_guide_frame_type=2));
    AddDef('Switch','FRAME_FLAT','Flat',OnOff(indi_guide_frame_type=3));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CCD_FRAME_TYPE',isOk);
    AddOne('Switch','FRAME_LIGHT',OnOff(indi_guide_frame_type=0));
    AddOne('Switch','FRAME_BIAS',OnOff(indi_guide_frame_type=1));
    AddOne('Switch','FRAME_DARK',OnOff(indi_guide_frame_type=2));
    AddOne('Switch','FRAME_FLAT',OnOff(indi_guide_frame_type=3));
    EndSet('Switch');
  end;
end;

procedure T_Indi_GuideCamera.SendTemperature(def: boolean; st: TIndiState);
begin
  if def then
  begin
    StartDef('Number','CCD_TEMPERATURE','Temperature',GroupMain,st,ipRW);
    AddDef('Number','CCD_TEMPERATURE_VALUE','Temperature (C)',
           F(indi_guide_sensor_temperature,2),
           ' format="%5.2f" min="-50" max="50" step="0.1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_TEMPERATURE',st);
    AddOne('Number','CCD_TEMPERATURE_VALUE',F(indi_guide_sensor_temperature,2));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendCooler(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','CCD_COOLER','Cooler',GroupMain,isOk,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','COOLER_ON','On',OnOff(indi_guide_cooler_on));
    AddDef('Switch','COOLER_OFF','Off',OnOff(not indi_guide_cooler_on));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CCD_COOLER',isOk);
    AddOne('Switch','COOLER_ON',OnOff(indi_guide_cooler_on));
    AddOne('Switch','COOLER_OFF',OnOff(not indi_guide_cooler_on));
    EndSet('Switch');
  end;
end;

procedure T_Indi_GuideCamera.SendCompression(def: boolean);
begin
  if def then
  begin
    StartDef('Switch','CCD_COMPRESSION','Image compression',GroupImage,
             isIdle,ipRW,' rule="OneOfMany"');
    AddDef('Switch','CCD_COMPRESS','Compressed',OnOff(indi_guide_compress));
    AddDef('Switch','CCD_RAW','Raw',OnOff(not indi_guide_compress));
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','CCD_COMPRESSION',isOk);
    AddOne('Switch','CCD_COMPRESS',OnOff(indi_guide_compress));
    AddOne('Switch','CCD_RAW',OnOff(not indi_guide_compress));
    EndSet('Switch');
  end;
end;

procedure T_Indi_GuideCamera.SendGain(def: boolean);
begin
  if def then
  begin
    StartDef('Number','CCD_GAIN','Gain',GroupOpt,isOk,ipRW);
    AddDef('Number','GAIN','Gain',F(indi_guide_camera_gain,0),
           ' format="%.f" min="'+IntToStr(indi_guide_gain_min)+
           '" max="'+IntToStr(indi_guide_gain_max)+'" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_GAIN',isOk);
    AddOne('Number','GAIN',F(indi_guide_camera_gain,0));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendOffset(def: boolean);
begin
  if def then
  begin
    StartDef('Number','CCD_OFFSET','Offset',GroupOpt,isOk,ipRW);
    AddDef('Number','OFFSET','Offset',F(indi_guide_camera_offset,0),
           ' format="%.f" min="'+IntToStr(indi_guide_offset_min)+
           '" max="'+IntToStr(indi_guide_offset_max)+'" step="1"');
    EndDef('Number');
  end
  else
  begin
    StartSet('Number','CCD_OFFSET',isOk);
    AddOne('Number','OFFSET',F(indi_guide_camera_offset,0));
    EndSet('Number');
  end;
end;

procedure T_Indi_GuideCamera.SendCfa(def: boolean);
{Only meaningful for a Bayer sensor.  Clients use it to debayer without
 having to read the FITS header first.}
begin
  if indi_guide_sensor_type<2 then exit;
  if def then
  begin
    StartDef('Text','CCD_CFA','Bayer Info',GroupImage,isIdle,ipRO);
    AddDef('Text','CFA_OFFSET_X','Offset X',IntToStr(indi_guide_bayeroffset_X));
    AddDef('Text','CFA_OFFSET_Y','Offset Y',IntToStr(indi_guide_bayeroffset_Y));
    AddDef('Text','CFA_TYPE','Type',indi_guide_bayer_pattern);
    EndDef('Text');
  end
  else
  begin
    StartSet('Text','CCD_CFA',isOk);
    AddOne('Text','CFA_OFFSET_X',IntToStr(indi_guide_bayeroffset_X));
    AddOne('Text','CFA_OFFSET_Y',IntToStr(indi_guide_bayeroffset_Y));
    AddOne('Text','CFA_TYPE',indi_guide_bayer_pattern);
    EndSet('Text');
  end;
end;

procedure T_Indi_GuideCamera.SendConfigProcess(def: boolean);
{CCDciel refuses to finish connecting to a CCD until it has seen
 CONFIG_PROCESS, and simply times out after a minute with
 "Missing property CONFIG_PROCESS" if it never arrives.  Every real
 libindi driver gets this vector for free from INDI::DefaultDevice, so
 nothing advertises that a hand written driver has to supply it.

 The simulator has no configuration file: all settings live in the
 Sky Simulator ini and in the form.  So the property exists to satisfy
 the client and the buttons are acknowledged without doing anything.
 The mount does not need this because CCDciel only checks it for a
 camera.}
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

procedure T_Indi_GuideCamera.SendUploadMode(def: boolean);
{Another vector every libindi CCD inherits and that a hand written driver
 has to supply.  CCDciel and Ekos both set it to UPLOAD_CLIENT before they
 will expose.  Local and Both mean "write the file on the server", which a
 simulator has no business doing, so only Client is ever On.}
begin
  if def then
  begin
    StartDef('Switch','UPLOAD_MODE','Upload',GroupOpt,isOk,ipRW,
             ' rule="OneOfMany"');
    AddDef('Switch','UPLOAD_CLIENT','Client','On');
    AddDef('Switch','UPLOAD_LOCAL','Local','Off');
    AddDef('Switch','UPLOAD_BOTH','Both','Off');
    EndDef('Switch');
  end
  else
  begin
    StartSet('Switch','UPLOAD_MODE',isOk);
    AddOne('Switch','UPLOAD_CLIENT','On');
    AddOne('Switch','UPLOAD_LOCAL','Off');
    AddOne('Switch','UPLOAD_BOTH','Off');
    EndSet('Switch');
  end;
end;

procedure T_Indi_GuideCamera.DefineBlob;
{Emitted by hand: defBLOB is an empty element and needs no value, which the
 generic AddDef helper cannot express.  Kept here so indi_server.pas does
 not have to grow a BLOB special case.}
begin
  Emit('<defBLOBVector device="'+XmlEsc(DeviceName)+'" name="CCD1"'+
       ' label="Image Data" group="'+GroupImage+'" state="Idle" perm="ro"'+
       ' timeout="60" timestamp="'+IndiTimeStamp+'">'+CRLF2);
  Emit('  <defBLOB name="CCD1" label="Image"/>'+CRLF2);
  Emit('</defBLOBVector>'+CRLF2);
end;

procedure T_Indi_GuideCamera.SendBlob;
{size = the UNCOMPRESSED byte count, len = the encoded length, format the
 file extension.  Clients use size to pre-allocate and reject a mismatch.

 INDI is a broadcast protocol: one finished exposure goes to every client
 that enabled BLOBs, and CCDciel uses a different connection for
 enableBLOB than for CCD_EXPOSURE.  So the frame is built once per
 exposure and cached at unit level; the second and later clients reuse it
 instead of re-encoding a few megabytes each.

 The cache is keyed on indi_guide_exposure_seq, which startexposure increments,
 so a new exposure always invalidates it.}
var
  b64: ansistring;
  ok : boolean;
begin
  {A client that never sent enableBLOB is normal once several connections
   exist - stay silent, do not log.}
  if not FBlobEnabled then exit;

  {build only if this exposure has not been encoded yet}
  if indi_guide_fits_cache_seq <> indi_guide_exposure_seq then
  begin
    indi_guide_fits_cache := indi_guide_camera_fits(ok);
    if (not ok) or (indi_guide_fits_cache = '') then
    begin
      indi_guide_fits_cache := '';
      LogMessage('No image available');
      exit;
    end;
    indi_guide_fits_cache_seq := indi_guide_exposure_seq;
  end;

  b64 := indi_guide_base64(indi_guide_fits_cache);

  Emit('<setBLOBVector device="' + XmlEsc(DeviceName) + '" name="CCD1"' +
       ' state="Ok" timestamp="' + IndiTimeStamp + '">' + CRLF2);
  Emit('  <oneBLOB name="CCD1" size="' + IntToStr(length(indi_guide_fits_cache)) +
       '" format=".fits" len="' + IntToStr(length(b64)) + '">' + CRLF2);
  Emit(b64 + CRLF2);
  Emit('  </oneBLOB>' + CRLF2);
  Emit('</setBLOBVector>' + CRLF2);
end;

{ ---------------- definition dump --------------------------------------- }

procedure T_Indi_GuideCamera.DefineDeviceProperties;
{Everything except CONNECTION and DRIVER_INFO.  A libindi driver defines
 these from updateProperties(), i.e. only AFTER the client has connected,
 so clients written against that model arm their wait for CCD_INFO only
 after sending CONNECT and miss anything defined earlier.  Called twice:
 once on getProperties, and again on CONNECT.  Re-defining a property is
 legal INDI, the client simply replaces its copy.}
var st: TIndiState;
begin
  if indi_guide_camera_state=2 then st:=isBusy else st:=isIdle;

  SendCcdInfo(true);
  SendExposure(true,st);
  SendAbort(true,isIdle);
  SendFrame(true,isOk);
  SendBinning(true,isOk);
  SendFrameType(true);
  SendTemperature(true,isOk);
  SendCooler(true);
  SendCompression(true);
  SendGain(true);
  SendOffset(true);
  SendCfa(true);
  SendUploadMode(true);
  DefineBlob;
end;

procedure T_Indi_GuideCamera.DefineAll;
begin
  SendConnection(true);
  SendDriverInfo;
  SendConfigProcess(true);
  DefineDeviceProperties;
end;

{ ---------------- command handling -------------------------------------- }

procedure T_Indi_GuideCamera.HandleElement(e: TIndiElement);
var v: string;
begin
  if e=nil then exit;

  if e.Tag='enableBLOB' then
  begin
    {Never, Also or Only.  Anything other than Never enables delivery.}
    v:=UpperCase(Trim(e.Content));
    FBlobEnabled:=(v<>'') and (v<>'NEVER');
    exit;
  end;

  if e.Tag='newSwitchVector' then DoSwitch(e)
  else if e.Tag='newNumberVector' then DoNumber(e);
  {this device has no writable text vector}
end;

procedure T_Indi_GuideCamera.DoSwitch(e: TIndiElement);
var nm, sw: string;
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

  if nm='CCD_ABORT_EXPOSURE' then
  begin
    indi_guide_camera_abort;
    FLastSeq:=indi_guide_exposure_seq;
    SendAbort(false,isOk);
    SendExposure(false,isIdle);
    FLastState:=isIdle; FLastExpValue:=0;
    LogMessage('Exposure aborted');
    exit;
  end;

  if nm='CCD_FRAME_TYPE' then
  begin
    if sw='FRAME_LIGHT' then indi_guide_frame_type:=0
    else if sw='FRAME_BIAS' then indi_guide_frame_type:=1
    else if sw='FRAME_DARK' then indi_guide_frame_type:=2
    else if sw='FRAME_FLAT' then indi_guide_frame_type:=3;
    SendFrameType(false);
    exit;
  end;

  if nm='CCD_COOLER' then
  begin
    indi_guide_cooler_on:=(sw='COOLER_ON');
    if indi_guide_cooler_on then
      indi_guide_sensor_temperature:=indi_guide_set_temperature
    else
      indi_guide_sensor_temperature:=20;
    SendCooler(false);
    SendTemperature(false,isOk);
    FLastCooler:=indi_guide_cooler_on;
    FLastTemp:=indi_guide_sensor_temperature;
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

  if nm='UPLOAD_MODE' then
  begin
    {stay on Client whatever was asked: the simulator writes no files}
    if sw<>'UPLOAD_CLIENT' then
      LogMessage('Only client upload is supported, staying on Client');
    SendUploadMode(false);
    exit;
  end;

  if nm='CCD_COMPRESSION' then
  begin
    {only the raw path is implemented; acknowledge but stay on raw so the
     client is never left waiting for a z compressed BLOB that never comes}
    if sw='CCD_COMPRESS' then
      LogMessage('Compressed BLOBs are not implemented, staying on raw');
    indi_guide_compress:=false;
    SendCompression(false);
    exit;
  end;
end;

procedure T_Indi_GuideCamera.DoNumber(e: TIndiElement);
var
  nm: string;
  v: double;
  err: integer;
begin
  nm:=e.A('name');

  if nm='CCD_EXPOSURE' then
  begin
    v:=e.ChildNum('CCD_EXPOSURE_VALUE',-1);
    if v<0 then exit;
    indi_guide_camera_refresh_size;
    indi_guide_camera_startexposure(v,err);
    if err=2 then
    begin
      LogMessage('Subframe outside the sensor, exposure refused');
      indi_guide_camera_abort;
      FLastSeq:=indi_guide_exposure_seq;
      SendExposure(false,isAlert);
      FLastState:=isAlert;
      exit;
    end;
    SendExposure(false,isBusy);
    FLastState:=isBusy;
    FLastExpValue:=indi_guide_exposure_remaining;
    exit;
  end;

  if nm='CCD_FRAME' then
  begin
    indi_guide_camera_refresh_size;
    indi_guide_start_x:=round(e.ChildNum('X',indi_guide_start_x));
    indi_guide_start_y:=round(e.ChildNum('Y',indi_guide_start_y));
    indi_guide_num_x  :=round(e.ChildNum('WIDTH',indi_guide_num_x));
    indi_guide_num_y  :=round(e.ChildNum('HEIGHT',indi_guide_num_y));
    {clamp here as well as in startexposure: an out of range frame that is
     never corrected would make every later exposure fail}
    indi_guide_start_x:=max(0,min(indi_guide_img_width -1,indi_guide_start_x));
    indi_guide_start_y:=max(0,min(indi_guide_img_height-1,indi_guide_start_y));
    indi_guide_num_x  :=max(1,min(indi_guide_img_width -indi_guide_start_x,indi_guide_num_x));
    indi_guide_num_y  :=max(1,min(indi_guide_img_height-indi_guide_start_y,indi_guide_num_y));
    SendFrame(false,isOk);
    exit;
  end;

  if nm='CCD_BINNING' then
  begin
    indi_guide_bin_x:=max(1,min(indi_guide_bin_maximum,round(e.ChildNum('HOR_BIN',indi_guide_bin_x))));
    indi_guide_bin_y:=indi_guide_bin_x;      {no asymmetric binning, as in Alpaca}
    SendBinning(false,isOk);
    exit;
  end;

  if nm='CCD_TEMPERATURE' then
  begin
    indi_guide_set_temperature:=min(max(-40,e.ChildNum('CCD_TEMPERATURE_VALUE',
                                                 indi_guide_set_temperature)),50);
    if indi_guide_cooler_on then
      indi_guide_sensor_temperature:=indi_guide_set_temperature
    else
      indi_guide_sensor_temperature:=20;
    {reached immediately: the simulation has no thermal mass}
    SendTemperature(false,isOk);
    FLastTemp:=indi_guide_sensor_temperature;
    exit;
  end;

  if nm='CCD_GAIN' then
  begin
    indi_guide_camera_gain:=min(max(round(e.ChildNum('GAIN',indi_guide_camera_gain)),
                              indi_guide_gain_min),indi_guide_gain_max);
    SendGain(false);
    exit;
  end;

  if nm='CCD_OFFSET' then
  begin
    indi_guide_camera_offset:=min(max(round(e.ChildNum('OFFSET',indi_guide_camera_offset)),
                                indi_guide_offset_min),indi_guide_offset_max);
    SendOffset(false);
    exit;
  end;
end;

{ ---------------- periodic push ----------------------------------------- }

procedure T_Indi_GuideCamera.Poll;
begin
  if indi_guide_camera_state=2 then
  begin
    {count down for the client's progress bar; 0.1 s is well below the
     one second simulation step so nothing is missed}
    if abs(indi_guide_exposure_remaining-FLastExpValue)>0.1 then
    begin
      SendExposure(false,isBusy);
      FLastExpValue:=indi_guide_exposure_remaining;
      FLastState:=isBusy;
    end;
    exit;
  end;

  if indi_guide_exposure_seq<>FLastSeq then
  begin
    if indi_guide_camera_imageready then
    begin
      SendBlob;                    {no-op if this client did not enable BLOBs}
      FLastSeq:=indi_guide_exposure_seq;
      SendExposure(false,isOk);
      FLastExpValue:=0;
      FLastState:=isOk;
    end;
    exit;
  end;

  {slow moving properties, only pushed when they actually changed}
  if abs(indi_guide_sensor_temperature-FLastTemp)>0.05 then
  begin
    SendTemperature(false,isOk);
    FLastTemp:=indi_guide_sensor_temperature;
  end;

  if indi_guide_cooler_on<>FLastCooler then
  begin
    SendCooler(false);
    FLastCooler:=indi_guide_cooler_on;
  end;
end;

end.
