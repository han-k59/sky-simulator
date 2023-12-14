unit sky_simulator_main; {Sky simulator. This program will update the camera image based on mount position and focuser position.}

{Copyright (C) 2019, 2022 by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,dateutils,strutils,
  {$ifdef mswindows}
  Windows,
  URLMon,   {For downloading files}
  comobj,   {for Ascom link}
  {$else}
  LCLintf, {for GetCursorPos}
  sky_simulator_unit_download, {Not available in Linux}
  {$endif}

  {$ifdef Darwin}{MacOS}
  {$else}
  {$endif}
  fileutil,
  LazSysUtils, {nowUtc}

  Menus,
  ExtCtrls, ComCtrls, Buttons,
  math,{for sincos}
  lcltype, {Trgbtriple}
  Variants, clipbrd, sky_annotation,
  cu_alpacaserver, cu_alpacadevice, alpaca_mount_protocol, alpaca_camera_protocol,alpaca_guidecamera_protocol,alpaca_focuser_protocol,alpaca_rotator_protocol, Types;


type

  { TForm1 }

  TForm1 = class(TForm)
    activate_filelog1: TCheckBox;
    alpaca_adress1: TEdit;
    alpaca_groupBox1: TGroupBox;
    alpaca_port1: TEdit;
    alpaca_port_number1: TLabel;
    Angle2: TEdit;
    arrowleft1: TLabel;
    Alpaca_tab1: TTabSheet;
    ascom_image1: TGroupBox;
    azimuth_error1: TEdit;
    azimuth_updown1: TUpDown;
    backlash1: TEdit;
    Button1: TButton;
    buttonNorth1: TButton;
    buttonEast1: TButton;
    buttonSouth1: TButton;
    buttonWest1: TButton;
    calculator1: TButton;
    NSswapped1: TCheckBox;
    pointing1: TLabel;
    mount_type1: TComboBox;
    DecPulseReverses1: TCheckBox;
    fliptext1: TCheckBox;
    fast_simulation1: TCheckBox;
    filter1: TEdit;
    focuser_position7: TLabel;
    Label12: TLabel;
    Label17: TLabel;
    Memo2: TMemo;
    menucopy1: TMenuItem;
    Menufind1: TMenuItem;
    menufindnext1: TMenuItem;
    MenuItem23: TMenuItem;
    PopupMenu_memo1: TPopupMenu;
    select_all1: TMenuItem;
    manipulations1: TComboBox;
    rotator_setpoint1: TLabel;
    rotator_reverse1: TCheckBox;
    clear_log_button1: TButton;
    camera_alpaca1: TRadioButton;
    camera_ascom1: TRadioButton;
    activate_log1: TCheckBox;
    connect_focuser1: TBitBtn;
    connect_rotator1: TBitBtn;
    connect_mount1: TBitBtn;
    database_selected1: TRadioButton;
    dec1: TLabel;
    Discovery_Info: TLabel;
    elevation: TEdit;
    elevation_error1: TEdit;
    elevation_updown1: TUpDown;
    esobutton1: TButton;
    eso_selected1: TRadioButton;
    exit1: TButton;
    flipH1: TCheckBox;
    flipV1: TCheckBox;
    focuser_alpaca1: TRadioButton;
    rotator_position1: TLabel;
    rotator_alpaca1: TRadioButton;
    focuser_ascom1: TRadioButton;
    rotator_ascom1: TRadioButton;
    focuser_position1: TLabel;
    focuser_position2: TLabel;
    focuser_position3: TLabel;
    focuser_position4: TLabel;
    focuser_position5: TLabel;
    focuser_position6: TLabel;
    focusfactor1: TLabel;
    focus_at1: TEdit;
    focus_range1: TEdit;
    go_default1: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    height1: TEdit;
    height_pixels1: TEdit;
    Image1: TImage;
    imagecounter1: TLabel;
    info5: TLabel;
    internetESO1: TEdit;
    internetskyview1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    instruction2: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label_azimuth1: TLabel;
    Label_elevation1: TLabel;
    Label_latitude1: TLabel;
    Label_longitude1: TLabel;
    latitude1: TEdit;
    longitude1: TEdit;
    maxmagn1: TEdit;
    Memo1: TMemo;
    mount1: TLabel;
    mount_alpaca1: TRadioButton;
    mount_ascom1: TRadioButton;
    mount_error1: TComboBox;
    mount_indication1: TLabel;
    PageControl1: TPageControl;
    path_to_image1: TEdit;
    plotted_info1: TComboBox;
    polar_alignment_error1: TCheckBox;
    ra1: TLabel;
    real_position1: TLabel;
    savesettings1: TButton;
    scale1: TLabel;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    selectpath1: TBitBtn;
    Server_Info: TLabel;
    skyviewbutton1: TButton;
    skyview_selected1: TRadioButton;
    start_button1: TButton;
    star_database1: TComboBox;
    hotpixels1: TComboBox;
    StatusBar1: TStatusBar;
    stopbutton1: TButton;
    Control_TabSheet1: TTabSheet;
    Image_TabSheet2: TTabSheet;
    Log: TTabSheet;
    telescope_position2000_dec1: TLabel;
    telescope_position2000_ra1: TLabel;
    Timer1: TTimer;
    UpDown1: TUpDown;
    width1: TLabel;
    width_pixels1: TEdit;
    procedure Angle2Change(Sender: TObject);
    procedure azimuth_error1Exit(Sender: TObject);
    procedure azimuth_updown1Changing(Sender: TObject; var AllowChange: Boolean
      );
    procedure backlash1Exit(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure buttonEast1Click(Sender: TObject);
    procedure buttonNorth1Click(Sender: TObject);
    procedure buttonSouth1Click(Sender: TObject);
    procedure buttonWest1Click(Sender: TObject);
    procedure calculator1Click(Sender: TObject);
    procedure clear_log_button1Click(Sender: TObject);
    procedure connect_rotator1Click(Sender: TObject);
    procedure elevation_error1Exit(Sender: TObject);
    procedure elevation_updown1Changing(Sender: TObject;  var AllowChange: Boolean);
    procedure focuser_alpaca1Change(Sender: TObject);
    procedure focus_at1Exit(Sender: TObject);
    procedure focus_range1Exit(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure go_default1Click(Sender: TObject);
    procedure esobutton1Click(Sender: TObject);
    procedure exit1Click(Sender: TObject);
    procedure flipH1Change(Sender: TObject);
    procedure flipV1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure height1Exit(Sender: TObject);
    procedure height_pixels1Exit(Sender: TObject);
    procedure hotpixels1Change(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Image_TabSheet2Show(Sender: TObject);
    procedure internetESO1Exit(Sender: TObject);
    procedure internetskyview1Exit(Sender: TObject);
    procedure latitude1Exit(Sender: TObject);
    procedure longitude1Exit(Sender: TObject);
    procedure maxmagn1Change(Sender: TObject);
    procedure menucopy1Click(Sender: TObject);
    procedure Menufind1Click(Sender: TObject);
    procedure menufindnext1Click(Sender: TObject);
    procedure mount_alpaca1Change(Sender: TObject);
    procedure mount_error1Change(Sender: TObject);
    procedure mount_type1Change(Sender: TObject);
    procedure plotted_info1Change(Sender: TObject);
    procedure polar_alignment_error1Change(Sender: TObject);
    procedure camera_alpaca1Change(Sender: TObject);
    procedure rotator_alpaca1Change(Sender: TObject);
    procedure savesettings1Click(Sender: TObject);
    procedure selectpath1Click(Sender: TObject);
    procedure select_all1Click(Sender: TObject);
    procedure skyviewbutton1Click(Sender: TObject);
    procedure start_button1Click(Sender: TObject);
    procedure star_database1Change(Sender: TObject);
    procedure star_database1CloseUp(Sender: TObject);
    procedure star_database1DropDown(Sender: TObject);
    procedure stopbutton1Click(Sender: TObject);
    procedure connect_focuser1Click(Sender: TObject);
    procedure connect_mount1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure skyview_selected1Change(Sender: TObject);
    procedure manipulations1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure width_pixels1Exit(Sender: TObject);

  private
    AlpacaIPAddr, AlpacaIPPort, AlpacaDiscoveryPort : string;
    AlpacaServer : T_AlpacaServer;
    MyMount      : T_Alpaca_Mount;
    MyCam        : T_Alpaca_Cam;
    MyGuideCam   : T_Alpaca_GuideCam;
    MyFocuser    : T_Alpaca_Focuser;
    MyRotator    : T_Alpaca_Rotator;
    procedure ShowError(var msg:string);{for Alpaca}
    procedure ShowMsg(var msg:string);{for Alpaca}
    procedure PortMsg(var msg:string);{for Alpaca}
    procedure DiscoveryPortMsg(var msg:string);{for Alpaca}
  public

  end;

var
  Form1: TForm1;


  ra_telescope_2000,dec_telescope_2000,ra_mount_indication,dec_mount_indication,ra_mount_indication_2000,dec_mount_indication_2000,jd,sidereal_time   : double;
  focuser_position,side_of_pier             : integer;
  dss_bitmap                                : graphics.TBitmap;

  documents_path : string='';
  application_path:string='';
const
  pi_=PI; {for debugger}
  graylevel=1000; //for annotation text

  function prepare_ra(rax:double):string; {radialen to text, format 24: 00 00.0 }
  function prepare_dec(decx:double):string; {radialen to text, format 90d 00 00}
  procedure calc_jd; {get julian day for date_obs, so the start of the observation}
  procedure memo2_message(s: string);{message to memo2}
  procedure calc_sidereal_time(longitude: double);{local sidereal time}

type
   Timg = array of array of word;

implementation

{$R *.lfm}

var
  oldreal_position,real_position,rotator_position,oldrotator_position : integer;
  rotator_reverse : boolean;
const
  esc_pressed: boolean=false ;
  save_thesettings   : boolean=true;
  ascom_mount_connected    : boolean=false;
  ascom_focuser_connected  : boolean=false;
  ascom_rotator_connected  : boolean=false;
  imagecounter: integer=0;
  update_required       : boolean=false;
  fl_info    : string='';{for calculator}
  crpix_info : string='';{for calculator}
  old_unix_time : longint=0;

  down_xy_valid: boolean=false;{required for Linux GTK.}

var
  ascom_mount           : variant; {for Ascom, telescope}
  ascom_focuser         : variant; {for Ascom, focuser}
  ascom_rotator         : variant; {for Ascom, rotator}
  ascom_mount_driver    : string ='ASCOM.Simulator.Telescope';
  ascom_focuser_driver  : string ='FocusSim.Focuser';
  ascom_rotator_driver  : string ='ASCOM.Simulator.Rotator';
  user_path    : string;{c:\users\name\appdata\local\sky_simulator   or ~/home/.config/sky_simulator}

  oldRA_telescope,oldDEC_telescope, starttick,old_starttick,pushbuttonRA,pushbuttonDEC : double;
  down_x, down_y: integer;
  PatternToFind : string=''; {for memo1 popup menu }
  position_find: Integer; {for memo1 popup menu}



procedure memo2_message(s: string);{message to memo2}
begin
  form1.statusbar1.simpletext:=s;
  form1.memo2.lines.add(TimeToStr(time)+'  '+s); {fill memo2 with log}
end;



procedure TForm1.ShowError(var msg:string); {Alpaca}
begin
  Memo1.Lines.Add('Error: '+msg);
end;


procedure log_to_file(logf,mess : string);{for testing}
var
  f   :  textfile;
begin
  assignfile(f,logf);
  if fileexists(logf)=false then
    rewrite(f)
  else
    append(f);
  writeln(f,mess);
  closefile(f);
end;

function time_inc_ms : string;
var
  myDate: TDateTime;
  myYear, myMonth, myDay: Word;
  myHour, myMin, mySec, myMilli: Word;
  milisecstr: string;
begin
  myDate := Now;
  DecodeDateTime(myDate, myYear, myMonth, myDay, myHour, myMin, mySec, myMilli);
  milisecstr:=inttostr(myMilli);
  while length(milisecstr)<3 do milisecstr:='0'+milisecstr;
  result:=inttostr(myhour)+':'+inttostr(mymin)+':'+inttostr(mysec)+'.'+milisecstr;
end;

procedure TForm1.ShowMsg(var msg:string);{Alpaca}
var
  len : integer;
  duration      : double;
  filter: string;
begin
  if activate_log1.checked then
  begin
    filter:=filter1.text;
    trim(filter);
    if length(filter)>1 then
      if pos(filter,msg)=0 then exit;

    if memo1.lines.count>400 then
    begin
      len:=length(memo1.text);
      memo1.text:=copy(memo1.text,len div 2, 99999999999999);{quick method to half log, Memo1.Lines.delete(0) is too slow}
    end;

    starttick:=gettickcount64;
    duration:=starttick - old_starttick;

    if ((duration>2000) and (duration<1000000)) then
       Memo1.Lines.Add('■■■■■■■■■ Duration: '+ floattostr(round((starttick - old_starttick)/100)/10)+' sec.');
    old_starttick:=starttick;


    Memo1.Lines.Add(time_inc_ms+'  '+msg);{add to log}
  end;
  if activate_filelog1.checked then
    log_to_file(documents_path+'alpaca_log.txt',msg);
end;


procedure TForm1.PortMsg(var msg:string);{Alpaca}
begin
  Server_Info.Caption:='Server running on port '+msg;
end;


procedure TForm1.DiscoveryPortMsg(var msg:string);{Alpaca}
begin
  Discovery_Info.Caption:='Discovery running on port '+msg;
end;

procedure start_alpaca(cam,foc,mount,rot: boolean);
begin
  if ((cam=false) and (foc=false) and  (mount=false)) then exit; {no need for server}

  with form1 do
  begin
    if AlpacaServer<>nil then exit; {already started}

    AlpacaIPAddr:=alpaca_adress1.text; {'0.0.0.0'};
    AlpacaIPPort:=alpaca_port1.text; {'11111'}
    AlpacaDiscoveryPort:='32227';

    Server_Info.Caption:='';
    Memo1.Clear;

    AlpacaServer:=T_AlpacaServer.Create(nil);
    AlpacaServer.ServerName:='Alpaca server';
    AlpacaServer.onShowError:=@ShowError;
    AlpacaServer.onShowMsg:=@ShowMsg;
    AlpacaServer.onPortMsg:=@PortMsg;
    AlpacaServer.onDiscoveryPortMsg:=@DiscoveryPortMsg;

    if mount then
    begin
       MyMount:=T_Alpaca_Mount.Create(nil);
       AlpacaServer.AddDevice(telescope,MyMount);
    end;
    if cam then
    begin
      MyCam:=T_Alpaca_Cam.Create(nil);
      AlpacaServer.AddDevice(camera,MyCam);
      MyGuideCam:=T_Alpaca_GuideCam.Create(nil);
      AlpacaServer.AddDevice(camera,MyGuideCam);
    end;
    if foc then
    begin
      MyFocuser:=T_Alpaca_focuser.Create(nil);
      AlpacaServer.AddDevice(focuser,MyFocuser);
    end;
    if rot then
    begin
      MyRotator:=T_Alpaca_rotator.Create(nil);
      AlpacaServer.AddDevice(rotator,MyRotator);
    end;


    AlpacaServer.IPAddr:=AlpacaIPAddr;
    AlpacaServer.IPPort:=AlpacaIPPort;
    AlpacaServer.DiscoveryPort:=AlpacaDiscoveryPort;
    AlpacaServer.StartServer;
  end;
end;

procedure stop_alpaca;
var i: integer;
begin
  with form1 do
  begin
    if AlpacaServer<>nil then
    begin
      if myMount<>nil then
              MyMount.SetConnected(False);
      if myCam<>nil then
          MyCam.SetConnected(False);
      if myFocuser<>nil then
         MyFocuser.SetConnected(False);
      if myRotator<>nil then
         MyRotator.SetConnected(False);

      AlpacaServer.StopServer;
      for i:=0 to 10 do begin
        sleep(200);
        Application.ProcessMessages;
      end;
      AlpacaServer.Free;
    end;
  end;
end;


Procedure GetDocumentsPath;
{$IFDEF fpc}
  {$ifdef mswindows}
  var
     PIDL : PItemIDList;
     Folder : array[0..MAX_PATH] of Char;
     const CSIDL_PERSONAL = $0005;
  begin
     SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, PIDL);
     SHGetPathFromIDList(PIDL, Folder);
     documents_path:=Folder+'\';{FPC mswindows solution}
  end;
  {$ELSE}{unix}
  begin
   {$IFDEF darwin}
    documents_path:=expandfilename('~/Documents/');{Will be created if it doesn't exist}
   {$ELSE} {linux}
    documents_path:=expandfilename('~/');{home, will be created if it doesn't exist}
   {$ENDIF}
  end;
  {$ENDIF}
{$ELSE}{delphi}
begin
   documents_path:=TPath.GetDocumentsPath+'\';{delphi solution}
end;
{$ENDIF}


function load_settings(lpath: string)  : boolean;
var
  dum : string;
  i               : integer;
  initstring :tstrings; {settings for save and loading}

  Procedure get_float(var float: double;s1 : string); {this give much smaller exe file then using strtofloat}
      var s2:string; err:integer; r:double;
      begin
        s2:=initstring.Values[s1];
        val(s2,r,err);
        if err=0 then float:=r;
      end;

  Procedure get_int(var integ: integer;s1 : string);{this give much smaller exe file then using strtoint}
      var s2:string; err:integer; r:integer;
      begin
        s2:=initstring.Values[s1];
        val(s2,r,err);
        if err=0 then integ:=r;
      end;

  Function get_boolean(s1 : string;default1:boolean): boolean;
      var s2:string; err:integer; r:integer;
      begin
        s2:=initstring.Values[s1];
        val(s2,r,err);
        if err<>0 then result:=default1 {if no data, result is default1}
        else
        begin
           if r<=0 then result:=false
           else result:=true;
        end;
      end;

begin
  result:=false;{assume failure}


  initstring := Tstringlist.Create;
   with initstring do
   begin
     try
      loadfromFile(lpath); { load from file}
     except
       initstring.Free;
       form1.path_to_image1.text:=documents_path;{set default path}
       form1.top:=0;{for case the form was not set at the main screen}
       form1.left:=0;
       exit; {no cfg file}
     end;
   end;
  result:=true;
  with form1 do
  begin
    i:=form1.left;get_int(i,'window_left2'); form1.left:=i;
    i:=form1.top;get_int(i,'window_top2'); form1.top:=i;
    i:=form1.height;get_int(i,'window_height'); form1.height:=i;
    i:=form1.width;get_int(i,'window_width'); form1.width:=i;

    form1.skyview_selected1.checked:=get_boolean('skyview',true);
    form1.eso_selected1.checked:=get_boolean('eso',false);
    form1.database_selected1.checked:=get_boolean('database',false);
    form1.flipv1.checked:=get_boolean('flipV',false);
    form1.flipH1.checked:=get_boolean('flipH',false);

    form1.mount_alpaca1.checked:=get_boolean('mount_alpaca',false);
    form1.focuser_alpaca1.checked:=get_boolean('focuser_alpaca',false);
    form1.camera_alpaca1.checked:=get_boolean('camera_alpaca',false);
    form1.rotator_alpaca1.checked:=get_boolean('rotator_alpaca',false);
    form1.DecPulseReverses1.checked:=get_boolean('decreverses',true);
    form1.NSswapped1.checked:=get_boolean('NSswapped',false);

    form1.fast_simulation1.checked:=get_boolean('fastsim',true);


    form1.polar_alignment_error1.checked:=get_boolean('polar_e',false);
    dum:=initstring.Values['elevation_e']; if dum<>'' then form1.elevation_error1.text:=dum;
    dum:=initstring.Values['azimuth_e']; if dum<>'' then form1.azimuth_error1.text:=dum;
    dum:=initstring.Values['latitude']; if dum<>'' then form1.latitude1.text:=dum;
    dum:=initstring.Values['longtude']; if dum<>'' then form1.longitude1.text:=dum;

    dum:=initstring.Values['height']; if dum<>'' then form1.height1.text:=dum;
    dum:=initstring.Values['width_pixels']; if dum<>'' then form1.width_pixels1.text:=dum;
    dum:=initstring.Values['height_pixels']; if dum<>'' then form1.height_pixels1.text:=dum;
    dum:=initstring.Values['star_database']; if dum<>'' then form1.star_database1.text:=dum;
    dum:=initstring.Values['max_magn']; if dum<>'' then form1.maxmagn1.text:=dum;

    dum:=initstring.Values['path_image']; if dum<>'' then path_to_image1.text:=dum;

    dum:=initstring.Values['crota2']; if dum<>'' then angle2.text:=dum;

    dum:=initstring.Values['focus_at']; if dum<>'' then focus_at1.text:=dum;
    dum:=initstring.Values['focus_range']; if dum<>'' then focus_range1.text:=dum;
    dum:=initstring.Values['backlash']; if dum<>'' then backlash1.text:=dum;
    dum:=initstring.Values['hot_pixels']; if dum<>'' then hotpixels1.text:=dum;

    dum:=initstring.Values['mount']; if dum<>'' then ascom_mount_driver:=dum;
    dum:=initstring.Values['focuser']; if dum<>'' then ascom_focuser_driver:=dum;
    dum:=initstring.Values['rotator']; if dum<>'' then ascom_rotator_driver:=dum;

    dum:=initstring.Values['skyview_url']; if dum<>'' then form1.internetSkyview1.text:=dum;
    dum:=initstring.Values['eso_url']; if dum<>'' then form1.internetESO1.text:=dum;

    i:=form1.mount_error1.itemindex;get_int(i,'mount_error'); form1.mount_error1.itemindex:=i;
    i:=form1.plotted_info1.itemindex;get_int(i,'labels'); form1.plotted_info1.itemindex:=i;
    form1.fliptext1.checked:=get_boolean('fliptext',false);
    i:=form1.manipulations1.itemindex;get_int(i,'manipulations'); form1.manipulations1.itemindex:=i;

    i:=form1.mount_type1.itemindex;get_int(i,'mount_type'); form1.mount_type1.itemindex:=i;
  end;

  initstring.free;
end;


procedure save_settings(lpath:string);
const
  BoolStr: array [boolean] of String = ('0', '1');
var
  initstring :tstrings; {settings for save and loading}
  i : integer;
begin
  with form1 do
  begin

    initstring := Tstringlist.Create;

    initstring.Values['window_left2']:=inttostr(form1.left);
    initstring.Values['window_top2']:=inttostr(form1.top);
    initstring.Values['window_height']:=inttostr(form1.height);
    initstring.Values['window_width']:=inttostr(form1.width);

    initstring.Values['skyview']:=BoolStr[skyview_selected1.checked];
    initstring.Values['eso']:=BoolStr[eso_selected1.checked];
    initstring.Values['database']:=BoolStr[database_selected1.checked];
    initstring.Values['flipV']:=BoolStr[flipv1.checked];
    initstring.Values['flipH']:=BoolStr[fliph1.checked];

    initstring.Values['mount_alpaca']:=BoolStr[form1.mount_alpaca1.checked];
    initstring.Values['focuser_alpaca']:=BoolStr[form1.focuser_alpaca1.checked];
    initstring.Values['camera_alpaca']:=BoolStr[form1.camera_alpaca1.checked];
    initstring.Values['rotator_alpaca']:=BoolStr[form1.rotator_alpaca1.checked];

    initstring.Values['fastsim']:=BoolStr[form1.fast_simulation1.checked];
    initstring.Values['decreverses']:=BoolStr[form1.DecPulseReverses1.checked];
    initstring.Values['NSswapped']:=BoolStr[form1.NSswapped1.checked];

    initstring.Values['mount_type']:=inttostr(mount_type1.itemindex);

    initstring.Values['polar_e']:=BoolStr[ form1.polar_alignment_error1.checked];
    initstring.Values['elevation_e']:=form1.elevation_error1.text;
    initstring.Values['azimuth_e']:=form1.azimuth_error1.text;
    initstring.Values['latitude']:=form1.latitude1.text;
    initstring.Values['longtude']:=form1.longitude1.text;

    initstring.Values['height']:=form1.height1.text;
    initstring.Values['width_pixels']:=form1.width_pixels1.text;
    initstring.Values['height_pixels']:=form1.height_pixels1.text;
    initstring.Values['star_database']:=form1.star_database1.text;
    initstring.Values['max_magn']:=form1.maxmagn1.text;

    initstring.Values['path_image']:=form1.path_to_image1.text;

    initstring.Values['crota2']:=form1.angle2.text;

    initstring.Values['focus_at']:=form1.focus_at1.text;
    initstring.Values['focus_range']:=form1.focus_range1.text;
    initstring.Values['backlash']:=form1.backlash1.text;
    initstring.Values['hot_pixels']:=form1.hotpixels1.text;

    initstring.Values['mount']:=ascom_mount_driver;
    initstring.Values['focuser']:=ascom_focuser_driver;
    initstring.Values['rotator']:=ascom_rotator_driver;

    initstring.Values['skyview_url']:=form1.internetSkyview1.text;
    initstring.Values['eso_url']:=form1.internetESO1.text;

    initstring.Values['mount_error']:=inttostr(form1.mount_error1.itemindex);
    initstring.Values['labels']:=inttostr(form1.plotted_info1.itemindex); {0 None, 1 HFD, 2 Info, 3 Objects, 4 All}
    initstring.Values['fliptext']:=BoolStr[ form1.fliptext1.checked];
    initstring.Values['manipulations']:=inttostr(form1.manipulations1.itemindex);



  with initstring do
  begin
    try
    savetoFile(lpath);{ save to file.}
    except
      application.messagebox(pchar('Error writing: '+lpath),pchar('Error'),MB_ICONWARNING+MB_OK);
      exit;
    end;
  end;
  initstring.free;
  end;
end;


function floattostrFdot(const x:double; width2,decimals1 :word): string;
begin
  str(x:width2:decimals1,result);
end;


function DownloadFile(SourceFile, DestFile: string): Boolean;{2013, download files from internet}
begin
  form1.cursor:=crHourGlass;
  application.processmessages; {required otherwise no wait cursor in Linux}
  if esc_pressed then exit;

  {$ifdef mswindows}
  try
    Result := UrlDownloadToFile(nil, PChar(SourceFile), PChar(DestFile), 0, nil) = 0;
  except
    Result := False;
  end;
  {$ELSE} {Linux}
   result:=download_file(SourceFile, DestFile);
   {$endif}
  form1.cursor:=crdefault;
end;

procedure ang_sep(ra1,dec1,ra2,dec2 : double;out sep: double);{version 2018-5-23, calculates angular separation. according formula 9.1 old Meeus or 16.1 new Meeus, version 2018-5-23}
var
  sin_dec1,cos_dec1,sin_dec2,cos_dec2, cos_sep:double;
begin
  sincos(dec1,sin_dec1,cos_dec1);{use sincos function for speed}
  sincos(dec2,sin_dec2,cos_dec2);

  cos_sep:=sin_dec1*sin_dec2+ cos_dec1*cos_dec2*cos(ra1-ra2);
  if cos_sep>1 then cos_sep:=1 else if cos_sep<-1 then cos_sep:=1; {sometimes 1.0000000000000002 resulting in an arccos error}
  sep:=arccos(cos_sep);
end;


function fnmodulo(x,range: double):double;
begin
  {range should be 2*pi or 24 hours or 0 .. 360}
  result:=range*frac(x/range);
  if result<0 then result:=result+range;   {do not like negative numbers}
end;


procedure calc_sidereal_time(longitude: double);{local sidereal time}
const
  siderealtime2000=(280.46061837)*pi/180;{[radians], sidereal time at 2000 jan 1.5 UT (12 hours) =Jd 2451545 at meridian greenwich, see new Meeus 11.4}
  earth_angular_velocity = pi*2*1.00273790935; {about(365.25+1)/365.25) or better (365.2421874+1)/365.2421874 velocity daily. See new Meeus page 83}

begin
  sidereal_time:=fnmodulo(+longitude+siderealtime2000 +(jd - 2451545  )* earth_angular_velocity,2*pi); {As in the FITS header in ASTAP the site longitude is positive if East1 and has to be added to the time}
end;


//procedure precession(jd, ra1,dec1 : double; out ra2,dec2 : double); {precession correction,  new Meeus chapter precession formula 20.1}
//var
//  t,dra,ddec,m,n,n2  : double;
//begin
//  t:=(jd-2451545)/36525; {time in julian centuries since j2000 }
//  m:=3.07496+0.00186*t;{seconds}
//  n:=1.33621-0.00057*t; {seconds}
//  n2:=20.0431-0.0085*t;{arcsec}
//  dra:=(m + n *sin(ra1)*tan(dec1))*pi/(3600*12);{yearly ra drift in radians}
//  ddec:=n2*cos(ra1)*pi/(3600*180); {yearly dec drift in radians}
//  ra2:=ra1+(dra*t*100);{multiply with number of years is t*100}
//  dec2:=dec1+(ddec*t*100);
//end;


procedure precession5(jd_start, jd_end, ra_in, dec_in: double; out ra_out,dec_out: double); {Rigorious method,  Meeus formulas 20.2, 20.3,20.4 }
var
  a,b,c,zeta,z,theta, TT, t : double;
begin
  TT := (jd_start - 2451545.0) / 36525;//Julian centuries for the start epoch from a base epoch 2000.0
  t := (jd_end - jd_start) / 36525;//Julian centuries from the start to end epoch

  // Expressions in arc seconds
  zeta := (2306.2181 + 1.39656*TT - 0.000139*TT*TT)*t +
    (0.30188 - 0.000344*TT)*t*t +  (0.017998)*t*t*t;

  z := (2306.2181 + 1.39656*TT - 0.000139*TT*TT)*t +
    (1.09468 + 0.000066*TT)*t*t + (0.018203)*t*t*t;

  theta := (2004.3109 - 0.85330*TT - 0.000217*TT*TT)*t +
    (-0.42665 - 0.000217*TT)*t*t + (-0.041833)*t*t*t;

  zeta := zeta * PI / (180*3600); //Convert to radians
  z := z *  PI / (180*3600);
  theta := theta * PI / (180*3600);

  {Calculate the precession}
  a := sin(ra_in + zeta)*cos(dec_in);
  b := cos(ra_in + zeta)*cos(theta)*cos(dec_in) -
        sin(theta)*sin(dec_in);
  c := cos(ra_in + zeta)*sin(theta)*cos(dec_in) +
        cos(theta)*sin(dec_in);

  if (c > 0.9999619) then {within 0.5 degrees of the pole equals sin(89.5*pi/180) Improve the accuracy }
  begin
    dec_out := arccos(sqrt(a*a+b*b));
  end
  else if (c < -0.9999619) then
  begin
    dec_out := -arccos(sqrt(a*a+b*b));
  end
  else
  begin
    dec_out := arcsin(c);
  end;

  ra_out := arctan2(a,b) + z;

  ra_out:=fnmodulo(ra_out,2*pi);

end;




FUNCTION julian_calc(yyyy,mm:integer;dd,hours,minutes,seconds:double):double; {##### calculate julian day, revised 2017}
var
   Y,M   : integer;
   A, B , XX : double;
begin
  IF MM>2 THEN  begin Y:=YYYY; M:=MM;end
  else {MM=1 OR MM=2}
    begin Y:=YYYY-1; M:=MM+12;end;

  DD:=DD+HOURS/24+MINUTES/(24*60)+SECONDS/(24*60*60);

  if ((YYYY+MM/100+DD/10000)<1582.10149999) then B:=0 {year 1582 October, 15, 00:00 reform Gregorian to julian, 1582.10149999=1582.1015 for rounding errors}
  else                                                {test year 837 april 10, 0 hours is Jd 2026871.5}
  begin
    A:=INT(Y/100);
    B:=+ 2 - A + INT(A/4)
  end;

  if Y<0 then XX:=0.75 else xx:=0;{correction for negative years}
    result:=INT(365.25*Y-XX)+INT(30.6001*(M+1))
         + DD
         + B
         + 1720994.5;
end;


procedure calc_jd; {get julian day for date_obs, so the start of the observation}
var
  yy,mm,dd :word;
  hour,min, ss,ms: Word;
  dt         :tdatetime;

begin
  dt:=LazSysUtils.NowUTC;
  DeCodeDate(dt,YY,MM,DD);
  DecodeTime(dt,hour,min,ss,ms);
  jd:=julian_calc(yy,mm,dd,hour,min,ss+ms/1000);{calculate julian day}
end;


Function Minmax(x,min2,max2:double):double;{Limit x to min and max}
var
  y: double;
begin
  if x<min2 then y:=min2
  else
  begin
    if x>max2 then y:=max2
    else y:=x;
  end;
  minmax:=y;
end;

procedure jpeg_to_picture(filen1: string);
var
  ajpg: graphics.TjpegImage;
begin
  try
    ajpg := graphics.TJPEGImage.Create;
    ajpg.LoadFromFile(filen1);

    dss_bitmap.assign(ajpg); {keep for later for display if in focus}
  except
  end;
  ajpg.free;
end;

procedure gif_to_picture(filen1: string);
var gif: graphics.TGIFImage;
begin

  try
    gif := graphics.TGIFImage.Create;
    gif.LoadFromFile(filen1);

    dss_bitmap.assign(gif); {keep for later}
  except
  end;
  gif.free
end;

procedure getDSSimage; {download DSS image}
var
  width_arcmin,height_arcmin  : double;
  pixelx, pixely,count,i      :integer;
  internetlink, destfile:string;
  myFile : File;
  charArray : array[0..255] of char;
  message   : string;
begin

 //result:=false; {assume failure}

  height_arcmin:=strtofloat(form1.height1.text);

  memo2_message('DSS image download started.');

  if form1.skyview_selected1.checked then
  begin
    pixelY:=strtoint(form1.height_pixels1.text);{height}
    pixelX:=strtoint(form1.width_pixels1.text);{height}
    form1.width1.caption:=inttostr(round(height_arcmin*pixelX/pixelY));

    width_arcmin:=height_arcmin*1.3333333333333;
    internetlink:= form1.internetskyview1.text;

    internetlink:=internetlink+'&VCOORD='+floattostrFdot(ra_telescope_2000*180/pi,0,4)+','+floattostrFdot(dec_telescope_2000*180/pi,0,4)+
      '&PIXELX='+inttostr(pixelx)+
      '&PIXELY='+inttostr(pixely)+
      '&SFACTR='+floattostrFdot(minmax(abs(width_arcmin),0.3,360*60)/60,0,4);{not limitations but skyview stops at around 140 degrees}

    form1.internetskyview1.hint:=internetlink;

      destfile:=form1.path_to_image1.text+'\image.tmpjpg';
      form1.scale1.Caption:=floattostrF(height_arcmin*60/pixelY,ffFixed,4,2);
  end
  else
  begin {eso}

    internetlink:= form1.internetESO1.text;   //  &ra=83.6072&dec=22.0533&x=46.6&y=36.4

    height_arcmin:=minmax(height_arcmin,0.3,60);
    width_arcmin:=height_arcmin*1.3333333333333;
    internetlink:=internetlink+'&ra='+floattostrFdot(ra_telescope_2000*180/pi,0,4)+'&dec='+floattostrFdot(dec_telescope_2000*180/pi,0,4)+'&x='+floattostrFdot(width_arcmin,0,1)+'&y='+floattostrFdot(height_arcmin,0,1);

    form1.internetESO1.hint:=internetlink;

    form1.height1.text:=floattostr(height_arcmin);{maximum 60 arcmin}
    destfile:=form1.path_to_image1.text+'\image.tmpgif'; {gif file !!!}
    form1.scale1.Caption:='1.7';
    pixelX:=round(width_arcmin*60/1.7);
    pixelY:=round(height_arcmin*60/1.7);
    form1.width_pixels1.text:=inttostr(pixelX);
    form1.height_pixels1.text:=inttostr(pixelY);
  end;

  application.processmessages; {in linux allow closing popup menu}
  if esc_pressed then exit;

  if DownloadFile(internetlink, destfile) then
  begin
    if filesize(destfile)<4096 then
    begin  {error handling}
      dss_bitmap.Height:=pixelY; {storage for DSS output to file in 24 bit grey levels}
      dss_bitmap.width:=pixelX;
      dss_bitmap.PixelFormat := pf24bit;
      with dss_bitmap.Canvas do {output to imaging program}
      begin
        Brush.Style := bsClear;
        Font.Color := clwhite;
        font.size:=50;
        TextOut(10,10, 'Download error!');
        font.size:=14;
        {$I-}
        try
          AssignFile(myFile,destfile);
          FileMode := fmOpenRead;
          Reset(myFile, 1);   // Now we define one record as 1 byte
          BlockRead(myFile, chararray[0], sizeof(chararray),count);
          CloseFile(myFile);
          chararray[count]:=#0; {set size}
          message:=trim(strpas(chararray));
          for i:=0 to 4 do
            TextOut(10,100+i*25,copy(message,1+i*60,60) ); {display  text if any}
        except
        end;
        {$I+}
      end;
    end


    else
    begin  {succes download}
      if form1.skyview_selected1.checked then
         jpeg_to_picture(destfile){load into dss_bitmap}
      else {ESO}
        gif_to_picture(destfile);{load into dss_bitmap}
    end;

    oldRA_telescope:=ra_telescope_2000;
    oldDEC_telescope:=dec_telescope_2000;
    inc(imagecounter);
    form1.imagecounter1.caption:='Images downloaded: '+inttostr(imagecounter);
    memo2_message('DSS image download finished.');
  end;
end;


Function LeadingZero(w : integer) : String;
 var
   s : String;
 begin
   Str(w:0,s);
   if Length(s) = 1 then
     s := '0' + s;
   LeadingZero := s;
 end;

function prepare_ra(rax:double):string; {radialen to text, format 24: 00 00.0 }
 var
   B : String[2];
   h,m,s,ds  :integer;
 begin {make from rax [0..pi*2] a text in array bericht. Length is 8 long}
  rax:=rax+pi*2*0.05/(24*60*60); {add 1/10 of half second to get correct rounding and not 7:60 results as with round}
  rax:=rax*12/pi; {make hours}
  h:=trunc(rax);
  m:=trunc((rax-h)*60);
  s:=trunc((rax-h-m/60)*3600);
  ds:=trunc((rax-h-m/60-s/3600)*36000);
  Str(trunc(h):2,b);
  result:=string(b)+': '+leadingzero(m)+' '+leadingzero(s)+'.'+char(ds+48);
end;

function prepare_dec(decx:double):string; {radialen to text, format 90d 00 00}
 var
   B : String[9];
   g,m,s  :integer;
   sign   : char;
begin {make from rax [0..pi*2] a text in array bericht. Length is 10 long}
  if decx<0 then sign:='-' else sign:='+';
  decx:=abs(decx)+pi*2*0.5/(360*60*60); {add half second to get correct rounding and not 7:60 results as with round}
  decx:=decx*180/pi; {make degrees}
  g:=trunc(decx);
  m:=trunc((decx-g)*60);
  s:=trunc((decx-g-m/60)*3600);
  Str(trunc(g):2,b);
  result:=sign+string(b)+'d '+leadingzero(m)+' '+leadingzero(s);
end;


procedure save_png(filen: string);
var png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.Assign(form1.image1.Picture.graphic);
    png.SaveToFile(filen);
 finally
    png.Free;
  end;
end;


procedure save_img_bitmap_to_png(bitm : graphics.TBitmap; filen: string);
var png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.Assign(bitm);
    png.SaveToFile(filen);
 finally
    png.Free;
  end;
end;


procedure image_to_array(var img_array :  image_array); {convert timage to image array}
var
  w,h,x, y,siz: Integer;
  xLine : PByteArray;
begin
  w:=form1.image1.Picture.Width;
  h:=form1.image1.Picture.Height;
  setlength(img_array,w,h);

  siz:=form1.image1.Picture.Bitmap.RawImage.Description.BitsPerPixel div 8; {Bitmap.PixelFormat=pf24bit is not reliable in Linux}
  for y := 0 to h -1 do
  begin // scan each line
    xline:=form1.image1.Picture.Bitmap.ScanLine[y];
    for x := 0 to w-1 do
    begin
      if siz=3 then
        img_array[x,y]:=10*xLine^[x*3+2] {do only red, increase level to 255*10=2550 [0..65535]}
      else {size=4}
        img_array[x,y]:=10*xLine^[x*4+2];{do only red}
    end;
  end;
end;


procedure  dss_bitmap_to_img_bitmap_with_blur(factor :integer);{blur dss_bitmap and put in img_bitmap}
var w,h, x, y,j,val,valR,valG,valB : Integer;
    xLine,xline2: PByteArray;
    img_array1   : array of array of byte;
    img_array2  : array of array of byte;
    valword : word;

begin
  try
    {For ASCOM only. Not required for linux and mac}
    w:=dss_bitmap.width;
    h:=dss_bitmap.height;

    img_bitmap.clear;{Important. required to remove old picture. Reason ???}
    img_bitmap.Height:=h; {storage for DSS output to file in 24 bit grey levels}
    img_bitmap.width:=w;
    img_bitmap.PixelFormat := pf24bit;

    setlength(img_array1,w,h);
    setlength(img_array2,w,h);

    for y := 0 to h -1 do
    begin
      xLine := dss_bitmap.ScanLine[y];
      for x := 0 to w -1 do
      begin
        img_array1[x,y]:=round((xLine^[x*4+1]+xLine^[x*4+2])/2);{THE ESO IMAGES DO NOT CONTAINS BLUE, ONLY RED AND GREEN}
      end;
    end;

    if factor>1 then  {blur active}
    begin
      for j:=1 to factor do
      begin
        for y := 1 to h -1-1 do {box blur}
          for x := 1 to w -1-1 do
            img_array2[x,y]:=(img_array1[x-1,y+1]+img_array1[x,y+1]+img_array1[x+1,y+1]+
                              img_array1[x-1,y  ]+img_array1[x,y  ]+img_array1[x+1,y  ]+
                              img_array1[x-1,y-1]+img_array1[x,y-1]+img_array1[x+1,y-1]) div 9;

        for y := 1 to h -1-1 do {put back}
          for x := 1 to w -1-1 do
             img_array1[x,y]:=img_array2[x,y];
      end;
    end;


   for y := 0 to h -1 do
   begin
      xLine2:= img_Bitmap.ScanLine[y];

      for x := 0 to w -1 do
      begin
        //img_array1[x,y]:=round(65535*y/h);
        if img_array1[x,y]=0 then
          val:=0  {ln(1) is zero. No need to take calculate ln()}
        else
          val:=round(189000*ln(1+img_array1[x,y]));{transport grey level logarithmic in 24 bit range [0..$FFFFFF]  Range [0 .. 3.4E38].  No negative values}
                                                  {note decoding:  value:=(-1 + Math.Exp(  ((red * 256 * 256) + (green * 256) + (blue)) / 189000);}


        valR:=hi(val); {use 3x 8 bytes to transport, range $0..$FFFFFF}
        valword:=lo(val);
        valG:=hi(valword);
        valB:=lo(valword);

        xLine2^[x*3]:=valB;
        xLine2^[x*3+1]:=valG;
        xLine2^[x*3+2]:=valR;
      end;
    end;


    img_array1:=nil;
    img_array2:=nil;

    with form1.image1.Canvas do {for local display}
    begin
      Brush.Style := bsClear;
      Font.Color := clwhite;
      font.size:=12;
      TextOut(10,10, inttostr(focuser_position));
    end;

    with img_bitmap.Canvas do {output to imaging program}
    begin
      Brush.Style := bsClear;
      Font.Color := clwhite;
      font.size:=12;
      TextOut(10,10, inttostr(focuser_position));
    end;

  except
  end;
end;



procedure  dss_bitmap_to_img_ARRAY_with_blur(factor :integer);{blur dss_bitmap and put in img_array}
var w,h, x, y,j   : Integer;
    xLine         : PByteArray;
    img_array2    : array of array of single;

begin
  try
    w:=dss_bitmap.width;
    h:=dss_bitmap.height;

    setlength(img_array,w,h);
    setlength(img_array2,w,h);

    for y := 0 to h -1 do
    begin
      xLine := dss_bitmap.ScanLine[y];
      for x := 0 to w -1 do
      begin
        img_array[x,y]:=((xLine^[x*4+1]+xLine^[x*4+2])/2);{THE ESO IMAGES DO NOT CONTAINS BLUE, ONLY RED AND GREEN}
      end;
    end;

    if factor>1 then  {blur active}
    begin
      for j:=1 to factor do
      begin
        for y := 1 to h -1-1 do {box blur}
          for x := 1 to w -1-1 do
            img_array2[x,y]:=(img_array[x-1,y+1]+img_array[x,y+1]+img_array[x+1,y+1]+
                              img_array[x-1,y  ]+img_array[x,y  ]+img_array[x+1,y  ]+
                              img_array[x-1,y-1]+img_array[x,y-1]+img_array[x+1,y-1]) / 9;

        for y := 1 to h -1-1 do {put back}
          for x := 1 to w -1-1 do
             img_array[x,y]:=img_array2[x,y];
      end;
    end;

    img_array2:=nil;

    annotation_to_array(inttostr(focuser_position),true{transparant},graylevel,1, 10,10 {screen coord},img_array);{string to image array as annotation, result is flicker free since the annotion is plotted as the rest of the image}

  except
  end;
end;


function position_to_blurfactor(position:integer):integer; {tries to have a linear function for HFD=position to blur factor}
var
     offset :integer;
begin
  offset:=round(abs(position-strtoint(form1.focus_at1.text))/(strtoint(form1.focus_range1.text)*0.1)); {position range 0..10  should be around hfd 0..10 }
  if offset<2 then offset:=2;{don't blur between 24800 and 25200 equals HFD <2.0 . Dead area}
  result:=round(0.24*offset*offset - 1.2*offset + 1.76);
  if result>15 then result:=15; {blur more then 15 doesn't work. HFD will be around 10 at blur factor 15}

  form1.focusfactor1.caption:='Blur: ' + inttostr(result)+'  HFD: '+ inttostr(offset);
end;


function get_tracking_error(cyclus_time {minutes}:double) : double;{Sinus shaped, range -1..1}
begin
  result:=frac(time*24*60/cyclus_time); {sawtooth shaped 0..1}
  result:=sin(result*pi*2); {convert sawtooth to sinus}
end;

procedure Wait(wt:single=500);
var endt: TDateTime;
begin
  endt:=now+wt/MSecsPerDay;
  while now<endt do begin
    Sleep(5);
    if GetCurrentThreadId=MainThreadID then Application.ProcessMessages;
  end;
end;

//procedure ra_az2(ra,dec,lat,long,t:double;out azimuth2,altitude2: double);{conversion ra & dec to altitude, azimuth, longitude is POSITIVE when west. At south azimuth is 180 }
//{input ra [0..2*pi], dec [-pi/2..+pi/2],lat[-pi/2..pi/2],long[0..2*pi],time[0..2*pi]}
//var t5 :double;
//    sin_lat,cos_lat,sin_dec,cos_dec,sin_t5,cos_t5:double;
//begin
//  t5:=-ra+t-long;
//  sincos(lat,sin_lat,cos_lat);
//  sincos(dec,sin_dec,cos_dec);
//  sincos(t5,sin_t5,cos_t5);
//  try
  {***** altitude calculation from ra&dec, meeus new 12.5 *******}
//  altitude2:=arcsin(sin_lat*sin_dec+cos_lat*cos_dec*cos_t5);

  {***** azimuth calculation from ra&dec, meeus new 12.6 ****** }
//  azimuth2:=arctan2(sin_t5,cos_t5*sin_lat- tan(dec)*cos_lat);
//  except
  {ignore floating point errors outside builder}
//  end;
//  azimuth2:=azimuth2+pi;
//end;


//procedure az_ra2(az,alt,lat,long,t:double;out ra,de: double);{conversion az,alt to ra,dec, longitude is POSITIVE when west. At south azimuth is 180}
{input az [0..2*pi], alt [-pi/2..+pi/2],lat[-pi/2..+pi/2],long[0..2*pi],time[0..2*pi]}
//var
//  sindec,sin_lat, cos_lat, sin_alt, cos_alt,sin_az,cos_az   : double;
//begin
//  sincos(lat,sin_lat,cos_lat);
//  sincos(alt,sin_alt,cos_alt);
//  sincos(az-pi,sin_az,cos_az); {south is 180 degrees, shift 180 degrees}

//  de:=arcsin(sin_lat*sin_alt - cos_lat*cos_alt*cos_az) ;{new meeus, formule behind 12.6}
//  ra:=arctan2(sin_az, cos_az*sin_lat+tan(alt)*cos_lat  );

//  ra:=-ra+t-long;

//  while ra<0 do ra:=ra+2*pi;
//  while ra>=2*pi do ra:=ra-2*pi;
//end;


function time_passed(delta : integer):boolean;{are delta seconds passed?}
var
  unix_time : longint;
begin
  unix_time:=round((now - 25569) * 86400); {local unix time}
  if (unix_time-old_unix_time)>delta then
  begin
    result:=true;
    old_unix_time:=unix_time;
  end
  else
  result:=false;
end;


procedure simulate;
const
   equinox_telescope  : integer = 0;
   slewtime     : integer=0;
   rotcounter   : integer=0;{unstable rotator}
   foccounter   : integer=0;{unstable focuser}
   cnt          : integer=0;
var
    eqs, blur_factor,itemindex,old,backlash,focus_backlash         : integer;
    filen                          : string;
    hfd,seperation,mount_error,seeing_errorRA, seeing_errorDEC, allowederror,ra3,dec3,dra,dDec,sep,cycletime,drift,orient : double;
    mount_slewing            : boolean;
    Save_Cursor:TCursor;

begin
  esc_pressed:=false;
  focus_backlash:=0;
  drift:=0;//drift in declination
  pushbuttonRA:=0;
  pushbuttonDEC:=0;

  while esc_pressed=false  do
  begin
    allowederror:=(1/(60))*pi/180; {alowed mount error one arc minute. could be set at 1 arc sec for tracking error simulation}
    try
    if ((ascom_mount_connected) or (form1.mount_alpaca1.checked)) then {if ascom_mount.connected then} {allow import}
    begin
      polar_alignment_error:=form1.polar_alignment_error1.checked;

      if form1.mount_alpaca1.checked then
      begin
        ra_mount_indication_2000:=alpaca_ra*pi/12;
        dec_mount_indication_2000:=alpaca_dec*pi/180;
        mount_slewing:=alpaca_mount_slewing;

        equatorial_mount:=(form1.mount_type1.itemindex=1);
        DecPulseReverses:=form1.DecPulseReverses1.checked;
        if form1.NSswapped1.checked then NSswapped:=-1 else NSswapped:=1;

        if  equatorial_mount=false then sideofpier_alpaca:=-1;
        side_of_pier:=sideofpier_alpaca; // is set while slewing in alpaca_mount_protocol.pas
      end
      else
      begin  //ascom
        try
          ra_mount_indication:=ascom_mount.RightAscension*pi/12; {equinox date}
          dec_mount_indication:=ascom_mount.declination*pi/180;
          side_of_pier:=ascom_mount.sideofpier;
         // form1.park1.checked:=ascom_mount.AtPark; {update menu telescope parked?}
         // form1.tracking1.Checked:=ascom_mount.Tracking;{tracking ?}
         // form1.home1.Checked:=ascom_mount.AtHome;{home ?}
        except
        end;
        try    {check if Ascom can specify equinox used}
           eqs:=ascom_mount.EquatorialSystem;
        except
           eqs:=0;
        end;
        case eqs of
           0 : equinox_telescope:=0;   {equOther	0	Custom or unknown equinox and/or reference frame.}
           1 : equinox_telescope:=0;    {equLocalTopocentric	1	Local topocentric; this is the most common for amateur telescopes.}
           2 : equinox_telescope:=2000; {equJ2000	2	J2000 equator/equinox, ICRS reference frame.}
           3 : equinox_telescope:=2050; {equJ2050	3	J2050 equator/equinox, ICRS reference frame.}
           4 : equinox_telescope:=1950; {equB1950	4	B1950 equinox, FK4 reference frame.}
        end;


        if equinox_telescope=2000 then
        begin
          ra_mount_indication_2000:=ra_mount_indication;
          dec_mount_indication_2000:=dec_mount_indication;
          form1.mount_indication1.caption:='';
        end
        else {Jnow}
        if equinox_telescope<=1 then
        begin
          form1.mount_indication1.caption:=prepare_ra(ra_mount_indication)+ '  '+prepare_dec(dec_mount_indication);
          calc_jd; {calc jd}
          precession5(jd,2451545 {go back to J2000}, ra_mount_indication,dec_mount_indication, ra_mount_indication_2000,dec_mount_indication_2000); {precession correction,  simple formula, new Meeus chapter precession} {convert telescope astrometric to 2000 for map}
        end
        else {2050}
        begin
          precession5(2451545+365.25*(equinox_telescope-2000),2451545 {go back to J2000}, ra_mount_indication,dec_mount_indication, ra_mount_indication_2000,dec_mount_indication_2000); {precession correction,  simple formula, new Meeus chapter precession} {convert telescope astrometric to 2000 for map}
          form1.mount_indication1.caption:='';
        end;

        try
          mount_slewing:=false;
          if ascom_mount.CanSlewAsync then
             mount_slewing:=ascom_mount.Slewing
        except
           on E: Exception do memo2_message('Get slewing error: ' + E.Message);
        end;

      end;{ascom}
      form1.ra1.Caption:=prepare_ra(ra_mount_indication_2000);
      form1.dec1.Caption:=prepare_dec(dec_mount_indication_2000);
      filen:=form1.path_to_image1.text+'\image.tmp';{png file}

      case side_of_pier of
         0: form1.pointing1.caption:='Pointing West';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
         1: form1.pointing1.caption:='Pointing East';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
        else
          form1.pointing1.caption:='-';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
      end;

      try
      if ((ascom_rotator_connected) or (form1.rotator_alpaca1.Checked)) then  {allow import rotator}
      begin
        oldrotator_position:=rotator_position;
        if form1.rotator_alpaca1.Checked=false then {ascom}
        begin
          rotator_position:=ascom_rotator.position;
          rotator_reverse:=ascom_rotator.reverse;
          form1.rotator_setpoint1.caption:='';
        end
        else {alpaca}
        begin
          rotator_position:=alpaca_rot_position_synced;
          form1.rotator_setpoint1.caption:=inttostr(alpaca_rot_position_target_synced);
          rotator_reverse:=alpaca_rot_reverse;
        end;

        if rotator_position=oldrotator_position then {stable?}
        begin
          inc(rotcounter);
        end
        else {moving focuser}
        begin
          rotcounter:=0; {unstable rotator position}
          form1.rotator_position1.Caption:=inttostr(rotator_position);
          form1.rotator_reverse1.checked:=rotator_reverse;
          form1.statusbar1.caption:='Rotator is rotating' ;
        end;

        if rotcounter=10 then {long enough stable, time for an update}
        begin
          if rotator_reverse then
            form1.updown1.position:=round(fnmodulo(360-rotator_position,360))
          else
            form1.updown1.position:=rotator_position;
          update_required:=true;
          memo2_message('Rotator reached new position.');
        end;
      end;
      except
      end;

      {FOCUSER PROCESSING}
      try
      if ((ascom_focuser_connected) or (form1.focuser_alpaca1.Checked)) then {allow import focuser}
      begin
        oldreal_position:=real_position;
        backlash:=strtoint(form1.backlash1.text) div 2;

        old:= focuser_position;

        if form1.focuser_alpaca1.Checked=false then {ascom}
          focuser_position:=ascom_focuser.position
        else {alpaca}
          focuser_position:=alpaca_foc_position;

        if focuser_position>old then begin focus_backlash:=-min(focuser_position-real_position,backlash); form1.arrowleft1.caption:='🢁'+inttostr(focus_backlash); end
        else
        if focuser_position<old then begin focus_backlash:=+min(real_position-focuser_position,backlash);form1.arrowleft1.caption:='🢃'+inttostr(focus_backlash); end;
        real_position:=focuser_position+focus_backlash;

        form1.real_position1.Caption:=inttostr(real_position);
        form1.focuser_position1.Caption:=inttostr(round(focuser_position));

        if oldreal_position=real_position then {stable focus}
        begin
          //log_to_file(documents_path+'\simulator_log.txt',DateTimeToStr(Now)+',     Indication,'+inttostr(round(focuser_position))+',    real_pos,'+inttostr(real_position)+',   hfd,'+floattostrFdot(hfd_calc(real_position,strtoint(form1.focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(form1.focus_range1.text)/10),0,2));
           inc(foccounter);
           if foccounter=10 then {long enough stable focus, time for an update}
           begin
             update_required:=true;
             memo2_message('Focuser reached new position.');
           end
        end
        else
        foccounter:=0;{unstable focus position}

        if old<>focuser_position then {unequal positions}
        begin
          form1.real_position1.Caption:=inttostr(real_position);
          form1.focuser_position1.Caption:=inttostr(round(focuser_position));
          form1.statusbar1.caption:='Focuser is moving' ;
        end;
      end;
      except
      end;

      application.processmessages;
      if esc_pressed then exit;

      if mount_slewing then
      begin
        if slewtime=0 then memo2_message('Start slewing.');
        form1.StatusBar1.simpletext:='Slewing('+inttostr(slewtime)+')......';
        slewtime:=slewtime+1;
      end
      else
      begin
        if slewtime>0 then memo2_message('Slewing finished.');
        seeing_errorRA:=0;
        seeing_errorDEC:=0;

        itemindex:=form1.mount_error1.itemindex;
        //0) 0   no slew error
        //1) ++  RA error based on slew distance
        //2) +     RA error based on slew distance
        //3) -      RA error based on slew distance
        //4) --    RA error based on slew distance
        //5) ++  RA error based on slew time
        //6) +    RA error based on slew time
        //7) -     RA error based on slew time
        //8) --   RA error based on slew time
        if ((itemindex>0) and (itemindex<=4)) then ang_sep(oldRA_telescope,oldDEC_telescope,ra_mount_indication_2000,dec_mount_indication_2000, seperation);{find offset}
        if itemindex<=0 then { 0} begin mount_error:=0;end else
        if itemindex=1 then {++} begin mount_error:= seperation*+0.02  ;{introduce error depending on slew distance}  end else
        if itemindex=2 then { +} begin mount_error:= seperation*+0.004 ;{introduce error depending on slew distance}  end else
        if itemindex=3 then { -} begin mount_error:= seperation*-0.004 ;{introduce error depending on slew distance}  end else
        if itemindex=4 then {--} begin mount_error:= seperation*-0.02  ;{introduce error depending on slew distance}  end else
        if itemindex=5 then {++} begin mount_error:= slewtime*+0.0002  ;{introduce error depending on slew time}  end else
        if itemindex=6 then { +} begin mount_error:= slewtime*+0.00004 ;{introduce error depending on slew time}  end else
        if itemindex=7 then { -} begin mount_error:= slewtime*-0.00004 ;{introduce error depending on slew time}  end else
        if itemindex=8 then {--} begin mount_error:= slewtime*-0.0002  ;{introduce error depending on slew time}  end else
        if itemindex=9 then //9) No tracking error (for guider calibration)
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=10 then //10) No tracking error | noise 0.4" (for guider calibration)
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=randg(0,0.4)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec.}
          seeing_errorDEC:=randg(0,0.4)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec}
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=11 then //11) No tracking error | noise 1.0" (for guider calibration)
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=randg(0,1.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec.}
          seeing_errorDEC:=randg(0,1.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec}
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if ((itemindex>=12) and (itemindex<=17)) then {RA or DEC square wave tracking error}
        begin
          //12) Tracking error  α, 10" square wave 1 min period
          //13) Tracking error  α, 10" square wave 2 min period
          //14) Tracking error  α, 10" square wave 5 min period
          //15) Tracking error  δ, 10" square wave 1 min period
          //16) Tracking error  δ, 10" square wave 2 min period
          //17) Tracking error  δ, 10" square wave 5 min period
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          if itemindex=12 then cycletime:=1 else
          if itemindex=13 then cycletime:=2 else
          if itemindex=14 then cycletime:=5 else
          if itemindex=15 then cycletime:=1 else
          if itemindex=16 then cycletime:=2 else
          if itemindex=17 then cycletime:=5;

          if get_tracking_error(cycletime {min})*20/3600*pi/180 >0 then
          begin
            if itemindex<=14 then
            begin
               seeing_errorRA:=+5*((1/3600)*pi/180); {square wave}
               seeing_errorDEC:=0;
            end
            else
            begin
              seeing_errorRA:=0;
              seeing_errorDEC:=+5*((1/3600)*pi/180); {square wave};
            end;
          end
          else
          begin
            if itemindex<=14 then
            begin
               seeing_errorRA:=-5*((1/3600)*pi/180); {square wave}
               seeing_errorDEC:=0;
            end
            else
            begin
              seeing_errorRA:=0;
              seeing_errorDEC:=-5*((1/3600)*pi/180); {square wave};
            end;
          end;
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=18 then //18) Tracking error α, 10" sinus wave 5 min period
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=get_tracking_error(5 {min})*(5/3600)*pi/180  ;{introduce 20 arc seconds cyclic error}
          seeing_errorDEC:=0;
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=19 then  //19) Tracking error δ , 10" sinus wave 5 min period
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=0;
          seeing_errorDEC:=get_tracking_error(5 {min})*(5/3600)*pi/180  ;{introduce 20 arc seconds cyclic error}
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=20 then //20) Mount steady
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=0;
          seeing_errorDEC:=0;
          drift:=0;
          wait(1000);{slow down loop}
          allowederror:=99;
          ra_telescope_2000:=oldRA_telescope;
          dec_telescope_2000:=oldDec_telescope;
          update_required:=false;
        end
        else
        if itemindex=21 then //21) Tracking error α, 20" in 10 min| δ drift
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=get_tracking_error(10 {min})*(20/3600)*pi/180  ;{introduce 20 arc seconds cyclic error}
          seeing_errorRA:=0;
          seeing_errorDEC:=0;
          drift:=drift+(0.1/3600)*pi/180;
          seeing_errorDEC:=seeing_errorDEC+drift; //drift
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        if itemindex=22 then //22) Tracking error α, 20" in 10 min| δ drift  | noise 1"
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=get_tracking_error(10 {min})*(20/3600)*pi/180  ;{introduce 20 arc seconds cyclic error}
          seeing_errorRA:=randg(0,1)*((1/3600)*pi/180);{Random seeing error 1 arc sec.}
          seeing_errorDEC:=randg(0,1)*((1/3600)*pi/180);{Random seeing error 1 arc sec}
          drift:=drift+(0.1/3600)*pi/180;
          seeing_errorDEC:=seeing_errorDEC+drift; //drift
          wait(1000);{slow down loop}
          update_required:=true; //force an update even for the smallest error or noise
        end;
        if itemindex=23 then //Mount backslash
        begin
          form1.database_selected1.checked:=true;{force database simulation}
          mount_error:=0;
          seeing_errorRA:=0;
          seeing_errorDEC:=0;
          drift:=0;
          seeing_errorDEC:=0;
          wait(1000);{slow down loop}
          backlash:=100; //100 ms backlash
          update_required:=true; //force an update even for the smallest error or noise
        end
        else
        backlash:=0;


        mount_error:= min(3*pi/180,mount_error);{maximum 3 degrees}

        dec_telescope_2000:=dec_mount_indication_2000+seeing_errorDEC+pushbuttonDEC;
        ra_telescope_2000:=ra_mount_indication_2000 + (mount_error+seeing_errorRA+pushbuttonRA)/max(0.000000000001,cos(dec_telescope_2000));//make error independent of declication. This will not be the case in practise.
        ra_telescope_2000:=fnmodulo(ra_telescope_2000,2*pi);{keep in range 0..2pi}

        ang_sep(oldRA_telescope,oldDEC_telescope,ra_telescope_2000,dec_telescope_2000, seperation);{find offset}
        if seperation>allowederror then update_required:=true; // already overruled for tracking error

        if ((update_required=false) and (polar_alignment_error) and  (time_passed(10))) {are 10 seconds passed and update required?} then
        begin
          update_required:=true;
          memo2_message('About 10 seconds passed, time to update image due to polar alignment error');
        end;

        if update_required  then {offset}
        begin
          form1.telescope_position2000_ra1.caption:=prepare_ra(ra_telescope_2000)+'      '+   floattostrF((mount_error+seeing_errorRA)*3600*180/pi,ffFixed,3,1)+'"';
          form1.telescope_position2000_dec1.caption:=prepare_dec(dec_telescope_2000)+'      '+floattostrF(seeing_errorDEC*3600*180/pi,ffFixed,3,1)+'"';


          if form1.database_selected1.checked=false then {dss image}
          begin {dss image}
            if seperation>allowederror then {offset}
            begin
              Save_Cursor := Screen.Cursor;
              Screen.Cursor := crHourglass;    { Show hourglass cursor }
              memo2_message('Downloading DSS image.');
              application.title:='. . . .';{show busy}
              getDSSimage;{Download DSS image. Equinox 2000}
              Screen.Cursor:=Save_Cursor;{back to normal cursor}
            end;
          end
          else
          begin  {new artificial image}
            application.title:='. . . .';{show busy}
            memo2_message('Preparing artificial image.');
            application.processmessages;
            if esc_pressed then exit;

            oldRA_telescope:=ra_telescope_2000; {here prior to polar alignment errors}
            oldDEC_telescope:=dec_telescope_2000;

            if ((polar_alignment_error) or (equatorial_mount)) then
            begin
              latitude:=strtofloat(form1.latitude1.text)*pi/180;;
              longitude:=strtofloat(form1.longitude1.text)*pi/180;

              calc_jd;{calc julian day from system clock}
              calc_sidereal_time(longitude);
              form1.caption:='Local sideral time '+prepare_ra(sidereal_time);

              meridian:=sidereal_time*12/pi;//for equatorial mount alpaca. [hours]
              if sideofpier_alpaca=99 then crosses_meridian(meridian); //initialise side of pier after program startup. Else it is done when slewing


              if polar_alignment_error then
              begin
              elevation_error:=form1.elevation_updown1.position*pi/(180*60);
              azimuth_error:=form1.azimuth_updown1.position*pi/(180*60);

              precession5(2451545,jd {go to jNow}, ra_telescope_2000,dec_telescope_2000, ra3,dec3);

                {Polar error calculation based on two celestial reference points and the error of the telescope mount at these point(s).
                 Based on formulas from Ralph Pass documented at https://rppass.com/align.pdf.
                 They are based on the book “Telescope Control’ by Trueblood and Genet, p.111
                 Ralph added sin(latitude) term in the equation for the error in RA.

                For one reference image the difference in RA and DEC caused by the misalignment of the polar axis, formula (3):
                   delta_ra:= de * TAN(dec)*SIN(h)  + da * (sin(lat)- COS(lat)*(TAN(dec1)*COS(h_1))
                   delta_dec:=de * COS(h)  + da * COS(lat)*SIN(h))}
                dRa:=-elevation_error*(TAN(dec3)*SIN(sidereal_time-ra3) ) +azimuth_error*(sin(latitude)-COS(latitude)*TAN(dec3)*COS(sidereal_time-ra3));
                dDec:=-elevation_error*(COS(sidereal_time-ra3))  +azimuth_error*COS(latitude)*(SIN(sidereal_time-ra3));

                //This is also possible
                //ra_az2(ra4,dec4,latitude-elevation_error,0,sidereal_time, azimuth2,altitude2);//conversion ra & dec to altitude, azimuth
                //az_ra2(azimuth2+azimuth_error,altitude2,latitude,0,sidereal_time, ra3,dec3);//conversion az,alt to ra,dec

                ra3:=ra3-dRa;
                dec3:=Dec3+dDec;
                precession5(jd, 2451545 {go back to J2000},ra3,dec3,ra_telescope_2000,dec_telescope_2000);
              end;
            end;

            orient:=form1.updown1.position*pi/180; {crota2,Image twist of Y axis(deg)}
            if ((equatorial_mount) and (side_of_pier=1)) then //  0 = pierEast/pointing West, 1 = pierWest/pointing East, -1= pierUnknown
                orient:=fnmodulo(orient+pi,2*pi); //rotation by meridian flip,

            if form1.mount_alpaca1.checked {alpaca} then
            begin
              prepare_plotting(ra_telescope_2000-ra_corr*pi/180,dec_telescope_2000-dec_corr*pi/180, orient,form1.flipH1.checked,form1.flipV1.checked); {if the mount is synced it should not create a new image. Calculate absolute encoder position. This will allow the CCDCeil polar align routine to wrok proper and allow sync for first measurment}
              ang_sep(ra_telescope_2000,dec_telescope_2000,ra_telescope_2000-ra_corr*pi/180,dec_telescope_2000-ra_corr*pi/180, sep);{calculate offset}
              if sep>5*pi/180 then memo2_message('Telescope is '+inttostr(round(sep*180/pi))+'° out of sync! Astrometric solving is required.');
            end
            else
            prepare_plotting(ra_telescope_2000,dec_telescope_2000,orient,form1.flipH1.checked,form1.flipV1.checked);

            hfd:=hfd_calc(real_position,strtoint(form1.focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(form1.focus_range1.text)/10); {a=2.35, b=2.35*1000/10, so hfd is 10 when focus position is 1000 position off}


            Save_Cursor := Screen.Cursor;
            Screen.Cursor := crHourglass;    { Show hourglass cursor }

            plot_stars(real_position,strtoint(form1.focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(form1.focus_range1.text)/10); {a=2.35, b=2.35*1000/10, so hfd is 10 when focus position is 1000 position off}
               {Plot stars. An offset of 1000 give a HFD of 10. Noise is added in ascom driver or in alpaca_camera_protocol}

            load_deep;{load the deepsky database once. If loaded no action}
            if form1.plotted_info1.itemindex < 5 then  {0 No labels, 1 HFD, 2 Info, 3 Objects labels, 4 All labels, 5 No deepsky, 6 No stars no deepsky}
              plot_deepsky;{plot the deep sky object on the image}

            image_array_to_screen;{luminance signal to screen. No noise in screen.  Noise is added in ascom driver or in alpaca_camera_protocol}

            Screen.Cursor:=Save_Cursor;{back to normal cursor}

            form1.statusbar1.simpletext:=('');

            if abs(real_position-strtoint(form1.focus_at1.text))>strtoint(form1.focus_range1.text)/40 then  {2.5 % offset}
              form1.focusfactor1.caption:='Out of focus.  HFD: '+ floattostrF(HFD,ffFixed,0,1)
              else form1.focusfactor1.caption:='In focus.';

            form1.scale1.Caption:=floattostrF(height_arcmin*60/height2,ffFixed,4,2);

            inc(imagecounter);
            form1.imagecounter1.caption:='Images created: '+inttostr(imagecounter);
          end;
        end;{update required}

        slewtime:=0;
      end;
    end;
    except
    end;

    application.processmessages;
    if esc_pressed then exit;

    {PROCESSING OF THE PREPARED IMAGE}
    if ((ascom_mount_connected) or (form1.mount_alpaca1.checked)) then
    if mount_slewing=false then
    begin
      try {update image for focus simulator}
      if update_required then
      begin
        memo2_message('Processing prepared image.');
        if ((form1.database_selected1.checked=false {DSS image}) ) then {DSS}
        begin  { DDS IMAGES}
          blur_factor:=position_to_blurfactor(real_position);
          if blur_factor>=15 then
          begin
            memo2_message('At maximum focus position.');
          end
          else
          memo2_message('Out of focus.');

          form1.Image1.picture.Bitmap.Assign(dss_bitmap); {display in tab image always in focus}

          if form1.camera_alpaca1.Checked=false then {ascom mode}
          begin {ASCOM MODE PROCESSING DSS IMAGES}
          {$ifdef mswindows}
            dss_bitmap_to_img_bitmap_with_blur(blur_factor);{blur dss_bitmap, display and  convert to img_bitmap}

            save_img_bitmap_to_png(img_bitmap,form1.path_to_image1.text+'\image.tmp_dss'); {save image1 as seperated out of focus file}
            try
              fileutil.CopyFile(form1.path_to_image1.text+'\image.tmp_dss', form1.path_to_image1.text+'\image.png');
              update_required:=false;
              memo2_message('Out of focus, image updated.');
            except;
            end;
            {$else}
             {not required for linux and mac}
            {$endif}
          end
          else
          begin {ALPACA MODE PROCESSING DSS IMAGES}
            camera_state:=2;
            dss_bitmap_to_img_ARRAY_with_blur(blur_factor);{blur dss_bitmap, display and  convert to img_ARRAY}
            camera_state:=0; {ready}
          end;
        end
        else
        begin  {ARTIFICIAL IMAGE}
          try
            if form1.camera_alpaca1.Checked then {alpaca mode}
            begin {ALPACA IMAGE}
              camera_state:=2;
              if form1.database_selected1.checked=false {DSS image} then
              begin
                form1.image1.Picture.assign(dss_bitmap);{show image}
                image_to_array(img_array);{convert timage to image array the_image}
              end
              else;{noting image is already availabe as array}
              camera_state:=0; {ready}
            end
            else
            begin {ASCOM IMAGE}
              image_array_to_bitmap(img_bitmap);{convert array to bitmap where greylevel:=255*255*red+256*green + blue }
              save_img_bitmap_to_png(img_bitmap,filen); {save form1.image1 to image.tmp}
              fileutil.CopyFile(filen {image.tmp}, form1.path_to_image1.text+'\image.png'); {fast copy}
            end;

            if form1.database_selected1.checked=false then
            begin
              memo2_message('In focus.');
            end;
            application.processmessages;
            wait(100);
          except;
          end;
        end;

        update_required:=false;
        foccounter:=999;{stable situation. Already 999 cycles no change}
        rotcounter:=999;{stable situation. Already 999 cycles no change}
        memo2_message('Image ready for download.');

      end;{processing image}
      except
      end;
    end {not slewing}
    else {slewing}
    begin
    end;

    wait(200);
    if form1.statusbar1.simpletext<>'' then inc(cnt);
    if cnt>30 then {leave message 30 cycles visible}
    begin
       form1.statusbar1.simpletext:=''; {clear statusbar}
       application.title:='sky_simulator';
       cnt:=0;
    end;
    application.processmessages;
    if esc_pressed then exit;
  end;
end;


procedure TForm1.start_button1Click(Sender: TObject);
begin

  if form1.start_button1.font.style=[fsbold] then begin esc_pressed:=true;exit;end;
  form1.start_button1.font.style:=[fsbold];

  if ((mount_alpaca1.checked=false) and (ascom_mount_connected=false)) then form1.connect_mount1Click(nil);
  if ((focuser_alpaca1.checked=false) and (ascom_focuser_connected=false)) then form1.connect_focuser1Click(nil);
  if ((rotator_alpaca1.checked=false) and (ascom_rotator_connected=false)) then form1.connect_rotator1Click(nil);

  start_alpaca(form1.camera_alpaca1.checked,form1.focuser_alpaca1.checked,form1.mount_alpaca1.checked,rotator_alpaca1.checked);

  form1.start_button1.caption:='Simulation running. (Hit ESC to stop)';
  simulate;
  form1.start_button1.font.style:=[];
  form1.start_button1.caption:='Start simulation';
end;

procedure TForm1.star_database1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.star_database1CloseUp(Sender: TObject);
begin
  maxmagn1.text:=copy(star_database1.text,2,2);
end;


procedure TForm1.star_database1DropDown(Sender: TObject);
var
  SearchRec: TSearchRec;
  s        : string;
begin
  star_database1.items.clear;
  if SysUtils.FindFirst(application_path+'*0101.*', faAnyFile, SearchRec)=0 then
  begin
    repeat
      s:=uppercase(copy(searchrec.name,1,3));
      star_database1.items.add(s);
    until FindNext(SearchRec) <> 0;
    SysUtils.FindClose(SearchRec);
  end;
end;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  stop_alpaca;
  if save_thesettings then {set in go_default1}
               save_settings(user_path+'sky_simulator.cfg');
  img_array:=nil;
  deepstring.free;{free deepsky}
  dss_bitmap.free;
  img_bitmap.free;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  deepstring := Tstringlist.Create;{for deepsky overlay}
  dss_bitmap:= graphics.TBitmap.Create;{storage for dss images}
  img_bitmap := graphics.TBitmap.Create;

end;


procedure TForm1.exit1Click(Sender: TObject);
begin
  esc_pressed:=true;{allow to leave}
  form1.close;
end;


procedure TForm1.Angle2Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.azimuth_error1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.azimuth_updown1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  update_required:=true;
end;

procedure TForm1.backlash1Exit(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  showmessage('Sky simulator for ASCOM and Alpaca. © 2018-2023 by Han Kleijn, www.hnsky.org. ');
end;

procedure TForm1.buttonEast1Click(Sender: TObject);
begin
  pushbuttonRA:=pushbuttonRA + ((5/3600)*pi/180);
end;

procedure TForm1.buttonNorth1Click(Sender: TObject);
begin
 pushbuttonDEC:=pushbuttonDEC + ((5/3600)*pi/180);
end;

procedure TForm1.buttonSouth1Click(Sender: TObject);
begin
  pushbuttonDEC:=pushbuttonDEC - ((5/3600)*pi/180);
end;

procedure TForm1.buttonWest1Click(Sender: TObject);
begin
  pushbuttonRA:=pushbuttonRA - ((1/3600)*pi/180);
end;


procedure TForm1.calculator1Click(Sender: TObject);
var
 height_info : string;
 fl,pixsize,height2 : double;
begin
  if InputQuery('Calculate field of view (height)', 'Focal length telescope [mm]?', fl_info)=false then exit;
  if fl_info='' then exit;
  fl:=strtofloat(fl_info);

  if InputQuery('Calculate field of view (height)', 'Pixel size[μm]?', crpix_info)=false then exit;
  if crpix_info='' then exit;
  pixsize:=strtofloat(crpix_info);

  height_info:=height_pixels1.caption;
  if InputQuery('Calculate field of view (height)', 'Image height[pixels]?', height_info)=false then exit;
  if height_info='' then exit;

  height_pixels1.caption:=height_info;

  height2:=strtofloat(height_info);

  height1.caption:=floattostrf((height2*pixsize/fl)*60*(180/1000)/pi, ffgeneral, 3, 3); {calculate image height in arc seconds}

  calculator1.hint:='Calculate the field of view (height) from focal length and pixel size.'+#10+#10+
                    'Last calculation input values:'+#10+#10+
                    'Focal length: '+floattostrF(fl,FFFixed,0,0)+' mm'+#10+
                    'Pixel size:   '+floattostrF(pixsize,FFFixed,0,2)+' μm';


  update_required:=true;
end;


procedure TForm1.clear_log_button1Click(Sender: TObject);
begin
  deletefile(pchar(documents_path+'alpaca_log.txt'));
  memo1.clear;
end;

procedure TForm1.connect_rotator1Click(Sender: TObject);
var
  V: variant;
begin
  {$ifdef mswindows}
  try
  if not VarIsEmpty(ascom_rotator) then  {Ascom alive?}
  begin
    ascom_rotator_connected:=false;
    ascom_rotator := Unassigned;
    connect_rotator1.font.color:=cldefault;
    connect_rotator1.caption:='Connect rotator';
    connect_rotator1.font.style:=[];
  end
  else
  begin
    if sender<>nil then {send from popup menu}
    begin {show this only when called from popup menu}
      try
        V := CreateOleObject('ASCOM.Utilities.Chooser'); {for 64 bit applications as suggested in http://www.ap-i.net/mantis/view.php?id=778#bugnotes, "Ascom." is required!!}
        except
        V := CreateOleObject('DriverHelper.Chooser');{This will work for old Ascom and 32 bit windows only. Note this give an error when run in the Delphi environment}
      end;
      V.devicetype:=string('Rotator');
      ascom_rotator_driver:=(V.Choose(ascom_rotator_driver));
      V:=Unassigned;
    end;{end send from popup menu}

    if ascom_rotator_driver='' then exit;

    {setup ASCOM }
    try
      ascom_rotator:= Unassigned;
      ascom_rotator := CreateOleObject(string(ascom_rotator_driver)); {See at the end of this file the Ascom fix 2018, """"SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]); """"}
     // ascom_rotator.link:=true;
      ascom_rotator.connected:=true;
      connect_rotator1.caption:=ascom_rotator_driver;
      connect_rotator1.font.style:=[fsbold,fsunderline];
      ascom_rotator_connected:=true;
      memo2_message('Rotator connected.');

    except
      on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
      else
      begin {other errors}
        connect_rotator1.font.color:=cldefault;
        connect_rotator1.caption:='Error rotator';
        connect_rotator1.font.style:=[];
        Showmessage('Error ASCOM driver "'+ascom_rotator_driver+'" not found!');
      end;
    end; {end setup ASCOM}

  end;

  except {Ascom alive}
    Showmessage('Error, No ASCOM detected. Install from http://ascom-standards.org');
  end;
  {$else}
    {Not available in Linux}
  {$endif}
end;



procedure TForm1.elevation_error1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.elevation_updown1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
  update_required:=true;
end;


procedure TForm1.focuser_alpaca1Change(Sender: TObject);
begin
   connect_focuser1.visible:=focuser_alpaca1.checked=false;
   if ((focuser_alpaca1.checked=false) and (ascom_focuser_connected=false)) then form1.connect_focuser1Click(nil);
   update_required:=true;
end;

procedure TForm1.focus_at1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.focus_range1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.FormResize(Sender: TObject);
begin
  horzScrollbar.visible:=form1.width<groupbox1.left+groupbox1.width; ;//show scrollbar;
  vertScrollbar.visible:=form1.height<start_button1.top+start_button1.height; ;//show scrollbar;
//    if scroll=false then horzScrollbar.position:=0;
end;


procedure TForm1.go_default1Click(Sender: TObject);
begin
  if (IDYES= Application.MessageBox('This will set all settings to default and close the program. Are you sure?', 'Default settings?', MB_ICONQUESTION + MB_YESNO) ) then
  begin
    if deletefile(pchar(user_path+'sky_simulator.cfg')) then
    begin
      save_thesettings:=false;{no save}
      form1.close;
    end
    else sysutils.beep();
  end;
end;


procedure TForm1.esobutton1Click(Sender: TObject);
begin
  Clipboard.AsText:=internetESO1.hint;
end;


procedure TForm1.flipH1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.flipV1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure enable_controls(polerr,equatmount :boolean);{for polar alignment error}
begin
 with form1 do
 begin
   Label_elevation1.enabled:=polerr;
   elevation_error1.enabled:=polerr;
   elevation_updown1.enabled:=polerr;
   Label_azimuth1.enabled:=polerr;
   azimuth_error1.enabled:=polerr;
   azimuth_updown1.enabled:=polerr;
   Label_latitude1.enabled:=((polerr) or (equatmount));
   latitude1.enabled:=((polerr) or (equatmount));
   Label_longitude1.enabled:=((polerr) or (equatmount));
   longitude1.enabled:=((polerr) or (equatmount));
 end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  user_path:=GetAppConfigDir(false);{get user path for app config}
  if load_settings(user_path+'sky_simulator.cfg')=false then
    if DirectoryExists(user_path)=false then createdir(user_path);{create c:\users\yourname\appdata\local\sky_simulator}

  focuser_position:=strtoint(form1.focus_at1.text);
  real_position:=focuser_position;
  oldreal_position:=focuser_position;

  enable_controls(polar_alignment_error1.checked, mount_type1.itemindex=1);{polar alingment error or equatorial mount}

  Form1.mount_error1Change(nil);//update north, south, East1 west button enabling

  {$ifdef mswindows}
  {$else}
    {Ascom not available in Linux, Mac}
    camera_alpaca1.checked:=true;
    focuser_alpaca1.checked:=true;
    mount_alpaca1.checked:=true;
    rotator_alpaca1.checked:=true;

    camera_ascom1.visible:=false;
    focuser_ascom1.visible:=false;
    mount_ascom1.visible:=false;
    rotator_ascom1.visible:=false;
    form1.caption:='Sky simulator for Alapaca. Camera simulator with "Deep Sky Survey" images or artificial images based on mount and focuser position.';
  {$endif}

  fast_simulation1.visible:=camera_alpaca1.checked;

end;


procedure TForm1.height1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.height_pixels1Exit(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.hotpixels1Change(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  down_x:=x;
  down_y:=y;
  down_xy_valid := True;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssleft in shift then {swipe effect}
  begin
    if down_xy_valid then
    begin
      if abs(y-down_y)>2 then
      begin
        form1.image1.Top:= form1.image1.Top+(y-down_y);
      end;
      if abs(x-down_x)>2 then
      begin
        form1.image1.left:= form1.image1.left+(x-down_x);
      end;
    end;
    exit;{no more to do}
  end
  else
  down_xy_valid := False; {every move without ssleft will invalidate down_xy}
end;


procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   down_xy_valid := False;
end;


procedure zoom(mousewheelfactor:double;MousePos: TPoint);
var
  maxw  : double;
begin

  {$ifdef mswindows}
   maxw:=65535; {will be 1.2*65535}
  {$else}
  {$ifdef CPUARM}
   maxw:=4000;{struggeling if above}
  {$else}
   maxw:=15000;
  {$endif}
  {$endif}

  if ( ((form1.image1.width<=maxw) or (mousewheelfactor<1){zoom out}) and {increased to 65535 for Windows only. Was above 12000 unequal stretch}
       ((form1.image1.width>=100 ) or (mousewheelfactor>1){zoom in})                                                                  )
  then
  begin
    {limit the mouse positions to positions within the image1}
    mousepos.x:=max(MousePos.X,form1.Image1.Left);
    mousepos.y:=max(MousePos.Y,form1.Image1.top);
    mousepos.x:=min(MousePos.X,form1.Image1.Left+form1.image1.width);
    mousepos.y:=min(MousePos.Y,form1.Image1.top++form1.image1.height);

    {scroll to compensate zoom}
    form1.image1.Left := Round((1 - mousewheelfactor) * MousePos.X + mousewheelfactor * form1.Image1.Left);
    form1.image1.Top  := Round((1 - mousewheelfactor) * MousePos.Y + mousewheelfactor * form1.Image1.Top);

    {zoom}
    form1.image1.height:=round(form1.image1.height * mousewheelfactor);
    form1.image1.width:= round(form1.image1.width * mousewheelfactor);

  end;
end;


procedure TForm1.Image1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  GetCursorPos(p);  {use this since in Lazarus the mousepos varies depending control under the mouse}
//  p:=image1.Screentoclient(p);
  p:=Image_TabSheet2.Screentoclient(p);
  zoom(1/1.1,p);
end;

procedure TForm1.Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  GetCursorPos(p);  {use this since in Lazarus the mousepos varies depending control under the mouse}
  p:=Image_TabSheet2.Screentoclient(p);
  zoom(1.1,p);

end;

procedure TForm1.Image_TabSheet2Show(Sender: TObject);
begin
 //form1.image1.height:=form1.height-3*form1.statusbar1.height;

end;


procedure TForm1.internetESO1Exit(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.internetskyview1Exit(Sender: TObject);
begin
  update_required:=true;
end;



procedure TForm1.latitude1Exit(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.longitude1Exit(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.maxmagn1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.menucopy1Click(Sender: TObject);{for memo1 popup menu}
begin
  Clipboard.AsText:=copy(Memo1.Text,Memo1.SelStart+1, Memo1.SelLength);
end;


procedure TForm1.Menufind1Click(Sender: TObject);
begin
  PatternToFind:=uppercase(inputbox('Find','Text to find:' ,PatternToFind));
  position_find := pos(PatternToFind, uppercase( Memo1.Text));
  if position_find > 0 then
  begin
    Memo1.SelStart := position_find-1;
    Memo1.SelLength := Length(PatternToFind);
    Memo1.SetFocus; // necessary so highlight is visible
  end;
end;


procedure TForm1.menufindnext1Click(Sender: TObject);
begin
  position_find := posex(PatternToFind, uppercase(Memo1.Text),position_find+1);
  if position_find > 0 then
  begin
     Memo1.SelStart := position_find-1;
     Memo1.SelLength := Length(PatternToFind);
     Memo1.SetFocus; // necessary so highlight is visible
  end;
end;


procedure TForm1.mount_alpaca1Change(Sender: TObject);
var
  alpacaMode :boolean;
begin
  alpacaMode:=mount_alpaca1.checked;

  if alpacaMode=false then
  begin
    if ascom_mount_connected=false then form1.connect_mount1Click(nil);
  end;

  connect_mount1.visible:=alpacaMode=false;
  mount_type1.visible:=alpacaMode;
  DecPulseReverses1.visible:=alpacaMode;
  NSswapped1.visible:=alpacaMode;
  update_required:=true;
end;

procedure TForm1.mount_error1Change(Sender: TObject);
var
  button_use : boolean;
begin
  button_use:=mount_error1.itemindex>=9;
  form1.buttonNorth1.enabled:=button_use;
  form1.buttonSouth1.enabled:=button_use;
  form1.buttonEast1.enabled:=button_use;
  form1.buttonWest1.enabled:=button_use;
end;

procedure TForm1.mount_type1Change(Sender: TObject);
begin
  if mount_type1.itemindex=0 then
  sideofpier_alpaca:=-1;
  pointing1.caption:='-';
  enable_controls(polar_alignment_error1.checked, mount_type1.itemindex=1);{polar alingment error or equatorial mount}
  update_required:=true;
end;


procedure TForm1.plotted_info1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.polar_alignment_error1Change(Sender: TObject);
begin
  update_required:=true;
  enable_controls(polar_alignment_error1.checked, mount_type1.itemindex=1);{polar alingment error or equatorial mount}
end;


procedure TForm1.camera_alpaca1Change(Sender: TObject);
begin
  ascom_image1.visible:=camera_alpaca1.checked=false;
  fast_simulation1.visible:=camera_alpaca1.checked;
  update_required:=true;
end;


procedure TForm1.rotator_alpaca1Change(Sender: TObject);
begin
  connect_rotator1.visible:=rotator_alpaca1.checked=false;
  if ((rotator_alpaca1.checked=false) and (ascom_rotator_connected=false)) then form1.connect_rotator1Click(nil);
  update_required:=true;
end;

procedure TForm1.savesettings1Click(Sender: TObject);
begin
  save_settings(user_path+'sky_simulator.cfg');
  memo2_message('Settings saved.');
end;


procedure TForm1.selectpath1Click(Sender: TObject);
begin
  SelectDirectoryDialog1.title:='Select directory where to store image.png';
  SelectDirectoryDialog1.initialdir:=path_to_image1.text;
  if SelectDirectoryDialog1.execute then
  begin
     path_to_image1.text:=SelectDirectoryDialog1.filename;
  end;
end;


procedure TForm1.select_all1Click(Sender: TObject);
begin
   memo1.setfocus;{required for selectall since hideselection is enabled when not having focus}
   Memo1.SelectAll;
end;


procedure TForm1.skyviewbutton1Click(Sender: TObject);
begin
  Clipboard.AsText:=internetSkyview1.hint;
end;


procedure TForm1.connect_focuser1Click(Sender: TObject);
var
  V: variant;
begin
  begin
    {$ifdef mswindows}
    try
    if not VarIsEmpty(ascom_focuser) then  {Ascom alive?}
    begin
      ascom_focuser_connected:=false;
      ascom_focuser := Unassigned;
      connect_focuser1.font.color:=cldefault;
      connect_focuser1.caption:='Connect focuser';
      connect_focuser1.font.style:=[];
    end
    else
    begin
      if sender<>nil then {send from popup menu}
      begin {show this only when called from popup menu}
        try
          V := CreateOleObject('ASCOM.Utilities.Chooser'); {for 64 bit applications as suggested in http://www.ap-i.net/mantis/view.php?id=778#bugnotes, "Ascom." is required!!}
          except
          V := CreateOleObject('DriverHelper.Chooser');{This will work for old Ascom and 32 bit windows only. Note this give an error when run in the Delphi environment}
        end;
        V.devicetype:='Focuser';
        ascom_focuser_driver:=(V.Choose(ascom_focuser_driver));
        V:=Unassigned;
      end;{end send from popup menu}

      if ascom_focuser_driver='' then exit; {e.g. 'ScopeSim.Telescope'}

      {setup ASCOM }
      try
        ascom_focuser:= Unassigned;
        ascom_focuser := CreateOleObject(ascom_focuser_driver); {See at the end of this file the Ascom fix 2018, """"SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]); """"}
        ascom_focuser.link:=true;
        connect_focuser1.caption:=ascom_focuser_driver;
        connect_focuser1.font.style:=[fsbold,fsunderline];
        ascom_focuser_connected:=true;
        memo2_message('Focuser connected.');

      except
        on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
        else
        begin {other errors}
          connect_focuser1.font.color:=cldefault;
          connect_focuser1.caption:='Error focuser';
          connect_focuser1.font.style:=[];
          Showmessage('Error ASCOM driver "'+ascom_focuser_driver+'" not found!');
        end;
      end; {end setup ASCOM}

    end;

    except {Ascom alive}
      Showmessage('Error, No ASCOM detected. Install from http://ascom-standards.org');
    end;
    {$else}
      {Not available in Linux}
    {$endif}
  end
end;


procedure TForm1.connect_mount1Click(Sender: TObject);
var
  V: variant;
begin
  {$ifdef mswindows}
  try
  if not VarIsEmpty(ascom_mount) then  {Ascom alive?}
  begin
    ascom_mount_connected:=false;
    ascom_mount.connected:=false;
    ascom_mount := Unassigned;
    connect_mount1.font.color:=cldefault;
    connect_mount1.caption:='Connect mount';
    connect_mount1.font.style:=[];
    {canslew and canslewasync will be asked later. Here to early}
  end
  else
  begin
    if sender<>nil then {send from popup menu}
    begin {show this only when called from popup menu}
      try
        V := CreateOleObject('ASCOM.Utilities.Chooser'); {for 64 bit applications as suggested in http://www.ap-i.net/mantis/view.php?id=778#bugnotes, "Ascom." is required!!}
        except
        V := CreateOleObject('DriverHelper.Chooser');{This will work for old Ascom and 32 bit windows only. Note this give an error when run in the Delphi environment}
      end;
      V.devicetype:='Telescope';
      ascom_mount_driver:=(V.Choose(ascom_mount_driver));
      V:=Unassigned;
    end;{end send from popup menu}

    if ascom_mount_driver='' then exit; {e.g. 'ScopeSim.Telescope'}

    {setup Ascom}
    try
      ascom_mount := CreateOleObject(ascom_mount_driver); {See at the end of this file the Ascom fix 2018, """"SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]); """"}
      ascom_mount.connected:=true;
      //if sender<>nil then Showmessage(ascom_mount.Description+#13+#10+#13+#10+ascom_mount.DriverInfo);{show this only when called from popup menu}
      ascom_mount.Tracking:=true;{set tracking on}
      connect_mount1.caption:=ascom_mount_driver;
      connect_mount1.font.style:=[fsbold,fsunderline];
      ascom_mount_connected:=true;
      memo2_message('Mount connected.');
    except
      on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
      else
      begin {other errors}
        connect_mount1.font.color:=cldefault;
        connect_mount1.caption:='Connect mount';
        connect_mount1.font.style:=[];
        Showmessage('Error ASCOM driver "'+ascom_mount_driver+'" not found!');
      end;
    end; {end setup Ascom}
  end;

  except {Ascom alive}
    Showmessage('Error, No ASCOM detected. Install from http://ascom-standards.org');
  end;
  {$else}
    {Not available in Linux}
  {$endif}
end;


procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if key=#27 then
  begin
    esc_pressed:=true;
    application.processmessages;
  end;
end;


procedure TForm1.skyview_selected1Change(Sender: TObject);
begin
  form1.width_pixels1.enabled:=eso_selected1.checked=false;
  form1.height_pixels1.enabled:=eso_selected1.checked=false;
  deletefile(pchar(form1.path_to_image1.text+'\image.tmp'));{could be gif or jpeg. Delete to prevent errors}

  oldDEC_telescope:=oldDEC_telescope+0.01; {trigger an refresh image}
end;


procedure TForm1.manipulations1Change(Sender: TObject);
begin
 update_required:=true;
end;


procedure TForm1.Timer1Timer(Sender: TObject);
begin
  mount_simulation;{move mount if required}
  focuser_simulation;{move focuser if required}
  rotator_simulation;{move rotator if required}
  camera_simulation(form1.fast_simulation1.checked);
  guide_camera_simulation(form1.fast_simulation1.checked);
end;


procedure TForm1.width_pixels1Exit(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.stopbutton1Click(Sender: TObject);
begin
   esc_pressed:=true;
   memo2_message('Stop pressed.');
end;


begin
  GetDocumentsPath;
  application_path:= extractfilepath(application.location);{for u290, set path}

 {ASCOM fix 2018}
  {"failed to load driver" fix by Patrick Chevalley for ASCOM problem getting properties of "ASCOM.AdvancedLX200.Telescope"}
  {   Probably .NET and other VB disable this FPU exception by default and this is why the exception in the driver initialization do not hurt them.}
  {   Now I add this to Skychart and also to CCDciel to be sure it continue to work if there is a change in the TAchart package.}
 {$if defined(cpui386) or defined(cpux86_64)}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,exOverflow, exUnderflow, exPrecision]);
 {$endif}
end.
