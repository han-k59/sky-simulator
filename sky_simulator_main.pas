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
  Variants, clipbrd, Spin, Grids, sky_annotation,
  sky_simulator_unit_save_image,
  cu_alpacaserver, cu_alpacadevice, alpaca_mount_protocol, alpaca_camera_protocol,alpaca_guidecamera_protocol,alpaca_focuser_protocol,alpaca_filterwheel_protocol,alpaca_rotator_protocol, Types;


type

  { TForm1 }
  TForm1 = class(TForm)
    abort_slew2: TMenuItem;
    activate_filelog1: TCheckBox;
    alpaca_adress1: TEdit;
    alpaca_groupBox1: TGroupBox;
    alpaca_port1: TEdit;
    alpaca_port_number1: TLabel;
    azimuth_error1: TFloatSpinEdit;
    bayer_change_warning1: TLabel;
    file_form_disk1: TBitBtn;
    Button1: TButton;
    file_from_disk_selected1: TRadioButton;
    GroupBox_camera_chooser1: TGroupBox;
    parked1: TMenuItem;
    Separator8: TMenuItem;
    tracking1: TMenuItem;
    pixelsizemicrometer1: TEdit;
    j2000_to_Jnow1: TMenuItem;
    Label18: TLabel;
    Label25: TLabel;
    Label27: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    File_from_disk2: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    message1: TLabel;
    park_label1: TLabel;
    focallength1: TEdit;
    ra_rate1: TLabel;
    offset1: TEdit;
    offset2: TEdit;
    offset3: TEdit;
    offset4: TEdit;
    offset5: TEdit;
    offset6: TEdit;
    offset7: TEdit;
    filter_1: TEdit;
    filter_2: TEdit;
    filter_3: TEdit;
    filter_4: TEdit;
    filter_5: TEdit;
    filter_6: TEdit;
    filter_7: TEdit;
    labelerrorRA1: TLabel;
    labelerrorDEC1: TLabel;
    manipulations1: TComboBox;
    OpenDialog1: TOpenDialog;
    seeingRA1: TLabel;
    seeingDEC1: TLabel;
    Separator7: TMenuItem;
    slew_to_jnow2: TMenuItem;
    slew_to_jnow1: TMenuItem;
    polar_error_describtion1: TLabel;
    polar_alignment_error1: TCheckBox;
    elevation_error1: TFloatSpinEdit;
    groupbox_polar_alignment_error1: TGroupBox;
    Label11: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label_azimuth1: TLabel;
    Label_elevation1: TLabel;
    Label_latitude1: TLabel;
    Label_longitude1: TLabel;
    latitude1: TEdit;
    longitude1: TEdit;
    equinox_communication1: TComboBox;
    jnow_to_j2000_1: TMenuItem;
    Slew_to_radec1: TMenuItem;
    dec_jnow1: TLabel;
    ra_jnow1: TLabel;
    slew_to_test_position1: TButton;
    tab_filterwheel1: TTabSheet;
    tab_alpaca1: TTabSheet;
    Angle2: TEdit;
    arrowleft1: TLabel;
    backlash1: TEdit;
    backlash_mount1: TCheckBox;
    about1: TButton;
    buttonEast1: TButton;
    buttonNorth1: TButton;
    buttonSouth1: TButton;
    buttonWest1: TButton;
    connect_focuser1: TBitBtn;
    connect_mount1: TBitBtn;
    connect_rotator1: TBitBtn;
    artificial_selected1: TRadioButton;
    dec1: TLabel;
    DecPulseReverses1: TCheckBox;
    esobutton1: TButton;
    eso_selected1: TRadioButton;
    exit1: TButton;
    fast_simulation1: TCheckBox;
    flipH1: TCheckBox;
    fliptext1: TCheckBox;
    flipV1: TCheckBox;
    focal_ratio1: TComboBox;
    focuser_alpaca1: TRadioButton;
    focuser_ascom1: TRadioButton;
    focuser_position1: TLabel;
    focuser_position2: TLabel;
    focuser_position3: TLabel;
    focuser_position4: TLabel;
    focuser_position5: TLabel;
    focuser_position6: TLabel;
    focuser_position7: TLabel;
    focusfactor1: TLabel;
    focus_at1: TEdit;
    focus_range1: TEdit;
    GroupBox1: TGroupBox;
    groupbox_mount_errors1: TGroupBox;
    groupbox_mount_offset1: TGroupBox;
    height_arcmin1: TEdit;
    height_pixels1: TEdit;
    hotpixels1: TComboBox;
    imagecounter1: TLabel;
    ImageList_colors: TImageList;
    instruction2: TLabel;
    internetESO1: TEdit;
    internetskyview1: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    looking_at1: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label26: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    abort_slew1: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    slew_to_mouse_pos_image1: TMenuItem;
    menu_slew_to1: TMenuItem;
    mouse_position1: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    popupmenu_map1: TPopupMenu;
    telescope_cursor1: TShape;
    mount_noise1: TComboBox;
    mount_periodic_error1: TComboBox;
    panel_sky1: TPanel;
    save_as_fits1: TMenuItem;
    save_as_tiff1: TMenuItem;
    mount_goto1: TMenuItem;
    save_imaga_as1: TMenuItem;
    image_to_clipboard1: TMenuItem;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    mount1: TLabel;
    mount_alpaca1: TRadioButton;
    mount_ascom1: TRadioButton;
    mount_type1: TComboBox;
    NSswapped1: TCheckBox;
    Panel1: TPanel;
    filter_alpaca_text1: TEdit;
    Label17: TLabel;
    Memo2: TMemo;
    menucopy1: TMenuItem;
    Menufind1: TMenuItem;
    menufindnext1: TMenuItem;
    MenuItem23: TMenuItem;
    plotted_info1: TComboBox;
    pointing1: TLabel;
    PopupMenu_image1: TPopupMenu;
    PopupMenu_memo1: TPopupMenu;
    ra1: TLabel;
    real_position1: TLabel;
    rotator_alpaca1: TRadioButton;
    rotator_ascom1: TRadioButton;
    rotator_position1: TLabel;
    rotator_reverse1: TCheckBox;
    rotator_setpoint1: TLabel;
    SaveDialog1: TSaveDialog;
    savesettings1: TButton;
    scale1: TLabel;
    select_all1: TMenuItem;
    clear_log_button1: TButton;
    activate_log1: TCheckBox;
    Discovery_Info: TLabel;
    elevation: TEdit;
    go_default1: TButton;
    Image1: TImage;
    Label16: TLabel;
    Memo1: TMemo;
    PageControl1: TPageControl;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Server_Info: TLabel;
    skyviewbutton1: TButton;
    skyview_selected1: TRadioButton;
    start_button1: TButton;
    star_database1: TComboBox;
    StatusBar1: TStatusBar;
    tab_image_source1: TTabSheet;
    tab_Image1: TTabSheet;
    tab_log1: TTabSheet;
    stopbutton1: TButton;
    tab_camera1: TTabSheet;
    tab_rotator1: TTabSheet;
    tab_mount1: TTabSheet;
    tab_focuser1: TTabSheet;
    telescope_position2000_dec1: TLabel;
    telescope_position2000_ra1: TLabel;
    telescope_positionJnow_ra1: TLabel;
    telescope_positionJnow_dec1: TLabel;
    Timer1: TTimer;
    UpDown1: TUpDown;
    UpDown_1: TUpDown;
    UpDown_2: TUpDown;
    UpDown_3: TUpDown;
    UpDown_4: TUpDown;
    UpDown_5: TUpDown;
    UpDown_6: TUpDown;
    UpDown_7: TUpDown;
    width_arcmin1: TLabel;
    width_pixels1: TEdit;
    procedure Angle2Change(Sender: TObject);
    procedure azimuth_error1Change(Sender: TObject);
    procedure azimuth_error1Exit(Sender: TObject);
    procedure azimuth_updown1Changing(Sender: TObject; var AllowChange: Boolean
      );
    procedure backlash1Exit(Sender: TObject);
    procedure about1Click(Sender: TObject);
    procedure file_form_disk1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure elevation_error1Change(Sender: TObject);
    procedure equinox_communication1Change(Sender: TObject);
    procedure fliptext1Change(Sender: TObject);
    procedure focallength1Change(Sender: TObject);
    procedure focallength1Exit(Sender: TObject);
    procedure j2000_to_Jnow1Click(Sender: TObject);
    procedure jnow_to_j2000_1Click(Sender: TObject);
    procedure parked1Click(Sender: TObject);
    procedure tracking1Click(Sender: TObject);
    procedure pixelsizemicrometer1Exit(Sender: TObject);
    procedure polar_alignment_mode1Change(Sender: TObject);
    procedure popupmenu_map1Popup(Sender: TObject);
    procedure slew_to_test_position1Click(Sender: TObject);
    procedure buttonEast1Click(Sender: TObject);
    procedure buttonNorth1Click(Sender: TObject);
    procedure buttonSouth1Click(Sender: TObject);
    procedure buttonWest1Click(Sender: TObject);
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
    procedure height_arcmin1Exit(Sender: TObject);
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
    procedure internetESO1Exit(Sender: TObject);
    procedure internetskyview1Exit(Sender: TObject);
    procedure latitude1Exit(Sender: TObject);
    procedure longitude1Exit(Sender: TObject);
    procedure focal_ratio1Change(Sender: TObject);
    procedure menucopy1Click(Sender: TObject);
    procedure Menufind1Click(Sender: TObject);
    procedure menufindnext1Click(Sender: TObject);
    procedure abort_slew1Click(Sender: TObject);
    procedure slew_to_mouse_pos_image1Click(Sender: TObject);
    procedure menu_slew_to1Click(Sender: TObject);
    procedure panel_sky1MouseLeave(Sender: TObject);
    procedure panel_sky1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure panel_sky1Paint(Sender: TObject);
    procedure save_as_fits1Click(Sender: TObject);
    procedure save_as_tiff1Click(Sender: TObject);
    procedure save_imaga_as1Click(Sender: TObject);
    procedure image_to_clipboard1Click(Sender: TObject);
    procedure mount_goto1Click(Sender: TObject);
    procedure mount_alpaca1Change(Sender: TObject);
    procedure mount_type1Change(Sender: TObject);
    procedure plotted_info1Change(Sender: TObject);
    procedure polar_alignment_error1Change(Sender: TObject);
    procedure PopupMenu_image1Popup(Sender: TObject);
    procedure rotator_alpaca1Change(Sender: TObject);
    procedure savesettings1Click(Sender: TObject);
    procedure select_all1Click(Sender: TObject);
    procedure skyviewbutton1Click(Sender: TObject);
    procedure start_button1Click(Sender: TObject);
    procedure star_database1Change(Sender: TObject);
    procedure star_database1DropDown(Sender: TObject);
    procedure stopbutton1Click(Sender: TObject);
    procedure connect_focuser1Click(Sender: TObject);
    procedure connect_mount1Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure skyview_selected1Change(Sender: TObject);
    procedure manipulations1Change(Sender: TObject);
    procedure tab_mount1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure telescope_cursor1MouseEnter(Sender: TObject);
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
    MyFilterwheel : T_Alpaca_Filterwheel;
    procedure ShowError(var msg:string);{for Alpaca}
    procedure ShowMsg(var msg:string);{for Alpaca}
    procedure PortMsg(var msg:string);{for Alpaca}
    procedure DiscoveryPortMsg(var msg:string);{for Alpaca}
  public

  end;

var
  Form1: TForm1;


  ra_telescope_2000,dec_telescope_2000,ra_mount_indication,dec_mount_indication,ra_mount_indication_2000,dec_mount_indication_2000,jd : double;
  focuser_position,side_of_pier             : integer;
  dss_bitmap                                : graphics.TBitmap;

  sidereal_time: double=0;
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
  procedure precession5(jd_start, jd_end, ra_in, dec_in: double; out ra_out,dec_out: double); {Rigorious method,  Meeus formulas 20.2, 20.3,20.4 }
  function strtofloat2(s:string): double;{works with either dot or komma as decimal separator}
  function floattostrFdot(const x:double; width2,decimals1 :word): string;

type
   Timg = array of array of word;

implementation

{$R *.lfm}

var
  oldreal_position,real_focuser_position,rotator_position,oldrotator_position : integer;
  rotator_reverse : boolean;
  panel_sky1RA: double=0;
  panel_sky1DEC: double=0;

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
  gotopos: string=''; //for got0 mount in popup menu image

  ascom_mount           : variant; {for Ascom, telescope}
  ascom_mount_capability:integer=0; {2= async slew, 1 slew, 0 no slew}
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
  form1.memo2.lines.add(TimeToStr(time)+'  '+s); {fill memo2 with tab_log1}
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
    filter:=filter_alpaca_text1.text;
    trim(filter);
    if length(filter)>1 then
      if pos(filter,msg)=0 then exit;

    if memo1.lines.count>400 then
    begin
      len:=length(memo1.text);
      memo1.text:=copy(memo1.text,len div 2, 99999999999999);{quick method to half tab_log1, Memo1.Lines.delete(0) is too slow}
    end;

    starttick:=gettickcount64;
    duration:=starttick - old_starttick;

    if ((duration>2000) and (duration<1000000)) then
       Memo1.Lines.Add('■■■■■■■■■ Duration: '+ floattostr(round((starttick - old_starttick)/100)/10)+' sec.');
    old_starttick:=starttick;


    Memo1.Lines.Add(time_inc_ms+'  '+msg);{add to tab_log1}
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

procedure start_alpaca(cam,foc,mount,rot,whe: boolean);
begin
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
     //  {$ifdef mswindows}
     //  tab_mount1.imageindex:=2 //blue
     // {$else}
     //  tab_mount1.imageindex:=4 //play gray icon
     // {$endif}
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
    if Whe then
    begin
      MyFilterWheel:=T_Alpaca_filterwheel.Create(nil);
      AlpacaServer.AddDevice(filterwheel,MyFilterwheel);
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


function strtofloat2(s:string): double;{works with either dot or komma as decimal separator}
var
  error1:integer;
begin
  s:=StringReplace(s,',','.',[]); {replaces komma by dot}
  s:=trim(s); {remove spaces}
  val(s,result,error1);
  if error1<>0 then result:=0;
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
      procedure doreset;
      begin
        initstring.Free;
//        form1.path_to_image1.text:=documents_path;{set default path}
        form1.top:=0;{for case the form was not set at the main screen}
        form1.left:=0;
      end;

begin
  result:=false;{assume failure}


  initstring := Tstringlist.Create;
   with initstring do
   begin
     try
      loadfromFile(lpath); { load from file}
     except
       doreset;
       exit; {no cfg file}
     end;
   end;
  with form1 do
  begin
    i:=987654321; get_int(i,'window_left2');
    if i<>987654321 then
    begin
      form1.left:=i;
    end
    else
    begin //bad file
      doreset;
      exit;
    end;
    i:=form1.top;get_int(i,'window_top2'); form1.top:=i;
    i:=form1.height;get_int(i,'window_height'); form1.height:=i;
    i:=form1.width;get_int(i,'window_width'); form1.width:=i;

    form1.skyview_selected1.checked:=get_boolean('skyview',true);
    form1.eso_selected1.checked:=get_boolean('eso',false);
    form1.file_from_disk_selected1.checked:=get_boolean('sfile',false);


    form1.artificial_selected1.checked:=get_boolean('database',false);
    form1.flipv1.checked:=get_boolean('flipV',false);
    form1.flipH1.checked:=get_boolean('flipH',false);

    form1.mount_alpaca1.checked:=get_boolean('mount_alpaca',false);
    form1.focuser_alpaca1.checked:=get_boolean('focuser_alpaca',false);
    form1.rotator_alpaca1.checked:=get_boolean('rotator_alpaca',false);
    form1.DecPulseReverses1.checked:=get_boolean('decreverses',true);
    form1.NSswapped1.checked:=get_boolean('NSswapped',false);

    form1.fast_simulation1.checked:=get_boolean('fastsim',true);


    dum:=initstring.Values['elevation_e']; if dum<>'' then form1.elevation_error1.text:=dum;
    dum:=initstring.Values['azimuth_e']; if dum<>'' then form1.azimuth_error1.text:=dum;
    dum:=initstring.Values['latitude']; if dum<>'' then form1.latitude1.text:=dum;
    dum:=initstring.Values['longtude']; if dum<>'' then form1.longitude1.text:=dum;

    dum:=initstring.Values['height']; if dum<>'' then form1.height_arcmin1.text:=dum;
    dum:=initstring.Values['width_pixels']; if dum<>'' then form1.width_pixels1.text:=dum;
    dum:=initstring.Values['height_pixels']; if dum<>'' then form1.height_pixels1.text:=dum;

    dum:=initstring.Values['px_size']; if dum<>'' then form1.pixelsizemicrometer1.text:=dum;
    dum:=initstring.Values['focal_length']; if dum<>'' then form1.focallength1.text:=dum;

    dum:=initstring.Values['star_database']; if dum<>'' then form1.star_database1.text:=dum;
    dum:=initstring.Values['f-ratio']; if dum<>'' then form1.focal_ratio1.text:=dum;

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
    dum:=initstring.Values['input_image']; if dum<>'' then form1.file_from_disk2.caption:=dum;



    i:=form1.mount_noise1.itemindex;get_int(i,'mount_noise'); form1.mount_noise1.itemindex:=i;
    i:=form1.mount_periodic_error1.itemindex;get_int(i,'mount_periodic_error'); form1.mount_periodic_error1.itemindex:=i;
    i:=form1.plotted_info1.itemindex;get_int(i,'labels'); form1.plotted_info1.itemindex:=i;
    form1.fliptext1.checked:=get_boolean('fliptext',false);
    form1.backlash_mount1.checked:=get_boolean('backlash_mount',false);
    i:=form1.manipulations1.itemindex;get_int(i,'manipulations'); form1.manipulations1.itemindex:=i;

    i:=form1.mount_type1.itemindex;get_int(i,'mount_type'); form1.mount_type1.itemindex:=i;
    i:=form1.equinox_communication1.itemindex; get_int(i,'alpaca_equinox'); form1.equinox_communication1.itemindex:=i;

    form1.polar_alignment_error1.checked:=get_boolean('polar_alignment',false);

    dum:=initstring.Values['filter1']; if dum<>'' then form1.filter_1.text:=dum;
    dum:=initstring.Values['filter2']; if dum<>'' then form1.filter_2.text:=dum;
    dum:=initstring.Values['filter3']; if dum<>'' then form1.filter_3.text:=dum;
    dum:=initstring.Values['filter4']; if dum<>'' then form1.filter_4.text:=dum;
    dum:=initstring.Values['filter5']; if dum<>'' then form1.filter_5.text:=dum;
    dum:=initstring.Values['filter6']; if dum<>'' then form1.filter_6.text:=dum;
    dum:=initstring.Values['filter7']; if dum<>'' then form1.filter_7.text:=dum;
    dum:=initstring.Values['offset1']; if dum<>'' then form1.offset1.text:=dum;
    dum:=initstring.Values['offset2']; if dum<>'' then form1.offset2.text:=dum;
    dum:=initstring.Values['offset3']; if dum<>'' then form1.offset3.text:=dum;
    dum:=initstring.Values['offset4']; if dum<>'' then form1.offset4.text:=dum;
    dum:=initstring.Values['offset5']; if dum<>'' then form1.offset5.text:=dum;
    dum:=initstring.Values['offset6']; if dum<>'' then form1.offset6.text:=dum;
    dum:=initstring.Values['offset7']; if dum<>'' then form1.offset7.text:=dum;
  end;

  initstring.free;
  result:=true;
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
    initstring.Values['sfile']:=BoolStr[file_from_disk_selected1.checked];
    initstring.Values['database']:=BoolStr[artificial_selected1.checked];

    initstring.Values['flipV']:=BoolStr[flipv1.checked];
    initstring.Values['flipH']:=BoolStr[fliph1.checked];

    initstring.Values['mount_alpaca']:=BoolStr[form1.mount_alpaca1.checked];
    initstring.Values['focuser_alpaca']:=BoolStr[form1.focuser_alpaca1.checked];
    initstring.Values['rotator_alpaca']:=BoolStr[form1.rotator_alpaca1.checked];

    initstring.Values['fastsim']:=BoolStr[form1.fast_simulation1.checked];
    initstring.Values['decreverses']:=BoolStr[form1.DecPulseReverses1.checked];
    initstring.Values['NSswapped']:=BoolStr[form1.NSswapped1.checked];

    initstring.Values['mount_type']:=inttostr(mount_type1.itemindex);
    initstring.Values['alpaca_equinox']:=inttostr(equinox_communication1.itemindex);


    initstring.Values['elevation_e']:=form1.elevation_error1.text;
    initstring.Values['azimuth_e']:=form1.azimuth_error1.text;
    initstring.Values['latitude']:=form1.latitude1.text;
    initstring.Values['longtude']:=form1.longitude1.text;

    initstring.Values['height']:=form1.height_arcmin1.text;
    initstring.Values['width_pixels']:=form1.width_pixels1.text;
    initstring.Values['height_pixels']:=form1.height_pixels1.text;
    initstring.Values['px_size']:=form1.pixelsizemicrometer1.text;
    initstring.Values['focal_length']:=form1.focallength1.text;

    initstring.Values['star_database']:=form1.star_database1.text;
    initstring.Values['f-ratio']:=form1.focal_ratio1.text;

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
    initstring.Values['input_image']:=File_from_disk2.caption;

    initstring.Values['mount_noise']:=inttostr(form1.mount_noise1.itemindex);
    initstring.Values['mount_periodic_error']:=inttostr(form1.mount_periodic_error1.itemindex);

    initstring.Values['labels']:=inttostr(form1.plotted_info1.itemindex); {0 None, 1 HFD, 2 Info, 3 Objects, 4 All}
    initstring.Values['fliptext']:=BoolStr[ form1.fliptext1.checked];
    initstring.Values['backlash_mount']:=BoolStr[ form1.backlash_mount1.checked];
    initstring.Values['manipulations']:=inttostr(form1.manipulations1.itemindex);

    initstring.Values['polar_alignment']:=BoolStr[ form1.polar_alignment_error1.checked];

    initstring.Values['filter1']:=form1.filter_1.text;
    initstring.Values['filter2']:=form1.filter_2.text;
    initstring.Values['filter3']:=form1.filter_3.text;
    initstring.Values['filter4']:=form1.filter_4.text;
    initstring.Values['filter5']:=form1.filter_5.text;
    initstring.Values['filter6']:=form1.filter_6.text;
    initstring.Values['filter7']:=form1.filter_7.text;

    initstring.Values['offset1']:=form1.offset1.text;
    initstring.Values['offset2']:=form1.offset2.text;
    initstring.Values['offset3']:=form1.offset3.text;
    initstring.Values['offset4']:=form1.offset4.text;
    initstring.Values['offset5']:=form1.offset5.text;
    initstring.Values['offset6']:=form1.offset6.text;
    initstring.Values['offset7']:=form1.offset7.text;


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

procedure ang_sep(ra1,dec1,ra_jnow1,dec2 : double;out sep: double);{version 2018-5-23, calculates angular separation. according formula 9.1 old Meeus or 16.1 new Meeus, version 2018-5-23}
var
  sin_dec1,cos_dec1,sin_dec2,cos_dec2, cos_sep:double;
begin
  sincos(dec1,sin_dec1,cos_dec1);{use sincos function for speed}
  sincos(dec2,sin_dec2,cos_dec2);

  cos_sep:=sin_dec1*sin_dec2+ cos_dec1*cos_dec2*cos(ra1-ra_jnow1);
  if cos_sep>1 then cos_sep:=1 else if cos_sep<-1 then cos_sep:=1; {sometimes 1.0000000000000002 resulting in an arccos error}
  sep:=arccos(cos_sep);
end;


function fnmodulo(x,range: double):double;
begin
  {range should be 2*pi or 24 hours or 0 .. 360}
  result:=range*frac(x/range);
  if result<0 then result:=result+range;   {do not like negative numbers}
end;


procedure calc_sidereal_time(longitude: double);{local sidereal time [0..2*pi]}
const
  siderealtime2000=(280.46061837)*pi/180;{[radians], sidereal time at 2000 jan 1.5 UT (12 hours) =Jd 2451545 at meridian greenwich, see new Meeus 11.4}
  earth_angular_velocity = pi*2*1.00273790935; {about(365.25+1)/365.25) or better (365.2421874+1)/365.2421874 velocity daily. See new Meeus page 83}

begin
  sidereal_time:=fnmodulo(+longitude+siderealtime2000 +(jd - 2451545  )* earth_angular_velocity,2*pi); {As in the FITS header in ASTAP the site longitude is positive if East1 and has to be added to the time}
end;


//procedure precession(jd, ra1,dec1 : double; out ra_jnow1,dec2 : double); {precession correction,  new Meeus chapter precession formula 20.1}
//var
//  t,dra,ddec,m,n,n2  : double;
//begin
//  t:=(jd-2451545)/36525; {time in julian centuries since j2000 }
//  m:=3.07496+0.00186*t;{seconds}
//  n:=1.33621-0.00057*t; {seconds}
//  n2:=20.0431-0.0085*t;{arcsec}
//  dra:=(m + n *sin(ra1)*tan(dec1))*pi/(3600*12);{yearly ra drift in radians}
//  ddec:=n2*cos(ra1)*pi/(3600*180); {yearly dec drift in radians}
//  ra_jnow1:=ra1+(dra*t*100);{multiply with number of years is t*100}
//  dec2:=dec1+(ddec*t*100);
//end;


procedure precession5(jd_start, jd_end, ra_in, dec_in: double; out ra_out,dec_out: double); {Rigorious method,  Meeus formulas 20.2, 20.3,20.4 }
var
  a,b,c,zeta,z,theta, TT, t : double;
begin
  TT := (jd_start - 2451545.0) / 36525;//Julian centuries for the start epoch from a base epoch 2000.0
  t := (jd_end - jd_start) / 36525;//Julian centuries from the start to end epoch

  // Expressions in arc seconds
  zeta := (2306.2181 + 1.39656*TT - 0.000139*TT*TT)*t + (0.30188 - 0.000344*TT)*t*t +  (0.017998)*t*t*t;

  z := (2306.2181 + 1.39656*TT - 0.000139*TT*TT)*t + (1.09468 + 0.000066*TT)*t*t + (0.018203)*t*t*t;

  theta := (2004.3109 - 0.85330*TT - 0.000217*TT*TT)*t + (-0.42665 - 0.000217*TT)*t*t + (-0.041833)*t*t*t;

  zeta := zeta * PI / (180*3600); //Convert to radians
  z := z *  PI / (180*3600);
  theta := theta * PI / (180*3600);

  {Calculate the precession}
  a := sin(ra_in + zeta)*cos(dec_in);
  b := cos(ra_in + zeta)*cos(theta)*cos(dec_in) - sin(theta)*sin(dec_in);
  c := cos(ra_in + zeta)*sin(theta)*cos(dec_in) + cos(theta)*sin(dec_in);

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
    dss_bitmap.PixelFormat:=ajpg.PixelFormat;//copy format. Required to update PixelFormat
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
    dss_bitmap.PixelFormat:=gif.PixelFormat;//copy format. Required to update PixelFormat
    dss_bitmap.assign(gif); {keep for later}
  except
  end;
  gif.free
end;


procedure png_to_picture(filen1: string);
var
  apng: graphics.TPortableNetworkGraphic;
begin
  try
    apng := graphics.TPortableNetworkGraphic.Create;
    apng.LoadFromFile(filen1);
    dss_bitmap.PixelFormat:=apng.PixelFormat;//copy format. Required to update PixelFormat
    dss_bitmap.assign(apng); {keep for later for display if in focus}
  except
  end;
  apng.free;
end;


procedure getDSSimage(ra1,dec1 : double); {download DSS image}
var
  width_arcmin,height_arcmin  : double;
  pixelx, pixely,count,i      :integer;
  internetlink, destfile:string;
  myFile : File;
  charArray : array[0..255] of char;
  message   : string;
begin

  height_arcmin:=strtofloat(form1.height_arcmin1.text);
  width_arcmin:=strtofloat(form1.width_arcmin1.caption);

  pixelY:=strtoint(form1.height_pixels1.text);{height}
  pixelX:=strtoint(form1.width_pixels1.text);{height}

  memo2_message('DSS image download started.');

  if form1.skyview_selected1.checked then
  begin
    pixelY:=strtoint(form1.height_pixels1.text);{height}
    internetlink:= form1.internetskyview1.text;

    internetlink:=internetlink+'&VCOORD='+floattostrFdot(ra1*180/pi,0,4)+','+floattostrFdot(dec1*180/pi,0,4)+
      '&PIXELX='+inttostr(pixelx)+
      '&PIXELY='+inttostr(pixely)+
      '&SFACTR='+floattostrFdot(minmax(MAX(width_arcmin,height_arcmin),0.3,360*60)/60,0,4);{not limitations but skyview stops at around 140 degrees}

    form1.internetskyview1.hint:=internetlink;

      destfile:=documents_path+'image.tmpjpg';//fix for linux
      form1.scale1.Caption:=floattostrF(height_arcmin*60/pixelY,ffFixed,4,2);
  end
  else
  begin {eso}
    internetlink:= form1.internetESO1.text;   //  &ra=83.6072&dec=22.0533&x=46.6&y=36.4
    internetlink:=internetlink+'&ra='+floattostrFdot(ra1*180/pi,0,4)+'&dec='+floattostrFdot(dec1*180/pi,0,4)+'&x='+floattostrFdot(width_arcmin,0,1)+'&y='+floattostrFdot(height_arcmin,0,1);

    form1.internetESO1.hint:=internetlink;
    destfile:=documents_path+'image.tmpgif'; {gif file !!!} //fix for linux
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

      ra0:=ra1;//store image position same as in artificial image for looking at position
      dec0:=dec1;

    end;

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
var
  png: TPortableNetworkGraphic;
begin
  png := TPortableNetworkGraphic.Create;
  try
    png.Assign(form1.image1.Picture.graphic);
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
  setlength(img_array,h,w);

  siz:=form1.image1.Picture.Bitmap.RawImage.Description.BitsPerPixel div 8; {Bitmap.PixelFormat=pf24bit is not reliable in Linux}
  for y := 0 to h -1 do
  begin // scan each line
    xline:=form1.image1.Picture.Bitmap.ScanLine[y];
    for x := 0 to w-1 do
    begin
      if siz=3 then
        img_array[y,x]:=10*xLine^[x*3+2] {do only filter_2, increase level to 255*10=2550 [0..65535]}
      else {size=4}
        img_array[y,x]:=10*xLine^[x*4+2];{do only filter_2}
    end;
  end;
end;


procedure dss_bitmap_to_img_ARRAY_with_blur(factor :integer; osc: boolean);{blur dss_bitmap and put in img_array}
var w,h, x, y,j   : Integer;
    xLine         : PByteArray;
    xLineW        : PWordArray;
    img_array2    : array of array of single;
    bytesPerColour: integer;

begin
  try
    w:=dss_bitmap.width;
    h:=dss_bitmap.height;

    setlength(img_array,h,w);
    setlength(img_array2,h,w);

    if ((osc=false) or (dss_bitmap.pixelformat=pf8bit) or (dss_bitmap.pixelformat=pf16bit)) then //process as greyscale images
    begin
      if  dss_bitmap.pixelformat=pf32bit then
      begin
        for y := 0 to h -1 do
        begin
          xLine := dss_bitmap.ScanLine[y];
          for x := 0 to w -1 do
          begin
            img_array[y,x]:=(xLine^[x*4]+xLine^[x*4+1]+xLine^[x*4+2]);{Note THE ESO IMAGES DO NOT CONTAINS BLUE, ONLY filter_2 AND GREEN}
          end;
        end;
      end
      else
      if dss_bitmap.pixelformat=pf24bit then //grey scale 16 bit for single file
      begin
        for y := 0 to h -1 do
        begin
          xLine := dss_bitmap.ScanLine[y];
          for x := 0 to w -1 do
          begin
            img_array[y,x]:=xLine^[x*4]+xLine^[x*4+1]+xLine^[x*4+2];
          end;
        end;
      end
      else
      if dss_bitmap.pixelformat=pf8bit then
      begin
        for y := 0 to h -1 do
        begin
          xLine := dss_bitmap.ScanLine[y];
          for x := 0 to w -1 do
          begin
            img_array[y,x]:=xLine^[x];//DSS images are grayscale jpeg's
          end;
        end;
      end
      else
      if  dss_bitmap.pixelformat=pf16bit then //grey scale 16 bit for single file
      begin
        for y := 0 to h -1 do
        begin
          xLineW := dss_bitmap.ScanLine[y];
          for x := 0 to w -1 do
          begin
            img_array[y,x]:=xLineW^[x];//16 bit
          end;
        end;
    end
    else
    memo2_message('Error, unknown pixelformat!' +inttostr(integer(dss_bitmap.pixelformat)));//pf48bit and pf64bit are not avialable in compiler
  end
  else
  begin
    if  dss_bitmap.pixelformat=pf32bit then
    begin
      for y := 0 to h -1 do
      begin
        xLine := dss_bitmap.ScanLine[y];
        for x := 0 to w -1 do
        begin //Convert colour to OSC RGGB Bayer pattern
          if ((odd(x)=true ) and (odd(y)=true )) then
            img_array[y,x]:=xLine^[x*4] //filter_2
          else
          if ((odd(x)=false) and (odd(y)=false)) then
            img_array[y,x]:=xLine^[x*4+2] //blue
          else
          if ((odd(x)=false) and (odd(y)=true)) then
            img_array[y,x]:=xLine^[x*4+1] //green
          else
          if ((odd(x)=true) and (odd(y)=false)) then
            img_array[y,x]:=xLine^[x*4+1]; //green
        end;
      end;
    end
    else
    if dss_bitmap.pixelformat=pf24bit then //grey scale 16 bit for single file
    begin
      for y := 0 to h -1 do
      begin
        xLine := dss_bitmap.ScanLine[y];
        for x := 0 to w -1 do
        begin //Convert colour to OSC RGGB Bayer pattern
          if ((odd(x)=true ) and (odd(y)=true )) then
            img_array[y,x]:=xLine^[x*3] //filter_2
          else
          if ((odd(x)=false) and (odd(y)=false)) then
            img_array[y,x]:=xLine^[x*3+2] //blue
          else
          if ((odd(x)=false) and (odd(y)=true)) then
            img_array[y,x]:=xLine^[x*3+1] //green
          else
          if ((odd(x)=true) and (odd(y)=false)) then
            img_array[y,x]:=xLine^[x*3+1]; //green
        end;
      end;
    end
    else
    if dss_bitmap.pixelformat=pfCustom {=8} then //48 bit colour for single file
    begin
      for y := 0 to h -1 do
      begin
        xLineW := dss_bitmap.ScanLine[y]; //3x16 bit
        for x := 0 to w -1 do
        begin //Convert colour to OSC RGGB Bayer pattern
          if ((odd(x)=true ) and (odd(y)=true )) then
            img_array[y,x]:=xLineW^[x*3] //filter_2
          else
          if ((odd(x)=false) and (odd(y)=false)) then
            img_array[y,x]:=xLineW^[x*3+2] //blue
          else
          if ((odd(x)=false) and (odd(y)=true)) then
            img_array[y,x]:=xLineW^[x*3+1] //green
          else
          if ((odd(x)=true) and (odd(y)=false)) then
            img_array[y,x]:=xLineW^[x*3+1]; //green
        end;
      end;
    end
    else
    memo2_message('Error, unknown pixelformat! '+inttostr(integer(dss_bitmap.pixelformat)));//pf48bit and
  end;



    if factor>1 then  {blur active}
    begin
      for j:=1 to factor do
      begin
        for y := 1 to h -1-1 do {box blur}
          for x := 1 to w -1-1 do
            img_array2[y,x]:=(img_array[y+1,x-1]+img_array[y+1,x]+img_array[y+1,x+1]+
                              img_array[y  ,x-1]+img_array[y  ,x]+img_array[y  ,x+1]+
                              img_array[y-1,x-1]+img_array[y-1,x]+img_array[y-1,x+1]) / 9;

        for y := 1 to h -1-1 do {put back}
          for x := 1 to w -1-1 do
             img_array[y,x]:=img_array2[y,x];
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


procedure plot_sky_map;
var
  ra,dec,azimuth,x,y,oldx,halftextw,ra_step,dec_step,ww,hh : integer;
  ra_jnow1,dec2                     : double;

  procedure draw_line;
  begin
    with form1.panel_sky1.canvas do
    begin
      moveto(x,y-4);
      lineto(x,y+4);
      moveto(x,y);
    end;
  end;

begin
  with form1.panel_sky1.canvas do
  begin
    Brush.Color := clmenu;
    FillRect(form1.panel_sky1.ClientRect);//clear

    Pen.Width := 1;{thickness lines}
    pen.color := clgray;
    pen.style:=psdot;

    hh:=form1.panel_sky1.height-5;
    ww:=form1.panel_sky1.width-5;
    ra_step:=ww div 8;
    dec_step:=hh div 6;

    for ra:=0 to 8 do
    begin
      moveto(ra*ra_step{45},0);
      lineto(ra*ra_step{45},hh);
    end;

    for dec:=0 to 6 do
    begin
      moveto(0,dec*dec_step);
      lineto(ww-5,dec*dec_step);
    end;


    pen.color := clred;
    halftextw:=textwidth('N') div 2;
    latitude:=strtofloat2(form1.latitude1.text)*pi/180;;
    longitude:=strtofloat2(form1.longitude1.text)*pi/180;
    calc_jd;{calc julian day from system clock}
    calc_sidereal_time(longitude);{local sidereal time}

    oldx:=-999;
    for azimuth:=0 to 24 do
    begin
      az_ra2(azimuth*15*pi/180 ,0{alt},latitude,0{longitude},sidereal_time,{out} ra_jnow1,dec2);//conversion az,alt to ra,dec, longitude is POSITIVE when west. At south azimuth is 180
      x:=round((ww/360)*ra_jnow1*180/pi);
      y:=round((hh/180)*(90-(dec2*180/pi)));
      if azimuth=0 then
      begin
        textout(x- halftextw,y,'N');
        draw_line;
      end
      else
      if azimuth=6 then
      begin
        draw_line;
        textout(x-halftextw,y,'E');
      end
      else
      if azimuth=12 then
      begin
        draw_line;
        textout(x-halftextw,y,'S');
      end
      else
      if azimuth=18 then
      begin
        draw_line;
        textout(x-halftextw,y,'W');
      end;

      if abs(oldx-x)>100 then //first step or passed 0 or 2*pi
        moveto(x,y)
      else
        lineto(x,y);
      oldx:=x;
    end;
  end;
end;


procedure slew_ascom_telescope(ra5,dec5 :double);
begin
  if (ascom_mount.connected) then {allow export}
  begin
    try {parked ?}
      if ascom_mount.AtPark then
      begin
         Showmessage('Error! Mount parked !');
         exit;
      end;
      except
        on E: Exception do
        begin
          application.messagebox(pchar(E.Message),'ASCOM returns following error:',MB_ICONWARNING+MB_OK); {show error from driver error, for example below horizon}
        end;
    end;{park test}
    try {export}

    ascom_mount.tracking:=true; //required for slewing;
    if Ascom_mount_capability=2 then  ascom_mount.SlewToCoordinatesAsync(ra5, dec5)
    else
    if Ascom_mount_capability=1 then  ascom_mount.SlewToCoordinates(ra5, dec5)
    else;
    {nothing}
    except
      on E: Exception do
      begin
        sysutils.beep();
        application.messagebox(pchar(E.Message),'ASCOM returns following error:',MB_ICONWARNING+MB_OK); {show error from driver error, for example below horizon}
      end;
   end;
 end;
end;


procedure telescope_abortslew;
var
  mount : T_Alpaca_Mount;
begin
   if form1.mount_alpaca1.checked then
  begin
     mount := T_Alpaca_Mount.Create(nil);
    try
       Mount.abortslew;
     finally
       mount.Free;
    end;
  end
  else
  begin
    {$ifdef mswindows}

    if (ascom_mount.connected) then {allow export}
    begin
      try
        ascom_mount.AbortSlew;
      except
         on E: Exception do
         begin
           beep(1000,200);
           application.messagebox(pchar(E.Message),'ASCOM returns following error:',MB_ICONWARNING+MB_OK); {show error from driver error, for example below horizon}
         end;
      end;
    end;
    {$else} {unix}
    {$endif}
  end;
end;


procedure mount_goto(ra,dec: double);
var
  ok : boolean;
  mount : T_Alpaca_Mount;
begin
  if form1.mount_alpaca1.checked then
  begin
    if is_parked<>0 then
    begin
      Showmessage('Can not slew. Alpaca Mount is parked !');
      exit;
    end;
    mount := T_Alpaca_Mount.Create(nil);
    try
      Mount.slewtocoordinates(fnmodulo(Ra,24), Dec, ok);
    finally
      mount.Free;
    end;
    if ok=false then
      application.messagebox(pchar('Error'),pchar('Range error!'),MB_ICONWARNING+MB_OK);
  end
  else
    slew_ascom_telescope(fnmodulo(Ra,24), Dec);
end;



var
  count_img: integer=-1;
procedure test;//test the simulator
var
  i, j : integer;
  thera,thedec : double;
begin
  application.processmessages;
  wait(2000);

  if count_img>=0 then
  begin
    thera:=count_img;  //hrs
    thedec:=90;
    equinox_communication:=1; //jnow
    mount_goto(count_img,89.666666666666666666666666667);

    memo2_message('file '+ inttostr(count_img));{message to memo2}
    save_fits('d:\temp\'+inttostr(count_img)+'.fits');

    if count_img>23 then halt;
  end;
  inc(count_img);
end;


procedure update_fov(ch : string ); //complicated logic to adaot to DSS download
begin
  with form1 do
  begin
    pixelsizemicrometer:=strtofloat2(pixelsizemicrometer1.caption);//always update since this procedure is called after loading settings
    height_arcmin:=strtofloat2(height_arcmin1.caption);
    width2:=strtoint(form1.width_pixels1.text);{width}
    height2:=strtoint(height_pixels1.caption);

    if eso_selected1.checked then
    begin
      height_arcmin:=minmax(height_arcmin,0.3,60);//limitation
      width_arcmin:=round(1.3333333333333333333*height_arcmin);
      height2:=round(height_arcmin*60/1.7);
      width2:=round(width_arcmin*60/1.7);
    end;

    focal_length_telescope:=(strtofloat2(focallength1.caption)/1000);//In meter. Always update since this procedure is called after loading settings

    if ((ch='hp') or (ch='ps') or (ch='fl')) then //change in height in pixel, pixel size
    begin
      height_arcmin:=(height2*pixelsizemicrometer/focal_length_telescope)*60*(180/1E6)/pi;
      height_arcmin1.caption:=floattostrf(height_arcmin, fffixed, 0, 0); {calculate image height in arc minutes}
    end;
    if ch='ha' then //change in height arcmin
    begin
      if focallength1.enabled then //not dicated by ascom driver
      begin
        focal_length_telescope:=(height2*pixelsizemicrometer/height_arcmin)*60*(180/1E6)/pi; //focal length in meter
        focallength1.caption:=floattostrf(focal_length_telescope*1000, fffixed, 0, 0);  //focal length in mm
      end
      else
      begin //focal length dictated by ascom mount. Adapt pixel size instead
        pixelsizemicrometer:=focal_length_telescope*height_arcmin/(height2*60*(180/1E6)/pi);
        pixelsizemicrometer1.caption:=floattostrf(pixelsizemicrometer, fffixed, 0, 1);
      end;
    end;

    scale1.Caption:=floattostrF(height_arcmin*60/height2,ffFixed,4,2);

    height_arcmin1.caption:=inttostr(round(height_arcmin));
    width_arcmin1.caption:=inttostr(round(height_arcmin*width2/height2));

    width_pixels1.text:=inttostr(width2);
    height_pixels1.text:=inttostr(height2);

  end;
end;


procedure TForm1.pixelsizemicrometer1Exit(Sender: TObject);
begin
  update_fov('ps');
  update_required:=true;
end;


procedure TForm1.focallength1Exit(Sender: TObject);
begin
  update_fov('fl');
  update_required:=true;
end;

procedure TForm1.height_arcmin1Exit(Sender: TObject);
begin
  update_fov('ha');
  update_required:=true;
end;


procedure TForm1.height_pixels1Exit(Sender: TObject);
begin
  update_fov('hp');
  update_required:=true;
end;



procedure simulate_sky;
const
   equinox_telescope  : integer = 0;
   slewtime     : integer=0;
   rotcounter   : integer=0;{unstable rotator}
   foccounter   : integer=0;{unstable focuser}
   cnt          : integer=0;
   cnt2         : integer=0;
var
    eqs, blur_factor,noise_index,periodic_error_index,old,backlash,focus_backlash                       : integer;
    hfd,seperation,seeing_errorRA, seeing_errorDEC, allowederror,ra3,dec3,dra,dDec,sep,cycletime,orient,dummy : double;
    mount_slewing, focal_length_ascom_driver_implemented      : boolean;
    Save_Cursor:TCursor;
    mess       : string;

begin
  esc_pressed:=false;
  focus_backlash:=0;
  pushbuttonRA:=0;
  pushbuttonDEC:=0;

  calc_jd; {calc jd}

//  while esc_pressed=false  do
  with form1 do
  repeat
  begin
    //if mount_slewing=false then test;

    application.processmessages;
    if esc_pressed then exit;

    if artificial_selected1.checked then
    begin
      if mount_alpaca1.checked then  //internal mount. More stable
        allowederror:=(0.03/(60*60))*pi/180 {allowed mount error 0.03 arc sec}
      else
        allowederror:=(0.1/(60*60))*pi/180 {GS server with HEQ5, allowed mount error 0.1 arc sec}
    end
    else
    allowederror:=(1/(60))*pi/180; {allowed mount error one arc minute}

    try
    if ((ascom_mount_connected) or (mount_alpaca1.checked)) then {if ascom_mount.connected then} {allow import}
    begin

      polar_alignment_error:=polar_alignment_error1.checked;
      telescope_cursor1.visible:=true;

      if mount_alpaca1.checked then
      begin
        if equinox_communication1.text='J2000' then
          equinox_communication:=2
        else
        if equinox_communication1.text='J2050' then
          equinox_communication:=3
        else
          equinox_communication:=1;//jnow

        ra_rate1.caption:='α_rate: '+floattostrF(3600*theaxisrates[0],fffixed,0,6)+'"/sec';

        ra_mount_indication:=alpaca_ra*pi/12;//read directly from the mount. Is in Jnow
        dec_mount_indication:=alpaca_dec*pi/180;

        ra_az2(ra_mount_indication,dec_mount_indication, latitude+elevation_error,0,sidereal_time{local incl longitude} {out}, alpaca_azimuth,alpaca_altitude);{conversion ra & dec to altitude, azimuth, longitude is POSITIVE when west. At south azimuth is 180 }

        precession5(jd, 2451545,ra_mount_indication,dec_mount_indication,ra_mount_indication_2000,dec_mount_indication_2000);//position to J2000 coordinate system
        if equinox_communication=2 then //communication in J2000
          equinox_telescope:=2000 //communication in
        else
          equinox_telescope:=0; //communication in jnow

        if is_parked=0 then
        begin
           if alpaca_tracking=false then
           begin
             park_label1.visible:=true;
             park_label1.caption:='Tracking off';
           end
           else
           begin
             park_label1.visible:=false
           end;
        end
        else
        begin
          park_label1.visible:=true;
          if is_parked=1 then park_label1.caption:='Slewing to park position';
          if is_parked=2 then park_label1.caption:='Mount parked';
        end;


        equinox_communication1.enabled:=true;//can be set for Alpaca simulator
        mount_slewing:=alpaca_mount_slewing;
        equatorial_mount:=(mount_type1.itemindex=1);
        DecPulseReverses:=DecPulseReverses1.checked;
        if NSswapped1.checked then NSswapped:=-1 else NSswapped:=1;

        //Alpaca side_of_pier is done later after processing time, location and so on
      end
      else
      begin  //ascom ascom_mount_connected
        park_label1.visible:=false;// not used for COM
        equinox_communication1.enabled:=false;//fixed
        try
          ra_mount_indication:=ascom_mount.RightAscension*pi/12; {equinox date}
          dec_mount_indication:=ascom_mount.declination*pi/180;


          if ascom_mount.atpark then
          begin
            park_label1.visible:=true;
            park_label1.caption:='Mount parked';
          end
          else
          if ascom_mount.tracking=false then
          begin
            park_label1.visible:=true;
            park_label1.caption:='Tracking off';
          end
          else
          begin
            park_label1.visible:=false
          end;

          focal_length_telescope:=0.333;
          focal_length_ascom_driver_implemented:=true;
          try
            focal_length_telescope:=ascom_mount.focallength;
          except
            focal_length_ascom_driver_implemented:=false;
            focallength1.enabled:=false;

          end;
          focallength1.enabled:=focal_length_ascom_driver_implemented=false;
          focallength1.caption:=floattostrf(focal_length_telescope*1000, fffixed, 0, 0);//all is updated in focallength1Change

          try side_of_pier:=ascom_mount.sideofpier except end;

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
          equinox_communication1.Text:='J2000';


        end
        else {Jnow}
        if equinox_telescope<=1 then
        begin
          precession5(jd,2451545 {go back to J2000}, ra_mount_indication,dec_mount_indication, ra_mount_indication_2000,dec_mount_indication_2000); {precession correction}
          equinox_communication1.Text:='Jnow';
        end
        else {2050}
        begin
          precession5(2451545+365.25*(equinox_telescope-2000),2451545 {go back to J2000}, ra_mount_indication,dec_mount_indication, ra_mount_indication_2000,dec_mount_indication_2000); {precession correction,  simple formula, new Meeus chapter precession} {convert telescope astrometric to 2000 for map}
          equinox_communication1.Text:='J2050';
        end;

        try
          mount_slewing:=false;
          if ascom_mount.CanSlewAsync then
             mount_slewing:=ascom_mount.Slewing
        except
           on E: Exception do memo2_message('Get slewing error: ' + E.Message);
        end;

      end;{ascom}
      ra1.Caption:=prepare_ra(ra_mount_indication_2000); //clean mount indication
      dec1.Caption:=prepare_dec(dec_mount_indication_2000);
      precession5(2451545,jd, ra_mount_indication_2000,dec_mount_indication_2000, ra_mount_indication,dec_mount_indication); {precession correction}
      ra_jnow1.Caption:=prepare_ra(ra_mount_indication);
      dec_jnow1.Caption:=prepare_dec(dec_mount_indication);

      telescope_cursor1.left:=round(ra_mount_indication_2000*(form1.panel_sky1.width-5)*0.5/pi) - telescope_cursor1.width div 2;
      telescope_cursor1.top:=round( ((form1.panel_sky1.height-5)/180)*(90-dec_mount_indication_2000*180/pi))- telescope_cursor1.height div 2;;

      try
      if ((ascom_rotator_connected) or (rotator_alpaca1.Checked)) then  {allow import rotator}
      begin
        oldrotator_position:=rotator_position;
        if rotator_alpaca1.Checked=false then {ascom}
        begin
          rotator_position:=ascom_rotator.position;
          rotator_reverse:=ascom_rotator.reverse;
          rotator_setpoint1.caption:='';
        end
        else {alpaca}
        begin
          rotator_position:=alpaca_rot_position_synced;
          rotator_setpoint1.caption:=inttostr(alpaca_rot_position_target_synced);
          rotator_reverse:=alpaca_rot_reverse;
        end;

        if rotator_position=oldrotator_position then {stable?}
        begin
          inc(rotcounter);
        end
        else {moving focuser}
        begin
          rotcounter:=0; {unstable rotator position}
          rotator_position1.Caption:=inttostr(rotator_position);
          rotator_reverse1.checked:=rotator_reverse;
          statusbar1.caption:='Rotator is rotating' ;
        end;

        if rotcounter=10 then {long enough stable, time for an update}
        begin
          if rotator_reverse then
            updown1.position:=round(fnmodulo(360-rotator_position,360))
          else
            updown1.position:=rotator_position;
          update_required:=true;
          memo2_message('Rotator reached new position.');
        end;
      end;

      //filterwheel
      filter_1.font.Bold:=alpaca_wheel_position=0;
      filter_2.font.Bold:=alpaca_wheel_position=1;
      filter_3.font.Bold:=alpaca_wheel_position=2;
      filter_4.font.Bold:=alpaca_wheel_position=3;
      filter_5.font.Bold:=alpaca_wheel_position=4;
      filter_6.font.Bold:=alpaca_wheel_position=5;
      filter_7.font.Bold:=alpaca_wheel_position=6;

      except
      end;

      {FOCUSER PROCESSING}
      try
      if ((ascom_focuser_connected) or (focuser_alpaca1.Checked)) then {allow import focuser}
      begin
        oldreal_position:=real_focuser_position;
        backlash:=strtoint(backlash1.text) div 2;

        old:= focuser_position;

        if focuser_alpaca1.Checked=false then {ascom}
          focuser_position:=ascom_focuser.position
        else {alpaca}
          focuser_position:=alpaca_foc_position;

        if focuser_position>old then begin focus_backlash:=-min(focuser_position-real_focuser_position,backlash); arrowleft1.caption:='🢁'+inttostr(focus_backlash); end
        else
        if focuser_position<old then begin focus_backlash:=+min(real_focuser_position-focuser_position,backlash);arrowleft1.caption:='🢃'+inttostr(focus_backlash); end;
        real_focuser_position:=focuser_position+focus_backlash;

        real_position1.Caption:=inttostr(real_focuser_position);
        focuser_position1.Caption:=inttostr(round(focuser_position));

        if oldreal_position=real_focuser_position then {stable focus}
        begin
          //log_to_file(documents_path+'\simulator_log.txt',DateTimeToStr(Now)+',     Indication,'+inttostr(round(focuser_position))+',    real_pos,'+inttostr(real_focuser_position)+',   hfd,'+floattostrFdot(hfd_calc(real_focuser_position,strtoint(focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(focus_range1.text)/10),0,2));
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
          real_position1.Caption:=inttostr(real_focuser_position);
          focuser_position1.Caption:=inttostr(round(focuser_position));
          statusbar1.caption:='Focuser is moving' ;
        end;
      end;
      except
      end;

      application.processmessages;
      if esc_pressed then exit;

      if mount_slewing then
      begin
        if slewtime=0 then memo2_message('Start slewing.');
        StatusBar1.simpletext:='Slewing('+inttostr(slewtime)+')......';
        slewtime:=slewtime+1;
      end
      else
      begin  //not slewing
        with form1 do
        if slewtime>0 then memo2_message('Slewing finished. MountJ2000 '+ra1.caption+',  '+dec1.caption+ ' | MountJNow '+ra_jnow1.caption+',  '+dec_Jnow1.caption+
              ' , TelescopeJ2000 '+telescope_position2000_ra1.caption+',  '+telescope_position2000_dec1.caption+ ' TelescopeJNow '+telescope_positionJnow_ra1.caption+',  '+telescope_positionJnow_dec1.caption );
        application.processmessages;
        seeing_errorRA:=0;
        seeing_errorDEC:=0;

        if artificial_selected1.checked then
        begin
          //update_required:=true;
          noise_index:=mount_noise1.itemindex;

          if noise_index=1 then
          begin
            seeing_errorRA:=randg(0,0.4)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec.}
            seeing_errorDEC:=randg(0,0.4)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec}
          end
          else
          if noise_index=2 then
          begin
            seeing_errorRA:=randg(0,1.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec.}
            seeing_errorDEC:=randg(0,1.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec}
          end
          else
          if noise_index=3 then
          begin
            seeing_errorRA:=randg(0,2.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec.}
            seeing_errorDEC:=randg(0,2.0)*((1/3600)*pi/180);{Random seeing error 1.0 arc sec}
          end;


          periodic_error_index:=mount_periodic_error1.itemindex;
          if periodic_error_index>0 then
          begin
            //0 None
            //1 Tracking error α, 10" square wave 1 min period
            //2 Tracking error α, 10" square wave 2 min period
            //3 Tracking error α, 10" square wave 5 min period
            //4 Tracking error δ, 10" square wave 1 min period
            //5 Tracking error δ, 10" square wave 2 min period
            //6 Tracking error δ, 10" square wave 5 min period
            //7 Tracking error α, 10" sinus wave 5 min period
            //8 Tracking error α, 20" sinus wave 5 min period

            case periodic_error_index of 1,4: cycletime:=1;
                                         2,5: cycletime:=2;
                                         3,6,7,8: cycletime:=2;
                                      end;

            if get_tracking_error(cycletime {min})*20/3600*pi/180 >0 then
            begin //positive part
              if periodic_error_index<=3 then //square ra error
                 seeing_errorRA:=seeing_errorRA +5*((1/3600)*pi/180) {square wave}
              else
                seeing_errorDEC:=seeing_errorDEC+5*((1/3600)*pi/180); {square wave};
            end
            else
            begin //negative part
              if periodic_error_index<=3 then
                 seeing_errorRA:=seeing_errorRA -5*((1/3600)*pi/180) {square wave}
              else
                seeing_errorDEC:=seeing_errorDEC-5*((1/3600)*pi/180); {square wave};
            end;

            if periodic_error_index=7 then // Tracking error α, 10" sinus wave 5 min period
              seeing_errorRA:=seeing_errorRA+get_tracking_error(5 {min})*(5/3600)*pi/180  {introduce 20 arc seconds cyclic error}
            else
            if periodic_error_index=8 then // Tracking error α, 20" in 5 min
              seeing_errorRA:=seeing_errorRA+get_tracking_error(5 {min})*(10/3600)*pi/180;{introduce 20 arc seconds cyclic error}
          end;

          if backlash_mount1.checked then //Mount backslash
          begin
            backlash:=100; //100 ms backlash
            update_required:=true; //force an update even for the smallest error or noise
          end
          else
          backlash:=0;

        end;//artificial image

        wait(1000);{slow down loop}


        dec_telescope_2000:=dec_mount_indication_2000+seeing_errorDEC+pushbuttonDEC;
        ra_telescope_2000:=ra_mount_indication_2000 + (seeing_errorRA+pushbuttonRA)/max(0.000000000001,cos(dec_telescope_2000));//make error independent of declication. This will not be the case in practise.
        ra_telescope_2000:=fnmodulo(ra_telescope_2000,2*pi);{keep in range 0..2pi}


        if update_required=false then
        begin
          ang_sep(oldRA_telescope,oldDEC_telescope,ra_telescope_2000,dec_telescope_2000, seperation);{find offset}

          if seperation>allowederror then update_required:=true; // already overruled for tracking error
        end;



        if ((update_required=false) and (polar_alignment_error) and  (time_passed(10))) {are 10 seconds passed and update required?} then
        begin
          update_required:=true;
          memo2_message('About 10 seconds passed, time to update image due to polar alignment error');
        end;

        if update_required  then {offset}
        begin

          telescope_cursor1.hint:=prepare_ra(ra_telescope_2000)+',  '+prepare_dec(dec_telescope_2000);//update telescope cursor hint

          if ((polar_alignment_error) or (equatorial_mount)) then
          begin
            latitude:=strtofloat(latitude1.text)*pi/180;;
            longitude:=strtofloat(longitude1.text)*pi/180;
            calc_sidereal_time(longitude);
            caption:='Local sidereal time '+copy(prepare_ra(sidereal_time),1,6);

            meridian:=sidereal_time*12/pi;//for equatorial mount alpaca. [hours]
            if mount_alpaca1.checked {alpaca} then
            begin
              if sideofpier_alpaca=99 then crosses_meridian(meridian); //initialise side of pier after program startup. Else it is done when slewing
              if  equatorial_mount=false then sideofpier_alpaca:=-1;
              side_of_pier:=sideofpier_alpaca; // is set while slewing in alpaca_mount_protocol.pas
            end;
          end
          else
          caption:='';

          case side_of_pier of //do this after above calculations
             0: pointing1.caption:='Pointing West';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
             1: pointing1.caption:='Pointing East';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
            else
              pointing1.caption:='-';  //0 = pierEast, 1 = pierWest, -1= pierUnknown
          end;



          if artificial_selected1.checked=false then {dss image or single file input}
          begin {dss image}
            if ((seperation>allowederror) or (update_required)) then {offset}
            begin
              if file_from_disk_selected1.checked then
              begin
                png_to_picture(file_from_disk2.caption);
                if ((dss_bitmap.PixelFormat=pf24bit) or (dss_bitmap.PixelFormat=pf32bit) or (dss_bitmap.PixelFormat=pfCustom)) then //colour
                begin
                  file_form_disk1.imageindex:=5; //Make from colour OSC raw
                  bayeroffset_X:=0;//RGGB
                  bayeroffset_Y:=0;
                  sensor_type:=2;//Bayer pattern
                end
                else
                begin
                  file_form_disk1.imageindex:=6;//keep Grey scale
                  sensor_type:=0;
                end;
              end
              else
              begin
                Save_Cursor := Screen.Cursor;
                Screen.Cursor := crHourglass;    { Show hourglass cursor }
                memo2_message('Downloading DSS image.');
                application.title:='. . . .';{show busy}
                application.processmessages;
                if esc_pressed then exit;
                if mount_alpaca1.checked {alpaca} then
                  getDSSimage(ra_telescope_2000-ra_corr*pi/180,dec_telescope_2000-dec_corr*pi/180){Download DSS image. Equinox 2000. Alpaca mount should have zero error but could be left over}
                else
                  getDSSimage(ra_telescope_2000,dec_telescope_2000);{Download DSS image. Equinox 2000}

                precession5(2451545, jd ,ra0,dec0,raJnow0,decJnow0);//position in Jnow coordinate system

                Screen.Cursor:=Save_Cursor;{back to normal cursor}
              end;
            end;
          end
          else
          begin  {new artificial image}
            application.title:='. . . .';{show busy}
            memo2_message('Preparing artificial image.');
            application.processmessages;
            if esc_pressed then exit;
            mess:='';
            if polar_alignment_error then
            begin
              elevation_error:=elevation_error1.value*pi/(180*60);
              azimuth_error:=azimuth_error1.value*pi/(180*60);
              if elevation_error>0 then mess:=floattostrF(abs(elevation_error)*(60*180/pi),fffixed,0,1)+char(39)+' above pole.'
              else
              if elevation_error<0 then mess:=floattostrF(abs(elevation_error)*(60*180/pi),fffixed,0,1)+char(39)+' below pole.';
              if azimuth_error>0 then mess:=mess+'  '+ floattostrF(abs(azimuth_error)*(60*180/pi)*cos(latitude),fffixed,0,1)+char(39)+' west of pole.'
              else
              if azimuth_error<0 then mess:=mess+'  '+ floattostrF(abs(azimuth_error)*(60*180/pi)*cos(latitude),fffixed,0,1)+char(39)+' east of pole.';
            end;
            polar_error_describtion1.caption:=mess;

            orient:=updown1.position*pi/180; {crota2,Image twist of Y axis(deg)}
            if ((equatorial_mount) and (side_of_pier=1)) then //  0 = pierEast/pointing West, 1 = pierWest/pointing East, -1= pierUnknown
                orient:=fnmodulo(orient+pi,2*pi); //rotation by meridian flip,

            if mount_alpaca1.checked {alpaca} then
            begin
              prepare_plotting(ra_telescope_2000-ra_corr*pi/180,dec_telescope_2000-dec_corr*pi/180, orient,flipH1.checked,flipV1.checked); {if the mount is synced it should not create a new image. Calculate absolute encoder position. This will allow the CCDCiel polar align routine to work proper and allow sync for first measurment}
              ang_sep(ra_telescope_2000,dec_telescope_2000,ra_telescope_2000-ra_corr*pi/180,dec_telescope_2000-ra_corr*pi/180, sep);{calculate offset}
              if sep>5*pi/180 then memo2_message('Telescope is '+inttostr(round(sep*180/pi))+'° out of sync! Astrometric solving is required.');
            end
            else
            prepare_plotting(ra_telescope_2000,dec_telescope_2000,orient,flipH1.checked,flipV1.checked);

            seeingRA1.caption:=floattostrF((seeing_errorRA)*3600*180/pi,ffFixed,3,1)+'"';
            seeingDEC1.caption:=floattostrF((seeing_errorDEC)*3600*180/pi,ffFixed,3,1)+'"';

           //if count_img=0 then    mess:=floattostr(ra0*12/pi)+',,,'+floattostr(dec0*180/pi)+',,,-25,'+inttostr(count_img)+',,-2'
           //               else       mess:=floattostr(ra0*12/pi)+',,,'+floattostr(dec0*180/pi)+',,,-25,'+inttostr(count_img)+',,-1';
           //log_to_file('c:\temp\test.sup',mess);{hnsky supplement for testing}

            hfd:=hfd_calc(real_focuser_position,strtoint(focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(focus_range1.text)/10); {a=2.35, b=2.35*1000/10, so hfd is 10 when focus position is 1000 position off}


            Save_Cursor := Screen.Cursor;
            Screen.Cursor := crHourglass;    { Show hourglass cursor }

            //polar alignment error will be applied in routine prepare_plot()
            //Plot stars. An offset of 1000 give a HFD of 10.
            plot_stars(real_focuser_position,strtoint(focus_at1.text){perfectfocusposition},2.35,2.35*strtoint(focus_range1.text)/10); {a=2.35, b=2.35*1000/10, so hfd is 10 when focus position is 1000 position off}
            load_deep;{load the deepsky database once. If loaded no action}
            if plotted_info1.itemindex < 5 then  {0 No labels, 1 HFD, 2 Info, 3 Objects labels, 4 All labels, 5 No deepsky, 6 No stars no deepsky}
              plot_deepsky;{plot the deep sky object on the image}

            image_array_stretched_to_screen;{luminance signal to screen. No noise in screen.  Noise is added in ascom driver or in alpaca_camera_protocol}
            Screen.Cursor:=Save_Cursor;{back to normal cursor}
            statusbar1.simpletext:=('');

            if abs(real_focuser_position-strtoint(focus_at1.text))>strtoint(focus_range1.text)/40 then  {2.5 % offset}
              focusfactor1.caption:='Out of focus.  HFD: '+ floattostrF(HFD,ffFixed,0,1)
              else focusfactor1.caption:='In focus.';

            inc(imagecounter);
            imagecounter1.caption:='Images created: '+inttostr(imagecounter);
          end;//artificial image

          telescope_position2000_ra1.caption:=prepare_ra(ra0) ;//Looking at.  ra0 is calculated in either procedure prepare_plotting and includes polar error or 2) recorded in procedure getDSSimage
          telescope_position2000_dec1.caption:=prepare_dec(dec0);
          telescope_positionjnow_ra1.caption:=prepare_ra(raJnow0);//Looking at. raJnow0 is calculated in procedure prepare_plotting and includes polar error
          telescope_positionJnow_dec1.caption:=prepare_dec(decJnow0);

          oldRA_telescope:=ra_telescope_2000; {}
          oldDEC_telescope:=dec_telescope_2000;

        end;{update required}

        slewtime:=0;
      end;//not slewing
    end
    else
    telescope_cursor1.visible:=false;

    except
    end;


    application.processmessages;
    if esc_pressed then exit;

    {PROCESSING OF THE PREPARED IMAGE}
    if ((ascom_mount_connected) or (mount_alpaca1.checked)) then
    if mount_slewing=false then
    begin
      try {update image for focus simulator}
      if update_required then
      begin
        memo2_message('Processing prepared image.');
        if ((artificial_selected1.checked=false {DSS image}) ) then {DSS}
        begin  { DDS IMAGES}
          blur_factor:=position_to_blurfactor(real_focuser_position);
          if blur_factor>=15 then
          begin
            memo2_message('At maximum focus position.');
          end
          else
          if blur_factor<>0 then
            memo2_message('Out of focus.');

          Image1.picture.Bitmap.Assign(dss_bitmap); {display in tab image always in focus}

          begin {ALPACA MODE PROCESSING DSS IMAGES}
            camera_state:=2;
            dss_bitmap_to_img_ARRAY_with_blur(blur_factor,file_from_disk_selected1.checked{create OSC if input is colour});{blur dss_bitmap, display and  convert to img_ARRAY}
            camera_state:=0; {ready}
          end;
        end
        else
        begin  {ARTIFICIAL IMAGE}
          try
            begin {ALPACA IMAGE}
              camera_state:=2;
              if artificial_selected1.checked=false {DSS image} then
              begin
                image1.Picture.assign(dss_bitmap);{show image}
                image_to_array(img_array);{convert timage to image array the_image}
              end
              else;{noting image is already availabe as array}
              camera_state:=0; {ready}
            end;

            if artificial_selected1.checked=false then
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
        with form1 do
        memo2_message('Image ready for download. J2000 '+telescope_position2000_ra1.caption+',  '+telescope_position2000_dec1.caption+ '   JNow '+telescope_positionJnow_ra1.caption+',  '+telescope_positionJnow_dec1.caption );

      end;{processing image}
      except
      end;
    end {not slewing}
    else {slewing}
    begin
    end;

    wait(200);
    if statusbar1.simpletext<>'' then inc(cnt);
    if cnt>30 then {leave message 30 cycles visible}
    begin
       statusbar1.simpletext:=''; {clear statusbar}
       application.title:='sky_simulator';
       cnt:=0;
    end;

    inc(cnt2);
    if cnt2>100 then //update sky map
    begin
      plot_sky_map;
      telescope_cursor1.Refresh;//show telescoe cursor again
      cnt2:=0;
    end;

//    application.processmessages;
//    if esc_pressed then exit;
  end;//endless repeat loop
  until false;

end;



procedure TForm1.start_button1Click(Sender: TObject);
begin
  if form1.start_button1.font.style=[fsbold] then begin esc_pressed:=true;exit;end;
  form1.start_button1.font.style:=[fsbold];

  if pos('-',form1.latitude1.text)<>0 then
       dec_encoder:=-abs(dec_encoder);//set internal mount postion at celestial pole

  if ((mount_alpaca1.checked=false) and (ascom_mount_connected=false)) then form1.connect_mount1Click(nil);
  if ((focuser_alpaca1.checked=false) and (ascom_focuser_connected=false)) then form1.connect_focuser1Click(nil);
  if ((rotator_alpaca1.checked=false) and (ascom_rotator_connected=false)) then form1.connect_rotator1Click(nil);


  start_alpaca(true {camera},form1.focuser_alpaca1.checked,form1.mount_alpaca1.checked,rotator_alpaca1.checked,true {wheel});

  if  mount_alpaca1.checked=false then
  begin
    if ascom_mount_connected then tab_mount1.imageindex:=1 {green} else tab_mount1.imageindex:=4;// grey
  end
  else
  {$ifdef mswindows}
   tab_mount1.imageindex:=2; //blue
  {$else}
   tab_mount1.imageindex:=4; //play gray icon
  {$endif}

  if focuser_alpaca1.checked=false then
  begin
    if ascom_focuser_connected then tab_focuser1.imageindex:=1 {green} else tab_focuser1.imageindex:=4;// grey
  end
  else
  {$ifdef mswindows}
   tab_focuser1.imageindex:=2; //blue
  {$else}
   tab_focuser1.imageindex:=4; //play gray icon
  {$endif}

  if  rotator_alpaca1.checked=false then
  begin
    if ascom_rotator_connected then tab_rotator1.imageindex:=1 {green} else tab_rotator1.imageindex:=4;// grey
  end
  else
  {$ifdef mswindows}
  tab_rotator1.imageindex:=2; //blue
  {$else}
  tab_rotator1.imageindex:=4; //play gray icon
  {$endif}

  {$ifdef mswindows}
  tab_camera1.imageindex:=2; //blue
  tab_filterwheel1.imageindex:=2; //blue
  {$else}
  tab_camera1.imageindex:=4; //play gray icon
  tab_filterwheel1.imageindex:=4; //play gray icon
  {$endif}

  form1.start_button1.caption:='Simulation running. (Hit ESC to stop)';

  simulate_sky;//run simulation
  form1.start_button1.font.style:=[];
  form1.start_button1.caption:='Start simulation';
  tab_camera1.imageindex:=3;//gray;
end;


procedure TForm1.star_database1Change(Sender: TObject);
begin
  update_required:=true;
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
  form1.statusbar1.simpletext:='STOPPING SERVER ................';

  stop_alpaca;
  wait(1000);
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

procedure TForm1.azimuth_error1Change(Sender: TObject);
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

procedure TForm1.about1Click(Sender: TObject);
begin
  showmessage('Sky simulator for ASCOM and Alpaca. © 2018-2024 by Han Kleijn, www.hnsky.org. ');
end;

procedure TForm1.file_form_disk1Click(Sender: TObject);
begin
 OpenDialog1.Title:='Select a png file';
 opendialog1.Filter:='PNG 8 or 16 bit|*.png';
 if OpenDialog1.Execute then
 begin
   file_from_disk2.caption:=OpenDialog1.Files[0];
   update_required:=true;
 end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ascom_driver    : widestring =''; //'ASCOM.Simulator.Telescope';
  V: variant;
begin
  {$ifdef mswindows}
  if form1.start_button1.font.style<>[fsbold] then
  begin
     application.messagebox(pchar('Start the simulation first.'),pchar('Abort!'),MB_OK);
     exit;
  end;
  try
  if not VarIsEmpty(ascom_mount) then  {ascom alive?}
  begin
    ascom_mount.connected:=false;
    ascom_mount := Unassigned;
  end
  else
  begin
    if sender<>nil then {send from popup menu}
    begin {show this only when called from popup menu}
      try
        V := CreateOleObject('ASCOM.Utilities.Chooser'); {for 64 bit applications as suggested in http://www.ap-i.net/mantis/view.php?id=778#bugnotes, "ASCOM." is required!!}
        except
        V := CreateOleObject('DriverHelper.Chooser');{This will work for old Ascom and 32 bit windows only. Note this give an error when run in the Delphi environment}
      end;
      V.devicetype:=widestring('Camera');
      ascom_driver:=(V.Choose(ascom_driver));
      V:=Unassigned;
    end;{end send from popup menu}
    exit;
  end;
  except {ascom alive}
    Showmessage('Error, No ASCOM detected. Install from http://ascom-standards.org');
  end;
  {$else}
    {Not available in Linux}
  {$endif}
end;


procedure TForm1.elevation_error1Change(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.equinox_communication1Change(Sender: TObject);
begin
   update_required:=true;
end;


procedure TForm1.fliptext1Change(Sender: TObject);
begin
  update_required:=true;
end;

procedure TForm1.focallength1Change(Sender: TObject);
begin
  // https://forum.lazarus.freepascal.org/index.php?topic=61256.0
  If ((Sender is TEdit) and (TEdit(Sender).Modified=true)) then
    Exit;
  update_fov('fl');
  TEdit(Sender).Modified := False;
end;


procedure convert_string_to_ra_dec(gotopos:string;out thera,thedec: double);

var
  ra_1,ra_2,ra_3,dec_1,dec_2,dec_3  : string;
  i,pos1,pos2,pos3,pos4,pos5,pos6    : integer;
begin
   repeat  {remove all double spaces}
     i:=pos('  ',gotopos);
     if i>0 then delete(gotopos,i,1);
   until i=0;;

   while ((length(gotopos)>=1) and (gotopos[1]=' ')) do {remove spaces in the front for pos1 detectie}
                                      delete(gotopos,1,1);
   while ((length(gotopos)>=1) and (gotopos[length(gotopos)]=' ')) do {remove spaces in the end since VAL( can't cope with them}
                                      delete(gotopos,length(gotopos),1);
   pos1:=pos(' ',gotopos);  if pos1=0 then exit;
   pos2:=posEX(' ',gotopos,pos1+1); if pos2=0 then pos2:=length(gotopos)+1;
   pos3:=posEX(' ',gotopos,pos2+1); if pos3=0 then pos3:=length(gotopos)+1;
   pos4:=posEX(' ',gotopos,pos3+1); if pos4=0 then pos4:=length(gotopos)+1;
   pos5:=posEX(' ',gotopos,pos4+1); if pos5=0 then pos5:=length(gotopos)+1;
   pos6:=posEX(' ',gotopos,pos5+1); if pos6=0 then pos6:=length(gotopos)+1;

   if pos5<>pos6  then {6 string position}
   begin
     ra_1:=copy(gotopos,1, pos1-1);
     ra_2:=copy(gotopos,pos1+1, pos2-pos1-1);
     ra_3:=copy(gotopos,pos2+1,pos3-pos2-1);
     dec_1:=copy(gotopos,pos3+1,pos4-pos3-1);
     dec_2:=copy(gotopos,pos4+1,pos5-pos4-1);
     dec_3:=copy(gotopos,pos5+1,99);
     theRa:=strtofloat(ra_1)+strtofloat(ra_2)/60+strtofloat(ra_3)/3600;
     theDec:=strtofloat(dec_1);
     theDec:=theDec+sign(theDec)*(abs(strtofloat(dec_2))/60+abs(strtofloat(dec_3)/3600));//apply sign of degrees
   end
   else
   if pos3<>pos4  then {4 string position}
   begin {4 string position}
     ra_1:=copy(gotopos,1, pos1-1);
     ra_2:=copy(gotopos,pos1+1, pos2-pos1-1);
     dec_1:=copy(gotopos,pos2+1,pos3-pos2-1);
     dec_2:=copy(gotopos,pos3+1,99);
     theRa:=strtofloat(ra_1)+strtofloat(ra_2)/60;
     theDec:=strtofloat(dec_1);
     theDec:=theDec+sign(theDec)*strtofloat(dec_2)/60;//apply sign of degrees

   end
   else
   begin {2 string position}
     ra_1:=copy(gotopos,1, pos1);
     dec_1:=copy(gotopos,pos1+1,99);
     theRa:=strtofloat(ra_1);
     theDec:=strtofloat(dec_1);
   end;
end;

procedure TForm1.j2000_to_Jnow1Click(Sender: TObject);
var
  thera, thedec : double;
begin
   if InputQuery('Convert J2000 to Jnow', 'Enter [hh  dd   or   hh mm  dd mm   or   hh mm ss  dd mm ss]?', gotopos)=false then exit;
   convert_string_to_ra_dec(gotopos,thera,thedec);

   precession5(2451545, jd ,thera*pi/12,theDec*pi/180,theRa,theDec);
   application.messagebox(pchar(prepare_ra(theRA)+',  '+prepare_dec(theDec)),pchar('JNOW'),MB_OK); {show error from driver error, for example below horizon}
end;

procedure TForm1.jnow_to_j2000_1Click(Sender: TObject);
var
  thera, thedec : double;
begin
   if InputQuery('Convert Jnow to J2000', 'Enter [hh  dd   or   hh mm  dd mm   or   hh mm ss  dd mm ss]?', gotopos)=false then exit;
   convert_string_to_ra_dec(gotopos,thera,thedec);

   precession5(jd, 2451545,thera*pi/12,theDec*pi/180,theRa,theDec);
   application.messagebox(pchar(prepare_ra(theRA)+',  '+prepare_dec(theDec)),pchar('J2000'),MB_OK); {show error from driver error, for example below horizon}
end;

procedure TForm1.parked1Click(Sender: TObject);
var
  mount : T_Alpaca_Mount;
begin
  if form1.mount_alpaca1.checked then
  begin
    mount := T_Alpaca_Mount.Create(nil);
    try
      if is_parked=2 then
      mount.unpark
      else
      Mount.park;

    finally
      mount.Free;
    end;
  end
  else
  begin
    if ascom_mount_connected then
    try
      if ascom_mount.atpark
        then ascom_mount.unPark
      else
      ascom_mount.Park;
    except
       Showmessage('Error SetPark!');
    end;
  end;
end;

procedure TForm1.tracking1Click(Sender: TObject);
var
  mount : T_Alpaca_Mount;
begin
   if form1.mount_alpaca1.checked then
   begin
     if is_parked<>0 then
     begin
       Showmessage('Can not set tracking. Alpaca Mount is parked !');
       exit;
     end;
     mount := T_Alpaca_Mount.Create(nil);
     try
       Mount.settracking(true);
     finally
       mount.Free;
     end;
   end
   else
  //   slew_ascom_telescope(fnmodulo(Ra,24), Dec);

  begin
    if ascom_mount_connected then
    try {parked ?}
      if ascom_mount.AtPark then
      begin
         Showmessage('Error! Mount parked !');
         exit;
      end;
      except
        on E: Exception do
        begin
          application.messagebox(pchar(E.Message),'ASCOM returns following error:',MB_ICONWARNING+MB_OK); {show error from driver error, for example below horizon}
        end;
    end;{park test}
    try
      ascom_mount.Tracking:=ascom_mount.Tracking=false;
      except
        Showmessage('Error tracking!');
      end;
  end;

end;


procedure enable_controls(polerr,equatmount :boolean);{for polar alignment error}
begin
  with form1 do
  begin
    Label_elevation1.enabled:=polerr;
    elevation_error1.enabled:=polerr;
    Label_azimuth1.enabled:=polerr;
    azimuth_error1.enabled:=polerr;
    slew_to_test_position1.enabled:=polerr;
    polar_error_describtion1.Visible:=polerr;
    Label_latitude1.enabled:=((polerr) or (equatmount));
    latitude1.enabled:=((polerr) or (equatmount));
    Label_longitude1.enabled:=((polerr) or (equatmount));
    longitude1.enabled:=((polerr) or (equatmount));
  end;
  update_required:=true;
end;


procedure TForm1.polar_alignment_mode1Change(Sender: TObject);
begin
  enable_controls(polar_alignment_error, mount_type1.itemindex=1);{polar alignment error or equatorial mount}
end;

procedure TForm1.popupmenu_map1Popup(Sender: TObject);
begin
 if form1.mount_alpaca1.checked then
 begin
   tracking1.checked:=alpaca_tracking;
   parked1.checked:=(is_parked=2);
 end
 else
 begin
    if ascom_mount_connected then
    begin
      try
      tracking1.checked:=ascom_mount.Tracking;
      parked1.checked:=ascom_mount.atpark;
      except
      end;
    end
    else
    begin
      tracking1.checked:=false;
      parked1.checked:=false;
    end;
  end;
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
    tab_rotator1.imageindex:=3;//gray
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
      tab_rotator1.imageindex:=1;//green
      ascom_rotator_connected:=true;
      memo2_message('Rotator connected.');

    except
      on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
      else
      begin {other errors}
        connect_rotator1.font.color:=cldefault;
        connect_rotator1.caption:='Error rotator';
        connect_rotator1.font.style:=[];
        tab_rotator1.imageindex:=0;//filter_2
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


procedure TForm1.FormShow(Sender: TObject);
begin
  user_path:=GetAppConfigDir(false);{get user path for app config}
  if load_settings(user_path+'sky_simulator.cfg')=false then
    if DirectoryExists(user_path)=false then createdir(user_path);{create c:\users\yourname\appdata\local\sky_simulator}

  update_fov('');//update scale after loading settings

  focuser_position:=strtoint(form1.focus_at1.text);
  real_focuser_position:=focuser_position;
  oldreal_position:=focuser_position;

  enable_controls(form1.polar_alignment_error1.checked, mount_type1.itemindex=1);{polar alignment error or equatorial mount}

  {$ifdef mswindows}
  {$else}
    {Ascom not available in Linux, Mac}
    focuser_ascom1.visible:=false;
    mount_ascom1.visible:=false;
    rotator_ascom1.visible:=false;
    form1.caption:='Sky simulator for Alapaca. Camera simulator with "Deep Sky Survey" images or artificial images based on mount and focuser position.';
    GroupBox_camera_chooser1.visible:=false;
  {$endif}

  groupbox_polar_alignment_error1.enabled:=artificial_selected1.checked;
  groupbox_mount_errors1.enabled:=artificial_selected1.checked;

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


procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
var
   magnification       : double;
   fitsX,fitsY, ra,dec :double;

begin
  magnification:=image1.Height/image1.canvas.height;

  fitsX:=0.5+x/magnification;
  fitsY:=0.5+(image1.Height-y)/magnification;


//  if flip_horizontal then fh:=-1 else fh:=+1;//different then for TIFF
//  if flip_vertical then fv:=+1 else fv:=-1;

//  cd1_1:=fh*cd1_1;
//  setvalue(21,fv*cd1_2);
//  setvalue(22,fh*cd2_1);
//  setvalue(23,fv*cd2_2);

  sensor_to_celestial(fitsx,fitsy, ra,dec);
  panel_sky1RA:=ra*12/pi;//in hours for popupmenu
  panel_sky1DEC:=dec*180/pi;//degrees

  form1.caption:='FITS coordinates: '+inttostr(round(fitsX))+',  '+inttostr(round(fitsY))+'        '+ prepare_ra(ra)+'   '+ prepare_dec(dec);
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
  p:=tab_Image1.Screentoclient(p);
  zoom(1/1.1,p);
end;

procedure TForm1.Image1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  P: TPoint;
begin
  GetCursorPos(p);  {use this since in Lazarus the mousepos varies depending control under the mouse}
  p:=tab_Image1.Screentoclient(p);
  zoom(1.1,p);

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
  plot_sky_map;//update horizon
end;


procedure TForm1.longitude1Exit(Sender: TObject);
begin
  update_required:=true;
  plot_sky_map;//update horizon
end;


procedure TForm1.focal_ratio1Change(Sender: TObject);
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


procedure TForm1.abort_slew1Click(Sender: TObject);
begin
  telescope_abortslew;
end;


procedure TForm1.menu_slew_to1Click(Sender: TObject);
begin
   mount_goto(panel_sky1RA,panel_sky1DEC);
end;


procedure TForm1.slew_to_mouse_pos_image1Click(Sender: TObject);
var
  thera, thedec : double;
begin
  if equinox_communication<>2 then //<> J2000
  begin
    precession5(2451545, jd ,panel_sky1RA*pi/12,panel_sky1DEC*pi/180,thera,thedec); //J2000 to Jnow
    thera:=thera*12/pi;//convert to hours
    theDec:=theDec*180/pi;//convert to degrees
  end;

  mount_goto(thera,thedec);
end;


procedure TForm1.panel_sky1MouseLeave(Sender: TObject);
begin
  mouse_position1.caption:='';//blank
end;

procedure TForm1.panel_sky1MouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
var
  ww,hh : integer;
begin
  hh:=form1.panel_sky1.height-5;
  ww:=form1.panel_sky1.width-5;

  panel_sky1RA:=(360/ww)*x/15;//in hours for popupmenu.
  panel_sky1DEC:=(90-y*(180/hh));//degrees
  mouse_position1.caption:= prepare_ra(panel_sky1RA*pi/12)+',  '+prepare_dec(panel_sky1DEC*pi/180);
end;


procedure TForm1.panel_sky1Paint(Sender: TObject);
begin
  plot_sky_map;
end;


procedure TForm1.save_as_fits1Click(Sender: TObject);
begin
 savedialog1.defaultExt:='.fits';
 savedialog1.filter:='.fits';
 savedialog1.filename:='sky_image.fits';
 if savedialog1.execute then
  begin
    if fileexists(savedialog1.filename) then
       if MessageDlg('Existing file ' +savedialog1.filename+ ' Overwrite?', mtConfirmation, [mbYes, mbNo], 0)<>6 then
         Exit;
    save_fits(savedialog1.filename);
  end;

end;


procedure TForm1.save_as_tiff1Click(Sender: TObject);
begin
  savedialog1.defaultExt:='.tiff';
  savedialog1.filter:='.tiff';
  savedialog1.filename:='sky_image.tiff';
  if savedialog1.execute then
   begin
     if fileexists(savedialog1.filename) then
        if MessageDlg('Existing file ' +savedialog1.filename+ ' Overwrite?', mtConfirmation, [mbYes, mbNo], 0)<>6 then
          Exit;
     save_tiff16(savedialog1.filename);
   end;
end;


procedure TForm1.save_imaga_as1Click(Sender: TObject);
var
  PNG: TPortableNetworkGraphic;{FPC}
begin
  savedialog1.filename:='sky_image.png';
  savedialog1.defaultExt:='.png';
  savedialog1.filter:='.png';
  if savedialog1.execute then
   begin
     if fileexists(savedialog1.filename) then
        if MessageDlg('Existing file ' +savedialog1.filename+ ' Overwrite?', mtConfirmation, [mbYes, mbNo], 0)<>6 then
          Exit;
     if ((pos('.PNG',uppercase(savedialog1.filename))>0) or (savedialog1.filterindex=1) )  then
     begin
        png:= TPortableNetworkGraphic.Create;   {FPC}
       try
         PNG.Assign(image1.Picture.Graphic);    //Convert data into png
         savedialog1.filename:=ChangeFileExt(savedialog1.filename,'.png');
         PNG.SaveToFile(savedialog1.filename);
       finally
        PNG.Free;
       end;
     end
  end;
end;


procedure TForm1.image_to_clipboard1Click(Sender: TObject);
begin
  Clipboard.Assign(Image1.Picture.Bitmap)
end;


procedure TForm1.mount_goto1Click(Sender: TObject);
var
  thera, thedec : double;
  jnow: boolean;
begin
  jnow:=((sender=form1.slew_to_jnow1) or (sender=form1.slew_to_jnow2));
  if jnow=false then
  begin
    if InputQuery('Slew to [J2000]?', 'Slew to [hh  dd   or   hh mm  dd mm   or   hh mm ss  dd mm ss]?', gotopos)=false then exit;
  end
  else
  begin
    if InputQuery('Slew to [JNow]?', 'Slew to [hh  dd   or   hh mm  dd mm   or   hh mm ss  dd mm ss]?', gotopos)=false then exit;
  end;

  if gotopos='' then exit;
  convert_string_to_ra_dec(gotopos,thera,thedec);

  if  ((jnow=false) and  (equinox_communication<>2)) then //<> J2000
  begin
    precession5(2451545, jd ,thera*pi/12,theDec*pi/180,theRa,theDec);
    thera:=thera*12/pi;//convert to hours
    theDec:=theDec*180/pi;//convert to degrees
  end
  else
  if  ((jnow) and  (equinox_communication=2)) then //J2000
  begin
    precession5(jd, 2451545 ,thera*pi/12,theDec*pi/180,theRa,theDec);
    thera:=thera*12/pi;//convert to hours
    theDec:=theDec*180/pi;//convert to degrees
  end;

  mount_goto(thera,thedec);
end;


procedure TForm1.slew_to_test_position1Click(Sender: TObject);
begin
  mount_goto(sidereal_time*12/pi+6 {hours},89); //mount down, 89 degrees from north pole
end;


procedure TForm1.mount_alpaca1Change(Sender: TObject);
var
  alpacaMode :boolean;
begin
  alpacaMode:=mount_alpaca1.checked;

  focallength1.enabled:=alpacaMode;//else take focal length form mount driver

  if alpacaMode=false then
  begin
    if ascom_mount_connected=false then form1.connect_mount1Click(nil);
  end;

  connect_mount1.visible:=alpacaMode=false;
  mount_type1.visible:=alpacaMode;
  DecPulseReverses1.visible:=alpacaMode;
  NSswapped1.visible:=alpacaMode;
  equinox_communication1.enabled:=alpacaMode;
  focallength1.enabled:=alpacaMode;
  update_required:=true;
end;


procedure TForm1.mount_type1Change(Sender: TObject);
begin
  if mount_type1.itemindex=0 then
  sideofpier_alpaca:=-1;
  pointing1.caption:='-';
  enable_controls(polar_alignment_error, mount_type1.itemindex=1);{polar alingment error or equatorial mount}
  update_required:=true;
end;



procedure TForm1.plotted_info1Change(Sender: TObject);
begin
  update_required:=true;
end;


procedure TForm1.polar_alignment_error1Change(Sender: TObject);
begin
  polar_alignment_error:=polar_alignment_error1.checked;
  enable_controls(form1.polar_alignment_error1.checked, mount_type1.itemindex=1);{polar alingment error or equatorial mount}
  update_required:=true;
end;


procedure TForm1.PopupMenu_image1Popup(Sender: TObject);
begin
  mount_goto1.enabled:=mount_alpaca1.checked;//only in alpca mode
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
      tab_focuser1.imageindex:=0;//red
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
        tab_focuser1.imageindex:=1;//green
        ascom_focuser_connected:=true;
        memo2_message('Focuser connected.');

      except
        on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
        else
        begin {other errors}
          connect_focuser1.font.color:=cldefault;
          connect_focuser1.caption:='Error focuser';
          connect_focuser1.font.style:=[];
          tab_focuser1.imageindex:=0;//red
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
    ascom_mount := Unassigned;
    connect_mount1.font.color:=cldefault;
    connect_mount1.caption:='Connect mount';
    connect_mount1.font.style:=[];
    tab_mount1.imageindex:=0;//red
    {canslew and canslewasync will be asked later. Here too early}
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
      wait(1000);
      connect_mount1.caption:=ascom_mount_driver;
      connect_mount1.font.style:=[fsbold,fsunderline];
//      tab_mount1.imageindex:=1;//green


      ascom_mount_connected:=true;
      memo2_message('Mount connected.');
      if ascom_mount.CanSlewAsync then Ascom_mount_capability:=2 else {async slew possible, live feed back Ra/DEC}
      if ascom_mount.CanSlew      then Ascom_mount_capability:=1 else {slew possible but not feedback of position at the same time}
         Ascom_mount_capability:=0;{no slew possible}

    except
      on E: EOleException do ShowMessage('ASCOM error : ' + E.Message) {e.g. wrong com port}
      else
      begin {other errors}
        connect_mount1.font.color:=cldefault;
        connect_mount1.caption:='Connect mount';
        connect_mount1.font.style:=[];
        tab_mount1.imageindex:=0;//red
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
  form1.width_pixels1.enabled:=((eso_selected1.checked=false) and (file_from_disk_selected1.checked=false));
  form1.height_pixels1.enabled:=((eso_selected1.checked=false) and(file_from_disk_selected1.checked=false)) ;
  form1.height_arcmin1.enabled:=file_from_disk_selected1.checked=false;

  deletefile(pchar(documents_path+'image.tmp'));{could be gif or jpeg. Delete to prevent errors} //fix for linux

  groupbox_polar_alignment_error1.enabled:=artificial_selected1.checked;
  groupbox_mount_errors1.enabled:=artificial_selected1.checked;
  update_required:=true;
end;


procedure TForm1.manipulations1Change(Sender: TObject);
begin
 update_required:=true;
 if manipulations1.itemindex>4 then
   bayer_change_warning1.Font.color:=clred
 else
   bayer_change_warning1.Font.color:=cldefault;
end;


procedure TForm1.tab_mount1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TForm1.telescope_cursor1MouseEnter(Sender: TObject);
begin
  mouse_position1.caption:=telescope_cursor1.hint;
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
   form1.caption:='Simulation stopped.';
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
