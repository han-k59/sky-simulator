unit sky_annotation; {deep sky and star annotation & photometry calibation of the image}
{$mode delphi}
{Copyright (C) 2018, 2019, 2021  by Han Kleijn, www.hnsky.org
 email: han.k.. at...hnsky.org

{This program is free software: you can redistribute it and/or modify
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


interface

uses
   forms,Classes, SysUtils,strutils, math,graphics, Controls {for tcursor},lcltype {Trgbtriple}, LazSysUtils; {nowUtc}

type
  image_array = array of array of single;

procedure plot_deepsky;{plot the deep sky object on the image}
procedure load_deep;{load the deepsky database once. If loaded no action}
procedure plot_stars(realposition, perfectposition,a,b : double);{plot stars on the image}
procedure image_array_stretched_to_screen;
procedure read_deepsky(searchmode:char; telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov : double; out ra2,dec2,length2,width2,pa : double);{deepsky database search}
procedure prepare_plotting(ra1,dec1,rota :double; fh,fv :boolean); {prepare image1 and set the parameters normally contained in fits header required for plotting}
function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
procedure annotation_to_array(thestring : ansistring;transparant:boolean;graylevel,size, x,y {screen coord}: integer; var img: image_array);{string to image array as annotation, result is flicker free since the annotion is plotted as the rest of the image}
procedure sensor_to_celestial(fitsx,fitsy : double; out ra,dec :double); //(x,y) -> (RA,DEC)
procedure az_ra2(az,alt,lat,long,t:double;out ra,de: double);{conversion az,alt to ra,dec, longitude is POSITIVE when west. At south azimuth is 180}
procedure ra_az2(ra,dec,lat,long,t:double;out azimuth2,altitude2: double);{conversion ra & dec to altitude, azimuth, longitude is POSITIVE when west. At south azimuth is 180 }
function  utc_date_time: string;// Current date&time in the FITS-standard / ISO-8601 CCYY-MM-DDThh:mm:ss[.sss…] format.


var
  img_array        : image_array;
  img_bitmap       : tbitmap; {contain simulation in greylevel[0..65535]:=red*255 +blue}

  deepstring       : Tstrings;
  linepos          : integer;
  width2,height2   : integer;
  labels           : integer;
  width_arcmin, height_arcmin   : double;
  naam2,naam3,naam4: string;
  latitude,longitude,elevation_error,azimuth_error : double;

  cd1_1,cd1_2,cd2_1, cd2_2,crpix1,crpix2,cdelt1,cdelt2,dec0,ra0,crota2,raJnow0,decJnow0  : double;
  flip_horizontal, flip_vertical : boolean;



const
  flux_magn_offset       : double=0;{offset between star magnitude and flux. Will be calculated in stars are annotated}
  counter_flux_measured  : integer=0;{how many stars used for flux calibration}
  polar_alignment_error  : boolean=false;


implementation

uses sky_simulator_main, sky_simulator_unit_290, alpaca_camera_protocol;

const font_5x9 : packed array[33..126,0..8,0..4] of byte=  {ASTAP native font for part of code page 437}
  ((
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,0,0),
  (0,0,1,0,0)),{!}
  (
  (0,1,0,1,0),
  (0,1,0,1,0),
  (0,1,0,1,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{"}
  (
  (0,1,0,1,0),
  (0,1,0,1,0),
  (0,1,0,1,0),
  (1,1,1,1,1),
  (0,1,0,1,0),
  (1,1,1,1,1),
  (0,1,0,1,0),
  (0,1,0,1,0),
  (0,1,0,1,0)),{#}
  (
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,1,1,1),
  (1,0,1,0,0),
  (0,1,1,1,0),
  (0,0,1,0,1),
  (1,1,1,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0)),{dollar sign}
  (
  (1,1,1,0,0),
  (1,0,1,0,0),
  (1,1,1,0,1),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (1,0,1,1,1),
  (0,0,1,0,1),
  (0,0,1,1,1)),{%}
  (
  (0,0,0,0,0),
  (0,1,1,0,0),
  (1,0,0,1,0),
  (1,0,0,1,0),
  (0,1,1,0,0),
  (1,0,1,0,0),
  (1,0,0,1,0),
  (1,0,0,1,1),
  (0,1,1,0,0)),{&}
  (
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{'}
  (
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0)),{(}
  (
  (0,1,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,0,0,0)),{)}
  (
  (0,0,0,0,0),
  (0,0,1,0,0),
  (1,0,1,0,1),
  (1,1,1,1,1),
  (0,1,1,1,0),
  (1,1,1,1,1),
  (1,0,1,0,1),
  (0,0,1,0,0),
  (0,0,0,0,0)),{*}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (1,1,1,1,1),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{+}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,0,0,0)),{,}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (1,1,1,1,1),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{-}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,1,1,0,0),
  (0,1,1,0,0)),{.}
  (
  (0,0,0,0,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (1,0,0,0,0),
  (1,0,0,0,0)),{/}
  (
  (0,1,1,1,0),{0}
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,0)),
  (
  (0,0,1,0,0),{1}
  (0,1,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,1,1,1,0)),
  (
  (0,1,1,1,0),{2}
  (1,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (1,0,0,0,0),
  (1,1,1,1,1)),
  (
  (1,1,1,1,0),{3}
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,1,1,1,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (1,1,1,1,0)),
  (
  (1,0,0,0,1),{4}
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,1,1,1,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,0,1)),
  (
  (1,1,1,1,1),{5}
  (1,0,0,0,0),
  (1,0,0,0,0),
  (1,0,0,0,0),
  (1,1,1,1,0),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,0)),
  (
  (0,1,1,1,0),{6}
  (1,0,0,0,0),
  (1,0,0,0,0),
  (1,0,0,0,0),
  (0,1,1,1,0),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,0)),
  (
  (1,1,1,1,1),{7}
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0)),
  (
  (0,1,1,1,0),{8}
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,0),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,0)),
  (
  (0,1,1,1,0),{9}
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (0,1,1,1,1),
  (0,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,1,1,0,0)),
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,1,1,0),
  (0,0,1,1,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,1,1,0),
  (0,0,1,1,0)),{:}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,1,1,0),
  (0,0,1,1,0),
  (0,0,0,0,0),
  (0,0,1,1,0),
  (0,0,1,1,0),
  (0,0,0,1,0),
  (0,1,1,1,0)),{;}
  (
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (1,0,0,0,0),
  (0,1,0,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,0,1)),{<}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (1,1,1,1,1),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (1,1,1,1,1),
  (0,0,0,0,0),
  (0,0,0,0,0)),{=}
  (
  (1,0,0,0,0),
  (0,1,0,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (1,0,0,0,0)),{>}

  (
  (1,1,1,1,0),
  (1,0,0,0,1),
  (0,0,0,0,1),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0)),{?}
  (
  (0,1,1,1,0),
  (1,0,0,0,1),
  (1,0,0,0,1),
  (1,0,1,1,1),
  (1,0,1,0,1),
  (1,0,1,1,1),
  (1,0,0,0,0),
  (1,0,0,0,1),
  (0,1,1,1,0)),{@}
  (
  (0,0,1,0,0),{A}
  (0,1,0,1,0),{A}
  (0,1,0,1,0),{A}
  (1,0,0,0,1),{A}
  (1,0,0,0,1),{A}
  (1,1,1,1,1),{A}
  (1,0,0,0,1),{A}
  (1,0,0,0,1),{A}
  (1,0,0,0,1)),{A}
  (
  (1,1,1,1,0),{B}
  (0,1,0,0,1),{B}
  (0,1,0,0,1),{B}
  (0,1,0,0,1),{B}
  (0,1,1,1,0),{B}
  (0,1,0,0,1),{B}
  (0,1,0,0,1),{B}
  (0,1,0,0,1),{B}
  (1,1,1,1,0)),{B}
  (
  (0,1,1,1,0),{C}
  (1,0,0,0,1),{C}
  (1,0,0,0,0),{C}
  (1,0,0,0,0),{C}
  (1,0,0,0,0),{C}
  (1,0,0,0,0),{C}
  (1,0,0,0,0),{C}
  (1,0,0,0,1),{C}
  (0,1,1,1,0)),{C}
  (
  (1,1,1,1,0),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (0,1,0,0,1),{D}
  (1,1,1,1,0)),{D}
  (
  (1,1,1,1,1),{E}
  (1,0,0,0,0),{E}
  (1,0,0,0,0),{E}
  (1,0,0,0,0),{E}
  (1,1,1,1,0),{E}
  (1,0,0,0,0),{E}
  (1,0,0,0,0),{E}
  (1,0,0,0,0),{E}
  (1,1,1,1,1)),{E}
  (
  (1,1,1,1,1),{F}
  (1,0,0,0,0),{F}
  (1,0,0,0,0),{F}
  (1,0,0,0,0),{F}
  (1,1,1,1,0),{F}
  (1,0,0,0,0),{F}
  (1,0,0,0,0),{F}
  (1,0,0,0,0),{F}
  (1,0,0,0,0)),{F}
  (
  (0,1,1,1,0),{G}
  (1,0,0,0,1),{G}
  (1,0,0,0,0),{G}
  (1,0,0,0,0),{G}
  (1,0,0,1,1),{G}
  (1,0,0,0,1),{G}
  (1,0,0,0,1),{G}
  (1,0,0,1,1),{G}
  (0,1,1,0,1)),{G}
  (
  (1,0,0,0,1),{H}
  (1,0,0,0,1),{H}
  (1,0,0,0,1),{H}
  (1,0,0,0,1),{H}
  (1,1,1,1,1),{H}
  (1,0,0,0,1),{H}
  (1,0,0,0,1),{H}
  (1,0,0,0,1),{H}
  (1,0,0,0,1)),{H}
  (
  (1,1,1,1,1),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (0,0,1,0,0),{I}
  (1,1,1,1,1)),{I}
  (
  (0,0,0,1,1),{J}
  (0,0,0,0,1),{J}
  (0,0,0,0,1),{J}
  (0,0,0,0,1),{J}
  (0,0,0,0,1),{J}
  (0,0,0,0,1),{J}
  (0,0,0,0,1),{J}
  (1,0,0,0,1),{J}
  (0,1,1,1,0)),{J}
  (
  (1,0,0,0,1),{K}
  (1,0,0,0,1),{K}
  (1,0,0,1,0),{K}
  (1,0,1,0,0),{K}
  (1,1,0,0,0),{K}
  (1,0,1,0,0),{K}
  (1,0,0,1,0),{K}
  (1,0,0,0,1),{K}
  (1,0,0,0,1)),{K}
  (
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,0,0,0,0),{L}
  (1,1,1,1,1)),{L}
  (
  (1,0,0,0,1),{M}
  (1,1,0,1,1),{M}
  (1,1,0,1,1),{M}
  (1,0,1,0,1),{M}
  (1,0,1,0,1),{M}
  (1,0,0,0,1),{M}
  (1,0,0,0,1),{M}
  (1,0,0,0,1),{M}
  (1,0,0,0,1)),{M}
  (
  (1,0,0,0,1),{N}
  (1,1,0,0,1),{N}
  (1,1,0,0,1),{N}
  (1,0,1,0,1),{N}
  (1,0,1,0,1),{N}
  (1,0,0,1,1),{N}
  (1,0,0,1,1),{N}
  (1,0,0,0,1),{N}
  (1,0,0,0,1)),{N}
  (
  (0,1,1,1,0),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (1,0,0,0,1),{O}
  (0,1,1,1,0)),{O}
  (
  (1,1,1,1,0),{P}
  (1,0,0,0,1),{P}
  (1,0,0,0,1),{P}
  (1,0,0,0,1),{P}
  (1,1,1,1,0),{P}
  (1,0,0,0,0),{P}
  (1,0,0,0,0),{P}
  (1,0,0,0,0),{P}
  (1,0,0,0,0)),{P}
  (
  (0,1,1,1,0),{Q}
  (1,0,0,0,1),{Q}
  (1,0,0,0,1),{Q}
  (1,0,0,0,1),{Q}
  (1,0,0,0,1),{Q}
  (1,0,1,0,1),{Q}
  (0,1,1,1,0),{Q}
  (0,0,0,1,0),{Q}
  (0,0,0,0,1)),{Q}
  (
  (1,1,1,1,0),{R}
  (1,0,0,0,1),{R}
  (1,0,0,0,1),{R}
  (1,0,0,0,1),{R}
  (1,1,1,1,0),{R}
  (1,1,0,0,0),{R}
  (1,0,1,0,0),{R}
  (1,0,0,1,0),{R}
  (1,0,0,0,1)),{R}
  (
  (0,1,1,1,0),{S}
  (1,0,0,0,1),{S}
  (1,0,0,0,0),{S}
  (0,1,0,0,0),{S}
  (0,0,1,0,0),{S}
  (0,0,0,1,0),{S}
  (0,0,0,0,1),{S}
  (1,0,0,0,1),{S}
  (0,1,1,1,0)),{S}
  (
  (1,1,1,1,1),{T}
  (1,0,1,0,1),{T}
  (0,0,1,0,0),{T}
  (0,0,1,0,0),{T}
  (0,0,1,0,0),{T}
  (0,0,1,0,0),{T}
  (0,0,1,0,0),{T}
  (0,0,1,0,0),{T}
  (0,1,1,1,0)),{T}
  (
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (1,0,0,0,1),{U}
  (0,1,1,1,0)),{U}
  (
  (1,0,0,0,1),{V}
  (1,0,0,0,1),{V}
  (1,0,0,0,1),{V}
  (1,0,0,0,1),{V}
  (1,0,0,0,1),{V}
  (1,0,0,0,1),{V}
  (0,1,0,1,0),{V}
  (0,1,0,1,0),{V}
  (0,0,1,0,0)),{V}
  (
  (1,0,0,0,1),{W}
  (1,0,0,0,1),{W}
  (1,0,0,0,1),{W}
  (1,0,0,0,1),{W}
  (1,0,1,0,1),{W}
  (1,0,1,0,1),{W}
  (1,1,0,1,1),{W}
  (1,1,0,1,1),{W}
  (1,0,0,0,1)),{W}
  (
  (1,0,0,0,1),{X}
  (1,0,0,0,1),{X}
  (0,1,0,1,0),{X}
  (0,1,0,1,0),{X}
  (0,0,1,0,0),{X}
  (0,1,0,1,0),{X}
  (0,1,0,1,0),{X}
  (1,0,0,0,1),{X}
  (1,0,0,0,1)),{X}
  (
  (1,0,0,0,1),{Y}
  (1,0,0,0,1),{Y}
  (1,0,0,0,1),{Y}
  (0,1,0,1,0),{Y}
  (0,0,1,0,0),{Y}
  (0,0,1,0,0),{Y}
  (0,0,1,0,0),{Y}
  (0,0,1,0,0),{Y}
  (0,0,1,0,0)),{Y}
  (
  (1,1,1,1,1),{Z}
  (0,0,0,0,1),{Z}
  (0,0,0,0,1),{Z}
  (0,0,0,1,0),{Z}
  (0,0,1,0,0),{Z}
  (0,1,0,0,0),{Z}
  (1,0,0,0,0),{Z}
  (1,0,0,0,0),{Z}
  (1,1,1,1,1)),{Z}
  (
  (0,1,1,1,1),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,1,1,1,1)),{[}
  (
  (0,0,0,0,0),
  (1,0,0,0,0),
  (1,0,0,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,1,0)),{\}
  (
  (1,1,1,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (1,1,1,1,0)),{]}
  (
  (0,0,1,0,0),
  (0,1,0,1,0),
  (1,0,0,0,1),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{^}
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (1,1,1,1,1)),{_}
  (
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)),{`}
  (
  (0,0,0,0,0),{a}
  (0,0,0,0,0),{a}
  (0,0,0,0,0),{a}
  (0,1,1,1,0),{a}
  (0,0,0,0,1),{a}
  (0,1,1,1,1),{a}
  (1,0,0,0,1),{a}
  (1,0,0,1,1),{a}
  (0,1,1,0,1)),{a}
  (
  (0,0,0,0,0),{b}
  (1,0,0,0,0),{b}
  (1,0,0,0,0),{b}
  (1,0,0,0,0),{b}
  (1,0,1,1,0),{b}
  (1,1,0,0,1),{b}
  (1,0,0,0,1),{b}
  (1,0,0,0,1),{b}
  (1,1,1,1,0)),{b}
  (
  (0,0,0,0,0),{c}
  (0,0,0,0,0),{c}
  (0,0,0,0,0),{c}
  (0,1,1,1,0),{c}
  (1,0,0,0,0),{c}
  (1,0,0,0,0),{c}
  (1,0,0,0,0),{c}
  (1,0,0,0,1),{c}
  (0,1,1,1,0)),{c}
  (
  (0,0,0,0,0),{d}
  (0,0,0,0,1),{d}
  (0,0,0,0,1),{d}
  (0,0,0,0,1),{d}
  (1,1,1,1,1),{d}
  (1,0,0,0,1),{d}
  (1,0,0,0,1),{d}
  (1,0,0,1,1),{d}
  (0,1,1,0,1)),{d}
  (
  (0,0,0,0,0),{e}
  (0,0,0,0,0),{e}
  (0,0,0,0,0),{e}
  (0,1,1,1,0),{e}
  (1,0,0,0,1),{e}
  (1,0,0,0,1),{e}
  (1,1,1,1,0),{e}
  (1,0,0,0,0),{e}
  (0,1,1,1,1)),{e}
  (
  (0,0,1,1,0),{f}
  (0,1,0,0,1),{f}
  (0,1,0,0,0),{f}
  (0,1,0,0,0),{f}
  (1,1,1,1,0),{f}
  (0,1,0,0,0),{f}
  (0,1,0,0,0),{f}
  (0,1,0,0,0),{f}
  (0,1,0,0,0)),{f}
  (
  (0,0,0,0,0),{g}
  (0,0,0,0,0),{g}
  (0,0,0,0,0),{g}
  (0,1,1,1,1),{g}
  (1,0,0,0,1),{g}
  (1,0,0,1,1),{g}
  (0,1,1,0,1),{g}
  (0,0,0,0,1),{g}
  (1,1,1,1,0)),{g}
  (
  (0,0,0,0,0),{h}
  (1,0,0,0,0),{h}
  (1,0,0,0,0),{h}
  (1,0,0,0,0),{h}
  (1,0,1,1,0),{h}
  (1,1,0,0,1),{h}
  (1,0,0,0,1),{h}
  (1,0,0,0,1),{h}
  (1,0,0,0,1)),{h}
  (
  (0,0,0,0,0),{i}
  (0,1,1,0,0),{i}
  (0,0,0,0,0),{i}
  (0,1,1,0,0),{i}
  (0,0,1,0,0),{i}
  (0,0,1,0,0),{i}
  (0,0,1,0,0),{i}
  (0,0,1,0,0),{i}
  (1,1,1,1,1)),{i}
  (
  (0,0,0,0,0),{j}
  (0,0,0,1,1),{j}
  (0,0,0,0,0),{j}
  (0,0,0,1,1),{j}
  (0,0,0,0,1),{j}
  (0,0,0,0,1),{j}
  (0,0,0,0,1),{j}
  (1,0,0,0,1),{j}
  (0,1,1,1,0)),{j}
  (
  (0,0,0,0,0),{k}
  (1,0,0,0,0),{k}
  (1,0,0,0,0),{k}
  (1,0,0,0,1),{k}
  (1,0,0,1,0),{k}
  (1,1,1,0,0),{k}
  (1,0,1,0,0),{k}
  (1,0,0,1,0),{k}
  (1,0,0,0,1)),{k}
  (
  (0,0,0,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,0,0),{l}
  (0,0,1,1,1)),{l}
  (
  (0,0,0,0,0),{m}
  (0,0,0,0,0),{m}
  (0,0,0,0,0),{m}
  (1,1,0,1,0),{m}
  (1,0,1,0,1),{m}
  (1,0,1,0,1),{m}
  (1,0,1,0,1),{m}
  (1,0,0,0,1),{m}
  (1,0,0,0,1)),{m}
  (
  (0,0,0,0,0),{n}
  (0,0,0,0,0),{n}
  (0,0,0,0,0),{n}
  (1,0,1,1,0),{n}
  (1,1,0,0,1),{n}
  (1,0,0,0,1),{n}
  (1,0,0,0,1),{n}
  (1,0,0,0,1),{n}
  (1,0,0,0,1)),{n}
  (
  (0,0,0,0,0),{o}
  (0,0,0,0,0),{o}
  (0,0,0,0,0),{o}
  (0,1,1,1,0),{o}
  (1,0,0,0,1),{o}
  (1,0,0,0,1),{o}
  (1,0,0,0,1),{o}
  (1,0,0,0,1),{o}
  (0,1,1,1,0)),{o}
  (
  (0,0,0,0,0),{p}
  (0,0,0,0,0),{p}
  (0,0,0,0,0),{p}
  (1,0,1,1,0),{p}
  (1,1,0,0,1),{p}
  (1,0,0,0,1),{p}
  (1,1,1,1,0),{p}
  (1,0,0,0,0),{p}
  (1,0,0,0,0)),{p}
  (
  (0,0,0,0,0),{q}
  (0,0,0,0,0),{q}
  (0,0,0,0,0),{q}
  (0,1,1,1,1),{q}
  (1,0,0,0,1),{q}
  (1,0,0,1,1),{q}
  (0,1,1,0,1),{q}
  (0,0,0,0,1),{q}
  (0,0,0,0,1)),{q}
  (
  (0,0,0,0,0),{r}
  (0,0,0,0,0),{r}
  (0,0,0,0,0),{r}
  (1,1,0,1,1),{r}
  (0,1,1,0,1),{r}
  (0,1,0,0,1),{r}
  (0,1,0,0,0),{r}
  (0,1,0,0,0),{r}
  (1,1,1,0,0)),{r}
  (
  (0,0,0,0,0),{s}
  (0,0,0,0,0),{s}
  (0,0,0,0,0),{s}
  (0,1,1,1,1),{s}
  (1,0,0,0,0),{s}
  (0,1,1,1,0),{s}
  (0,0,0,0,1),{s}
  (0,0,0,0,1),{s}
  (1,1,1,1,0)),{s}
  (
  (0,0,0,0,0),{t}
  (0,1,0,0,0),{t}
  (0,1,0,0,0),{t}
  (1,1,1,1,0),{t}
  (0,1,0,0,0),{t}
  (0,1,0,0,0),{t}
  (0,1,0,0,0),{t}
  (0,1,0,0,1),{t}
  (0,0,1,1,0)),{t}
  (
  (0,0,0,0,0),{u}
  (0,0,0,0,0),{u}
  (0,0,0,0,0),{u}
  (1,0,0,0,1),{u}
  (1,0,0,0,1),{u}
  (1,0,0,0,1),{u}
  (1,0,0,0,1),{u}
  (1,0,0,1,1),{u}
  (0,1,1,0,1)),{u}
  (
  (0,0,0,0,0),{v}
  (0,0,0,0,0),{v}
  (0,0,0,0,0),{v}
  (1,0,0,0,1),{v}
  (1,0,0,0,1),{v}
  (1,0,0,0,1),{v}
  (0,1,0,1,0),{v}
  (0,1,0,1,0),{v}
  (0,0,1,0,0)),{v}
  (
  (0,0,0,0,0),{w}
  (0,0,0,0,0),{w}
  (0,0,0,0,0),{w}
  (1,0,0,0,1),{w}
  (1,0,0,0,1),{w}
  (1,0,1,0,1),{w}
  (1,0,1,0,1),{w}
  (1,1,0,1,1),{w}
  (1,0,0,0,1)),{w}

  (
  (0,0,0,0,0),{x}
  (0,0,0,0,0),{x}
  (0,0,0,0,0),{x}
  (1,0,0,0,1),{x}
  (0,1,0,1,0),{x}
  (0,0,1,0,0),{x}
  (0,1,0,1,0),{x}
  (1,0,0,0,1),{x}
  (1,0,0,0,1)),{x}
  (
  (0,0,0,0,0),{y}
  (0,0,0,0,0),{y}
  (0,0,0,0,0),{y}
  (1,0,0,0,1),{y}
  (1,0,0,0,1),{y}
  (1,0,0,1,1),{y}
  (0,1,1,0,1),{y}
  (0,0,0,0,1),{y}
  (1,1,1,1,0)),{y}
  (
  (0,0,0,0,0),{z}
  (0,0,0,0,0),{z}
  (0,0,0,0,0),{z}
  (1,1,1,1,1),{z}
  (0,0,0,1,0),{z}
  (0,0,1,0,0),{z}
  (0,1,0,0,0),{z}
  (1,0,0,0,0),{z}
  (1,1,1,1,1)),{z}
  (
  (0,0,1,1,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,0,1,0,0),
  (1,1,1,0,0),
  (0,0,1,0,0),
  (0,1,0,0,0),
  (0,1,0,0,0),
  (0,0,1,1,0)),//{  Open curly bracket or open brace
  (
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0),
  (0,0,1,0,0)),{|}
  (
  (0,1,1,0,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,0,1,0,0),
  (0,0,1,1,1),
  (0,0,1,0,0),
  (0,0,0,1,0),
  (0,0,0,1,0),
  (0,1,1,0,0)),//}  Close curly bracket or close brace
  (
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,1,0,0,1),
  (1,0,1,0,1),
  (1,0,1,0,1),
  (1,0,0,1,0),
  (0,0,0,0,0),
  (0,0,0,0,0),
  (0,0,0,0,0)){~}
  );

procedure annotation_to_array(thestring : ansistring;transparant:boolean;graylevel,size, x,y {screen coord}: integer; var img: image_array);{string to image array as annotation, result is flicker free since the annotion is plotted as the rest of the image}
var                                                                                       {Screen coordinates are used to have the font with the correct orientation}
 w,h,i,j,k,value,flipH,flipV,len,x2,y2: integer;
 ch : pansichar;
begin
  if img=nil then exit;
  w:=Length(img[0]); {width}
  h:=Length(img); {height}

  flipH:=1;

  if form1.fliptext1.checked then flipV:=-1 else flipV:=1;

  len:=length(thestring);
  for k:=1 to len do
  begin
    ch:=Pansichar(copy(thestring,k,1));
    value:=ord(ch[0]);
    if ((value>=33) and (value<=126)) then
    for j:=(9*size)-1 downto 0 do
      for i:=0 to (5*size)-1 do
      begin
        x2:=x+(i+(k-1)*7*size)*flipH;
        y2:=y-(j*flipV);
        if ((x2>=0) and (x2<w) and (y2>=0) and (y2<h)) then {within image}
        if (((transparant=false)) or (font_5x9[value,j div size ,i div size]<>0)) then img[y2,x2]:=font_5x9[value,j div size,i div size]*graylevel;{write the font to the array}
      end;
  end;
end;


function  utc_date_time : string;// Current date&time in the FITS-standard / ISO-8601 CCYY-MM-DDThh:mm:ss[.sss…] format.
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
  result:=inttostr(YY)+'-'+fl(MM)+'-'+fl(DD)+'T'+fl(hour)+':'+fl(min)+':'+fl(ss);////2000-01-01T12:00:00.1234567  no 'Z' added
end;


procedure ra_az2(ra,dec,lat,long,t:double;out azimuth2,altitude2: double);{conversion ra & dec to altitude, azimuth, longitude is POSITIVE when west. At south azimuth is 180 }
{input ra [0..2*pi], dec [-pi/2..+pi/2],lat[-pi/2..pi/2],long[0..2*pi],time[0..2*pi]}
var t5 :double;
    sin_lat,cos_lat,sin_dec,cos_dec,sin_t5,cos_t5:double;
begin
  t5:=-ra+t-long;
  sincos(lat,sin_lat,cos_lat);
  sincos(dec,sin_dec,cos_dec);
  sincos(t5,sin_t5,cos_t5);
  try
  {***** altitude calculation from ra&dec, meeus new 12.5 *******}
  altitude2:=arcsin(sin_lat*sin_dec+cos_lat*cos_dec*cos_t5);

  {***** azimuth calculation from ra&dec, meeus new 12.6 ****** }
  azimuth2:=arctan2(sin_t5,cos_t5*sin_lat- tan(dec)*cos_lat);
  except
  {ignore floating point errors outside builder}
  end;
  azimuth2:=azimuth2+pi;
end;


procedure az_ra2(az,alt,lat,long,t:double;out ra,de: double);{conversion az,alt to ra,dec, longitude is POSITIVE when west. At south azimuth is 180}
{input az [0..2*pi], alt [-pi/2..+pi/2],lat[-pi/2..+pi/2],long[0..2*pi],time[0..2*pi]}
var
  sin_lat, cos_lat, sin_alt, cos_alt,sin_az,cos_az   : double;
begin
  sincos(lat,sin_lat,cos_lat);
  sincos(alt,sin_alt,cos_alt);
  sincos(az-pi,sin_az,cos_az); {south is 180 degrees, shift 180 degrees}

  de:=arcsin(sin_lat*sin_alt - cos_lat*cos_alt*cos_az) ;{new meeus, formule behind 12.6}
  ra:=arctan2(sin_az, cos_az*sin_lat+tan(alt)*cos_lat  );

  ra:=-ra+t-long;

  while ra<0 do ra:=ra+2*pi;
  while ra>=2*pi do ra:=ra-2*pi;
end;


function position_angle(ra1,dec1,ra0,dec0 : double): double;//Position angle between a line from ra0,dec0 to ra1,dec1 and a line from ra0, dec0 to the celestial north . Rigorous method
//See book Meeus, Astronomical Algorithms, formula 46.5 edition 1991 or 48.5 edition 1998, angle of moon limb or page 116 edition 1998.
//See also https://astronomy.stackexchange.com/questions/25306/measuring-misalignment-between-two-positions-on-sky
//   PA=arctan2(cos(δ0)sin(α1−α0), sin(δ1)cos(δ0)−sin(δ0)cos(δ1)cos(α1−α0))      In lazarus the function is arctan2(y/x)
//   is seen at point α0,δ0. This means you are calculating the angle at point α0,δ0 (the reference point) towards point α1,δ1 (the target point).
//   To clarify:
//     Point α0,δ0 (Reference Point): This is where the observation is made from, or the point of reference.
//     Point α1,δ1 (Target Point): This is the point towards which the position angle is being measured.
//     Position Angle (PA): This is the angle measured at the reference point α0,δ0, going from the direction of the North Celestial Pole towards the target point α1,δ1, measured eastward (or counter-clockwise).
//     So in your observational scenario, if you were at point α0,δ0 and wanted to determine the direction to point α1,δ1, the PA would tell you the angle to rotate from the north, moving eastward, to align with the target point.

var
  sinDeltaRa,cosDeltaRa,
  sinDec0,cosDec0,
  sinDec1,cosDec1 : double;
begin
  sincos(ra1-ra0,sinDeltaRa,cosDeltaRa);
  sincos(dec0,sinDec0,cosDec0);
  sincos(dec1,sinDec1,cosDec1);
  result:=arctan2(cosDec1*sinDeltaRa,sinDec1*cosDec0 - cosDec1*sinDec0*cosDeltaRa);
end;


procedure precession(jd, ra1,dec1 : double; out ra_jnow1,dec2 : double); {precession correction,  new Meeus chapter precession formula 20.1}
var
  t,dra,ddec,m,n,n2  : double;
begin
  t:=(jd-2451545)/36525; {time in julian centuries since j2000 }
  m:=3.07496+0.00186*t;{seconds}
  n:=1.33621-0.00057*t; {seconds}
  n2:=20.0431-0.0085*t;{arcsec}
  dra:=(m + n *sin(ra1)*tan(dec1))*pi/(3600*12);{yearly ra drift in radians}
  ddec:=n2*cos(ra1)*pi/(3600*180); {yearly dec drift in radians}
  ra_jnow1:=ra1+(dra*t*100);{multiply with number of years is t*100}
  dec2:=dec1+(ddec*t*100);
end;


procedure prepare_plotting(ra1,dec1,rota :double; fh,fv :boolean); {prepare image1 and set the parameters normally contained in fits header required for plotting}
var
   sign        : integer;
   raJnow1,decJnow1,jnow_angle,dRa,dDec  : double;
begin
  ra0:=ra1;//global variables
  dec0:=dec1;

  height2:=strtoint(form1.height_pixels1.text);{height}
  width2:=strtoint(form1.width_pixels1.text);{width}

  height_arcmin:=strtofloat2(form1.height_arcmin1.text);

  width_arcmin:=height_arcmin*width2/height2;

//  form1.width1.caption:=inttostr(round(width_arcmin));
  form1.image1.picture.Bitmap.Width:=width2;
  form1.image1.picture.Bitmap.Height:=height2;


  form1.image1.Canvas.Brush.Color :=rgbtocolor(1,1,1);//clblack; clear fill bitmap with background color. A value of 1 gives better result for jpeg artefacts. An  bias and noise will be added later}

  form1.image1.Canvas.fillrect(rect(0,0,width2,height2){.clientrect}); //wis canvas using current brush


  crota2:=rota;
  crpix1:=(width2+1)/2;//since FITS pixels start at 1. So the middle of a 2x2 pixels image is at 1.5,1.5
  crpix2:=(height2+1)/2;
  cdelt1:=-(width_arcmin/60)/width2;//X pixel size (deg).
  cdelt2:=cdelt1;// Y pixel size (deg)

  if polar_alignment_error=false then //no error
  begin
    //step one pixel in declination to calculate the angle in Jnow
    precession5(2451545, jd ,ra0,dec0-cdelt2*10*pi/180,raJnow1,decJnow1);//position in Jnow coordinate system
    precession5(2451545, jd ,ra0,dec0      ,raJnow0,decJnow0);//position in Jnow coordinate system

    jnow_angle:=position_angle(raJnow1,decJnow1,raJnow0,decJnow0);//Position angle in Jnow
    crota2:=crota2+jnow_angle; //correct the angle for Jnow pole
  end
  else
  begin //polar alignment error
     precession5(2451545, jd ,ra0,dec0,raJnow0,decJnow0);//position in Jnow coordinate system

     //add error
     //Polar error calculation based on two celestial reference points and the error of the telescope mount at these point(s).
     //  Based on formulas from Ralph Pass documented at https://rppass.com/align.pdf.
     //  They are based on the book “Telescope Control’ by Trueblood and Genet, p.111
     //  Ralph added sin(latitude) term in the equation for the error in RA.

     // For one reference image the difference in RA and DEC caused by the misalignment of the polar axis, formula (3):
     //   delta_ra:= de * TAN(dec)*SIN(h)  + da * (sin(lat)- COS(lat)*(TAN(dec)*COS(h))
     //   delta_dec:=de * COS(h)  + da * COS(lat)*SIN(h))

     //   where de is the polar error in elevation (altitude)
     //   where da is the polar error in azimuth
     //   where h is the hour angle of the reference point equal ra - local_sidereal_time

     dRa:=-elevation_error*(TAN(decJnow0)*SIN(sidereal_time-raJnow0) ) +azimuth_error*(sin(latitude)-COS(latitude)*TAN(decJnow0)*COS(sidereal_time-raJnow0));
     dDec:=-elevation_error*(COS(sidereal_time-raJnow0))  +azimuth_error*COS(latitude)*(SIN(sidereal_time-raJnow0));
     raJnow0:=raJnow0-dRa;
     decJnow0:=decJnow0+dDec;

    //This is also possible but not used:
    //ra_az2(raJnow0,decJnow0,latitude+elevation_error,0,sidereal_time{local incl longitude}, azimuth2,altitude2);//conversion ra & dec to altitude, azimuth
    // az_ra2(azimuth2-azimuth_error,altitude2{+elevation_error},latitude,0,sidereal_time, raJnow0,decJnow0);//conversion az,alt to ra,dec

     precession5(jd, 2451545, raJnow0,decJnow0,ra0,dec0);//Back to J2000. Update J2000 position with polar alignment error

    //calculate angle
    //step one pixel in declination to calculate the angle in Jnow coordinate system
    precession5(2451545, jd ,ra0,dec0-cdelt2*10*pi/180,raJnow1,decJnow1);//position in Jnow coordinate system
    jnow_angle:=position_angle(raJnow1,decJnow1,raJnow0,decJnow0);//Position angle in Jnow
    crota2:=crota2+jnow_angle; //correct the angle for Jnow pole
  end;


  {CD matrix to convert (x,y) to (Ra, Dec)}
  cd1_1:=-cdelt1*cos(crota2); {note 2013 should be crota1 if skewed}
  if cdelt1>=0 then sign:=+1 else sign:=-1;
  cd1_2:=+abs(cdelt2)*sign*sin(crota2);{note 2013 should be crota1 if skewed}
  if cdelt2>=0 then sign:=+1 else sign:=-1;
  cd2_1:=+abs(cdelt1)*sign*sin(crota2);
  cd2_2:=+cdelt2*cos(crota2);


  flip_vertical:=fv;//form1.flipV1.checked;
  flip_horizontal:=fh;//form1.flipH1.checked;

  if fh then cd1_1:=-cd1_1;//flip
  if fv then cd1_2:=-cd1_2;
  if fh then cd2_1:=-cd2_1;
  if fv then cd2_2:=-cd2_2;
end;


procedure load_deep;{load the deepsky database once. If loaded no action}
begin
  if ((deepstring.count<10000) or (deepstring.count>=50000)) {empthy or variable or hyperleda loaded} then {load deepsky database}
  begin
    with deepstring do
    begin
       try
       LoadFromFile(application_path+'deep_sky.csv');{load deep sky data from file }
       except;
         clear;
         beep;
         application.messagebox(pchar('Deep sky database not found. Download and unpack in program directory'),'',0);
       end;
    end;
  end;
end;


//http://fastcode.sourceforge.net/
//function ValLong_JOH_PAS_4_c(Value: Integer): string;
function Valint32(const s; var code: Integer): Longint;{fast val function, about 4 x faster}
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PChar;
begin
  Code := 0;
  P := Pointer(S);
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if P^ in ['+', '-'] then
    begin
      Neg := (P^ = '-');
      inc(P);
    end;
  if P^ = '$' then
    begin
      inc(P);
      Hex := True;
    end
  else
    begin
      if P^ = '0' then
        begin
          inc(P);
          Valid := True;
        end;
      if Upcase(P^) = 'X' then
        begin
          Hex := True;
          inc(P);
        end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do
        begin
          case P^ of
            '0'..'9': Digit := Ord(P^) - Ord('0');
            'a'..'f': Digit := Ord(P^) - Ord('a') + 10;
            'A'..'F': Digit := Ord(P^) - Ord('A') + 10;
            else      Break;
          end;
          if (Result < 0) or (Result > $0FFFFFFF) then
            Break;
          Result := (Result shl 4) + Digit;
          Valid := True;
          inc(P);
        end;
    end
  else
    begin
      while True do
        begin
          if not (P^ in ['0'..'9']) then
            break;
          if Result > (MaxInt div 10) then
            break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(LongInt) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    Code := P-@S+1;
end;


procedure read_deepsky(searchmode:char; telescope_ra,telescope_dec, cos_telescope_dec {cos(telescope_dec},fov : double; out ra2,dec2,length2,width2,pa : double);{deepsky database search}
var
  x,z,y      : integer;
  fout,fout2, backsl1, backsl2,length_regel : integer;
  regel, data1     :  string;
  delta_ra : double;
  p2,p1: pchar;
begin
  repeat {until fout is 0}

    if linepos>=deepstring.count then
      begin
        linepos:=$FFFFFF;{mark as completed}
        exit;
      end;
    regel:=deepstring.strings[linepos]; {using regel,is faster then deepstring.strings[linepos]}
    inc(linepos);
    x:=1; z:=0; y:=0;

    P1 := Pointer(REGEL);
    length_regel:=length(regel);

    repeat
      {fast replacement for y:=posEx(',',regel,y+1);}
      while ((y<length_regel) and (p1^<>',')) do
             begin inc(y); inc(p1,1) end;
      inc(y); inc(p1,1);

      {fast replacement for data1:=copy(regel,x,y-x);}
      SetLength(data1, y-x);
      if y<>x then {not empthy 2018}
      begin
        P2 := Pointer(regel);
        inc(P2, X-1);
        move(P2^,data1[1], y-x);

        while ((length(data1)>1) and (data1[length(data1)]=' ')) do {remove spaces in the end since VAL( can't cope with them}
                                      delete(data1,length(data1),1);
      end;{not empthy}
      x:=y;
      inc(z); {new data field}

      if data1='' then   { to be checked by Han }
      begin
//         beep;
//         continue;
      end;

      case z of 1:  ra2:=valint32(data1,fout)*pi*2/864000;{10*60*60*24, so RA 00:00 00.1=1}
                        {valint32 takes 1 ms instead of 4ms}

                2: begin
                     dec2:=valint32(data1,fout)*pi*0.5/324000;{60*60*90, so DEC 00:00 01=1}
                     delta_ra:=abs(ra2-telescope_ra); if delta_ra>pi then delta_ra:=pi*2-delta_ra;

                     if ((searchmode<>'T') and                                                        {if searchmode is 'T' then full database search else within FOV}
                         ( sqr( delta_ra*cos_telescope_dec)  + sqr(dec2-telescope_dec)> sqr(fov)  ) ) {calculate angular distance and skip when outside FOV}
                           then  fout:=99; {if true then outside screen,go to next line}

                   end;
                3: begin
                     naam2:='';{for case data1='';}
                     naam3:='';
                     naam4:='';
                     while ((length(data1)>0) and (data1[1]=' ')) do
                             delete(data1,1,1); {remove spaces in front of the name, in practice faster then trimleft}
                     backsl1:=pos('/',data1);
                     if backsl1=0 then naam2:=data1
                     else
                     begin
                       naam2:=copy(data1,1,backsl1-1);
                       backsl2:=posEX('/',data1,backsl1+2);     { could also use LastDelimiter}
                       if backsl2=0 then naam3:=copy(data1,backsl1+1,length(data1)-backsl1+1)
                       else
                       begin
                         naam3:=copy(data1,backsl1+1,backsl2-backsl1-1);
                         naam4:=copy(data1,backsl2+1,length(data1)-backsl2+1);
                       end;
                     end;
                   end;
                4: begin
                      val(data1,length2,fout2);{accept floating points}
                   end;{go to next object}
                5: begin
                     val(data1,width2,fout2);{accept floating points}
                   end;
                6: begin val(data1,pa,fout2);{accept floating points}
                         if fout2<>0 then pa:=999;  {unknown orientation}
                   end;

       end;
       inc(x);
    until ((z>=6) or (fout<>0));
  until fout=0;  {repeat until no errors }
end;


procedure line_in_array(img : image_array; xStart, yStart, xEnd, yEnd : integer; color : longword);{draw a line in an array}
// Bresenham's Line Algorithm.  Byte, March 1988, pp. 249-253. http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm
var
  a, b       :  integer;  // displacements in x and y
  d          :  integer;  // decision variable
  diag_inc   :  integer;  // d's increment for diagonal steps
  dx_diag    :  integer;  // diagonal x step for next pixel
  dx_nondiag :  integer;  // nondiagonal x step for next pixel
  dy_diag    :  integer;  // diagonal y step for next pixel
  dy_nondiag :  integer;  // nondiagonal y step for next pixel
  i          :  integer;  // loop index
  nondiag_inc:  integer;  // d's increment for nondiagonal steps
  swap       :  integer;  // temporary variable for swap
  x,y        :  integer;  // current x and y coordinates
  img_width,img_height: integer;
begin
  img_width:=length(img[0]);
  img_height:=length(img);


  x:=xStart;              // line starting point}
  y:=yStart;
  // Determine drawing direction and step to the next pixel.
  a:=xEnd - xStart;       // difference in x dimension
  b:=yEnd - yStart;       // difference in y dimension
  // Determine whether end point lies to right or left of start point.
  if a < 0 then             // drawing towards smaller x values?
  begin
    a:=-a;               // make 'a' positive
    dx_diag:=-1
    end
  else
  dx_diag:=1;

  // Determine whether end point lies above or below start point.
  if b < 0 then             // drawing towards smaller x values?
  begin
    b:=-b;               // make 'a' positive
    dy_diag:=-1
  end
  else
  dy_diag:=1;
  // Identify octant containing end point.
  if a < b then
  begin
    swap:=a;
    a:=b;
    b:=swap;
    dx_nondiag:=0;
    dy_nondiag:=dy_diag
  end
  else
  begin
    dx_nondiag:=dx_diag;
    dy_nondiag:=0
  end;
  d:=b + b - a;           // initial value for d is 2*b - a
  nondiag_inc:=b + b;     // set initial d increment values
  diag_inc   :=b + b - a - a;
  for i:=0 to a do
  begin   /// draw the a+1 pixels
    if ((x>=0) and (x<img_width) and (y>=0) and (y<img_height)) then
    img[y,x]:=color;
    if d < 0 then            // is midpoint above the line?
    begin                 // step nondiagonally
      x:=x + dx_nondiag;
      y:=y + dy_nondiag;
      d:=d + nondiag_inc  // update decision variable
    end
    else
    begin                 // midpoint is above the line; step diagonally}
      x:=x + dx_diag;
      y:=y + dy_diag;
      d:=d + diag_inc
  end;
end;
end;


procedure plot_glx2(img: image_array;x9,y9,diameter,neigung {ratio width/length},orientation:double; colour :longword); {draw oval or galaxy}
var   i,nr,x,y,oldx,oldy,startx,starty  : integer;
      r, sin_ori,cos_ori                : double;
begin
   if diameter<10 then nr:=22
   else
     if diameter<20 then nr:=44
   else
     nr:=127;
  x:=0;
  y:=0;


  if abs(neigung)<0.00001 then neigung:=0.00001;{show ring always also when it is flat}
   for i:=0 to nr+1 do
   begin
     r:=sqrt(sqr(diameter*neigung)/(1.00000000000001-(1-sqr(neigung))*sqr(cos(-pi*i*2/(nr))))); {radius ellips}
     sincos(orientation + pi*i*2/nr, sin_ori, cos_ori);
     oldx:=x;
     oldy:=y;
     x:=round(x9+r * sin_ori);
     y:=round(y9+r * cos_ori);
     if i<>0 then
        line_in_array(img, oldx,oldy,x,y,colour){draw a line in an array}
     else
     begin
       startx:=x;
       starty:=y;
     end;
   end;
   line_in_array(img, x,y,startx,starty,colour){close the loop}
end;


procedure rotate(rot,x,y :double;var  x2,y2:double);{rotate a vector point, angle seen from y-axis, counter clockwise}
var
  sin_rot, cos_rot :double;
begin
  sincos(rot, sin_rot, cos_rot);
  x2:=x * + sin_rot + y*cos_rot;
  y2:=x * - cos_rot + y*sin_rot;//SEE PRISMA WIS VADEMECUM BLZ 68}
end;


procedure sensor_to_celestial(fitsx,fitsy : double; out ra,dec :double); //(x,y) -> (RA,DEC)
var
  u2,v2,xi,eta,
  sindec0,cosdec0,delta  : double;
begin
  u2:=fitsx-crpix1;
  v2:=fitsy-crpix2;
  xi :=(cd1_1*(u2)+cd1_2*(v2))*pi/180;
  eta:=(cd2_1*(u2)+cd2_2*(v2))*pi/180;

  sincos(dec0,sindec0,cosdec0);
  delta:=cosdec0-eta*sindec0;

  ra:=ra0+arctan2(xi,delta); {atan2 is required for images containing celestial pole}
  dec:=arctan((sindec0+eta*cosdec0)/sqrt(sqr(xi)+sqr(delta)));

  if ra<0 then ra:=ra+pi*2;
  if ra>pi*2 then ra:=ra-pi*2;
end;


{ transformation of equatorial coordinates into CCD pixel coordinates for optical projection, rigid method}
{ ra0,dec0: right ascension and declination of the optical axis}
{ ra,dec:   right ascension and declination}
{ xx,yy :   CCD coordinates}
{ cdelt:    CCD scale in arcsec per pixel}
procedure equatorial_standard(ra0,dec0,ra,dec, cdelt : double; var xx,yy: double);
var dv,sin_dec0,cos_dec0,sin_dec ,cos_dec,sin_deltaRA,cos_deltaRA: double;
begin
  sincos(dec0  ,sin_dec0 ,cos_dec0);
  sincos(dec   ,sin_dec  ,cos_dec );
  sincos(ra-ra0, sin_deltaRA,cos_deltaRA);
  dv  := (cos_dec0 * cos_dec * cos_deltaRA + sin_dec0 * sin_dec) / (3600*180/pi)*cdelt; {/ (3600*180/pi)*cdelt, factor for onversion standard coordinates to CCD pixels}
  xx := - cos_dec *sin_deltaRA / dv;{tangent of the angle in RA}
  yy := -(sin_dec0 * cos_dec * cos_deltaRA - cos_dec0 * sin_dec) / dv;  {tangent of the angle in DEC}
end;


procedure plot_deepsky;{plot the deep sky object on the image}
var
  fitsX,fitsY,dra,ddec, cos_telescope_dec,fov,ra2,dec2,length1,width1,pa,len,flipped,
  gx_orientation, delta_ra,det,SIN_dec_ref,COS_dec_ref,SIN_dec_new,COS_dec_new,SIN_delta_ra,COS_delta_ra,hh,
  ra_database, dec_database: double;
  name: string;
  x,y,labels                   : integer;
begin
  if cd1_1<>0 then
  begin
    labels:=form1.plotted_info1.itemindex;{0 None, 1 HFD, 2 Info, 3 Objects, 4 All, 5 no deepsky, 6 no star saturation, 7 north-east indicator}


    ra_database:=ra0;
    dec_database:=dec0;

    fov:=1.5*sqrt(sqr(0.5*width2*cdelt1)+sqr(0.5*height2*cdelt2))*pi/180; {field of view with 50% extra}
    linepos:=0;
    if ((cdelt1>0) = (cdelt2>0)) then flipped:=-1 {n-s or e-w flipped} else flipped:=1;  {Flipped image. Either flipped vertical or horizontal but not both. Flipped both horizontal and vertical is equal to 180 degrees rotation and is not seen as flipped}

    form1.image1.canvas.pen.color:=clyellow;

    form1.image1.Canvas.font.size:=14;
    form1.image1.Canvas.Pen.width :=2;

    form1.image1.Canvas.brush.Style:=bsClear;
    form1.image1.Canvas.font.color:=clyellow;


    cos_telescope_dec:=cos(dec_database);
    sincos(dec0,SIN_dec_ref,COS_dec_ref);{do this in advance since it is for each pixel the same}

    repeat
      read_deepsky('S',ra_database, dec_database, cos_telescope_dec {cos(telescope_dec},fov,{var} ra2,dec2,length1,width1,pa);{deepsky database search}

      {5. Conversion (RA,DEC) -> (x,y). See http://alain.klotz.free.fr/audela/libtt/astm1-fr.htm}
      sincos(dec2,SIN_dec_new,COS_dec_new);{sincos is faster then seperate sin and cos functions}
      delta_ra:=ra2-ra0;
      sincos(delta_ra,SIN_delta_ra,COS_delta_ra);
      HH := SIN_dec_new*sin_dec_ref + COS_dec_new*COS_dec_ref*COS_delta_ra;
      dRA := (COS_dec_new*SIN_delta_ra / HH)*180/pi;
      dDEC:= ((SIN_dec_new*COS_dec_ref - COS_dec_new*SIN_dec_ref*COS_delta_ra ) / HH)*180/pi;
      det:=CD2_2*CD1_1 - CD1_2*CD2_1;
      fitsX:= +crpix1 - (CD1_2*dDEC - CD2_2*dRA) / det;{1..width2}
      fitsY:= +crpix2 + (CD1_1*dDEC - CD2_1*dRA) / det;{1..height2}
      x:=round(fitsX-1);{0..width2-1}
      y:=round(fitsY-1);{0..height2-1}


      if ((x>-0.25*width2) and (x<=1.25*width2) and (y>-0.25*height2) and (y<=1.25*height2)) then {within image1 with some overlap}
      begin
        gx_orientation:=pa*flipped+crota2*180/pi;
        if flip_horizontal then gx_orientation:=-gx_orientation;
        if flip_vertical then gx_orientation:=-gx_orientation;
        if ((labels>=3) and (labels<=4) and (x>=0) and (x<=width2-1) and (y>=0) and (y<=height2-1) ) then {plot only text if center object is visible}
        begin
          if naam3='' then name:=naam2
          else
          if naam4='' then name:=naam2+'/'+naam3
          else
          name:=naam2+'/'+naam3+'/'+naam4;

          annotation_to_array(name,true{transparant},3*graylevel,1, x,y {screen coord},img_array);{string to image array as annotation, result is flicker free since the annotion is plotted as the rest of the image}
       end;{centre object visible}


       {plot deepsky object}
       if width1=0 then begin width1:=length1;pa:=999;end;
       len:=length1/(abs(cdelt2)*60*10*2); {Length in pixels}

       if len<=2 then {too small to plot an ellipse or circle, plot just four dots}
       begin
         if ( (x>=2) and (x<=width2-1-2) and (y>=2) and (y<=height2-1-2) ) then {plot only if visible}
         begin
           img_array[y+2,x-2]:=3000;
           img_array[y+2,x+2]:=3000;
           img_array[y-2,x-2]:=3000;
           img_array[y-2,x+2]:=3000;
         end;
       end
       else
       begin
         if PA<>999 then
         begin
           plot_glx2(img_array,x,y,len,width1/length1,gx_orientation*pi/180,$040402) {draw oval or galaxy}
         end
         else
          plot_glx2(img_array,x,y,len,1,0,$040402) {draw circle}
       end;
     end;
    until linepos>=$FFFFFF;{end of database}
  end;
end;{plot deep_sky}


function hfd_calc(position,perfectfocusposition,a,b:double) :double; {calculate HFD from position and perfectfocusposition using hyperbola parameters}
{The HFD (half flux diameter) of the imaged star disk as function of the focuser position can be described as hyperbola}
{a,b are the hyperbola parameters, a is the lowest HFD value at focus position, the asymptote y:= +-x*a/b} {rev1}
{A hyperbola is defined as: }
{x=b*sinh(t)                }
{y=a*cosh(t)                }
{Using the arccosh and arsinh functions it is possible to inverse}
{above calculations and convert x=>t and t->y or y->t and t->x}
var
  x,t : double;
begin
  x:=perfectfocusposition - position;
  t:=arsinh(x/b);{calculate t-position in hyperbola}
  result:=a*cosh(t);{convert t-position to y/hfd value}
end;


procedure colourshift(pattern,offsetX,offsetY : integer; var img: image_array);//colour shift OSC image
var
  w,h,x,y : integer;
begin
  w:=Length(img[0]); {width}
  h:=Length(img); {height}

  if pattern=1 then // make image reddish
  begin
    for y := 0 to h -1 do
    begin
      for x := 0 to w -1 do
      begin
        if ((odd(x+offsetX)=false) and (odd(y+offsetY)=true)) then //red  sensitive pixels
          img[y,x]:=img[y,x]*4;
      end;
    end;
  end
  else
  if pattern=2 then // make image greenish
  begin
    for y := 0 to h -1 do
    begin
      for x := 0 to w -1 do
        begin
          if odd(x+offsetX)=odd(y+offsetY) then //green sensitive pixels
            img[y,x]:=img[y,x]*4;
        end;
    end;
  end
  else
  if pattern=3 then // make image blueish
  begin
    for y := 0 to h -1 do
    begin
      for x := 0 to w -1 do
        begin
          if ((odd(x+offsetX)=true) and (odd(y+offsetY)=false)) then //blue  sensitive pixels
            img[y,x]:=img[y,x]*4;
        end;
    end;
  end
  else
  if pattern=11 then // make FLAT
  begin
    for y := 0 to h -1 do
    begin
      for x := 0 to w -1 do
        begin
          img[y,x]:=40000; //in one second
        end;
    end;
  end
  else
  if pattern=12 then // make a green OSC FLAT
  begin
    for y := 0 to h -1 do
    begin
      for x := 0 to w -1 do
        begin
          if odd(x+offsetX)=odd(y+offsetY) then //green sensitive pixels
             img[y,x]:=40000
          else
          img[y,x]:=0
        end;
    end;
  end
end;

procedure coloured_lines(x2,y2,bayeroffset_X,bayeroffset_Y:integer; var img: image_array);//mark with colour lines for raw OSC

var
  x,y,textoffset : integer;
const
  white=3000;
  black=0;
begin
  if form1.fliptext1.checked then textoffset:=0 else textoffset:=18;

  annotation_to_array('R=',true{transparant},graylevel,2,x2,y2+textoffset+40 {screen coord},img);
  annotation_to_array('G=',true{transparant},graylevel,2,x2,y2+textoffset+20 {screen coord},img);
  annotation_to_array('B=',true{transparant},graylevel,2,x2,y2+textoffset {screen coord},img);

  for x:=x2+30   to x2+100 do
    for y:=y2+2 to y2+16 do
    begin
      if ((odd(x+bayeroffset_X)=true) and (odd(y+bayeroffset_Y)=false))  then img[y,x]:=white else  img[y,x]:=black;//blue
      if ( odd(x+bayeroffset_X) = odd(y+bayeroffset_Y) ) then img[y+20,x]:=white else  img[y+20,x]:=black;//green
      if  ((odd(x+bayeroffset_X)=false) and (odd(y+bayeroffset_Y)=true)) then img[y+40,x]:=white else  img[y+40,x]:=black;//red
    end;


  annotation_to_array('RED',true{transparant},graylevel,2,x2+70,y2+textoffset+70 {screen coord},img);
  annotation_to_array('GREEN',true{transparant},graylevel,2,x2+120,y2+textoffset+70 {screen coord},img);
  annotation_to_array('BLUE',true{transparant},graylevel,2,x2+200,y2+textoffset+70 {screen coord},img);

  annotation_to_array('RGGB',true{transparant},graylevel,2,x2,y2+textoffset+90 {screen coord},img);
  annotation_to_array('GRBG',true{transparant},graylevel,2,x2,y2+textoffset+110 {screen coord},img);
  annotation_to_array('GBRG',true{transparant},graylevel,2,x2,y2+textoffset+130 {screen coord},img);
  annotation_to_array('BGGR',true{transparant},graylevel,2,x2,y2+textoffset+150 {screen coord},img);

  for x:=x2+70 to x2+250 do
  for y:=y2+90 to y2+170 do
  begin
    if ((x<x2+120) and                (y<y2+110)) then  begin if ((odd(x+1)) and (odd(y))) then img[y,x]:=white else  img[y,x]:=black; end;//rggb red
    if ((x>x2+120) and (x<x2+190) and (y<y2+110)) then  begin if odd(x)=odd(y) then img[y,x]:=white else  img[y,x]:=black;  end; //rggb green
    if ((x>x2+190) and (x<x2+250) and (y<y2+110)) then  begin if ((odd(x)) and (odd(y+1))) then img[y,x]:=white else  img[y,x]:=black; end; //rggb blue

    if ((x<x2+120) and (y>y2+110) and (y>y2+110) and (y<y2+130)) then begin if ((odd(x)) and (odd(y))) then img[y,x]:=white else  img[y,x]:=black; end;//grbG red
    if ((x>x2+120) and (x<x2+190) and (y>y2+110) and (y<y2+130)) then  begin if odd(x+1)=odd(y) then img[y,x]:=white else  img[y,x]:=black;  end; //grbg green
    if ((x>x2+190) and (x<x2+250) and (y>y2+110) and (y<y2+130)) then  begin if ((odd(x+1)) and (odd(y)=false)) then img[y,x]:=white else  img[y,x]:=black; end; //grbg blue

    if ((x<x2+120) and (y>y2+110) and (y>y2+130) and (y<y2+150)) then begin if ((odd(x+1)) and (odd(y+1))) then img[y,x]:=white else  img[y,x]:=black; end;//gbrg red
    if ((x>x2+120) and (x<x2+190) and (y>y2+130) and (y<y2+150)) then  begin if odd(x)=odd(y+1) then img[y,x]:=white else  img[y,x]:=black;  end; //gbrg green
    if ((x>x2+190) and (x<x2+250) and (y>y2+130) and (y<y2+150)) then  begin if ((odd(x)) and (odd(y+1)=false)) then img[y,x]:=white else  img[y,x]:=black; end; //gbrg blue


    if ((x<x2+120) and (y>y2+110) and (y>y2+150) and (y<y2+170)) then begin if ((odd(x)) and (odd(y+1))) then img[y,x]:=white else  img[y,x]:=black; end;//bggr red
    if ((x>x2+120) and (x<x2+190) and (y>y2+150) and (y<y2+170)) then  begin if odd(x+1)=odd(y+1) then img[y,x]:=white else  img[y,x]:=black;  end; //bggr green
    if ((x>x2+190) and (x<x2+250) and (y>y2+150) and (y<y2+170)) then  begin if ((odd(x+1)) and (odd(y+1)=false)) then img[y,x]:=white else  img[y,x]:=black; end; //bggr blue
  end;
end;


procedure plot_stars(realposition, perfectposition,a,b : double);{plot stars}
var
  hfd,fitsX,fitsY, x1,y1,
  dra,ddec, ra_database,dec_database, fov,ra2,dec2, mag2,Bp_Rp, peakvalue,
  delta_ra,det,SIN_dec_ref,COS_dec_ref,SIN_dec_new,COS_dec_new,SIN_delta_ra,COS_delta_ra,hh,sigma,max_magn,
  focal_ratio,angle,distance,sqr_distance,pedestal,cosdec,  frac1,frac2,frac3,frac4,val,
  distance3,distance1,distanceX, distanceY,sqr_distance_norm,xc,yc,angle_starpos : double;
  star_total_counter, stepsize,i, area1,area2,area3,area4,w,h,x,y,hotpixels,
  tilt_index,half_width,half_height                                                           : integer;

    PROCEDURE plot_star;
    var
       m,n,subsampling,xx,yy       : integer;
       sqrdistance,hfd2 : double;
    begin
     {5. Conversion (RA,DEC) -> (x,y)}
      sincos(dec2,SIN_dec_new,COS_dec_new);{sincos is faster then seperate sin and cos functions}
      delta_ra:=ra2-ra0;
      sincos(delta_ra,SIN_delta_ra,COS_delta_ra);
      HH := SIN_dec_new*sin_dec_ref + COS_dec_new*COS_dec_ref*COS_delta_ra;
      dRA := (COS_dec_new*SIN_delta_ra / HH)*180/pi;
      dDEC:= ((SIN_dec_new*COS_dec_ref - COS_dec_new*SIN_dec_ref*COS_delta_ra ) / HH)*180/pi;
      det:=CD2_2*CD1_1 - CD1_2*CD2_1;
      fitsX:= +crpix1 - (CD1_2*dDEC - CD2_2*dRA) / det; {1..width2}
      fitsY:= +crpix2 + (CD1_1*dDEC - CD2_1*dRA) / det; {1..height2}
      x1:=fitsX-1; {0..width2-1}
      y1:=fitsY-1; {0..height2-1}


      if ((x1>=0) and (x1<width2) and (y1>=0) and (y1<height2)) then {within image1}
      begin
       begin {annotate}
         //tilt
         case tilt_index of 1: hfd2:=hfd_calc(realposition+  {tilt in focus pos} b*5*((half_width-x1)/half_height), perfectposition,a,b); {tilt around vertical axis}
                            2:begin
                                distance:=sqrt(sqr(x1-half_width)+sqr(y1-half_height));{distance from center}

                                angle:=arctan2(y1-half_height,x1-half_width); {angle}
                                if angle<0 then angle:=angle+pi;
                                distance:=abs(sin(angle-135*pi/180)*distance);  {distance from -45 degree line}
                                hfd2:=hfd_calc(realposition+  {tilt in focus pos} b*5*(distance/half_height), perfectposition,a,b); {tilt around horizontal axis}
                              end;
                            3: hfd2:=hfd_calc(realposition+  {tilt in focus pos} b*5*((half_height-y1)/half_height), perfectposition,a,b); {tilt around horizontal axis}
                            4: begin {sqr_distance}
                                 xc:=x1-half_width;
                                 yc:=y1-half_height;
                                 sqr_distance:=(sqr(xc)+sqr(yc));
                                 sqr_distance_norm:=sqr_distance/(sqr(half_width)+sqr(half_height));{normalise distance in about 0..1}
                                 hfd2:=hfd_calc(realposition+  {tilt in focus pos} b*2* sqr_distance_norm, perfectposition,a,b); {curvature, hfd increases with the sqr distance}
                                 distance1:=sqrt(sqr_distance_norm);
                                 distance3:=distance1*distance1*distance1;//3th order distance
                                 angle_starpos:=arctan2(yc,xc);
                                 distanceX:=distance3*cos(angle_starpos);
                                 distanceY:=distance3*sin(angle_starpos);

                                 x1:=x1+15*distanceX;//about 15 pixel pincushion distortion max
                                 y1:=y1+15*distanceY;//about 15 pixel pincushion distortion max
                                 x1:=x1+15*distanceX;//about 15 pixel pincushion distortion max
                                 y1:=y1+15*distanceY;//about 15 pixel pincushion distortion max
                               end;
                          else  hfd2:=hfd;
                          end; {case}

         Stepsize:=round(HFD2*2.5);
         sigma:=HFD2/2.35;  {Define star size, HFD=FWHM=2*sqrt(2*ln(2*sigma))=2.35 sigma}

         if hfd2<=3 then subsampling:=5 //5 {sampling within the pixel}
                   else subsampling:=1;{out of focus stars, position less important}
         inc(star_total_counter);
         peakvalue:=$FFFFFF*power(2.5,(max_magn-mag2-140)/10);  {Sensitivity of the telescope. Limiting magnitude give gives a peak pixel value 45}
         peakvalue:=peakvalue*(2.35*2.35)/(hfd2*hfd2);{reduce peak value sqr of the hfd}
         if peakvalue>5 then //above noise
         begin
           stepsize:=stepsize*subsampling;{subsample within a pixel}
           for m:=-stepsize to stepsize do
           for n:=-stepsize to stepsize do
           begin
             sqrdistance:=sqr(m/subsampling)+sqr(n/subsampling);
             val:=peakvalue*(1/sqr(subsampling))*EXP(-0.5*(sqrdistance)/(sigma*sigma)); {gaussian shaped stars sampled subsampling x subsampling within a pixel}
             if val>0 then
             begin
               xx:=round(x1+m/subsampling);
               yy:=round(y1+n/subsampling);
               if ((xx>=0) and (xx<width2) and (yy>=0) and (yy<height2)) then {within image}
                 img_array[yy,xx]:=min(img_array[yy,xx]+val,$FFFFFF);{integrate supsamples in case subsampling is larger then one. Integration is required for close overlapping double like Sirus. Prevent values above 24 bit equals $FFFFFF}
             end;
           end;
         end;//above noise
        end;
     end;
   end;

begin
  flux_magn_offset:=0;

  if cd1_1<>0 then
  begin
    hfd:=hfd_calc(realposition, perfectposition,a,b); {a=2.35, b=2.35*1000/10, so hfd is 10 when focus position is 1000 position off}

    tilt_index:=form1.manipulations1.itemindex;
    half_width:= width2 div 2;
    half_height:=height2 div 2;

    counter_flux_measured:=0;

    labels:=form1.plotted_info1.itemindex; {0 None, 1 HFD, 2 Info, 3 Objects labels, 4 All labels, 5 No deepsky, 6 No star saturation, 7 NE indicator}

    bp_rp:=999;{not defined in mono versions}


    ra_database:=ra0;
    dec_database:=dec0;

    fov:= sqrt(sqr(width2*cdelt1)+sqr(height2*cdelt2))*pi/180; {field of view diagonal with 0% extra}

     fov:=min(fov,9.53*pi/180);{warning FOV should be less the database tiles dimensions, so <=9.53 degrees. Otherwise a tile beyond next tile could be selected}

    star_total_counter:=0;{total counter}
    focal_ratio:=max(2,strtofloat(copy(form1.focal_ratio1.text,3,2)));
    max_magn:=(14 + ln(sqr(7/focal_ratio))/ln(2.5))*10;


    if select_star_database(form1.star_database1.text)=false then
    begin
      application.messagebox(pchar('No star database found at '+application_path+' !'+#13+'Download the g14, g16 or g17 and extract the files to the program directory'), pchar('No star database!'),0);
      exit;
    end;
    form1.star_database1.text:=name_star;

    find_areas( ra_database,dec_database, fov,{var} area1,area2,area3,area4, frac1,frac2,frac3,frac4);{find up to four star database areas for the square image}

    sincos(dec0,SIN_dec_ref,COS_dec_ref);{do this in advance since it is for each pixel the same}

    {prepare array for fast writing}
    w:=form1.image1.picture.Bitmap.Width;
    h:=form1.image1.picture.Bitmap.Height;

    if labels<>6 {no dark} then pedestal:=100 else pedestal:=50; {give it a fixed skybackground of 50}

    setlength(img_array,h,w);
    for y := 0 to h -1 do
    begin {clear array}
      for x := 0 to w -1 do
        begin
          img_array[y,x]:=pedestal;
        end;
    end;

    {add hot pixels}
    hotpixels:=strtoint(form1.hotpixels1.text);
    if hotpixels<>0 then
    for i:=1 to hotpixels do
    begin
      val:=round(65535*random);
      x:=round((w-1)*random);
      y:=round((h-1)*random);
      img_array[y,x]:=val;
    end;

    if labels<>6 {no dark} then
    begin
      {read 1th area}
      if area1<>0 then {read 1th area}
      begin
        while ((readdatabase290(ra_database,dec_database, fov,area1,{var} ra2,dec2, mag2,Bp_Rp)) ) do
               plot_star;{add star}
        close_star_database;{close reader, so next time same file is read from beginning}
      end;

      {read 2th area}
      if area2<>0 then {read 2th area}
      begin
        while ((readdatabase290(ra_database,dec_database, fov,area2,{var} ra2,dec2, mag2,Bp_Rp))) do
               plot_star;{add star}
        close_star_database;{close reader, so next time same file is read from beginning}
      end;

      {read 3th area}
      if area3<>0 then {read 3th area}
      begin
        while ((readdatabase290(ra_database,dec_database, fov,area3,{var} ra2,dec2, mag2,Bp_Rp)) ) do plot_star;{add star}
        close_star_database;{close reader, so next time same file is read from beginning}
      end;
      {read 4th area}
      if area4<>0 then {read 4th area}
      begin
        while ((readdatabase290(ra_database,dec_database, fov,area4,{var} ra2,dec2, mag2,Bp_Rp)) ) do plot_star;{add star}
        close_star_database;{close reader, so next time same file is read from beginning}
      end;

      if ((labels=2) or (labels=4)) then
      begin
        annotation_to_array('ra_2000: '+prepare_ra(ra0),true{transparant},graylevel,1, 10,20 {screen coord},img_array);
        annotation_to_array('de_2000:'+ prepare_dec(dec0),true{transparant},graylevel,1, 10,20+1*20 {screen coord}, img_array);
        annotation_to_array('Focuser: '+inttostr(focuser_position),true{transparant},graylevel,1, 10,20+2*20 {screen coord}, img_array);
      end;
      if ((labels=1) or (labels=2) or (labels=4)) then  {0 None, 1 HFD, 2 Info, 3 Objects, 4 All}
         annotation_to_array('HFD: '+floattostrF(HFD,ffFixed,0,1),true{transparant},graylevel,1, 10,20+3*20 {screen coord}, img_array);
    end;{not a dark}


    if labels=7 then //north-east indicator
    begin
      cosdec:=cos(dec0);
      if abs(cosdec)<0.00001 then exit;
      mag2:=9999;//not a star
      ra2:=ra0;
      dec2:=dec0+fov*0.1;
      plot_star;//find x,y position
      annotation_to_array('N',true{transparant},2*graylevel,3, round(x1),round(y1) {screen coord}, img_array);
      dec2:=dec0-fov*0.1;;
      plot_star;
      annotation_to_array('S',true{transparant},2*graylevel,3, round(x1),round(y1) {screen coord}, img_array);
      ra2:=ra0+fov*0.1/cosdec;
      dec2:=dec0;
      plot_star;
      annotation_to_array('E',true{transparant},2*graylevel,3, round(x1),round(y1) {screen coord}, img_array);
      ra2:=ra0-fov*0.1/cosdec;;
      dec2:=dec0;
      plot_star;
      annotation_to_array('W',true{transparant},2*graylevel,3, round(x1),round(y1) {screen coord}, img_array);
    end;

    //colour shift for OSC
    case tilt_index of
                  //0    Mono
                  //1    Tilt, orientation 0°
                  //2    Tilt, orientation 45°
                  //3    Tilt, orientation 90°
                  //4    Pincushion distortion
                  //5    RGGB reddish
                  //6    RGGB greenish
                  //7    RGGB bluish

                  //8    GRBG reddish
                  //9    GRBG greenish
                  //10    GRBG bluish

                  //11    GBRG reddish
                  //12    GBRG greenish
                  //13    GBRG bluish

                  //14    BGGR reddish
                  //15    BGGR greenish
                  //16    BGGR bluish

                  //17    Mono FLAT
                  //18    RGGB FLAT
                  //19    RGGB FLAT green only
                  //20    GRBG FLAT green only

//  Bayer offsets are both zero = RGGB
//  Bayer offset is 1 and y bayer offset is 0, GRBG.
//  Bayer offset is 0 and y bayer offset is 1, GBRG.
//  both Bayer offsets are 1, BGGR.


                      5:begin //rggb reddish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(1,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to red
                           annotation_to_array('RGGB (OffsetX=0, OffsetY=0). Reddish',true{transparant},1000 {graylevel},2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      6:begin //rggb greenish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(2,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to green
                           annotation_to_array('RGGB (OffsetX=0, OffsetY=0). Greenish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                         end;
                      7:begin //rggb bluish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(3,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to blue
                           annotation_to_array('RGGB (OffsetX=0, OffsetY=0). Bluish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;


                      8:begin //grbg reddish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(1,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to red
                           annotation_to_array('GRBG (RGGB & OffsetX=1, OffsetY=0). Reddish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      9:begin //grbg greenish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(2,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to green
                           annotation_to_array('GRGB (RGGB & OffsetX=1, OffsetY=0). GRGB. Greenish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      10:begin //grbg blueish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           colourshift(3,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to blue
                           annotation_to_array('GRGB (RGGB & OffsetX=1, OffsetY=0). Bluish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;


                      11:begin //gbrg reddish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(1,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to red
                           annotation_to_array('GBRG (RGGB & OffsetX=0, OffsetY=1). Reddish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      12:begin //gbrg greenish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(2,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to green
                           annotation_to_array('GBRG (RGGB & OffsetX=0, OffsetY=1). Greenish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      13:begin //gbrg blueish
                           bayeroffset_X:=0;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(3,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to blue
                           annotation_to_array('GBRG (RGGB & OffsetX=0, OffsetY=1). Bluish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;


                      14:begin //bggr reddish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(1,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to red
                           annotation_to_array('BGGR (RGGB & OffsetX=1, OffsetY=1). Reddish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      15:begin //bggr greenish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(2,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to green
                           annotation_to_array('BGGR (RGGB & OffsetX=1, OffsetY=1). Greenish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;
                      16:begin //bggr blueish
                           bayeroffset_X:=1;
                           bayeroffset_Y:=1;
                           sensor_type:=2;//OSC
                           colourshift(3,bayeroffset_X,bayeroffset_Y,img_array);//colour shift OSC image to blue
                           annotation_to_array('BGGR (RGGB & OffsetX=1, OffsetY=1). Bluish',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           coloured_lines(250,50,bayeroffset_X,bayeroffset_Y,img_array);
                        end;

                      //  Bayer offsets are both zero = RGGB
                      //  Bayer offset is 1 and y bayer offset is 0, GRBG.
                      //  Bayer offset is 0 and y bayer offset is 1, GBRG.
                      //  both Bayer offsets are 1, BGGR.

                      17:begin //Mono flat
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=0;//mono camera
                           colourshift(11,bayeroffset_X,bayeroffset_Y,img_array);//flat
                        end;
                      18:begin //RGGB flat
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           annotation_to_array('RGGB (OffsetX=0, OffsetY=0)',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           colourshift(11,bayeroffset_X,bayeroffset_Y,img_array);//flat
                        end;
                      19:begin //RGGB flat green only
                           bayeroffset_X:=0;
                           bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           annotation_to_array('RGGB (OffsetX=0, OffsetY=0). Green only',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           colourshift(12,bayeroffset_X,bayeroffset_Y,img_array);//flat green only
                        end;
                      20:begin //GRBG flat green only
                            bayeroffset_X:=1;
                            bayeroffset_Y:=0;
                           sensor_type:=2;//OSC
                           annotation_to_array('GRGB (RGGB & OffsetX=1, OffsetY=0). Green only.',true{transparant},graylevel,2,250,20 {screen coord},img_array);
                           colourshift(12,bayeroffset_X,bayeroffset_Y,img_array);//flat green only
                        end;
            else
            begin
              sensor_type:=0;//mono
              bayeroffset_X:=0;
              bayeroffset_Y:=0;
            end;
     end;

  end;
end;{plot stars}


procedure image_array_stretched_to_screen;
var
   x,y,w,h,val,valG    : integer;
   Bitmap  : TBitmap;{for fast pixel routine}
   xLine :  PByteArray;
   min_val : double;
begin
  if img_array=nil then exit;
  w:=form1.image1.picture.Bitmap.Width;
  h:=form1.image1.picture.Bitmap.Height;

  {create temporary bitmap}
  bitmap := TBitmap.Create;
  try
    with bitmap do
    begin
      Width := w;
      Height := h;
        // Unclear why this must follow width/height to work correctly.
        // If PixelFormat precedes width/height, bitmap will always be black.
      bitmap.PixelFormat := pf24bit;
    end;
    except;
  end;

 min_val:=65535;
  {copy array to bitmap}
  for y:=0 to h-1 do
  begin
    xLine :=   Bitmap.ScanLine[y];
    for x:=0 to w-1 do
    begin
      val:=round(img_array[h-1-y,x]);//grey level [0..$FFFFFF], follow fits convention. Pixel 1,1 bottom left
      min_val:=min(min_val,val);{for removing dark current}
      valG:=min(255,1+trunc(15*255*power(((val-min_val)/$FFFF),0.5))); {stretch data for display. give it a bias of 1 for case it is saved with the popup menu of the image tab}


      {$ifdef mswindows}
         xLine^[x*3]  :=valG; {blue}
         xLine^[x*3+1]:=valG; {green}
         xLine^[x*3+2]:=valG; {red}
      {$endif}
      {$ifdef darwin} {MacOS}
         xLine^[x*4+1]:=valG; {red, 4*8=32 bit}
         xLine^[x*4+2]:=valG; {green }
         xLine^[x*4+3]:=valG; {blue}
      {$endif}
      {$ifdef linux}
         xLine^[x*4]  :=valG; {blue, 4*8=32 bit}
         xLine^[x*4+1]:=valG; {green}
         xLine^[x*4+2]:=valG; {red}
       {$endif}
    end;
  end;
  {copy bitmap to image1}
  form1.Image1.picture.Graphic := Bitmap; {show image}


  Bitmap.Free;

  form1.Image1.Picture.Bitmap.Transparent := True;
  form1.Image1.Picture.Bitmap.TransparentColor := clblack;

  form1.Image1.refresh;
end;


end.

