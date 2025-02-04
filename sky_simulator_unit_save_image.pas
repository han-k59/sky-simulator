unit sky_simulator_unit_save_image;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  fpimage,fpwriteTIFF, fptiffcmn,math,dialogs;

type
  image_array = array of array of single;

function save_tiff16(filen2:string): boolean;{save to 16 bit TIFF file }
function save_fits(filen2:ansistring): boolean;{save 16 fits file}

implementation

uses sky_annotation,sky_simulator_main, alpaca_camera_protocol;

var
  head : array [0..30] of ansistring=
  (
     {0}('SIMPLE  =                    T / FITS header                                    '),
     {1}('BITPIX  =                   16 / Bits per entry                                 '),
     {2}('NAXIS   =                    2 / Number of dimensions                           '),
     {3}('NAXIS1  =                  100 / length of x axis                               '),
     {4}('NAXIS2  =                  100 / length of y axis                               '),
     {5}('EQUINOX =               2000.0 / Equinox of coordinates                         '),
     {6}('DATAMIN =                    0 / Minimum data value                             '),
     {7}('DATAMAX =                65535 / Maximum data value                             '),
     {8}('BZERO   =                  0.0 / physical_value = BZERO + BSCALE * array_value  '),
    { 9}('BSCALE  =                  1.0 / physical_value = BZERO + BSCALE * array_value  '),
    {10}('CTYPE1  = '+#39+'RA---TAN'+#39+'           / first parameter RA  ,  projection TANgential   '),
    {11}('CTYPE2  = '+#39+'DEC--TAN'+#39+'           / second parameter DEC,  projection TANgential   '),
    {12}('CUNIT1  = '+#39+'deg     '+#39+'           / Unit of coordinates                            '),
    {13}('RA      =                  0.0 / RA of image center                             '),
    {14}('DEC     =                  0.0 / DEC of image center                            '),
    {15}('JD      =                  0.0 / Julian day                                     '),
    {16}('CRPIX1  =                  0.0 / X of reference pixel                           '),
    {17}('CRPIX2  =                  0.0 / Y of reference pixel                           '),
    {18}('CRVAL1  =                  0.0 / RA of reference pixel (deg)                    '),
    {19}('CRVAL2  =                  0.0 / DEC of reference pixel (deg)                   '),
    {20}('CD1_1   =                  0.0 / CD matrix to convert (x,y) to (Ra, Dec)        '),
    {21}('CD1_2   =                  0.0 / CD matrix to convert (x,y) to (Ra, Dec)        '),
    {22}('CD2_1   =                  0.0 / CD matrix to convert (x,y) to (Ra, Dec)        '),
    {23}('CD2_2   =                  0.0 / CD matrix to convert (x,y) to (Ra, Dec)        '),
    {24}('PLTSOLVD=                    T / Astrometric solution by Sky Simulator          '),
    {25}('SWCREATE=  Sky Simulator for Ascom and Alpaca                                   '),
    {26}('COMMENT                                                                         '),
    {27}('COMMENT                                                                         '),
    {28}('COMMENT                                                                         '),
    {29}('COMMENT                                                                         '),
    {30}('END                                                                             '));

procedure setvalue(index : integer; value: single);
var
  i,len: integer;
  s2,s : string;
begin
  s2:=head[index];
  for i:=11 to 30 do s2[i]:=' ';//clear contains especially when integers change to floats
  if frac(value)=0 then
    str(round(value),s) //integers
  else
    str(value,s);//floats
  len:=length(s);
  for i:=1 to length(s) do
    s2[30-len+i]:=s[i];
  head[index]:=s2;
end;


function save_tiff16(filen2:string): boolean;{save to 16 bit TIFF file }
var
  i, j, k,m,width5,height5,bzero  :integer;
  image: TFPCustomImage;
  writer: TFPCustomImageWriter;
  format,s3 : string;
  thecolor  : Tfpcolor;
  flip_H,flip_V:boolean;
  fv,fh        : double;

begin
  if img_array=nil then
  begin
    beep;
    exit;
  end;

  width5:=length(img_array[0]);{width}
  height5:=length(img_array);{height}
  bzero:=0;

  //update header
  setvalue(3,width5);
  setvalue(4,height5);
  setvalue(8,bzero);
  setvalue(13,ra0{ra_telescope_2000}*180/pi);//ra
  setvalue(14,dec0{dec_telescope_2000*180/pi});//dec
  setvalue(15,jd);
  setvalue(16,(width5+1)/2);
  setvalue(17,(height5+1)/2);
  setvalue(18,ra0{ra_telescope_2000}*180/pi);//crval1
  setvalue(19,dec0{dec_telescope_2000}*180/pi);

   flip_v:=false; //no flip
  flip_h:=false; //no flip

  if flip_v then fv:=-1 else fv:=1;
  if flip_h then fh:=-1 else fh:=1;

  setvalue(20,fh*cd1_1);
  setvalue(21,fv*cd1_2);
  setvalue(22,fh*cd2_1);
  setvalue(23,fv*cd2_2);
  head[26]:='COMMENT  RA0='+prepare_RA(ra0)+'    DEC0='+prepare_DEC(dec0)+'                                             ';

  //  Bayer offsets are both zero = RGGB
  //  Bayer offset is 1 and y bayer offset is 0, GRBG.
  //  Bayer offset is 0 and y bayer offset is 1, GBRG.
  //  both Bayer offsets are 1, BGGR
  if sensor_type=2 then
  begin
    if ((bayeroffset_X=0) and (bayeroffset_Y=0))  then
        head[27]:='BAYERPAT= '+char(39)+'RGGB'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=1) and (bayeroffset_Y=0))  then
        head[27]:='BAYERPAT= '+char(39)+'GRBG'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=0) and (bayeroffset_Y=1))  then
        head[27]:='BAYERPAT= '+char(39)+'GBRG'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=1) and (bayeroffset_Y=1))  then
        head[27]:='BAYERPAT= '+char(39)+'BGGR'+char(39)+'               / Bayer color pattern'; //will be extended to 80 chars later

    head[28]:='ROWORDER= '+char(39)+'BOTTOM-UP'+char(39)+'          / Bottom row image is the first row in the file.'; //will be extended to 80 chars later
  end
  else
  begin
    head[27]:='COMMENT ';//will be extended to 80 chars later
    head[28]:='COMMENT ';//will be extended to 80 chars later
  end;


  Image := TFPMemoryImage.Create(width5, height5);
  Writer := TFPWriterTIFF.Create;

  Image.Extra[TiffAlphaBits]:='0';

  format:='16'; {32 bit is not available}

  Image.Extra[TiffRedBits]:=format;
  Image.Extra[TiffGreenBits]:=format;
  Image.Extra[TiffBlueBits]:=format;
  Image.Extra[TiffGrayBits]:=format;   {add unit fptiffcmn to make this work. see https://bugs.freepascal.org/view.php?id=35081}

  {grayscale}
  Image.Extra[TiffPhotoMetric]:='1'; {PhotometricInterpretation = 0 (Min-is-White), 1 (Min-is-Black),  so for 1  black is $0000, White is $FFFF}

  image.Extra[TiffSoftware]:='Sky Simulator';

  s3:='';
  for i:=0 to length(head)-1  do
  begin
  if ((cd1_1<>0) or (i<16) or (i>24 )) then
    s3:=s3+head[i]+#10;
  end;
  image.Extra[TiffImageDescription]:=s3; {store full header in TIFF}

  Image.Extra[TiffCompression]:= '8'; {FPWriteTiff only support only writing Deflate compression. Any other compression setting is silently replaced in FPWriteTiff at line 465 for Deflate. FPReadTiff that can read other compressed files including LZW.}

  For i:=0 to height5-1 do
  begin
    if flip_V=false then k:=height5-1-i else k:=i;{reverse fits down to counting}
    for j:=0 to width5-1 do
    begin
      if flip_H=true then m:=width5-1-j else m:=j;
      thecolor.red:=min(65535,round(img_array[k,m]));
      thecolor.green:=thecolor.red;
      thecolor.blue:=thecolor.red;
      thecolor.alpha:=65535;
      image.Colors[j,i]:=thecolor;
    end;
  end;


  result:=true;
  try
    Image.SaveToFile(filen2, Writer);
  except
    result:=false;
    exit;
  end;
  image.Free;
  writer.Free;
end;



function save_fits(filen2:ansistring): boolean;{save 16 fits file}
const
   bufwide=1024*20;
var
  fitsbuffer : array[0..bufwide] of byte;{buffer for 8 bit FITS file}
  fitsbuffer2: array[0..round(bufwide/2)] of word absolute fitsbuffer;{buffer for 16 bit FITS file}
  TheFile4 : tfilestream;
  I,j,bzero, dum, remain,height5,width5 : integer;
  line0                : ansistring;
  aline,empthy_line    : array[0..80] of ansichar;{79 required but a little more to have always room}

begin
  result:=false;
  if img_array=nil then
  begin
    beep;
    exit;
  end;

  bzero:=32768;
  width5:=length(img_array[0]);{width}
  height5:=length(img_array);{height}

  //update header
  setvalue(3,width5);
  setvalue(4,height5);
  setvalue(8,bzero);
  setvalue(13,ra0{ra_telescope_2000}*180/pi);//ra
  setvalue(14,dec0{dec_telescope_2000}*180/pi);//dec
  setvalue(15,jd);
  setvalue(16,(width5+1)/2);
  setvalue(17,(height5+1)/2);
  setvalue(18,ra0{ra_telescope_2000}*180/pi);//crval1
  setvalue(19,dec0{dec_telescope_2000}*180/pi);
  setvalue(20,cd1_1);
  setvalue(21,cd1_2);
  setvalue(22,cd2_1);
  setvalue(23,cd2_2);
  head[26]:='COMMENT  RA0='+prepare_RA(ra0)+'    DEC0='+prepare_DEC(dec0)+'                                             ';

  //  Bayer offsets are both zero = RGGB
  //  Bayer offset is 1 and y bayer offset is 0, GRBG.
  //  Bayer offset is 0 and y bayer offset is 1, GBRG.
  //  both Bayer offsets are 1, BGGR
  if sensor_type=2 then
  begin
    if ((bayeroffset_X=0) and (bayeroffset_Y=0))  then
        head[27]:='BAYERPAT= '+char(39)+'RGGB'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=1) and (bayeroffset_Y=0))  then
        head[27]:='BAYERPAT= '+char(39)+'GRBG'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=0) and (bayeroffset_Y=1))  then
        head[27]:='BAYERPAT= '+char(39)+'GBRG'+char(39)+'               / Bayer color pattern' //will be extended to 80 chars later
    else
    if ((bayeroffset_X=1) and (bayeroffset_Y=1))  then
        head[27]:='BAYERPAT= '+char(39)+'BGGR'+char(39)+'               / Bayer color pattern'; //will be extended to 80 chars later

    head[28]:='ROWORDER= '+char(39)+'BOTTOM-UP'+char(39)+'          / Bottom row image is the first row in the file.'; //will be extended to 80 chars later
  end
  else
  begin
    head[27]:='COMMENT ';//will be extended to 80 chars later
    head[28]:='COMMENT ';//will be extended to 80 chars later
  end;


  try
    TheFile4:=tfilestream.Create(filen2, fmcreate );
    try
      {write memo1 header to file}
      for i:=0 to 79 do empthy_line[i]:=#32;{space}
      i:=0;
      repeat
         if i<length(head) then
         begin
           if ((cd1_1<>0) or (i<16) or (i>24)) then
             line0:=head[i]
             else
             line0:='COMMENT   No solution when "polar alignment error" is checked';
           while length(line0)<80 do line0:=line0+' ';{extend to length 80 if required}

           strpcopy(aline,(copy(line0,1,80)));{copy 80 and not more}
           thefile4.writebuffer(aline,80);{write updated header from memo1}
         end
         else
         thefile4.writebuffer(empthy_line,80);{write empthy line}
         inc(i);
      until ((i>=length(head)-1) and (frac(i*80/2880)=0)); {write multiply records 36x80 or 2880 bytes}

      for i:=0 to height5-1 do
      begin
        for j:=0 to width5-1 do
        begin
          dum:=max(0,min(65535,round(img_array[i,j]))) - bzero;{limit data between 0 and 65535 and shift it to -32768.. 32767}
          { value  - bzero              result  shortint    word
           ($0000  - $8000) and $FFFF = $8000 (-32768       32768 )  note  $0000 - $8000 ==>  $FFFF8000. Highest bits are skipped
           ($0001  - $8000) and $FFFF = $8001 (-32767       32769 )  note  $0001 - $8000 ==>  $FFFF8001. Highest bits are skipped
           ($2000  - $8000) and $FFFF = $A000 (-24576       40960 )
           ($7FFF  - $8000) and $FFFF = $FFFF (    -1       65535 )
           ($8000  - $8000) and $FFFF = $0000 (     0           0 )
           ($8001  - $8000) and $FFFF = $0001 (     1           1 )
           ($A000  - $8000) and $FFFF = $2000 (  8192        8192 )  note $A000 - $8000 equals  $2000.
           ($FFFE  - $8000) and $FFFF = $7FFE (+32766       32766 )
           ($FFFF  - $8000) and $FFFF = $7FFF (+32767       32767 )
          }
          fitsbuffer2[j]:=swap(word(dum));{in FITS file hi en low bytes are swapped}
        end;
        thefile4.writebuffer(fitsbuffer2,width5+width5); {write as bytes}
      end;


      remain:=round(2880*(1-frac(thefile4.position/2880)));{follow standard and only write in a multi of 2880 bytes}
      if ((remain<>0) and (remain<>2880)) then
      begin
        FillChar(fitsbuffer, remain, 0);
        thefile4.writebuffer(fitsbuffer,remain);{write some bytes}
      end;
    finally
      TheFile4.free;
    end;
  result:=true;
  except
    beep;//write error
  end;
end;



end.

