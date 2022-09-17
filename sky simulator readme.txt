Sky simulator for ASCOM using artifical images or online DSS images. The camera simulator will show a deep sky image based on the mount position. The image will be defocused or blurred based on the focuser position.

In the image acquisition programs CCDCiel/NINA/APT/SGP you should select the folowing ASCOM simulators

Camera:
 "Sky Simulator"

Focuser:
 "Simulator"

Mount:
"Telescope Simulator for .NET". 

The program will create or download images and feed the camera simulator with these.

First  start the simulation program. Click on Start simulation. Then a first image.png will be created in default folder documents. Then select in the image acquisition program for the camera  the option "Sky simulator". Click on properties and select for image file the file image.png in folder documents. Select in the simulation program and image acquisition program the same focuser and mount. For full simulation select the focuser and mount as shown above. But you can also select a real mount or focuser.

As soon the mount moves to a new position or the focuser position changes a new image will be created.  Keep the defocus range small.

Keep image size small something like 1200 x1000 pixels. For the artifical sky the exposure time should be set at a few seconds minimum. For the DSS images download the exposure time should be set longer.

With this simulator you can test focuser routine, centering using plate solving and sequences.

--- Han