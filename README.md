Sky simulator for ASCOM & Alpaca is a program for simulating an astronomical camera on a computer controlled telescope. The camera will show images of the night sky based on the telescope pointing direction.  The images are created artificially or are from the online deep sky survey. The images can be defocused/ blurred by the focuser position.

Setup instructions are available here: https://sky-simulator.sourceforge.io/

In the image acquisition programs CCDCiel/NINA/APT/SGP you should select the folowing ASCOM simulators

Camera:
 "Sky Simulator"

Focuser:
 "Simulator" or your own focuser.

Mount:
"Telescope Simulator for .NET".  or your own mount

The program will create or download images and feed the camera simulator with these.

First  start the simulation program. Click on Start simulation. Then a first image.png will be created in default folder documents. Then select in the image acquisition program for the camera  the option "Sky simulator". Click on properties and select for image file the file image.png in folder documents. Select in the simulation program and image acquisition program the same focuser and mount. For full simulation select the focuser and mount as shown above. But you can also select a real mount or focuser.

As soon the mount moves to a new position or the focuser position changes a new image will be created. 

Keep image size small something like 1200 x1000 pixels. For the artifical sky the exposure time should be a few seconds minimum. For the DSS images download the exposure time should be longer.

With this simulator you can test the focuser routine, centering using plate solving, guiding of PHD2 and sequences.

If the Alpaca mount is used, the mount flipping and reverse declination can be tested.

For feedback and comments use the forum at SourceForge (https://sourceforge.net/p/sky-simulator/discussion/) or make a ticket.

For a small field-of-view setting a seperate star database down to magnitude 16 packed in a zip file is provided (G16). Also the G17 star database down to magitude 17 of the HNSKY program could be used.

# Han


History of the Sky Simulator for ASCOM

2024-06-07,  Version 2.1.1. Some optimisations. Added a popup menu to save the created image directly to FITS or AstroFITS including astrometrical solution.
2024-02-20,  Version 2.1.0a. Menu optimisation. Show better why artificial sky is forced.
2024-02-17,  Version 2.1.0. Menu reorganisation. Put each equimpment simulation in a seperate tab.
2023-12-28,  Version 2.0.3. Introduced menu option telescope focal ratio. This allows sufficient stars for guiding testing having a small FOV.
2023-11-04,  Version 2.0.2c. While in simulation allow hot switching from Alpaca to Ascom.
2023-09-15,  Version 2.0.2b. Fix initialise sideofPier correctly when starting up (Alpaca Only). 
2023-08-16,  Version 2.0.2a, fixed Alpaca guider camera which was broken in version 2.0.2. Reduced gray level annotations
2023-08-05,  Version 2.0.2, Added some OSC camera capabilities.
2023-02-19,  Version 2.0.1c, For Alpaca mount: fixed RAtrackingrate to ra sec/sec, fixed direction for moveaxis
2023-02-06,  Version 2.0.1b, Added for the Alpaca mount an option to swap north and south
2023-02-06,  Version 2.0.1, Added equatorial mount for Alpaca. Improved simulation of meridian flip
2023-01-31,  Version 2.0.0f, Added pulse guiding and pier side for Alpaca. Some minor changes
2022-10-20,  Version 2.0.0e, Some cosmetic changes in calculator.
2022-08-16,  Version 2.0.0d, Fixed the default Documents folder path.
2022-06-30,  Version 2.0.0c, Added option for flipping annotated text verticial.
2022-06-27,  Version 2.0.0b, Flipped the Alpaca subframe location vertical
2022-06-24,  Version 2.0.0a, Added an Alpaca option for normal exposure durations. Uncheck option "fast simulation".
2022-06-21,  Version 2.0.0, Only some cosmetic corrections           
2022-06-07,  Version 2.0.0ß34,  Allowed in alpaca setting guide rate.
2022-05-31,  Version 2.0.0ß33,  Fixed Alpaca IsPulseGuider bug
2022-05-30,  Version 2.0.0ß31,  Fixed an Alpaca  server bug. Added an Alpaca Guider.
2022-05-20,  Version 2.0.0ß29,  Added more tracking errors for the mount.
2022-05-18,  Version 2.0.0ß28,  Added pulse guiding to Alpaca. modified the tracking error options
2022-04-30,  Version 2.0.0ß27,  Very minor change in server.
2022-04-29,  Version 2.0.0ß26,  Fixed a problem if Alpaca packages are coming very fast behind each other.
2022-04-22,  Version 2.0.0ß25,  Modified Alpaca server for persistent connections. So for HTTP 1.1
2022-04-15,  Version 2.0.0ß22,23,24  Worked on alpaca conformity.
2022-04-12,  Version 2.0.0ß21, Added more Alpaca methods and exceptions.
2022-04-10,  Version 2.0.0ß20, Fixed some Alpaca methods
2022-04-07,  Implemented two more Alpaca camera methods. Improved image stretch for image tab.
2022-03-13,  Replaced the G16 star database  with a mono version to reduce disk space.
2022-03-11,  Version 2.0.0ß16, Added tilt and curvature to artifical sky simulation.
2022-02-17,  Version 2.0.0ß15, Significant change in ASCOM Sky simulator camera. Image.png is now used to transport floating point values in 24 bit. Image.png in a normal viewer will now look like a  a piece of art but it is functional.
2022-02-15,  Version 2.0.0ß14, Made noise setting in ASCOM camera adjustable
2022-02-05,  Version 2.0.0ß13, Made image in tab image sizable.
2022-02-04,  Version 2.0.0ß12
2022-02-03,  Version 2.0.0ß11, Fixed the DSS option.
2022-01-29,  Version 2.0.0ß5,  Added gain in the output calculation.  Higher gain reduces the read noise but for longer exposures the dark current (Ascom) or sky signal (Alpaca dominates) 
2022-01-29,  Version 2.0.0ß4,  Added hot pixels. Reinstated subpixel calculation accidently disabled in ß3.
2022-01-28,  Version 2.0.0ß3,  Fixed artifical stars  maintaining the same total flux when out of focus.
2022-01-28,  Version 2.0.0ß2,  Fixed noise ascom sky_simulator driver.
2022-01-27,  Version 2.0.0ß1,  Artificial images have now 16 bit resolution! Flux and SNR are now depending on exposure duration. 
2022-01-09,  Version 1.0.0,  Memory server fixed. Some small improvements. Changed the Alpaca driver names.
2021-12-31,  Version 1.0.0RC4,  Added a fov calculator.
2021-12-14,  Version 1.0.0RC3,  Improved mount simulation in Alpaca. Mount sync can be far off.
2021-12-14,  Version 1.0.0RC2,  Added rotator. Added  log. Improved program.
2021-12-12,  Version 1.0.0RC1 Release candidate 1. Recompiled with the program window on the main monitor. If you can not see the program window, delete the config file at  %LOCALAPPDATA%\sky_simulator
2021-12-06,  Version 1.0.0RC1 Release candidate 1.  Binary transport added for Alpaca. Image transfer will now be very fast. If an ASCOM server is used, ASCOM v6.5SP2 is required.
2021-12-06,  Version 0.9.03. Bugfix removed a piece of code forcing longitude internally at 20 degrees east. Used for polar error only.
2021-11-03,  Version 0.9.02b. Added auto scroll bars if window is made smaller.
2021-10-05,  Version 0.9.02. Fixed some server problems.
2021-10-05,  Version 0.9.01. Fixed some problems. Release of the first Linux and MacOS versions using Alpaca protocol only.
2021-10-05,  Version 0.9.00. Added Alpa interface. Reorganised the menus. Added polar alignment error. Added Linux version.
2021-08-26,  Version 0.1.28. Replaced the colour star database by a monochrome version. This will allow better recognition of star saturation in the final monochrome camera image. Improved the star shape for close double stars.
2021-08-26,  Version 0.1.27. Some cosmetic improvements.
2021-04-19,  Version 0.1.26. Fixed a last minute bug of 0.1.25
2021-04-17,  Version 0.1.25. Added two more options for mount error. 
2021-02-12,  Version 0.1.24. Improved backlash indication.
2021-01-29,  Version 0.1.23. Added backlash for focuser.
2020-10-28,  Version 0.1.22. Fixed flipped deepsky annotation. Improved error message download failure.
2020-10-27,  Version 0.1.21. Creates much faster the artificial defocused images.
2020-10-26,  Version 0.1.20. Modified/improved the focus simulator.
2020-10-26,  Version 0.1.19. The artificial star positions will be correct on sub pixel level. This works a little better for PHD2. De-focusing of artificial stars will follow a hyperbolic curve.
2020-10-22,  Version 0.1.18. Cosmetic changes in the interface.
2020-10-05,  Version 0.1.17. Fixed sluggish behavior. Improved artificial stars. Added magnitude limit for artificial images.
2020-09-16,  Version 0.1.16a. Fixed a problem if no image.png file is found while setting the properties of the camera simulator driver "Sky simulator"
2020-08-26,  Version 0.1.16. Fixed major problem in the Ascom camera driver, selecting a sub section. Changed in Ascom driver the explorer default to image.png
2020-08-03,  Version 0.1.15. Modified output to .PNG for better image quality. PLEASE UPDATE IMAGE FILE IN CAMERA SIMULATOR TO PNG!  Interim  images are kept in memory for speed. Camera simulator (sky simulator) recompiled and fixed maximum binning at 1x1. Forced also monochrome.
2020-07-16,  Version 0.1.14. Added an option for guiding simulation. E.g. with PHD2
2020-04-25,  Version 0.1.13. Fixed image pixel scale indication for artifical images.
2020-03-04,  Version 0.1.12. Added mount error based on slew time
2020-03-04,  Version 0.1.11. Introduced mount error. Improved the speed and reorganised the menu.
2020-03-02,  Version 0.1.10. Fixed a major problem (never ending slewing) in case the mount is communicating in J2000.
2020-02-02,  Version 0.1.9. Fixed go default and default documents path.
2020-02-02,  Version 0.1.8. The images dimensions are now set at one place in the Sky Simulator program. The ASCOM camera simulator "Sky Simulator" uses the image file dimensions to set the size. 
2020-01-18,  Version 0.1.7, Expanded the star database from magnitude 13 to magnitude 14 (g14). An other star database can now be selected like g16, g17 from www.hnsky.org.
2019-11-19,  Version 0.1.6, Improved artificial stars. Set jpeg compression quality to 100%.
2019-11-17,  Version 0.1.5, Made focus range adjustable. Fixed bug in ESO routine. Some cosmetic changes.
2019-11-15,  Version 0.1.4, Added an option to switch off labels in the artificial sky
2019-11-15,  Version 0.1.3b, Swapped width and height in the menu.
2019-10-18,  Version 0.1.3, Fixed some minor things.
2019-10-15,  Version 0.1.2, Fixed a resize problem artifical image.
2019-10-15,  Version 0.1.1, Several small improvements.
2019-10-14,  Version 0.1.0, Added artificial sky with mag 13 star database and 30.000 deepsky object.
2019-10-12,  Version 0.0.5,  ASCOM simulator is now seperate. Please select "Sky Simulator" as camera in your imaging program. The 64 bit program is now installed C:\Program Files (x86) due to the 32 bit ASCOM driver. Please uninstall your old version first.
2019-10-10,  Version 0.0.4,  Added an adjustable path for the processed image.jpg.
2019-10-09,  Version 0.0.3,  Moved image to a second tab.
2019-10-09,  Version 0.0.2,  Some small improvements.
2019-10-08,  Version 0.0.1,  Fixed a decimal seperator problem for regions using the comma.
2019-10-07,  Version 0.0.0,  Initial release.
