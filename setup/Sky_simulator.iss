
[Setup]
AppName=Sky Simulator

AppVerName=Sky Simulator 3.0.7d

;Keep this always the same for identification. This id is not shown to users
AppId=Sky_Simulator


AppPublisher=Han Kleijn
AppPublisherURL=http://www.hnsky.org
AppSupportURL=http://www.hnsky.org
AppUpdatesURL=http://www.hnsky.org

DefaultDirName={pf}\Sky Simulator
UsePreviousAppDir=yes
; Setup will look in the registry to see if the same application is already installed, and if so, it will use the directory of the previous installation as the default directory presented to the user in the wizard
DefaultGroupName=Sky_simulator
AllowNoIcons=yes

; InfoAfterFile=C:\astap.fpc\astap_install.txt

; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).



[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"; MinVersion: 4,4

[Dirs]  


[InstallDelete]

 
[Files]
Source: "c:\sky_simulator.fpc\sky_simulator.exe"; DestDir: "{app}"; DestName: "sky_simulator.exe";  Flags: ignoreversion
Source: "c:\sky_simulator.fpc\g14*.290"; DestDir: "{app}"; Flags: ignoreversion
Source: "c:\sky_simulator.fpc\deep_sky.csv"; DestDir: "{app}"; Flags: ignoreversion


; Source: "C:\sky_simulator.fpc\ASCOM.Sky_Simulator.Camera.dll"; DestDir: "{cf}\ASCOM\Camera"; Flags: ignoreversion

Source: "C:\sky_simulator.fpc\sky simulator readme.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion
Source: "C:\sky_simulator.fpc\acknowledgement of databases.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion
Source: "C:\sky_simulator.fpc\copyright.txt"; DestDir: "{app}"; Flags: isreadme ignoreversion



[Icons]
Name: "{group}\Sky simulator"; Filename: "{app}\sky_simulator.exe";WorkingDir: "{app}"
; Note Han working dir added"
Name: "{userdesktop}\Sky_simulator"; Filename: "{app}\sky_simulator.exe"; MinVersion: 4,4; Tasks: desktopicon;WorkingDir: "{app}"
; Note Han working dir added"


[Run]
Filename: "{app}\sky_simulator.exe"; Description: "Launch the Sky simulator"; Flags: nowait postinstall skipifsilent



