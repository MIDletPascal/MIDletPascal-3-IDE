[Setup]
AppCopyright=Copyright by MIDletPascal project
AppName=MIDletPascal 3.3 BETA
AppVerName=MIDletPascal 3.3 BETA
DefaultDirName={pf}\MIDletPascal 3.3 BETA
DefaultGroupName=MIDletPascal 3.3 BETA
ShowLanguageDialog=yes
AppID={{03CC75C8-4327-46F0-A248-C859D063DCE6}
UninstallDisplayIcon={app}\mp3IDE.exe
OutputDir=dist
OutputBaseFilename=mp3setup
WizardImageFile=setupbigimage.bmp
WizardSmallImageFile=setupsmallimage.bmp
ShowUndisplayableLanguages=yes
UninstallIconFile=yes

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "ru"; MessagesFile: "compiler:Languages\Russian.isl"
Name: "es"; MessagesFile: "compiler:Languages\Spanish.isl"
Name: "pl"; MessagesFile: "compiler:Languages\Polish.isl"
Name: "hu"; MessagesFile: "compiler:Languages\Hungarian.isl"
Name: "fr"; MessagesFile: "compiler:Languages\French.isl"

[Files]
Source: bin\mp3IDE.exe; DestDir: {app}
Source: bin\mp3PP.dll; DestDir: {app}
Source: bin\mp3CC.exe; DestDir: {app}
Source: bin\mp3CLPM.exe; DestDir: {app}
Source: bin\mp3CLGM.exe; DestDir: {app}
Source: bin\mpsrc.ico; DestDir: {app}
Source: bin\mpproj.ico; DestDir: {app}
Source: bin\mpgroup.ico; DestDir: {app}
Source: bin\HISTORY.txt; DestDir: {app}
Source: DEPENDENCIES.html; DestDir: {app}; DestName: mp3IDE-DEPENDENCIES.html
Source: LICENSE.txt; DestDir: {app}; DestName: mp3IDE-LICENSE.txt
Source: bin\LICENSE.txt; DestDir: {app}
Source: bin\README.txt; DestDir: {app}; Flags: isreadme
Source: bin\Stubs\*.class; DestDir: {app}\Stubs\
Source: default.po; DestDir: {app}\Locale\
Source: bin\Locale\*; DestDir: {app}\Locale\; Flags: recursesubdirs createallsubdirs
Source: bin\Help\*; DestDir: {app}\Help\; Flags: recursesubdirs createallsubdirs
Source: bin\Libs\Lib_rsenum.class; DestDir: {app}\Libs\
Source: bin\Libs\Lib_rsenum.java; DestDir: {app}\Libs\
Source: bin\Demos\*; DestDir: {commondocs}\MIDletPascal\Demos\; Flags: recursesubdirs createallsubdirs
Source: bin\Styles\*.ces; DestDir: {commondocs}\MIDletPascal\Styles\
Source: bin\Skins\*.ini; DestDir: {commondocs}\MIDletPascal\Skins\; Flags: recursesubdirs createallsubdirs

[Icons]
Name: {group}\MIDletPascal 3.3 BETA; Filename: {app}\mp3IDE.exe; WorkingDir: {app}; IconFilename: {app}\mp3IDE.exe; IconIndex: 0
Name: {group}\MIDletPascal 3.3 BETA Demos; Filename: {commondocs}\MIDletPascal\Demos\
Name: {group}\MIDletPascal 3.3 BETA Help; Filename: {app}\Help\en\Index.htm
Name: {group}\MIDletPascal 3.3 BETA Readme; Filename: {app}\README.txt
Name: {group}\MIDletPascal 3.3 BETA History; Filename: {app}\HISTORY.txt
Name: {group}\MIDletPascal 3.3 BETA License; Filename: {app}\LICENSE.txt

[Registry]
Root: HKCR; Subkey: ".mpsrc"; ValueType: "string"; ValueData: "MidletPascal.Source"
Root: HKCR; Subkey: ".mpproj"; ValueType: "string"; ValueData: "MidletPascal.Project"
Root: HKCR; Subkey: ".mpgroup"; ValueType: "string"; ValueData: "MidletPascal.Group"
Root: HKCR; Subkey: "MidletPascal.Group"; ValueType: "string"; ValueData: "MIDletPascal group file"
Root: HKCR; Subkey: "MidletPascal.Group\DefaultIcon"; ValueType: "string"; ValueData: "{app}\mpgroup.ico"
Root: HKCR; Subkey: "MidletPascal.Group\Shell"; ValueType: "string"; ValueData: "Open"
Root: HKCR; Subkey: "MidletPascal.Group\Shell\Open\command"; ValueType: "string"; ValueData: """{app}\mp3IDE.exe"" ""%1"""
Root: HKCR; Subkey: "MidletPascal.Project"; ValueType: "string"; ValueData: "MIDletPascal project file"
Root: HKCR; Subkey: "MidletPascal.Project\DefaultIcon"; ValueType: "string"; ValueData: "{app}\mpproj.ico"
Root: HKCR; Subkey: "MidletPascal.Project\Shell"; ValueType: "string"; ValueData: "Open"
Root: HKCR; Subkey: "MidletPascal.Project\Shell\Open\command"; ValueType: "string"; ValueData: """{app}\mp3IDE.exe"" ""%1"""
Root: HKCR; Subkey: "MidletPascal.Source"; ValueType: "string"; ValueData: "MIDletPascal source file"
Root: HKCR; Subkey: "MidletPascal.Source\DefaultIcon"; ValueType: "string"; ValueData: "{app}\mpsrc.ico"
Root: HKCR; Subkey: "MidletPascal.Source\Shell"; ValueType: "string"; ValueData: "Open"
Root: HKCR; Subkey: "MidletPascal.Source\Shell\Open\command"; ValueType: "string"; ValueData: """{app}\mp3IDE.exe"" ""%1"""







