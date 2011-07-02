(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Settings;

interface

uses
  Windows, ShlObj, ActiveX,
  Forms, Classes, SysUtils, IniFiles,
  mp3Consts,
  sitSynCodeEditorStylesPas, sitMRUIni, sitOSUtils;

type
  Tmp3Settings = class
  private
    FIsPortable: boolean;
    FAppPath: string;
    FConfigPath: string;
    FIniFile: TIniFile;
    FCodeEditorStyles: TsitSynCodeEditorStylesPas;
    FEmulators: TStringList;
    FCurrentEmulatorName: string;
    FDefaultProjectLocation: string;
    FCurrentSkin: string;
    FLibrariesDirectory: string;
    FStubsDirectory: string;
    FSideBar: string;
    FWelcomePage: boolean;
    FGroupManager: boolean;
    FProjectManager: boolean;
    FGroupManagerPosition: string;
    FProjectManagerPosition: string;
    FLeftDockWidth: integer;
    FRightDockWidth: integer;
    FFullScreen: boolean;
    FMinimizeToTray: boolean;
    FMaxBackups: integer;
    FLanguage: string;
    FCodeEditorStyle: string;
    FCodeEditorFontName: string;
    FCodeEditorFontSize: integer;
    FRecents: TsitMRUIni;
    function GetConfigurationFile: string;
    procedure InitConfigPath;
    function GetHelpFile: string;
    function GetCurrentEmulatorCommandLine: string;
    function GetCodeEditorStyles: TsitSynCodeEditorStylesPas;
    function NormalizeSideBarValue(AValue: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property AppPath: string read FAppPath;
    property ConfigPath: string read FConfigPath;
    property ConfigurationFile: string read GetConfigurationFile;
    property HelpFile: string read GetHelpFile;
    property Emulators: TStringList read FEmulators;
    property CurrentEmulatorName: string read FCurrentEmulatorName write FCurrentEmulatorName;
    property DefaultProjectLocation: string read FDefaultProjectLocation write FDefaultProjectLocation;
    property CurrentSkin: string read FCurrentSkin write FCurrentSkin;
    property CurrentEmulatorCommandLine: string read GetCurrentEmulatorCommandLine;
    property LibrariesDirectory: string read FLibrariesDirectory;
    property StubsDirectory: string read FStubsDirectory;
    property Language: string read FLanguage write FLanguage;
    property CodeEditorStyles: TsitSynCodeEditorStylesPas read GetCodeEditorStyles;
    property CodeEditorStyle: string read FCodeEditorStyle write FCodeEditorStyle;
    property CodeEditorFontName: string read FCodeEditorFontName write FCodeEditorFontName;
    property CodeEditorFontSize: integer read FCodeEditorFontSize write FCodeEditorFontSize;
    property SideBar: string read FSideBar write FSideBar;
    property WelcomePage: boolean read FWelcomePage write FWelcomePage;
    property GroupManager: boolean read FGroupManager write FGroupManager;
    property ProjectManager: boolean read FProjectManager write FProjectManager;
    property GroupManagerPosition: string read FGroupManagerPosition write FGroupManagerPosition;
    property ProjectManagerPosition: string read FProjectManagerPosition write FProjectManagerPosition;
    property LeftDockWidth: integer read FLeftDockWidth write FLeftDockWidth;
    property RightDockWidth: integer read FRightDockWidth write FRightDockWidth;
    property FullScreen: boolean read FFullScreen write FFullScreen;
    property MinimizeToTray: boolean read FMinimizeToTray write FMinimizeToTray;
    property MaxBackups: integer read FMaxBackups write FMaxBackups;
    property Recents: TsitMRUIni read FRecents;
  end;

var
  gSettings: Tmp3Settings;

implementation

{ Tmp3Settings }

procedure Tmp3Settings.InitConfigPath;

  function GetSpecialFolder(const CSIDL: integer): string;
  var path: pchar; pPIDL: PItemIDList; pMalloc: IMalloc;
  begin
    GetMem(path,MAX_PATH*sizeOf(char));
    try
      if Succeeded(SHGetSpecialFolderLocation(0, CSIDL, pPIDL)) then begin
        SHGetPathFromIDList(pPIDL, path);
        if Succeeded(SHGetMalloc(pMalloc)) then
          pMalloc.Free(pPIDL);
      end else
        StrCopy(path, '');
      result := string(path);
    finally
      FreeMem(path);
    end;
  end;

const CSIDL_COMMON_DOCUMENTS = $002E; //All Users\Documents
var s: string;
begin
  FIsPortable := FileExists(PORTABLE_FILENAME) or IsUnderWine;
  if FIsPortable then
    FConfigPath := FAppPath
  else begin
    s := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_COMMON_DOCUMENTS));
    if s = '\' then // 9x/ME = MyDocuments
      s := IncludeTrailingPathDelimiter(GetSpecialFolder(CSIDL_PERSONAL));
    FConfigPath := s+PROJECT_NAME+'\';
    ForceDirectories(FConfigPath);
  end;
end;

constructor Tmp3Settings.Create;
begin
  FAppPath := ExtractFilePath(Application.Exename);
  InitConfigPath;
  FIniFile := TIniFile.Create(ConfigurationFile);
  FCodeEditorStyles := TsitSynCodeEditorStylesPas.Create(FConfigPath+STYLES_DIR+'\', EXTENSION_CES);
  FEmulators := TStringList.Create;
  FLibrariesDirectory := ExtractFilePath(Application.Exename)+LIBS_DIR+'\';
  FStubsDirectory := ExtractFilePath(Application.Exename)+STUBS_DIR+'\';
  FRecents := TsitMRUIni.Create;
  FRecents.Filename := FConfigPath+RECENTS_FILENAME;
end;

destructor Tmp3Settings.Destroy;
begin
  FreeAndNil(FIniFile);
  FreeAndNil(FCodeEditorStyles);
  FreeAndNil(FEmulators);
  FreeAndNil(FRecents);
  inherited;
end;

function Tmp3Settings.GetCodeEditorStyles: TsitSynCodeEditorStylesPas;
begin
  result := FCodeEditorStyles;
end;

function Tmp3Settings.GetConfigurationFile: string;
begin
  result := FConfigPath+ChangeFileExt(ExtractFileName(Application.Exename),EXTENSION_INI)
end;

function Tmp3Settings.GetCurrentEmulatorCommandLine: string;
begin
  result := FEmulators.Values[FCurrentEmulatorName];
end;

function Tmp3Settings.GetHelpFile: string;
begin
  result := FAppPath+Format(HELP_FILE,[FLanguage]);
  if not FileExists(result) then
    result := FAppPath+Format(HELP_FILE,['en']);
end;

function Tmp3Settings.NormalizeSideBarValue(AValue: string): string;
begin
  result := AValue;
  if (result <> SIDEBAR_LEFT) and (result <> SIDEBAR_RIGHT) and (result <> SIDEBAR_HIDDEN) then
    result := CONFIG_IDE_SECTION_SIDEBAR_DEFAULT;
end;

procedure Tmp3Settings.Load;
begin
  FCurrentSkin := FIniFile.ReadString(CONFIG_IDE_SECTION,CONFIG_IDE_SECTION_SKIN,DEFAULT_SKIN);
  FWelcomePage := StrToBool(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_WELCOMEPAGE,CONFIG_IDE_SECTION_WELCOMEPAGE_DEFAULT));
  FSideBar := NormalizeSideBarValue(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_SIDEBAR,CONFIG_IDE_SECTION_SIDEBAR_DEFAULT));
  FGroupManager := StrToBool(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_GROUPMANAGER,CONFIG_IDE_SECTION_GROUPMANAGER_DEFAULT));
  FProjectManager := StrToBool(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_PROJECTMANAGER,CONFIG_IDE_SECTION_PROJECTMANAGER_DEFAULT));
  FGroupManagerPosition := FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_GROUPMANAGER_POSITION,CONFIG_IDE_SECTION_GROUPMANAGER_POSITION_DEFAULT);
  FProjectManagerPosition := FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_PROJECTMANAGER_POSITION,CONFIG_IDE_SECTION_PROJECTMANAGER_POSITION_DEFAULT);
  FLeftDockWidth := StrToInt(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_LEFTDOCK_WIDTH,CONFIG_IDE_SECTION_LEFTDOCK_WIDTH_DEFAULT));
  if FLeftDockWidth = 0 then
    FLeftDockWidth := StrToInt(CONFIG_IDE_SECTION_LEFTDOCK_WIDTH_DEFAULT);
  FRightDockWidth := StrToInt(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH,CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH_DEFAULT));
  if FRightDockWidth = 0 then
    FRightDockWidth := StrToInt(CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH_DEFAULT);
  FFullScreen := StrToBool(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_FULLSCREEN,CONFIG_IDE_SECTION_FULLSCREEN_DEFAULT));
  FMinimizeToTray := StrToBool(FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_MINIMIZETOTRAY,CONFIG_IDE_SECTION_MINIMIZETOTRAY_DEFAULT));
  FMaxBackups := FIniFile.ReadInteger(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_MAXBACKUPS,CONFIG_IDE_SECTION_MAXBACKUPS_DEFAULT);
  FLanguage := FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_LANGUAGE,CONFIG_IDE_SECTION_LANGUAGE_DEFAULT);
  FCodeEditorStyle := FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORSTYLE,CONFIG_IDE_SECTION_CODEEDITORSTYLE_DEFAULT);
  FCodeEditorFontName := FIniFile.ReadString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORFONTNAME,CONFIG_IDE_SECTION_CODEEDITORFONTNAME_DEFAULT);
  FCodeEditorFontSize := FIniFile.ReadInteger(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORFONTSIZE,CONFIG_IDE_SECTION_CODEEDITORFONTSIZE_DEFAULT);
  FEmulators.StrictDelimiter := True;
  FEmulators.Delimiter := ',';
  FEmulators.DelimitedText := FIniFile.ReadString(CONFIG_EMULATOR_SECTION,
    CONFIG_EMULATOR_SECTION_EMULATORS,
    CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME_DEFAULT+'='+CONFIG_EMULATOR_SECTION_COMMANDLINE_MP2_DEFAULT);
  FCurrentEmulatorName := FIniFile.ReadString(CONFIG_EMULATOR_SECTION,
    CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME,CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME_DEFAULT);
  FDefaultProjectLocation := FIniFile.ReadString(CONFIG_OPTIONS_SECTION,
    CONFIG_OPTIONS_SECTION_DEFAULTPROJECTLOCATION,DEFAULT_PROJECT_LOCATION);
end;

procedure Tmp3Settings.Save;
begin
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_SKIN,FCurrentSkin);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_WELCOMEPAGE,BoolToStr(FWelcomePage));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_SIDEBAR,NormalizeSideBarValue(FSideBar));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_GROUPMANAGER,BoolToStr(FGroupManager));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_PROJECTMANAGER,BoolToStr(FProjectManager));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_GROUPMANAGER_POSITION,FGroupManagerPosition);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_PROJECTMANAGER_POSITION,FProjectManagerPosition);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_LEFTDOCK_WIDTH,IntToStr(FLeftDockWidth));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_RIGHTDOCK_WIDTH,IntToStr(FRightDockWidth));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_FULLSCREEN,BoolToStr(FFullScreen));
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_MINIMIZETOTRAY,BoolToStr(FMinimizeToTray));
  FIniFile.WriteInteger(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_MAXBACKUPS,FMaxBackups);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_LANGUAGE,FLanguage);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORSTYLE,FCodeEditorStyle);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORFONTNAME,FCodeEditorFontName);
  FIniFile.WriteString(CONFIG_IDE_SECTION,
    CONFIG_IDE_SECTION_CODEEDITORFONTSIZE,IntToStr(FCodeEditorFontSize));
  FIniFile.WriteString(CONFIG_EMULATOR_SECTION,
    CONFIG_EMULATOR_SECTION_EMULATORS,FEmulators.DelimitedText);
  FIniFile.WriteString(CONFIG_EMULATOR_SECTION,
    CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME,FCurrentEmulatorName);
  FIniFile.WriteString(CONFIG_OPTIONS_SECTION,
    CONFIG_OPTIONS_SECTION_DEFAULTPROJECTLOCATION,FDefaultProjectLocation);
  FIniFile.UpdateFile;
end;

initialization
  gSettings := Tmp3Settings.Create;
  gSettings.Load;
finalization
  gSettings.Save;
  gSettings.Free;
end.

