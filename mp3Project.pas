(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Project;

interface

uses
  Windows, ShellAPI,
  SysUtils, Classes,
  mp3SourceFiles, mp3ResourceFiles,
  mp3BuildConfigurations, mp3MIDletInfo,
  mp3Consts;

type
  Tmp3Project = class
  private
    FFilename: string;
    FJavaDescriptorFilename: string;
    FJavaArchiveFilename: string;
    FIsNew: boolean;
    FLoaded: boolean;
    FProjectDirectory: string;
    FClassesDirectory: string;
    FLibrariesDirectory: string;
    FOutputDirectory: string;
    FSourceDirectory: string;
    FResourceDirectory: string;
    FHistoryDirectory: string;
    FScriptsDirectory: string;
    FMidletInfo: Tmp3MIDletInfo;
    FSourceFiles: Tmp3SourceFiles;
    FResourceFiles: Tmp3ResourceFiles;
    FBuildConfigurations: Tmp3BuildConfigurations;
    FMaxBackUps: integer;
    procedure Init(AFilename: string);
    function ExpandJavaFiles(AString: string): string;
    function BuildBackupNameForVersion(ASourceFile: Tmp3SourceFile; AVersion: integer): string;
    procedure PurgeBackUpsCountFor(ASourceFile: Tmp3SourceFile; ATop: integer);
    function GetNextBackUpVersionFor(ASourceFile: Tmp3SourceFile): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function EnsureDirectories: boolean;
    function GetSourceFileContentFromBackupVersion(ASourceFile: Tmp3SourceFile; AVersion: integer): string;
    procedure RollbackSourceFileToBackupVersion(ASourceFile: Tmp3SourceFile; AVersion: integer);
    procedure GenerateNewBackupFor(ASourceFile: Tmp3SourceFile);
    function GetBackUpsCountFor(ASourceFile: Tmp3SourceFile): integer;
    procedure RetrieveBackUpsForFile(ASourceFile: Tmp3SourceFile; var AFiles: TStringList);
    function GetActiveConfigurationName: string;
    function GetClassFileFor(ASourceFile: Tmp3SourceFile): string;
    function FileBelongsToProject(AFilename: string): boolean;
    procedure CleanClassesDirectory;
    procedure Close;
    function New(AFilename: string): boolean;
    function Load(AFilename: string): boolean;
    function Save: boolean;
    function ExpandRunCommandLine(ACommandLine: string = ''): string;
    procedure RunScript(AScriptName: string);
    function ScriptExists(AScriptName: string): boolean;
    property IsNew: boolean read FIsNew;
    property Loaded: boolean read FLoaded;
    property MaxBackups: integer read FMaxBackUps write FMaxBackUps;
    property ProjectDirectory: string read FProjectDirectory;
    property ClassesDirectory: string read FClassesDirectory;
    property LibrariesDirectory: string read FLibrariesDirectory;
    property OutputDirectory: string read FOutputDirectory;
    property SourceDirectory: string read FSourceDirectory;
    property ResourceDirectory: string read FResourceDirectory;
    property HistoryDirectory: string read FHistoryDirectory;
    property ScriptsDirectory: string read FScriptsDirectory;
    property Filename: string read FFilename;
    property JavaDescriptorFilename: string read FJavaDescriptorFilename;
    property JavaArchiveFilename: string read FJavaArchiveFilename;
    property MidletInfo: Tmp3MIDletInfo read FMidletInfo;
    property SourceFiles: Tmp3SourceFiles read FSourceFiles;
    property ResourceFiles: Tmp3ResourceFiles read FResourceFiles;
    property BuildConfigurations: Tmp3BuildConfigurations read FBuildConfigurations;
  end;

implementation

uses
  mp3ProjectPersistence;

{ Tmp3Project }

constructor Tmp3Project.Create;
begin
  FIsNew := true;
  FLoaded := false;
  FMidletInfo := Tmp3MIDletInfo.Create;
  FSourceFiles := Tmp3SourceFiles.Create;
  FResourceFiles := Tmp3ResourceFiles.Create;
  FBuildConfigurations := Tmp3BuildConfigurations.Create;
  FMaxBackUps := BACKUPS_UNLIMITED;
end;

destructor Tmp3Project.Destroy;
begin
  FreeAndNil(FMidletInfo);
  FreeAndNil(FResourceFiles);
  FreeAndNil(FSourceFiles);
  FreeAndNil(FBuildConfigurations);
  inherited;
end;

function Tmp3Project.GetActiveConfigurationName: string;
begin
  result := BuildConfigurations[BuildConfigurations.ActiveConfigurationIndex].Name;
end;

function Tmp3Project.BuildBackupNameForVersion(ASourceFile: Tmp3SourceFile;
  AVersion: integer): string;
begin
  result := FHistoryDirectory + ExtractFileName(ASourceFile.GetFullName)
    + '.~'+IntToStr(AVersion)+'~';
end;

procedure Tmp3Project.GenerateNewBackupFor(ASourceFile: Tmp3SourceFile);
var x: integer;
begin
  if FMaxBackUps = BACKUPS_DISABLED then
    exit;
  x := GetNextBackUpVersionFor(ASourceFile);
  // j-a-s-d: will backup only if history exists, that means only new projects
  CopyFile(
    pchar(ASourceFile.GetFullname),
    pchar(BuildBackupNameForVersion(ASourceFile,x)),
    true
  );
  if x > FMaxBackUps then
    PurgeBackUpsCountFor(ASourceFile,x);
end;

function Tmp3Project.GetBackUpsCountFor(ASourceFile: Tmp3SourceFile): integer;
var SR: TSearchRec; x: integer;
begin
  result := 0;
  x := FindFirst(FHistoryDirectory+ASourceFile.Filename+'.*', 0, SR);
  if x = 0 then repeat
    inc(result);
    x := FindNext(SR);
  until x <> 0;
end;

procedure Tmp3Project.RetrieveBackUpsForFile(ASourceFile: Tmp3SourceFile; var AFiles: TStringList);
var SR: TSearchRec;
begin
  if assigned(AFiles) then begin
    AFiles.Clear;
    if not DirectoryExists(FHistoryDirectory) then
      exit;
    if FindFirst(FHistoryDirectory+ExtractFileName(ASourceFile.Filename)+'.~*~', 0, SR) = 0 then
    try
      repeat
        if (SR.Attr and faDirectory = 0) then
          AFiles.Insert(0,FHistoryDirectory + SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  end;
end;

function Tmp3Project.GetSourceFileContentFromBackupVersion(
  ASourceFile: Tmp3SourceFile; AVersion: integer): string;
var fs: TFileStream;
begin
  result := '';
  fs := TFileStream.Create(
    BuildBackupNameForVersion(ASourceFile,AVersion),
    fmOpenRead or fmShareDenyWrite
  );
  try
    SetLength(result, fs.Size);
    fs.Read(result[1], fs.Size);
  finally
    fs.Free;
  end;
end;

procedure Tmp3Project.RollbackSourceFileToBackupVersion(
  ASourceFile: Tmp3SourceFile; AVersion: integer);
var st: TStringList; i,x: integer; s,t: string;
begin
  // overwrite current version
  s := BuildBackupNameForVersion(ASourceFile, AVersion);
  if not FileExists(s) then
    exit;
  t := ASourceFile.GetFullName;
  if not DeleteFile(t) then
    exit;
  CopyFile(pchar(s),pchar(t),false);
  // adjust history
  st := TStringList.Create;
  try
    RetrieveBackUpsForFile(ASourceFile, st);
    if st.Count > 0 then begin
      // preserve older versions
      for i := 0 to AVersion - 1 do
      begin
        x := st.IndexOf(BuildBackupNameForVersion(ASourceFile, i));
        if x > -1 then
          st.Delete(x);
      end;
      // delete newer versions
      for i := st.Count - 1 downto 0 do begin
        DeleteFile(st[i]);
        st.Delete(i);
      end;
    end;
  finally
    st.Free;
  end;
end;

function Tmp3Project.GetClassFileFor(ASourceFile: Tmp3SourceFile): string;
begin
  if ASourceFile = FSourceFiles.ProgramFile then
    result := FClassesDirectory + DEFAULT_PROGRAM_CLASS
  else
    result := FClassesDirectory + LowerCase(ChangeFileExt(ASourceFile.GetUnitName,EXTENSION_CLASS));
end;

function Tmp3Project.GetNextBackUpVersionFor(ASourceFile: Tmp3SourceFile): integer;
var SR: TSearchRec; x: integer; st: TStringList; s: string;
begin
  result := -1;
  st := TStringList.Create;
  st.Sorted := false;
  try
    x := FindFirst(FHistoryDirectory+ASourceFile.Filename+'.*', 0, SR);
    if x = 0 then repeat
      s := StringReplace(ExtractFileExt(SR.Name),'~','',[rfReplaceAll]);
      Delete(s,1,1);
      while Length(s)<4 do
        s := '0'+s;
      st.Add(s);
      x := FindNext(SR);
    until x <> 0;
    st.Sort;
    for x := st.Count - 1 downto 0 do begin
      result := StrToIntDef(st[x],-1);
      if result > -1 then
        break;
    end;
  finally
    st.Free;
  end;
  inc(result);
end;

procedure Tmp3Project.Init(AFilename: string);
begin
  Close;
  FFilename := AFilename;
  FProjectDirectory := ExtractFilePath(AFilename);
  if FProjectDirectory = '' then
    FProjectDirectory := '.\';
  FOutputDirectory := FProjectDirectory + DIR_OUTPUT + '\';
  FJavaDescriptorFilename := FOutputDirectory +
    ChangeFileExt(ExtractFileName(AFilename),EXTENSION_JAD);
  FJavaArchiveFilename := FOutputDirectory +
    ChangeFileExt(ExtractFileName(AFilename),EXTENSION_JAR);
  FClassesDirectory := FProjectDirectory + DIR_CLASSES + '\';
  FLibrariesDirectory := FProjectDirectory + DIR_LIBS + '\';
  FResourceDirectory := FProjectDirectory + DIR_RESOURCES + '\';
  FSourceDirectory := FProjectDirectory + DIR_SOURCES + '\';
  FHistoryDirectory := FProjectDirectory + DIR_HISTORY + '\';
  FScriptsDirectory := FProjectDirectory + DIR_SCRIPTS + '\';
  FSourceFiles.Directory := FSourceDirectory;
  FResourceFiles.Directory := FResourceDirectory;
end;

procedure Tmp3Project.CleanClassesDirectory;
var SR : TSearchRec; x: integer;
begin
  x := FindFirst(FClassesDirectory+'*.*', 0, SR);
  if x = 0 then repeat
    DeleteFile(FClassesDirectory+SR.Name);
    x := FindNext(SR);
  until x <> 0;
  FindClose(SR);
end;

procedure Tmp3Project.Close;
begin
  FIsNew := false;
  FLoaded := false;
  FFilename := '';
  FProjectDirectory := '';
  FOutputDirectory := '';
  FJavaDescriptorFilename := '';
  FJavaArchiveFilename := '';
  FClassesDirectory := '';
  FLibrariesDirectory := '';
  FResourceDirectory := '';
  FSourceDirectory := '';
  FHistoryDirectory := '';
  FScriptsDirectory := '';
  FMidletInfo.Clear;
  FSourceFiles.Clear;
  FResourceFiles.Clear;
  FBuildConfigurations.Clear;
end;

function Tmp3Project.Load(AFilename: string): boolean;
begin
  Init(AFilename);
  FLoaded := FileExists(FFilename)
    and (ReadProject(Self) or ImportMP1Project(Self)) and EnsureDirectories;
  result := FLoaded;
end;

function Tmp3Project.EnsureDirectories: boolean;
begin
  try
    if not DirectoryExists(FOutputDirectory) then
      ForceDirectories(FOutputDirectory);
    if not DirectoryExists(FClassesDirectory) then
      ForceDirectories(FClassesDirectory);
    if not DirectoryExists(FLibrariesDirectory) then
      ForceDirectories(FLibrariesDirectory);
    if not DirectoryExists(FResourceDirectory) then
      ForceDirectories(FResourceDirectory);
    if not DirectoryExists(FSourceDirectory) then
      ForceDirectories(FSourceDirectory);
    if not DirectoryExists(FHistoryDirectory) then
      ForceDirectories(FHistoryDirectory);
    if not DirectoryExists(FScriptsDirectory) then
      ForceDirectories(FScriptsDirectory);
    result := DirectoryExists(FOutputDirectory) and
      DirectoryExists(FClassesDirectory) and
      DirectoryExists(FLibrariesDirectory) and
      DirectoryExists(FResourceDirectory) and
      DirectoryExists(FHistoryDirectory) and
      DirectoryExists(FScriptsDirectory) and
      DirectoryExists(FSourceDirectory);
  except
    result := false;
  end;
end;

function Tmp3Project.New(AFilename: string): boolean;
begin
  Init(ChangeFileExt(AFilename, EXTENSION_PROJECT));
  if not EnsureDirectories then
  begin
    result := false;
    exit;
  end;
  // fill default data
  FMidletInfo.Name := ChangeFileExt(ExtractFileName(AFilename),'');
  FMidletInfo.Vendor := DEFAULT_VENDOR;
  FMidletInfo.Version := DEFAULT_VERSION;
  FMidletInfo.Icon := DEFAULT_ICON;
  if not FSourceFiles.New(ChangeFileExt(FMidletInfo.Name, EXTENSION_SOURCEFILE)) then
  begin
    result := false;
    exit;
  end;
  with TStringList.Create do
  try
    Text := Format(DEFAULT_PROGRAM_CODE,[FMidletInfo.Name]);
    SaveToFile(FSourceFiles.SourceFile[0].GetFullname);
  finally
    Free;
  end;
  FResourceFiles.Add(DEFAULT_ICON_NAME, DEFAULT_ICON_CONFIGS);
  with TFileStream.Create(FResourceDirectory+DEFAULT_ICON_NAME,fmCreate) do
  try
    Write(DEFAULT_ICON_CONTENT,SizeOf(DEFAULT_ICON_CONTENT));
  finally
    Free;
  end;
  FBuildConfigurations.Add(DEFAULT_CONFIGURATION_NAME,
    mtClassicMidlet, mvMIDP10, rnFixedPoint);
  result := Save;
end;

procedure Tmp3Project.PurgeBackUpsCountFor(ASourceFile: Tmp3SourceFile;
  ATop: integer);
var SR: TSearchRec; filesToKeep: TStringList; limit,x: integer;
begin
  limit := ATop - FMaxBackups;
  if (not DirectoryExists(FHistoryDirectory)) or
    (FMaxBackUps=BACKUPS_UNLIMITED) or (limit<1) then
    exit;
  filesToKeep := TStringList.Create;
  try
    // determine files to keep
    for x := ATop downto limit do
      filesToKeep.Add(BuildBackupNameForVersion(ASourceFile,x));
    // drop backups outside the limits
    if FindFirst(FHistoryDirectory+ExtractFileName(ASourceFile.Filename)+'.~*~', 0, SR) = 0 then
    try
      repeat
        if (SR.Attr and faDirectory = 0) then
          if filesToKeep.IndexOf(FHistoryDirectory + SR.Name) = -1 then
            DeleteFile(FHistoryDirectory + SR.Name);
      until FindNext(SR) <> 0;
    finally
      FindClose(SR);
    end;
  finally
    filesToKeep.Free;
  end;
end;

function Tmp3Project.ExpandJavaFiles(AString: string): string;
begin
  result := AString;
  // jar
  if Pos('"'+PATTERN_JAR+'"',result)>0 then
    result := StringReplace(result,
      '"'+PATTERN_JAR+'"', JavaArchiveFilename, [rfReplaceAll])
  else
    if Pos(PATTERN_JAR,result)>0 then
      result := StringReplace(result,
        PATTERN_JAR, '"'+JavaArchiveFilename+'"', [rfReplaceAll]);
  // jad
  if Pos('"'+PATTERN_JAD+'"',result)>0 then
    result := StringReplace(result,
      '"'+PATTERN_JAD+'"', JavaDescriptorFilename, [rfReplaceAll])
  else
    if Pos(PATTERN_JAD,result)>0 then
      result := StringReplace(result,
        PATTERN_JAD, '"'+JavaDescriptorFilename+'"', [rfReplaceAll]);
end;

function Tmp3Project.FileBelongsToProject(AFilename: string): boolean;
begin
  result := assigned(SourceFiles.GetSourceFileByName(AFilename))
    or assigned(ResourceFiles.GetResourceFileByName(AFilename));
end;

function Tmp3Project.ExpandRunCommandLine(ACommandLine: string = ''): string;
begin
  if ACommandLine = '' then
    result := FJavaDescriptorFilename
  else
    result := ExpandJavaFiles(ACommandLine);
end;

procedure Tmp3Project.RunScript(AScriptName: string);
var sf: string;
begin
  sf := FScriptsDirectory+AScriptName+EXTENSION_BAT;
  if FileExists(sf) then
    ShellExecute(0,'open',PChar(sf),'','',1);
end;

function Tmp3Project.ScriptExists(AScriptName: string): boolean;
begin
  result := FileExists(FScriptsDirectory+AScriptName+EXTENSION_BAT);
end;

function Tmp3Project.Save: boolean;
begin
  result := WriteProject(Self);
end;

end.
