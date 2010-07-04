(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3ProjectBuilding;

interface

uses
  Windows, Classes, SysUtils, StrUtils,
  AbZipper, AbUtils, AbArcTyp, AbZipTyp,
  sitCompilerInvoker, sitFileUtils,
  mp3Preprocessor, mp3Project, mp3SourceFiles,
  mp3ResourceFiles, mp3BuildConfigurations,
  mp3Consts, mp3Settings;

type
  Tmp3CompilerInvoker = class(TsitCompilerInvoker)
  protected
    class procedure BeforeCompile; override;
  public
    class procedure ShowCompilerNewLine(Sender: TObject;
      NewLine: string; IsNewLine: boolean);
  end;

  TCompilerMessageHandler = procedure(AMessage: string) of object;
  TCompilerProgressHandler = procedure(AValue: DWORD) of object;

var
  CompilerMessageHandler: TCompilerMessageHandler = nil;
  CompilerProgressHandler: TCompilerProgressHandler = nil;
  {$IFDEF WOW64_WORKAROUND}
  CompilerMessageHandlerHandle: THandle = INVALID_HANDLE_VALUE;
  {$ENDIF}

procedure ExecuteScript(AProject: Tmp3Project; AScript: string);
function PreprocessSourceFile(ASrcDirectory, AFilename, AProcessedFilename,
  ABuildConfiguration: string): boolean;
function BuildSourceFile(AProject: Tmp3Project; ASourceFile: Tmp3SourceFile;
  var AError: string): boolean;
function Build(AProject: Tmp3Project; var AError: string): boolean;
procedure StopBuilding;
function CompilerPresent: boolean;

implementation

var
// compiler interaction related fields
  stop, ErrorStop: boolean;
  next_record_ID: integer;
  current_unit_name: string;
  units: TStringList;
  stubs: TStringList;
  libs: TStringList;

procedure StopBuilding;
begin
  stop := true;
  ErrorStop := false;
end;

procedure DefaultCompilerMessageHandler(AMessage: pchar); cdecl;
begin
  if assigned(CompilerMessageHandler) then
    CompilerMessageHandler(AMessage);
end;

procedure DefaultCompilerProgressHandler(AValue: DWORD); cdecl;
begin
  if assigned(CompilerProgressHandler) then
    CompilerProgressHandler(AValue);
end;

procedure DefaultCompilerRequirementsHandler(AType: DWORD; AName: PCHAR); cdecl;
begin
  case AType of
    0: // unit
      if SameText(current_unit_name, AName) then
        DefaultCompilerMessageHandler(pchar(Format('[Pascal Hint] Unit %s tries to recursively use itself.',[AName])))
      else begin
        units.Add(AName);
        DefaultCompilerMessageHandler(pchar('  using unit '+AName));
      end;
    1: // lib
      begin
        if libs.IndexOf(AName)=-1 then
          libs.Add(AName);
        DefaultCompilerMessageHandler(pchar('  using external library '+
          EXTERNAL_LIBRARY_PREFIX+AName+EXTENSION_CLASS));
      end;
    2: // stub
      begin
        if stubs.IndexOf(AName)=-1 then
          stubs.Add(AName);
        DefaultCompilerMessageHandler(pchar('  using stub library '+AName));
      end;
    3: // record
      begin
        Inc(next_record_ID);
        DefaultCompilerMessageHandler(pchar('  using generated record '+AName));
      end;
  end;
end;

procedure ExecuteScript(AProject: Tmp3Project; AScript: string);
begin
  if AProject.ScriptExists(AScript) then begin
    DefaultCompilerMessageHandler(pchar('Executing '+AScript+'...'));
    AProject.RunScript(AScript);
  end;
end;

function CompilerPresent: boolean;
begin
  result := FileExists(gSettings.AppPath+COMPILER_EXE);
end;

function PreprocessSourceFile(ASrcDirectory, AFilename, AProcessedFilename,
  ABuildConfiguration: string): boolean;
var st: TStringList; tmp: string;
begin
  result := true;
  st := TStringList.Create;
  try
    st.Text := GetFileContent(ASrcDirectory+AFilename);
    if Tmp3Preprocessor.Present then begin
      tmp := ExtractFilePath(AProcessedFilename)+
        ExtractFileName(StringReplace(AFilename,'/','\',[rfReplaceAll]));
      st.SaveToFile(tmp);
      try
        result := Tmp3Preprocessor.Run(ASrcDirectory,tmp,AProcessedFilename,
          MPDEFINES+COMMA+ABuildConfiguration,@DefaultCompilerMessageHandler)>0;
      finally
        DeleteFile(tmp);
      end;
    end else
      st.SaveToFile(ASrcDirectory+AProcessedFilename);
  finally
    st.Free;
  end;
end;

{ Tmp3CompilerInvoker }

class procedure Tmp3CompilerInvoker.BeforeCompile;
begin
  CompatibilityLayer := 'Win95';
  NewLineHandler := ShowCompilerNewLine;
end;

class procedure Tmp3CompilerInvoker.ShowCompilerNewLine(Sender: TObject;
  NewLine: string; IsNewLine: boolean);
var x: integer;
begin
  if length(NewLine)<3 then
    exit;
  if IsNewLine and EndsText(#10,NewLine) then
    NewLine := Copy(NewLine,1,Length(NewLine)-1);
  if NewLine[1]='@' then begin
    Delete(NewLine,1,1);
    x := StrToIntDef(NewLine,-1);
    if x <> -1 then begin
      DefaultCompilerProgressHandler(x);
      exit;
    end;
  end else if NewLine[1]='^' then begin
    x := StrToInt(NewLine[2]);
    Delete(NewLine,1,2);
    DefaultCompilerRequirementsHandler(x, pchar(NewLine));
    exit;
  end;
  if StartsStr('[Pascal Error]',NewLine) then begin
    stop := true;
    ErrorStop := true;
  end;
  DefaultCompilerMessageHandler(pchar(NewLine));
end;

function InvokeCompiler(ASourceFile, AOutputDirectory, ALibDirectory: string;
    ACanvasType, AMathType: integer; ADetectUnitsOnly: boolean): DWORD;
var cmd: string;
begin
  cmd := '"'+gSettings.AppPath+COMPILER_EXE+'"'+
    ' -s"'+ASourceFile+'"'+
    ' -o"'+ExcludeTrailingPathDelimiter(AOutputDirectory)+'"'+
    ' -l"'+ExcludeTrailingPathDelimiter(ALibDirectory) +'"'+
    ' -c'+IntToStr(ACanvasType)+
    ' -m'+IntToStr(AMathType)+
    ' -r'+IntToStr(next_record_ID)
  {$IFDEF WOW64_WORKAROUND}
    +' -n'+IntToStr(CompilerMessageHandlerHandle)
  {$ENDIF}
    ;
  if ADetectUnitsOnly then
    cmd := cmd + ' -d';
  result := Tmp3CompilerInvoker.Run(cmd);
end;

function DetectUnitsOfSourceFile(AProject: Tmp3Project; ASourceFilename: string;
  var AError: string): boolean;
var ftc: string;
begin
  result := false;
  if not assigned(AProject) then
  begin
    AError := 'ERROR: the file must belong to a project';
    exit;
  end;
  units.Clear;
  current_unit_name := ChangeFileExt(ExtractFileName(ASourceFilename),'');
  if ExtractFilePath(ASourceFilename) = '' then
    ftc := AProject.SourceDirectory+ASourceFilename
  else
    ftc := ASourceFilename;
  try
    result := InvokeCompiler(
      ftc, AProject.ClassesDirectory, gSettings.LibrariesDirectory,
      DWORD(AProject.BuildConfigurations[
        AProject.BuildConfigurations.ActiveConfigurationIndex].MIDletType),
      StrToInt(RealNumbersToMP2ProjectValue(AProject.BuildConfigurations[
        AProject.BuildConfigurations.ActiveConfigurationIndex].RealNumbers)),
      TRUE
    ) = 0;
  except
    DefaultCompilerMessageHandler('ERROR: compiler internal error');
  end;
end;

function CompileSourceFile(AProject: Tmp3Project; ASourceFilename: string;
  var AError: string): boolean;
var a: integer; ftc: string;
begin
  result := false;
  if not assigned(AProject) then
  begin
    AError := 'ERROR: the file must belong to a project';
    exit;
  end;
  units.Clear;
  DefaultCompilerProgressHandler(0);
  a := 0;
  if ExtractFilePath(ASourceFilename) = '' then
    ftc := AProject.SourceDirectory+ASourceFilename
  else
    ftc := ASourceFilename;
  ftc := LowerCase(ftc); // .class files must be lowercased!
  while (not result)and(a<COMPILER_INTERNAL_ERROR_RETRIES) do
  try
    result := InvokeCompiler(
      ftc, AProject.ClassesDirectory, gSettings.LibrariesDirectory,
      DWORD(AProject.BuildConfigurations[
        AProject.BuildConfigurations.ActiveConfigurationIndex].MIDletType),
      StrToInt(RealNumbersToMP2ProjectValue(AProject.BuildConfigurations[
        AProject.BuildConfigurations.ActiveConfigurationIndex].RealNumbers)),
      FALSE
    ) = 0;
    break;
  except
    Sleep(250);
    DefaultCompilerMessageHandler('ERROR: compiler internal error');
    inc(a);
    if a < COMPILER_INTERNAL_ERROR_RETRIES then
      DefaultCompilerMessageHandler(pchar('Retry #'+IntToStr(a)));
  end;
  if result then
    DefaultCompilerProgressHandler(100);
end;

function BuildSourceFile(AProject: Tmp3Project; ASourceFile: Tmp3SourceFile;
  var AError: string): boolean;
var prp,ftc,buildDir: string;
begin
  // clean
  if FileExists(AProject.GetClassFileFor(ASourceFile)) then
    DeleteFile(AProject.GetClassFileFor(ASourceFile));
  // preprocess
  buildDir := AProject.ProjectDirectory+IntToHex(Random(-1),8)+'\';
  ForceDirectories(buildDir);
  prp := buildDir+ChangeFileExt(ExtractFileName(ASourceFile.Filename),EXTENSION_PREPROCESSED);
  ftc := buildDir+ExtractFileName(ASourceFile.Filename);
  result := PreprocessSourceFile(
    AProject.SourceDirectory,
    ASourceFile.Filename,
    prp,
    AProject.GetActiveConfigurationName);
  if result then
    result := RenameFile(prp,ftc);
  // compile
  stubs.Clear;
  libs.Clear;
  if result then
    result := CompileSourceFile(AProject, ftc, AError)
  else
    AError := 'ERROR: failed trying to preprocess '+ASourceFile.Filename;
  DeleteFile(ftc);
  RemoveDir(buildDir);
end;

function Build(AProject: Tmp3Project; var AError: string): boolean;
var ftcFiles, jarFiles: TStringList; mContent, jarSize, buildDir: string;

  function MakeManifestMF: boolean;
  begin
    DefaultCompilerMessageHandler('Making MANIFEST.MF...');
    mContent := StringReplace(MANIFEST_TEMPLATE,PATTERN_PROJECT_NAME,AProject.MidletInfo.Name,[rfReplaceAll]);
    mContent := StringReplace(mContent,PATTERN_PROJECT_ICON,AProject.MidletInfo.Icon,[rfReplaceAll]);
    mContent := StringReplace(mContent,PATTERN_PROJECT_VENDOR,AProject.MidletInfo.Vendor,[rfReplaceAll]);
    mContent := StringReplace(mContent,PATTERN_PROJECT_VERSION,AProject.MidletInfo.Version,[rfReplaceAll]);
    mContent := StringReplace(mContent,PATTERN_CLDC_VERSION,CLDC_10,[rfReplaceAll]);
    case AProject.BuildConfigurations[
      AProject.BuildConfigurations.ActiveConfigurationIndex].MIDPVersion of
      mvMIDP10: mContent := StringReplace(mContent,PATTERN_MIDP_VERSION,MIDP_10,[rfReplaceAll]);
      mvMIDP20: mContent := StringReplace(mContent,PATTERN_MIDP_VERSION,MIDP_20,[rfReplaceAll]);
    end;
    result := true;
    if not result then
      AError := 'error generating '+MANIFEST_MF;
  end;

  // j-a-s-d: implements the same order method of MP 2.0
  function OrderUnitsByDependency(AResultantSortedList, AUnitsRequirementsList: TStringList): boolean;
  var i,j: integer; dependencies: TStringList;
  begin
    // j-a-s-d: TODO! replace for a faster sorting algorithm... topological?
    result := true;
    dependencies := TStringList.Create;
    try
      // sort by dependencies
      i := 0;
      while i < AUnitsRequirementsList.Count - 1 do begin      
        j := i + 1;
        while j < AUnitsRequirementsList.Count do begin
          dependencies.CommaText := AUnitsRequirementsList.ValueFromIndex[i];
          if dependencies.IndexOf(AUnitsRequirementsList.Names[j]) > -1 then
          begin
            AResultantSortedList.Exchange(i,j);
            AUnitsRequirementsList.Exchange(i,j);
          end;
          inc(j);
        end;
        inc(i);
      end;
      // check for circular unit reference
      i := 0;
      while i < AUnitsRequirementsList.Count - 1 do begin      
        j := i + 1;
        while j <  AUnitsRequirementsList.Count do begin
          dependencies.CommaText := AUnitsRequirementsList.ValueFromIndex[i];
          if dependencies.IndexOf(AUnitsRequirementsList.Names[j]) > -1 then
          begin
            AError := 'Circular unit dependencies between unit '
              + AUnitsRequirementsList.Names[i] + ' and ' + AUnitsRequirementsList.Names[j] + '.';
            result := false;
            break;
          end;
          inc(j);
        end;
        inc(i);
      end;
    finally
      dependencies.Free;
    end;
  end;

  function UnitDependanceResolution(AResultantSortedList: TStringList): boolean;
  var i: integer; AUnitsRequirementsList: TStringList; sf: Tmp3SourceFile;
  begin
    AUnitsRequirementsList := TStringList.Create;
    try
      // for each unit detect requirements
      for i := 0 to AResultantSortedList.Count-1 do begin
        sf := AProject.SourceFiles.GetSourceFileByName(AResultantSortedList.Names[i]);
        if assigned(sf) and sf.Exists then begin
          result := DetectUnitsOfSourceFile(AProject,sf.GetFullname,AError);
          if not result then
            exit;
          AUnitsRequirementsList.Add(sf.GetUnitName + '=' + units.CommaText);
        end else begin
          AError := 'ERROR: source file not found';
          result := false;
          exit;
        end;
        if stop then begin
          result := false;
          exit;
        end;
      end;
      result := AResultantSortedList.Count < 2;
      if not result then begin
        DefaultCompilerMessageHandler('Solving compilation order...');
        result := (AResultantSortedList.Count = AUnitsRequirementsList.Count) and
          OrderUnitsByDependency(AResultantSortedList,AUnitsRequirementsList);
        if not result then begin
          DefaultCompilerMessageHandler(pchar(AError));
          AError := 'ERROR: could not solve unit dependency';
        end else
          for i := 0 to AResultantSortedList.Count - 1 do
            DefaultCompilerMessageHandler(pchar('  '+AResultantSortedList.Names[i]));
      end;
    finally
      AUnitsRequirementsList.Free;
    end;
  end;

  function BuildUnits(ASortedUnitList: TStringList): boolean;
  var i: integer; sf: Tmp3SourceFile; s: string;
  begin
    result := true;
    DefaultCompilerMessageHandler(pchar('Target Info'));
    DefaultCompilerMessageHandler(pchar('  MIDlet Type: '+AProject.BuildConfigurations[
      AProject.BuildConfigurations.ActiveConfigurationIndex].GetMIDletTypeAsString));
    DefaultCompilerMessageHandler(pchar('  Real Numbers: '+AProject.BuildConfigurations[
      AProject.BuildConfigurations.ActiveConfigurationIndex].GetRealNumbersAsString));
    stubs.Clear;
    libs.Clear;
    AProject.CleanClassesDirectory;
    next_record_ID := 0;
    for i := 0 to ASortedUnitList.Count - 1 do begin
      sf := AProject.SourceFiles.GetSourceFileByName(ASortedUnitList.Names[i]);
      if not assigned(sf) or (not sf.Exists) then begin
        AError := 'ERROR: could not find source file '+ASortedUnitList.Names[i];
        result := false;
        break;
      end else if not CompileSourceFile(AProject,ASortedUnitList.ValueFromIndex[i],AError) then begin
        AError := 'ERROR: an error occurred compiling '+ExtractFileName(ASortedUnitList.Names[i]);
        result := false;
        break;
      end;
      if stop then begin
        result := false;
        exit;
      end;
      if result then begin
        s := AProject.GetClassFileFor(sf);
        jarFiles.Add(ExtractFileName(s) + '=' + s);
      end;
    end;
    for i := 0 to MAXWORD do begin
      s := AProject.ClassesDirectory + 'R_'+IntToStr(i)+'.class';
      if FileExists(s) then
        jarFiles.Add(ExtractFileName(s) + '=' + s)
      else
        break;
      if stop then begin
        result := false;
        exit;
      end;
    end;
  end;

  function IncludeStubs: boolean;
  var stubsDir: string; x: integer;

    function AddStub(AStubFilename: string): boolean;
    begin
      result := FileExists(stubsDir+AStubFilename);
      if result then
        jarFiles.Add(AStubFilename + '=' + stubsDir+AStubFilename)
      else
        AError := 'ERROR: could not find stub '+AStubFilename;
    end;

  begin
    result := true;
    stubsDir := gSettings.StubsDirectory;
    stubs.Insert(0,FRAMEWORK_CLASS);
    for x := 0 to stubs.Count - 1 do begin
      result := AddStub(stubs[x]);
      if not result then
        break;
      if stop then begin
        result := false;
        exit;
      end;
    end;
  end;

  function IncludeLibs: boolean;
  var libsDir: string; x: integer;

    function AddLib(ALibFilename: string): boolean;
    begin
      result := FileExists(libsDir+ALibFilename);
      if result then
        jarFiles.Add(ALibFilename + '=' + libsDir+ALibFilename)
      else
        AError := 'ERROR: could not find lib '+ALibFilename;
    end;

  begin
    result := true;
    libsDir := gSettings.LibrariesDirectory;
    for x := 0 to libs.Count - 1 do begin
      result := AddLib(EXTERNAL_LIBRARY_PREFIX+libs[x]+EXTENSION_CLASS);
      if not result then
        break;
      if stop then begin
        result := false;
        exit;
      end;
    end;
  end;

  function IncludeResources: boolean;
  var x: integer; rf: Tmp3ResourceFile;
  begin
    result := true;
    for x := 0 to AProject.ResourceFiles.Count - 1 do begin
      rf := AProject.ResourceFiles[x];
      if not rf.Exists then begin
        AError := 'ERROR: could not find resource '+rf.Filename;
        result := false;
        break;
      end else
        if rf.PresentInConfiguration(AProject.GetActiveConfigurationName) then
          jarFiles.Add(rf.Filename + '=' + rf.GetFullname);
      if stop then begin
        result := false;
        exit;
      end;
    end;
  end;

  function MakeJAR: boolean;
  var zipper: TAbZipper; x: integer; mStream: TStringStream;
  begin
    DefaultCompilerMessageHandler('Making JAR file...');
    result := false;
    if FileExists(AProject.JavaArchiveFilename) then begin
      DeleteFile(AProject.JavaArchiveFilename);
      if FileExists(AProject.JavaArchiveFilename) then begin
        AError := 'ERROR: could not delete old jar file';
        exit;
      end;
    end;
    zipper := TAbZipper.Create(nil);
    try
      zipper.ArchiveType := atZip;
      zipper.StoreOptions := [];
      zipper.Filename := AProject.JavaArchiveFilename;
      // insert manifest (can be placed on the start to avoid the full flush)
      mStream := TStringStream.Create(mContent);
      try
        DefaultCompilerMessageHandler(pchar('  adding '+MANIFEST_ARCHIVE_DIR+'/'+MANIFEST_MF));
        zipper.AddFromStream(MANIFEST_ARCHIVE_DIR+'/'+MANIFEST_MF,mStream);
      finally
        mStream.Free;
      end;
       // do not flush to disk after every file addition
      zipper.AutoSave := false;
      for x := 0 to jarFiles.Count - 1 do
      begin
        DefaultCompilerProgressHandler((x+1)*100 div jarFiles.Count);
        DefaultCompilerMessageHandler(pchar('  adding '+jarFiles.Names[x]));
        zipper.AddFiles(jarFiles.ValueFromIndex[x],0);
        if stop then begin
          result := false;
          exit;
        end;
      end;
      // flush archive to disk
      zipper.Save;
      // change name entries (does not requires full archive flush)
      for x := 0 to jarFiles.Count - 1 do
        zipper.Move(zipper.Items[x+1],jarFiles.Names[x]);
      zipper.CloseArchive;
    finally
      zipper.Free;
    end;
    result := FileExists(AProject.JavaArchiveFilename);
    if result then begin
      jarSize := GetFileSizeAsString(AProject.JavaArchiveFilename);
      DefaultCompilerMessageHandler(pchar('  File: '+AProject.JavaArchiveFilename));
      DefaultCompilerMessageHandler(pchar('  Size: '+jarSize+' bytes'));
    end else
      AError := 'ERROR: could not create jar file';
  end;

  function MakeJAD: boolean;
  var s: string;
  begin
    DefaultCompilerMessageHandler('Making JAD file...');
    s := StringReplace(JAD_TEMPLATE,PATTERN_PROJECT_NAME,AProject.MidletInfo.Name,[rfReplaceAll]);
    s := StringReplace(s,PATTERN_PROJECT_ICON,AProject.MidletInfo.Icon,[rfReplaceAll]);
    s := StringReplace(s,PATTERN_PROJECT_VENDOR,AProject.MidletInfo.Vendor,[rfReplaceAll]);
    s := StringReplace(s,PATTERN_PROJECT_VERSION,AProject.MidletInfo.Version,[rfReplaceAll]);
    s := StringReplace(s,PATTERN_JAR_URL,ExtractFileName(AProject.JavaArchiveFilename),[rfReplaceAll]);
    s := StringReplace(s,PATTERN_JAR_SIZE,jarSize,[rfReplaceAll]);
    s := StringReplace(s,PATTERN_CLDC_VERSION,CLDC_10,[rfReplaceAll]);
    case AProject.BuildConfigurations[
      AProject.BuildConfigurations.ActiveConfigurationIndex].MIDPVersion of
      mvMIDP10: s := StringReplace(s,PATTERN_MIDP_VERSION,MIDP_10,[rfReplaceAll]);
      mvMIDP20: s := StringReplace(s,PATTERN_MIDP_VERSION,MIDP_20,[rfReplaceAll]);
    end;
    with TStringList.Create do
    try
      Text := s;
      if stubs.IndexOf(HTTP_CLASS)>-1 then
        Add('MIDlet-Permissions: '+PERMISSION_HTTP);
      if stubs.IndexOf(SMS_CLASS)>-1 then
        Add('MIDlet-Permissions: '+PERMISSION_SMS);
      SaveToFile(AProject.JavaDescriptorFilename);
    finally
      Free;
    end;
    result := FileExists(AProject.JavaDescriptorFilename);
    if not result then
      AError := 'ERROR: could not create jad file';
  end;

  function PreprocessFiles(APreprocessedFiles: TStringList): boolean;
  var ASourceFile: Tmp3SourceFile; i: integer; fn,prp,ftc: string;
  begin
    result := assigned(APreprocessedFiles) and Tmp3Preprocessor.Present;
    if result then
      for i := AProject.SourceFiles.Count-1 downto 0 do
      begin
        ASourceFile := AProject.SourceFiles[i];
        result := ASourceFile.Exists;
        if not result then begin
          AError := 'ERROR: '+ASourceFile.Filename+' does not exists';
          break;
        end;
        fn := ExtractFileName(StringReplace(ASourceFile.Filename,'/','\',[rfReplaceAll]));
        prp := buildDir+ChangeFileExt(fn,EXTENSION_PREPROCESSED);
        ftc := buildDir+fn;
        result := PreprocessSourceFile(
          AProject.SourceDirectory,
          ASourceFile.Filename,
          prp,
          AProject.GetActiveConfigurationName);
        RenameFile(prp,ftc);
        APreprocessedFiles.Add(ASourceFile.Filename + '=' + ftc);
        if not result then begin
          AError := 'ERROR: failed trying to preprocess '+ASourceFile.Filename;
          break;
        end else if stop then begin
          result := false;
          exit;
        end;
      end;
  end;

  procedure CleanUp;
  var i: integer; s: string;
  begin
    for i := 0 to ftcFiles.Count - 1 do begin
      if ExtractFilePath(ftcFiles.ValueFromIndex[i])='' then
        s := buildDir+ftcFiles.ValueFromIndex[i]
      else
        s := ftcFiles.ValueFromIndex[i];
      if FileExists(s) then
        DeleteFile(s);
    end;
    RemoveDir(buildDir);
  end;

begin
  result := false;
  stop := false;
  ErrorStop := false;
  AError := '';
  ftcFiles := TStringList.Create;
  jarFiles := TStringList.Create;
  buildDir := AProject.ProjectDirectory+IntToHex(Random(-1),8)+'\';
  ForceDirectories(buildDir);
  if (not DirectoryExists(buildDir))or(not AProject.EnsureDirectories) then
    AError := 'Could not ensure directory structure.'
  else
  try
    if not stop and PreprocessFiles(ftcFiles) then
      if not stop and UnitDependanceResolution(ftcFiles) then
        if not stop and BuildUnits(ftcFiles) then
          if not stop and IncludeStubs then
            if not stop and IncludeLibs then
              if not stop and IncludeResources then
                if not stop and MakeManifestMF then
                  if not stop and MakeJAR then
                    result := not stop and MakeJAD;
    CleanUp;
  finally
    ftcFiles.Free;
    jarFiles.Free;
  end;
  if stop and (not ErrorStop) then
    AError := 'Build process has been stopped.'
  else if result then begin
    ExecuteScript(AProject,SCRIPT_AFTER_SUCCESSFUL_BUILD);
    DefaultCompilerMessageHandler('Successful build.');
  end else begin
    ExecuteScript(AProject,SCRIPT_AFTER_UNSUCCESSFUL_BUILD);
  end;
end;

initialization
  Randomize;
  units:=TStringList.Create;
  stubs:=TStringList.Create;
  libs:=TStringList.Create;
finalization
  units.Free;
  stubs.Free;
  libs.Free;
end.
