(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Main;

interface

uses
  Windows, Messages, Dialogs, Controls, ActnList,
  Classes, ImgList, Menus, ExtCtrls, Forms, Graphics,
  DosCommand,
  OtlTask, OtlTaskControl, OtlEventMonitor, OtlComm,
  sitGenericMainForm, sitCompilerMessagesPanel,
  mp3ProjectManager, mp3GroupManager, mp3MainFrame;

const
  WM_PERFORMBUILD = WM_USER + $0888;
  MESSAGE_DELAY = 1500;

type
  Tmp3MainForm = class(TsitGenericMainForm)
    alMainMenu: TActionList;
    actFile: TAction;
    actEdit: TAction;
    actView: TAction;
    actHelp: TAction;
    al_actFile: TActionList;
    actExit: TAction;
    al_actHelp: TActionList;
    actOpenHelp: TAction;
    AboutSeparator: TAction;
    actAbout: TAction;
    actNew: TAction;
    al_actNew: TActionList;
    actNewGroup: TAction;
    actNewProject: TAction;
    actNewUnit: TAction;
    ExitSeparator: TAction;
    actOpen: TAction;
    actProject: TAction;
    al_actProject: TActionList;
    actBuild: TAction;
    actRun: TAction;
    actBuildAndRun: TAction;
    actSaveFile: TAction;
    actCloseFile: TAction;
    actSaveProject: TAction;
    al_actEdit: TActionList;
    al_actView: TActionList;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actDelete: TAction;
    actSelectAll: TAction;
    actFullScreen: TAction;
    actProjectManager: TAction;
    ilMain: TImageList;
    actCloseProject: TAction;
    pmProjectManager: TPopupMenu;
    CloseProject1: TMenuItem;
    SaveProject1: TMenuItem;
    RunMIDlet1: TMenuItem;
    BuildProject1: TMenuItem;
    BuildandRun1: TMenuItem;
    N1: TMenuItem;
    actCloseAllFiles: TAction;
    actUndo: TAction;
    actRedo: TAction;
    EditSeparator1: TAction;
    EditSeparator2: TAction;
    actCompileCurrentFile: TAction;
    CompileCurrentFileSeparator: TAction;
    OpenFoldersSeparator: TAction;
    LanguageSeparator: TAction;
    actLanguage: TAction;
    actNewImage: TAction;
    actCompilerMessagesPanel: TAction;
    pmCompilerMessagesPanel: TPopupMenu;
    alCompilerMessages: TActionList;
    actClearCompilerMessages: TAction;
    Clear1: TMenuItem;
    dlgSaveCompilerMessages: TSaveDialog;
    actSaveCompilerMessages: TAction;
    SaveToFile1: TMenuItem;
    al_actCodeEditorStyle: TActionList;
    N2: TMenuItem;
    Copy1: TMenuItem;
    SelectAll1: TMenuItem;
    actCopyCompilerMessage: TAction;
    actSelectAllCompilerMessages: TAction;
    alPMSourceFiles: TActionList;
    alPMResourceFiles: TActionList;
    actOpenSourceFile: TAction;
    actOpenResourceFile: TAction;
    actAddResourceFile: TAction;
    actAddSourceFile: TAction;
    actDeleteSourceFile: TAction;
    actDeleteResourceFile: TAction;
    actRenameSourceFile: TAction;
    actRenameResourceFile: TAction;
    ReopenSeparator: TAction;
    actReopen: TAction;
    al_actReopen: TActionList;
    actRecreateSourceFile: TAction;
    alPMBuildConfigurations: TActionList;
    actAddBuildConfiguration: TAction;
    actDeleteBuildConfiguration: TAction;
    actRenameBuildConfiguration: TAction;
    al_actSearch: TActionList;
    actSearch: TAction;
    actFindText: TAction;
    actFindNext: TAction;
    actFindPrevious: TAction;
    actReplaceText: TAction;
    actGroupManager: TAction;
    alGMProjects: TActionList;
    pmGroupManager: TPopupMenu;
    actIncludeProject: TAction;
    actExcludeProject: TAction;
    actOpenProject: TAction;
    actBuildAllProjectsFromHere: TAction;
    actCloseGroup: TAction;
    CloseGroup1: TMenuItem;
    al_actSettings: TActionList;
    actKeepHistory: TAction;
    actCodeEditorStyle: TAction;
    actSettings: TAction;
    actEmulators: TAction;
    al_actEmulators: TActionList;
    actNewSourceFile: TAction;
    actNewResourceFile: TAction;
    actStopBuildProcess: TAction;
    GotoSeparator: TAction;
    actGoto: TAction;
    actNextTab: TAction;
    actPreviousTab: TAction;
    al_actKeepHistory: TActionList;
    actBackupsDisabled: TAction;
    actBackupsUnlimited: TAction;
    actBackups10: TAction;
    actBackups25: TAction;
    actBackups50: TAction;
    ProjectSiteSeparator: TAction;
    actVisitTheProjectSite: TAction;
    DemosSeparator: TAction;
    actOpenDemosFolder: TAction;
    actOpenFolder: TAction;
    al_actOpenFolder: TActionList;
    actOpenBINFolder: TAction;
    actOpenScriptsFolder: TAction;
    actOpenProjectFolder: TAction;
    ProjecFoldertSeparator: TAction;
    actOpenSourceFolder: TAction;
    actOpenResourceFolder: TAction;
    VisibilitySeparrator: TAction;
    FullScreenSeparator: TAction;
    al_actLanguage: TActionList;
    actCheckForUpdates: TAction;
    FormatSeparator: TAction;
    actFormatSourceCode: TAction;
    ilLanguages: TImageList;
    CommentSeparator: TAction;
    actCommentSelectedText: TAction;
    actCommentSelectedLines: TAction;
    actSetDefaultProjectLocation: TAction;
    SkinSeparator: TAction;
    actSkin: TAction;
    al_actSkin: TActionList;
    DefaultSeparator: TAction;
    imgReadOnly: TImage;
    actOpenClassesFolder: TAction;
    actMinimizeToTray: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actExitExecute(Sender: TObject);
    procedure actOpenHelpExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actBuildExecute(Sender: TObject);
    procedure actRunExecute(Sender: TObject);
    procedure actBuildAndRunExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure actCloseFileExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actSelectAllExecute(Sender: TObject);
    procedure actNewUnitExecute(Sender: TObject);
    procedure actFullScreenExecute(Sender: TObject);
    procedure actProjectManagerExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actCloseProjectExecute(Sender: TObject);
    procedure actSaveProjectExecute(Sender: TObject);
    procedure actCloseAllFilesExecute(Sender: TObject);
    procedure actFormatSourceCodeExecute(Sender: TObject);
    procedure actUndoExecute(Sender: TObject);
    procedure actRedoExecute(Sender: TObject);
    procedure actCompileCurrentFileExecute(Sender: TObject);
    procedure actOpenBinFolderExecute(Sender: TObject);
    procedure actNewImageExecute(Sender: TObject);
    procedure actCompilerMessagesPanelExecute(Sender: TObject);
    procedure actClearCompilerMessagesExecute(Sender: TObject);
    procedure actSaveCompilerMessagesExecute(Sender: TObject);
    procedure actMidletStyleExecute(Sender: TObject);
    procedure actClassicStyleExecute(Sender: TObject);
    procedure actCopyCompilerMessageExecute(Sender: TObject);
    procedure actSelectAllCompilerMessagesExecute(Sender: TObject);
    procedure actOpenSourceFileExecute(Sender: TObject);
    procedure actOpenResourceFileExecute(Sender: TObject);
    procedure actAddResourceFileExecute(Sender: TObject);
    procedure actAddSourceFileExecute(Sender: TObject);
    procedure actDeleteSourceFileExecute(Sender: TObject);
    procedure actDeleteResourceFileExecute(Sender: TObject);
    procedure actGotoExecute(Sender: TObject);
    procedure actRenameSourceFileExecute(Sender: TObject);
    procedure actRenameResourceFileExecute(Sender: TObject);
    procedure actRecreateSourceFileExecute(Sender: TObject);
    procedure actAddBuildConfigurationExecute(Sender: TObject);
    procedure actDeleteBuildConfigurationExecute(Sender: TObject);
    procedure actRenameBuildConfigurationExecute(Sender: TObject);
    procedure actFindTextExecute(Sender: TObject);
    procedure actFindNextExecute(Sender: TObject);
    procedure actFindPreviousExecute(Sender: TObject);
    procedure actReplaceTextExecute(Sender: TObject);
    procedure actGroupManagerExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure actIncludeProjectExecute(Sender: TObject);
    procedure actExcludeProjectExecute(Sender: TObject);
    procedure actBuildAllProjectsFromHereExecute(Sender: TObject);
    procedure actCloseGroupExecute(Sender: TObject);
    procedure actNewGroupExecute(Sender: TObject);
    procedure actOpenScriptsFolderExecute(Sender: TObject);
    procedure actOpenProjectFolderExecute(Sender: TObject);
    procedure actNewSourceFileExecute(Sender: TObject);
    procedure actNewResourceFileExecute(Sender: TObject);
    procedure actStopBuildProcessExecute(Sender: TObject);
    procedure actNextTabExecute(Sender: TObject);
    procedure actPreviousTabExecute(Sender: TObject);
    procedure actSetFontExecute(Sender: TObject);
    procedure actBackupsLimitExecute(Sender: TObject);
    procedure actVisitTheProjectSiteExecute(Sender: TObject);
    procedure actOpenDemosFolderExecute(Sender: TObject);
    procedure actOpenSourceFolderExecute(Sender: TObject);
    procedure actOpenResourceFolderExecute(Sender: TObject);
    procedure actCheckForUpdatesExecute(Sender: TObject);
    procedure actCommentSelectedTextExecute(Sender: TObject);
    procedure actCommentSelectedLinesExecute(Sender: TObject);
    procedure actSetDefaultProjectLocationExecute(Sender: TObject);
    procedure actOpenClassesFolderExecute(Sender: TObject);
    procedure actMinimizeToTrayExecute(Sender: TObject);
  private
    FBuilding: boolean;
    FBuildingGroup: boolean;
    FBuildNextInGroup: boolean;
    FRunBuiltMIDlet: boolean;
    FRunningEmulator: TDosCommand;
    FStopCloseAll: boolean;
    FMainFrame: Tmp3MainFrame;
    FGroupManager: Tmp3GroupManager;
    FProjectManager: Tmp3ProjectManager;
    FCompilerMessagesPanel: TsitCompilerMessagesPanel;
    FCompilerError: string;
    FCompilerTask: IOmniTask;
    FMessageDispatch: TOmniEventMonitor;
    FReferenceTopics: TStringList;
    FAvailableTranslations: TStringList;
    FLastDownloadProgressMessageTickCount: DWORD;
    procedure OnManageCodeEditorStylesExecute(Sender: TObject);
    procedure OnManageEmulatorsExecute(Sender: TObject);
    procedure OnSetDefaultEmulatorExecute(Sender: TObject);
    procedure OnOpenRecentProjectExecute(Sender: TObject);
    procedure OnClearRecentProjectsExecute(Sender: TObject);
    procedure OnGroupManagerClose(Sender: TObject);
    procedure OnGroupProjectChange(Sender: TObject);
    procedure OnProjectManagerClose(Sender: TObject);
    procedure OnProjectManagerEditSourceFile(Sender: TObject);
    procedure OnProjectManagerEditResourceFile(Sender: TObject);
    procedure OnActiveTabChange(Sender: TObject; TabIndex: Integer);
    procedure OnCompilerMessagesPanelClose(Sender: TObject);
    procedure OnDoubleClickLine(Sender: TObject; ALine: string);
    procedure OnMainFramePopup(Sender: TObject);
    procedure OnSetCodeEditorStyle(Sender: TObject);
    procedure OnCodeEditorCaretMove(Sender: TObject);
    procedure OnCodeEditorPreprocess(Sender: TObject);
    procedure OnCodeEditorHistory(Sender: TObject);
    procedure OnCodeEditorOpenFileAtCursor(Sender: TObject);
    procedure OnCodeEditorGetHelpOnWord(Sender: TObject);
    procedure OnCodeEditorRollback(Sender: TObject);
    procedure OnCodeEditorSendToBuffer(Sender: TObject);
    procedure RefreshAvailableTranslations;
    procedure RefreshLanguageActions;
    procedure RefreshBackupsActions;
    procedure RefreshStyleActions;
    procedure RefreshActions;
    procedure RefreshCompilerActions;
    procedure RefreshReopen;
    procedure RefreshEmulators;
    procedure RefreshSubmenus(AAvoidRetranslation: boolean = false);
    procedure RefreshTranslation;
    procedure SendToRecents(AFilename: string);
    function PrepareCompilerMessagesPanel: boolean;
    procedure BackgroundBuild(const task: IOmniTask);
    procedure ShowCompilerMessage(AMessage: string);
    procedure ShowCompilerProgress(AValue: DWORD);
    procedure HandleTaskTerminated(const task: IOmniTaskControl);
    procedure HandleTaskMessage(const task: IOmniTaskControl; const msg: TOmniMessage);
    procedure LoadGroup(AFilename: string);
    procedure LoadProject(AFilename: string);
    procedure LoadInCodeEditor(AFilename: string);
    procedure LoadInHexEditor(AFilename: string);
    procedure LoadInImageEditor(AFilename: string);
    procedure BuildNextInGroup;
    procedure WaitUntilBuildFinishes;
    procedure FillReferenceTopics;
    procedure SetLanguage(ALanguageCode: string);
    function GetReferenceTopic(ATopic: string): integer;
    procedure InternalPerformBuild(var Msg: TMessage); message WM_PERFORMBUILD;
    procedure OnRunningEmulatorFinish(Sender: TObject; ExitCode: LongWord);
    procedure OnLanguageClick(Sender: TObject);
{$IFDEF WOW64_WORKAROUND}
    procedure CompilerMessageHandle(var Msg: TMessage); message WM_COPYDATA;
{$ENDIF}
    procedure OnDowloadProgress(ADownloadedBytes: integer);
    procedure UpdateStatusBarText(AText: widestring = ' ');
    procedure OnInstallUpdateClick(Sender: TObject);
    procedure OnDropFileHandler(const AFilename: string);
    procedure OnSkinExecute(Sender: TObject);
    procedure RefreshSkins;
    procedure RefreshStatusBarEditorInfo;
  public
    procedure Load(AFilename: string);
    procedure PerformBuild;
    procedure ReadSettings;
    procedure WriteSettings;
    procedure SetTitleBarCaption(ACaption: string = '');
  end;

implementation

uses
  SysUtils, StrUtils, ShellAPI,
  gnugettext, languagecodes,
  SpTBXMessageDlg, SpTBXInputBox,
  tuiItemWithLocationDialog, tuiItemWithSizeDialog, tuiControls,
  tuiColorManager, tuiActionsUtils, tuiDialogs,
  sitOSUtils, sitDirUtils, sitConsts, sitDelforObjectPascalFormatter,
  sitEditorFrame, sitInformationBar,
  mp3Consts, mp3About, mp3ProjectBuilding, mp3Core,
  mp3Group, mp3Project, mp3SourceFiles, mp3ResourceFiles,
  mp3EmulatorsDialog, mp3CodeEditorStylesDialog,
  mp3Settings, mp3CodeEditorFrame, mp3FileKind;

{$R *.dfm}

procedure Tmp3MainForm.FormCreate(Sender: TObject);
var i: integer; ti: TtuiMenuItem; si: TtuiSeparatorMenuItem;
begin
  inherited;
  KeyPreview := true;
  TtuiColorManager.AddSkinsFromFolder(gSettings.ConfigPath+'Skins');
  sitInformationBar.SetReadOnlyPicture(imgReadOnly.Picture);
  // web update related
  gCore.WebUpdate.OnDownloadProgress := OnDowloadProgress;
  FLastDownloadProgressMessageTickCount := GetTickCount;
  // language related
  UseLanguage(DEFAULT_LANGUAGE_CODE);
  FAvailableTranslations := TStringList.Create;
  RefreshAvailableTranslations;
  RefreshLanguageActions;
  // help topics
  FReferenceTopics := TStringList.Create;
  FillReferenceTopics;
  // main form
  Maximize;
  OnDropFile := OnDropFileHandler;
  SetTitleBarCaption;
  RefreshSubmenus(true);
  // dialogs
  dlgFont.Options := dlgFont.Options + [fdNoStyleSel,fdNoSimulations] - [fdEffects];
  // main frame
  FMainFrame := Tmp3MainFrame.Create(Self);
  FMainFrame.Parent := TitleBar;
  FMainFrame.Align := alClient;
  FMainFrame.TabControl.Images := ilMain;
  FMainFrame.CloseTabButton.OnClick := actCloseFile.OnExecute;
  FMainFrame.CloseTabButton.ShortCut := actCloseFile.ShortCut;
  FMainFrame.CloseTabButton.Hint := actCloseFile.Caption;
  FMainFrame.CloseTabButton.ImageIndex := 20;
  FMainFrame.TabControl.OnActiveTabChange := OnActiveTabChange;
  FMainFrame.OnPopup := OnMainFramePopup;
  FMainFrame.OnCodeEditorCaretMove := OnCodeEditorCaretMove;
  FMainFrame.OnCodeEditorPreprocess := OnCodeEditorPreprocess;
  FMainFrame.OnCodeEditorHistory := OnCodeEditorHistory;
  FMainFrame.OnCodeEditorOpenFileAtCursor := OnCodeEditorOpenFileAtCursor;
  FMainFrame.OnCodeEditorGetHelpOnWord := OnCodeEditorGetHelpOnWord;
  FMainFrame.OnCodeEditorRollback := OnCodeEditorRollback;
  FMainFrame.OnCodeEditorSendToBuffer := OnCodeEditorSendToBuffer;
  // project manager
  FProjectManager := Tmp3ProjectManager.Create(Self);
  FProjectManager.Parent := FMainFrame.DockLeft;
  FProjectManager.Images := ilMain;
  FProjectManager.OnClose := OnProjectManagerClose;
  FProjectManager.OnSourceFileEdit := OnProjectManagerEditSourceFile;
  FProjectManager.OnSourceFileRename := actRenameSourceFile.OnExecute;
  FProjectManager.OnResourceFileEdit := OnProjectManagerEditResourceFile;
  FProjectManager.OnResourceFileRename := actRenameResourceFile.OnExecute;
  FProjectManager.OnBuildConfigurationRename := actRenameBuildConfiguration.OnExecute;
  for i := 0 to pmProjectManager.Items.Count - 1 do
    if pmProjectManager.Items[i].Caption = '-' then begin
      si := TtuiSeparatorMenuItem.Create(FProjectManager.PopupMenu);
      si.Tag := 0;
      FProjectManager.PopupMenu.Items.Add(si);
    end else begin
      ti := TtuiMenuItem.Create(FProjectManager.PopupMenu);
      ti.Action := pmProjectManager.Items[i].Action;
      ti.Tag := 0;
      FProjectManager.PopupMenu.Items.Add(ti);
    end;
  FProjectManager.SourceFilesActions := alPMSourceFiles;
  FProjectManager.ResourceFilesActions := alPMResourceFiles;
  FProjectManager.BuildConfigurationsActions := alPMBuildConfigurations;
  // group manager
  FGroupManager := Tmp3GroupManager.Create(Self);
  FGroupManager.Parent := FMainFrame.DockLeft;
  FGroupManager.Images := ilMain;
  FGroupManager.OnClose := OnGroupManagerClose;
  FGroupManager.OnProjectChange := OnGroupProjectChange;
  for i := 0 to pmGroupManager.Items.Count - 1 do
    if pmGroupManager.Items[i].Caption = '-' then begin
      si := TtuiSeparatorMenuItem.Create(FGroupManager.PopupMenu);
      si.Tag := 0;
      FGroupManager.PopupMenu.Items.Add(si);
    end else begin
      ti := TtuiMenuItem.Create(FGroupManager.PopupMenu);
      ti.Action := pmGroupManager.Items[i].Action;
      ti.Tag := 0;
      FGroupManager.PopupMenu.Items.Add(ti);
    end;
  FGroupManager.ProjectsActions := alGMProjects;
  // compiler messages
  FMessageDispatch := TOmniEventMonitor.Create(Self);
  FMessageDispatch.OnTaskMessage := HandleTaskMessage;
  FMessageDispatch.OnTaskTerminated := HandleTaskTerminated;
  FCompilerMessagesPanel := TsitCompilerMessagesPanel.Create(Self);
  FCompilerMessagesPanel.Parent := FMainFrame.DockBottom;
  FCompilerMessagesPanel.OnClose := OnCompilerMessagesPanelClose;
  FCompilerMessagesPanel.OnDoubleClickLine := OnDoubleClickLine;
{$IFDEF WOW64_WORKAROUND}
  if IsUnderWow64 then
    mp3ProjectBuilding.CompilerMessageHandlerHandle := Self.Handle;
{$ENDIF}
  mp3ProjectBuilding.CompilerMessageHandler := ShowCompilerMessage;
  mp3ProjectBuilding.CompilerProgressHandler := ShowCompilerProgress;
  for i := 0 to pmCompilerMessagesPanel.Items.Count - 1 do
    if pmCompilerMessagesPanel.Items[i].Caption = '-' then
      FCompilerMessagesPanel.PopupMenu.Items.Add(TtuiSeparatorMenuItem.Create(FCompilerMessagesPanel.PopupMenu))
    else begin
      ti := TtuiMenuItem.Create(FCompilerMessagesPanel.PopupMenu);
      ti.Action := pmCompilerMessagesPanel.Items[i].Action;
      FCompilerMessagesPanel.PopupMenu.Items.Add(ti);
    end;
  // add up controls for translation
  TranslateComponent(FCompilerMessagesPanel);
  TranslateComponent(FGroupManager);
  TranslateComponent(FProjectManager);
  TranslateComponent(FMainFrame);
  TranslateComponent(Self);
  // refresh
  FBuilding := false;
  FBuildingGroup := false;
  FBuildNextInGroup := false;
  FRunBuiltMIDlet := false;
  RefreshActions;
  RefreshCompilerActions;
  RefreshBackupsActions;
  RefreshTranslation;
  // running emulator
  FRunningEmulator := TDosCommand.Create(nil);
  FRunningEmulator.ShowWindow := swSHOW;
  FRunningEmulator.OnTerminated := OnRunningEmulatorFinish;
end;

procedure Tmp3MainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FRunningEmulator);
  FreeAndNil(FReferenceTopics);
  FreeAndNil(FAvailableTranslations);
  if FBuilding then
    actStopBuildProcess.Execute;
end;

procedure Tmp3MainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  actCloseAllFilesExecute(nil);
  CanClose := not FStopCloseAll;
end;

function Tmp3MainForm.GetReferenceTopic(ATopic: string): integer;
var x: integer;
begin
  result := -1;
  for x := 0 to FReferenceTopics.Count-1 do
    if SameText(FReferenceTopics.Names[x],ATopic) then begin
      result := x;
      break;
    end;
end;

procedure Tmp3MainForm.FillReferenceTopics;
var SR : TSearchRec; x: integer; path: string;
begin
  FReferenceTopics.Clear;
  path := gSettings.AppPath+'Help\'+gSettings.Language+'\reference\';
  if not DirectoryExists(path) then
    path := gSettings.AppPath+'Help\en\reference\';
  x := FindFirst(path + '*', 0, SR);
  if x = 0 then repeat
    FReferenceTopics.Add(
      StringReplace(ChangeFileExt(SR.Name,''),'mr_','',[])+'='+path+SR.Name
    );
    x := FindNext(SR);
  until x <> 0;
  FindClose(SR);
end;

procedure Tmp3MainForm.RefreshAvailableTranslations;

  procedure AddFlag(ALanguageCode: string);
  var bmp: TBitmap;
  begin
    bmp := TBitmap.Create;
    bmp.LoadFromFile(gSettings.AppPath+'Locale\'+ALanguageCode+'\flag.bmp');
    try
      ilLanguages.AddMasked(bmp, clNone);
    finally
      bmp.Free;
    end;
  end;

var i: integer;
begin
  // refresh language codes
  FAvailableTranslations.Clear;
  gnugettext.DefaultInstance.GetListOfLanguages(
    gnugettext.DefaultInstance.getcurrenttextdomain,
    FAvailableTranslations
  );
  // refresh language flags
  ilLanguages.Clear;
  AddFlag(DEFAULT_LANGUAGE_CODE);
  for i := 0 to FAvailableTranslations.Count - 1 do
    AddFlag(FAvailableTranslations[i]);
end;

procedure Tmp3MainForm.LoadInCodeEditor(AFilename: string);
var inProj: boolean;
begin
  inProj := FProjectManager.HasItemLoaded and assigned(
      FProjectManager.CurrentProject.SourceFiles.GetSourceFileByName(AFilename));
  FMainFrame.NewCodeEditor(
    AFilename, inProj, (gSettings.MaxBackups <> BACKUPS_DISABLED) and inProj
  );
end;

procedure Tmp3MainForm.LoadInHexEditor(AFilename: string);
begin
  FMainFrame.NewHexEditor(AFilename);
end;

procedure Tmp3MainForm.LoadInImageEditor(AFilename: string);
begin
  FMainFrame.NewImageEditor(AFilename);
end;

procedure Tmp3MainForm.LoadProject(AFilename: string);
begin
  if AFilename='' then
    exit;
  if FProjectManager.HasItemLoaded then
    actCloseProject.Execute;
  FProjectManager.Load(AFilename);
  if FProjectManager.HasItemLoaded then
  begin
    FProjectManager.CurrentProject.MaxBackups := gSettings.MaxBackups;  
    FProjectManager.RefreshManager;
    with FProjectManager.CurrentProject.SourceFiles do
      if (ProgramFile <> nil) and ProgramFile.Exists then
        LoadInCodeEditor(ProgramFile.GetFullname);
    // j-a-s-d: Niksa has this in his TODO list
    SetTitleBarCaption('['+FProjectManager.CurrentProject.MidletInfo.Name+']');
    RefreshActions;
    try
      SendToRecents(AFilename);
    finally
      RefreshSubmenus;
    end;
  end else
    MessageDlg(_('Could not load project'),mtError,[mbOk],0);
end;

procedure Tmp3MainForm.Load(AFilename: string);
begin
  case RetrieveFileKind(AFilename) of
  fkGroup:
    LoadGroup(AFilename);
  fkProject:
    LoadProject(AFilename);
  fkSourceFile:
    LoadInCodeEditor(AFilename);
  fkImage:
    LoadInImageEditor(AFilename);
  fkOther:
    LoadInHexEditor(AFilename);
  end;
end;

procedure Tmp3MainForm.LoadGroup(AFilename: string);
begin
  if AFilename='' then
    exit;
  if FGroupManager.HasItemLoaded then
    actCloseGroup.Execute;
  FGroupManager.Load(AFilename);
  if FGroupManager.HasItemLoaded then begin
    FGroupManager.RefreshManager;
    LoadProject(FGroupManager.ActiveProject);
  end else
    MessageDlg(_('Could not load group'),mtError,[mbOk],0);
end;

procedure Tmp3MainForm.OnClearRecentProjectsExecute(Sender: TObject);
begin
  if MessageDlg(_('Are you sure you want to clean it?'),mtConfirmation,mbYesNo,0)=mrYes then begin
    gSettings.Recents.Delete;
    RefreshSubmenus;
  end;
end;

procedure Tmp3MainForm.OnCodeEditorHistory(Sender: TObject);
var st: TStringList; AProject: Tmp3Project; ASourceFile: Tmp3SourceFile;
begin
  AProject := FProjectManager.CurrentProject;
  if not assigned(AProject) then
    exit;
  ASourceFile := AProject.SourceFiles.GetSourceFileByName(
    FMainFrame.CurrentEditor.Filename);
  if not assigned(ASourceFile) then
    exit;
  st := TStringList.Create;
  try
    AProject.RetrieveBackUpsForFile(ASourceFile,st);
    Tmp3CodeEditorFrame(Sender).SetHistoricalItems(st.Text);
  finally
    st.Free;
  end;
end;

procedure Tmp3MainForm.OnCodeEditorCaretMove(Sender: TObject);
begin
  RefreshStatusBarEditorInfo;
end;

procedure Tmp3MainForm.OnCodeEditorGetHelpOnWord(Sender: TObject);
var AProject: Tmp3Project; x: integer;
begin
  AProject := FProjectManager.CurrentProject;
  if not assigned(AProject) then
    exit;
  x := GetReferenceTopic(Tmp3CodeEditorFrame(Sender).GetWordAtCursor);
  if x > -1 then
    ShellExecute(0,'open',pchar(FReferenceTopics.ValueFromIndex[x]),'','',1)
  else
    actOpenHelp.Execute;
end;

procedure Tmp3MainForm.OnCodeEditorOpenFileAtCursor(Sender: TObject);
var s: string; AProject: Tmp3Project; ASourceFile: Tmp3SourceFile;
begin
  AProject := FProjectManager.CurrentProject;
  if not assigned(AProject) then
    exit;
  s := Tmp3CodeEditorFrame(Sender).GetWordAtCursor;
  ASourceFile := AProject.SourceFiles.GetSourceFileByName(s);
  if assigned(ASourceFile) then
    LoadInCodeEditor(ASourceFile.GetFullName)
  else begin
    dlgOpen.Filename := s;
    if dlgOpen.Execute then
      Load(dlgOpen.Filename);
    dlgOpen.Filename := '';
  end;
end;

procedure Tmp3MainForm.OnCodeEditorPreprocess(Sender: TObject);
var ftc,buildDir: string; AProject: Tmp3Project; ASourceFile: Tmp3SourceFile;
begin
  AProject := FProjectManager.CurrentProject;
  if not assigned(AProject) then
    exit;
  ASourceFile := AProject.SourceFiles.GetSourceFileByName(
    FMainFrame.CurrentEditor.Filename);
  if not assigned(ASourceFile) then
    exit;
  buildDir := AProject.ProjectDirectory+IntToHex(Random(-1),8)+'\';
  ForceDirectories(buildDir);
  ftc := buildDir+ChangeFileExt(ExtractFileName(ASourceFile.Filename),EXTENSION_PREPROCESSED);
  if PreprocessSourceFile(AProject.SourceDirectory,
    ExtractFileName(ASourceFile.Filename), ftc,
    FProjectManager.CurrentProject.GetActiveConfigurationName) then
  begin
    with TStringList.Create do
    try
      LoadFromFile(ftc);
      Tmp3CodeEditorFrame(Sender).SetPreprocessedCode(Text);
    finally
      Free;
    end;
  end else
    FCompilerMessagesPanel.Visible := true;
  DeleteFile(ftc);
  RemoveDir(buildDir);
end;

procedure Tmp3MainForm.OnCodeEditorRollback(Sender: TObject);
var AProject: Tmp3Project; ASourceFile: Tmp3SourceFile; h: integer;
begin
  h := Tmp3CodeEditorFrame(Sender).GetCurrentHistoricalVersion;
  if h > -1 then begin
    if MessageDlg(_('Are you sure you want to rollback?'),
      mtConfirmation, mbYesNo, 0) = mrNo then
      exit;
    AProject := FProjectManager.CurrentProject;
    if not assigned(AProject) then
      exit;
    ASourceFile := AProject.SourceFiles.GetSourceFileByName(
      FMainFrame.CurrentEditor.Filename);
    if not assigned(ASourceFile) then
      exit;
    AProject.RollbackSourceFileToBackupVersion(ASourceFile, h);
    Tmp3CodeEditorFrame(Sender).Load(ASourceFile.GetFullName);
    Tmp3CodeEditorFrame(Sender).SetFocusOnEditor;
  end;
end;

procedure Tmp3MainForm.OnCodeEditorSendToBuffer(Sender: TObject);
var AProject: Tmp3Project; ASourceFile: Tmp3SourceFile; h: integer;
begin
  h := Tmp3CodeEditorFrame(Sender).GetCurrentHistoricalVersion;
  if h > -1 then begin
    if MessageDlg(_('Are you sure you want to overwrite your buffer?'),
      mtConfirmation, mbYesNo, 0) = mrNo then
      exit;
    AProject := FProjectManager.CurrentProject;
    if not assigned(AProject) then
      exit;
    ASourceFile := AProject.SourceFiles.GetSourceFileByName(
      FMainFrame.CurrentEditor.Filename);
    if not assigned(ASourceFile) then
      exit;
    Tmp3CodeEditorFrame(Sender).Content :=
      AProject.GetSourceFileContentFromBackupVersion(ASourceFile, h);
    Tmp3CodeEditorFrame(Sender).SetFocusOnEditor;
  end;
end;

procedure Tmp3MainForm.OnCompilerMessagesPanelClose(Sender: TObject);
begin
  actCompilerMessagesPanel.Checked := false;
end;

procedure Tmp3MainForm.OnDoubleClickLine(Sender: TObject; ALine: string);
var fn: string; sf: Tmp3SourceFile;
begin
  if StartsText('[Pascal ', ALine) then begin
    System.Delete(ALine,1,Pos(']',ALine)+1);
    fn := System.Copy(ALine,1,Pos('.',ALine)-1);
    System.Delete(ALine,1,Pos('(',ALine));
    ALine := System.Copy(ALine,1,Pos(')',ALine)-1);
    sf := FProjectManager.CurrentProject.SourceFiles.GetSourceFileByName(fn);
    if assigned(sf) then
      FMainFrame.GotoSourceFileLine(sf.GetFullName, StrToIntDef(ALine,0));
  end;
end;

procedure Tmp3MainForm.OnOpenRecentProjectExecute(Sender: TObject);
begin
  if assigned(Sender) and (Sender is TAction) then
    LoadProject(TAction(Sender).Caption);
end;

procedure Tmp3MainForm.OnGroupManagerClose(Sender: TObject);
begin
  actGroupManager.Checked := false;
end;

procedure Tmp3MainForm.OnGroupProjectChange(Sender: TObject);
begin
  LoadProject(FGroupManager.ActiveProject);
end;

procedure Tmp3MainForm.OnLanguageClick(Sender: TObject);
begin
  if not assigned(Sender) or (TAction(Sender).Tag = -1) then
    SetLanguage(DEFAULT_LANGUAGE_CODE)
  else
    SetLanguage(FAvailableTranslations[TAction(Sender).Tag]);
end;

procedure Tmp3MainForm.OnProjectManagerClose(Sender: TObject);
begin
  actProjectManager.Checked := false;
end;

procedure Tmp3MainForm.OnProjectManagerEditResourceFile(Sender: TObject);
var rf: Tmp3ResourceFile;
begin
  rf := Tmp3ResourceFile(Sender);
  if assigned(rf) and rf.Exists then
    if rf.Kind=fkImage then
      FMainFrame.NewImageEditor(rf.GetFullname)
    else
      FMainFrame.NewHexEditor(rf.GetFullname);
end;

procedure Tmp3MainForm.OnProjectManagerEditSourceFile(Sender: TObject);
var sf: Tmp3SourceFile;
begin
  sf := Tmp3SourceFile(Sender);
  if assigned(sf) and sf.Exists then
    FMainFrame.NewCodeEditor(sf.GetFullname);
end;

procedure Tmp3MainForm.OnRunningEmulatorFinish(Sender: TObject;
  ExitCode: LongWord);
begin
  //actRun.Enabled := true;
end;

function Tmp3MainForm.PrepareCompilerMessagesPanel: boolean;
begin
  try
    if not FCompilerMessagesPanel.IsReady then begin
      FCompilerMessagesPanel.Clear;
      if not actCompilerMessagesPanel.Checked then
        actCompilerMessagesPanel.Execute;
    end;
  finally
    result := true;
  end;
end;

procedure Tmp3MainForm.OnActiveTabChange(Sender: TObject; TabIndex: Integer);
begin
  RefreshActions;
  RefreshStatusBarEditorInfo;
end;

procedure Tmp3MainForm.SendToRecents(AFilename: string);
begin
  if not gSettings.Recents.Has(AFilename) then
    gSettings.Recents.Add(AFilename)
  else
    gSettings.Recents.MoveToTop(AFilename);
end;

procedure Tmp3MainForm.SetLanguage(ALanguageCode: string);
begin
  gSettings.Language := ALanguageCode;
  RefreshLanguageActions;
  RefreshSubmenus;
  RefreshTranslation;
end;

procedure Tmp3MainForm.SetTitleBarCaption(ACaption: string = '');
begin
  if ACaption = '' then
    TitleBar.Caption := PROJECT_NAME + ' ' + PROJECT_VERSION
  else
    TitleBar.Caption := PROJECT_NAME + ' ' + PROJECT_VERSION + ' - ' +
      ACaption;
  // j-a-s-d: WINE displays the Form's Caption
  Caption := TitleBar.Caption;
end;

procedure Tmp3MainForm.actOpenScriptsFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.ScriptsDirectory),1);
end;

procedure Tmp3MainForm.actOpenSourceFileExecute(Sender: TObject);
begin
  FProjectManager.ExecuteCurrentElement;
end;

procedure Tmp3MainForm.actOpenSourceFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.SourceDirectory),1);
end;

procedure Tmp3MainForm.actOpenResourceFileExecute(Sender: TObject);
begin
  FProjectManager.ExecuteCurrentElement;
end;

procedure Tmp3MainForm.actOpenResourceFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.ResourceDirectory),1);
end;

procedure Tmp3MainForm.actStopBuildProcessExecute(Sender: TObject);
begin
  StopBuilding;
  actStopBuildProcess.Enabled := false;
  UpdateStatusBarText(_('stopping...'));
end;

procedure Tmp3MainForm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure Tmp3MainForm.actFindTextExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).ShowFindDialog;
end;

procedure Tmp3MainForm.actFormatSourceCodeExecute(Sender: TObject);

  function GetSourceCodeToFormat: widestring;
  begin
    with Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).SynEdit do begin
      if SelText = '' then
        SelectAll;
      result := SelText;
    end;
  end;

var s: widestring;
begin
  if assigned(FMainFrame.CurrentEditor) then
    if MessageDlg(_('Are you sure you want to format it?'),
      mtConfirmation, mbYesNo, 0) = mrYes then
      with TsitDelforObjectPascalFormatter.Create do
      try
        s := Format(GetSourceCodeToFormat);
        if s <> '' then
          Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).SynEdit.SelText := s;
      finally
        Free;
      end;
end;

procedure Tmp3MainForm.actFindNextExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).FindNext;
end;

procedure Tmp3MainForm.actFindPreviousExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).FindPrevious;
end;

procedure Tmp3MainForm.actFullScreenExecute(Sender: TObject);
begin
  actFullScreen.Checked := not actFullScreen.Checked;
  WindowState := wsNormal;
  TitleBar.FullScreenMaximize := actFullScreen.Checked;
  if actFullScreen.Checked then
    WindowState := wsMaximized;
end;

procedure Tmp3MainForm.actGotoExecute(Sender: TObject);
var value: integer; c,p: widestring;
begin
  if assigned(FMainFrame.CurrentEditor) then begin
    if FMainFrame.CurrentEditor.Kind = ekCode then begin
      c := _('Go to Line Number...');
      p := _('Enter new Line Number:');
    end else if FMainFrame.CurrentEditor.Kind = ekHex then begin
      c := _('Go to Offset...');
      p := _('Enter new Offset:');
    end;
    value := StrToIntDef(InputBox(c,p,''),-1);
    if value > -1 then
      with FMainFrame.CurrentEditor do begin
        GotoOffset(value);
        SetFocusOnEditor;
      end;
  end;
end;

procedure Tmp3MainForm.actMidletStyleExecute(Sender: TObject);
begin
  gSettings.CodeEditorStyle := CODE_EDITOR_STYLE_MIDLET;
  FMainFrame.RefreshCodeEditorStyle;
  RefreshStyleActions;
end;

procedure Tmp3MainForm.actMinimizeToTrayExecute(Sender: TObject);
begin
  actMinimizeToTray.Checked := not actMinimizeToTray.Checked;
  gSettings.MinimizeToTray := actMinimizeToTray.Checked;
end;

procedure Tmp3MainForm.OnDowloadProgress(ADownloadedBytes: integer);
begin
  Application.ProcessMessages;
  if GetTickCount > FLastDownloadProgressMessageTickCount + MESSAGE_DELAY then begin
    FLastDownloadProgressMessageTickCount := GetTickCount;
    UpdateStatusBarText(_('Downloaded:')+' '+IntToStr(ADownloadedBytes)+' bytes');
  end;
end;

procedure Tmp3MainForm.actCheckForUpdatesExecute(Sender: TObject);
var x: integer;
begin
  actCheckForUpdates.Enabled := false;
  try
    with gCore.WebUpdate do
      if CheckForUpdates and (AvailableUpdates.Count > 0) then begin
        x := ShowPickDialog;
        if x > -1 then
          if DownloadUpdate(AvailableUpdates[x]) then
            UpdateStatusBarText
          else
            MessageDlg(_('Error downloading.'),mtError,[mbOk],0);
      end else
        MessageDlg(_('There is no updates available.'),mtInformation,[mbOk],0);
  finally
     actCheckForUpdates.Enabled := true;
  end;
end;

procedure Tmp3MainForm.actClassicStyleExecute(Sender: TObject);
begin
  gSettings.CodeEditorStyle := CODE_EDITOR_STYLE_CLASSIC;
  FMainFrame.RefreshCodeEditorStyle;
  RefreshStyleActions;
end;

procedure Tmp3MainForm.actOpenHelpExecute(Sender: TObject);
begin
  ShellExecute(0,'open',pchar(gSettings.HelpFile),'','',1);
end;

procedure Tmp3MainForm.actGroupManagerExecute(Sender: TObject);
begin
  actGroupManager.Checked := not actGroupManager.Checked;
  FGroupManager.Visible := actGroupManager.Checked;
end;

procedure Tmp3MainForm.InternalPerformBuild(var Msg: TMessage);
begin
  if FGroupManager.HasItemLoaded then
    actBuildAllProjectsFromHere.Execute
  else if FProjectManager.HasItemLoaded then
    actBuild.Execute;
  WaitUntilBuildFinishes;
end;

procedure Tmp3MainForm.PerformBuild;
begin
  PostMessage(Handle,WM_PERFORMBUILD,0,0);
end;

procedure Tmp3MainForm.BuildNextInGroup;
begin
  with FGroupManager do begin
    if ActiveProjectIndex = (CurrentGroup.Projects.Count-1) then begin
      FBuildingGroup := false;
      exit;
    end;
    ActiveProject := CurrentGroup.Projects[FGroupManager.ActiveProjectIndex+1];
    actBuildAllProjectsFromHere.Execute;
  end;
end;

procedure Tmp3MainForm.actBackupsLimitExecute(Sender: TObject);
begin
  gSettings.MaxBackups := TAction(Sender).Tag;
  if FProjectManager.HasItemLoaded then
    FProjectManager.CurrentProject.MaxBackups := gSettings.MaxBackups;
  RefreshBackupsActions;
end;

procedure Tmp3MainForm.actBuildAllProjectsFromHereExecute(Sender: TObject);
begin
  FBuildingGroup := true;
  actBuild.Execute;
  FBuildNextInGroup := true;
end;

procedure Tmp3MainForm.actOpenProjectExecute(Sender: TObject);
begin
  FGroupManager.ActiveProject := FGroupManager.SelectedItemName;
end;

procedure Tmp3MainForm.actOpenProjectFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.ProjectDirectory),1);
end;

procedure Tmp3MainForm.actExcludeProjectExecute(Sender: TObject);
begin
  with FGroupManager do
    if HasItemLoaded then begin
      if MessageDlg(_('Are you sure you want to exclude it?'),mtConfirmation,mbYesNo,0)=mrYes then
        if CurrentGroup.Projects.Exclude(SelectedItemName) then
          Save;
      RefreshManager;
    end;
end;

procedure Tmp3MainForm.actIncludeProjectExecute(Sender: TObject);
var old: string;
begin
  old := dlgOpen.Filter;
  dlgOpen.Filter := _('MIDletPascal Project Files')+' '+FILTER_PROJECTFILES;
  if dlgOpen.Execute then
    with FGroupManager do
      if HasItemLoaded then begin
        if CurrentGroup.Projects.Include(dlgOpen.Filename) then
          Save;
        RefreshManager;
        if CurrentGroup.Projects.Count=1 then
          ActiveProject := CurrentGroup.Projects[0];
      end;
  dlgOpen.Filter := old;
end;

procedure Tmp3MainForm.actAboutExecute(Sender: TObject);
begin
  with Tmp3AboutForm.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure Tmp3MainForm.actOpenBinFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.OutputDirectory),1);
end;

procedure Tmp3MainForm.actOpenClassesFolderExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    ShellExecute(0,'open','','',pchar(FProjectManager.CurrentProject.ClassesDirectory),1);
end;

procedure Tmp3MainForm.actOpenDemosFolderExecute(Sender: TObject);
begin
  ShellExecute(0,'open',pchar(gSettings.ConfigPath+'Demos'),'','',1);
end;

procedure Tmp3MainForm.actOpenExecute(Sender: TObject);
begin
  if dlgOpen.Execute then
    Load(dlgOpen.Filename);
end;

procedure Tmp3MainForm.actSaveCompilerMessagesExecute(Sender: TObject);
begin
  if dlgSaveCompilerMessages.Execute then
    FCompilerMessagesPanel.SaveToFile(dlgSaveCompilerMessages.Filename);
end;

procedure Tmp3MainForm.actSaveFileExecute(Sender: TObject);
var sf: Tmp3SourceFile;
begin
  if assigned(FMainFrame.CurrentEditor) then
    if FMainFrame.CurrentEditor.IsNew or (FMainFrame.CurrentEditor.Filename = '')
      or (not FileExists(FMainFrame.CurrentEditor.Filename)) then begin
        dlgSave.FileName := FMainFrame.CurrentEditor.Filename;
        if dlgSave.Execute then
          if (not FileExists(dlgSave.Filename)) or (MessageDlg(
            _('Are you sure you want to overwrite it?'),mtConfirmation,mbYesNo,0)=mrYes) then
            FMainFrame.CurrentEditor.SaveAs(dlgSave.Filename);
    end else begin
      if (gSettings.MaxBackups <> BACKUPS_DISABLED) and FProjectManager.HasItemLoaded then begin
        sf := FProjectManager.CurrentProject.SourceFiles.GetSourceFileByName(
          FMainFrame.CurrentEditor.Filename
        );
        if assigned(sf) then
          FProjectManager.CurrentProject.GenerateNewBackupFor(sf);
      end;
      FMainFrame.CurrentEditor.Save;
    end;
end;

procedure Tmp3MainForm.actSaveProjectExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then
    FProjectManager.Save;
end;

procedure Tmp3MainForm.actClearCompilerMessagesExecute(Sender: TObject);
begin
  FCompilerMessagesPanel.Clear;
end;

procedure Tmp3MainForm.actCloseAllFilesExecute(Sender: TObject);
begin
  FStopCloseAll := false;
  while FMainFrame.FocusLastEditor and (not FStopCloseAll) do
    actCloseFile.Execute;
  RefreshActions;
end;

procedure Tmp3MainForm.actCloseFileExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then begin
    if FMainFrame.CurrentEditor.Modified then
      case MessageDlg(_('Do you want to save the changes?'),mtConfirmation,mbYesNoCancel,0) of
        mrCancel:
          begin
            FStopCloseAll:=true;
            exit;
          end;
        mrYes:
          FMainFrame.CurrentEditor.Save;
      end;
    FMainFrame.CloseCurrentEditor;
    if (not assigned(FMainFrame.CurrentEditor))
      and FProjectManager.HasItemLoaded then
        FProjectManager.Activate;
    RefreshActions;
  end;
end;

procedure Tmp3MainForm.actCloseGroupExecute(Sender: TObject);
begin
  if FGroupManager.HasItemLoaded then begin
    FGroupManager.Close;
    FGroupManager.RefreshManager;
    RefreshActions;
  end;
end;

procedure Tmp3MainForm.actCloseProjectExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded then begin
    if FBuilding then begin
      actStopBuildProcess.Execute;
      WaitUntilBuildFinishes;
    end;
    actCloseAllFilesExecute(nil);
    FProjectManager.Close;
    FProjectManager.RefreshManager;
    SetTitleBarCaption;
    RefreshActions;
  end;
end;

procedure Tmp3MainForm.actCutExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Cut;
end;

procedure Tmp3MainForm.actCopyExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Copy;
end;

procedure Tmp3MainForm.actPasteExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Paste;
end;

procedure Tmp3MainForm.actCompilerMessagesPanelExecute(Sender: TObject);
begin
  actCompilerMessagesPanel.Checked := not actCompilerMessagesPanel.Checked;
  FCompilerMessagesPanel.Visible := actCompilerMessagesPanel.Checked;
end;

procedure Tmp3MainForm.actCopyCompilerMessageExecute(Sender: TObject);
begin
  FCompilerMessagesPanel.Copy;
end;

procedure Tmp3MainForm.actSelectAllCompilerMessagesExecute(Sender: TObject);
begin
  FCompilerMessagesPanel.SelectAll;
end;

procedure Tmp3MainForm.actProjectManagerExecute(Sender: TObject);
begin
  actProjectManager.Checked := not actProjectManager.Checked;
  FProjectManager.Visible := actProjectManager.Checked;
end;

procedure Tmp3MainForm.actDeleteExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Delete;
end;

procedure Tmp3MainForm.actDeleteResourceFileExecute(Sender: TObject);
var b: boolean;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      if CheckBoxMessageDlg(_('Are you sure you want to delete it?'),mtConfirmation,mbYesNo,0,
        b,_('Delete it from disk'))=mrYes then
        if CurrentProject.ResourceFiles.Delete(SelectedItemName,b) then
          Save;
      RefreshManager;
    end;
end;

procedure Tmp3MainForm.actDeleteSourceFileExecute(Sender: TObject);
var b: boolean;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      b := false;
      if CheckBoxMessageDlg(_('Are you sure you want to delete it?'),mtConfirmation,mbYesNo,0,
        b,_('Delete it from disk'))=mrYes then
        if CurrentProject.SourceFiles.Delete(SelectedItemName,b) then
          Save;
      RefreshManager;
    end;
end;

procedure Tmp3MainForm.actRecreateSourceFileExecute(Sender: TObject);
begin
  with FProjectManager do
    if HasItemLoaded then begin
      if CurrentProject.SourceFiles.Ensure(SelectedItemName) then
        Save;
      RefreshManager;
    end;
end;

procedure Tmp3MainForm.actSelectAllExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.SelectAll;
end;

procedure Tmp3MainForm.actSetDefaultProjectLocationExecute(Sender: TObject);
var s: string;
begin
  s := InputBox(_('Project Location'), _('Please, enter a new value:'), gSettings.DefaultProjectLocation);
  if IsValidPath(s) then
    gSettings.DefaultProjectLocation := s
  else
    ShowError(_('You have entered a invalid path.'));
end;

procedure Tmp3MainForm.actSetFontExecute(Sender: TObject);
begin
  dlgFont.Font.Name := gSettings.CodeEditorFontName;
  dlgFont.Font.Size := gSettings.CodeEditorFontSize;
  if dlgFont.Execute then begin
    gSettings.CodeEditorFontName := dlgFont.Font.Name;
    gSettings.CodeEditorFontSize := dlgFont.Font.Size;
    FMainFrame.RefreshCodeEditorStyle;
  end;
end;

procedure Tmp3MainForm.actUndoExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Undo;
end;

procedure Tmp3MainForm.actVisitTheProjectSiteExecute(Sender: TObject);
begin
  ShellExecute(0,'open',PROJECT_SITE_URL,'','',1);
end;

procedure Tmp3MainForm.actRedoExecute(Sender: TObject);
begin
  if assigned(FMainFrame.CurrentEditor) then
    FMainFrame.CurrentEditor.Redo;
end;

procedure Tmp3MainForm.actRenameBuildConfigurationExecute(Sender: TObject);
var NewName: string;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      NewName := InputBox(_('Rename Build Configuration'),_('Enter the new name:'),SelectedItemName);
      if not SameText(NewName,SelectedItemName) then
        if CurrentProject.BuildConfigurations.Rename(SelectedItemName,NewName) then begin
          Save;
          RefreshManager;
        end;
    end;
end;

procedure Tmp3MainForm.actRenameResourceFileExecute(Sender: TObject);
var OldName, NewName: string; resourcefile: Tmp3ResourceFile; editor: TsitEditorFrame;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      NewName := InputBox(_('Rename Source File'),_('Enter the new name:'),SelectedItemName);
      OldName := SelectedItemName;
      if not SameText(NewName,OldName) then
        if CurrentProject.ResourceFiles.Rename(OldName,NewName) then begin
          Save;
          RefreshManager;
          // refresh tab if was being edited
          resourcefile := FProjectManager.CurrentProject.ResourceFiles.GetResourceFileByName(NewName);
          if assigned(resourcefile) then begin
            editor := FMainFrame.FindEditorByFile(OldName);
            if assigned(editor) then
              editor.Filename := resourcefile.GetFullName;
          end;
        end;
    end;
end;

procedure Tmp3MainForm.actRenameSourceFileExecute(Sender: TObject);
var OldName,NewName: string; sourcefile: Tmp3SourceFile; editor: TsitEditorFrame;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      NewName := InputBox(_('Rename Source File'),_('Enter the new name:'),SelectedItemName);
      OldName := SelectedItemName;
      if not SameText(NewName,OldName) then
        if CurrentProject.SourceFiles.Rename(OldName,NewName) then begin
          Save;
          RefreshManager;
          // refresh tab if was being edited
          sourcefile := FProjectManager.CurrentProject.SourceFiles.GetSourceFileByName(NewName);
          if assigned(sourcefile) then begin
            editor := FMainFrame.FindEditorByFile(OldName);
            if assigned(editor) then
            editor.Filename := sourcefile.GetFullName;
          end;
        end;
    end;
end;

procedure Tmp3MainForm.actReplaceTextExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).ShowReplaceDialog;
end;

procedure Tmp3MainForm.actCommentSelectedLinesExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).ToggleCommentInSelectedLines;
end;

procedure Tmp3MainForm.actCommentSelectedTextExecute(Sender: TObject);
begin
  if Assigned(FMainFrame.CurrentEditor)and(FMainFrame.CurrentEditor.Kind=ekCode) then
    Tmp3CodeEditorFrame(FMainFrame.CurrentEditor).ToggleCommentInSelectedText;
end;

procedure Tmp3MainForm.actCompileCurrentFileExecute(Sender: TObject);
var s: string; sf: Tmp3SourceFile;
begin
  if FProjectManager.HasItemLoaded then begin
    sf := FProjectManager.CurrentProject.SourceFiles.GetSourceFileByName(
      FMainFrame.CurrentEditor.Filename);
    if assigned(sf) then
      if (not FMainFrame.HasEditorOpened) then
        UpdateStatusBarText(_('ERROR: you are not editing any file'))
      else begin
        PrepareCompilerMessagesPanel;
        BuildSourceFile(FProjectManager.CurrentProject, sf, s);
        UpdateStatusBarText(s);
      end;
  end else
    UpdateStatusBarText(_('ERROR: the file must belong to a project'));
end;

procedure Tmp3MainForm.HandleTaskTerminated(const task: IOmniTaskControl);
var bSuccess: boolean;
begin
  bSuccess := FCompilerError = '';
  if not bSuccess then
    FCompilerMessagesPanel.ShowCompilerMessage(FCompilerError)
  else
    FCompilerMessagesPanel.ShowCompilerMessage(_('Finished at')+' '+DateTimeToStr(Now));
  FCompilerTask := nil;
  UpdateStatusBarText;
  FBuilding := false;
  RefreshCompilerActions;
  if bSuccess then begin
    if FRunBuiltMIDlet then
      actRun.Execute;
    if FBuildNextInGroup then
      BuildNextInGroup;
  end else
    FBuildNextInGroup := false;
end;

procedure Tmp3MainForm.HandleTaskMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  case msg.MsgID of
    0: FCompilerMessagesPanel.ShowCompilerMessage(msg.MsgData.AsString);
    1: FCompilerMessagesPanel.SetCompilerProgress(msg.MsgData.AsCardinal);
  end;
end;

procedure Tmp3MainForm.ShowCompilerMessage(AMessage: string);
var msg: TOmniMessage;
begin
  if FCompilerTask = nil then
    FCompilerMessagesPanel.ShowCompilerMessage(AMessage)
  else begin
    msg.MsgID := 0;
    msg.MsgData.AsString := AMessage;
    FCompilerTask.Comm.Send(msg);
  end;
end;

procedure Tmp3MainForm.ShowCompilerProgress(AValue: DWORD);
var msg: TOmniMessage;
begin
  if FCompilerTask = nil then
    FCompilerMessagesPanel.SetCompilerProgress(AValue)
  else begin
    msg.MsgID := 1;
    msg.MsgData.AsCardinal := AValue;
    FCompilerTask.Comm.Send(msg);
  end;
end;

procedure Tmp3MainForm.OnInstallUpdateClick(Sender: TObject);
begin
  case MessageDlg(_('Do you want to install the update?')+#10+
    _('Click NO to discard.')+#10+_('Click CANCEL to decide later.'),
    mtConfirmation, mbYesNoCancel, 0) of
    mrNo:
      begin
        gCore.WebUpdate.DiscardInstallationPendingUpdate;
        UpdateStatusBarText;
      end;
    mrYes:
      if gCore.WebUpdate.LaunchDownloadedUpdate then
        Close;
  end;
end;

procedure Tmp3MainForm.UpdateStatusBarText(AText: widestring = ' ');

  procedure SetRegularStatusBarText;
  begin
    StatusBarText.OnClick := nil;
    StatusBarText.Cursor := crArrow;
    StatusBarText.Caption := AText;
    StatusBarText.Font.Style := [];
    StatusBarText.Font.Color := clBlack;
  end;

  procedure Wait(ASleepTime: integer);
  var c: word;
  begin
    c := 0;
    repeat
      Application.ProcessMessages;
      Sleep(1);
      inc(c);
    until c = ASleepTime;
  end;

begin
  if gCore.WebUpdate.HasInstallationPendingUpdate then begin
    if AText <> ' ' then begin
      SetRegularStatusBarText;
      Wait(MESSAGE_DELAY);
    end;
    StatusBarText.OnClick := OnInstallUpdateClick;
    StatusBarText.Cursor := crHandPoint;
    StatusBarText.Caption := _('Click here to apply the downloaded update.');
    StatusBarText.Font.Style := [fsUnderline];
    StatusBarText.Font.Color := clBlue;
  end else
    SetRegularStatusBarText;
end;

procedure Tmp3MainForm.BackgroundBuild(const task: IOmniTask);
begin
  FCompilerTask := task;
  RefreshCompilerActions;
  Build(FProjectManager.CurrentProject, FCompilerError);
end;

procedure Tmp3MainForm.actBuildExecute(Sender: TObject);
begin
  if FProjectManager.HasItemLoaded and (not FBuilding) then begin
    FBuildNextInGroup := false;
    FBuilding := true;
    PrepareCompilerMessagesPanel;
    UpdateStatusBarText(_('building...'));
    FCompilerMessagesPanel.ShowCompilerMessage(_('Building')+' '+FProjectManager.CurrentProject.Filename);
    FCompilerMessagesPanel.ShowCompilerMessage(_('Started at')+' '+DateTimeToStr(Now));
    if mp3ProjectBuilding.CompilerMessageHandlerHandle <> INVALID_HANDLE_VALUE then
      FCompilerMessagesPanel.ShowCompilerMessage(_('Compiler Communication Method')+': WM_COPTYDATA (Win32.Wow64)')
    else
      FCompilerMessagesPanel.ShowCompilerMessage(_('Compiler Communication Method')+': Console Redirection (Win32.Common)');
    FMessageDispatch.Monitor(CreateTask(BackgroundBuild,'BackgroundBuild')).Run;
  end;
end;

procedure Tmp3MainForm.actRunExecute(Sender: TObject);
var s: string;
begin
  FRunBuiltMIDlet := false;
  if FProjectManager.HasItemLoaded then begin
    ExecuteScript(FProjectManager.CurrentProject,SCRIPT_BEFORE_RUN);
    s := FProjectManager.CurrentProject.ExpandRunCommandLine(
      gSettings.CurrentEmulatorCommandLine
    );
    FCompilerMessagesPanel.Visible := True;
    FCompilerMessagesPanel.ShowCompilerMessage(_('Launching')+' '+widestring(s));
    //actRun.Enabled := false;
    FRunningEmulator.CommandLine := s;
    FRunningEmulator.Execute;
  end;
end;

procedure Tmp3MainForm.actAddBuildConfigurationExecute(Sender: TObject);
var lName: string;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      lName := InputBox(_('Add Build Configuration'),_('Enter the name:'),'');
      if (lName <> '') and CurrentProject.BuildConfigurations.Add(lName) then begin
        Save;
        RefreshManager;
      end;
    end;
end;

procedure Tmp3MainForm.actDeleteBuildConfigurationExecute(Sender: TObject);
begin
  with FProjectManager do
    if HasItemLoaded then begin
      if MessageDlg(_('Are you sure you want to delete it?'),mtConfirmation,mbYesNo,0)=mrYes then
        if CurrentProject.BuildConfigurations.Delete(SelectedItemName) then
          Save;
      RefreshManager;
    end;
end;

procedure Tmp3MainForm.actAddResourceFileExecute(Sender: TObject);
var old: string;
begin
  old := dlgOpen.Filter;
  dlgOpen.Filter := _('All Files')+' '+FILTER_ALLFILES;
  if dlgOpen.Execute then
    with FProjectManager do
      if HasItemLoaded then begin
        if CurrentProject.ResourceFiles.Add(dlgOpen.FileName) then
          Save
        else
          MessageDlg(_('Could not add resource'),mtError,[mbOk],0);
        RefreshManager;
      end;
  dlgOpen.Filter := old;
end;

procedure Tmp3MainForm.actAddSourceFileExecute(Sender: TObject);
var old: string;
begin
  old := dlgOpen.Filter;
  dlgOpen.Filter := _('MIDletPascal Source Files')+' '+FILTER_SOURCEFILES;
  if dlgOpen.Execute then
    with FProjectManager do
      if HasItemLoaded then begin
        if CurrentProject.SourceFiles.Add(dlgOpen.Filename) then
          Save;
        RefreshManager;
      end;
  dlgOpen.Filter := old;
end;

procedure Tmp3MainForm.actBuildAndRunExecute(Sender: TObject);
begin
  FRunBuiltMIDlet := true;
  actBuild.Execute;
end;

procedure Tmp3MainForm.actNewGroupExecute(Sender: TObject);
begin
  with TTurboUIItemWithLocationDialog.Create(Self) do
  try
    ItemName := DEFAULT_GROUP_NAME;
    ItemLocation := gSettings.DefaultProjectLocation;
    SetTitle(_('New Group')+'...');
    SetItemNameLabel(_('Group Name'));
    SetItemLocationLabel(_('Group Location'));
    if ShowModal = mrOk then begin
      if FGroupManager.HasItemLoaded then
        actCloseGroup.Execute;
      FGroupManager.New(ItemName,ItemLocation);
      FGroupManager.RefreshManager;
    end;
  finally
    Free;
  end;
  RefreshActions;
end;

procedure Tmp3MainForm.actNewImageExecute(Sender: TObject);
begin
  with TTurboUIItemWithSizeDialog.Create(Self) do
  try
    SetTitle(_('New Image')+'...');
    SetItemNameLabel(_('Name'));
    SetItemWidthLabel(_('Width'));
    SetItemHeightLabel(_('Height'));
    ItemName := DEFAULT_IMAGE_NAME;
    ItemWidth := DEFAULT_IMAGE_WIDTH;
    ItemHeight := DEFAULT_IMAGE_HEIGHT;
    if ShowModal = mrOk then
      FMainFrame.NewImageEditor(ChangeFileExt(ItemName,EXTENSION_IMAGE),ItemWidth,ItemHeight,true);
  finally
    Free;
  end;
end;

procedure Tmp3MainForm.actNewProjectExecute(Sender: TObject);
begin
  with TTurboUIItemWithLocationDialog.Create(Self) do
  try
    ItemName := DEFAULT_PROJECT_NAME;
    ItemLocation := gSettings.DefaultProjectLocation;
    SetTitle(_('New Project')+'...');
    SetItemNameLabel(_('Project Name'));
    SetItemLocationLabel(_('Project Location'));
    if ShowModal = mrOk then begin
      if (length(ItemName)<UNIT_NAME_LENGTH_MINIMUM) then begin
        MessageDlg(_('Name is invalid: the minimum length is')+' '
          +IntToStr(UNIT_NAME_LENGTH_MINIMUM)+'.',mtError,[mbOk],0);
        exit;
      end;
      if FProjectManager.HasItemLoaded then
        actCloseAllFilesExecute(nil);
      if FProjectManager.New(ItemName,ItemLocation) then
      begin
        FProjectManager.RefreshManager;
        LoadInCodeEditor(FProjectManager.CurrentProject.SourceFiles.ProgramFile.GetFullname);
        try
          SendToRecents(FProjectManager.CurrentProject.Filename);
        finally
          RefreshSubmenus;
        end;
      end;
    end;
  finally
    Free;
  end;
end;

procedure Tmp3MainForm.actNewResourceFileExecute(Sender: TObject);
var lName, cName: string; nu: boolean;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      with TTurboUIItemWithSizeDialog.Create(Self) do
      try
        SetTitle(_('New Image')+'...');
        SetItemNameLabel(_('Name'));
        SetItemWidthLabel(_('Width'));
        SetItemHeightLabel(_('Height'));
        ItemName := DEFAULT_IMAGE_NAME;
        ItemWidth := DEFAULT_IMAGE_WIDTH;
        ItemHeight := DEFAULT_IMAGE_HEIGHT;
        if ShowModal = mrOk then begin
          lName := ChangeFileExt(ItemName,EXTENSION_IMAGE);
          cName := CurrentProject.ResourceFiles.Directory + lName;
          nu := not FileExists(cName);
          FMainFrame.NewImageEditor(cName,ItemWidth,ItemHeight,nu);
          if nu then
            FMainFrame.CurrentEditor.SaveAs(cName);
          if CurrentProject.ResourceFiles.Add(lName) then begin
            Save;
            RefreshManager;
          end;
        end;
      finally
        Free;
      end;
    end;
end;

function IsLibraryName(AName: string): boolean;

  procedure RetrieveLibraries(var ALibList: TStringList);
  var SR : TSearchRec; x: integer;
  begin
    x := FindFirst(gSettings.LibrariesDirectory+EXTERNAL_LIBRARY_PREFIX+'*'
      +EXTENSION_CLASS, 0, SR);
    if x = 0 then repeat
      ALibList.Add(SR.Name);
      x := FindNext(SR);
    until x <> 0;
    FindClose(SR);
  end;

var st: TStringList;
begin
  st := TStringList.Create;
  try
    RetrieveLibraries(st);
    result := st.IndexOf(EXTERNAL_LIBRARY_PREFIX+AName+EXTENSION_CLASS) > -1;
  finally
    st.Free;
  end;
end;

procedure Tmp3MainForm.actNewSourceFileExecute(Sender: TObject);
var lName, uName: string;
begin
  with FProjectManager do
    if HasItemLoaded then begin
      lName := InputBox(_('New Source File'),_('Enter the name:'),'');
      if (lName <> '') then begin
        if ExtractFileExt(lName) = '' then
          lName := ChangeFileExt(lName,EXTENSION_SOURCEFILE);
        uName := ChangeFileExt(lName,'');
        if (length(uName)<UNIT_NAME_LENGTH_MINIMUM) then
          MessageDlg(_('Name is invalid: the minimum length is')+' '
            +IntToStr(UNIT_NAME_LENGTH_MINIMUM)+'.',mtError,[mbOk],0)
        else if IsLibraryName(uName) then
          MessageDlg(_('Name is invalid: there is a Library with the same name.'),mtError,[mbOk],0)
        else if CurrentProject.SourceFiles.New(lName) then begin
          Save;
          RefreshManager;
        end;
      end;
    end;
end;

procedure Tmp3MainForm.actNewUnitExecute(Sender: TObject);
begin
  FMainFrame.NewCodeEditor('',false,false,true);
end;

procedure Tmp3MainForm.actPreviousTabExecute(Sender: TObject);
begin
  with FMainFrame.TabControl do
    if ActiveTabIndex = 0 then
      ActiveTabIndex := PagesCount - 1
    else
      ActiveTabIndex := ActiveTabIndex - 1;
end;

procedure Tmp3MainForm.actNextTabExecute(Sender: TObject);
begin
  with FMainFrame.TabControl do
    if ActiveTabIndex = PagesCount - 1 then
      ActiveTabIndex := 0
    else
      ActiveTabIndex := ActiveTabIndex + 1;
end;

procedure Tmp3MainForm.OnDropFileHandler(const AFilename: string);
begin
  if FileExists(AFilename) then
    Load(AFilename);
end;

procedure Tmp3MainForm.RefreshActions;
var i: integer; eo, pl, gl: boolean; ce: TsitEditorFrame;
begin
  eo := FMainFrame.HasEditorOpened;
  ce := FMainFrame.CurrentEditor;
  pl := FProjectManager.HasItemLoaded;
  gl := FGroupManager.HasItemLoaded;
  actSaveFile.Enabled := eo;
  actCloseFile.Enabled := eo;
  actCloseAllFiles.Enabled := eo;
  actSaveProject.Enabled := pl;
  actCloseProject.Enabled := pl;
  actCloseGroup.Enabled := gl;
  for i := 0 to al_actProject.ActionCount - 1 do
    TAction(al_actProject.Actions[i]).Enabled := pl;
  for i := 0 to al_actEdit.ActionCount - 1 do
    TAction(al_actEdit.Actions[i]).Enabled := eo and(ce.Kind<>ekImage);
  for i := 0 to al_actSearch.ActionCount - 1 do
    TAction(al_actSearch.Actions[i]).Enabled := eo and(ce.Kind=ekCode);
  actStopBuildProcess.Enabled := FBuilding;
end;

procedure Tmp3MainForm.RefreshBackupsActions;
var x: integer;
begin
  for x := 0 to al_actKeepHistory.ActionCount - 1 do
    TAction(al_actKeepHistory.Actions[x]).Checked :=
      al_actKeepHistory.Actions[x].Tag = gSettings.MaxBackups;
end;

procedure Tmp3MainForm.RefreshCompilerActions;
var b, eo, pl: boolean; ce: TsitEditorFrame;
begin
  b := CompilerPresent and (not FBuilding);
  eo := FMainFrame.HasEditorOpened;
  ce := FMainFrame.CurrentEditor;
  pl := FProjectManager.HasItemLoaded;
  actCompileCurrentFile.Enabled := b and eo and assigned(ce)and(ce.Kind=ekCode)and pl and
    FProjectManager.CurrentProject.FileBelongsToProject(ce.Filename);
  actBuild.Enabled := b and pl;
  actRun.Enabled := b and pl;
  actBuildAndRun.Enabled := b and pl;
  actStopBuildProcess.Enabled := FBuilding;
end;

procedure Tmp3MainForm.RefreshLanguageActions;
var i: integer; action: TAction;
begin
  ClearActionList(al_actLanguage);
  action := TAction.Create(Self);
  action.Caption := getlanguagename(DEFAULT_LANGUAGE_CODE);
  action.OnExecute := OnLanguageClick;
  action.Tag := -1;
  action.ActionList := al_actLanguage;
  action.Checked := gSettings.Language = DEFAULT_LANGUAGE_CODE;
  action.ImageIndex := 0;
  for i := 0 to FAvailableTranslations.Count-1 do begin
    action := TAction.Create(Self);
    action.Caption := getlanguagename(FAvailableTranslations[i]);
    action.OnExecute := OnLanguageClick;
    action.Tag := i;
    action.ActionList := al_actLanguage;
    action.Checked := gSettings.Language = FAvailableTranslations[i];
    action.ImageIndex := i + 1;
  end;
end;

procedure Tmp3MainForm.OnMainFramePopup(Sender: TObject);

  procedure AddSeparator;
  begin
    TtuiPopupMenu(Sender).Items.Add(TtuiSeparatorMenuItem.Create(FMainFrame));
  end;

  procedure AddSubmenu(AActionList: TActionList);
  var ti: TtuiMenuItem; i: integer; action: TAction;
  begin
    for i := 0 to AActionList.ActionCount - 1 do begin
      action := TAction(AActionList.Actions[i]);
      if action.Visible then
        if action.Caption = '-' then
          AddSeparator
        else if action.Enabled then begin
          ti := TtuiMenuItem.Create(FMainFrame);
          ti.Action := action;
          TtuiPopupMenu(Sender).Items.Add(ti);
        end;
    end;
  end;

begin
  if assigned(Sender) then begin
    AddSeparator;
    AddSubmenu(al_actEdit);
    AddSeparator;
    AddSubmenu(al_actSearch);
  end;
end;

procedure Tmp3MainForm.OnManageCodeEditorStylesExecute(Sender: TObject);
begin
  with Tmp3CodeEditorStylesDialog.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
  FMainFrame.RefreshCodeEditorStyle;
  RefreshSubmenus(true);
end;

procedure Tmp3MainForm.OnManageEmulatorsExecute(Sender: TObject);
begin
  with Tmp3EmulatorsDialog.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
  RefreshSubmenus;
end;

procedure Tmp3MainForm.OnSetDefaultEmulatorExecute(Sender: TObject);
var i: integer;
begin
  if assigned(Sender) and (Sender is TAction) then begin
    for i := al_actEmulators.ActionCount-1 downto 0 do
      TAction(al_actEmulators.Actions[i]).Checked := false;
    gSettings.CurrentEmulatorName := TAction(Sender).Caption;
    TAction(Sender).Checked := true;
  end;
end;

procedure Tmp3MainForm.RefreshEmulators;
var i: integer; action: TAction;
begin
  ClearActionList(al_actEmulators);
  // be sure current emulator exists
  if gSettings.Emulators.IndexOfName(gSettings.CurrentEmulatorName)=-1 then
    gSettings.CurrentEmulatorName := CONFIG_EMULATOR_SECTION_CURRENT_EMULATOR_NAME_DEFAULT;
  // add emulators
  for i := 0 to EMULATORS_MAX - 1 do
  begin
    action := TAction.Create(Self);
    if i <= gSettings.Emulators.Count - 1 then
      action.Caption := gSettings.Emulators.Names[i]
    else
      action.Visible := false;
    action.Checked := SameText(action.Caption,gSettings.CurrentEmulatorName);
    action.OnExecute := OnSetDefaultEmulatorExecute;
    action.ActionList := al_actEmulators;
  end;
  // add separator
  action := TAction.Create(Self);
  action.Caption := '-';
  action.ActionList := al_actEmulators;
  // add manage
  action := TAction.Create(Self);
  action.Caption := _('Manage...');
  action.OnExecute := OnManageEmulatorsExecute;
  action.ActionList := al_actEmulators;
end;

procedure Tmp3MainForm.RefreshReopen;
var i: integer; st: TStringList; action: TAction;
begin
  ClearActionList(al_actReopen);
  st := TStringList.Create;
  try
    gSettings.Recents.GetRecents(st);
    if actReopen.Enabled then begin
      for i := MRUINI_MAX-1 downto 0 do begin
        action := TAction.Create(Self);
        if i <= st.Count-1 then
          action.Caption := st[i]
        else
          action.Visible := false;
        action.OnExecute := OnOpenRecentProjectExecute;
        action.ActionList := al_actReopen;
      end;
      // add separator
      action := TAction.Create(Self);
      action.Caption := '-';
      action.ActionList := al_actReopen;
      // add clear recent projects
      action := TAction.Create(Self);
      action.Caption := _('Clear');
      action.OnExecute := OnClearRecentProjectsExecute;
      action.ActionList := al_actReopen;
    end;
  finally
    st.Free;
  end;
end;

procedure Tmp3MainForm.OnSetCodeEditorStyle(Sender: TObject);
begin
  if assigned(Sender) and (Sender is TAction) then begin
    gSettings.CodeEditorStyle := TAction(Sender).Caption;
    FMainFrame.RefreshCodeEditorStyle;
    RefreshSubmenus;
  end;
end;

procedure Tmp3MainForm.RefreshStyleActions;
var i: integer; action: TAction; o: boolean;
begin
  ClearActionList(al_actCodeEditorStyle);
  o := false;
  gSettings.CodeEditorStyles.Refresh;
  for i := 0 to gSettings.CodeEditorStyles.List.Count - 1 do begin
    action := TAction.Create(Self);
    action.Caption := gSettings.CodeEditorStyles.List[i];
    action.Checked := SameText(gSettings.CodeEditorStyle, action.Caption);
    o := o or action.Checked;
    action.OnExecute := OnSetCodeEditorStyle;
    action.ActionList := al_actCodeEditorStyle;
  end;
  action := TAction.Create(Self);
  action.Caption := CODE_EDITOR_STYLE_CLASSIC;
  action.Checked := (not o) or SameText(gSettings.CodeEditorStyle, action.Caption);
  action.OnExecute := OnSetCodeEditorStyle;
  action.ActionList := al_actCodeEditorStyle;
  // add separator
  action := TAction.Create(Self);
  action.Caption := '-';
  action.ActionList := al_actCodeEditorStyle;
  // add manage
  action := TAction.Create(Self);
  action.Caption := _('Manage...');
  action.OnExecute := OnManageCodeEditorStylesExecute;
  action.ActionList := al_actCodeEditorStyle;
  // add separator
  action := TAction.Create(Self);
  action.Caption := '-';
  action.ActionList := al_actCodeEditorStyle;
  // add set font
  action := TAction.Create(Self);
  action.Caption := _('Set &Font');
  action.OnExecute := actSetFontExecute;
  action.ActionList := al_actCodeEditorStyle;
end;

procedure Tmp3MainForm.OnSkinExecute(Sender: TObject);
begin
  if assigned(Sender) and (Sender is TAction) then begin
    TurboUIColorManager.Skin := TAction(Sender).Caption;
    OnlyCheckThisActionInActionList(al_actSkin, TAction(Sender));
  end;
end;

procedure Tmp3MainForm.RefreshSkins;
var i: integer; action: TAction; st: TStringList; s: string;
begin
  ClearActionList(al_actSkin);
  st := TStringList.Create;
  try
    TurboUIColorManager.GetAvailableSkins(st);
    s := gSettings.CurrentSkin;
    for i := 0 to st.Count - 1 do
    begin
      action := TAction.Create(Self);
      action.Caption := st[i];
      action.OnExecute := OnSkinExecute;
      action.ActionList := al_actSkin;
      action.Checked := SameText(st[i], s);
    end;
  finally
    st.Free;
  end;
end;

procedure Tmp3MainForm.RefreshSubmenus(AAvoidRetranslation: boolean = false);
begin
  RefreshReopen;
  RefreshSkins;
  RefreshEmulators;
  RefreshStyleActions;
  if AAvoidRetranslation then
    SetSubmenus(alMainMenu)
  else begin
    MainMenu.BeginUpdate;
    try
      UseLanguage('en');
      SetSubmenus(alMainMenu);
      RetranslateComponent(Self);
      RefreshTranslation;
    finally
      MainMenu.EndUpdate;
    end;
  end;
end;

procedure Tmp3MainForm.RefreshStatusBarEditorInfo;
var x, y: integer; ce: TsitEditorFrame;
begin
  ce := FMainFrame.CurrentEditor;
  if assigned(ce) and (ce.Kind = ekCode) then
    with Tmp3CodeEditorFrame(ce) do
    begin
      GetCaretPosition(x, y);
      SetTabControlText(IntToStr(y)+' : '+IntToStr(x));
    end;
end;

procedure Tmp3MainForm.RefreshTranslation;
begin
  UseLanguage(gSettings.Language);
  RetranslateComponent(Self);
  RetranslateComponent(FMainFrame);
  FMainFrame.RefreshTranslation;
  RetranslateComponent(FGroupManager);
  FGroupManager.RefreshManager;
  RetranslateComponent(FProjectManager);
  FProjectManager.RefreshManager;
  RetranslateComponent(FCompilerMessagesPanel);
  dlgOpen.Filter := _('All MIDletPascal Files')+' '+FILTER_ALLMPFILES+'|'+
    _('MIDletPascal Project Files')+' '+FILTER_PROJECTFILES+'|'+
    _('MIDletPascal Source Files')+' '+FILTER_SOURCEFILES+'|'+
    _('MIDletPascal Group Files')+' '+FILTER_GROUPFILES+'|'+
    _('All Files')+' '+FILTER_ALLFILES;
  dlgSave.Filter := _('All Files')+' '+FILTER_ALLFILES;
  dlgSaveCompilerMessages.Filter := _('All Files')+' '+FILTER_ALLFILES;
  if FProjectManager.HasItemLoaded then  
    SetTitleBarCaption('['+FProjectManager.CurrentProject.MidletInfo.Name+']');
  SpTBXMessageDlg.mbYesCaption := _('Yes');
  SpTBXMessageDlg.mbNoCaption := _('No');
  SpTBXMessageDlg.mbOKCaption := _('OK');
  SpTBXMessageDlg.mbCancelCaption := _('Cancel');
  SpTBXMessageDlg.mdcWarning := _('Warning');
  SpTBXMessageDlg.mdcError := _('Error');
  SpTBXMessageDlg.mdcInformation := _('Information');
  SpTBXMessageDlg.mdcConfirm := _('Confirm');
  SpTBXInputBox.mbOKCaption := _('OK');
  SpTBXInputBox.mbCancelCaption := _('Cancel');
end;

procedure Tmp3MainForm.WaitUntilBuildFinishes;
begin
  repeat
    Application.ProcessMessages;
  until (not FBuilding)and(not FBuildingGroup);
end;

procedure Tmp3MainForm.ReadSettings;
begin
  actMinimizeToTray.Checked := gSettings.MinimizeToTray;
  TurboUIColorManager.Skin := gSettings.CurrentSkin;
  if SameText(gSettings.ProjectManagerPosition,FMainFrame.DockRight.Name) then
    FProjectManager.Parent := FMainFrame.DockRight;
  if SameText(gSettings.GroupManagerPosition,FMainFrame.DockRight.Name) then
    FGroupManager.Parent := FMainFrame.DockRight;
  FMainFrame.DockLeft.Width := gSettings.LeftDockWidth;
  FMainFrame.DockRight.Width := gSettings.RightDockWidth;
  if gSettings.GroupManager <> actGroupManager.Checked then
      actGroupManager.Execute;
  if gSettings.ProjectManager <> actProjectManager.Checked then
      actProjectManager.Execute;
  if gSettings.FullScreen <> actFullScreen.Checked then
      actFullScreen.Execute;
end;

procedure Tmp3MainForm.WriteSettings;
begin
  gSettings.CurrentSkin := TurboUIColorManager.Skin;
  gSettings.GroupManager := actGroupManager.Checked;
  gSettings.ProjectManager := actProjectManager.Checked;
  gSettings.GroupManagerPosition := FGroupManager.Parent.Name;
  gSettings.ProjectManagerPosition := FProjectManager.Parent.Name;
  gSettings.LeftDockWidth := FMainFrame.DockLeft.Width;
  gSettings.RightDockWidth := FMainFrame.DockRight.Width;
  gSettings.FullScreen := actFullScreen.Checked;
end;

{$IFDEF WOW64_WORKAROUND}
procedure Tmp3MainForm.CompilerMessageHandle(var Msg: TMessage);
begin
  Tmp3CompilerInvoker.ShowCompilerNewLine(Self,
    PChar(PCopyDataStruct(Msg.LParam).lpData), true);
end;
{$ENDIF}

end.
