(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3CodeEditorFrame;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Math,
  Controls, StdCtrls, ComCtrls, Graphics, Grids,
  SpTBXTabs,
  TB2Dock,
  Diff, HashUnit,
  SynEdit,
  UniSynHighlighterPas,
  SynUnicode,
  SynEditTypes,
  SynEditMiscClasses, SynEditSearch, SynEditRegexSearch,
  SynEditAutoComplete, SynEditKeyCmds,
  SynEditHighlighter,
  gnugettext,
  tuiFindDialog, tuiReplaceDialog, tuiControls,
  sitEditorFrame, sitSynCodeEditorStyle, sitSynCodeEditorStylePas,
  mp3FileKind, mp3Consts, mp3Settings;

type
  Tmp3CodeEditorFrame = class(TsitEditorFrame)
  private
    FTabControl: TtuiTabControl;
    FCodeTab: TtuiTabItem;
    FPreprocessTab: TtuiTabItem;
    FHistoryTab: TtuiTabItem;
    FSynEdit: TSynEdit;
    FPreprocessSynEdit: TSynEdit;
    FHistoryContentsSynEdit: TSynEdit;
    FAutoComplete: TSynAutoComplete;
    FOnPreprocess: TNotifyEvent;
    FOnHistory: TNotifyEvent;
    FOnOpenFileAtCursor: TNotifyEvent;
    FOnGetHelpOnWord: TNotifyEvent;
    FOnRollback: TNotifyEvent;
    FOnSendToBuffer: TNotifyEvent;
    FHistoryContentsComboBox: TtuiComboBox;
    FDiff1StringGrid: TStringGrid;
    FDiff2StringGrid: TStringGrid;
    FDiff1Source: TUnicodeStringList;
    FDiff2Source: TUnicodeStringList;
    FDiff1ComboBox: TtuiComboBox;
    FDiff2ComboBox: TtuiComboBox;
    FDiff1Panel: TtuiPanel;
    FDiff2Panel: TtuiPanel;
    FDiff1HashList: TList;
    FDiff2HashList: TList;
    FDiff: TDiff;
    FDiffGutterColor: TColor;
    FDiffModifyColor: TColor;
    FDiffDeleteColor: TColor;
    FDiffAddColor: TColor;
    FHistoryTabControl: TtuiTabControl;
    FHistoryContentsTab: TtuiTabItem;
    FHistoryDiffTab: TtuiTabItem;
    FSearch: TSynEditSearch;
    FSearchRegex: TSynEditRegexSearch;
    FSearchCaseSensitive: boolean;
    FSearchWholeWords: boolean;
    FSearchText: string;
    FReplaceText: string;
    FReplaceAll: boolean;
    FHistoryDockTop: TtuiDock;
    FHistoryToolbar: TtuiToolbar;
    FHistoryRollbackButton: TtuiMenuItem;
    FHistorySendToBufferButton: TtuiMenuItem;
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure GridTopLeftChanged(Sender: TObject);
    procedure OnEditorChange(Sender: TObject);
    procedure OnEditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnHistoryContentsComboBoxChange(Sender: TObject);
    procedure OnDiff1ComboBoxChange(Sender: TObject);
    procedure OnDiff2ComboBoxChange(Sender: TObject);
    function GetContent: string;
    procedure SetContent(const Value: string);
    procedure OnPreprocessClick(Sender: TObject);
    procedure OnHistoryClick(Sender: TObject);
    procedure WhenResizing(Sender: TObject);
    procedure Diff;
    function DoSearchReplaceText(ABackwards, AReplace: boolean): boolean;
    function SortHistoricalItemsByIndex(AItems: string): string;
    procedure OnRollbackClick(Sender: TObject);
    procedure OnSendToBufferClick(Sender: TObject);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Load(AFilename: string); override;
    procedure Save; override;
    procedure SaveAs(AFilename: string); override;
    procedure SetFocusOnEditor; override;
    procedure GotoOffset(AOffset: integer); override;
    procedure Undo; override;
    procedure Redo; override;
    procedure Cut; override;
    procedure Copy; override;
    procedure Paste; override;
    procedure Delete; override;
    procedure SelectAll; override;
    procedure ShowFindDialog;
    procedure ShowReplaceDialog;
    function FindNext: boolean;
    function FindPrevious: boolean;
    procedure SetPreprocessedCode(ACode: string);
    procedure SetHistoricalItems(AItems: string);
    procedure SetColorSpeedSetting(AColorSetting: string);
    procedure SetPreprocessTabVisibility(AValue: boolean);
    procedure SetHistoryTabVisibility(AValue: boolean);
    function GetWordAtCursor: string;
    function GetCurrentHistoricalVersion: integer;
    property SynEdit: TSynEdit read FSynEdit;
    property Content: string
      read GetContent write SetContent;
    property OnPreprocess: TNotifyEvent
      read FOnPreprocess write FOnPreprocess;
    property OnHistory: TNotifyEvent
      read FOnHistory write FOnHistory;
    property OnOpenFileAtCursor: TNotifyEvent
      read FOnOpenFileAtCursor write FOnOpenFileAtCursor;
    property OnGetHelpOnWord: TNotifyEvent
      read FOnGetHelpOnWord write FOnGetHelpOnWord;
    property OnRollback: TNotifyEvent
      read FOnRollback write FOnRollback;
    property OnSendToBuffer: TNotifyEvent
      read FOnSendToBuffer write FOnSendToBuffer;
  end;

implementation

uses
  tuiColorManager;

{$R *.dfm}

{ Tmp3CodeEditorFrame }

procedure Tmp3CodeEditorFrame.AfterConstruction;

  procedure InitCodeTab;
  begin
    FCodeTab := FTabControl.Add(_('Code'));
    FSynEdit := TSynEdit.Create(Self);
    FSynEdit.Parent := FTabControl.Pages[0];
    FSynEdit.Align := alClient;
    FSynEdit.BorderStyle := bsNone;
    FSynEdit.WantTabs := true;
    FSynEdit.WantReturns := true;
    FSynEdit.TabWidth := 2;
    FSynEdit.Options := [
      eoAltSetsColumnMode,       //Holding down the Alt Key will put the selection mode into columnar format
      eoAutoIndent,              //Will indent the caret on new lines with the same amount of leading white space as the preceding line
      eoAutoSizeMaxScrollWidth,  //Automatically resizes the MaxScrollWidth property when inserting text
      eoDisableScrollArrows,     //Disables the scroll bar arrow buttons when you can't scroll in that direction any more
      eoDragDropEditing,         //Allows you to select a block of text and drag it within the document to another location
      eoEnhanceHomeKey,          //enhances home key positioning, similar to visual studio
      eoEnhanceEndKey,           //enhances End key positioning, similar to JDeveloper
      eoKeepCaretX,              //When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
      eoRightMouseMovesCursor,   //When clicking with the right mouse for a popup menu, move the cursor to that location
      eoShowScrollHint,          //Shows a hint of the visible line numbers when scrolling vertically
      eoTabIndent,               //When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
      eoTabsToSpaces,            //Converts a tab character to a specified number of space characters
      eoTrimTrailingSpaces       //Spaces at the end of lines will be trimmed and not saved
    ];
    FSynEdit.ActiveLineColor := clNone;
    FSynEdit.BookMarkOptions.EnableKeys := true;
    FSynEdit.OnChange := OnEditorChange;
    FSynEdit.OnKeyDown := OnEditorKeyDown;

    // gutter set up
    with FSynEdit.Gutter do begin
      ShowLineNumbers := true;
      DigitCount := 3;
      LeadingZeros := false;
      AutoSize := true;
      Gradient := true;
      GradientStartColor := TurboUIColorManager.Getcolor(tccPanelGradientStart);
      GradientEndColor := TurboUIColorManager.Getcolor(tccPanelGradientEnd);
      GradientSteps := 100;
    end;

    // highlighter set up
    FSynEdit.Highlighter := TSynPasSyn.Create(nil);
    TSynPasSyn(FSynEdit.Highlighter).DelphiVersion := dvMidletPascal3;

    // auto complete
    FAutoComplete := TSynAutoComplete.Create(Self);
    FAutoComplete.Editor := FSynEdit;
    FAutoComplete.AutoCompleteList.Text := DEFAULT_AUTO_COMPLETE_LIST;
    FSynEdit.AddKey(ecAutoCompletion, word('J'), [ssCtrl], 0, []);

    // search engine
    FSearch := TSynEditSearch.Create(Self);
    FSearchRegex := TSynEditRegexSearch.Create(Self);
    FSynEdit.SearchEngine := FSearch;
  end;

  procedure InitPreprocessTab;
  begin
    FPreprocessTab := FTabControl.Add(_('Preprocess'));
    FPreprocessTab.OnClick := OnPreprocessClick;
    FPreprocessSynEdit := TSynEdit.Create(Self);
    FPreprocessSynEdit.Parent := FTabControl.Pages[1];
    FPreprocessSynEdit.Align := alClient;
    FPreprocessSynEdit.BorderStyle := bsNone;
    FPreprocessSynEdit.ReadOnly := true;
  end;

  procedure InitHistoryTab;

    procedure InitContentsTab;
    begin
      FHistoryContentsTab := FHistoryTabControl.Add(_('Contents'));
      FHistoryDockTop := TtuiDock.Create(Self);
      FHistoryDockTop.Parent := FHistoryTabControl.Pages[0];
      FHistoryDockTop.Position := dpTop;
      FHistoryDockTop.AllowDrag := false;
      FHistoryDockTop.Align := alTop;
      FHistoryDockTop.Height := 16;
      FHistoryToolbar := TtuiToolbar.Create(Self);
      FHistoryToolbar.Parent := FHistoryDockTop;
      FHistoryRollbackButton := TtuiMenuItem.Create(Self);
      FHistoryRollbackButton.Caption := _('Rollback to this version');
      FHistoryRollbackButton.OnClick := OnRollbackClick;
      FHistorySendToBufferButton := TtuiMenuItem.Create(Self);
      FHistorySendToBufferButton.Caption := _('Send to buffer');
      FHistorySendToBufferButton.OnClick := OnSendToBufferClick;
      FHistoryToolbar.Items.Add(FHistoryRollbackButton);
      FHistoryToolbar.Items.Add(TtuiSeparatorMenuItem.Create(Self));
      FHistoryToolbar.Items.Add(FHistorySendToBufferButton);
      FHistoryContentsComboBox := TtuiComboBox.Create(Self);
      FHistoryContentsComboBox.Parent := FHistoryTabControl.Pages[0];
      FHistoryContentsComboBox.Align := alTop;
      FHistoryContentsComboBox.OnChange := OnHistoryContentsComboBoxChange;
      FHistoryContentsSynEdit := TSynEdit.Create(Self);
      FHistoryContentsSynEdit.Parent := FHistoryTabControl.Pages[0];
      FHistoryContentsSynEdit.Align := alClient;
      FHistoryContentsSynEdit.BorderStyle := bsNone;
      FHistoryContentsSynEdit.ReadOnly := true;
    end;

    procedure InitDiffTab;
    begin
      FHistoryDiffTab := FHistoryTabControl.Add(_('Difference'));
      FDiff1Panel := TtuiPanel.Create(Self);
      FDiff1Panel.Parent := FHistoryTabControl.Pages[1];
      FDiff1Panel.Borders := false;
      FDiff1Panel.Align := alLeft;
      FDiff1ComboBox := TtuiComboBox.Create(Self);
      FDiff1ComboBox.Parent := FDiff1Panel;
      FDiff1ComboBox.Align := alTop;
      FDiff1ComboBox.OnChange := OnDiff1ComboBoxChange;
      FDiff1StringGrid := TStringGrid.Create(Self);
      FDiff1StringGrid.Parent := FDiff1Panel;
      FDiff1StringGrid.Align := alClient;
      FDiff1StringGrid.BorderStyle := bsNone;
      FDiff1StringGrid.ParentFont := False;
      FDiff1StringGrid.OnDrawCell := GridDrawCell;
      FDiff1StringGrid.OnTopLeftChanged := GridTopLeftChanged;
      FDiff1StringGrid.ColCount := 2;
      FDiff1StringGrid.DefaultRowHeight := 17;
      FDiff1StringGrid.DefaultDrawing := False;
      FDiff1StringGrid.FixedCols := 0;
      FDiff1StringGrid.RowCount := 1;
      FDiff1StringGrid.FixedRows := 0;
      FDiff1StringGrid.GridLineWidth := 0;
      FDiff1StringGrid.Options := [goFixedVertLine, goFixedHorzLine,
        goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect];
      with TtuiSplitter.Create(Self) do begin
        Parent := FHistoryTabControl.Pages[1];
        Align := alLeft;
        Visible := true;
      end;
      FDiff2Panel := TtuiPanel.Create(Self);
      FDiff2Panel.Parent := FHistoryTabControl.Pages[1];
      FDiff2Panel.Borders := false;
      FDiff2Panel.Align := alClient;
      FDiff2ComboBox := TtuiComboBox.Create(Self);
      FDiff2ComboBox.Parent := FDiff2Panel;
      FDiff2ComboBox.Align := alTop;
      FDiff2ComboBox.OnChange := OnDiff2ComboBoxChange;
      FDiff2StringGrid := TStringGrid.Create(Self);
      FDiff2StringGrid.Parent := FDiff2Panel;
      FDiff2StringGrid.Align := alClient;
      FDiff2StringGrid.BorderStyle := bsNone;
      FDiff2StringGrid.ParentFont := False;
      FDiff2StringGrid.OnDrawCell := GridDrawCell;
      FDiff2StringGrid.OnTopLeftChanged := GridTopLeftChanged;
      FDiff2StringGrid.ColCount := 2;
      FDiff2StringGrid.DefaultRowHeight := 17;
      FDiff2StringGrid.DefaultDrawing := False;
      FDiff2StringGrid.FixedCols := 0;
      FDiff2StringGrid.RowCount := 1;
      FDiff2StringGrid.FixedRows := 0;
      FDiff2StringGrid.GridLineWidth := 0;
      FDiff2StringGrid.Options := [goFixedVertLine, goFixedHorzLine,
        goVertLine, goHorzLine, goDrawFocusSelected, goRowSelect];
      FDiff1Source := TUnicodeStringList.Create;
      FDiff2Source := TUnicodeStringList.Create;
      FDiff1HashList := TList.Create;
      FDiff2HashList := TList.Create;
      FDiff := TDiff.Create(Self);
    end;

  begin
    FHistoryTab := FTabControl.Add(_('History'));
    FHistoryTab.OnClick := OnHistoryClick;
    FHistoryTabControl := TtuiTabControl.Create(Self);
    FHistoryTabControl.Parent := FTabControl.Pages[2];
    FHistoryTabControl.Align := alClient;
    FHistoryTabControl.TabPosition := ttpBottom;
    InitContentsTab;
    InitDiffTab;
  end;

begin
  inherited;
  Kind := ekCode;
  FTabControl := TtuiTabControl.Create(Self);
  FTabControl.Parent := Self;
  FTabControl.Align := alClient;
  FTabControl.TabPosition := ttpBottom;
  InitCodeTab;
  InitPreprocessTab;
  InitHistoryTab;
  SetColorSpeedSetting(gSettings.CodeEditorStyle);
  FCodeTab.Click;
  OnResize := WhenResizing;
end;

procedure Tmp3CodeEditorFrame.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FDiff1Source);
  FreeAndNil(FDiff2Source);
  FreeAndNil(FDiff1HashList);
  FreeAndNil(FDiff2HashList);
  FreeAndNil(FAutoComplete);
  if assigned(FPreprocessSynEdit) then
    FPreprocessSynEdit.Highlighter := nil;
  if assigned(FSynEdit) then begin
    FSynEdit.SearchEngine.Free;
    FSynEdit.Highlighter.Free;
    FreeAndNil(FSynEdit);
  end;
end;

function GetFileContentAsString(AFilename: string): string;
var fs: TFileStream;
begin
  result := '';
  fs := TFileStream.Create(AFilename,0);
  try
    SetLength(result,fs.Size);
    fs.Read(result[1],fs.Size);
  finally
    FreeAndNil(fs);
  end;
end;

procedure Tmp3CodeEditorFrame.Load(AFilename: string);
const CRLF = #13#10; CRCRLF = #13 + CRLF;
var bom: boolean; ss: TStringStream;
begin
  Filename := AFilename;
  if FileExists(Filename) then
  begin
    ss := TStringStream.Create(
      StringReplace(GetFileContentAsString(Filename),CRCRLF,CRLF,[rfReplaceAll])
    );
    try
      LoadFromStream(FSynEdit.Lines,ss,GetEncoding(Filename,bom),bom);
    finally
      FreeAndNil(ss);
    end;
    //LoadFromFile(FSynEdit.Lines,Filename,GetEncoding(Filename,bom),bom);
  end;
  FSynEdit.ReadOnly := ReadOnly;
end;

procedure Tmp3CodeEditorFrame.OnHistoryContentsComboBoxChange(Sender: TObject);
var AHistoryFilename: string; bom: boolean;
begin
  AHistoryFilename := FHistoryContentsComboBox.Text;
  if FileExists(AHistoryFilename) then
    LoadFromFile(FHistoryContentsSynEdit.Lines,AHistoryFilename,GetEncoding(Filename,bom),bom);
end;

procedure Tmp3CodeEditorFrame.OnDiff1ComboBoxChange(Sender: TObject);
var AHistoryFilename: string; bom: boolean;
begin
  AHistoryFilename := FDiff1ComboBox.Text;
  if FileExists(AHistoryFilename) then begin
    LoadFromFile(FDiff1Source,AHistoryFilename,GetEncoding(Filename,bom),bom);
    Diff;
  end;
end;

procedure Tmp3CodeEditorFrame.OnDiff2ComboBoxChange(Sender: TObject);
var AHistoryFilename: string; bom: boolean;
begin
  AHistoryFilename := FDiff2ComboBox.Text;
  if FileExists(AHistoryFilename) then begin
    LoadFromFile(FDiff2Source,AHistoryFilename,GetEncoding(Filename,bom),bom);
    Diff;
  end;
end;

procedure Tmp3CodeEditorFrame.OnEditorChange(Sender: TObject);
begin
  Modified := true;
end;

procedure Tmp3CodeEditorFrame.OnEditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ssCtrl in Shift then
    case Key of
      VK_RETURN:
        if assigned(FOnOpenFileAtCursor) then
          FOnOpenFileAtCursor(Self);
      VK_F1:
        if assigned(FOnGetHelpOnWord) then
          FOnGetHelpOnWord(Self);
    end;
end;

procedure Tmp3CodeEditorFrame.OnHistoryClick(Sender: TObject);
begin
  if assigned(FOnHistory) then
    FOnHistory(Self);
end;

procedure Tmp3CodeEditorFrame.OnPreprocessClick(Sender: TObject);
begin
  if assigned(FOnPreprocess) then
    FOnPreprocess(Self);
end;

procedure Tmp3CodeEditorFrame.OnRollbackClick(Sender: TObject);
begin
  if assigned(FOnRollback) then
    FOnRollback(Self);
end;

procedure Tmp3CodeEditorFrame.OnSendToBufferClick(Sender: TObject);
begin
  if assigned(FOnSendToBuffer) then
    FOnSendToBuffer(Self);
end;

procedure Tmp3CodeEditorFrame.Save;
begin
  SaveToFile(FSynEdit.Lines,Filename,seUTF8,false);
  Modified := false;
end;

procedure Tmp3CodeEditorFrame.SaveAs(AFilename: string);
begin
  Filename := AFilename;
  Save;
end;

procedure Tmp3CodeEditorFrame.Copy;
begin
  FSynEdit.CopyToClipboard;
end;

procedure Tmp3CodeEditorFrame.Cut;
begin
  FSynEdit.CutToClipboard;
end;

procedure Tmp3CodeEditorFrame.Delete;
begin
  FSynEdit.Perform(WM_KEYDOWN,VK_DELETE,0);
end;

procedure Tmp3CodeEditorFrame.Paste;
begin
  FSynEdit.PasteFromClipboard;
end;

procedure Tmp3CodeEditorFrame.SelectAll;
begin
  FSynEdit.SelectAll;
end;

procedure Tmp3CodeEditorFrame.Undo;
begin
  FSynEdit.Undo;
end;

procedure Tmp3CodeEditorFrame.Redo;
begin
  FSynEdit.Redo;
end;

procedure Tmp3CodeEditorFrame.WhenResizing(Sender: TObject);
begin
  FDiff1Panel.Width := FHistoryTabControl.ClientWidth div 2;
  FDiff1StringGrid.ColWidths[1] := FDiff1Panel.ClientWidth-FDiff1StringGrid.ColWidths[0];
  FDiff2StringGrid.ColWidths[1] := FDiff1StringGrid.ColWidths[1];
end;

function Tmp3CodeEditorFrame.GetContent: string;
begin
  result := FSynEdit.Text;
end;

function Tmp3CodeEditorFrame.GetWordAtCursor: string;
begin
  result := FSynEdit.GetWordAtRowCol(FSynEdit.CaretXY);
end;

procedure Tmp3CodeEditorFrame.GotoOffset(AOffset: integer);
begin
  FSynEdit.GotoLineAndCenter(AOffset);
end;

procedure Tmp3CodeEditorFrame.SetContent(const Value: string);
begin
  FSynEdit.Text := Value;
  Modified := true;
end;

procedure Tmp3CodeEditorFrame.SetFocusOnEditor;
begin
  Tab.Click;
  FCodeTab.Click;
  FSynEdit.SetFocus;
end;

function GetHistoricalIndex(AFilename: string): integer;
var s: string;
begin
  s := ExtractFileExt(AFilename);
  Delete(s,1,1);
  result := StrToIntDef(StringReplace(s,'~','',[rfReplaceAll]),-1);
end;

function CompareIndexes(List: TStringList; Index1, Index2: Integer): Integer;
var i1, i2: integer;
begin
  i1 := GetHistoricalIndex(List[Index1]);
  i2 := GetHistoricalIndex(List[Index2]);
  if i1 > i2 then
    result := -1
  else if i1 < i2 then
    result := 1
  else
    result := 0;
end;

function Tmp3CodeEditorFrame.SortHistoricalItemsByIndex(AItems: string): string;
begin
  result := AItems;
  with TStringList.Create do
  try
    Text := result;
    CustomSort(CompareIndexes);
    result := Text;
  finally
    Free;
  end;
end;

procedure Tmp3CodeEditorFrame.SetHistoricalItems(AItems: string);

  procedure SetComboBox(AComboBox: TtuiComboBox);
  begin
    AComboBox.Items.Text := AItems;
    AComboBox.Enabled := AComboBox.Items.Count > 0;
    if AComboBox.Enabled then begin
      AComboBox.Items.Insert(0, Filename);
      AComboBox.ItemIndex := 0;
      if assigned(AComboBox.OnChange) then
        AComboBox.OnChange(nil);
    end;
  end;

begin
  AItems := SortHistoricalItemsByIndex(AItems);
  WhenResizing(nil); // be sure they will be half sized
  SetCombobox(FHistoryContentsComboBox);
  SetCombobox(FDiff1ComboBox);
  SetCombobox(FDiff2ComboBox);
end;

procedure Tmp3CodeEditorFrame.SetHistoryTabVisibility(AValue: boolean);
begin
  FHistoryTab.Visible := AValue;
end;

function Tmp3CodeEditorFrame.GetCurrentHistoricalVersion: integer;
begin
  result := GetHistoricalIndex(FHistoryContentsComboBox.Text);
end;

procedure Tmp3CodeEditorFrame.SetPreprocessedCode(ACode: string);
begin
  FPreprocessSynEdit.Text := ACode;
end;

procedure Tmp3CodeEditorFrame.SetPreprocessTabVisibility(AValue: boolean);
begin
  FPreprocessTab.Visible := AValue;
end;

procedure Tmp3CodeEditorFrame.ShowFindDialog;
begin
  with TTurboUIFindDialog.Create(Self) do
  try
    if FSynEdit.SelText <> '' then
      FindWhat := FSynEdit.SelText
    else
      FindWhat := FSearchText;
    WholeWords := FSearchWholeWords;
    CaseSensitive := FSearchCaseSensitive;
    RegularExpressions := FSynEdit.SearchEngine = FSearchRegex;
    if ShowModal=mrOk then begin
      if RegularExpressions then
        FSynEdit.SearchEngine := FSearchRegex
      else
        FSynEdit.SearchEngine := FSearch;
      FSearchWholeWords := WholeWords;
      FSearchCaseSensitive := CaseSensitive;
      FSearchText := FindWhat;
      FindNext;
    end;
  finally
    Free;
  end;
end;

procedure Tmp3CodeEditorFrame.ShowReplaceDialog;
begin
  with TTurboUIReplaceDialog.Create(Self) do
  try
    if FSynEdit.SelText <> '' then
      ReplaceWhat := FSynEdit.SelText
    else
      ReplaceWhat := FSearchText;
    ReplaceWith := '';
    CaseSensitive := FSearchCaseSensitive;
    if ShowModal=mrOk then begin
      FSynEdit.SearchEngine := FSearch;    
      FSearchCaseSensitive := CaseSensitive;
      FSearchText := ReplaceWhat;
      FReplaceText := ReplaceWith;
      FReplaceAll := ReplaceAll;
      DoSearchReplaceText(false,true);
    end;
  finally
    Free;
  end;
end;

function Tmp3CodeEditorFrame.DoSearchReplaceText(ABackwards, AReplace: boolean): boolean;
var Options: TSynSearchOptions; AReplaceText: string;
begin
  if AReplace then begin
    Options := [ssoPrompt, ssoReplace];
    if FReplaceAll then
      Include(Options, ssoReplaceAll);
    AReplaceText := FReplaceText;
  end else begin
    Options := [];
    if FSearchWholeWords then
      Include(Options, ssoWholeWord);
    AReplaceText := '';
  end;
  if ABackwards then
    Include(Options, ssoBackwards);
  if FSearchCaseSensitive then
    Include(Options, ssoMatchCase);
//  if not fSearchFromCaret then
//    Include(Options, ssoEntireScope);
//  if gbSearchSelectionOnly then
//    Include(Options, ssoSelectedOnly);
  result := FSynEdit.SearchReplace(FSearchText, AReplaceText, Options) <> 0;
  if not result then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      FSynEdit.BlockEnd := FSynEdit.BlockBegin
    else
      FSynEdit.BlockBegin := FSynEdit.BlockEnd;
    FSynEdit.CaretXY := FSynEdit.BlockBegin;
  end;
end;

function Tmp3CodeEditorFrame.FindNext: boolean;
begin
  result := DoSearchReplaceText(false,false);
end;

function Tmp3CodeEditorFrame.FindPrevious: boolean;
begin
  result := DoSearchReplaceText(true,false);
end;

procedure Tmp3CodeEditorFrame.SetColorSpeedSetting(AColorSetting: string);

  procedure SetStringGridPreferences(AStringGrid: TStringGrid);
  begin
    AStringGrid.Font.Assign(SynEdit.Font);
    AStringGrid.Canvas.Font.Assign(SynEdit.Font);
    AStringGrid.ColWidths[0] := SynEdit.Gutter.Width;
    AStringGrid.Color := SynEdit.Color;
    AStringGrid.Font.Color :=
      SynEdit.Highlighter.IdentifierAttribute.Foreground;
    AStringGrid.Canvas.Font.Color :=
      SynEdit.Highlighter.IdentifierAttribute.Foreground;
  end;

var s: string;
begin
  s := gSettings.CodeEditorStyles.Path+AColorSetting+EXTENSION_CES;
  if (AColorSetting = CODE_EDITOR_STYLE_CLASSIC) or not FileExists(s) then begin
    SetPascalCodeEditorClassicStyle(FSynEdit);
    FDiffModifyColor := CODE_EDITOR_STYLE_CLASSIC_DIFF_MOD;
    FDiffDeleteColor := CODE_EDITOR_STYLE_CLASSIC_DIFF_DEL;
    FDiffAddColor := CODE_EDITOR_STYLE_CLASSIC_DIFF_ADD;
  end else begin
    SetPascalCodeEditorStyleFromFile(FSynEdit, s);
    FDiffModifyColor := CODE_EDITOR_STYLE_MIDLET_DIFF_MOD;
    FDiffDeleteColor := CODE_EDITOR_STYLE_MIDLET_DIFF_DEL;
    FDiffAddColor := CODE_EDITOR_STYLE_MIDLET_DIFF_ADD;
  end;
  FDiffGutterColor := FSynEdit.Gutter.GradientStartColor;
  FSynEdit.Font.Name := gSettings.CodeEditorFontName;
  FSynEdit.Font.Size := gSettings.CodeEditorFontSize;
  CopyStyle(FSynEdit, FPreprocessSynEdit);
  CopyStyle(FSynEdit, FHistoryContentsSynEdit);
  SetStringGridPreferences(FDiff1StringGrid);
  SetStringGridPreferences(FDiff2StringGrid);
end;

procedure Tmp3CodeEditorFrame.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var clr: Tcolor;
begin
  clr := TStringGrid(Sender).Color;
  if ARow < FDiff.Count then
    case ACol of
      0: clr := FDiffGutterColor;
      1:
      case FDiff.Compares[ARow].Kind of
        ckNone: clr := TStringGrid(Sender).Color;
        ckModify: clr := FDiffModifyColor;
        ckDelete: clr := FDiffDeleteColor;
        ckAdd: clr := FDiffAddColor;
      end;
    end;
  with TStringGrid(Sender).Canvas do
  begin
    Brush.Color := clr;
    FillRect(Rect);
    TextRect(Rect,Rect.Left+3,Rect.Top+2,TStringGrid(Sender).Cells[ACol,ARow]);
    if (FDiff1Source.Count=0) and (FDiff2Source.Count=0) then
      exit;
    if (ACol in [0,2]) then begin
      Pen.Color := TStringGrid(Sender).Color;
      MoveTo(Rect.Right-1,0);
      LineTo(Rect.Right-1,Rect.Bottom);
    end else begin
      if (ACol = 1) then
      begin
        Pen.Color := $333333;
        MoveTo(Rect.Right-1,0);
        LineTo(Rect.Right-1,Rect.Bottom);
      end;
      Pen.Color := clSilver;
      MoveTo(Rect.Left,0);
      LineTo(Rect.Left,Rect.Bottom);
    end;
    if (gdSelected in State) and (ACol = 3) then
    begin
      rect.Left := 0;
      DrawFocusRect(Rect);
    end;
  end;
end;

procedure Tmp3CodeEditorFrame.GridTopLeftChanged(Sender: TObject);
var TheOtherGrid: TStringGrid;
begin
  if TStringGrid(Sender) = FDiff1StringGrid then
    TheOtherGrid := FDiff2StringGrid
  else
    TheOtherGrid := FDiff1StringGrid;
  TheOtherGrid.TopRow := TStringGrid(Sender).TopRow;
  TheOtherGrid.LeftCol := TStringGrid(Sender).LeftCol;
end;

procedure Tmp3CodeEditorFrame.Diff;

  procedure ClearAll;
  begin
    FDiff1HashList.Clear;
    FDiff2HashList.Clear;
    FDiff1StringGrid.RowCount := 0;
    FDiff2StringGrid.RowCount := 0;
    FDiff.Clear;
  end;

  procedure HashLines;
  var i: integer;
  begin
    FDiff1HashList.Capacity := FDiff1Source.Count;
    FDiff1HashList.Clear;
    for i := 0 to FDiff1Source.Count -1 do
      FDiff1HashList.Add(HashLine(FDiff1Source[i], false, false));
    FDiff2HashList.Clear;
    for i := 0 to FDiff2Source.Count -1 do
      FDiff2HashList.Add(HashLine(FDiff2Source[i], false, false));
  end;

  procedure CalculateDifferences;
  begin
    FDiff.Execute(PInteger(FDiff1HashList.list), PInteger(FDiff2HashList.list),
      FDiff1HashList.Count, FDiff2HashList.Count);
    {MessageBox(0,PChar(' Matches: ' + inttostr(FDiff.DiffStats.Matches)+
      ' Modifies: ' + inttostr(FDiff.DiffStats.Modifies) +
      ' Adds: ' + inttostr(FDiff.DiffStats.Adds) +
      ' Deletes: ' + inttostr(FDiff.DiffStats.Deletes)),'Results',1);}
  end;

  procedure ShowDifferences;
  var i: integer;
  begin
    FDiff1StringGrid.RowCount := max(FDiff1Source.Count, FDiff2Source.Count);
    FDiff2StringGrid.RowCount := max(FDiff1Source.Count, FDiff2Source.Count);
    for i := 0 to 1 do begin
      FDiff1StringGrid.Cols[i].BeginUpdate;
      FDiff1StringGrid.Cols[i].Clear;
      FDiff2StringGrid.Cols[i].BeginUpdate;
      FDiff2StringGrid.Cols[i].Clear;
    end;
    try
      FDiff1StringGrid.RowCount := FDiff.Count;
      FDiff2StringGrid.RowCount := FDiff.Count;
      for i := 0 to FDiff.Count-1 do
        with FDiff.Compares[i] do
        begin
          if Kind <> ckAdd then
          begin
            FDiff1StringGrid.Cells[0,i] := inttostr(oldIndex1+1);
            FDiff1StringGrid.Cells[1,i] := FDiff1Source[oldIndex1];
          end;
          if Kind <> ckDelete then
          begin
            FDiff2StringGrid.Cells[0,i] := inttostr(oldIndex2+1);
            FDiff2StringGrid.Cells[1,i] := FDiff2Source[oldIndex2];
          end;
        end;
    finally
      for i := 0 to 1 do begin
        FDiff1StringGrid.Cols[i].EndUpdate;
        FDiff2StringGrid.Cols[i].EndUpdate;
      end;
    end;
  end;

begin
  Screen.Cursor := crHourGlass;
  try
    ClearAll;
    HashLines;
    CalculateDifferences;
    ShowDifferences;
  finally
    Screen.Cursor := crDefault;
  end;
end;

end.
