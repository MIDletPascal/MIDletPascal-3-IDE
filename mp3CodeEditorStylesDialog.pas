(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3CodeEditorStylesDialog;

interface

uses
  Forms, Controls, Graphics,
  tuiForm, tuiUtils, tuiItemWithStringValueDialog, tuiDialog, tuiControls,
  SpTBXMessageDlg, SpTBXInputBox,
  gnugettext,
  mp3Consts, mp3Settings;

type
  Tmp3CodeEditorStylesDialog = class(TTurboUIDialog)
    procedure FormCreate(Sender: TObject);
  private
    FCodeEditorStylesLabel: TtuiLabel;
    FCodeEditorStylesListBox: TtuiListBox;
    FAddButton: TtuiButton;
    FEditButton: TtuiButton;
    FDeleteButton: TtuiButton;
    FCloseButton: TtuiButton;
    procedure OnAddClick(Sender: TObject);
    procedure OnEditClick(Sender: TObject);
    procedure OnDeleteClick(Sender: TObject);
    procedure OnCodeEditorStylesListDoubleClick(Sender: TObject);
    procedure RefreshCodeEditorStyles;
  end;

implementation

{$R *.dfm}

procedure Tmp3CodeEditorStylesDialog.FormCreate(Sender: TObject);
begin
  SetTitle(_('Code Editor Styles Management'));
  with ControlFactory do
    with NewFactory(NewPanel.SetAlign(alClient).SetBorders(false).GetInstance) do begin
        NewLabel.SetCaption(_('Styles')).SetFontStyle([fsBold])
        .SetTop(16).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(16).SetWidth(80)
        .GetInstance(FCodeEditorStylesLabel)
      .GetFactory
        .NewListBox
        .SetTop(FCodeEditorStylesLabel.Top + FCodeEditorStylesLabel.Height + 4).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(128).SetWidth(TitleBar.Width -
          (TUI_DIALOG_HORIZONTAL_MARGIN * 2) - TUI_DIALOG_BUTTON_WIDTH - TUI_DIALOG_BUTTON_SEPARATION)
        .SetCursor(crHandPoint).SetOnDblClick(OnCodeEditorStylesListDoubleClick)
        .GetInstance(FCodeEditorStylesListBox)
      .GetFactory
        .NewButton.SetCaption(_('Close')).SetDefault(true).SetModalResult(mrOk)
        .SetTop(FCodeEditorStylesListBox.Top + FCodeEditorStylesListBox.Height +
          (TUI_DIALOG_BUTTON_SEPARATION * 2)).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(24).SetWidth(TUI_DIALOG_BUTTON_WIDTH).SetCursor(crHandPoint)
        .GetInstance(FCloseButton)
      .GetFactory
        .NewButton.SetCaption(_('Add')).SetOnclick(OnAddClick)
        .SetTop(FCodeEditorStylesListBox.Top).SetLeft(FCodeEditorStylesListBox.Left +
           FCodeEditorStylesListBox.Width + TUI_DIALOG_BUTTON_SEPARATION)
        .SetHeight(24).SetWidth(TUI_DIALOG_BUTTON_WIDTH + 8).SetCursor(crHandPoint)
        .GetInstance(FAddButton)
      .GetFactory
        .NewButton.SetCaption(_('Edit')).SetOnclick(OnEditClick)
        .SetTop(FAddButton.Top + FAddButton.Height + TUI_DIALOG_BUTTON_SEPARATION).SetLeft(FAddButton.Left)
        .SetHeight(24).SetWidth(TUI_DIALOG_BUTTON_WIDTH + 8).SetCursor(crHandPoint)
        .GetInstance(FEditButton)
      .GetFactory
        .NewButton.SetCaption(_('Delete')).SetOnclick(OnDeleteClick)
        .SetTop(FEditButton.Top + FEditButton.Height + TUI_DIALOG_BUTTON_SEPARATION).SetLeft(FEditButton.Left)
        .SetHeight(24).SetWidth(TUI_DIALOG_BUTTON_WIDTH + 8).SetCursor(crHandPoint)
        .GetInstance(FDeleteButton);
    end;
  Height := FCloseButton.Top + FCloseButton.Height + (TUI_DIALOG_VERTICAL_MARGIN * 2);
  Width := Width + (TUI_DIALOG_HORIZONTAL_MARGIN div 2);
  RefreshCodeEditorStyles;
end;

procedure Tmp3CodeEditorStylesDialog.RefreshCodeEditorStyles;
var i: integer;
begin
  FCodeEditorStylesListBox.Items.Clear;
  gSettings.CodeEditorStyles.Refresh;
  for i := 0 to gSettings.CodeEditorStyles.List.Count - 1 do
    FCodeEditorStylesListBox.Items.Add(gSettings.CodeEditorStyles.List[i]);
end;

procedure Tmp3CodeEditorStylesDialog.OnAddClick(Sender: TObject);
var s: string;
begin
  s := InputBox(_('Add Style'),_('Enter the name:'),'');
  if (s <> '') and (gSettings.CodeEditorStyles.List.IndexOf(s) = -1) then
    gSettings.CodeEditorStyles.Add(s);
  RefreshCodeEditorStyles;
end;

procedure Tmp3CodeEditorStylesDialog.OnEditClick(Sender: TObject);
var isActive: boolean; theName: string;
begin
  if FCodeEditorStylesListBox.ItemIndex = -1 then
    exit;
  theName := gSettings.CodeEditorStyles.List[FCodeEditorStylesListBox.ItemIndex];
  isActive := theName = gSettings.CodeEditorStyle;
  gSettings.CodeEditorStyles.Edit(theName);
  if isActive then
    gSettings.CodeEditorStyle := theName;
  RefreshCodeEditorStyles;
end;

procedure Tmp3CodeEditorStylesDialog.OnCodeEditorStylesListDoubleClick(Sender: TObject);
begin
  FEditButton.Click;
end;

procedure Tmp3CodeEditorStylesDialog.OnDeleteClick(Sender: TObject);
begin
  if FCodeEditorStylesListBox.ItemIndex = -1 then
    exit;
  if MessageDlg(_('Are you sure you want to delete it?'),mtConfirmation,mbYesNo,0)=mrYes then
  begin
    gSettings.CodeEditorStyles.Delete(
      gSettings.CodeEditorStyles.List[FCodeEditorStylesListBox.ItemIndex]
    );
    RefreshCodeEditorStyles;
  end;
end;

end.
