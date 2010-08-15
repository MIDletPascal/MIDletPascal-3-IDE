(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3CodeEditorStylesDialog;

interface

uses
  Forms, Controls, Graphics,
  tuiForm, tuiUtils, tuiItemWithStringValueDialog, tuiDialog,
  SpTBXControls, SpTBXEditors,
  SpTBXMessageDlg, SpTBXInputBox,
  gnugettext,
  mp3Consts, mp3Settings;

type
  Tmp3CodeEditorStylesDialog = class(TTurboUIDialog)
    procedure FormCreate(Sender: TObject);
  private
    FCodeEditorStylesLabel: TSpTBXLabel;
    FCodeEditorStylesListBox: TSpTBXListBox;
    FAddButton: TSpTBXButton;
    FEditButton: TSpTBXButton;
    FDeleteButton: TSpTBXButton;
    FCloseButton: TSpTBXButton;
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
  FCodeEditorStylesLabel := NewLabel(Self,_('Styles'),
    40,TUI_DIALOG_HORIZONTAL_MARGIN,16,80);
  FCodeEditorStylesLabel.Font.Style := [fsBold];
  FCodeEditorStylesListBox := NewListBox(Self, '',
    60,TUI_DIALOG_HORIZONTAL_MARGIN,128,
    TitleBar.Width - (TUI_DIALOG_HORIZONTAL_MARGIN * 2) - TUI_DIALOG_BUTTON_WIDTH - TUI_DIALOG_BUTTON_SEPARATION);
  FCodeEditorStylesListBox.Cursor := crHandPoint;
  FCodeEditorStylesListBox.OnDblClick := OnCodeEditorStylesListDoubleClick;
  FAddButton := NewButton(Self, _('Add'),
    FCodeEditorStylesListBox.Top,
    FCodeEditorStylesListBox.Left + FCodeEditorStylesListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FAddButton.OnClick := OnAddClick;
  FAddButton.Cursor := crHandPoint;
  FEditButton := NewButton(Self, _('Edit'),
    FAddButton.Top + FAddButton.Height + TUI_DIALOG_BUTTON_SEPARATION,
    FCodeEditorStylesListBox.Left + FCodeEditorStylesListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FEditButton.OnClick := OnEditClick;
  FEditButton.Cursor := crHandPoint;
  FDeleteButton := NewButton(Self, _('Delete'),
    FEditButton.Top + FEditButton.Height + TUI_DIALOG_BUTTON_SEPARATION,
    FCodeEditorStylesListBox.Left + FCodeEditorStylesListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FDeleteButton.OnClick := OnDeleteClick;
  FDeleteButton.Cursor := crHandPoint;
  FCloseButton := NewButton(Self, _('Close'),
    FCodeEditorStylesListBox.Top + FCodeEditorStylesListBox.Height + (TUI_DIALOG_BUTTON_SEPARATION * 2),
    TUI_DIALOG_HORIZONTAL_MARGIN,
    //TitleBar.Width - TUI_DIALOG_HORIZONTAL_MARGIN - TUI_DIALOG_BUTTON_WIDTH,
    24, TUI_DIALOG_BUTTON_WIDTH);
  FCloseButton.Cursor := crHandPoint;
  FCloseButton.ModalResult := mrOK;
  FCloseButton.Default := true;
  Height := FCloseButton.Top + FCloseButton.Height + TUI_DIALOG_VERTICAL_MARGIN;
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
