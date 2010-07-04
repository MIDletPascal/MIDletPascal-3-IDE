(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3EmulatorsDialog;

interface

uses
  Forms, Controls, Graphics,
  tuiForm, tuiUtils, tuiItemWithStringValueDialog, tuiDialog,
  SpTBXControls, SpTBXEditors,
  SpTBXMessageDlg,
  gnugettext,
  mp3Consts, mp3Settings;

type
  Tmp3EmulatorsDialog = class(TTurboUIDialog)
    procedure FormCreate(Sender: TObject);
  private
    FEmulatorsLabel: TSpTBXLabel;
    FEmulatorsListBox: TSpTBXListBox;
    FAddButton: TSpTBXButton;
    FEditButton: TSpTBXButton;
    FDeleteButton: TSpTBXButton;
    FCloseButton: TSpTBXButton;
    procedure OnAddClick(Sender: TObject);
    procedure OnEditClick(Sender: TObject);
    procedure OnDeleteClick(Sender: TObject);
    procedure OnEmulatorsListDoubleClick(Sender: TObject);
    procedure RefreshEmulators;
  end;

implementation

{$R *.dfm}

procedure Tmp3EmulatorsDialog.FormCreate(Sender: TObject);
begin
  SetTitle(_('Emulators Management'));
  FEmulatorsLabel := NewLabel(Self,_('Emulators'),
    40,TUI_DIALOG_HORIZONTAL_MARGIN,16,80);
  FEmulatorsLabel.Font.Style := [fsBold];
  FEmulatorsListBox := NewListBox(Self, '',
    60,TUI_DIALOG_HORIZONTAL_MARGIN,128,
    TitleBar.Width - (TUI_DIALOG_HORIZONTAL_MARGIN * 2) - TUI_DIALOG_BUTTON_WIDTH - TUI_DIALOG_BUTTON_SEPARATION);
  FEmulatorsListBox.Cursor := crHandPoint;
  FEmulatorsListBox.OnDblClick := OnEmulatorsListDoubleClick;
  FAddButton := NewButton(Self, _('Add'),
    FEmulatorsListBox.Top,
    FEmulatorsListBox.Left + FEmulatorsListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FAddButton.OnClick := OnAddClick;
  FAddButton.Cursor := crHandPoint;
  FEditButton := NewButton(Self, _('Edit'),
    FAddButton.Top + FAddButton.Height + TUI_DIALOG_BUTTON_SEPARATION,
    FEmulatorsListBox.Left + FEmulatorsListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FEditButton.OnClick := OnEditClick;
  FEditButton.Cursor := crHandPoint;
  FDeleteButton := NewButton(Self, _('Delete'),
    FEditButton.Top + FEditButton.Height + TUI_DIALOG_BUTTON_SEPARATION,
    FEmulatorsListBox.Left + FEmulatorsListBox.Width + TUI_DIALOG_BUTTON_SEPARATION,
    24, TUI_DIALOG_BUTTON_WIDTH + 8);
  FDeleteButton.OnClick := OnDeleteClick;
  FDeleteButton.Cursor := crHandPoint;
  FCloseButton := NewButton(Self, _('Close'),
    FEmulatorsListBox.Top + FEmulatorsListBox.Height + (TUI_DIALOG_BUTTON_SEPARATION * 2),
    TUI_DIALOG_HORIZONTAL_MARGIN,
    //TitleBar.Width - TUI_DIALOG_HORIZONTAL_MARGIN - TUI_DIALOG_BUTTON_WIDTH,
    24, TUI_DIALOG_BUTTON_WIDTH);
  FCloseButton.Cursor := crHandPoint;
  FCloseButton.ModalResult := mrOK;
  FCloseButton.Default := true;
  Height := FCloseButton.Top + FCloseButton.Height + TUI_DIALOG_VERTICAL_MARGIN;
  RefreshEmulators;
end;

procedure Tmp3EmulatorsDialog.RefreshEmulators;
var i: integer;
begin
  FEmulatorsListBox.Items.Clear;
  for i := 0 to gSettings.Emulators.Count - 1 do
    FEmulatorsListBox.Items.Add(gSettings.Emulators.Names[i]);
end;

procedure Tmp3EmulatorsDialog.OnAddClick(Sender: TObject);
begin
  if gSettings.Emulators.Count < EMULATORS_MAX then
    with TTurboUIItemWithStringValueDialog.Create(Self) do
    try
      SetTitle(_('Emulator'));
      SetItemNameLabel(_('Name'));
      SetItemStringValueLabel(_('Command Line'));
      ItemStringValue := PATTERN_JAD;
      if ShowModal=mrOk then
        if (ItemName <> '') and (ItemStringValue <> '') then
        begin
          gSettings.Emulators.Add(ItemName+'='+ItemStringValue);
          RefreshEmulators;
        end;
    finally
      Free;
    end;
end;

procedure Tmp3EmulatorsDialog.OnEditClick(Sender: TObject);
var isActive: boolean;
begin
  if FEmulatorsListBox.ItemIndex = -1 then
    exit;
  with TTurboUIItemWithStringValueDialog.Create(Self) do
  try
    SetTitle(_('Emulator'));
    SetItemNameLabel(_('Name'));
    SetItemStringValueLabel(_('Command Line'));
    ItemName := gSettings.Emulators.Names[FEmulatorsListBox.ItemIndex];
    ItemStringValue := gSettings.Emulators.ValueFromIndex[FEmulatorsListBox.ItemIndex];
    isActive := ItemName = gSettings.CurrentEmulatorName;
    if ShowModal=mrOk then begin
      gSettings.Emulators[FEmulatorsListBox.ItemIndex] := ItemName+'='+ItemStringValue;
      if isActive then
        gSettings.CurrentEmulatorName := ItemName;
      RefreshEmulators;
    end;
  finally
    Free;
  end;
end;

procedure Tmp3EmulatorsDialog.OnEmulatorsListDoubleClick(Sender: TObject);
begin
  FEditButton.Click;
end;

procedure Tmp3EmulatorsDialog.OnDeleteClick(Sender: TObject);
begin
  if FEmulatorsListBox.ItemIndex = -1 then
    exit;
  if MessageDlg(_('Are you sure you want to delete it?'),mtConfirmation,mbYesNo,0)=mrYes then
  begin
    gSettings.Emulators.Delete(FEmulatorsListBox.ItemIndex);
    RefreshEmulators;
  end;
end;

end.
