(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3EmulatorsDialog;

interface

uses
  Forms, Controls, Graphics,
  tuiForm, tuiUtils, tuiItemWithStringValueDialog, tuiDialog, tuiControls,
  SpTBXMessageDlg,
  gnugettext,
  mp3Consts, mp3Settings;

type
  Tmp3EmulatorsDialog = class(TTurboUIDialog)
    procedure FormCreate(Sender: TObject);
  private
    FEmulatorsLabel: TtuiLabel;
    FEmulatorsListBox: TtuiListBox;
    FAddButton: TtuiButton;
    FEditButton: TtuiButton;
    FDeleteButton: TtuiButton;
    FCloseButton: TtuiButton;
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
  with ControlFactory do
    with NewFactory(NewPanel.SetAlign(alClient).SetBorders(false).GetInstance) do begin
        NewLabel.SetCaption(_('Emulators')).SetFontStyle([fsBold])
        .SetTop(16).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(16).SetWidth(80)
        .GetInstance(FEmulatorsLabel)
      .GetFactory
        .NewListBox
        .SetTop(FEmulatorsLabel.Top + FEmulatorsLabel.Height + 4).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(128).SetWidth(TitleBar.Width -
          (TUI_DIALOG_HORIZONTAL_MARGIN * 2) - TUI_DIALOG_BUTTON_WIDTH - TUI_DIALOG_BUTTON_SEPARATION)
        .SetCursor(crHandPoint).SetOnDblClick(OnEmulatorsListDoubleClick)
        .GetInstance(FEmulatorsListBox)
      .GetFactory
        .NewButton.SetCaption(_('Close')).SetDefault(true).SetModalResult(mrOk)
        .SetTop(FEmulatorsListBox.Top + FEmulatorsListBox.Height +
          (TUI_DIALOG_BUTTON_SEPARATION * 2)).SetLeft(TUI_DIALOG_HORIZONTAL_MARGIN)
        .SetHeight(24).SetWidth(TUI_DIALOG_BUTTON_WIDTH).SetCursor(crHandPoint)
        .GetInstance(FCloseButton)
      .GetFactory
        .NewButton.SetCaption(_('Add')).SetOnclick(OnAddClick)
        .SetTop(FEmulatorsListBox.Top).SetLeft(FEmulatorsListBox.Left +
           FEmulatorsListBox.Width + TUI_DIALOG_BUTTON_SEPARATION)
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
