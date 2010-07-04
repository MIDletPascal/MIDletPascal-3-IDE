(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3ProjectManager;

interface

uses
  Windows, Messages, SysUtils, Dialogs,
  Classes, Graphics, Controls, Forms,
  ImgList, ActnList,
  SpTBXControls, SpTBXItem, SpTBXEditors,
  VirtualTrees,
  gnugettext,
  sitFileUtils, sitDockableManagers,
  mp3Project, mp3BuildConfigurations, mp3Consts, mp3FileKind;

type
  Tmp3PropertiesPanelMode = (ppmProject, ppmSourceFile, ppmResourceFile, ppmBuildConfiguration);

  Tmp3ProjectManager = class(TsitTreeDockableManager)
  private
    FPropertiesPanel: TSpTBXPanel;
    FPropertiesPanelLabel1: TSpTBXLabel;
    FPropertiesPanelLabel2: TSpTBXLabel;
    FPropertiesPanelLabel3: TSpTBXLabel;
    FPropertiesPanelLabel4: TSpTBXLabel;
    FPropertiesPanelEdit1: TSpTBXEdit;
    FPropertiesPanelEdit2: TSpTBXEdit;
    FPropertiesPanelEdit3: TSpTBXEdit;
    FPropertiesPanelEdit4: TSpTBXEdit;
    FPropertiesPanelSpin1: TSpTBXSpinButton;
    FPropertiesPanelSpin2: TSpTBXSpinButton;
    FPropertiesPanelSpin3: TSpTBXSpinButton;
    FPropertiesPanelSpin4: TSpTBXSpinButton;
    FPropertiesPanelCheckListBox: TSpTBXCheckListBox;
    FPropertiesPanelMode: Tmp3PropertiesPanelMode;
    FSourceFilesActions: TActionList;
    FResourceFilesActions: TActionList;
    FBuildConfigurationsActions: TActionList;
    FCurrentProject: Tmp3Project;
    FOnSourceFileEdit: TNotifyEvent;
    FOnSourceFileRename: TNotifyEvent;
    FOnResourceFileEdit: TNotifyEvent;
    FOnResourceFileRename: TNotifyEvent;
    FOnBuildConfigurationRename: TNotifyEvent;
    FoldItemLevel, FoldItemIndex, FoldItemParentIndex: integer;
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: UnicodeString);
    procedure GetImageIndexEx(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: Integer; var ImageList: TCustomImageList);
    procedure InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure PaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure DoubleClickHandler(Sender: TObject);
    procedure PopupMenuHandler(Sender: TObjecT);
    procedure KeyDownHandler(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure Editing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      var Allowed: Boolean);
    procedure ExecNode(ANode: PVirtualNode);
    procedure ResourceConfigurationsChange(Sender: TObject);
    procedure SpinClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure NewEmptyProject;
    function GetSelectedItemName: string;
    procedure SetPropertiesPanelMode(const Value: Tmp3PropertiesPanelMode);
    property PropertiesPanelMode: Tmp3PropertiesPanelMode
      read FPropertiesPanelMode write SetPropertiesPanelMode;
    procedure CustomizeTree; override;
    procedure PerformRefresh; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function New(AName, ALocation: string): boolean; override;
    procedure Load(AFilename: string); override;
    procedure Save(ARefresh: boolean = True); override;
    procedure Close; override;
    function HasItemLoaded: boolean; override;
    procedure ExecuteCurrentElement;
    property CurrentProject: Tmp3Project read FCurrentProject;
    property SelectedItemName: string read GetSelectedItemName;
    property OnSourceFileEdit: TNotifyEvent
      read FOnSourceFileEdit write FOnSourceFileEdit;
    property OnSourceFileRename: TNotifyEvent
      read FOnSourceFileRename write FOnSourceFileRename;
    property OnResourceFileEdit: TNotifyEvent
      read FOnResourceFileEdit write FOnResourceFileEdit;
    property OnResourceFileRename: TNotifyEvent
      read FOnResourceFileRename write FOnResourceFileRename;
    property OnBuildConfigurationRename: TNotifyEvent
      read FOnBuildConfigurationRename write FOnBuildConfigurationRename;
    property SourceFilesActions: TActionList
      read FSourceFilesActions write FSourceFilesActions;
    property ResourceFilesActions: TActionList
      read FResourceFilesActions write FResourceFilesActions;
    property BuildConfigurationsActions: TActionList
      read FBuildConfigurationsActions write FBuildConfigurationsActions;
  end;

implementation

{ Tmp3ProjectManager }

procedure Tmp3ProjectManager.AfterConstruction;

  procedure InitPropertiesPanel;

    function NewLabel(ATop,ALeft:integer):TSpTBXLabel;
    begin
      result := TSpTBXLabel.Create(FPropertiesPanel);
      result.Parent := FPropertiesPanel;
      result.Top := ATop;
      result.Left := ALeft;
    end;

    function NewEdit(ATop,ALeft:integer):TSpTBXEdit;
    begin
      result := TSpTBXEdit.Create(FPropertiesPanel);
      result.Parent := FPropertiesPanel;
      result.Top := ATop;
      result.Left := ALeft;
      result.Width := Width - result.Left - 16;
      result.OnChange := EditChange;
      result.Anchors := [akTop,akLeft,akRight];
    end;

    function NewSpinButton(AEdit: TSpTBXEdit): TSpTBXSpinButton;
    begin
      result := TSpTBXSpinButton.Create(FPropertiesPanel);
      result.Parent := FPropertiesPanel;
      result.Top := AEdit.Top;
      result.Left := Width - AEdit.Left - 32;
      result.Cursor := crHandPoint;
      result.OnClick := SpinClick;
      result.Anchors := [akTop,akRight];
    end;

  begin
    FPropertiesPanel := TSpTBXPanel.Create(BackgroundPanel);
    FPropertiesPanel.Parent := BackgroundPanel;
    FPropertiesPanel.Align := alBottom;
    FPropertiesPanel.Borders := false;
    FPropertiesPanel.Height := PROJECT_MANAGER_PROPERTIES_PANEL_HEIGHT_DEFAULT;
    FPropertiesPanel.Visible := false;
    FPropertiesPanelLabel1 := NewLabel(8,8);
    FPropertiesPanelLabel2 := NewLabel(FPropertiesPanelLabel1.Top+40,8);
    FPropertiesPanelLabel3 := NewLabel(FPropertiesPanelLabel2.Top+40,8);
    FPropertiesPanelLabel4 := NewLabel(FPropertiesPanelLabel3.Top+40,8);
    FPropertiesPanelEdit1 := NewEdit(FPropertiesPanelLabel1.Top+16,6);
    FPropertiesPanelEdit2 := NewEdit(FPropertiesPanelLabel2.Top+16,6);
    FPropertiesPanelEdit3 := NewEdit(FPropertiesPanelLabel3.Top+16,6);
    FPropertiesPanelEdit4 := NewEdit(FPropertiesPanelLabel4.Top+16,6);
    FPropertiesPanelSpin1 := NewSpinButton(FPropertiesPanelEdit1);
    FPropertiesPanelSpin2 := NewSpinButton(FPropertiesPanelEdit2);
    FPropertiesPanelSpin3 := NewSpinButton(FPropertiesPanelEdit3);
    FPropertiesPanelSpin4 := NewSpinButton(FPropertiesPanelEdit4);
    FPropertiesPanelCheckListBox := TSpTBXCheckListBox.Create(FPropertiesPanel);
    FPropertiesPanelCheckListBox.Parent := FPropertiesPanel;
    FPropertiesPanelCheckListBox.Top := FPropertiesPanelLabel3.Top+16;
    FPropertiesPanelCheckListBox.Left := 6;
    FPropertiesPanelCheckListBox.Height := FPropertiesPanel.Height - FPropertiesPanelCheckListBox.Top - 16;
    FPropertiesPanelCheckListBox.Width := Width - FPropertiesPanelCheckListBox.Left - 16;
    FPropertiesPanelCheckListBox.Anchors := [akTop,akLeft,akRight];
    FPropertiesPanelCheckListBox.Visible := false;
    FPropertiesPanelCheckListBox.Cursor := crHandPoint;
    FPropertiesPanelCheckListBox.OnClickCheck := ResourceConfigurationsChange;
  end;

begin
  inherited;
  Caption := _('Project Manager');
  Width := PROJECT_MANAGER_WIDTH_DEFAULT;
  InitPropertiesPanel;
end;

procedure Tmp3ProjectManager.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FCurrentProject);
end;

procedure Tmp3ProjectManager.CustomizeTree;
begin
  Tree.OnEditing := Editing;
  Tree.OnKeyDown := KeyDownHandler;
  Tree.OnInitNode := InitNode;
  Tree.OnInitChildren := InitChildren;
  Tree.OnGetImageIndexEx := GetImageIndexEx;
  Tree.OnGetText := GetText;
  Tree.OnPaintText := PaintText;
  Tree.OnDblClick := DoubleClickHandler;
  Tree.OnChange := SelectionChange;
  Tree.PopupMenu.OnPopup := PopupMenuHandler;
end;

procedure Tmp3ProjectManager.PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

  procedure SetNodeColor(AColor, ASelectedColor: TColor);
  begin
    if Tree.Selected[Node] then
      TargetCanvas.Font.Color := ASelectedColor
    else
      TargetCanvas.Font.Color := AColor;
  end;

begin
  SetNodeColor(DEFAULT_MANAGER_ITEM_ENABLED_TEXTCOLOR, DEFAULT_MANAGER_ITEM_ENABLED_SELECTED_TEXTCOLOR);
  if Tree.GetNodeLevel(Node) = 2 then
    case Node.Parent.Index of
      0:
      begin
        if Node.Index = 0 then
          TargetCanvas.Font.Style := DEFAULT_MANAGER_UNDERLINEDITEM_FONTSTYLE;
        if not FCurrentProject.SourceFiles[Node.Index].Exists then begin
          SetNodeColor(DEFAULT_MANAGER_ITEM_DISABLED_TEXTCOLOR, DEFAULT_MANAGER_ITEM_DISABLED_SELECTED_TEXTCOLOR);
          TargetCanvas.Font.Style := DEFAULT_MANAGER_INEXISTENTITEM_FONTSTYLE;
        end;
      end;
      1:
        if not FCurrentProject.ResourceFiles[Node.Index].Exists then begin
          SetNodeColor(DEFAULT_MANAGER_ITEM_DISABLED_TEXTCOLOR, DEFAULT_MANAGER_ITEM_DISABLED_SELECTED_TEXTCOLOR);
          TargetCanvas.Font.Style := DEFAULT_MANAGER_INEXISTENTITEM_FONTSTYLE;
        end;
      2:
        if Node.Index = FCurrentProject.BuildConfigurations.ActiveConfigurationIndex then
          TargetCanvas.Font.Style := DEFAULT_MANAGER_UNDERLINEDITEM_FONTSTYLE;
    end;
end;

procedure Tmp3ProjectManager.Close;
begin
  if assigned(FCurrentProject) then
    FCurrentProject.Close;
  Modified := false;
end;

procedure Tmp3ProjectManager.PerformRefresh;
var ANode: PVirtualNode;
begin
  ANode := Tree.GetFirstSelected;
  if Assigned(ANode) and Assigned(ANode.Parent) then begin
    FoldItemLevel := Tree.GetNodeLevel(ANode);
    FoldItemIndex := ANode.Index;
    FoldItemParentIndex := ANode.Parent.Index;
  end else begin
    FoldItemLevel := -1;
    FoldItemIndex := -1;
    FoldItemParentIndex := -1;
  end;
  Tree.Images := Images;
  Tree.Clear;
  if HasItemLoaded then
    Tree.RootNodeCount := 1;
end;

procedure Tmp3ProjectManager.DoubleClickHandler(Sender: TObject);
var Node: PVirtualNode;
begin
  Node := Tree.GetFirstSelected;
  if assigned(Node) then
    ExecNode(Node);
end;

procedure Tmp3ProjectManager.Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := false; // disallow
end;

procedure Tmp3ProjectManager.ExecNode(ANode: PVirtualNode);
begin
  if assigned(ANode) then
    if Tree.GetNodeLevel(ANode) = 2 then
      case ANode.Parent.Index of
        0: if assigned(FOnSourceFileEdit) then
             FOnSourceFileEdit(FCurrentProject.SourceFiles[ANode.Index]);
        1: if assigned(FOnResourceFileEdit) then
             FOnResourceFileEdit(FCurrentProject.ResourceFiles[ANode.Index]);
        2:
          begin
            FCurrentProject.BuildConfigurations.ActiveConfigurationIndex := ANode.Index;
            if FCurrentProject.Save then
              RefreshManager;
          end;
      end;
end;

procedure Tmp3ProjectManager.ExecuteCurrentElement;
begin
  ExecNode(Tree.GetFirstSelected);
end;

procedure Tmp3ProjectManager.GetImageIndexEx(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: Integer;
    var ImageList: TCustomImageList);
begin
  ImageIndex := -1;
  case Sender.GetNodeLevel(Node) of
  0: ImageIndex := 5;
  1: case Node.Index of
      0: ImageIndex := 7;
      1: ImageIndex := 14;
      2: ImageIndex := 1;
     end;
  2: case Node.Parent.Index of
      0: ImageIndex := 8;
      1: if FCurrentProject.ResourceFiles[Node.Index].Kind=fkImage then
        ImageIndex := 17
      else
        ImageIndex := 15;
     end;
  end;
end;

function Tmp3ProjectManager.GetSelectedItemName: string;
var ANode: PVirtualNode;
begin
  result := '';
  ANode := Tree.GetFirstSelected;
  if assigned(ANode) then
    case Tree.GetNodeLevel(ANode) of
      2: case ANode.Parent.Index of
          0: result := FCurrentProject.SourceFiles[ANode.Index].Filename;
          1: result := FCurrentProject.ResourceFiles[ANode.Index].Filename;
          2: result := FCurrentProject.BuildConfigurations[ANode.Index].Name;
         end;
    end;
end;

procedure Tmp3ProjectManager.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
begin
  case Sender.GetNodeLevel(Node) of
  0:
    begin
      CellText := ExtractFileName(FCurrentProject.Filename);
      if Modified then
        CellText := CellText + '*';
    end;
  1: case Node.Index of
      0: CellText := WideFormat(_('Sources')+' (%d)',[FCurrentProject.SourceFiles.Count]);
      1: CellText := WideFormat(_('Resources')+' (%d)',[FCurrentProject.ResourceFiles.Count]);
      2: CellText := WideFormat(_('Configurations')+' (%d)',[FCurrentProject.BuildConfigurations.Count]);
     end;
  2: case Node.Parent.Index of
      0: CellText := FCurrentProject.SourceFiles[Node.Index].Filename;
      1: CellText := FCurrentProject.ResourceFiles[Node.Index].Filename;
      2: CellText := FCurrentProject.BuildConfigurations[Node.Index].Name;
     end;
  end;
end;

function Tmp3ProjectManager.HasItemLoaded: boolean;
begin
  result := assigned(FCurrentProject) and (FCurrentProject.Loaded or FCurrentProject.IsNew);
end;

procedure Tmp3ProjectManager.InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    ChildCount := 3
  else if Sender.GetNodeLevel(Node) = 1 then begin
    case Node.Index of
      0: ChildCount := FCurrentProject.SourceFiles.Count;
      1: ChildCount := FCurrentProject.ResourceFiles.Count;
      2: ChildCount := FCurrentProject.BuildConfigurations.Count;
    end;
  end;
end;

procedure Tmp3ProjectManager.InitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var nl: integer;
begin
  nl := Sender.GetNodeLevel(Node);
  if nl < 2 then
    InitialStates := InitialStates + [ivsHasChildren,ivsExpanded];
  if assigned(Node.Parent) then
    if (nl=FoldItemLevel) and (Node.Index=FoldItemIndex)
      and (Node.Parent.Index=FoldItemParentIndex) then
      Tree.Selected[Node] := true;
end;

procedure Tmp3ProjectManager.KeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Node: PVirtualNode;
begin
  case Key of
    VK_F2:
    begin
      Node := Tree.GetFirstSelected;
      if Tree.GetNodeLevel(Node) = 2 then
        case Node.Parent.Index of
          0:
            if assigned(FOnSourceFileRename) then
              FOnSourceFileRename(Self);
          1:
            if assigned(FOnResourceFileRename) then
              FOnResourceFileRename(Self);
          2:
            if assigned(FOnBuildConfigurationRename) then
              FOnBuildConfigurationRename(Self);
        end;
    end;
    VK_F5:
      RefreshManager;
    VK_RETURN:
    begin
      Node := Tree.GetFirstSelected;
      if assigned(Node) then
        ExecNode(Node);
    end;
  end;
end;

procedure Tmp3ProjectManager.Load(AFilename: string);
begin
  if AFilename = '' then
    exit;
  if not Assigned(FCurrentProject) then
    NewEmptyProject;
  FCurrentProject.Load(AFilename);
  Modified := false;
end;

function Tmp3ProjectManager.New(AName, ALocation: string): boolean;
var fn: string;
begin
  result := false;
  NewEmptyProject;
  fn := IncludeTrailingPathDelimiter(ALocation)+AName+EXTENSION_PROJECT;
  if FCurrentProject.New(fn) then
    result := FCurrentProject.Load(fn);
end;

procedure Tmp3ProjectManager.NewEmptyProject;
begin
  if assigned(FCurrentProject) then begin
    FCurrentProject.Close;
    FCurrentProject.Free;
  end;
  FCurrentProject := Tmp3Project.Create;
  Modified := false;
end;

procedure Tmp3ProjectManager.ResourceConfigurationsChange(Sender: TObject);

  function GetSelectedConfigurationsAsCSV: string;
  var i: integer;
  begin
    result := '';
    with FPropertiesPanelCheckListBox do
      for i := 0 to Items.Count - 1 do
        if Checked[i] then
          if result = '' then
            result := Items[i]
          else
            result := result + ',' + Items[i];
  end;

begin
  if FCurrentProject.ResourceFiles.ModifyConfigurations(
    FCurrentProject.ResourceFiles[Tree.GetFirstSelected.Index].Filename,
    GetSelectedConfigurationsAsCSV
  ) then
    Save;
end;

procedure Tmp3ProjectManager.Save(ARefresh: boolean = True);
begin
  if assigned(FCurrentProject) then
    FCurrentProject.Save;
  Modified := false;
  if ARefresh then
    RefreshManager;
end;

procedure Tmp3ProjectManager.SelectionChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if Tree.RootNodeCount = 0 then
    FPropertiesPanel.Visible := false
  else
    case Sender.GetNodeLevel(Node) of
      0:
        SetPropertiesPanelMode(ppmProject);
      2:
        case Node.Parent.Index of
          0: SetPropertiesPanelMode(ppmSourceFile);
          1: SetPropertiesPanelMode(ppmResourceFile);
          2: SetPropertiesPanelMode(ppmBuildConfiguration);
        end;
    else
      FPropertiesPanel.Visible := false;
    end;
end;

procedure Tmp3ProjectManager.SetPropertiesPanelMode(
  const Value: Tmp3PropertiesPanelMode);
var i,x: integer;
begin
  BeginUpdate;
  try
    FPropertiesPanelMode := Value;
    case FPropertiesPanelMode of
      ppmProject:
        begin
          FPropertiesPanelLabel1.Caption := _('MIDlet Name');
          FPropertiesPanelLabel2.Caption := _('MIDlet Vendor');
          FPropertiesPanelLabel3.Caption := _('MIDlet Version');
          FPropertiesPanelLabel4.Caption := _('MIDlet Icon');
          FPropertiesPanelEdit1.Visible := true;
          FPropertiesPanelEdit2.Visible := true;
          FPropertiesPanelEdit3.Visible := true;
          FPropertiesPanelEdit4.Visible := true;
          FPropertiesPanelEdit1.ReadOnly := false;
          FPropertiesPanelEdit2.ReadOnly := false;
          FPropertiesPanelEdit3.ReadOnly := false;
          FPropertiesPanelEdit4.ReadOnly := false;
          FPropertiesPanelSpin1.Visible := false;
          FPropertiesPanelSpin2.Visible := false;
          FPropertiesPanelSpin3.Visible := false;
          FPropertiesPanelSpin4.Visible := false; // TODO: icon iteration
          FPropertiesPanelCheckListBox.Visible := false;
          FPropertiesPanelEdit1.Text := FCurrentProject.MidletInfo.Name;
          FPropertiesPanelEdit2.Text := FCurrentProject.MidletInfo.Vendor;
          FPropertiesPanelEdit3.Text := FCurrentProject.MidletInfo.Version;
          FPropertiesPanelEdit4.Text := FCurrentProject.MidletInfo.Icon;
        end;
      ppmSourceFile:
        begin
          FPropertiesPanelLabel1.Caption := _('Name');
          FPropertiesPanelLabel2.Caption := _('Size');
          FPropertiesPanelLabel3.Caption := '';
          FPropertiesPanelLabel4.Caption := '';
          FPropertiesPanelEdit1.Visible := true;
          FPropertiesPanelEdit2.Visible := true;
          FPropertiesPanelEdit3.Visible := false;
          FPropertiesPanelEdit4.Visible := false;
          FPropertiesPanelEdit1.ReadOnly := true;
          FPropertiesPanelEdit2.ReadOnly := true;
          FPropertiesPanelEdit3.ReadOnly := true;
          FPropertiesPanelEdit4.ReadOnly := true;
          FPropertiesPanelSpin1.Visible := false;
          FPropertiesPanelSpin2.Visible := false;
          FPropertiesPanelSpin3.Visible := false;
          FPropertiesPanelSpin4.Visible := false;
          FPropertiesPanelCheckListBox.Visible := false;
          if FCurrentProject.SourceFiles[
            Tree.GetFirstSelected.Index].Exists then
          begin
            FPropertiesPanelEdit1.Text :=
              FCurrentProject.SourceFiles[Tree.GetFirstSelected.Index].Filename;
            FPropertiesPanelEdit2.Text := GetFileSizeAsString(
              FCurrentProject.SourceFiles[Tree.GetFirstSelected.Index].GetFullname);
          end;
        end;
      ppmResourceFile:
        begin
          FPropertiesPanelLabel1.Caption := _('Name');
          FPropertiesPanelLabel2.Caption := _('Size');
          FPropertiesPanelLabel3.Caption := _('Configurations');
          FPropertiesPanelLabel4.Caption := '';
          FPropertiesPanelEdit1.Visible := true;
          FPropertiesPanelEdit2.Visible := true;
          FPropertiesPanelEdit3.Visible := false;
          FPropertiesPanelEdit4.Visible := false;
          FPropertiesPanelEdit1.ReadOnly := true;
          FPropertiesPanelEdit2.ReadOnly := true;
          FPropertiesPanelEdit3.ReadOnly := true;
          FPropertiesPanelEdit4.ReadOnly := true;
          FPropertiesPanelSpin1.Visible := false;
          FPropertiesPanelSpin2.Visible := false;
          FPropertiesPanelSpin3.Visible := false;
          FPropertiesPanelSpin4.Visible := false;
          FPropertiesPanelCheckListBox.Visible := true;
          FPropertiesPanelCheckListBox.Items.CommaText :=
            FCurrentProject.BuildConfigurations.GetBuildConfigurationNamesAsCSV;
          if FCurrentProject.ResourceFiles[
            Tree.GetFirstSelected.Index].Exists then
          begin
            FPropertiesPanelEdit1.Text :=
              FCurrentProject.ResourceFiles[Tree.GetFirstSelected.Index].Filename;
            FPropertiesPanelEdit2.Text := GetFileSizeAsString(
              FCurrentProject.ResourceFiles[Tree.GetFirstSelected.Index].GetFullname);
            with TStringList.Create do
            try
              CommaText :=
                FCurrentProject.ResourceFiles[Tree.GetFirstSelected.Index].Configurations;
              if Count > 0 then
                for i := 0 to Count-1 do
                  for x := 0 to FPropertiesPanelCheckListBox.Items.Count - 1 do
                    FPropertiesPanelCheckListBox.Checked[x] :=
                      FPropertiesPanelCheckListBox.Checked[x]
                      or SameText(Strings[i],CONFIGS_ALL)
                      or SameText(Strings[i],FPropertiesPanelCheckListBox.Items[x]);
            finally
              Free;
            end;
          end;
        end;
      ppmBuildConfiguration:
        begin
          FPropertiesPanelLabel1.Caption := _('MIDlet Type');
          FPropertiesPanelLabel2.Caption := _('MIDP Version');
          FPropertiesPanelLabel3.Caption := _('Real Numbers');
          FPropertiesPanelLabel4.Caption := '';
          FPropertiesPanelEdit1.Visible := true;
          FPropertiesPanelEdit2.Visible := true;
          FPropertiesPanelEdit3.Visible := true;
          FPropertiesPanelEdit4.Visible := false;
          FPropertiesPanelEdit1.ReadOnly := true;
          FPropertiesPanelEdit2.ReadOnly := true;
          FPropertiesPanelEdit3.ReadOnly := true;
          FPropertiesPanelEdit4.ReadOnly := true;
          FPropertiesPanelSpin1.Visible := true;
          FPropertiesPanelSpin2.Visible := true;
          FPropertiesPanelSpin3.Visible := true;
          FPropertiesPanelSpin4.Visible := false;
          FPropertiesPanelCheckListBox.Visible := false;
          FPropertiesPanelEdit1.Text :=
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].GetMIDletTypeAsString;
          FPropertiesPanelEdit2.Text :=
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].GetMIDPVersionAsString;
          FPropertiesPanelEdit3.Text :=
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].GetRealNumbersAsString;
        end;
    end;
    if FPropertiesPanelSpin1.Visible then
      FPropertiesPanelEdit1.Width := FPropertiesPanelSpin1.Left - FPropertiesPanelEdit1.Left - 4
    else
      FPropertiesPanelEdit1.Width := Width - FPropertiesPanelEdit1.Left - 16;
    if FPropertiesPanelSpin2.Visible then
      FPropertiesPanelEdit2.Width := FPropertiesPanelSpin2.Left - FPropertiesPanelEdit2.Left - 4
    else
      FPropertiesPanelEdit2.Width := Width - FPropertiesPanelEdit2.Left - 16;
    if FPropertiesPanelSpin3.Visible then
      FPropertiesPanelEdit3.Width := FPropertiesPanelSpin3.Left - FPropertiesPanelEdit3.Left - 4
    else
      FPropertiesPanelEdit3.Width := Width - FPropertiesPanelEdit3.Left - 16;
    if FPropertiesPanelSpin4.Visible then
      FPropertiesPanelEdit4.Width := FPropertiesPanelSpin4.Left - FPropertiesPanelEdit4.Left - 4
    else
      FPropertiesPanelEdit4.Width := Width - FPropertiesPanelEdit4.Left - 16;
    FPropertiesPanel.Visible := true;
  finally
    EndUpdate;
  end;
end;

procedure Tmp3ProjectManager.EditChange(Sender: TObject);
begin
  case FPropertiesPanelMode of
    ppmProject:
      try
        if Sender = FPropertiesPanelEdit1 then
          FCurrentProject.MIDletInfo.Name := FPropertiesPanelEdit1.Text
        else if Sender = FPropertiesPanelEdit2 then
          FCurrentProject.MIDletInfo.Vendor := FPropertiesPanelEdit2.Text
        else if Sender = FPropertiesPanelEdit3 then
          FCurrentProject.MIDletInfo.Version := FPropertiesPanelEdit3.Text
        else {if Sender = FPropertiesPanelEdit4 then}
          FCurrentProject.MIDletInfo.Icon := FPropertiesPanelEdit4.Text
      finally
        Save(false);
      end;
    ppmSourceFile: ;
    ppmResourceFile: ;
    ppmBuildConfiguration: ;
  end;
end;

procedure Tmp3ProjectManager.SpinClick(Sender: TObject);
var u,d,e: boolean; mt,nt: Tmp3MIDletType; mv,nv: Tmp3MIDPVersion; mr,nr: Tmp3RealNumbers;
begin
  TSpTBXSpinButton(Sender).IsHotTracking(u,d,e);
  case FPropertiesPanelMode of
    ppmProject: ;
    ppmSourceFile: ;
    ppmResourceFile: ;
    ppmBuildConfiguration:
      begin
        if Sender = FPropertiesPanelSpin1 then begin
          mt := FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].MIDletType;
          nt := mt;
          if u and (mt < High(Tmp3MIDletType)) then
            Inc(nt)
          else if d and (mt > Low(Tmp3MIDletType)) then
            Dec(nt);
          if mt <> nt then
          try
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].MIDletType := nt;
          finally
            Save;
          end;
        end else if Sender = FPropertiesPanelSpin2 then begin
          mv := FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].MIDPVersion;
          nv := mv;
          if u and (mv < High(Tmp3MIDPVersion)) then
            Inc(nv)
          else if d and (mv > Low(Tmp3MIDPVersion)) then
            Dec(nv);
          if mv <> nv then
          try
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].MIDPVersion := nv;
          finally
            Save;
          end;
        end else {if Sender = FPropertiesPanelSpin3 then} begin
          mr := FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].RealNumbers;
          nr := mr;
          if u and (mr < High(Tmp3RealNumbers)) then
            Inc(nr)
          else if d and (mr > Low(Tmp3RealNumbers)) then
            Dec(nr);
          if mr <> nr then
          try
            FCurrentProject.BuildConfigurations[Tree.GetFirstSelected.Index].RealNumbers := nr;
          finally
            Save;
          end;
        end;
      end;
  end;
end;

procedure Tmp3ProjectManager.PopupMenuHandler(Sender: TObjecT);

  procedure RevertToBasePopupItems;
  var x: integer; action: TAction;
  begin
    for x := PopupMenu.Items.Count-1 downto 0 do begin
      action := TAction(PopupMenu.Items[x].Action);
      if assigned(action) and (action.Tag > 0) then
        PopupMenu.Items.Delete(x);
    end;
  end;

  function NewPopupItem(AAction: TAction): TSpTBXItem;
  begin
    result := TSpTBXItem.Create(PopupMenu);
    result.Action := AAction;
  end;

var ANode: PVirtualNode; si: TSpTBXSeparatorItem; x,n,nl: integer; de,re: boolean;
begin
  ANode := Tree.GetFirstSelected;
  if not assigned(ANode) then
    exit;
  RevertToBasePopupItems;
  nl := Tree.GetNodeLevel(ANode);
  if nl > 0 then begin
    si := TSpTBXSeparatorItem.Create(PopupMenu);
    si.Tag := nl;
    PopupMenu.Items.Insert(0,si);
    if nl = 1 then begin
      n := ANode.Index;
      de := false;
    end else begin
      n := ANode.Parent.Index;
      de := ANode.Index > 0;
    end;
    case n of
      0:
      begin
        re := de and (not FCurrentProject.SourceFiles[ANode.Index].Exists);
        for x := 0 to FSourceFilesActions.ActionCount - 1 do begin
          if TAction(FSourceFilesActions[x]).Name = 'actDeleteSourceFile' then
            TAction(FSourceFilesActions[x]).Enabled := de;
          if TAction(FSourceFilesActions[x]).Name = 'actRecreateSourceFile' then
            TAction(FSourceFilesActions[x]).Visible := re;
          if TAction(FSourceFilesActions[x]).Tag = nl then
            PopupMenu.Items.Insert(0,NewPopupItem(TAction(FSourceFilesActions[x])));
        end;
      end;
      1:
        for x := 0 to FResourceFilesActions.ActionCount - 1 do
          if TAction(FResourceFilesActions[x]).Tag = nl then
            PopupMenu.Items.Insert(0,NewPopupItem(TAction(FResourceFilesActions[x])));
      2:
      begin
        re := FCurrentProject.BuildConfigurations.Count > 1;
        for x := 0 to FBuildConfigurationsActions.ActionCount - 1 do begin
          if TAction(FBuildConfigurationsActions[x]).Name = 'actDeleteBuildConfiguration' then
            TAction(FBuildConfigurationsActions[x]).Visible := re;
          if TAction(FBuildConfigurationsActions[x]).Tag = nl then
            PopupMenu.Items.Insert(0,NewPopupItem(TAction(FBuildConfigurationsActions[x])));
        end;
      end;
    end;
  end;
end;

end.
