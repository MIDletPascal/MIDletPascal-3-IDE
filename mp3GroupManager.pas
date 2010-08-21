(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3GroupManager;

interface

uses
  Windows, Messages, SysUtils, Dialogs,
  Classes, Graphics, Controls, Forms,
  ImgList, ActnList,
  VirtualTrees,
  gnugettext,
  tuiControls,
  sitDockableManagers,
  mp3Group, mp3Consts, mp3FileKind;

type
  Tmp3GroupManager = class(TsitTreeDockableManager)
  private
    FProjectsActions: TActionList;
    FOnProjectChange: TNotifyEvent;
    FCurrentGroup: Tmp3Group;
    FActiveProject: string;
    FActiveProjectIndex: integer;
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
    procedure NewEmptyGroup;
    function GetSelectedItemName: string;
    procedure SetActiveProject(const Value: string);
    procedure CustomizeTree; override;
    procedure PerformRefresh; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function New(AName, ALocation: string): boolean; override;
    procedure Load(AFilename: string); override;
    procedure Save(ARefresh: boolean = True); override;
    procedure Close; override;
    function HasItemLoaded: boolean;
    procedure ExecuteCurrentElement;
    property CurrentGroup: Tmp3Group read FCurrentGroup;
    property ActiveProject: string read FActiveProject write SetActiveProject;
    property ActiveProjectIndex: integer read FActiveProjectIndex;
    property SelectedItemName: string read GetSelectedItemName;
    property OnProjectChange: TNotifyEvent
      read FOnProjectChange write FOnProjectChange;
    property ProjectsActions: TActionList
      read FProjectsActions write FProjectsActions;
  end;

implementation

{ Tmp3GroupManager }

procedure Tmp3GroupManager.AfterConstruction;
begin
  inherited;
  Caption := _('Group Manager');
  Width := GROUP_MANAGER_WIDTH_DEFAULT;
  MinClientHeight := GROUP_MANAGER_HEIGHT_DEFAULT;
end;

procedure Tmp3GroupManager.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FCurrentGroup);
end;

procedure Tmp3GroupManager.CustomizeTree;
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

procedure Tmp3GroupManager.PaintText(Sender: TBaseVirtualTree;
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
        if SameText(FActiveProject,FCurrentGroup.Projects[Node.Index]) then
          TargetCanvas.Font.Style := DEFAULT_MANAGER_ACTIVEITEM_FONTSTYLE;
        if not FileExists(FCurrentGroup.Projects[Node.Index]) then begin
          SetNodeColor(DEFAULT_MANAGER_ITEM_DISABLED_TEXTCOLOR, DEFAULT_MANAGER_ITEM_DISABLED_SELECTED_TEXTCOLOR);
          TargetCanvas.Font.Style := DEFAULT_MANAGER_INEXISTENTITEM_FONTSTYLE;
        end;
      end;
    end;
end;

procedure Tmp3GroupManager.Close;
begin
  if assigned(FCurrentGroup) then
    FCurrentGroup.Close;
  Modified := false;
end;

procedure Tmp3GroupManager.PerformRefresh;
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

procedure Tmp3GroupManager.DoubleClickHandler(Sender: TObject);
var Node: PVirtualNode;
begin
  Node := Tree.GetFirstSelected;
  if assigned(Node) then
    ExecNode(Node);
end;

procedure Tmp3GroupManager.Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := false;
end;

procedure Tmp3GroupManager.ExecNode(ANode: PVirtualNode);
begin
  if assigned(ANode) then
    if Tree.GetNodeLevel(ANode) = 2 then
      case ANode.Parent.Index of
        0: ActiveProject := SelectedItemName;
      end;
end;

procedure Tmp3GroupManager.ExecuteCurrentElement;
begin
  ExecNode(Tree.GetFirstSelected);
end;

procedure Tmp3GroupManager.GetImageIndexEx(Sender: TBaseVirtualTree;
    Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
    var Ghosted: Boolean; var ImageIndex: Integer;
    var ImageList: TCustomImageList);
begin
  ImageIndex := -1;
  case Sender.GetNodeLevel(Node) of
  0: ImageIndex := 10;
  1: case Node.Index of
      0: ImageIndex := -1;
     end;
  2: case Node.Parent.Index of
      0: ImageIndex := 5;
     end;
  end;
end;

function Tmp3GroupManager.GetSelectedItemName: string;
var ANode: PVirtualNode;
begin
  result := '';
  ANode := Tree.GetFirstSelected;
  if assigned(ANode) then
    case Tree.GetNodeLevel(ANode) of
      2: case ANode.Parent.Index of
          0: result := FCurrentGroup.Projects[ANode.Index];
         end;
    end;
end;

procedure Tmp3GroupManager.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
begin
  case Sender.GetNodeLevel(Node) of
  0:
    begin
      CellText := ExtractFileName(FCurrentGroup.Filename);
      if Modified then
        CellText := CellText + '*';
    end;
  1: case Node.Index of
      0: CellText := WideFormat(_('Projects')+' (%d)',[FCurrentGroup.Projects.Count]);
     end;
  2: case Node.Parent.Index of
      0: CellText := ExtractFileName(FCurrentGroup.Projects[Node.Index]);
     end;
  end;
end;

function Tmp3GroupManager.HasItemLoaded: boolean;
begin
  result := assigned(FCurrentGroup) and (FCurrentGroup.Loaded{ or FCurrentGroup.IsNew});
end;

procedure Tmp3GroupManager.InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    ChildCount := 1
  else if Sender.GetNodeLevel(Node) = 1 then begin
    case Node.Index of
      0: ChildCount := FCurrentGroup.Projects.Count;
    end;
  end;
end;

procedure Tmp3GroupManager.InitNode(Sender: TBaseVirtualTree; ParentNode,
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

procedure Tmp3GroupManager.KeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var Node: PVirtualNode;
begin
  case Key of
    VK_F2:
    begin
      Node := Tree.GetFirstSelected;
      if assigned(Node) then
        Tree.EditNode(Node, -1);
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

procedure Tmp3GroupManager.Load(AFilename: string);
begin
  if AFilename = '' then
    exit;
  if not Assigned(FCurrentGroup) then
    NewEmptyGroup;
  FCurrentGroup.Load(AFilename);
  if FCurrentGroup.Loaded and(FCurrentGroup.Projects.Count>0) then
    FActiveProject := FCurrentGroup.Projects[0];
  Modified := false;
end;

function Tmp3GroupManager.New(AName, ALocation: string): boolean;
var fn: string;
begin
  result := false;
  NewEmptyGroup;
  fn := IncludeTrailingPathDelimiter(ALocation)+AName+EXTENSION_GROUP;
  if FCurrentGroup.New(fn) then
    result := FCurrentGroup.Load(fn);
  FActiveProject := '';
  Modified := false;
end;

procedure Tmp3GroupManager.NewEmptyGroup;
begin
  if assigned(FCurrentGroup) then begin
    FCurrentGroup.Close;
    FCurrentGroup.Free;
  end;
  FCurrentGroup := Tmp3Group.Create;
end;

procedure Tmp3GroupManager.Save(ARefresh: boolean = True);
begin
  if assigned(FCurrentGroup) then
    FCurrentGroup.Save;
  Modified := false;
  RefreshManager;
end;

procedure Tmp3GroupManager.SelectionChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  //
end;

procedure Tmp3GroupManager.SetActiveProject(const Value: string);
var i: integer;
begin
  for i := 0 to FCurrentGroup.Projects.Count - 1 do
    if SameText(Value,FCurrentGroup.Projects[i]) then
    begin
      FActiveProjectIndex := i;
      FActiveProject := Value;
      if assigned(FOnProjectChange) then
        FOnProjectChange(Self);
      RefreshManager;
      break;
    end;
end;

procedure Tmp3GroupManager.PopupMenuHandler(Sender: TObjecT);

  procedure RevertToBasePopupItems;
  var x: integer; action: TAction;
  begin
    for x := PopupMenu.Items.Count-1 downto 0 do begin
      action := TAction(PopupMenu.Items[x].Action);
      if assigned(action) and (action.Tag > 0) then
        PopupMenu.Items.Delete(x);
    end;
  end;

  function NewPopupItem(AAction: TAction): TtuiMenuItem;
  begin
    result := TtuiMenuItem.Create(PopupMenu);
    result.Action := AAction;
  end;

var ANode: PVirtualNode; si: TtuiSeparatorMenuItem; x,n,nl: integer; de: boolean;
begin
  ANode := Tree.GetFirstSelected;
  if not assigned(ANode) then
    exit;
  RevertToBasePopupItems;
  nl := Tree.GetNodeLevel(ANode);
  if nl > 0 then begin
    si := TtuiSeparatorMenuItem.Create(PopupMenu);
    si.Tag := nl;
    PopupMenu.Items.Insert(0,si);
    if nl = 1 then begin
      n := ANode.Index;
      de := false;
    end else begin
      n := ANode.Parent.Index;
      de := true;
    end;
    case n of
      0:
      begin
        for x := 0 to FProjectsActions.ActionCount - 1 do begin
          if TAction(FProjectsActions[x]).Name = 'actExcludeProject' then
            TAction(FProjectsActions[x]).Enabled := de;
          if TAction(FProjectsActions[x]).Tag = nl then
            PopupMenu.Items.Insert(0,NewPopupItem(TAction(FProjectsActions[x])));
        end;
      end;
    end;
  end;
end;

end.
