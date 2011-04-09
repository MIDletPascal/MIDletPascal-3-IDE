unit mp3WelcomePageFrame;

interface

uses
  Windows, Forms, ActnList, Controls, Graphics, StdCtrls, ExtCtrls,
  VirtualTrees,
  tuiControlFactory, tuiControls,
  sitFrame;

type
  Tmp3WelcomePageSection = class;

  Tmp3WelcomePageFrame = class(TsitFrame)
  strict private
    const SECTIONS_SEPARATION_MARGIN: integer = 32;
  private
    FHeaderImage: TImage;
    FRecentProjectsSection: Tmp3WelcomePageSection;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure SetTitle(ATitle: widestring);
    procedure SetHeader(AImage: TImage);
    property Recents: Tmp3WelcomePageSection read FRecentProjectsSection;
  end;

  Tmp3WelcomePageSection = class
  strict private
    const MAINITEMS_SEPARATION_MARGIN: integer = 48;
  private
    FChangingSelection: boolean;
    FItemsChildCount: integer;
    FMaxItems: integer;
    FLabel: TtuiLabel;
    FPanel: TtuiPanel;
    FActionList: TActionList;
    FVst: TVirtualStringTree;
    procedure InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode; var ChildCount: Cardinal);
    procedure InitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure GetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: UnicodeString);
    procedure SelectionChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SetTop(AValue: integer);
    function GetTop: integer;
    procedure SetItemsChildCount(const Value: Integer);
    procedure SetMaxItems(const Value: Integer);
    procedure RefreshPanelHeight;
    function GetNodeAction(const ANodeIndex: Integer): TAction;
  public
    constructor Create(AOwner: Tmp3WelcomePageFrame);
    destructor Destroy; override;
    procedure LoadActionList(AActionList: TActionList);
    procedure SetSubTitle(ASubTitle: widestring);
    function GetHeight: integer;
    property Top: integer read GetTop write SetTop;
    property MaxItems: Integer read FMaxItems write SetMaxItems;
    property ItemsChildCount: Integer read FItemsChildCount write SetItemsChildCount;
  end;

implementation

uses
  SysUtils, ShellAPI,
  gnugettext,
  sitConsts,
  mp3Core;

{ Tmp3WelcomePageFrame }

procedure Tmp3WelcomePageFrame.AfterConstruction;
begin
  inherited;
  Color := clWhite;
  FRecentProjectsSection := Tmp3WelcomePageSection.Create(Self);
  FRecentProjectsSection.Top := SECTIONS_SEPARATION_MARGIN;
  FRecentProjectsSection.SetSubtitle('Recent Projects');
  FRecentProjectsSection.SetMaxItems(MRUINI_MAX);
end;

procedure Tmp3WelcomePageFrame.BeforeDestruction;
begin
  if assigned(FRecentProjectsSection) then
    FRecentProjectsSection.Free;
  inherited;
end;

procedure Tmp3WelcomePageFrame.SetTitle(ATitle: widestring);
begin
  Tab.Caption := ATitle;
end;

procedure Tmp3WelcomePageFrame.SetHeader(AImage: TImage);
begin
  if not assigned(FHeaderImage) then begin
    FHeaderImage := TImage.Create(Self);
    FHeaderImage.Parent := Self;
    FHeaderImage.AutoSize := true;
    FHeaderImage.Left := SECTIONS_SEPARATION_MARGIN - 16;
    FHeaderImage.Top := SECTIONS_SEPARATION_MARGIN - 12;
  end;
  FHeaderImage.Picture.Assign(AImage.Picture);
  FRecentProjectsSection.Top := FRecentProjectsSection.Top + FHeaderImage.Height;
end;

{ Tmp3WelcomePageSection }

constructor Tmp3WelcomePageSection.Create(AOwner: Tmp3WelcomePageFrame);
begin
  inherited Create;
  FItemsChildCount := 1;
  FMaxItems := 9;
  FChangingSelection := false;
  with AOwner.ControlFactory do
  begin
    FLabel := NewLabel.SetLeft(32).SetCaption('').SetFontSize(18).GetInstance;
    FPanel := NewPanel.SetLeft(32).SetColor($FAFAFA).SetBorders(false)
      .SetAnchors([akRight, akLeft, akTop]).GetInstance;
    FPanel.ClientWidth := AOwner.ClientWidth - 64;
  end;
  FVst := TVirtualStringTree.Create(AOwner);
  FVst.Parent := FPanel;
  FVst.ParentColor := true;
  FVst.Cursor := crHandPoint;
  FVst.BorderStyle := bsNone;
  FVst.Align := alClient;
  FVst.TreeOptions.PaintOptions := FVst.TreeOptions.PaintOptions
    - [toShowRoot,toShowDropmark,toAlwaysHideSelection] + [toShowHorzGridLines,toHotTrack,toUseBlendedSelection];
  FVst.ScrollBarOptions.ScrollBars := ssNone;
  FVst.Colors.FocusedSelectionColor := FVst.Color;
  FVst.Colors.FocusedSelectionBorderColor := clBlue;
  FVst.Colors.SelectionRectangleBorderColor := clRed;
  FVst.Colors.SelectionRectangleBlendColor := clRed;
  FVst.OnInitNode := InitNode;
  FVst.OnInitChildren := InitChildren;
  FVst.OnGetText := GetText;
  FVst.OnChange := SelectionChange;
  FActionList := TActionList.Create(AOwner);
end;

destructor Tmp3WelcomePageSection.Destroy;
begin
  //
  inherited;
end;

function Tmp3WelcomePageSection.GetHeight: integer;
begin
  result := FLabel.Height + MAINITEMS_SEPARATION_MARGIN + FPanel.Height;
end;

procedure Tmp3WelcomePageSection.RefreshPanelHeight;
begin
  FPanel.Height := integer(FVst.DefaultNodeHeight) * FMaxItems * (FItemsChildCount + 1);
end;

procedure Tmp3WelcomePageSection.SetItemsChildCount(const Value: Integer);
begin
  FItemsChildCount := Value;
  RefreshPanelHeight;
end;

procedure Tmp3WelcomePageSection.SetMaxItems(const Value: Integer);
begin
  FMaxItems := Value;
  RefreshPanelHeight;
end;

procedure Tmp3WelcomePageSection.SetSubtitle(ASubtitle: widestring);
begin
  FLabel.Caption := ASubtitle;
end;

function Tmp3WelcomePageSection.GetTop: integer;
begin
  result := FLabel.Top;
end;

procedure Tmp3WelcomePageSection.InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    ChildCount := FItemsChildCount;
end;

procedure Tmp3WelcomePageSection.InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  if Sender.GetNodeLevel(Node) = 0 then
    InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
end;

procedure Tmp3WelcomePageSection.SetTop(AValue: integer);
begin
  FLabel.Top := AValue;
  FPanel.Top := FLabel.Top + FLabel.Height + MAINITEMS_SEPARATION_MARGIN;
end;

function Tmp3WelcomePageSection.GetNodeAction(
  const ANodeIndex: Integer): TAction;
begin
  result := TAction(FActionList.Actions[ANodeIndex]);
end;

procedure Tmp3WelcomePageSection.LoadActionList(AActionList: TActionList);
var i: integer; action: TAction;
begin
  for i := FActionList.ActionCount-1 downto 0 do begin
    action := GetNodeAction(i);
    action.ActionList := nil;
    action.Free;
  end;
  for i := 0 to AActionList.ActionCount-1 do
    if TAction(AActionList.Actions[i]).Tag > -1 then begin
      action := TAction.Create(FActionList);
      action.Caption := TAction(AActionList.Actions[i]).Caption;
      action.Enabled := TAction(AActionList.Actions[i]).Enabled;
      action.OnExecute := TAction(AActionList.Actions[i]).OnExecute;
      action.ActionList := FActionList;
    end;
  FVst.RootNodeCount := FActionList.ActionCount;
end;

procedure Tmp3WelcomePageSection.GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: UnicodeString);
begin
  if FItemsChildCount = 0 then
    CellText := GetNodeAction(Node.Index).Caption
  else
    if Sender.GetNodeLevel(Node) = 0 then
      CellText := ExtractFileName(GetNodeAction(Node.Index).Caption)
    else
      CellText := ExtractFilePath(GetNodeAction(Node.Parent.Index).Caption);
end;

procedure Tmp3WelcomePageSection.SelectionChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  if FChangingSelection then
    exit
  else
    FChangingSelection := true;
  FVst.Selected[Node] := false;
  FVst.FocusedNode := nil;
  if Sender.GetNodeLevel(Node) = 0 then begin
    if GetNodeAction(Node.Index).Enabled then
      GetNodeAction(Node.Index).Execute;
  end else
    ShellExecute(0,'open',pchar(
      ExtractFilePath(GetNodeAction(Node.Parent.Index).Caption)
    ),'','',1);
  FChangingSelection := false;
end;

end.
