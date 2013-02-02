(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3ClassExplorerFrame;

interface

uses
  Windows, Classes, SysUtils, Controls, Forms,
  Graphics, Dialogs, ComCtrls, Grids, ImgList,
  SpTBXControls, SpTBXTabs, SpTBXEditors;

type
  Tmp3ClassExplorerFrame = class(TFrame)
    imgTree: TImageList;
  private
    FTreeViewPanel: TSpTBXPanel;
    FTreeView: TTreeView;
    FWorkAreaPanel: TSpTBXPanel;
    FPages: TSpTBXTabControl;
    FDetailsGrid: TStringGrid;
    FDataOffsetPanel: TSpTBXPanel;
    FDataOffsetTitleLabel: TSpTBXLabel;
    FDataOffsetLabel: TSpTBXLabel;
    FDataGrid: TStringGrid;
    FDisasmFindPanel: TSpTBXPanel;
    FDisasmFindButton: TSpTBXButton;
    FDisasmFindEdit: TSpTBXEdit;
    FDisasmGrid: TStringGrid;
    FExceptionsGrid: TStringGrid;
    FDetailsTab: TSpTBXTabItem;
    FDataTab: TSpTBXTabItem;
    FDisasmTab: TSpTBXTabItem;
    FExceptionsTab: TSpTBXTabItem;
  protected
    procedure FTreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure FTreeViewCollapsed(Sender: TObject; Node: TTreeNode);
    procedure FTreeViewExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure FDataGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FDataGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
      State: TGridDrawState);
    procedure FDisasmFindEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FDisamFindButtonClick(Sender: TObject);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure OpenClassFile(const AFilename: string);
  end;

implementation

{$R *.dfm}

uses
  ClassFile, ConstantPool, ByteArray, DisAsm, VMTypes,
  MethodList, FieldList, CodeAttribute, AttributeList;

const
  tagMagic = pointer(0);
  tagVersion = pointer(1);
  tagConstantPoolFolder = pointer(3);
  tagAccessFlags = pointer(4);
  tagThisClass = pointer(5);
  tagSuperClass = pointer(6);
  tagInterfacesFolder = pointer(7);
  tagFieldsFolder = pointer(8);
  tagMethodsFolder = pointer(9);
  tagAttributesFolder = pointer(10);

  tagConstant = pointer(11);
  tagInterface = pointer(12);
  tagField = pointer(13);
  tagMethod = pointer(14);
  tagAttribute = pointer(15);
  tagCode = pointer(16);
  tagMethodAttribFolder = pointer(17);
  tagMethodAttribute = pointer(18);
  tagMethodAccessFlags = pointer(19);

  levRoot = 0;
  levSecond = 1;
  levThird = 2;
  levMethod = 2;
  levConstant = 2;
  levMethodAttr = 2;

  imgString = 0;
  imgUTF8 = 1;
  imgInt = 2;
  imgFloat = 3;
  imgLong = 4;
  imgDouble = 5;
  imgClass = 6;
  imgInterface = 7;
  imgMethod = 8;
  imgIMethod = 9;
  imgField = 10;
  imgNAT = 11;
  imgMagic = 12;
  imgFolder = 13;
  imgFolderOpen = 14;
  imgGear = 15;
  imgByte = 16;
  imgChar = 17;
  imgArray = 18;
  imgShort = 19;
  imgBoolean = 20;

  NumDataCols = 16;

var
  theClass: TClassFile;
  CurData: TByteArray;

function TConstToStr(Constant: TConstant): string; inline;
begin
  if Constant is TConstant_Class then
    Result := 'CLASS'
  else if Constant is TConstant_FieldRef then
    Result := 'FIELDREF'
  else if Constant is TConstant_MethodRef then
    Result := 'METHODREF'
  else if Constant is TConstant_IMethodRef then
    Result := 'IMETHODREF'
  else if Constant is TConstant_String then
    Result := 'STRING'
  else if Constant is TConstant_NameAndType then
    Result := 'NAME_AND_TYPE'
  else if Constant is TConstant_UTF8 then
    Result := 'UTF8'
  else if Constant is TConstant_Integer then
    Result := 'INT'
  else if Constant is TConstant_Float then
    Result := 'FLOAT'
  else if Constant is TConstant_Long then
    Result := 'LONG'
  else if Constant is TConstant_Double then
    Result := 'DOUBLE'
  else
    Result := 'UNDETERMINED TYPE';
end;

{ Tmp3ClassExplorerFrame }

procedure Tmp3ClassExplorerFrame.AfterConstruction;
begin
  inherited;
  FTreeViewPanel := TSpTBXPanel.Create(Self);
  FTreeViewPanel.Parent := Self;
  FTreeViewPanel.Align := alLeft;
  FTreeViewPanel.Width := 200;
  FTreeView := TTreeView.Create(Self);
  FTreeView.Parent := FTreeViewPanel;
  FTreeView.Align := alClient;
  FTreeView.BorderStyle := bsNone;
  FTreeView.Images := imgTree;
  FTreeView.ReadOnly := true;
  FWorkAreaPanel := TSpTBXPanel.Create(Self);
  FWorkAreaPanel.Parent := Self;
  FWorkAreaPanel.Align := alClient;
  FPages := TSpTBXTabControl.Create(Self);
  FPages.Parent := FWorkAreaPanel;
  FPages.Align := alClient;

  FDetailsTab := FPages.Add('Details');
  FDetailsGrid := TStringGrid.Create(Self);
  FDetailsGrid.Parent := FPages.Pages[0];
  FDetailsGrid.Align := alClient;
  FDetailsGrid.BorderStyle := bsNone;
  FDetailsGrid.ColCount := 2;
  FDetailsGrid.DefaultColWidth := 200;
  FDetailsGrid.DefaultRowHeight := 16;
  FDetailsGrid.FixedCols := 0;
  FDetailsGrid.RowCount := 1;
  FDetailsGrid.FixedRows := 0;
  FDetailsGrid.Options := [goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking];

  FDataTab := FPages.Add('Data');
  FDataOffsetPanel := TSpTBXPanel.Create(Self);
  FDataOffsetPanel.Parent := FPages.Pages[1];
  FDataOffsetPanel.Align := alTop;
  FDataOffsetPanel.Height := 25;
  FDataOffsetTitleLabel := TSpTBXLabel.Create(Self);
  FDataOffsetTitleLabel.Parent := FDataOffsetPanel;
  FDataOffsetTitleLabel.Caption := 'Offset:';
  FDataOffsetTitleLabel.Font.Style := [fsBold];
  FDataOffsetTitleLabel.Left := 6;
  FDataOffsetTitleLabel.Top := 4;
  FDataOffsetLabel := TSpTBXLabel.Create(Self);
  FDataOffsetLabel.Parent := FDataOffsetPanel;
  FDataOffsetLabel.Caption := '0';
  FDataOffsetLabel.Left := 48;
  FDataOffsetLabel.Top := 4;
  FDataGrid := TStringGrid.Create(Self);
  FDataGrid.Parent := FPages.Pages[1];
  FDataGrid.Align := alClient;
  FDataGrid.BorderStyle := bsNone;
  FDataGrid.ColCount := NumDataCols;
  FDataGrid.DefaultColWidth := 20;
  FDataGrid.DefaultRowHeight := 20;
  FDataGrid.FixedCols := 0;
  FDataGrid.FixedRows := 0;
  FDataGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goThumbTracking];

  FDisasmTab := FPages.Add('Disassembly');
  FDisasmFindPanel := TSpTBXPanel.Create(Self);
  FDisasmFindPanel.Parent := FPages.Pages[2];
  FDisasmFindPanel.Align := alTop;
  FDisasmFindPanel.Height := 25;
  FDisasmFindButton := TSpTBXButton.Create(Self);
//  FDisasmFindButton.Caption := 'Find';
  FDisasmFindButton.Height := 21;
  FDisasmFindButton.Width := 23;
  FDisasmFindButton.Parent := FDisasmFindPanel;
  FDisasmFindButton.Left := 4;
  FDisasmFindButton.Top := 2;
  FDisasmFindButton.Cursor := crHandPoint;
  FDisasmFindButton.Images := imgTree;
  FDisasmFindButton.ImageIndex := 7;
  FDisasmFindEdit := TSpTBXEdit.Create(Self);
  FDisasmFindEdit.Parent := FDisasmFindPanel;
  FDisasmFindEdit.Left := 28;
  FDisasmFindEdit.Top := 2;
  FDisasmGrid := TStringGrid.Create(Self);
  FDisasmGrid.Parent := FPages.Pages[2];
  FDisasmGrid.Align := alClient;
  FDisasmGrid.BorderStyle := bsNone;
  FDisasmGrid.ColCount := 3;
  FDisasmGrid.DefaultColWidth := 200;
  FDisasmGrid.DefaultRowHeight := 18;
  FDisasmGrid.FixedCols := 0;
  FDisasmGrid.RowCount := 2;
  FDisasmGrid.Font.Name := 'Courier New';
  FDisasmGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect, goThumbTracking];

  FExceptionsTab := FPages.Add('Exceptions');
  FExceptionsGrid := TStringGrid.Create(Self);
  FExceptionsGrid.Parent := FPages.Pages[3];
  FExceptionsGrid.Align := alClient;
  FExceptionsGrid.BorderStyle := bsNone;
  FExceptionsGrid.ColCount := 4;
  FExceptionsGrid.DefaultColWidth := 100;
  FExceptionsGrid.DefaultRowHeight := 18;
  FExceptionsGrid.FixedCols := 0;
  FExceptionsGrid.RowCount := 2;
  FExceptionsGrid.Font.Name := 'Courier New';
  FExceptionsGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRowSelect, goThumbTracking];

  FTreeView.OnChange := FTreeViewChange;
  FTreeView.OnExpanding := FTreeViewExpanding;
  FTreeView.OnCollapsed := FTreeViewCollapsed;

  FDataGrid.OnDrawCell := FDataGridDrawCell;
  FDataGrid.OnSelectCell := FDataGridSelectCell;

  FDisasmFindEdit.OnKeyDown := FDisasmFindEditKeyDown;
  FDisasmFindButton.OnClick := FDisamFindButtonClick;

  FDetailsGrid.ColWidths[0] := 120;
  FDetailsGrid.ColWidths[1] := 800;

  FDisasmGrid.ColWidths[0] := 50;
  FDisasmGrid.ColWidths[1] := 120;
  FDisasmGrid.ColWidths[2] := 800;
  FDisasmGrid.Cells[0, 0] := 'Offset';
  FDisasmGrid.Cells[1, 0] := 'Instruction';
  FDisasmGrid.Cells[2, 0] := 'Comments';
end;

procedure Tmp3ClassExplorerFrame.BeforeDestruction;
begin
  FreeAndNil(theClass);
  inherited;
end;

procedure Tmp3ClassExplorerFrame.OpenClassFile(const AFilename: string);
var Node: TTreeNode;
begin
  if not FileExists(AFilename) then
    exit;

  CurData := nil;
  FExceptionsTab.Visible := False;
  FDisasmTab.Visible := False;
  FDataTab.Visible := False;

  FreeAndNil(theClass);
  theClass := TClassFile.Create(AFilename);

  FTreeView.Items.Clear;

  Node := FTreeView.Items.AddChild(nil, 'Magic');
  Node.Data := tagMagic;
  Node.ImageIndex := imgMagic;
  Node.SelectedIndex := imgMagic;
  Node := FTreeView.Items.AddChild(nil, 'Version');
  Node.Data := tagVersion;
  Node.ImageIndex := imgGear;
  Node.SelectedIndex := imgGear;
  Node := FTreeView.Items.AddChild(nil, 'Access Flags');
  Node.Data := tagAccessFlags;
  Node.ImageIndex := imgGear;
  Node.SelectedIndex := imgGear;
  Node := FTreeView.Items.AddChild(nil, 'This Class');
  Node.Data := tagThisClass;
  Node.ImageIndex := imgGear;
  Node.SelectedIndex := imgGear;
  Node := FTreeView.Items.AddChild(nil, 'Super Class');
  Node.Data := tagSuperClass;
  Node.ImageIndex := imgGear;
  Node.SelectedIndex := imgGear;

  Node := FTreeView.Items.AddChild(nil, 'Constant Pool');
  Node.Data := tagConstantPoolFolder;
  Node.HasChildren := theClass.ConstantPool.Count > 0;
  Node.ImageIndex := imgFolder;
  Node.SelectedIndex := imgFolderOpen;

  Node := FTreeView.Items.AddChild(nil, 'Interfaces');
  Node.Data := tagInterfacesFolder;
  Node.HasChildren := theClass.Interfaces.Count > 0;
  Node.ImageIndex := imgFolder;
  Node.SelectedIndex := imgFolderOpen;
  Node := FTreeView.Items.AddChild(nil, 'Fields');
  Node.Data := tagFieldsFolder;
  Node.HasChildren := theClass.Fields.Count > 0;
  Node.ImageIndex := imgFolder;
  Node.SelectedIndex := imgFolderOpen;
  Node := FTreeView.Items.AddChild(nil, 'Methods');
  Node.Data := tagMethodsFolder;
  Node.HasChildren := theClass.Methods.Count > 0;
  Node.ImageIndex := imgFolder;
  Node.SelectedIndex := imgFolderOpen;
  Node := FTreeView.Items.AddChild(nil, 'Attributes');
  Node.Data := tagAttributesFolder;
  Node.HasChildren := theClass.Attributes.Count > 0;
  Node.ImageIndex := imgFolder;
  Node.SelectedIndex := imgFolderOpen;
end;

procedure Tmp3ClassExplorerFrame.FDisamFindButtonClick(Sender: TObject);
var Col, Row: integer;
begin
  for Row := FDisasmGrid.Row + 1 to FDisasmGrid.RowCount - 1 do
    for Col := 0 to FDisasmGrid.ColCount - 1 do begin
      if pos(FDisasmFindEdit.Text, FDisasmGrid.Cells[Col, Row]) <> 0 then begin
        FDisasmGrid.Row := Row;
        FDisasmGrid.Col := Col;
        Exit;
      end;
    end;
end;

procedure Tmp3ClassExplorerFrame.FDisasmFindEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    FDisasmFindButton.Click;
end;

procedure Tmp3ClassExplorerFrame.FDataGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var Offs: integer; s: string;
begin
  if CurData = nil then
    Exit;
  Offs := ACol + ARow * NumDataCols;
  if Offs > CurData.Count - 1 then
    Exit;
  s := format('%x', [CurData.Items[Offs]]);
  if length(s) < 2 then
    s := '0' + s;
  FDataGrid.Canvas.TextOut(Rect.Left + 4, Rect.Top + 2, s);
end;

procedure Tmp3ClassExplorerFrame.FDataGridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  FDataOffsetLabel.Caption := inttostr(ACol + ARow * NumDataCols);
end;

procedure Tmp3ClassExplorerFrame.FTreeViewChange(Sender: TObject; Node: TTreeNode);

  procedure ShowMagic;
  begin
    FDetailsGrid.RowCount := 1;
    FDetailsGrid.Cells[0, 0] := 'Magic';
    FDetailsGrid.Cells[1, 0] := '0xCAFEBABE';
  end;

  procedure ShowVersion;
  begin
    FDetailsGrid.RowCount := 2;
    FDetailsGrid.Cells[0, 0] := 'Major Version';
    FDetailsGrid.Cells[0, 1] := 'Minor Version';
    FDetailsGrid.Cells[1, 0] := inttostr(theClass.MajorVersion);
    FDetailsGrid.Cells[1, 1] := inttostr(theClass.MinorVersion);
  end;

  function Disp_Masked(masked: integer): string;
  begin
    if masked = 0 then
      Result := ''
    else
      Result := 'ON';
  end;

  procedure ShowAccessFlags;
  begin
    FDetailsGrid.RowCount := 5;
    FDetailsGrid.Cells[0, 0] := 'ACC_PUBLIC';
    FDetailsGrid.Cells[1, 0] := disp_masked(theClass.AccessFlags and CACC_PUBLIC);
    FDetailsGrid.Cells[0, 1] := 'ACC_FINAL';
    FDetailsGrid.Cells[1, 1] := disp_masked(theClass.AccessFlags and CACC_FINAL);
    FDetailsGrid.Cells[0, 2] := 'ACC_SUPER';
    FDetailsGrid.Cells[1, 2] := disp_masked(theClass.AccessFlags and CACC_SUPER);
    FDetailsGrid.Cells[0, 3] := 'ACC_INTERFACE';
    FDetailsGrid.Cells[1, 3] := disp_masked(theClass.AccessFlags and CACC_INTERFACE);
    FDetailsGrid.Cells[0, 4] := 'ACC_ABSTRACT';
    FDetailsGrid.Cells[1, 4] := disp_masked(theClass.AccessFlags and CACC_ABSTRACT);
  end;

  procedure ShowMethodAccessFlags;
  var Method: MethodList.TMethod;
  begin
    Method := theClass.Methods[Node.Parent.Index]; (* LEVEL DEPENDENT!!! *)

    FDetailsGrid.RowCount := 9;
    FDetailsGrid.Cells[0, 0] := 'ACC_PUBLIC';
    FDetailsGrid.Cells[1, 0] := disp_masked(Method.AccessFlags and MACC_PUBLIC);
    FDetailsGrid.Cells[0, 1] := 'ACC_PRIVATE';
    FDetailsGrid.Cells[1, 1] := disp_masked(Method.AccessFlags and MACC_PRIVATE);
    FDetailsGrid.Cells[0, 2] := 'ACC_PROTECTED';
    FDetailsGrid.Cells[1, 2] := disp_masked(Method.AccessFlags and MACC_PROTECTED);
    FDetailsGrid.Cells[0, 3] := 'ACC_STATIC';
    FDetailsGrid.Cells[1, 3] := disp_masked(Method.AccessFlags and MACC_STATIC);
    FDetailsGrid.Cells[0, 4] := 'ACC_FINAL';
    FDetailsGrid.Cells[1, 4] := disp_masked(Method.AccessFlags and MACC_FINAL);
    FDetailsGrid.Cells[0, 5] := 'ACC_SYNCHRONIZED';
    FDetailsGrid.Cells[1, 5] := disp_masked(Method.AccessFlags and MACC_SYNCHRONIZED);
    FDetailsGrid.Cells[0, 6] := 'ACC_NATIVE';
    FDetailsGrid.Cells[1, 6] := disp_masked(Method.AccessFlags and MACC_NATIVE);
    FDetailsGrid.Cells[0, 7] := 'ACC_ABSTRACT';
    FDetailsGrid.Cells[1, 7] := disp_masked(Method.AccessFlags and MACC_ABSTRACT);
    FDetailsGrid.Cells[0, 8] := 'ACC_STRICT';
    FDetailsGrid.Cells[1, 8] := disp_masked(Method.AccessFlags and MACC_STRICT);
  end;

   // j-a-s-d: comp is an 64-bit signed integer but the compiler treats it as a real type,
   // so casting is not an option. I don't like using trunc() or creating a record
   // with an Int64 field and casting the comp value. So I chose the "absolute" way.
  function comptostr(AComp: comp): string;
  var AInt64: Int64 absolute AComp;
  begin
    result := inttostr(AInt64);
  end;

  procedure ShowConstantDetails;
  var Constant: TConstant;
  begin
    Constant := theClass.ConstantPool[Node.Index];

    FDetailsGrid.RowCount := 1;

    FDetailsGrid.Cells[0, 0] := 'Constant Type';
    FDetailsGrid.Cells[1, 0] := TConstToStr(Constant);

    if Constant is TConstant_Class then begin
      FDetailsGrid.RowCount := 3;
      FDetailsGrid.Cells[0, 1] := 'Name Index';
      FDetailsGrid.Cells[1, 1] := inttostr(TConstant_Class(Constant).NameIndex);
      FDetailsGrid.Cells[0, 2] := 'Class Name';
      FDetailsGrid.Cells[1, 2] :=
        theClass.ResolveAtom(TConstant_Class(Constant).NameIndex);
    end else
      if (Constant is TConstant_FMI) then begin
        FDetailsGrid.RowCount := 5;
        FDetailsGrid.Cells[0, 1] := 'Class Index';
        FDetailsGrid.Cells[1, 1] := inttostr(TConstant_FMI(Constant).ClassIndex);
        FDetailsGrid.Cells[0, 2] := 'Name And Type Index';
        FDetailsGrid.Cells[1, 2] :=
          inttostr(TConstant_FMI(Constant).NameAndTypeIndex);

        FDetailsGrid.Cells[0, 3] := 'Class String';
        FDetailsGrid.Cells[1, 3] :=
          theClass.ResolveAtom(TConstant_FMI(Constant).ClassIndex);
        FDetailsGrid.Cells[0, 4] := 'Name And Type String';
        FDetailsGrid.Cells[1, 4] :=
          theClass.ResolveAtom(TConstant_FMI(Constant).NameAndTypeIndex);
      end else
        if Constant is TConstant_String then begin
          FDetailsGrid.RowCount := 3;
          FDetailsGrid.Cells[0, 1] := 'StringIndex';
          FDetailsGrid.Cells[1, 1] :=
            inttostr(TConstant_String(Constant).StringIndex);
          FDetailsGrid.Cells[0, 2] := 'String';
          FDetailsGrid.Cells[1, 2] :=
            theClass.ResolveAtom(TConstant_String(Constant).StringIndex);
        end else
          if Constant is TConstant_NameAndType then begin
            FDetailsGrid.RowCount := 5;
            FDetailsGrid.Cells[0, 1] := 'NameIndex';
            FDetailsGrid.Cells[1, 1] :=
              inttostr(TConstant_NameAndType(Constant).NameIndex);
            FDetailsGrid.Cells[0, 2] := 'DescriptorIndex';
            FDetailsGrid.Cells[1, 2] :=
              inttostr(TConstant_NameAndType(Constant).DescriptorIndex);
            FDetailsGrid.Cells[0, 3] := 'Name String';
            FDetailsGrid.Cells[1, 3] :=
              theClass.ResolveAtom(TConstant_NameAndType(Constant).NameIndex);
            FDetailsGrid.Cells[0, 4] := 'Descriptor String';
            FDetailsGrid.Cells[1, 4] :=
              theClass.ResolveAtom(TConstant_NameAndType(Constant).DescriptorIndex);
          end else
            if Constant is TConstant_UTF8 then begin
              FDetailsGrid.RowCount := 3;
              FDetailsGrid.Cells[0, 1] := 'Length';
              FDetailsGrid.Cells[1, 1] :=
                inttostr(TConstant_UTF8(Constant).Length);
              FDetailsGrid.Cells[0, 2] := 'UTF8 String';
              FDetailsGrid.Cells[1, 2] := TConstant_UTF8(Constant).Bytes;
            end else
              if Constant is TConstant_Integer then begin
                FDetailsGrid.RowCount := 2;
                FDetailsGrid.Cells[0, 1] := 'Value';
                FDetailsGrid.Cells[1, 1] :=
                  inttostr(TConstant_Integer(Constant).Bytes);
              end else
                if Constant is TConstant_Float then begin
                  FDetailsGrid.RowCount := 2;
                  FDetailsGrid.Cells[0, 1] := 'Value';
                  FDetailsGrid.Cells[1, 1] :=
                    floattostr(TConstant_Float(Constant).Value);
                end else
                  if Constant is TConstant_Long then begin
                    FDetailsGrid.RowCount := 2;
                    FDetailsGrid.Cells[0, 1] := 'Value';
                    FDetailsGrid.Cells[1, 1] :=
                      comptostr(TConstant_Long(Constant).Value);
                  end else
                    if Constant is TConstant_Double then begin
                      FDetailsGrid.RowCount := 2;
                      FDetailsGrid.Cells[0, 1] := 'Value';
                      FDetailsGrid.Cells[1, 1] :=
                        floattostr(TConstant_Double(Constant).Value);
                    end;
  end;
                 
  procedure ShowThisClass;
  begin
    FDetailsGrid.RowCount := 2;
    FDetailsGrid.Cells[0, 0] := 'This Class Index';
    FDetailsGrid.Cells[0, 1] := 'This Class Name';
    FDetailsGrid.Cells[1, 0] := inttostr(theClass.ThisClass);
    FDetailsGrid.Cells[1, 1] := theClass.ResolveAtom(theClass.ThisClass);
  end;

  procedure ShowSuperClass;
  begin
    FDetailsGrid.RowCount := 2;
    FDetailsGrid.Cells[0, 0] := 'Super Class Index';
    FDetailsGrid.Cells[0, 1] := 'Super Class Name';
    FDetailsGrid.Cells[1, 0] := inttostr(theClass.SuperClass);
    FDetailsGrid.Cells[1, 1] := theClass.ResolveAtom(theClass.SuperClass);
  end;

  procedure ShowMethodDetails;
  var Method: MethodList.TMethod;
  begin
    Method := theClass.Methods[Node.Index];

    FDetailsGrid.RowCount := 4;
    FDetailsGrid.Cells[0, 0] := 'NameIndex';
    FDetailsGrid.Cells[1, 0] := IntToStr(Method.NameIndex);
    FDetailsGrid.Cells[0, 1] := 'DescriptorIndex';
    FDetailsGrid.Cells[1, 1] := IntToStr(Method.DescriptorIndex);
    FDetailsGrid.Cells[0, 2] := 'Name';
    FDetailsGrid.Cells[1, 2] := theClass.ResolveAtom(Method.NameIndex);
    FDetailsGrid.Cells[0, 3] := 'Method Signature';
    FDetailsGrid.Cells[1, 3] := theClass.ResolveAtom(Method.DescriptorIndex);
    FExceptionsTab.Visible := True;
    FDisasmTab.Visible := True;
    FDataTab.Visible := True;
  end;

  procedure ShowFieldDetails;
  var Field: TField;
  begin
    Field := theClass.Fields[Node.Index];

    FDetailsGrid.RowCount := 4;
    FDetailsGrid.Cells[0, 0] := 'NameIndex';
    FDetailsGrid.Cells[1, 0] := IntToStr(Field.NameIndex);
    FDetailsGrid.Cells[0, 1] := 'DescriptorIndex';
    FDetailsGrid.Cells[1, 1] := IntToStr(Field.DescriptorIndex);
    FDetailsGrid.Cells[0, 2] := 'Name';
    FDetailsGrid.Cells[1, 2] := theClass.ResolveAtom(Field.NameIndex);
    FDetailsGrid.Cells[0, 3] := 'Field Signature';
    FDetailsGrid.Cells[1, 3] := theClass.ResolveAtom(Field.DescriptorIndex);
  end;

  procedure ShowAttributeDetails(Attr: TAttribute);
  begin
    FDetailsGrid.RowCount := 2;
    FDetailsGrid.Cells[0, 0] := 'NameIndex';
    FDetailsGrid.Cells[1, 0] := IntToStr(Attr.NameIndex);
    FDetailsGrid.Cells[0, 1] := 'Data (Info) Length';
    FDetailsGrid.Cells[1, 1] := IntToStr(Attr.Info.Count);

    CurData := Attr.Info;
    FDataGrid.RowCount := CurData.Count div NumDataCols + 1;
    FDataGrid.Invalidate;
  end;

  procedure ShowCodeDetails;
  var Code: TCodeAttribute; Ptr, Row, RowCount, x: integer; Comments, s: string;
    Offs: integer; tmp: string; Method: MethodList.TMethod;
  begin
    Method := theClass.Methods[Node.Index];

    Code := theClass.Methods[Node.Index].Code;
    if Code = nil then
    begin
      FExceptionsTab.Visible := False;
      FDisasmTab.Visible := False;
      FDataTab.Visible := False;
      Exit;
    end;

    FDetailsGrid.RowCount := 8;
    FDetailsGrid.Cells[0, 0] := 'NameIndex';
    FDetailsGrid.Cells[1, 0] := IntToStr(Method.NameIndex);
    FDetailsGrid.Cells[0, 1] := 'DescriptorIndex';
    FDetailsGrid.Cells[1, 1] := IntToStr(Method.DescriptorIndex);
    FDetailsGrid.Cells[0, 2] := 'Name';
    FDetailsGrid.Cells[1, 2] := theClass.ResolveAtom(Method.NameIndex);
    FDetailsGrid.Cells[0, 3] := 'Method Signature';
    FDetailsGrid.Cells[1, 3] := theClass.ResolveAtom(Method.DescriptorIndex);
    FDetailsGrid.Cells[0, 4] := 'Max Stack';
    FDetailsGrid.Cells[1, 4] := IntToStr(Code.MaxStack);
    FDetailsGrid.Cells[0, 5] := 'Max Locals';
    FDetailsGrid.Cells[1, 5] := IntToStr(Code.MaxLocals);
    FDetailsGrid.Cells[0, 6] := 'Code Length';
    FDetailsGrid.Cells[1, 6] := IntToStr(Code.CodeLength);

    // j-a-s-d
    FDetailsGrid.Cells[0, 7] := 'Exception Table Length';
    FDetailsGrid.Cells[1, 7] := IntToStr(Code.ExceptionTableLength);
    if Code.ExceptionTableLength = 0 then begin
      FExceptionsGrid.RowCount := 0;
    end else begin
      FExceptionsGrid.Visible := true;
      FExceptionsGrid.RowCount := Code.ExceptionTable.Count * 4;
      FExceptionsGrid.Invalidate;
      FExceptionsGrid.Cells[0, 0] := 'Start';
      FExceptionsGrid.Cells[1, 0] := 'End';
      FExceptionsGrid.Cells[2, 0] := 'Handler';
      FExceptionsGrid.Cells[3, 0] := 'Catch Type';
      for x := 0 to Code.ExceptionTable.Count - 1 do
        with TExceptionEntry(Code.ExceptionTable.Items[x]) do begin
          FExceptionsGrid.Cells[0, x + 1] := IntToStr(StartPC);
          FExceptionsGrid.Cells[1, x + 1] := IntToStr(EndPC);
          FExceptionsGrid.Cells[2, x + 1] := IntToStr(HandlerPC);
          FExceptionsGrid.Cells[3, x + 1] := IntToStr(CatchType);
        end;
    end;

    CurData := Code.Code;
    FDataGrid.RowCount := CurData.Count div NumDataCols + 1;
    FDataGrid.Invalidate;

    Ptr := 0;
    RowCount := 2;
    Row := 1;
    while Ptr <= Code.CodeLength - 1 do begin
      FDisasmGrid.RowCount := RowCount;

      FDisasmGrid.Cells[0, Row] := inttostr(Ptr);
      s := Disassemble(Code.Code, Ptr, False, theClass.ConstantPool, Comments);
      FDisasmGrid.Cells[1, Row] := s;
      FDisasmGrid.Cells[2, Row] := Comments;

      if copy(s, 1, 5) = 'table' then begin
        tmp := '';

        for Offs := 1 to length(s) do begin
          if s[Offs] = #0 then begin
            FDisasmGrid.Cells[1, Row] := tmp;
            tmp := '';
            inc(RowCount);
            inc(Row);
          end else
            tmp := tmp + s[Offs];
        end;

        inc(RowCount);
        inc(Row);

        {while (P+Offs)^ <> #0 do
        begin
             StrECopy( tmp, P+Offs );
             FDisasmGrid.Cells[1,Row] := s;

             inc( RowCount );
             inc( Row );
        end;}
      end;

      inc(RowCount);
      inc(Row);
    end;

    FExceptionsTab.Visible := True;
    FDisasmTab.Visible := True;

    FDisasmGrid.FixedRows := 1;
  end;

begin
  FDetailsGrid.RowCount := 0;
  FDetailsGrid.Rows[0].Clear;
  FDataGrid.RowCount := 0;
  FDisasmGrid.RowCount := 0;
  //FDetailsTab.Visible := False;
  FDataTab.Visible := False;
  FDisasmTab.Visible := False;
  FExceptionsTab.Visible := False;

  FPages.ActiveTabIndex := 0;

  if Node.Data = tagMagic then
    ShowMagic
  else if Node.Data = tagVersion then
    ShowVersion
  else if Node.Data = tagThisClass then
    ShowThisClass
  else if Node.Data = tagSuperClass then
    ShowSuperClass
  else if Node.Data = tagAccessFlags then
    ShowAccessFlags
  else
    if Node.Data = tagConstant then
      ShowConstantDetails
    else
      if Node.Data = tagMethod then begin
        ShowMethodDetails;
        ShowCodeDetails;
      end else
        if Node.Data = tagField then
          ShowFieldDetails
        else
          if Node.Data = tagAttribute then begin
            ShowAttributeDetails(theClass.Attributes[Node.Index]);
          end else
            if Node.Data = tagMethodAttribute then begin
              ShowAttributeDetails(theClass.Methods[Node.Index].Attributes[Node.Index]);
            end else
              if Node.Data = tagMethodAccessFlags then
                ShowMethodAccessFlags
              else
                if Node.Data = tagCode then
                  ShowCodeDetails
                else
                  // ?
end;

procedure Tmp3ClassExplorerFrame.FTreeViewCollapsed(Sender: TObject; Node: TTreeNode);
begin
  Node.DeleteChildren;
  Node.HasChildren := True;
end;

procedure Tmp3ClassExplorerFrame.FTreeViewExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);

  procedure ExpandConstantPool;
  var NewNode: TTreeNode; i: integer; Constant: TConstant;
  begin
    for i := 0 to theClass.ConstantPool.Count - 1 do begin
      Constant := theClass.ConstantPool[i];
      NewNode := FTreeView.Items.AddChild(Node, '[' + inttostr(i) + '] ' +
        theClass.ConstantPool.ResolveString(i));
      NewNode.Data := tagConstant;

      if i = 0 then
      begin
        NewNode.ImageIndex := imgGear;
        NewNode.SelectedIndex := imgGear;
      end

      else if Constant is TConstant_String then
      begin
        NewNode.ImageIndex := imgString;
        NewNode.SelectedIndex := imgString;
      end

      else if Constant is TConstant_UTF8 then
      begin
        NewNode.ImageIndex := imgUTF8;
        NewNode.SelectedIndex := imgUTF8;
      end

      else if Constant is TConstant_Integer then
      begin
        NewNode.ImageIndex := imgInt;
        NewNode.SelectedIndex := imgInt;
      end

      else if Constant is TConstant_Float then
      begin
        NewNode.ImageIndex := imgFloat;
        NewNode.SelectedIndex := imgFloat;
      end

      else if Constant is TConstant_Long then
      begin
        NewNode.ImageIndex := imgLong;
        NewNode.SelectedIndex := imgLong;
      end

      else if Constant is TConstant_Double then
      begin
        NewNode.ImageIndex := imgDouble;
        NewNode.SelectedIndex := imgDouble;
      end

      else if Constant is TConstant_Class then
      begin
        NewNode.ImageIndex := imgClass;
        NewNode.SelectedIndex := imgClass;
      end

      else if Constant is TConstant_MethodRef then
      begin
        NewNode.ImageIndex := imgMethod;
        NewNode.SelectedIndex := imgMethod;
      end

      else if Constant is TConstant_IMethodRef then
      begin
        NewNode.ImageIndex := imgIMethod;
        NewNode.SelectedIndex := imgIMethod;
      end

      else if Constant is TConstant_FieldRef then
      begin
        NewNode.ImageIndex := imgField;
        NewNode.SelectedIndex := imgField;
      end

      else if Constant is TConstant_NameAndType then
      begin
        NewNode.ImageIndex := imgNAT;
        NewNode.SelectedIndex := imgNAT;
      end

      else
      begin
        NewNode.ImageIndex := -1;
        NewNode.SelectedIndex := -1;
      end;
    end;
  end;

  procedure ExpandInterfaces;
  var NewNode: TTreeNode; i: integer;
  begin
    for i := 0 to theClass.Interfaces.Count - 1 do begin
      NewNode := FTreeView.Items.AddChild(Node, 'INTERFACE');
      NewNode.Data := tagInterface;
    end;
  end;

  procedure ExpandFields;
  var NewNode: TTreeNode; Field: TField; i: integer;
  begin
    for i := 0 to theClass.Fields.Count - 1 do begin
      Field := theClass.Fields[i];
      NewNode := FTreeView.Items.AddChild(Node, '[' + inttostr(i) + '] ' +
        theClass.ResolveAtom(Field.NameIndex));
      NewNode.Data := tagField;

      case theClass.ResolveAtom(Field.DescriptorIndex)[1] of
        'B':
          begin
            NewNode.ImageIndex := imgByte;
            NewNode.SelectedIndex := imgByte;
          end;
        'C':
          begin
            NewNode.ImageIndex := imgChar;
            NewNode.SelectedIndex := imgChar;
          end;
        'D':
          begin
            NewNode.ImageIndex := imgDouble;
            NewNode.SelectedIndex := imgDouble;
          end;
        'F':
          begin
            NewNode.ImageIndex := imgFloat;
            NewNode.SelectedIndex := imgFloat;
          end;
        'I':
          begin
            NewNode.ImageIndex := imgInt;
            NewNode.SelectedIndex := imgInt;
          end;
        'J':
          begin
            NewNode.ImageIndex := imgLong;
            NewNode.SelectedIndex := imgLong;
          end;
        'L':
          begin
            NewNode.ImageIndex := imgClass;
            NewNode.SelectedIndex := imgClass;
          end;
        'S':
          begin
            NewNode.ImageIndex := imgShort;
            NewNode.SelectedIndex := imgShort;
          end;
        'Z':
          begin
            NewNode.ImageIndex := imgBoolean;
            NewNode.SelectedIndex := imgBoolean;
          end;

        '[':
          begin
            NewNode.ImageIndex := imgArray;
            NewNode.SelectedIndex := imgArray;
          end;
        else begin
          NewNode.ImageIndex := -1;
          NewNode.SelectedIndex := -1;
        end;
      end;
    end;
  end;

  procedure ExpandMethods;
  var NewNode: TTreeNode; i: integer;
  begin
    with theClass do
      for i := 0 to Methods.Count - 1 do begin
        NewNode := FTreeView.Items.AddChild(Node, ResolveAtom(Methods[i].NameIndex));
        NewNode.HasChildren := True;
        NewNode.ImageIndex := imgMethod;
        NewNode.SelectedIndex := imgMethod;
        NewNode.Data := tagMethod;
      end;
  end;

  procedure ExpandAttributes;
  var NewNode: TTreeNode; i: integer;
  begin
    with theClass do
      for i := 0 to Attributes.Count - 1 do begin
        NewNode := FTreeView.Items.AddChild(Node, ResolveAtom(Attributes[i].NameIndex));
        NewNode.ImageIndex := imgGear;
        NewNode.SelectedIndex := imgGear;
        NewNode.Data := tagAttribute;
      end;
  end;

  procedure ExpandMethod;
  var NewNode: TTreeNode;
  begin
    {NewNode := FTreeView.Items.AddChild( Node, 'Code' );
    NewNode.ImageIndex := imgGear;
    NewNode.SelectedIndex := imgGear;
    NewNode.Data := tagCode;}

    NewNode := FTreeView.Items.AddChild(Node, 'Access Flags');
    NewNode.ImageIndex := imgGear;
    NewNode.SelectedIndex := imgGear;
    NewNode.HasChildren := False;
    NewNode.Data := tagMethodAccessFlags;

    NewNode := FTreeView.Items.AddChild(Node, 'Attributes');
    NewNode.ImageIndex := imgFolder;
    NewNode.SelectedIndex := imgFolder;
    NewNode.HasChildren := True;
    NewNode.Data := tagMethodAttribFolder;
  end;

  procedure ExpandMethodAttributes;
  var NewNode: TTreeNode; i: integer; Attributes: TAttributeList;
  begin
    Attributes := theClass.Methods[Node.Index].Attributes;
    for i := 0 to Attributes.Count - 1 do begin
      //NewNode := FTreeView.Items.AddChild( Node, 'Attributes' );
      NewNode := FTreeView.Items.AddChild(Node, theClass.ResolveAtom(Attributes[i].NameIndex));
      NewNode.ImageIndex := imgGear;
      NewNode.SelectedIndex := imgGear;
      NewNode.Data := tagMethodAttribute;
    end;
  end;

begin
  if Node.Data = tagConstantPoolFolder then
    ExpandConstantPool
  else if Node.Data = tagInterfacesFolder then
    ExpandInterfaces
  else if Node.Data = tagFieldsFolder then
    ExpandFields
  else if Node.Data = tagMethodsFolder then
    ExpandMethods
  else if Node.Data = tagAttributesFolder then
    ExpandAttributes
  else if Node.Data = tagMethodAttribFolder then
    ExpandMethodAttributes
  else if Node.Data = tagMethod then
    ExpandMethod
  else
    // ?
end;

end.
