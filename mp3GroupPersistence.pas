(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3GroupPersistence;

interface
{$IFDEF FPC}
  //
{$ELSE}
  {$DEFINE MSXML}
{$ENDIF}
uses
  SysUtils, Classes,
  {$IFDEF FPC}
  DOM, xmlread, xmlwrite,
  {$ELSE}
  {$IFDEF MSXML}
  Forms, XMLDoc, XMLIntf, ActiveX,
  {$ENDIF}
  {$ENDIF}
  mp3Group, mp3Consts;

function ReadGroup(AGroup: Tmp3Group): boolean;
function WriteGroup(AGroup: Tmp3Group): boolean;

implementation

{$IFDEF FPC}
function ReadGroup(AGroup: Tmp3Group): boolean;
var i,h: integer; xml: TXMLDocument;
  rootChilds: TDOMNodeList; projects: TDOMNode;
begin
  result := true;
  xml := TXMLDocument.Create;
  try
    try
    ReadXMLFile(xml, AGroup.Filename);
    except
      result := false;
      exit;
    end;
    if not SameText(xml.DocumentElement.NodeName,TAG_GROUP) then
    begin
      result := false;
      exit;
    end;
    rootChilds := xml.DocumentElement.ChildNodes;
    for h := 0 to rootChilds.Count - 1 do
      if rootChilds[h].NodeName = TAG_PROJECTS then begin
        projects := xml.DocumentElement.ChildNodes[h];
        for i := 0 to projects.ChildNodes.Count - 1 do
          if projects.ChildNodes[i].NodeName = TAG_PROJECTS_PROJECT then
            AGroup.Projects.Add(
              projects.ChildNodes[i].Attributes.GetNamedItem(TAG_PROJECTS_PROJECT_FILENAME).NodeValue
            );
      end;
  finally
    if assigned(xml) then
      xml.Free;
  end;
end;

function WriteGroup(AGroup: Tmp3Group): boolean;
var i:integer; st: TStringList;
  xml: TXMLDocument; root, node, n: TDOMNode;
begin
  xml := TXMLDocument.Create;
  st := TStringList.Create;
  try
    // Group
    root := xml.CreateElement(TAG_GROUP);
    xml.AppendChild(root);
    root := xml.DocumentElement;
    // Group.projects
    node := xml.CreateElement(TAG_PROJECTS);
    for i := 0 to AGroup.Projects.Count - 1 do begin
      n := xml.CreateElement(TAG_PROJECTS_PROJECT);
      TDOMElement(n).SetAttribute(TAG_PROJECTS_PROJECT_FILENAME, AGroup.Projects[i]);
      node.AppendChild(n);
    end;
    root.AppendChild(node);
    // write xml file
    WriteXMLFile(xml, AGroup.Filename);
    // adjust xml encoding
    st.LoadFromFile(AGroup.Filename);
    //st.Insert(0, XML_ENCODING_TAG);
    st.Strings[0] := XML_ENCODING_TAG;
    st.SaveToFile(AGroup.Filename);
  finally
    result := true;
    st.Free;
    xml.Free;
  end;
end;
{$ELSE}
{$IFDEF MSXML}
function ReadGroup(AGroup: Tmp3Group): boolean;
var h, i: integer; xml: TXMLDocument;
  rootChilds: IXMLNodeList; projects: IXMLNode;
begin
  result := true;
  xml := TXMLDocument.Create(Application);
  try
    try
      xml.LoadFromFile(AGroup.Filename);
    except
      result := false;
      exit;
    end;
    if not SameText(xml.DocumentElement.NodeName,TAG_GROUP) then
    begin
      result := false;
      exit;
    end;
    rootChilds := xml.DocumentElement.ChildNodes;
    for h := 0 to rootChilds.Count - 1 do
      if rootChilds[h].NodeName = TAG_PROJECTS then begin
        projects := xml.DocumentElement.ChildNodes[h];
        for i := 0 to projects.ChildNodes.Count - 1 do
          if SameText(projects.ChildNodes[i].NodeName,TAG_PROJECTS_PROJECT) then
            AGroup.Projects.Include(
              projects.ChildNodes[i].Attributes[TAG_PROJECTS_PROJECT_FILENAME]
            );
      end;
  finally
    if assigned(xml) then
      xml.Free;
  end;
end;

function WriteGroup(AGroup: Tmp3Group): boolean;
var i:integer; st: TStringList;
  xml: TXMLDocument; root, node, n: IXMLNode;
begin
  xml := TXMLDocument.Create(Application);
  st := TStringList.Create;
  try
    xml.Active := true;
    root := xml.AddChild(TAG_GROUP);
    node := root.AddChild(TAG_PROJECTS);
    for i := 0 to AGroup.Projects.Count - 1 do begin
      n := node.AddChild(TAG_PROJECTS_PROJECT);
      n.Attributes[TAG_PROJECTS_PROJECT_FILENAME] := AGroup.Projects[i];
    end;
    st.Text := FormatXMLData(xml.XML.Text);
    st.Insert(0, XML_ENCODING_TAG);
    st.SaveToFile(AGroup.Filename);
  finally
    result := true;
    st.Free;
    xml.Free;
  end;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
{$IFDEF MSXML}
initialization
  CoInitialize(nil);
finalization
  CoUninitialize;
{$ENDIF}
{$ENDIF}
end.

