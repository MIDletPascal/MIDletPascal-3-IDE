(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3ProjectPersistence;

interface
{$IFDEF FPC}
  //
{$ELSE}
  {$DEFINE MSXML}
{$ENDIF}
uses
  SysUtils, Classes, Variants,
  {$IFDEF FPC}
  DOM, xmlread, xmlwrite,
  {$ELSE}
  {$IFDEF MSXML}
  Forms, XMLDoc, XMLIntf, ActiveX,
  {$ENDIF}
  {$ENDIF}
  mp3Project, mp3Consts;

function ReadProject(AProject: Tmp3Project): boolean;
function WriteProject(AProject: Tmp3Project): boolean;

function ImportMP1Project(AProject: Tmp3Project): boolean;

implementation

function ImportMP1Project(AProject: Tmp3Project): boolean;
var x: integer;
begin
  result := false;
  with TStringList.Create do
  try
    LoadFromFile(AProject.Filename);
    if Count > 0 then
      if Strings[0] = MP1_PROJECT_TAG then
      begin
        AProject.MIDletInfo.Name := Strings[1];
        AProject.MIDletInfo.Vendor := Strings[2];
        AProject.MIDletInfo.Version := Strings[3];
        AProject.MIDletInfo.Icon := Strings[4];
        AProject.SourceFiles.Add(Strings[5]);
        for x := 6 to Count - 1 do
          AProject.ResourceFiles.Add(Strings[x],':all:');
        AProject.BuildConfigurations.Add(DEFAULT_CONFIGURATION_NAME);
        result := true;
      end;
  finally
    Free;
  end;
end;

{$IFDEF FPC}
function ReadProject(AProject: Tmp3Project): boolean;
var i,h: integer; s: string;
  xml: TXMLDocument; rootChilds: TDOMNodeList;
  sources, resources, buildconfigs, rsc, tmp: TDOMNode;
begin
  result := true;
  xml := TXMLDocument.Create;
  try
    try
      ReadXMLFile(xml, AProject.Filename);
    except
      result := false;
      exit;
    end;
    if not SameText(xml.DocumentElement.NodeName,TAG_PROJECT) then
    begin
      result := false;
      exit;
    end;
    rootChilds := xml.DocumentElement.ChildNodes;
    for h := 0 to rootChilds.Count - 1 do
      if rootChilds[h].NodeName = TAG_MIDLET then begin
        with rootChilds[h] do begin
          AProject.MidletInfo.Name := Attributes.GetNamedItem(TAG_MIDLET_NAME).NodeValue;
          AProject.MidletInfo.Vendor := Attributes.GetNamedItem(TAG_MIDLET_VENDOR).NodeValue;
          AProject.MidletInfo.Version := Attributes.GetNamedItem(TAG_MIDLET_VERSION).NodeValue;
          AProject.MidletInfo.Icon := Attributes.GetNamedItem(TAG_MIDLET_ICON).NodeValue;
        end;
      end else if rootChilds[h].NodeName = TAG_SOURCES then begin
        sources := xml.DocumentElement.ChildNodes[h];
        for i := 0 to sources.ChildNodes.Count - 1 do
          if sources.ChildNodes[i].NodeName = TAG_SOURCES_SOURCE then
            AProject.SourceFiles.Add(sources.ChildNodes[i].Attributes.GetNamedItem(TAG_SOURCES_FILENAME).NodeValue);
      end else if rootChilds[h].NodeName = TAG_RESOURCES then begin
        resources := xml.DocumentElement.ChildNodes[h];
        for i := 0 to resources.ChildNodes.Count - 1 do
          if resources.ChildNodes[i].NodeName = TAG_RESOURCES_RESOURCE then begin
            rsc := resources.ChildNodes[i];
            tmp := rsc.Attributes.GetNamedItem(TAG_RESOURCES_CONFIGURATION);
            if not assigned(tmp) then
              tmp := rsc.Attributes.GetNamedItem(TAG_RESOURCES_CONFIGURATIONS);
            if not assigned(tmp) then
              s := ''
            else
              s := tmp.NodeValue;
            AProject.ResourceFiles.Add(rsc.Attributes.GetNamedItem(TAG_RESOURCES_FILENAME).NodeValue,s);
          end;
      end else if rootChilds[h].NodeName = TAG_BUILDCONFIGS then begin
        buildconfigs := xml.DocumentElement.ChildNodes[h];
        for i := 0 to buildconfigs.ChildNodes.Count - 1 do
          if buildconfigs.ChildNodes[i].NodeName = TAG_BUILDCONFIGS_CONFIGURATION then begin
            with buildconfigs.ChildNodes[i] do
              AProject.BuildConfigurations.Add(
                Attributes.GetNamedItem(TAG_BUILDCONFIGS_CONFIGURATION_NAME).NodeValue,
                string(Attributes.GetNamedItem(TAG_BUILDCONFIGS_CONFIGURATION_TYPE).NodeValue),
                string(Attributes.GetNamedItem(TAG_BUILDCONFIGS_CONFIGURATION_VERSION).NodeValue),
                string(Attributes.GetNamedItem(TAG_BUILDCONFIGS_CONFIGURATION_MATH).NodeValue));
          end else if buildconfigs.ChildNodes[i].NodeName = TAG_BUILDCONFIGS_ACTIVECONFIGURATION then
            AProject.BuildConfigurations.ActiveConfigurationIndex := StrToIntDef(
              buildconfigs.ChildNodes[i].Attributes.GetNamedItem(TAG_BUILDCONFIGS_ACTIVECONFIGURATION_INDEX).NodeValue,0
            );
      end;
  finally
    if assigned(xml) then
      xml.Free;
  end;
end;

function WriteProject(AProject: Tmp3Project): boolean;
var i:integer; xml: TXMLDocument; root, node, n: TDOMNode; st: TStringList;
begin
  xml := TXMLDocument.Create;
  st := TStringList.Create;
  try
    // project
    root := xml.CreateElement(TAG_PROJECT);
    xml.AppendChild(root);
    root := xml.DocumentElement;
    // project.midlet
    node := xml.CreateElement(TAG_MIDLET);
    TDOMElement(node).SetAttribute(TAG_MIDLET_NAME, AProject.MidletInfo.Name);
    TDOMElement(node).SetAttribute(TAG_MIDLET_VENDOR, AProject.MidletInfo.Vendor);
    TDOMElement(node).SetAttribute(TAG_MIDLET_VERSION, AProject.MidletInfo.Version);
    TDOMElement(node).SetAttribute(TAG_MIDLET_ICON, AProject.MidletInfo.Icon);
    root.AppendChild(node);
    // project.sources
    node := xml.CreateElement(TAG_SOURCES);
    for i := 0 to AProject.SourceFiles.Count - 1 do begin
      n := xml.CreateElement(TAG_SOURCES_SOURCE);
      TDOMElement(n).SetAttribute(TAG_SOURCES_FILENAME, AProject.SourceFiles[i].Filename);
      node.AppendChild(n);
    end;
    root.AppendChild(node);
    // project.resources
    node := xml.CreateElement(TAG_RESOURCES);
    for i := 0 to AProject.ResourceFiles.Count - 1 do begin
      n := xml.CreateElement(TAG_RESOURCES_RESOURCE);
      TDOMElement(n).SetAttribute(TAG_RESOURCES_FILENAME, AProject.ResourceFiles[i].Filename);
      TDOMElement(n).SetAttribute(TAG_RESOURCES_CONFIGURATIONS, AProject.ResourceFiles[i].Configurations);
      node.AppendChild(n);
    end;
    root.AppendChild(node);
    // project.buildconfigurations
    node := xml.CreateElement(TAG_BUILDCONFIGS);
    for i := 0 to AProject.BuildConfigurations.Count - 1 do begin
      n := xml.CreateElement(TAG_BUILDCONFIGS_CONFIGURATION);
      TDOMElement(n).SetAttribute(TAG_BUILDCONFIGS_CONFIGURATION_NAME,
        AProject.BuildConfigurations[i].Name);
      TDOMElement(n).SetAttribute(TAG_BUILDCONFIGS_CONFIGURATION_TYPE,
        AProject.BuildConfigurations[i].GetMidletTypeAsMP2ProjectValue);
      TDOMElement(n).SetAttribute(TAG_BUILDCONFIGS_CONFIGURATION_VERSION,
        AProject.BuildConfigurations[i].GetMIDPVersionAsMP2ProjectValue);
      TDOMElement(n).SetAttribute(TAG_BUILDCONFIGS_CONFIGURATION_MATH,
        AProject.BuildConfigurations[i].GetRealNumbersAsMP2ProjectValue);
      node.AppendChild(n);
    end;
    n := xml.CreateElement(TAG_BUILDCONFIGS_ACTIVECONFIGURATION);
    TDOMElement(n).SetAttribute(TAG_BUILDCONFIGS_ACTIVECONFIGURATION_INDEX,
      IntToStr(AProject.BuildConfigurations.ActiveConfigurationIndex));
    node.AppendChild(n);
    root.AppendChild(node);
    // write xml file
    WriteXMLFile(xml, AProject.Filename);
    // adjust xml encoding
    st.LoadFromFile(AProject.Filename);
    st.Delete(0);
    //st.Strings[0] := XML_ENCODING_TAG;
    for i := 0 to st.Count - 1 do
      st[i] := StringReplace(StringReplace(st[i],'    <',#9#9+'<',[]),'  <',#9+'<',[]);
    st.SaveToFile(AProject.Filename);
  finally
    result := true;
    st.Free;
    xml.Free;
  end;
end;
{$ELSE}
{$IFDEF MSXML}
function ReadProject(AProject: Tmp3Project): boolean;
var h, i: integer; cfg: OleVariant; s: string;
  xml: TXMLDocument; rootChilds: IXMLNodeList;
  sources, resources, buildconfigs, tmp: IXMLNode;
begin
  result := true;
  xml := TXMLDocument.Create(Application);
  try
    try
      xml.LoadFromFile(AProject.Filename);
    except
      result := false;
      exit;
    end;
    if not SameText(xml.DocumentElement.NodeName,TAG_PROJECT) then
    begin
      result := false;
      exit;
    end;
    rootChilds := xml.DocumentElement.ChildNodes;
    for h := 0 to rootChilds.Count - 1 do
      if rootChilds[h].NodeName = TAG_MIDLET then begin
        with rootChilds[h] do begin
          AProject.MidletInfo.Name := Attributes[TAG_MIDLET_NAME];
          AProject.MidletInfo.Vendor := Attributes[TAG_MIDLET_VENDOR];
          AProject.MidletInfo.Version := Attributes[TAG_MIDLET_VERSION];
          AProject.MidletInfo.Icon := Attributes[TAG_MIDLET_ICON];
        end;
      end else if rootChilds[h].NodeName = TAG_SOURCES then begin
        sources := xml.DocumentElement.ChildNodes[h];
        for i := 0 to sources.ChildNodes.Count - 1 do
          if SameText(sources.ChildNodes[i].NodeName,TAG_SOURCES_SOURCE) then
            AProject.SourceFiles.Add(sources.ChildNodes[i].Attributes[TAG_SOURCES_FILENAME]);
      end else if rootChilds[h].NodeName = TAG_RESOURCES then begin
        resources := xml.DocumentElement.ChildNodes[h];
        for i := 0 to resources.ChildNodes.Count - 1 do
          if SameText(resources.ChildNodes[i].NodeName,TAG_RESOURCES_RESOURCE) then
          begin
            tmp := resources.ChildNodes[i];
            cfg := tmp.Attributes[TAG_RESOURCES_CONFIGURATION];
            if VarType(cfg) = varNull then
              cfg := tmp.Attributes[TAG_RESOURCES_CONFIGURATIONS];
            if VarType(cfg) = varNull then
              s := ''
            else
              s := cfg;
            AProject.ResourceFiles.Add(tmp.Attributes[TAG_RESOURCES_FILENAME], s);
          end;
      end else if rootChilds[h].NodeName = TAG_BUILDCONFIGS then begin
        buildconfigs := xml.DocumentElement.ChildNodes[h];
        for i := 0 to buildconfigs.ChildNodes.Count - 1 do
          if SameText(buildconfigs.ChildNodes[i].NodeName,TAG_BUILDCONFIGS_CONFIGURATION) then begin
            with buildconfigs.ChildNodes[i] do
              AProject.BuildConfigurations.Add(
                Attributes[TAG_BUILDCONFIGS_CONFIGURATION_NAME],
                string(Attributes[TAG_BUILDCONFIGS_CONFIGURATION_TYPE]),
                string(Attributes[TAG_BUILDCONFIGS_CONFIGURATION_VERSION]),
                string(Attributes[TAG_BUILDCONFIGS_CONFIGURATION_MATH]));
          end else if SameText(buildconfigs.ChildNodes[i].NodeName,TAG_BUILDCONFIGS_ACTIVECONFIGURATION) then
            AProject.BuildConfigurations.ActiveConfigurationIndex := StrToIntDef(
              buildconfigs.ChildNodes[i].Attributes[TAG_BUILDCONFIGS_ACTIVECONFIGURATION_INDEX],0
            );
      end;
  finally
    if assigned(xml) then
      xml.Free;
  end;
end;

function WriteProject(AProject: Tmp3Project): boolean;
var i:integer; xml: TXMLDocument; root, node, n: IXMLNode; //st: TStringList;
begin
  xml := TXMLDocument.Create(Application);
  //st := TStringList.Create;
  try
    xml.Active := true;
    xml.NodeIndentStr := #9;
    xml.Options := xml.Options + [doNodeAutoIndent];
    root := xml.AddChild(TAG_PROJECT);
    node := root.AddChild(TAG_MIDLET);
    node.Attributes[TAG_MIDLET_NAME] := AProject.MidletInfo.Name;
    node.Attributes[TAG_MIDLET_VENDOR] := AProject.MidletInfo.Vendor;
    node.Attributes[TAG_MIDLET_VERSION] := AProject.MidletInfo.Version;
    node.Attributes[TAG_MIDLET_ICON] := AProject.MidletInfo.Icon;
    node := root.AddChild(TAG_SOURCES);
    for i := 0 to AProject.SourceFiles.Count - 1 do begin
      n := node.AddChild(TAG_SOURCES_SOURCE);
      n.Attributes[TAG_SOURCES_FILENAME] := AProject.SourceFiles[i].Filename;
    end;
    node := root.AddChild(TAG_RESOURCES);
    for i := 0 to AProject.ResourceFiles.Count - 1 do begin
      n := node.AddChild(TAG_RESOURCES_RESOURCE);
      n.Attributes[TAG_RESOURCES_FILENAME] := AProject.ResourceFiles[i].Filename;
      n.Attributes[TAG_RESOURCES_CONFIGURATIONS] := AProject.ResourceFiles[i].Configurations;
    end;
    node := root.AddChild(TAG_BUILDCONFIGS);
    for i := 0 to AProject.BuildConfigurations.Count - 1 do begin
      n := node.AddChild(TAG_BUILDCONFIGS_CONFIGURATION);
      n.Attributes[TAG_BUILDCONFIGS_CONFIGURATION_NAME] :=
        AProject.BuildConfigurations[i].Name;
      n.Attributes[TAG_BUILDCONFIGS_CONFIGURATION_TYPE] :=
        AProject.BuildConfigurations[i].GetMidletTypeAsMP2ProjectValue;
      n.Attributes[TAG_BUILDCONFIGS_CONFIGURATION_VERSION] :=
        AProject.BuildConfigurations[i].GetMIDPVersionAsMP2ProjectValue;
      n.Attributes[TAG_BUILDCONFIGS_CONFIGURATION_MATH] :=
        AProject.BuildConfigurations[i].GetRealNumbersAsMP2ProjectValue;
    end;
    n := node.AddChild(TAG_BUILDCONFIGS_ACTIVECONFIGURATION);
    n.Attributes[TAG_BUILDCONFIGS_ACTIVECONFIGURATION_INDEX] :=
      AProject.BuildConfigurations.ActiveConfigurationIndex;
    //st.Text := FormatXMLData(xml.XML.Text);
    //st.Insert(0, XML_ENCODING_TAG);
    //st.SaveToFile(AProject.Filename);
    xml.XML.SaveToFile(AProject.Filename);
  finally
    result := true;
    //st.Free;
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

