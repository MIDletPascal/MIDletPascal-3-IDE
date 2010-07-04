(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3BuildConfigurations;

interface

uses
  Windows, SysUtils, Contnrs, Classes,
  mp3Consts;

type
  Tmp3MIDletType = (mtClassicMidlet, mtMIDP20Fullscreen, mtNokiaAPIFullscreen);
  Tmp3MIDPVersion = (mvMIDP10, mvMIDP20);
  Tmp3RealNumbers = (rnFixedPoint, rnFloatingPoint);

  Tmp3BuildConfiguration = class
  private
    FName: string;
    // "Type" field:
    //   "normal" value means Classic Midlet
    //   "fullscreen" value means MIDP2.0 Fullscreen
    //   "nokia" value means NokiaAPI Fullscreen
    FMIDletType: Tmp3MIDletType;
    // "Version" field:
    //   "1" value means MIDP1.0
    //   "2" value means MIDP2.0
    FMIDPVersion: Tmp3MIDPVersion;
    // "Math" field:
    //   "1" value means Fixed-point
    //   "2" value means Floating-point
    FRealNumbers: Tmp3RealNumbers;
  public
    function GetMIDletTypeAsString: string;
    function GetMIDPVersionAsString: string;
    function GetRealNumbersAsString: string;
    function GetMIDletTypeAsMP2ProjectValue: string;
    function GetMIDPVersionAsMP2ProjectValue: string;
    function GetRealNumbersAsMP2ProjectValue: string;
    function SetMIDletTypeAsMP2ProjectValue(AValue: string): boolean;
    function SetMIDPVersionAsMP2ProjectValue(AValue: string): boolean;
    function SetRealNumbersAsMP2ProjectValue(AValue: string): boolean;
    property Name: string read FName write FName;
    property MIDletType: Tmp3MIDletType read FMIDletType write FMIDletType;
    property MIDPVersion: Tmp3MIDPVersion read FMIDPVersion write FMIDPVersion;
    property RealNumbers: Tmp3RealNumbers read FRealNumbers write FRealNumbers;
  end;

  Tmp3BuildConfigurations = class
  private
    FList: TList;
    FActiveConfigurationIndex: integer;
    function GetBuildConfiguration(AIndex: integer): Tmp3BuildConfiguration;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AName, AType, AVersion, AMath: string): boolean; overload;
    function Add(AName: string; AType: Tmp3MIDletType = mtClassicMidlet;
      AVersion: Tmp3MIDPVersion = mvMIDP10;
      AMath: Tmp3RealNumbers = rnFixedPoint): boolean; overload;
    function Rename(AOldName, ANewName: string): boolean;
    function Delete(AName: string): boolean;
    function GetBuildConfigurationNamesAsCSV: string;
    function GetBuildConfigurationByName(AName: string): Tmp3BuildConfiguration;
    function GetBuildConfigurationIndex(AName: string): integer;
    function IsValidBuildConfigurationName(AName: string): boolean;
    property BuildConfiguration[AIndex: integer]: Tmp3BuildConfiguration
      read GetBuildConfiguration; default;
    property Count: integer read GetCount;
    property ActiveConfigurationIndex: integer
      read FActiveConfigurationIndex write FActiveConfigurationIndex;
  end;

function MIDletTypeFromMP2ProjectValue(AType: string): Tmp3MIDletType;
function MIDPVersionFromMP2ProjectValue(AVersion: string): Tmp3MIDPVersion;
function RealNumbersFromMP2ProjectValue(AMath: string): Tmp3RealNumbers;

function MIDletTypeToMP2ProjectValue(AType: Tmp3MIDletType): string;
function MIDPVersionToMP2ProjectValue(AVersion: Tmp3MIDPVersion): string;
function RealNumbersToMP2ProjectValue(AMath: Tmp3RealNumbers): string;

function MIDletTypeToString(AType: Tmp3MIDletType): string;
function MIDPVersionToString(AVersion: Tmp3MIDPVersion): string;
function RealNumbersToString(AMath: Tmp3RealNumbers): string;

implementation

function MIDletTypeFromMP2ProjectValue(AType: string): Tmp3MIDletType;
begin
  if AType = 'nokia' then
    result := mtNokiaAPIFullscreen
  else if AType = 'fullscreen' then
    result := mtMIDP20Fullscreen
  else // AType = 'normal'
    result := mtClassicMidlet;
end;

function MIDletTypeToMP2ProjectValue(AType: Tmp3MIDletType): string;
begin
  case AType of
    mtClassicMidlet: result := 'normal';
    mtMIDP20Fullscreen: result := 'fullscreen';
    mtNokiaAPIFullscreen: result := 'nokia';
  end;
end;

function MIDPVersionFromMP2ProjectValue(AVersion: string): Tmp3MIDPVersion;
begin
  if AVersion = '2' then
    result := mvMIDP20
  else
    result := mvMIDP10;
end;

function MIDPVersionToMP2ProjectValue(AVersion: Tmp3MIDPVersion): string;
begin
  case AVersion of
    mvMIDP10: result := '1';
    mvMIDP20: result := '2';
  end;
end;

function RealNumbersFromMP2ProjectValue(AMath: string): Tmp3RealNumbers;
begin
  if AMath = '2' then
    result := rnFloatingPoint
  else
    result := rnFixedPoint;
end;

function RealNumbersToMP2ProjectValue(AMath: Tmp3RealNumbers): string;
begin
  case AMath of
    rnFixedPoint: result := '1';
    rnFloatingPoint: result := '2';
  end;
end;

function MIDletTypeToString(AType: Tmp3MIDletType): string;
begin
  case AType of
    mtClassicMidlet: result := 'Classic MIDlet';
    mtMIDP20Fullscreen: result := 'MIDP2.0 Fullscreen';
    mtNokiaAPIFullscreen: result := 'NokiaAPI Fullscreen';
  end;
end;

function MIDPVersionToString(AVersion: Tmp3MIDPVersion): string;
begin
  case AVersion of
    mvMIDP10: result := 'MIDP-1.0';
    mvMIDP20: result := 'MIDP-2.0';
  end;
end;

function RealNumbersToString(AMath: Tmp3RealNumbers): string;
begin
  case AMath of
    rnFixedPoint: result := 'Fixed-point';
    rnFloatingPoint: result := 'Floating-point';
  end;
end;

{ Tmp3BuildConfigurations }

procedure Tmp3BuildConfigurations.Clear;
begin
  FActiveConfigurationIndex := 0;
  FList.Clear;
end;

constructor Tmp3BuildConfigurations.Create;
begin
  FList := TObjectList.Create;
  Clear;
end;

destructor Tmp3BuildConfigurations.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function Tmp3BuildConfigurations.GetCount: integer;
begin
  result := FList.Count;
end;

function Tmp3BuildConfigurations.GetBuildConfiguration(
  AIndex: integer): Tmp3BuildConfiguration;
begin
  result := Tmp3BuildConfiguration(FList[AIndex]);
end;

function Tmp3BuildConfigurations.GetBuildConfigurationNamesAsCSV: string;
var i: integer;
begin
  if FList.Count=0 then
    exit;
  result := GetBuildConfiguration(0).Name;
  if FList.Count>0 then
    for i := 1 to FList.Count - 1 do
      result := result+','+GetBuildConfiguration(i).Name;
end;

function Tmp3BuildConfigurations.GetBuildConfigurationByName(
  AName: string): Tmp3BuildConfiguration;
var i: integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
    if Tmp3BuildConfiguration(FList[i]).FName = AName then begin
      result := Tmp3BuildConfiguration(FList[i]);
      break;
    end;
end;

function Tmp3BuildConfigurations.GetBuildConfigurationIndex(AName: string): integer;
var i: integer;
begin
  result := -1;
  for i := 0 to FList.Count-1 do
    if Tmp3BuildConfiguration(FList[i]).FName = AName then begin
      result := i;
      break;
    end;
end;

function Tmp3BuildConfigurations.Add(AName: string; AType: Tmp3MIDletType;
  AVersion: Tmp3MIDPVersion; AMath: Tmp3RealNumbers): boolean;
var buildconfig: Tmp3BuildConfiguration;
begin
  buildconfig := Tmp3BuildConfiguration.Create;
  buildconfig.FName := AName;
  buildconfig.FMIDletType := AType;
  buildconfig.FMIDPVersion := AVersion;
  buildconfig.FRealNumbers := AMath;
  FList.Add(buildconfig);
  result := True;
end;

function Tmp3BuildConfigurations.Add(AName, AType, AVersion, AMath: string): boolean;
begin
  result := Add(AName,
    MIDletTypeFromMP2ProjectValue(AType),
    MIDPVersionFromMP2ProjectValue(AVersion),
    RealNumbersFromMP2ProjectValue(AMath)
  );
end;

// j-a-s-d: keeps the MP2 logic
// does not delete if there is only one bc
// and update index to index-1 if active bc is deleted
function Tmp3BuildConfigurations.Delete(AName: string): boolean;
var buildconfig: Tmp3BuildConfiguration; aux: integer;
begin
  result := Count > 1;
  if result then begin
    aux := GetBuildConfigurationIndex(ParamStr(3));
    if (aux > 0) and (aux = ActiveConfigurationIndex) then
      ActiveConfigurationIndex := aux - 1;
    buildconfig := GetBuildConfigurationByName(AName);
    result := assigned(buildconfig);
    if result then
      FList.Remove(buildconfig);
  end;
end;

function Tmp3BuildConfigurations.Rename(AOldName, ANewName: string): boolean;
var buildconfig: Tmp3BuildConfiguration;
begin
  buildconfig := GetBuildConfigurationByName(AOldName);
  result := assigned(buildconfig);
  if result then
    buildconfig.FName := ANewName;
end;

function Tmp3BuildConfigurations.IsValidBuildConfigurationName(AName: string): boolean;
var i: integer;
begin
  result := AName <> '';
  if result then
    if (AName <> CONFIGS_NONE) and (AName <> CONFIGS_ALL) then
      for i := 1 to Length(AName) do
        if not (AName[i] in ['A'..'Z','a'..'z','_','0'..'9']) then
        begin
          result := false;
          break;
        end;
end;

{ Tmp3BuildConfiguration }

function Tmp3BuildConfiguration.GetMIDletTypeAsString: string;
begin
  result := MIDletTypeToString(FMidletType);
end;

function Tmp3BuildConfiguration.GetMIDPVersionAsString: string;
begin
  result := MIDPVersionToString(FMIDPVersion);
end;

function Tmp3BuildConfiguration.GetRealNumbersAsString: string;
begin
  result := RealNumbersToString(FRealNumbers);
end;

function Tmp3BuildConfiguration.GetMIDletTypeAsMP2ProjectValue: string;
begin
  result := MIDletTypeToMP2ProjectValue(FMidletType);
end;

function Tmp3BuildConfiguration.GetMIDPVersionAsMP2ProjectValue: string;
begin
  result := MIDPVersionToMP2ProjectValue(FMIDPVersion);
end;

function Tmp3BuildConfiguration.GetRealNumbersAsMP2ProjectValue: string;
begin
  result := RealNumbersToMP2ProjectValue(FRealNumbers);
end;

function Tmp3BuildConfiguration.SetMIDletTypeAsMP2ProjectValue(AValue: string): boolean;
begin
  result := (AValue = 'normal') or (AValue = 'fullscreen') or (AValue = 'nokia');
  FMidletType := MIDletTypeFromMP2ProjectValue(AValue);
end;

function Tmp3BuildConfiguration.SetMIDPVersionAsMP2ProjectValue(
  AValue: string): boolean;
begin
  result := (AValue = '1') or (AValue = '2');
  FMIDPVersion := MIDPVersionFromMP2ProjectValue(AValue);
end;

function Tmp3BuildConfiguration.SetRealNumbersAsMP2ProjectValue(
  AValue: string): boolean;
begin
  result := (AValue = '1') or (AValue = '2');
  FRealNumbers := RealNumbersFromMP2ProjectValue(AValue);
end;

end.
