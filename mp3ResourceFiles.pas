(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3ResourceFiles;

interface

uses
  Windows, SysUtils, Contnrs, Classes,
  mp3Consts, mp3FileKind;

type
  Tmp3ResourceFile = class
  private
    FDirectory: string;
    FFilename: string;
    FConfigurations: string;
    function GetKind: Tmp3FileKind;
  public
    function Exists: boolean;
    function GetFullname: string;
    function PresentInConfiguration(AConfigurationName: string): boolean;
    property Filename: string read FFilename;
    property Configurations: string read FConfigurations;
    property Kind: Tmp3FileKind read GetKind;
  end;

  Tmp3ResourceFiles = class
  private
    FDirectory: string;
    FList: TObjectList;
    function GetCount: integer;
    function GetResourceFile(AIndex: integer): Tmp3ResourceFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(AFilename: string; AConfigurations: string = ':all:'): boolean;
    function Rename(AOldFilename, ANewFilename: string): boolean;
    function ModifyConfigurations(AFileName, ANewConfigurations: string): boolean;
    function Delete(AFilename: string; ADeleteFromDisk: boolean = true): boolean;
    function GetResourceFileByName(AFilename: string): Tmp3ResourceFile;
    property ResourceFile[AIndex: integer]: Tmp3ResourceFile read GetResourceFile; default;
    property Count: integer read GetCount;
    property Directory: string read FDirectory write FDirectory;
  end;

implementation

{ Tmp3ResourceFile }

function Tmp3ResourceFile.Exists: boolean;
begin
  result := FileExists(FDirectory + FFilename);
end;

function Tmp3ResourceFile.GetKind: Tmp3FileKind;
begin
  result := RetrieveFileKind(FFilename);
end;

function Tmp3ResourceFile.GetFullname: string;
begin
  result := FDirectory + FFilename;
end;

function Tmp3ResourceFile.PresentInConfiguration(
  AConfigurationName: string): boolean;
var x: integer;
begin
  result := false;
  if FConfigurations = CONFIGS_ALL then
    result := true
  else
    with TStringList.Create do
    try
      CommaText := FConfigurations;
      for x := 0 to Count - 1 do
        if SameText(Strings[x],AConfigurationName) then begin
          result := true;
          break;
        end;
    finally
      Free;
    end;
end;

{ Tmp3ResourceFiles }

procedure Tmp3ResourceFiles.Clear;
begin
  FDirectory := '';
  FList.Clear;
end;

constructor Tmp3ResourceFiles.Create;
begin
  FList := TObjectList.Create;
end;

destructor Tmp3ResourceFiles.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function Tmp3ResourceFiles.GetCount: integer;
begin
  result := FList.Count;
end;

function Tmp3ResourceFiles.GetResourceFile(AIndex: integer): Tmp3ResourceFile;
begin
  result := Tmp3ResourceFile(FList[AIndex]);
end;

function Tmp3ResourceFiles.GetResourceFileByName(AFilename: string): Tmp3ResourceFile;
var i: integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
    if Tmp3ResourceFile(FList[i]).FFilename = AFilename then begin
      result := Tmp3ResourceFile(FList[i]);
      break;
    end;
end;

// j-a-s-d: also works as MP2's Import Resource
function Tmp3ResourceFiles.Add(AFilename: string;
  AConfigurations: string = ':all:'): boolean;
var resourcefile: Tmp3ResourceFile; resName: string;
begin
  result := false;
  resName := ExtractFileName(AFilename);
  if (GetResourceFileByName(AFilename)<>nil)
    or (GetResourceFileByName(resName)<>nil) then
    exit;
  resourcefile := Tmp3ResourceFile.Create;
  resourcefile.FDirectory := FDirectory;
  resourcefile.FFilename := resName;
  resourcefile.FConfigurations := AConfigurations;
  FList.Add(resourcefile);
  if (not FileExists(resourcefile.GetFullname))and(FileExists(AFilename)) then
    CopyFile(pchar(AFilename), pchar(resourcefile.GetFullname), false);
  result := FileExists(resourcefile.GetFullname);
end;

function Tmp3ResourceFiles.Delete(AFilename: string; ADeleteFromDisk: boolean = true): boolean;
var resourcefile: Tmp3ResourceFile;
begin
  resourcefile := GetResourceFileByName(AFilename);
  if assigned(resourcefile) and ADeleteFromDisk then begin
    DeleteFile(pchar(FDirectory + AFilename));
    result := not resourcefile.Exists;
  end else
    result := true;
  if result then
    FList.Remove(resourcefile);
end;

function Tmp3ResourceFiles.ModifyConfigurations(AFileName,
  ANewConfigurations: string): boolean;
var resourcefile: Tmp3ResourceFile;
begin
  result := false;
  resourcefile := GetResourceFileByName(AFileName);
  if assigned(resourcefile) then begin
    resourcefile.FConfigurations := ANewConfigurations;
    result := true;
  end;
end;

function Tmp3ResourceFiles.Rename(AOldFilename, ANewFilename: string): boolean;
var resourcefile: Tmp3ResourceFile;
begin
  result := false;
  if GetResourceFileByName(ANewFilename)<>nil then
    exit;
  resourcefile := GetResourceFileByName(AOldFilename);
  if assigned(resourcefile) then begin
    result := RenameFile(pchar(FDirectory + AOldFilename), pchar(FDirectory + ANewFilename));
    if result then
      resourcefile.FFilename := ANewFilename;
  end;
end;

end.
