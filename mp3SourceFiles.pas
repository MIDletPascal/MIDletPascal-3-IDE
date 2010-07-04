(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3SourceFiles;

interface

uses
  Windows, SysUtils, Contnrs, Classes,
  mp3Consts;

type
  Tmp3SourceFile = class
  private
    FFilename: string;
    FDirectory: string;
  public
    function Exists: boolean;
    function IsReadOnly: boolean;
    function GetFullName: string;
    function GetUnitName: string;
    property Filename: string read FFilename;
  end;

  Tmp3SourceFiles = class
  private
    FDirectory: string;
    FList: TObjectList;
    function GetCount: integer;
    function GetSourceFile(AIndex: integer): Tmp3SourceFile;
    function GetProgramFile: Tmp3SourceFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function New(AFilename: string): boolean;
    function Add(AFilename: string): boolean;
    function Ensure(AFilename: string): boolean;
    function Rename(AOldFilename, ANewFilename: string): boolean;
    function Delete(AFilename: string; ADeleteFromDisk: boolean = true): boolean;
    function GetSourceFileByName(AFilename: string): Tmp3SourceFile;
    property ProgramFile: Tmp3SourceFile read GetProgramFile;
    property SourceFile[AIndex: integer]: Tmp3SourceFile read GetSourceFile; default;
    property Count: integer read GetCount;
    property Directory: string read FDirectory write FDirectory;
  end;

implementation

{ Tmp3SourceFile }

function Tmp3SourceFile.Exists: boolean;
begin
  result := FileExists(FDirectory + FFilename);
end;

function Tmp3SourceFile.GetFullname: string;
begin
  result := FDirectory + StringReplace(FFilename,'/','\',[rfReplaceAll]);
end;

function Tmp3SourceFile.GetUnitName: string;
begin
  result := ChangeFileExt(ExtractFileName(StringReplace(FFilename,'/','\',[rfReplaceAll])),'');
end;

function Tmp3SourceFile.IsReadOnly: boolean;
begin
  result := (FileGetAttr(FFilename) and faReadOnly) > 0;
end;

{ Tmp3SourceFiles }

procedure Tmp3SourceFiles.Clear;
begin
  FDirectory := '';
  FList.Clear;
end;

constructor Tmp3SourceFiles.Create;
begin
  FList := TObjectList.Create;
end;

destructor Tmp3SourceFiles.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function Tmp3SourceFiles.GetCount: integer;
begin
  result := FList.Count;
end;

function Tmp3SourceFiles.GetProgramFile: Tmp3SourceFile;
begin
  result := nil;
  if FList.Count > 0 then
    result := Tmp3SourceFile(FList[0]);
end;

function Tmp3SourceFiles.GetSourceFile(AIndex: integer): Tmp3SourceFile;
begin
  result := Tmp3SourceFile(FList[AIndex]);
end;

function Tmp3SourceFiles.GetSourceFileByName(AFilename: string): Tmp3SourceFile;
var i: integer;
begin
  result := nil;
  for i := 0 to FList.Count-1 do
    if SameText(Tmp3SourceFile(FList[i]).GetUnitName, AFilename) or
      SameText(Tmp3SourceFile(FList[i]).FFilename, AFilename) or
     SameText(Tmp3SourceFile(FList[i]).GetFullname, AFilename) then
    begin
      result := Tmp3SourceFile(FList[i]);
      break;
    end;
end;

function Tmp3SourceFiles.Ensure(AFilename: string): boolean;
var unitName: string;
begin
  unitName := ChangeFileExt(ExtractFileName(
      StringReplace(AFilename,'/','\',[rfReplaceAll])
    ), '');
  if Length(unitName)<UNIT_NAME_LENGTH_MINIMUM then
    exit;
  with TStringList.Create do
  try
    Text := Format(NEW_UNIT_PATTERN, [unitName]);
    SaveToFile(FDirectory + AFilename);
  finally
    Free;
  end;
  result := FileExists(FDirectory + AFilename);
end;

function Tmp3SourceFiles.New(AFilename: string): boolean;
begin
  result := Ensure(AFilename);
  if result then
    result := Add(AFilename);
end;

function Tmp3SourceFiles.Add(AFilename: string): boolean;
var sourcefile: Tmp3SourceFile; unitName: string;
begin
  result := false;
  unitName := ChangeFileExt(ExtractFileName(AFilename),'');
  if (Length(unitName)<UNIT_NAME_LENGTH_MINIMUM)
  or (GetSourceFileByName(unitName)<>nil) then
    exit;
  sourcefile := Tmp3SourceFile.Create;
  sourcefile.FDirectory := FDirectory;
  sourcefile.FFilename := ExtractFileName(AFilename);
  FList.Add(sourcefile);
  if (not FileExists(sourcefile.GetFullname))and(FileExists(AFilename)) then
    CopyFile(pchar(AFilename), pchar(sourcefile.GetFullname), false);
  result := FileExists(sourcefile.GetFullname);
end;

function Tmp3SourceFiles.Delete(AFilename: string; ADeleteFromDisk: boolean = true): boolean;
var sourcefile: Tmp3SourceFile;
begin
  result := false;
  sourcefile := GetSourceFileByName(AFilename);
  if assigned(sourcefile) and ADeleteFromDisk then begin
    if sourcefile<>GetProgramFile then
      result := DeleteFile(pchar(FDirectory + AFilename)) or (not sourcefile.Exists)
  end else
    result := true;
  if result then
    FList.Remove(sourcefile);
end;

function Tmp3SourceFiles.Rename(AOldFilename, ANewFilename: string): boolean;
var sourcefile: Tmp3SourceFile; unitName, newFullName: string;
begin
  result := false;
  unitName := ChangeFileExt(ExtractFileName(ANewFilename),'');
  if (Length(unitName)<UNIT_NAME_LENGTH_MINIMUM)
  or (GetSourceFileByName(unitName)<>nil) then
    exit;
  newFullName := FDirectory + ANewFilename;
  sourcefile := GetSourceFileByName(AOldFilename);
  if assigned(sourcefile) then begin
    result := RenameFile(pchar(sourcefile.GetFullName), pchar(newFullName))
      or (not sourcefile.Exists and FileExists(newFullName));
    if result then
      sourcefile.FFilename := ANewFilename;
  end;
end;

end.
