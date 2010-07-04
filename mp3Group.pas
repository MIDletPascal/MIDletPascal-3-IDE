(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Group;

interface

uses
  Windows,
  SysUtils, Classes;

type
  Tmp3Projects = class
  private
    FList: TStringList;
    function GetCount: integer;
    function GetItems(AIndex: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Include(AFilename: string): boolean;
    function Exclude(AFilename: string): boolean;
    procedure Clear;
    property Items[AIndex: integer]: string read GetItems; default;
    property Count: integer read GetCount;
  end;

  Tmp3Group = class
  private
    FProjects: Tmp3Projects;
    FFilename: string;
    FLoaded: boolean;
    procedure Init(AFilename: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Close;
    function New(AFilename: string): boolean;
    function Load(AFilename: string): boolean;
    function Save: boolean;
    property Filename: string read FFilename;
    property Projects: Tmp3Projects read FProjects;
    property Loaded: boolean read FLoaded;
  end;

implementation

uses
  mp3GroupPersistence, mp3Consts;

{ Tmp3Projects }

constructor Tmp3Projects.Create;
begin
  FList := TStringList.Create;
end;

destructor Tmp3Projects.Destroy;
begin
  FList.Free;
end;

function Tmp3Projects.Include(AFilename: string): boolean;
begin
  if FList.IndexOf(AFilename)=-1 then
    FList.Add(AFilename);
  result := true;
end;

function Tmp3Projects.Exclude(AFilename: string): boolean;
var i: integer;
begin
  i := FList.IndexOf(AFilename);
  if i>-1 then
    FList.Delete(i);
  result := FList.IndexOf(AFilename)=-1;
end;

procedure Tmp3Projects.Clear;
begin
  FList.Clear;
end;

function Tmp3Projects.GetCount: integer;
begin
  result := FList.Count;
end;

function Tmp3Projects.GetItems(AIndex: integer): string;
begin
  result := FList[AIndex];
end;

{ Tmp3Group }

constructor Tmp3Group.Create;
begin
  FLoaded := false;
  FProjects := Tmp3Projects.Create;
end;

destructor Tmp3Group.Destroy;
begin
  FProjects.Free;
end;

procedure Tmp3Group.Init(AFilename: string);
begin
  Close;
  FFilename := AFilename;
end;

procedure Tmp3Group.Close;
begin
  FLoaded := false;
  FFilename := '';
  FProjects.Clear;
end;

function Tmp3Group.Load(AFilename: string): boolean;
begin
  Init(AFilename);
  FLoaded := FileExists(FFilename) and ReadGroup(Self);
  result := FLoaded;
end;

function Tmp3Group.New(AFilename: string): boolean;
begin
  Init(ChangeFileExt(AFilename, EXTENSION_GROUP));
  // ?
  result := Save;
end;

function Tmp3Group.Save: boolean;
begin
  result := WriteGroup(Self);
end;

end.
