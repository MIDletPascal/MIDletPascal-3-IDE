(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Core;

interface

uses
  Forms, SysUtils,
  sitCore,
  mp3Main, mp3FileKind, mp3WebUpdate, mp3Consts;

type
  Tmp3Core = class sealed(TsitCore)
  strict private
    FMainForm: Tmp3MainForm;
    FWebUpdate: Tmp3WebUpdate;
    procedure ParseCommandLine;
  public
    constructor Create;
    destructor Destroy; override;
    property WebUpdate: Tmp3WebUpdate read FWebUpdate;
  end;

var
  gCore: Tmp3Core;

implementation

{ Tmp3Core }

constructor Tmp3Core.Create;
begin
  if assigned(gCore) then
    raise Exception.Create('It already has a Core!');
  gCore := Self;
  inherited;
  Application.Title := PROJECT_NAME;
  FWebUpdate := Tmp3WebUpdate.Create;
  Application.CreateForm(Tmp3MainForm, FMainForm);
  FMainForm.ReadSettings;
  ParseCommandLine;
end;

destructor Tmp3Core.Destroy;
begin
  FMainForm.WriteSettings;
  FWebUpdate.Free;
  inherited;
end;

procedure Tmp3Core.ParseCommandLine;
begin
  if SameText(ParamStr(1),BUILD_SWITCH) then begin
    if FileExists(ParamStr(2)) then begin
      FMainForm.Load(ParamStr(2));
      FMainForm.PerformBuild;
      Terminating := true;
    end;
  end else if FileExists(ParamStr(1)) then
    FMainForm.Load(ParamStr(1));
end;

end.
