(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Core;

interface

uses
  Forms, SysUtils,
  tuiTrayIcon,
  sitCore,
  mp3Main, mp3FileKind, mp3WebUpdate, mp3Consts, mp3Settings;

type
  Tmp3Core = class sealed(TsitCore)
  strict private
    FMainForm: Tmp3MainForm;
    FWebUpdate: Tmp3WebUpdate;
    FTrayIcon: TtuiTrayIcon;
    procedure ParseCommandLine;
    procedure OnTrayIconDoubleClick(Sender: TObject);
    procedure OnApplicationMinimize(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property WebUpdate: Tmp3WebUpdate read FWebUpdate;
  end;

var
  gCore: Tmp3Core;

implementation

uses
  tuiGUIUtils;

{ Tmp3Core }

constructor Tmp3Core.Create;
begin
  if assigned(gCore) then
    FreeAndNil(gCore);
  gCore := Self;
  inherited;
  Application.Title := PROJECT_NAME;
  FTrayIcon := TtuiTrayIcon.Create;
  FTrayIcon.Icon := Application.Icon;
  FTrayIcon.OnDblClick := OnTrayIconDoubleClick;
  Application.OnMinimize := OnApplicationMinimize;
  FWebUpdate := Tmp3WebUpdate.Create;
  Application.CreateForm(Tmp3MainForm, FMainForm);
  FMainForm.ReadSettings;
  ParseCommandLine;
end;

destructor Tmp3Core.Destroy;
begin
  if assigned(FTrayIcon) then begin
    FTrayIcon.Visible := false;
    FreeAndNil(FTrayIcon);
  end;
  FMainForm.WriteSettings;
  FWebUpdate.Free;
  inherited;
end;

procedure Tmp3Core.OnApplicationMinimize(Sender: TObject);
begin
  if FTrayIcon.Visible <> gSettings.MinimizeToTray then
    FTrayIcon.Visible := gSettings.MinimizeToTray;
end;

procedure Tmp3Core.OnTrayIconDoubleClick(Sender: TObject);
begin
  ShowMainForm;
  FTrayIcon.Visible := false;
end;

procedure Tmp3Core.ParseCommandLine;
begin
  if SameText(ParamStr(1),BUILD_SWITCH) then begin
    FMainForm.CommandLineBuild(ParamStr(2));
    Terminating := true;
  end else if FileExists(ParamStr(1)) then
    FMainForm.Load(ParamStr(1));
end;

end.
