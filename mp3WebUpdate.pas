(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3WebUpdate;

interface

uses
  Windows, SysUtils, Forms, Dialogs, StrUtils,
  sitInetUtils, sitSFWebUpdate,
  mp3Settings, mp3Consts;

type
  Tmp3WebUpdate = class(TsitSFWebUpdate)
  strict private
    function IsNewerByDate(ADateStr: string): boolean;
  strict protected
    function IsUpgradeDownload(ADownload: TsitSFDownload): boolean; override;
  public
    constructor Create;
    function DownloadUpdate(ADownload: TsitSFDownload): boolean;
    function LaunchDownloadedUpdate: boolean;
  end;

implementation

{ Tmp3WebUpdate }

constructor Tmp3WebUpdate.Create;
begin
  inherited;
  AgentName := PROJECT_NAME;
  UpdateRssUrl := PROJECT_ALL_DOWNLOADS_RSS_URL;
end;

function Tmp3WebUpdate.IsNewerByDate(ADateStr: string): boolean;
var dRemote, dLocal: TDateTime; y, m, d: word;
begin
  if TryRFC822DateToDateTime(ADateStr, dRemote) then begin
    // only remote date
    DecodeDate(dRemote, y, m, d);
    dRemote := EncodeDate(y, m, d);
    // get local date
    dLocal := EncodeDate(StrToInt(RELEASE_DATE_YEAR), StrToInt(RELEASE_DATE_MONTH),
      StrToInt(RELEASE_DATE_DAY));
    result := dRemote > dLocal;
  end else
    result := false;
end;

function Tmp3WebUpdate.IsUpgradeDownload(ADownload: TsitSFDownload): boolean;
begin
  result := inherited IsUpgradeDownload(ADownload)
    and StartsText('/'+PROJECT_MAJOR_VERSION+'.',ADownload.Title)
    and EndsText('-setup.exe',ADownload.Title)
    and IsNewerByDate(ADownload.PubDate);
end;

function Tmp3WebUpdate.LaunchDownloadedUpdate: boolean;
begin
  result := LaunchInstallationPendingUpdate('/UPDATE');
end;

function Tmp3WebUpdate.DownloadUpdate(ADownload: TsitSFDownload): boolean;
var s: string;
begin
  result := false;
  s := PROJECT_DOWNLOAD_URL + ADownload.Title;
  if assigned(ADownload) then
    result := DownloadUpdateFromURL(s, gSettings.ConfigPath + ExtractUrlFileName(s), true);
end;

end.
