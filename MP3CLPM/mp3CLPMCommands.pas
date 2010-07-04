(*
    MIDletPascal 3.0 Command Line Project Manager
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3CLPMCommands;

interface

uses
  SysUtils;

const
  PROGRAM_NAME = 'mp3clpm';

  CMD_NEW_PROJECT = 'new';
  CMD_REMOVE_PROJECT = 'remove';
  CMD_SET_VERSION = 'setversion';
  CMD_SET_VENDOR = 'setvendor';
  CMD_SET_ICON = 'seticon';
  CMD_ADD_SOURCEFILE = 'addsf';
  CMD_DEL_SOURCEFILE = 'delsf';
  CMD_REN_SOURCEFILE = 'rensf';
  CMD_ADD_RESOURCEFILE = 'addrf';
  CMD_DEL_RESOURCEFILE = 'delrf';
  CMD_REN_RESOURCEFILE = 'renrf';
  CMD_SET_RESOURCEFILE_C = 'setrfc';
  CMD_ADD_BUILDCONFIG = 'addbc';
  CMD_DEL_BUILDCONFIG = 'delbc';
  CMD_REN_BUILDCONFIG = 'renbc';
  CMD_SET_BUILDCONFIG_TYPE = 'setbct';
  CMD_SET_BUILDCONFIG_VERSION = 'setbcv';
  CMD_SET_BUILDCONFIG_MATH = 'setbcm';
  CMD_SET_ACTIVE_BUILDCONFIG = 'setabc';
  CMD_UNKNOWN = 'Unknown';

type
  Tmp3CLPMCommand = (
    pmNone,
    pmNew, pmRemove,
    pmSetVersion, pmSetVendor, pmSetIcon,
    pmAddSF, pmDelSF, pmRenSF,
    pmAddRF, pmDelRF, pmRenRF, pmSetRFC,
    pmAddBC, pmDelBC, pmRenBC, pmSetBCT, pmSetBCV, pmSetBCM, pmSetABC,
    pmUnknown
  );

function StringToCLPMCommand(AString: string): Tmp3CLPMCommand;
function CLPMCommandToString(ACommand: Tmp3CLPMCommand): string;

function GetCommandUsage(ACommand: Tmp3CLPMCommand): string;

implementation

function StringToCLPMCommand(AString: string): Tmp3CLPMCommand;
begin
  if AString = '' then
    result := pmNone
  else if CompareText(AString, CMD_NEW_PROJECT) = 0 then
    result := pmNew
  else if CompareText(AString, CMD_REMOVE_PROJECT) = 0 then
    result := pmRemove
  else if CompareText(AString, CMD_SET_VERSION) = 0 then
    result := pmSetVersion
  else if CompareText(AString, CMD_SET_VENDOR) = 0 then
    result := pmSetVendor
  else if CompareText(AString, CMD_SET_ICON) = 0 then
    result := pmSetIcon
  else if CompareText(AString, CMD_ADD_SOURCEFILE) = 0 then
    result := pmAddSF
  else if CompareText(AString, CMD_DEL_SOURCEFILE) = 0 then
    result := pmDelSF
  else if CompareText(AString, CMD_REN_SOURCEFILE) = 0 then
    result := pmRenSF
  else if CompareText(AString, CMD_ADD_RESOURCEFILE) = 0 then
    result := pmAddRF
  else if CompareText(AString, CMD_DEL_RESOURCEFILE) = 0 then
    result := pmDelRF
  else if CompareText(AString, CMD_REN_RESOURCEFILE) = 0 then
    result := pmRenRF
  else if CompareText(AString, CMD_SET_RESOURCEFILE_C) = 0 then
    result := pmSetRFC
  else if CompareText(AString, CMD_ADD_BUILDCONFIG) = 0 then
    result := pmAddBC
  else if CompareText(AString, CMD_DEL_BUILDCONFIG) = 0 then
    result := pmDelBC
  else if CompareText(AString, CMD_REN_BUILDCONFIG) = 0 then
    result := pmRenBC
  else if CompareText(AString, CMD_SET_BUILDCONFIG_TYPE) = 0 then
    result := pmSetBCT
  else if CompareText(AString, CMD_SET_BUILDCONFIG_VERSION) = 0 then
    result := pmSetBCV
  else if CompareText(AString, CMD_SET_BUILDCONFIG_MATH) = 0 then
    result := pmSetBCM
  else if CompareText(AString, CMD_SET_ACTIVE_BUILDCONFIG) = 0 then
    result := pmSetABC
  else
    result := pmUnknown;
end;

function CLPMCommandToString(ACommand: Tmp3CLPMCommand): string;
begin
  case ACommand of
    pmNone : result := '';
    pmNew : result := CMD_NEW_PROJECT;
    pmRemove : result := CMD_REMOVE_PROJECT;
    pmSetVersion : result := CMD_SET_VERSION;
    pmSetVendor : result := CMD_SET_VENDOR;
    pmSetIcon : result := CMD_SET_ICON;
    pmAddSF : result := CMD_ADD_SOURCEFILE;
    pmDelSF : result := CMD_DEL_SOURCEFILE;
    pmRenSF : result := CMD_REN_SOURCEFILE;
    pmAddRF : result := CMD_ADD_RESOURCEFILE;
    pmDelRF : result := CMD_DEL_RESOURCEFILE;
    pmRenRF : result := CMD_REN_RESOURCEFILE;
    pmSetRFC : result := CMD_SET_RESOURCEFILE_C;
    pmAddBC : result := CMD_ADD_BUILDCONFIG;
    pmDelBC : result := CMD_DEL_BUILDCONFIG;
    pmRenBC : result := CMD_REN_BUILDCONFIG;
    pmSetBCT : result := CMD_SET_BUILDCONFIG_TYPE;
    pmSetBCV : result := CMD_SET_BUILDCONFIG_VERSION;
    pmSetBCM : result := CMD_SET_BUILDCONFIG_MATH;
    pmSetABC : result := CMD_SET_ACTIVE_BUILDCONFIG;
  else
    //pmUnknown :
    result := CMD_UNKNOWN;
  end;
end;

function GetCommandUsage(ACommand: Tmp3CLPMCommand): string;
begin
  case ACommand of
    pmNone : result := PROGRAM_NAME+' <command> <project_fullpath> [<parameter1>] [<parameter2>]';
    pmNew : result := PROGRAM_NAME+' '+CMD_NEW_PROJECT+' <project_filename>';
    pmRemove : result := PROGRAM_NAME+' '+CMD_REMOVE_PROJECT+' <project_filename>';
    pmSetVersion : result := PROGRAM_NAME+' '+CMD_SET_VERSION+' <project_filename> <version>';
    pmSetVendor : result := PROGRAM_NAME+' '+CMD_SET_VENDOR+' <project_filename> <vendor>';
    pmSetIcon : result := PROGRAM_NAME+' '+CMD_SET_ICON+' <project_filename> <icon>';
    pmAddSF : result := PROGRAM_NAME+' '+CMD_ADD_SOURCEFILE+' <project_filename> <source_filename>';
    pmDelSF : result := PROGRAM_NAME+' '+CMD_DEL_SOURCEFILE+' <project_filename> <source_filename>';
    pmRenSF : result := PROGRAM_NAME+' '+CMD_REN_SOURCEFILE+' <project_filename> <source_filename> <new_filename>';
    pmAddRF : result := PROGRAM_NAME+' '+CMD_ADD_RESOURCEFILE+' <project_filename> <resource_filename>';
    pmDelRF : result := PROGRAM_NAME+' '+CMD_DEL_RESOURCEFILE+' <project_filename> <resource_filename>';
    pmRenRF : result := PROGRAM_NAME+' '+CMD_REN_RESOURCEFILE+' <project_filename> <resource_filename>';
    pmSetRFC : result := PROGRAM_NAME+' '+CMD_SET_RESOURCEFILE_C+' <project_filename> <resource_filename> <build_configs_as_csv>';
    pmAddBC : result := PROGRAM_NAME+' '+CMD_ADD_BUILDCONFIG+' <project_filename> <build_config_name>';
    pmDelBC : result := PROGRAM_NAME+' '+CMD_DEL_BUILDCONFIG+' <project_filename> <build_config_name>';
    pmRenBC : result := PROGRAM_NAME+' '+CMD_REN_BUILDCONFIG+' <project_filename> <build_config_name> <new_name>';
    pmSetBCT : result := PROGRAM_NAME+' '+CMD_SET_BUILDCONFIG_TYPE+' <project_filename> <build_config_name> <new_type>';
    pmSetBCV : result := PROGRAM_NAME+' '+CMD_SET_BUILDCONFIG_VERSION+' <project_filename> <build_config_name> <new_version>';
    pmSetBCM : result := PROGRAM_NAME+' '+CMD_SET_BUILDCONFIG_MATH+' <project_filename> <build_config_name> <new_math>';
    pmSetABC : result := PROGRAM_NAME+' '+CMD_SET_ACTIVE_BUILDCONFIG+' <project_filename> <build_config_name>';
  else
    result := '-';
  end;
end;


end.
