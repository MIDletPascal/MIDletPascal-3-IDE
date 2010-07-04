(*
    MIDletPascal 3.0 Command Line Group Manager
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3CLGMCommands;

interface

uses
  SysUtils;

const
  PROGRAM_NAME = 'mp3clgm';

  CMD_NEW_GROUP = 'new';
  CMD_REMOVE_GROUP = 'remove';
  CMD_INCLUDE_PROJECT = 'include';
  CMD_EXCLUDE_PROJECT = 'exclude';
  CMD_UNKNOWN = 'Unknown';

type
  Tmp3CLGMCommand = (
    gmNone,
    gmNew, gmRemove,
    gmIncludeProject, gmExcludeProject,
    gmUnknown
  );

function StringToCLGMCommand(AString: string): Tmp3CLGMCommand;
function CLGMCommandToString(ACommand: Tmp3CLGMCommand): string;

function GetCommandUsage(ACommand: Tmp3CLGMCommand): string;

implementation

function StringToCLGMCommand(AString: string): Tmp3CLGMCommand;
begin
  if AString = '' then
    result := gmNone
  else if CompareText(AString, CMD_NEW_GROUP) = 0 then
    result := gmNew
  else if CompareText(AString, CMD_REMOVE_GROUP) = 0 then
    result := gmRemove
  else if CompareText(AString, CMD_INCLUDE_PROJECT) = 0 then
    result := gmIncludeProject
  else if CompareText(AString, CMD_EXCLUDE_PROJECT) = 0 then
    result := gmExcludeProject
  else
    result := gmUnknown;
end;

function CLGMCommandToString(ACommand: Tmp3CLGMCommand): string;
begin
  case ACommand of
    gmNone : result := '';
    gmNew : result := CMD_NEW_GROUP;
    gmRemove : result := CMD_REMOVE_GROUP;
    gmIncludeProject : result := CMD_INCLUDE_PROJECT;
    gmExcludeProject : result := CMD_EXCLUDE_PROJECT;
  else
    //gmUnknown :
    result := CMD_UNKNOWN;
  end;
end;

function GetCommandUsage(ACommand: Tmp3CLGMCommand): string;
begin
  case ACommand of
    gmNone : result := PROGRAM_NAME+' <command> <group_fullpath> [<parameter1>] [<parameter2>]';
    gmNew : result := PROGRAM_NAME+' '+CMD_NEW_GROUP+' <group_filename>';
    gmRemove : result := PROGRAM_NAME+' '+CMD_REMOVE_GROUP+' <group_filename>';
    gmIncludeProject : result := PROGRAM_NAME+' '+CMD_INCLUDE_PROJECT+' <group_filename> <project_filename>';
    gmExcludeProject : result := PROGRAM_NAME+' '+CMD_EXCLUDE_PROJECT+' <group_filename> <project_filename>';
  else
    result := '-';
  end;
end;

end.
