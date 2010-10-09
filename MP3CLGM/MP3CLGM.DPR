(*
    MIDletPascal 3.x Command Line Group Manager
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)

    2009 december 24 - started coding
    2009 december 26 - last modification
*)

program mp3clgm;
{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$R-}
{$ENDIF}
uses
  Windows, SysUtils, Classes,
  mp3Group, mp3Consts,
  mp3CLGMCommands, mp3CLGMStrings;

procedure ShowUsage(ACommand: Tmp3CLGMCommand = gmNone);
begin
  WriteLn('-------------------------------------------------------------------------');
  WriteLn('MIDletPascal 3.0 Command Line Group Manager');
  WriteLn('by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)');
  WriteLn('-------------------------------------------------------------------------');
  if ACommand = gmNone then begin
    WriteLn(S_AVAILABLE_COMMANDS+':');
    WriteLn(' '+CMD_NEW_GROUP+', '+
                CMD_REMOVE_GROUP+', // '+S_GROUP_COMMANDS);
    WriteLn(' '+CMD_INCLUDE_PROJECT+', '+
                CMD_EXCLUDE_PROJECT+', // '+S_GROUP_PROJECTS_COMMANDS);
  end else
    WriteLn(S_COMMAND+': '+CLGMCommandToString(ACommand));
  WriteLn('-------------------------------------------------------------------------');
  WriteLn(S_USAGE+': '+GetCommandUsage(ACommand));
end;

function PerformGroupCommand(AGroup: Tmp3Group; ACommand: Tmp3CLGMCommand; var AError: string): boolean;
var aux: string;
begin
  result := ParamStr(2) <> '';
  if result then begin
    case ACommand of
    gmNew:
      begin
        result := not AGroup.Load(ParamStr(2));
        if result then begin
          result := AGroup.New(ParamStr(2));
          if not result then
            AError := S_ERROR_CMD_NEW;
        end else
          AError := S_ERROR_GROUP_ALREADY_EXISTS;
      end;
    gmRemove:
      begin
        result := AGroup.Load(ParamStr(2));
        if result then begin
          aux := AGroup.Filename;
          AGroup.Close;
          result := DeleteFile(aux);
          if not result then
            AError := S_ERROR_CMD_REMOVE;
        end else
          AError := S_ERROR_COULD_NOT_LOAD_GROUP;
      end;
    end;
  end else
    AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
end;

function PerformProjectsCommand(AGroup: Tmp3Group; ACommand: Tmp3CLGMCommand; var AError: string): boolean;
begin
  result := AGroup.Load(ParamStr(2));
  if result then begin
    result := ParamStr(3) <> '';
    if result then begin
      case ACommand of
      gmIncludeProject:
        begin
          result := AGroup.Projects.Include(ParamStr(3)) and AGroup.Save;
          if not result then
            AError := S_ERROR_CMD_INCLUDE_PROJECT;
        end;
      gmExcludeProject:
        begin
          result := AGroup.Projects.Exclude(ParamStr(3)) and AGroup.Save;
          if not result then
            AError := S_ERROR_CMD_EXCLUDE_PROJECT;
        end;
      end;
    end else
      AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
  end else
    AError := S_ERROR_COULD_NOT_LOAD_GROUP;
end;

var AGroup: Tmp3Group; ACommand: Tmp3CLGMCommand; AError: string; ASuccess: boolean;
begin
  ACommand := StringToCLGMCommand(ParamStr(1));

  if ParamCount<2 then
  begin
    ShowUsage(ACommand);
    exit;
  end;

  ASuccess := false;
  AGroup := Tmp3Group.Create;
  try
    if ACommand in [gmNew,gmRemove] then
      ASuccess := PerformGroupCommand(AGroup,ACommand,AError)
    else if ACommand in [gmIncludeProject,gmExcludeProject] then
      ASuccess := PerformProjectsCommand(AGroup,ACommand,AError);
  finally
    AGroup.Free;
  end;
  if not ASuccess then
    WriteLn(S_ERROR+': '+AError);
end.
