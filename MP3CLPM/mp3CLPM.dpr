(*
    MIDletPascal 3.x Command Line Project Manager
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)

    2009 november 02 - started coding
    2009 november 07 - last modification
*)

program mp3clpm;
{$APPTYPE CONSOLE}
{$IFDEF FPC}
  {$MODE DELPHI}
  {$R-}
{$ENDIF}
uses
  Windows, SysUtils, Classes,
  mp3Project, mp3Consts,
  mp3CLPMCommands, mp3CLPMStrings;

procedure ShowUsage(ACommand: Tmp3CLPMCommand = pmNone);
begin
  WriteLn('-------------------------------------------------------------------------');
  WriteLn('MIDlet Pascal 3.0 Command Line Project Manager');
  WriteLn('by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)');
  WriteLn('-------------------------------------------------------------------------');
  if ACommand = pmNone then begin
    WriteLn(S_AVAILABLE_COMMANDS+':');
    WriteLn(' '+CMD_NEW_PROJECT+', '+
                CMD_REMOVE_PROJECT+', // '+S_PROJECT_COMMANDS);
    WriteLn(' '+CMD_SET_VERSION+', '+
                CMD_SET_VENDOR+', '+
                CMD_SET_ICON+', // '+S_PROJECT_MIDLET_INFO_COMMANDS);
    WriteLn(' '+CMD_ADD_SOURCEFILE+', '+
                CMD_DEL_SOURCEFILE+', '+
                CMD_REN_SOURCEFILE+', // '+S_PROJECT_SOURCE_FILES_COMMANDS);
    WriteLn(' '+CMD_ADD_RESOURCEFILE+', '+
                CMD_DEL_RESOURCEFILE+', '+
                CMD_REN_RESOURCEFILE+', '+
                CMD_SET_RESOURCEFILE_C+', // '+S_PROJECT_RESOURCE_FILES_COMMANDS);
    WriteLn(' '+CMD_ADD_BUILDCONFIG+', '+
                CMD_DEL_BUILDCONFIG+', '+
                CMD_REN_BUILDCONFIG+', '+
                CMD_SET_ACTIVE_BUILDCONFIG+' // '+S_PROJECT_BUILD_CONFIGS_COMMANDS);
  end else
    WriteLn(S_COMMAND+': '+CLPMCommandToString(ACommand));
  WriteLn('-------------------------------------------------------------------------');
  WriteLn(S_USAGE+': '+GetCommandUsage(ACommand));
end;

function DelTree(Dir:string): boolean;
var SR: TSearchRec; i: integer; DirList, FileList: TStringList;
begin
  result := true;
  if Dir[Length(Dir)] <> '\' then
    Dir := Dir + '\';
  if FindFirst(Dir + '*.*',faAnyFile,SR) = 0 then
  begin
    DirList := TStringList.create;
    FileList := TStringList.create;
    try
      if (SR.Attr and faDirectory > 0) then
      begin
       if (SR.Name <> '.') and (SR.Name <> '..') then
         DirList.Add(Dir + SR.Name)
      end else
        FileList.Add(Dir + SR.Name);
      while FindNext(SR) = 0 do
        if (SR.Attr and faDirectory > 0) then
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            DirList.Add(Dir + SR.Name)
        end else
          FileList.Add(Dir + SR.Name);
      FindClose(SR);
      for i := 0 to FileList.Count -1 do
      begin
        result := DeleteFile(FileList.Strings[I]);
        if not result then
          exit;
      end;
      for i := 0 to DirList.Count -1 do
      begin
        result := DelTree(DirList.Strings[I]);
        if not result then
          exit;
      end;
      result := RemoveDir(Dir);
    finally
      DirList.free;
      FileList.free;
    end;
  end;
end;

function PerformProjectCommand(AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; var AError: string): boolean;
var aux: string;
begin
  result := ParamStr(2) <> '';
  if result then begin
    case ACommand of
    pmNew:
      begin
        result := not AProject.Load(ParamStr(2));
        if result then begin
          result := AProject.New(ParamStr(2));
          if not result then
            AError := S_ERROR_CMD_NEW;
        end else
          AError := S_ERROR_PROJECT_ALREADY_EXISTS;
      end;
    pmRemove:
      begin
        result := AProject.Load(ParamStr(2));
        if result then begin
          aux := AProject.ProjectDirectory;
          AProject.Close;
          DelTree(aux);
          result := not DirectoryExists(aux);
          if not result then
            AError := S_ERROR_CMD_REMOVE;
        end else
          AError := S_ERROR_COULD_NOT_LOAD_PROJECT;
      end;
    end;
  end else
    AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
end;

function PerformMidletInfoCommand(AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; var AError: string): boolean;
begin
  result := AProject.Load(ParamStr(2));
  if result then begin
    result := ParamStr(3) <> '';
    if result then begin
      case ACommand of
      pmSetIcon:
        begin
          AProject.MidletInfo.Icon := ParamStr(3);
          result := AProject.Save;
          if not result then
            AError := S_ERROR_CMD_SETICON;
        end;
      pmSetVersion:
        begin
          AProject.MidletInfo.Version := ParamStr(3);
          result := AProject.Save;
          if not result then
            AError := S_ERROR_CMD_SETVERSION;
        end;
      pmSetVendor:
        begin
          AProject.MidletInfo.Vendor := ParamStr(3);
          result := AProject.Save;
          if not result then
            AError := S_ERROR_CMD_SETVENDOR;
        end;
      end;
    end else
      AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
  end else
    AError := S_ERROR_COULD_NOT_LOAD_PROJECT;
end;


function PerformSourceFilesCommand(AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; var AError: string): boolean;
begin
  result := AProject.Load(ParamStr(2));
  if result then begin
    result := ParamStr(3) <> '';
    if result then begin
      case ACommand of
      pmAddSF:
        begin
          result := AProject.SourceFiles.Add(ParamStr(3)) and AProject.Save;
          if not result then
            AError := S_ERROR_CMD_ADDSF;
        end;
      pmDelSF:
        begin
          result := AProject.SourceFiles.Delete(ParamStr(3)) and AProject.Save;
          if not result then
            AError := S_ERROR_CMD_DELSF;
        end;
      pmRenSF:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            result := AProject.SourceFiles.Rename(ParamStr(3),ParamStr(4)) and AProject.Save;
            if not result then
              AError := S_ERROR_CMD_RENSF;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      end;
    end else
      AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
  end else
    AError := S_ERROR_COULD_NOT_LOAD_PROJECT;
end;

function PerformResourceFilesCommand(AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; var AError: string): boolean;
begin
  result := AProject.Load(ParamStr(2));
  if result then begin
    result := ParamStr(3) <> '';
    if result then begin
      case ACommand of
      pmAddRF:
        begin
          result := AProject.ResourceFiles.Add(ParamStr(3)) and AProject.Save;
          if not result then
            AError := S_ERROR_CMD_ADDRF;
        end;
      pmDelRF:
        begin
          result := AProject.ResourceFiles.Delete(ParamStr(3)) and AProject.Save;
          if not result then
            AError := S_ERROR_CMD_DELRF;
        end;
      pmRenRF:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            result := AProject.ResourceFiles.Rename(ParamStr(3),ParamStr(4)) and AProject.Save;
            if not result then
              AError := S_ERROR_CMD_RENRF;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      pmSetRFC:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            result := AProject.ResourceFiles.ModifyConfigurations(ParamStr(3),ParamStr(4)) and AProject.Save;
            if not result then
              AError := S_ERROR_CMD_SETRFC;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      end;
    end else
      AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
  end else
    AError := S_ERROR_COULD_NOT_LOAD_PROJECT;
end;

function PerformBuildConfigurationsCommand(AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; var AError: string): boolean;
var aux: integer;
begin
  result := AProject.Load(ParamStr(2));
  if result then begin
    result := ParamStr(3) <> '';
    if result then begin
      case ACommand of
      pmAddBC:
        begin
          result := AProject.BuildConfigurations.IsValidBuildConfigurationName(ParamStr(3));
          if result then begin
            result := AProject.BuildConfigurations.Add(ParamStr(3)) and AProject.Save;
            if not result then
              AError := S_ERROR_CMD_ADDBC;
          end else
            AError := S_ERROR_BUILDCONFIG_NAME_NOT_VALID;
        end;
      pmDelBC:
        begin
          result := AProject.BuildConfigurations.Delete(ParamStr(3)) and AProject.Save;
          if not result then
            AError := S_ERROR_CMD_DELBC;
        end;
      pmRenBC:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            result := AProject.BuildConfigurations.IsValidBuildConfigurationName(ParamStr(4));
            if result then begin
              result := AProject.BuildConfigurations.Rename(ParamStr(3),ParamStr(4)) and AProject.Save;
              if not result then
                AError := S_ERROR_CMD_RENBC;
            end else
              AError := S_ERROR_BUILDCONFIG_NAME_NOT_VALID;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      pmSetBCT:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            aux := AProject.BuildConfigurations.GetBuildConfigurationIndex(ParamStr(3));
            result := aux > -1;
            if result then begin
              result := AProject.BuildConfigurations[aux].SetMIDletTypeAsMP2ProjectValue(ParamStr(4));
              if result then begin
                result := AProject.Save;
                if not result then
                  AError := S_ERROR_CMD_SETBCT;
              end else
                AError := S_ERROR_REQUIRED_PARAMETER_IS_WRONG;
            end else
              AError := S_ERROR_BUILDCONFIG_DOES_NOT_EXIST;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      pmSetBCV:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            aux := AProject.BuildConfigurations.GetBuildConfigurationIndex(ParamStr(3));
            result := aux > -1;
            if result then begin
              result := AProject.BuildConfigurations[aux].SetMIDPVersionAsMP2ProjectValue(ParamStr(4));
              if result then begin
                result := AProject.Save;
                if not result then
                  AError := S_ERROR_CMD_SETBCT;
              end else
                AError := S_ERROR_REQUIRED_PARAMETER_IS_WRONG;
            end else
              AError := S_ERROR_BUILDCONFIG_DOES_NOT_EXIST;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      pmSetBCM:
        begin
          result := ParamStr(4) <> '';
          if result then begin
            aux := AProject.BuildConfigurations.GetBuildConfigurationIndex(ParamStr(3));
            result := aux > -1;
            if result then begin
              result := AProject.BuildConfigurations[aux].SetRealNumbersAsMP2ProjectValue(ParamStr(4));
              if result then begin
                result := AProject.Save;
                if not result then
                  AError := S_ERROR_CMD_SETBCT;
              end else
                AError := S_ERROR_REQUIRED_PARAMETER_IS_WRONG;
            end else
              AError := S_ERROR_BUILDCONFIG_DOES_NOT_EXIST;
          end else
            AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
        end;
      pmSetABC:
        begin
          aux := AProject.BuildConfigurations.GetBuildConfigurationIndex(ParamStr(3));
          result := aux > -1;
          if result then begin
            AProject.BuildConfigurations.ActiveConfigurationIndex := aux;
            result := AProject.Save;
            if not result then
              AError := S_ERROR_CMD_SETABC;
          end else
            AError := S_ERROR_BUILDCONFIG_DOES_NOT_EXIST;
        end;
      end;
    end else
      AError := S_ERROR_REQUIRED_PARAMETER_NOT_PRESENT;
  end else
    AError := S_ERROR_COULD_NOT_LOAD_PROJECT;
end;

var AProject: Tmp3Project; ACommand: Tmp3CLPMCommand; AError: string; ASuccess: boolean;
begin
  ACommand := StringToCLPMCommand(ParamStr(1));

  if ParamCount<2 then
  begin
    ShowUsage(ACommand);
    exit;
  end;

  ASuccess := false;
  AProject := Tmp3Project.Create;
  try
    if ACommand in [pmNew,pmRemove] then
      ASuccess := PerformProjectCommand(AProject,ACommand,AError)
    else if ACommand in [pmSetVersion,pmSetVendor,pmSetIcon] then
      ASuccess := PerformMidletInfoCommand(AProject,ACommand,AError)
    else if ACommand in [pmAddSF,pmDelSF,pmRenSF] then
      ASuccess := PerformSourceFilesCommand(AProject,ACommand,AError)
    else if ACommand in [pmAddRF,pmDelRF,pmRenRF,pmSetRFC] then
      ASuccess := PerformResourceFilesCommand(AProject,ACommand,AError)
    else if ACommand in [pmAddBC,pmDelBC,pmRenBC,pmSetBCT,pmSetBCV,pmSetBCM,pmSetABC] then
      ASuccess := PerformBuildConfigurationsCommand(AProject,ACommand,AError)
    else if ACommand in [pmUnknown] then
      AError := S_ERROR_UNKNOWN_CMD;
  finally
    AProject.Free;
  end;
  if not ASuccess then
    WriteLn(S_ERROR+': '+AError);
end.
