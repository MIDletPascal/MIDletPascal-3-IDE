(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3FileKind;

interface

uses
  SysUtils,
  mp3Consts;

type
  Tmp3FileKind = (fkGroup, fkProject, fkSourceFile, fkImage, fkOther);

function RetrieveFileKind(AFilename: string): Tmp3FileKind;

implementation

function RetrieveFileKind(AFilename: string): Tmp3FileKind;
var ext: string;
begin
  result := fkOther;
  ext := ExtractFileExt(AFilename);
  if SameText(ext, EXTENSION_GROUP) then
    result := fkGroup
  else if SameText(ext, EXTENSION_PROJECT) then
    result := fkProject
  else if SameText(ext, EXTENSION_SOURCEFILE) or
    SameText(ext, EXTENSION_PAS) or
    SameText(ext, EXTENSION_PP) or
    SameText(ext, EXTENSION_P) or
    SameText(ext, EXTENSION_DPR) or
    SameText(ext, EXTENSION_LPR) or
    SameText(ext, EXTENSION_INC) then
    result := fkSourceFile
  else if SameText(ext, EXTENSION_PNG) then
    result := fkImage;
end;

end.