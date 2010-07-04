(*
    MIDletPascal 3.0 Command Line Group Manager
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)

    18-11-2009
*)

library mp3PP;

uses
  Windows,
  FastMM4,
  sitJEDIPascalPreprocessor;

const
  WARNING_PP_GENERATED =
    '{************************************************************************}'#13#10+
    '{     WARNING: This is a MIDletPascal 3 preprocessor generated unit.     }'#13#10+
    '{************************************************************************}'#13#10;

type
  TCompilerMessageHandler = procedure(AMessage: pchar); cdecl;

function preprocess(ARootDirectory, AFilename, AOutputFilename,
  AConditionalDefinesCSV: pchar; ACompilerMessageHandler: DWORD): DWORD; stdcall;
var s: string; cmh: TCompilerMessageHandler;
begin
  result := PreprocessPascalFile(ARootDirectory, AFilename, AOutputFilename,
    AConditionalDefinesCSV, WARNING_PP_GENERATED, s);
  if (result = 0) and (s <> '') then begin
    cmh := TCompilerMessageHandler(ACompilerMessageHandler);
    if assigned(cmh) then
      cmh(pchar(s));
  end;
end;

exports
  preprocess;

end.
