(*
    MIDletPascal 3.x Preprocessor
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

library mp3PP;

uses
  Windows,
  FastMM4,
  sitJEDIPascalPreprocessor;

type
  TCompilerMessageHandler = procedure(AMessage: pchar); cdecl;

function preprocess(ARootDirectory, AFilename, AOutputFilename,
  AConditionalDefinesCSV: pchar; ACompilerMessageHandler: DWORD): DWORD; stdcall;
var s, w: string; cmh: TCompilerMessageHandler;
begin
  w := '(*--------------------------------------------------------------------------'#13#10+
    '  WARNING'#13#10+
    '  This is a MIDletPascal 3 preprocessor generated unit.'#13#10+
    #13#10+
    '  CONDITIONAL DEFINES'#13#10+
    '  '+ AConditionalDefinesCSV +#13#10+
    '  --------------------------------------------------------------------------*)'#13#10;

  result := PreprocessPascalFile(ARootDirectory, AFilename, AOutputFilename,
    AConditionalDefinesCSV, pchar(w), s);
  if (result = 0) and (s <> '') then begin
    cmh := TCompilerMessageHandler(ACompilerMessageHandler);
    if assigned(cmh) then
      cmh(pchar(s));
  end;
end;

exports
  preprocess;

end.
