(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3Preprocessor;

interface

uses
  Windows,
  mp3Consts;

type
  TPreprocessFunction = function(src_directory, source_file, output_file,
    conditional_defines_csv: pchar; compiler_message_handler: DWORD): DWORD; stdcall;

  Tmp3Preprocessor = class
  strict private
    class var hPreprocessorDLL: THandle;
    class var fPreprocess: TPreprocessFunction;
  private
    class procedure Load;
    class procedure Unload;
  public
    class function Present: boolean;
    class function Run(ASourceDirectory, ASourceFile, AOutputFile,
      AConditionalDefinesCSV: string; ACcompilerMessageHandler: Pointer): DWORD;
  end;

implementation

class procedure Tmp3Preprocessor.Load;
begin
  hPreprocessorDLL := LoadLibrary(pchar(PREPROCESSOR_DLL));
  if hPreprocessorDLL <> 0 then
    @fPreprocess := GetProcAddress(hPreprocessorDLL,
      pchar(PREPROCESSOR_PREPROCESS_FUNCTION))
  else
    @fPreprocess := nil;
end;

class procedure Tmp3Preprocessor.Unload;
begin
  FreeLibrary(hPreprocessorDLL);
end;

class function Tmp3Preprocessor.Present: boolean;
begin
  result := (hPreprocessorDLL<>0) and assigned(fPreProcess);
end;

class function Tmp3Preprocessor.Run(ASourceDirectory, ASourceFile, AOutputFile,
      AConditionalDefinesCSV: string; ACcompilerMessageHandler: Pointer): DWORD;
begin
  result := fPreprocess(pchar(ASourceDirectory), pchar(ASourceFile), pchar(AOutputFile),
      pchar(AConditionalDefinesCSV), DWORD(ACcompilerMessageHandler));
end;

initialization
  Tmp3Preprocessor.Load;
finalization
  Tmp3Preprocessor.Unload;
end.
