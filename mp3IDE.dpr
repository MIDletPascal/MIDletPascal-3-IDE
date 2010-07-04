(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

program mp3IDE;

uses
  FastMM4,
  mp3Core in 'mp3Core.pas';

{$R *.res}

begin
  with Tmp3Core.Create do
  try
    Run;
  finally
    Free;
  end;
end.
