(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3About;

interface

uses
  Forms, Controls,
  Classes, ExtCtrls, jpeg,
  mp3Consts;

type
  Tmp3AboutForm = class(TForm)
    imgAbout: TImage;
    pnlAbout: TPanel;
    procedure imgAboutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ Tmp3AboutForm }

procedure Tmp3AboutForm.imgAboutClick(Sender: TObject);
begin
  Close;
end;

end.
