(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3About;

interface

uses
  Forms, Controls, StdCtrls, Classes,
  ExtCtrls, Graphics, jpeg, ShellAPI,
  tuiHTMLControls,
  mp3Consts,
  gnugettext;

type
  Tmp3AboutForm = class(TForm)
    imgAbout: TImage;
    pnlAbout: TPanel;
    procedure imgAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCompilerLabel: TLabel;
    FIDELabel: TLabel;
    FTranslationsLabel: TLabel;
    FOtherLabel: TLabel;
    FHtmlDisplay: TtuiHtmlViewer;
    procedure SelectLabel(ALabel: TLabel);
    procedure OnCompilerLabelClick(Sender: TObject);
    procedure OnIDELabelClick(Sender: TObject);
    procedure OnTranslationsLabelClick(Sender: TObject);
    procedure OnOtherLabelClick(Sender: TObject);
    procedure OnHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
  end;

implementation

{$R *.dfm}

{ Tmp3AboutForm }

procedure Tmp3AboutForm.FormCreate(Sender: TObject);

  function NewLabel(ACaption: widestring; ALeft: integer; AOnClick: TNotifyEvent): TLabel;
  begin
    result := TLabel.Create(Self);
    result.Parent := pnlAbout;
    result.Cursor := crHandPoint;
    result.Top := FHtmlDisplay.Top - 20;
    result.Font.Size := 10;
    result.Font.Style := [fsBold];
    result.Transparent := true;
    result.Caption := ACaption;
    result.Left := ALeft;
    result.OnClick := AOnClick;
  end;

begin
  FHtmlDisplay := TtuiHtmlViewer.Create(Self);
  FHtmlDisplay.Parent := pnlAbout;
  FHtmlDisplay.SetBounds(66, 200, 512, 208);
  FHtmlDisplay.DefBackground := $20D0FE;
  FHtmlDisplay.DefFontColor := $000000;
  FHtmlDisplay.DefFontSize := 8;
  FHtmlDisplay.DefFontName := 'Tahoma';
  FHtmlDisplay.OnHotSpotClick := OnHotSpotClick;
  FCompilerLabel := NewLabel(
    _('Compiler'), FHtmlDisplay.Left, OnCompilerLabelClick
  );
  FIDELabel := NewLabel(
    _('IDE'), FHtmlDisplay.Left + 96, OnIDELabelClick
  );
  FTranslationsLabel := NewLabel(
    _('Translations'), FHtmlDisplay.Left + 360, OnTranslationsLabelClick
  );
  FOtherLabel := NewLabel(
    _('Other'), FHtmlDisplay.Left + 468, OnOtherLabelClick
  );
  OnCompilerLabelClick(FCompilerLabel);
end;

procedure Tmp3AboutForm.OnHotSpotClick(Sender: TObject; const SRC: string; var Handled: Boolean);
begin
  ShellExecute(0,'open',pchar(SRC),'','',1);
end;

procedure Tmp3AboutForm.imgAboutClick(Sender: TObject);
begin
  Close;
end;

procedure Tmp3AboutForm.OnCompilerLabelClick(Sender: TObject);
begin
  SelectLabel(TLabel(Sender));
  FHtmlDisplay.RenderHTML(
    '<div bgcolor="#FED020" align="center"><font color="#444444">'+
    '<b>MIDletPascal compiler</b> [license: GPL]<br>'+
    '</font><font color="#555555">'+
    'Copyright (c) 2004-2006 Mobile Experts Group / 2009-2011 MIDletPascal project<br>'+
    '<br>'+
    '<b>3.0-3.4</b><br><i>enhancements/maintenance</i><br>Javier Santo Domingo ( j-a-s-d )<br><br>'+
    '<b>3.0</b><br><i>enhancements</i><br>Artem ( abcdef )<br><br>'+
    '<b>1.0-2.0</b><br><i>original development</i><br>Niksa Orlic ( norlic )'+
    '</font></div>');
end;

procedure Tmp3AboutForm.OnIDELabelClick(Sender: TObject);
begin
  SelectLabel(TLabel(Sender));
  FHtmlDisplay.RenderHTML(
    '<div bgcolor="#FED020" align="center"><font color="#444444">'+
    '<b>MIDletPascal 3.x IDE</b> [license: BSD]<br>'+
    '</font><font color="#555555">'+
    'Copyright (c) 2010-2011 Javier Santo Domingo ( j-a-s-d )<br>'+
    '<br>'+
    'For more information about MP3IDE visit:'+
    '<a href="http://coderesearchlabs.com/mp3ide">http://coderesearchlabs.com/mp3ide</a>'+
    '</font></div>');
end;

procedure Tmp3AboutForm.OnTranslationsLabelClick(Sender: TObject);
begin
  SelectLabel(TLabel(Sender));
  FHtmlDisplay.RenderHTML(
    '<div bgcolor="#FED020" align="center"><font color="#444444">'+
    '<b>MIDletPascal 3.x translations</b><br>'+
    '<br>'+
    '</font><font color="#555555">'+
    '<b><i>russian</i></b><br>'+
    'Sergey Naydenov ( tronix286 )<br>'+
    '<br>'+
    '<b><i>polish</i></b><br>'+
    'Adam Perek ( adamm0 )<br>'+
    '<br>'+
    '<b><i>hungarian</i></b><br>'+
    'Péter Gábor ( ptrg )<br>'+
    '<br>'+
    '<b><i>french</i></b><br>'+
    'François Jouen ( ldci )<br>'+
    '</font></div>');
end;

procedure Tmp3AboutForm.OnOtherLabelClick(Sender: TObject);
begin
  SelectLabel(TLabel(Sender));
  FHtmlDisplay.RenderHTML(
    '<div bgcolor="#FED020" align="center"><font color="#444444">'+
    '<b>MIDletPascal 3.x quality assurance, forum support, etc</b><br>'+
    '<br>'+
    '</font><font color="#555555">'+
    'Wes Williams ( wesw )<br>'+
    'K. W. Yan ( kwyan )<br>'+
    'Prof. Yaffle ( professoryaffle )<br>'+
    'Cepreu H. ( cepreuh )<br>'+
    'Vadim ( yellowafterlife )<br>'+
    '<br>'+
    'and many others at the <a href="http://sourceforge.net/projects/midletpascal/forums/forum/1013750">project''s forum</a><br>'+
    '</font></div>');
end;

procedure Tmp3AboutForm.SelectLabel(ALabel: TLabel);
begin
  FCompilerLabel.Font.Color := clTeal;
  FIDELabel.Font.Color := clTeal;
  FTranslationsLabel.Font.Color := clTeal;
  FOtherLabel.Font.Color := clTeal;
  ALabel.Font.Color := clBlack;
end;

end.
