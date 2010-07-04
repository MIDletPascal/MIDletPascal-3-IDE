(*
    MIDletPascal 3.0 IDE
    by Javier Santo Domingo (j-a-s-d@users.sourceforge.net)
*)

unit mp3MIDletInfo;

interface

type
  Tmp3MIDletInfo = class
  private
    FName: string;
    FVendor: string;
    FVersion: string;
    FIcon: string;
  public
    procedure Clear;
    property Name: string read FName write FName;
    property Vendor: string read FVendor write FVendor;
    property Version: string read FVersion write FVersion;
    property Icon: string read FIcon write FIcon;
  end;
  
implementation

{ Tmp3MIDletInfo }

procedure Tmp3MIDletInfo.Clear;
begin
  FName := '';
  FVendor := '';
  FVersion := '';
  FIcon := '';
end;

end.