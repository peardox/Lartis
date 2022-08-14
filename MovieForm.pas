unit MovieForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation;
type
  TfrmMovie = class(TEmbeddedForm)
    Text1: TText;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Text1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMovie: TfrmMovie;

implementation

{$R *.fmx}

procedure TfrmMovie.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

procedure TfrmMovie.Text1Click(Sender: TObject);
begin
  Text1.Text := 'Lights, Camera, Action';
end;

end.
