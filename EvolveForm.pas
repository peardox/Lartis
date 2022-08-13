unit EvolveForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmEvolve = class(TEmbeddedForm)
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
  frmEvolve: TfrmEvolve;

implementation

{$R *.fmx}

procedure TfrmEvolve.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

procedure TfrmEvolve.Text1Click(Sender: TObject);
begin
  Text1.Text := 'Darwin';
end;

end.
