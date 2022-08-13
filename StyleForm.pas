unit StyleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmStyle = class(TEmbeddedForm)
    Text1: TText;
    Panel1: TPanel;
    Button1: TButton;
    procedure Text1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStyle: TfrmStyle;

implementation

{$R *.fmx}

procedure TfrmStyle.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

procedure TfrmStyle.Text1Click(Sender: TObject);
begin
  Text1.Text := 'Click';
end;

end.
