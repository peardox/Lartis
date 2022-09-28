unit ErrorForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TfrmErrorDialog = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure FormReset;
  public
    { Public declarations }
    procedure ErrorMode(const AErrorMode: String);
  end;

var
  frmErrorDialog: TfrmErrorDialog;

implementation

{$R *.fmx}

procedure TfrmErrorDialog.Button1Click(Sender: TObject);
begin
  ModalResult := mrRetry;
end;

procedure TfrmErrorDialog.Button2Click(Sender: TObject);
begin
  ModalResult := mrAbort;
end;

procedure TfrmErrorDialog.ErrorMode(const AErrorMode: String);
begin
  FormReset;
  if AErrorMode = 'ERR_GPU_OOM' then
    begin
      Label1.Text := 'This style appears to have used up all the GPU memory' + sLineBreak
       + sLineBreak
       + 'You can either try running it with the CPU or Abort the style' + sLineBreak
       + sLineBreak
       + 'If you select [Use CPU] then the GPU will be ignored for'
       + 'the retry (render will be slow but it should finish)';
      Button1.Text := 'Use CPU';
      Button2.Text := 'Abort';
    end;

end;

procedure TfrmErrorDialog.FormReset;
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  ModalResult := mrNone;
end;


end.
