unit SetupForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TfrmSetup = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    SetupRunning: Boolean;
    {$IFDEF SPLASH}
    procedure LoadMainForm;
    {$ENDIF}
    procedure SetupComplete(Sender: TObject; const AStatus: Boolean);
  public
    { Public declarations }
  end;

var
  frmSetup: TfrmSetup;

implementation

uses
  Settings,
  Math,
  PythonSystem;

{$R *.fmx}

procedure TfrmSetup.FormCreate(Sender: TObject);
begin
    SetupRunning := False;
    SystemActive := False;
end;

procedure TfrmSetup.FormShow(Sender: TObject);
begin
  if not SetupRunning and not SystemActive then
    begin
      SetupRunning := True;
      PySys := TPySys.Create(Sender as TComponent);
      PySys.LogTarget := Memo1;
      PySys.SetupSystem(SetupComplete);
    end;
end;

procedure TfrmSetup.SetupComplete(Sender: TObject; const AStatus: Boolean);
begin
  Close;
end;

{$IFDEF SPLASH}
procedure TfrmSetup.LoadMainForm;
var
  form: TForm;
begin
//  form := TMainForm.Create(Application);
  form.Show;
  Application.MainForm := form;
  Close;
end;
{$ENDIF}

end.
