unit SetupForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TfrmSetup = class(TForm)
    Memo1: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    SetupRunning: Boolean;
  public
    { Public declarations }
  end;

var
  frmSetup: TfrmSetup;

implementation

uses
  PythonSystem;

{$R *.fmx}

procedure TfrmSetup.FormActivate(Sender: TObject);
begin
  if not SystemActive then
    frmSetup.BringToFront;
end;

procedure TfrmSetup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//  Action := TCloseAction.caFree;
//  Parent := Nil;
end;

procedure TfrmSetup.FormCreate(Sender: TObject);
begin
    SetupRunning := False;
    SystemActive := False;
end;

procedure TfrmSetup.FormShow(Sender: TObject);
begin
  Memo1.Lines.Add('Show');
  if not SetupRunning and not SystemActive then
    begin
      SetupRunning := True;
      Memo1.Lines.Add('Setting Up');
      PySys := TPySys.Create(Sender as TComponent);
      PySys.LogTarget := Memo1.Lines;
      PySys.SetupSystem;
      BringToFront;
    end;
end;

end.
