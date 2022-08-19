unit DebugForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TfrmDebug = class(TForm)
    Layout1: TLayout;
    btnClose: TButton;
    mmDebug: TMemo;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDebug: TfrmDebug;

implementation

uses
  PythonSystem;

{$R *.fmx}

procedure TfrmDebug.btnCloseClick(Sender: TObject);
begin
  PySys.LogTarget := Nil;
  Close;
end;

procedure TfrmDebug.FormShow(Sender: TObject);
begin
  mmDebug.Lines.Clear;
  PySys.LogTarget := mmDebug;
end;

end.
