unit TrainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation;

type
  TfrmTrain = class(TEmbeddedForm)
    Panel1: TPanel;
    Button1: TButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    Button2: TButton;
    OpenTrainingImageDialog: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure TrackToScale;
    procedure TrackBar1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmTrain: TfrmTrain;

function ScalePower(const n: Single): Single;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  PythonSystem,
  Math;


function ScalePower(const n: Single): Single;
var
  base: Single;
  power10: Single;
begin
  base := ((n / 4) - floor(n / 4)) * 10;
  if base = 0 then
    base := 1;
  power10 := floor(n / 4) + 8;
  PySys.Log('Base = ' + FloatToStr(base) + ', Power = ' + FloatToStr(power10));
  Result := base * Power(10, power10);
end;

procedure TfrmTrain.TrackToScale;
var
  DispWeight: Single;
begin
  DispWeight := ScalePower(TrackBar1.Value);
  Label1.Text := FormatFloat('0.0E+', DispWeight);
end;

procedure TfrmTrain.Button2Click(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
      OpenTrainingImageDialog.Filter:='Lartis Style Archives (*.pth)|*.pth';
      if OpenTrainingImageDialog.Execute then
        begin
          PySys.modTrain.Train(OpenTrainingImageDialog.Filename,
          System.IOUtils.TPath.GetFileNameWithoutExtension(OpenTrainingImageDialog.Filename),
          ScalePower(TrackBar1.Value));
        end;
    end;
end;

procedure TfrmTrain.TrackBar1Change(Sender: TObject);
begin
  TrackToScale;
end;

procedure TfrmTrain.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

end.
