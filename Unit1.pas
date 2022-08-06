unit Unit1;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Threading, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, PyCommon,
  PyModule, PyPackage, PSUtil, CV2_Contrib;

type
  TfrmMain = class(TForm)
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
    procedure Log(const AMsg: String);
    procedure TrackToScale;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math,
  PythonSystem,
  SetupForm;

{$R *.fmx}

procedure TfrmMain.Log(const AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

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

procedure TfrmMain.TrackToScale;
var
  DispWeight: Single;
begin
  DispWeight := ScalePower(TrackBar1.Value);
  Label1.Text := FormatFloat('0.0E+', DispWeight);
end;

procedure TfrmMain.TrackBar1Change(Sender: TObject);
begin
  TrackToScale;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          PySys.modStyle.Stylize(OpenDialog1.Filename);
        end;
    end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          PySys.modTrain.Train(OpenDialog1.Filename,
          System.IOUtils.TPath.GetFileNameWithoutExtension(OpenDialog1.Filename),
          ScalePower(TrackBar1.Value));
        end;
    end;
end;

procedure TfrmMain.FormActivate(Sender: TObject);
begin
{
  if not SystemActive then
    frmSetup.BringToFront;
}
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  PyCleanOnExit := CheckBox1.IsChecked;
  if Assigned(PySys) then
    PySys.ShutdownSystem;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
//  Application.CreateForm(TfrmSetup, frmSetup);
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
{
  frmSetup.Show;
  // frmSetup.Parent := frmMain;
  if not SystemActive then
    frmSetup.BringToFront;
}
//  TrackToScale;
  PySys := TPySys.Create(Application); // Self as TComponent);
  PySys.LogTarget := Memo1.Lines;
  PySys.SetupSystem;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
//      var t: Variant  := PySys.Torch.torch.cuda.get_device_name(0);
      var t: Variant := PySys.PSUtil.psutil.cpu_count();
      var m: Variant := PySys.PSUtil.psutil.virtual_memory();
      PySys.Log('api returned ' + t);
      PySys.Log('api returned ' + m.total);

{
      GetAllModels;
      for var I := 0 to ModelList.Count - 1 do
        Log(ModelList[I]);
}
      end;
end;

end.
