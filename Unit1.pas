unit Unit1;

interface

{$DEFINE ENABLE_PYTHON}
// {$DEFINE ENABLE_EVOLVE}
// {$DEFINE ENABLE_MOVIE}

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Threading, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, PyCommon,
  PyModule, PyPackage, PSUtil, FMX.Menus, FMX.Layouts,
  EmbeddedForm;

type
  TfrmMain = class(TForm)
    StatusBar1: TStatusBar;
    CheckBox1: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    StyleLayout: TLayout;
    TrainLayout: TLayout;
    ChoiceLayout: TLayout;
    OptionsMenu: TMenuItem;
    ExitMenu: TMenuItem;
    {$IFDEF ENABLE_EVOLVE}
    EvolveLayout: TLayout;
    {$ENDIF}
    {$IFDEF ENABLE_MOVIE}
    MovieLayout: TLayout;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure OptionsMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
  private
    { Private declarations }
    function EmbedForm(AParent:TControl; AForm:TEmbeddedForm): TEmbeddedForm;
    procedure ChildCloser(Sender: TObject);
    procedure ChildSwitcher(Sender: TObject);
    procedure SwitchToForm(AForm: TEmbeddedForm);
    procedure TrackToScale;
  public
    { Public declarations }
    ActiveForm: TEmbeddedForm;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Settings,
  Math,
  PythonSystem,
  OptionsForm,
  ChoiceForm,
  StyleForm,
  TrainForm,
  {$IFDEF ENABLE_EVOLVE}
  EvolveForm,
  {$ENDIF}
  {$IFDEF ENABLE_MOVIE}
  MovieForm,
  {$ENDIF}
  SetupForm;

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

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

procedure TfrmMain.ChildCloser(Sender: TObject);
begin
  SwitchToForm(frmChoice);
end;

procedure TfrmMain.ChildSwitcher(Sender: TObject);
begin
  SwitchToForm(TEmbeddedForm(Sender));
end;

procedure TfrmMain.SwitchToForm(AForm: TEmbeddedForm);
begin
  if Assigned(ActiveForm) then
    begin
      ActiveForm.HideForm;
      ActiveForm := Nil;
    end;
  if Assigned(AForm) then
    begin
      ActiveForm := AForm;
      ActiveForm.ShowForm;
    end;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
ShowMessage('Button3Click');
{
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          PySys.modTrain.Train(OpenDialog1.Filename,
          System.IOUtils.TPath.GetFileNameWithoutExtension(OpenDialog1.Filename),
          ScalePower(TrackBar1.Value));
        end;
    end;
}
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

function TfrmMain.EmbedForm(AParent:TControl; AForm: TEmbeddedForm): TEmbeddedForm;
begin
  while AForm.ChildrenCount>0 do
    AForm.Children[0].Parent:=AParent;

  TLayout(AParent).Align := TAlignLayout.Client;

  TEmbeddedForm(AForm).ParentLayout := AParent;
  TEmbeddedForm(AForm).HideForm;
  TEmbeddedForm(AForm).CloseMyself := ChildCloser;
  TEmbeddedForm(AForm).SwitchToOther := ChildSwitcher;

  Result := AForm;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
//  StatusBar1.Visible := False;

  frmStyle := EmbedForm(StyleLayout, TfrmStyle.Create(Self)) as TfrmStyle;
  frmTrain := EmbedForm(TrainLayout, TfrmTrain.Create(Self)) as TfrmTrain;
  {$IFDEF ENABLE_EVOLVE}
  frmEvolve := EmbedForm(EvolveLayout, TfrmEvolve.Create(Self)) as TfrmEvolve;
  {$ENDIF}
  {$IFDEF ENABLE_MOVIE}
  frmMovie := EmbedForm(MovieLayout, TfrmMovie.Create(Self)) as TfrmMovie;
  {$ENDIF}

  // Must be created last
  frmChoice := EmbedForm(ChoiceLayout, TfrmChoice.Create(Self)) as TfrmChoice;

  frmChoice.ShowForm;
  ActiveForm := frmChoice;

  {$IFDEF ENABLE_PYTHON}
  frmSetup := TfrmSetup.Create(Self);
  frmSetup.Parent := Self;
  frmSetup.Show;
  Caption := appname;
  {$ENDIF}
end;

procedure TfrmMain.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.OptionsMenuClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
var
  I: Integer;
begin
 ShowMessage('Button1Click');
 Exit;

 if Assigned(PySys) then
    begin
//    {$IFDEF MSWINDOWS}
      var gpu_count: Variant := PySys.Torch.torch.cuda.device_count();
      var cpu_cores: Variant := PySys.PSUtil.psutil.cpu_count(False);
      var cpu_threads: Variant := PySys.PSUtil.psutil.cpu_count(True);
      var cpu_freq: Variant := PySys.PSUtil.psutil.cpu_freq();
      var virtual_memory: Variant := PySys.PSUtil.psutil.virtual_memory();

      PySys.Log('Torch returned gpu_count = ' + gpu_count);
      if gpu_count > 0 then
        begin
          for I := 0 to gpu_count - 1 do
            begin
              var gpu_props: Variant := PySys.Torch.torch.cuda.get_device_properties(i);

              PySys.Log('Torch returned Name = ' + gpu_props.name);
              PySys.Log('Torch returned CudaMajor = ' + gpu_props.major);
              PySys.Log('Torch returned CudaMajor = ' + gpu_props.minor);
              PySys.Log('Torch returned Memory = ' + gpu_props.total_memory);
              PySys.Log('Torch returned CUs = ' + gpu_props.multi_processor_count);
            end;
        end;
      PySys.Log('PSUtil returned cpu_cores = ' + cpu_cores);
      PySys.Log('PSUtil returned cpu_threads = ' + cpu_threads);
      PySys.Log('PSUtil returned cpu_freq = ' + cpu_freq.current);
      PySys.Log('PSUtil returned total_memory = ' + virtual_memory.total);
      PySys.Log('PSUtil returned available_memory = ' + virtual_memory.available);
//    {$ENDIF}
{
      GetAllModels;
      for var I := 0 to ModelList.Count - 1 do
        Log(ModelList[I]);
}
      end;
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

end.
