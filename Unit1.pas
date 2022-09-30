unit Unit1;

interface

{$DEFINE ENABLE_PYTHON}
// {$DEFINE ENABLE_TRAIN}
// {$DEFINE ENABLE_EVOLVE}
// {$DEFINE ENABLE_MOVIE}
{$DEFINE JSONTEST}

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Threading, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.DialogService,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls, PyCommon,
  PyModule, PyPackage,
  {$IF NOT DEFINED(CPUARM)}
  PSUtil,
  {$ENDIF}
  DW.OSDevice,
  FMX.Menus, FMX.Layouts, FMX.Styles,
  EmbeddedForm, PythonEngine;

type
  TfrmMain = class(TForm)
    StyleBook1: TStyleBook;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    StyleLayout: TLayout;
    {$IFDEF ENABLE_TRAIN}
    TrainLayout: TLayout;
    {$ENDIF}
    ChoiceLayout: TLayout;
    OptionsMenu: TMenuItem;
    ExitMenu: TMenuItem;
    DebugMenu: TMenuItem;
    SystemTestMenu: TMenuItem;
    InitLayout: TLayout;
    CalibrateMenu: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    mnuCalibrateStyleGPU: TMenuItem;
    mnuCalibrateStyleCPU: TMenuItem;
    mnuCalibrateTrainGPU: TMenuItem;
    mnuCalibrateTrainCPU: TMenuItem;
    OpenFileMenu: TMenuItem;
    SaveFileMenu: TMenuItem;
    ImportMenu: TMenuItem;
    HelpMenu: TMenuItem;
    BugMenu: TMenuItem;
    PatreonMenu: TMenuItem;
    DiscordMenu: TMenuItem;
    {$IFDEF ENABLE_EVOLVE}
    EvolveLayout: TLayout;
    {$ENDIF}
    {$IFDEF ENABLE_MOVIE}
    MovieLayout: TLayout;
    {$ENDIF}
    procedure FormCreate(Sender: TObject);
    procedure OptionsMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure DebugMenuClick(Sender: TObject);
    procedure SystemTestMenuClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure InstallFinished(Sender: TObject; const AStatus: Boolean);
    procedure CreateEmbeddedForms;
    procedure SystemCalibrateTrainCPU(Sender: TObject);
    procedure SystemCalibrateStyleCPU(Sender: TObject);
    procedure SystemCalibrateTrainGPU(Sender: TObject);
    procedure SystemCalibrateStyleGPU(Sender: TObject);
    procedure mnuCalibrateStyleCPUClick(Sender: TObject);
    procedure mnuCalibrateStyleGPUClick(Sender: TObject);
    procedure mnuCalibrateTrainCPUClick(Sender: TObject);
    procedure mnuCalibrateTrainGPUClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure OpenFileMenuClick(Sender: TObject);
    procedure SaveFileMenuClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure ImportMenuClick(Sender: TObject);
    procedure BugMenuClick(Sender: TObject);
    procedure PatreonMenuClick(Sender: TObject);
    procedure DiscordMenuClick(Sender: TObject);
  private
    { Private declarations }
    FrameCount: Integer;
    function EmbedForm(AParent:TControl; AForm:TEmbeddedForm): TEmbeddedForm;
    procedure ChildCloser(Sender: TObject);
    procedure ChildSwitcher(Sender: TObject);
    procedure SwitchToForm(AForm: TEmbeddedForm);
  public
    { Public declarations }
    ActiveForm: TEmbeddedForm;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Settings,
  StyleModel,
  FunctionLibrary,
  InitializeForm,
  PythonSystem,
  OptionsForm,
  DebugForm,
  ZipLartis,
  ChoiceForm,
  StyleForm,
  {$IFDEF ENABLE_TRAIN}
  TrainForm,
  {$ENDIF}
  {$IFDEF ENABLE_EVOLVE}
  EvolveForm,
  {$ENDIF}
  {$IFDEF ENABLE_MOVIE}
  MovieForm,
  {$ENDIF}
  Math;

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  MenuItem1.Enabled := False;
  FrameCount := 0;
  Caption := AppName; // + ' - ' + AppHome;
  PySys := TPySys.Create(Sender as TComponent);
  frmInit := EmbedForm(InitLayout, TfrmInit.Create(Self)) as TfrmInit;
  frmInit.ShowForm;
  frmInit.OnInstallFinished := InstallFinished;
  ActiveForm := frmInit;
end;

procedure TfrmMain.CreateEmbeddedForms;
begin
  StyleModels := TStyleModels.Create(Self);
  StyleModels.GetAllModels;

  frmStyle := EmbedForm(StyleLayout, TfrmStyle.Create(Self)) as TfrmStyle;
  {$IFDEF ENABLE_TRAIN}
  frmTrain := EmbedForm(TrainLayout, TfrmTrain.Create(Self)) as TfrmTrain;
  {$ENDIF}
  {$IFDEF ENABLE_EVOLVE}
  frmEvolve := EmbedForm(EvolveLayout, TfrmEvolve.Create(Self)) as TfrmEvolve;
  {$ENDIF}
  {$IFDEF ENABLE_MOVIE}
  frmMovie := EmbedForm(MovieLayout, TfrmMovie.Create(Self)) as TfrmMovie;
  {$ENDIF}

  // Must be created last
  frmChoice := EmbedForm(ChoiceLayout, TfrmChoice.Create(Self)) as TfrmChoice;

  {$IFDEF ENABLE_TRAIN}
  frmChoice.ShowForm;
  ActiveForm := frmChoice;
  {$ELSE}
  frmStyle.btnBack.Visible := False;
  frmStyle.ShowForm;
  ActiveForm := frmStyle;
  {$ENDIF}

  MenuItem1.Enabled := True;

end;

procedure TfrmMain.BugMenuClick(Sender: TObject);
begin
  TOSDevice.OpenURL('https://github.com/peardox/Lartis/issues');
end;

procedure TfrmMain.PatreonMenuClick(Sender: TObject);
begin
  TOSDevice.OpenURL('https://www.patreon.com/Peardox');
end;

procedure TfrmMain.DiscordMenuClick(Sender: TObject);
begin
  TOSDevice.OpenURL('https://discord.gg/e2rGEEHrBd');
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

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(StyleModels) then
    FreeAndNil(StyleModels);
  if Assigned(frmStyle) then
    FreeAndNil(frmStyle);
  {$IFDEF ENABLE_TRAIN}
  if Assigned(frmTrain) then
    FreeAndNil(frmTrain);
  {$ENDIF}
  {$IFDEF ENABLE_EVOLVE}
  if Assigned(frmEvolve) then
    FreeAndNil(frmEvolve);
  {$ENDIF}
  {$IFDEF ENABLE_MOVIE}
  if Assigned(frmMovie) then
    FreeAndNil(frmMovie);
  {$ENDIF}
  DestroySettings;
end;

procedure TfrmMain.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  Inc(FrameCount);

//  Caption := AppName + ' - ' + IntToStr(FrameCount);
  if Assigned(ActiveForm) then
    ActiveForm.Paint;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  frmInit.CheckWipe;
end;

procedure TfrmMain.InstallFinished(Sender: TObject; const AStatus: Boolean);
begin
  if AStatus then
    begin
      CreateEmbeddedForms
    end
  else
    begin
      ShowMessage('Please Restart Lartis');
      SystemSettings.PythonInstalled := True;
      MenuItem1.Enabled := True;
    end;
end;

procedure TfrmMain.DebugMenuClick(Sender: TObject);
begin
  frmDebug.Show;
end;

procedure TfrmMain.ExitMenuClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfrmMain.OptionsMenuClick(Sender: TObject);
begin
  frmOptions.ShowModal;
end;

procedure TfrmMain.mnuCalibrateStyleCPUClick(Sender: TObject);
begin
  SystemCalibrateStyleCPU(Self);
end;

procedure TfrmMain.mnuCalibrateStyleGPUClick(Sender: TObject);
begin
  SystemCalibrateStyleGPU(Self);
end;

procedure TfrmMain.mnuCalibrateTrainCPUClick(Sender: TObject);
begin
  SystemCalibrateTrainCPU(Self);
end;

procedure TfrmMain.mnuCalibrateTrainGPUClick(Sender: TObject);
begin
  SystemCalibrateTrainGPU(Self);
end;

procedure TfrmMain.SystemCalibrateStyleGPU(Sender: TObject);
begin
  frmDebug.Show;
  PySys.modCalibrate.CalibrateStyle(True, 16 / 9);
end;

procedure TfrmMain.SystemCalibrateTrainGPU(Sender: TObject);
begin
  frmDebug.Show;
  PySys.modCalibrate.CalibrateTrain(True, 16 / 9);
end;

procedure TfrmMain.SystemCalibrateStyleCPU(Sender: TObject);
begin
  frmDebug.Show;
  PySys.modCalibrate.CalibrateStyle(False, 16 / 9);
end;

procedure TfrmMain.SystemCalibrateTrainCPU(Sender: TObject);
begin
  frmDebug.Show;
  PySys.modCalibrate.CalibrateTrain(False, 16 / 9);
end;

procedure TfrmMain.SystemTestMenuClick(Sender: TObject);
var
  I: Integer;
begin
  frmDebug.Show;

  if Assigned(PySys) and SystemActive then
    begin
      if not PySys.Torch.IsImported then
        Exit;
      var gpu_count: Variant := PySys.Torch.torch.cuda.device_count();
      var mps_available: Variant := PySys.Torch.torch.has_mps;
      PySys.Log('Torch returned gpu_count = ' + gpu_count);
      PySys.Log('Torch returned MPS = ' + mps_available);
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
  {$IF NOT DEFINED(CPUARM)}
      if not PySys.PSUtil.IsImported then
        Exit;
      var cpu_cores: Variant := PySys.PSUtil.psutil.cpu_count(False);
      var cpu_threads: Variant := PySys.PSUtil.psutil.cpu_count(True);
      var cpu_freq: Variant := PySys.PSUtil.psutil.cpu_freq();
      var virtual_memory: Variant := PySys.PSUtil.psutil.virtual_memory();

      PySys.Log('PSUtil returned cpu_cores = ' + cpu_cores);
      PySys.Log('PSUtil returned cpu_threads = ' + cpu_threads);
      PySys.Log('PSUtil returned cpu_freq = ' + cpu_freq.current);
      PySys.Log('PSUtil returned total_memory = ' + virtual_memory.total);
      PySys.Log('PSUtil returned available_memory = ' + virtual_memory.available);
  {$ENDIF}
      end;
end;

procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  CalibrateMenu.Enabled := False;
  OpenFileMenu.Enabled := False;
  SaveFileMenu.Enabled := False;
  OptionsMenu.Enabled := False;
  DebugMenu.Enabled := False;
  SystemTestMenu.Enabled := False;
  ImportMenu.Enabled := False;

  if ActiveForm = frmStyle then
    begin
      CalibrateMenu.Enabled := True;
      OpenFileMenu.Enabled := True;
      SaveFileMenu.Enabled := True;
      ImportMenu.Enabled := True;
      OptionsMenu.Enabled := True;
      DebugMenu.Enabled := True;
      SystemTestMenu.Enabled := True;
    end;

end;

procedure TfrmMain.OpenFileMenuClick(Sender: TObject);
begin
  if ActiveForm = frmStyle then
    frmStyle.AddStyleLayer;
end;

procedure TfrmMain.SaveFileMenuClick(Sender: TObject);
begin
  if ActiveForm = frmStyle then
    frmStyle.SaveStyleImage;
end;

procedure TfrmMain.ImportMenuClick(Sender: TObject);
var
  mr: TModalResult;
begin
  mr := ZipExtractForm.ShowModal;
  if mr = mrOK then
    begin
      FreeAndNil(StyleModels);
      StyleModels := TStyleModels.Create(Self);
      StyleModels.GetAllModels;
      frmStyle.MakeStyleSelectors;
    end;
end;

end.
