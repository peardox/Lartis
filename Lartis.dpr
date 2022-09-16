program Lartis;

uses
  FMX.Forms,
  FMX.Types,
  FMX.Styles,
  System.UITypes,
  Skia,
  Skia.FMX,
  Unit1 in 'Unit1.pas' {frmMain},
  PythonSystem in 'PythonSystem.pas',
  Modules in 'Modules.pas',
  SetupForm in 'SetupForm.pas' {frmSetup},
  OptionsForm in 'OptionsForm.pas' {frmOptions},
  StyleForm in 'StyleForm.pas' {frmStyle},
  TrainForm in 'TrainForm.pas' {frmTrain},
  EmbeddedForm in 'EmbeddedForm.pas',
  ChoiceForm in 'ChoiceForm.pas' {frmChoice},
  EvolveForm in 'EvolveForm.pas' {frmEvolve},
  MovieForm in 'MovieForm.pas' {frmMovie},
  FunctionLibrary in 'FunctionLibrary.pas',
  Shaders in 'Shaders.pas',
  Settings in 'Settings.pas',
  DebugForm in 'DebugForm.pas' {frmDebug},
  StyleModel in 'StyleModel.pas',
  StyleProject in 'StyleProject.pas',
  InitializeForm in 'InitializeForm.pas' {frmInit},
  Downloader in 'Downloader.pas',
  LartisTypes in 'LartisTypes.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
//  ReportMemoryLeaksOnShutdown := True;
  //For macOS/iOS
  GlobalUseMetal := True;

  // GPU is priorty everywhere but Windows,
  // this line improves Windows shader performance
  GlobalUseSkiaRasterWhenAvailable := False;

  Application.Initialize;
  CreateSettings;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.Run;
end.
