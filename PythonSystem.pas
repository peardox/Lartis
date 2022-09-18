unit PythonSystem;

{$DEFINE USESAFEMASK}

interface
uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Threading, PyEnvironment,
  FMX.Types, FMX.Dialogs, FMX.Memo, FMX.Forms, Math,
  {$IF DEFINED(MACOS64)}
  PyEnvironment.Local,
  {$ELSE}
  PyEnvironment.Embeddable, PyEnvironment.Embeddable.Res,
  PyEnvironment.Embeddable.Res.Python39,
  {$ENDIF}
  PyEnvironment.AddOn, PyEnvironment.AddOn.EnsurePip, PyEnvironment.Distribution,
  PyCommon, PyModule, PyPackage, PythonEngine,
  TorchVision, PyTorch, NumPy, SciPy,
  {$IF NOT DEFINED(CPUARM)}
  PSUtil,
  {$ENDIF}
  Pillow,
  Modules;

type
  TPySysFinishedEvent = procedure(Sender: TObject; const AStatus: Boolean) of object;

  TPySys = class(TComponent)
  private
    { Private declarations }
    Installing: Boolean;
    SystemCode: TStringList;
    SystemError: Boolean;
    PyCleanOnExit: Boolean;
    PyIsReactivating: Boolean;
    PyPackagesInstalled: Integer;
    LastShimPath: String;
    FTask: ITask;

    PyEng: TPythonEngine;
    {$IF DEFINED(MACOS64)}
    PyEnv: TPyLocalEnvironment;
    {$ELSE}
    PyEnv: TPyEmbeddedResEnvironment39;
    {$ENDIF}
    PyIO: TPythonInputOutput;
    PyEnsurePip: TPyEnvironmentAddOnEnsurePip;


    NumPy: TNumPy;
    SciPy: TSciPy;
    Pillow: TPillow;
    TorchVision: TTorchVision;

    FPySysFinishedEvent: TPySysFinishedEvent;
    FLogTarget: TMemo;

    procedure DoPySysFinishedEvent(const AStatus: Boolean);

    procedure PyEngBeforeUnload(Sender: TObject);

    procedure PackageBeforeInstall(Sender: TObject);
    procedure PackageAfterInstall(Sender: TObject);
    procedure PackageInstallError(Sender: TObject; AErrorMessage: string);
    procedure PackageAfterImport(Sender: TObject);
    procedure PackageBeforeImport(Sender: TObject);
    procedure PackageBeforeUnInstall(Sender: TObject);
    procedure PackageAfterUnInstall(Sender: TObject);
    procedure PackageUnInstallError(Sender: TObject; AErrorMessage: string);
    procedure AddExtraUrl(APackage: TPyManagedPackage; const AUrl: string);
    procedure PyEnvAfterDeactivate(Sender: TObject; const APythonVersion: string);
    procedure PyIOSendUniData(Sender: TObject; const Data: string);

    procedure EnurePipError(const ASender: TObject; const ADistribution: TPyDistribution; const AException: Exception);

    procedure SafeInstall(Sender: TObject; FTask: ITask = Nil);
    procedure SafeImport(Sender: TObject);

    procedure DoReady(Sender: TObject; const APythonVersion: string);
    procedure ReActivate;
    procedure SetLogTarget(AStringContainer: TMemo);
    procedure SetupPackage(APackage: TPyManagedPackage; const AExtraURL: String = '');
    function RunShim(const ShimPath: String): Boolean;
    procedure ThreadedSetup;
  public
    Torch: TPyTorch;
    {$IF NOT DEFINED(CPUARM)}
    PSUtil: TPSUtil;
    {$ENDIF}

    modStyle: TModStyle;
    modTrain: TModTrain;
    modPyIO: TModPyIO;
    modCalibrate: TModCalibration;

    property LogTarget: TMemo read FLogTarget write SetLogTarget;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupSystem(OnSetupComplete: TPySysFinishedEvent = Nil; const HaveGPU: Boolean = False; const InstallationActive: Boolean = False);
    function RunTest: Boolean;
    function RunSystem: Boolean;
    procedure ShutdownSystem;
    procedure Log(const AMsg: String; const SameLine: Boolean = False);
    procedure LogClear;
  published
    property PySysFinishedEvent: TPySysFinishedEvent read FPySysFinishedEvent write FPySysFinishedEvent;
  end;

var
  PySys: TPySys;
  FPUMASK: TArithmeticExceptionMask;

function EscapeBackslashForPython(const AStr: String): String;
procedure SafeMaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean = True);

implementation

uses
  Settings,
  Unit1,
  SetupForm,
  {$IFDEF MESSAGE_SUPPORT}
  FMX.Dialogs,
  {$ENDIF}
  PyPackage.Manager.Pip,
  PyPackage.Manager.Defs.Pip;

procedure SafeMaskFPUExceptions(ExceptionsMasked : boolean;
  MatchPythonPrecision : Boolean);
begin
  {$IFDEF USESAFEMASK}
  {$IF Defined(CPUX86) or Defined(CPUX64)}
  if ExceptionsMasked then
    begin
    FPUMASK := GetExceptionMask;
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide,
      exOverflow, exUnderflow, exPrecision]);
    end
  else
    SetExceptionMask(FPUMASK);
  {$WARN SYMBOL_PLATFORM OFF}
  {$IF Defined(FPC) or Defined(MSWINDOWS)}
  if MatchPythonPrecision then
      SetPrecisionMode(pmDouble)
    else
      SetPrecisionMode(pmExtended);
  {$WARN SYMBOL_PLATFORM ON}
  {$IFEND}
  {$IFEND}
  {$ELSE}
    MaskFPUExceptions(ExceptionsMasked, MatchPythonPrecision);
  {$IFEND}
end;

function EscapeBackslashForPython(const AStr: String): String;
begin
  Result := StringReplace(AStr, '\', '\\', [rfIgnoreCase, rfReplaceAll]);
end;

destructor TPySys.Destroy;
begin
  if Assigned(SystemCode) then
    FreeAndNil(SystemCode);

  modStyle.Free;
  modTrain.Free;
  modPyIO.Free;
  modCalibrate.Free;

  Torch.Free;
  {$IF NOT DEFINED(CPUARM)}
  PSUtil.Free;
  {$ENDIF}
  NumPy.Free;
  SciPy.Free;
  TorchVision.Free;

  PyEng.Free;
  PyEnv.Free;
  PyIO.Free;

  inherited;
end;

constructor TPySys.Create(AOwner: TComponent);
begin
  inherited;
  SystemError := False;
  SystemActive := False;
  PyIsReactivating := False;
  PyCleanOnExit := False;
  // Wipe Python when finished
  if not DirectoryExists(IncludeTrailingPathDelimiter(AppHome) + pyshim) then
    ForceDirectories(IncludeTrailingPathDelimiter(AppHome) + pyshim);
  if FileExists(IncludeTrailingPathDelimiter(AppHome) + pycode) then
    begin
      SystemCode := TStringList.Create;
      SystemCode.LoadFromFile(IncludeTrailingPathDelimiter(AppHome) + pycode)
    end;
end;

procedure TPySys.DoPySysFinishedEvent(const AStatus: Boolean);
begin
  if Assigned(FPySysFinishedEvent) then
    FPySysFinishedEvent(Self, AStatus);
end;

procedure TPySys.SetLogTarget(AStringContainer: TMemo);
begin
  if FLogTarget <> AStringContainer then
    FLogTarget := AStringContainer;
end;

procedure TPySys.LogClear;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil,
      procedure()
      begin
      if Assigned(FLogTarget) then
        FLogTarget.Lines.Clear;
      end
      )
  else
    begin
      if Assigned(FLogTarget) then
        begin
          FLogTarget.Lines.Clear;
        end;
    end;
end;


procedure TPySys.Log(const AMsg: String; const SameLine: Boolean = False);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil,
      procedure()
      begin
      if Assigned(FLogTarget) then
        FLogTarget.Lines.Add('* ' + AMsg);
        FLogTarget.GoToTextEnd;
        FLogTarget.Repaint;
        Application.ProcessMessages;
      end
      )
  else
    begin
      if Assigned(FLogTarget) then
        begin
          if SameLine then
            begin
              var LineIdx: Integer;
              LineIdx := FLogTarget.Lines.Count - 1;
              FLogTarget.Lines[LineIdx] := AMsg;
              FLogTarget.GoToTextEnd;
              FLogTarget.Repaint;
              Application.ProcessMessages;
            end
          else
            begin
              FLogTarget.Lines.Add(AMsg);
              FLogTarget.GoToTextEnd;
              FLogTarget.Repaint;
              Application.ProcessMessages;
            end;
        end;
    end;
end;

procedure TPySys.AddExtraUrl(APackage: TPyManagedPackage; const AUrl: string);
var
  popts: TPyPackageManagerDefsPip;
begin
  popts := TPyPackageManagerDefsPip(APackage.Managers.Pip);
  popts.InstallOptions.ExtraIndexUrl := AUrl;
end;

procedure TPySys.PackageBeforeInstall(Sender: TObject);
begin
  Log('Installing ' + TPyPackage(Sender).PyModuleName);
end;

procedure TPySys.PackageAfterInstall(Sender: TObject);
begin
//  Log('Installed ' + TPyPackage(Sender).PyModuleName);
  Inc(PyPackagesInstalled);
end;

procedure TPySys.PackageInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log('Error for ' + TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
end;

procedure TPySys.PackageBeforeUnInstall(Sender: TObject);
begin
  Log('UnInstalling ' + TPyPackage(Sender).PyModuleName);
end;

procedure TPySys.PackageAfterUnInstall(Sender: TObject);
begin
//  Log('UnInstalled ' + TPyPackage(Sender).PyModuleName);
  Dec(PyPackagesInstalled);
end;

procedure TPySys.PackageUnInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log('Error for ' + TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
end;

procedure TPySys.PackageBeforeImport(Sender: TObject);
begin
  Log('Importing ' + TPyPackage(Sender).PyModuleName);
end;

procedure TPySys.PackageAfterImport(Sender: TObject);
begin
  Log('Imported ' + TPyPackage(Sender).PyModuleName);
end;

procedure TPySys.SafeInstall(Sender: TObject; FTask: ITask = Nil);
begin
  SafeMaskFPUExceptions(True);
  TPyManagedPackage(Sender).Install();
  SafeMaskFPUExceptions(False);
  if not (FTask = Nil) then
    FTask.CheckCanceled;
end;

procedure TPySys.SafeImport(Sender: TObject);
begin
  SafeMaskFPUExceptions(True);
  TPyManagedPackage(Sender).Import();
  SafeMaskFPUExceptions(False);
end;

procedure TPySys.PyEnvAfterDeactivate(Sender: TObject;
  const APythonVersion: string);
begin
  if PyIsReactivating then
    begin
      PyIsReactivating := False;
      PyEnv.Activate(pyver);
    end
  else if PyCleanOnExit then
    begin
      {$IFDEF MACOS64}
      {$ELSE}
      TDirectory.Delete(PyEnv.EnvironmentPath, True);
      {$ENDIF}
    end;
end;

procedure TPySys.PyIOSendUniData(Sender: TObject; const Data: string);
begin
  Log(Data);
end;

procedure TPySys.ReActivate;
begin
//  PyIsReactivating := True;
//  PyEnv.Deactivate;
end;

procedure TPySys.SetupPackage(APackage: TPyManagedPackage; const AExtraURL: String = '');
begin
  APackage.PythonEngine := PyEng;
  APackage.PyEnvironment := PyEnv;

  APackage.AutoImport := False;
  APackage.AutoInstall := False;

  APackage.BeforeInstall := PackageBeforeInstall;
  APackage.AfterInstall := PackageAfterInstall;
  APackage.OnInstallError := PackageInstallError;
  APackage.BeforeImport := PackageBeforeImport;
  APackage.AfterImport := PackageAfterImport;
  APackage.BeforeUnInstall := PackageBeforeUnInstall;
  APackage.AfterUnInstall := PackageAfterUnInstall;
  APackage.OnUnInstallError := PackageUnInstallError;

  if (AExtraUrl <> '') then
    AddExtraUrl(APackage, AExtraUrl);
end;

procedure TPySys.DoReady(Sender: TObject; const APythonVersion: string);
begin
  Log('Ready Event');
end;

procedure TPySys.EnurePipError(const ASender: TObject;
  const ADistribution: TPyDistribution; const AException: Exception);
begin
  Log('Unhandled Exception in EnurePipError');
  Log('Class : ' + AException.ClassName);
  Log('Error : ' + AException.Message);
end;

procedure TPySys.SetupSystem(OnSetupComplete: TPySysFinishedEvent = Nil; const HaveGPU: Boolean = False; const InstallationActive: Boolean = False);
begin
  Installing := InstallationActive;
  PySysFinishedEvent := OnSetupComplete;

  PyIO := TPythonInputOutput.Create(Self);
  PyIO.UnicodeIO := True;
  PyIO.OnSendUniData := PyIOSendUniData;
  // Python IO handler

  PyEng := TPythonEngine.Create(Self);
  PyEng.UseLastKnownVersion := False;
  PyEng.AutoLoad := False;
  PyEng.IO := PyIO;
  PyEng.RedirectIO := True;
  PyEng.OnBeforeUnload := PyEngBeforeUnload;
  // Python Engine

  {$IF DEFINED(MACOS64)}
  PyEnv := TPyLocalEnvironment.Create(Self);
  PyEnv.FilePath := '../Resources/macpython.json';
  {$ELSE}
  PyEnv := TPyEmbeddedResEnvironment39.Create(Self);
  PyEnv.EnvironmentPath := IncludeTrailingPathDelimiter(AppHome) + 'python';
  if not DirectoryExists(PyEnv.EnvironmentPath) then
    begin
      if not ForceDirectories(PyEnv.EnvironmentPath) then
        begin
          ShowMessage('Couldn''t create ' + PyEnv.EnvironmentPath);
        end;
    end;

  {$ENDIF}
  PyEnv.OnReady := DoReady;
  PyEnv.PythonEngine := PyEng;
  PyEnv.PythonVersion := pyver;

  // Python Environment

  PyEnsurePip := TPyEnvironmentAddOnEnsurePip.Create(Self);
  PyEnsurePip.Environment := PyEnv;
  PyEnsurePip.OnExecuteError := EnurePipError;
//  PyEnv.AfterDeactivate := PyEnvAfterDeactivate;
  // Tidy up on exit (clean Python for testing)

  // Create NumPy
  NumPy := TNumPy.Create(Self);
  SetupPackage(NumPy);

  // Create Pillow
  Pillow := TPillow.Create(Self);
  SetupPackage(Pillow);

  // Create SciPy
  SciPy := TSciPy.Create(Self);
  SetupPackage(SciPy);

  // Create PSUtil
  {$IF NOT DEFINED(CPUARM)}
  PSUtil := TPSUtil.Create(Self);
  SetupPackage(PSUtil);
  {$ENDIF}

  // Create Torch
  Torch := TPyTorch.Create(Self);
  if haveGPU then
    SetupPackage(Torch, 'https://download.pytorch.org/whl/cu116')
  else
    SetupPackage(Torch);

  // Create TorchVision
  TorchVision := TTorchVision.Create(Self);
  if haveGPU then
    SetupPackage(TorchVision, 'https://download.pytorch.org/whl/cu116')
  else
    SetupPackage(TorchVision);

  //  Call Setup
  FTask := TTask.Run(ThreadedSetup);

end;

procedure TPySys.ThreadedSetup;
begin
  // Install Python if required
  PyEnv.Setup(pyver);
  FTask.CheckCanceled();

  TThread.Synchronize(nil,
    procedure()
    begin
      Log('pyver = ' + pyver);

      try
      // Activate Python
      if PyEnv.Activate(pyver) then
        begin
          Log('Python activate returned true');

          {$IF NOT DEFINED(CPUARM)}
          SafeInstall(PSUtil);
          {$ENDIF}

          SafeInstall(Numpy, FTask);
          SafeInstall(Pillow, FTask);
          SafeInstall(SciPy, FTask);
          SafeInstall(Torch, FTask);
          SafeInstall(TorchVision, FTask);

          TThread.Queue(nil,
            procedure()
            begin
              {$IF NOT DEFINED(CPUARM)}
              SafeImport(PSUtil);
              {$ENDIF}
              SafeImport(Torch);
              SafeImport(Pillow);
            end
            );

          TThread.Synchronize(nil,
            procedure()
            begin
              //  'Add Modules
              Log('Adding Modules');
              modStyle := TModStyle.Create(Self);
              modStyle.Engine := PyEng;
              modStyle.ModuleName := 'pstyle';
              modStyle.Initialize;

              modTrain := TModTrain.Create(Self);
              modTrain.Engine := PyEng;
              modTrain.ModuleName := 'ptrain';
              modTrain.Initialize;

              modPyIO := TModPyIO.Create(Self);
              modPyIO.Engine := PyEng;
              modPyIO.ModuleName := 'pinout';
              modPyIO.Initialize;

              modCalibrate := TModCalibration.Create(Self);
              modCalibrate.Engine := PyEng;
              modCalibrate.ModuleName := 'pcalibrate';
              modCalibrate.Initialize;
            end
          );

          TThread.Synchronize(nil,
            procedure()
            begin
              Log('Testing Python');

              SystemError := RunShim(pyshim);
              if not SystemError then
                SystemError := RunTest;
              if not SystemError then
                SystemError := RunSystem;

              Log('Python Available');
              if not Installing then
                begin
                  FLogTarget.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'startup.log');
                  FLogTarget := Nil;
                end;
              SystemActive := not SystemError;
              DoPySysFinishedEvent(SystemActive);
            end
          );
        end
      else
        begin
          Log('Python activate returned false');
          DoPySysFinishedEvent(SystemActive);
        end;
      except
        on E: Exception do
          begin
            Log('Unhandled Exception in ThreadedSetup');
            Log('Class : ' + E.ClassName);
            Log('Error : ' + E.Message);
          end;
      end;
    end
  );
end;

procedure TPySys.ShutdownSystem;
begin
  PyEnv.Deactivate;
end;

function TPySys.RunShim(const ShimPath: String): Boolean;
var
  Shim: TStringList;
begin
  Result := False;
  Shim := Nil;
  try
    try
      Shim := TStringList.Create;
      Shim.Add('import os');
      Shim.Add('import sys');
      if not(LastShimPath = String.Empty) then
      begin
        Shim.Add('for p in reversed(sys.path):');
        Shim.Add('  if p == "' + EscapeBackslashForPython(LastShimPath) + '":');
        Shim.Add('    sys.path.remove(p)');
      end;
      Shim.Add('sys.path.append("' + EscapeBackslashForPython(IncludeTrailingPathDelimiter(AppHome)) + ShimPath + '")');
      Shim.Add('os.chdir("' + EscapeBackslashForPython(AppHome) + '")');
      Shim.Add('__embedded_python__ = True');

      SafeMaskFPUExceptions(True);
      PyEng.ExecStrings(Shim);
      SafeMaskFPUExceptions(False);

      LastShimPath := IncludeTrailingPathDelimiter(AppHome) + ShimPath;
    except
      on E: EPyImportError do
        begin
          Result := True;
          Log('Import Exception in RunShim');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
          Log('Value : ' + E.EValue);
          Log('Name : ' + E.EName);
        end;
      on E: EPyIndentationError do
        begin
          Result := True;
          Log('Indentation Exception in RunShim');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
          Log('Line : ' + IntToStr(E.ELineNumber));
          Log('Offset : ' + IntToStr(E.EOffset));
        end;
      on E: EPyException do
        begin
          Result := True;
          Log('Unhandled Python Exception in RunShim');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
      on E: Exception do
        begin
          Result := True;
          Log('Unhandled Exception in RunShim');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
    if not(Shim = Nil) then
      Shim.Free;
  end;
  Log('RunShim Completed');
end;

function TPySys.RunTest: Boolean;
var
  PythonCode: TStringList;
begin
  Result := False;

  PythonCode := TStringList.Create;
  PythonCode.Add('import sys');
  PythonCode.Add('print("Hello World from ", sys.version)');
  PythonCode.Add('print("Python =", sys.executable)');
  PythonCode.Add('import torch');
  PythonCode.Add('print("Torch =", torch.__version__)');
  // A little script to check we're working as expected
  try
    try
      SafeMaskFPUExceptions(True);
      PyEng.ExecStrings(PythonCode);
      SafeMaskFPUExceptions(False);
    except
      on E: EPyImportError do
        begin
          Result := True;
          Log('Import Exception in RunTest');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
          Log('Value : ' + E.EValue);
          Log('Name : ' + E.EName);
        end;
      on E: EPyIndentationError do
        begin
          Result := True;
          Log('Indentation Exception in RunTest');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
          Log('Line : ' + IntToStr(E.ELineNumber));
          Log('Offset : ' + IntToStr(E.EOffset));
        end;
      on E: EPyException do
        begin
          Result := True;
          Log('Unhandled Python Exception in RunTest');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
      on E: Exception do
        begin
          Result := True;
          Log('Unhandled Exception in RunTest');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
    PythonCode.Free;
  end;

  Log('RunTest Completed');
end;

function TPySys.RunSystem: Boolean;
begin
  Result := False;
  try
    SafeMaskFPUExceptions(True);
    PyEng.ExecStrings(SystemCode);
    SafeMaskFPUExceptions(False);
  except
    on E: EPyImportError do
      begin
        Result := True;
        Log('Import Exception in RunSystem');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Value : ' + E.EValue);
        Log('Name : ' + E.EName);
      end;
    on E: EPyIndentationError do
      begin
        Result := True;
        Log('Indentation Exception in RunSystem');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Line : ' + IntToStr(E.ELineNumber));
        Log('Offset : ' + IntToStr(E.EOffset));
      end;
    on E: EPyException do
      begin
        Result := True;
        Log('Unhandled Python Exception in RunSystem');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
    on E: Exception do
      begin
        Result := True;
        Log('Unhandled Exception in RunSystem');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
  end;

  Log('RunSystem Completed');
end;

procedure TPySys.PyEngBeforeUnload(Sender: TObject);
begin
  {$IFDEF MESSAGE_SUPPORT}
  ShowMessage('Python Unloading');
  {$ENDIF}
end;

end.
