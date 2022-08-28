unit PythonSystem;

{$DEFINE USESAFEMASK}

interface
uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, System.Threading, PyEnvironment,
  FMX.Types, FMX.Memo, FMX.Forms, Math,
  PyEnvironment.Embeddable, PythonEngine, PyCommon,
  PyEnvironment.Embeddable.Res, PyEnvironment.Embeddable.Res.Python39,
  PyModule, PyPackage,
  TorchVision, PyTorch, NumPy, SciPy, PSUtil, Boto3,
  Modules;

type
  TPySysFinishedEvent = procedure(Sender: TObject; const AStatus: Boolean) of object;

  TPySys = class(TComponent)
  private
    { Private declarations }
    SystemCode: TStringList;
    PyCleanOnExit: Boolean;
    PyIsReactivating: Boolean;
    PyPackagesInstalled: Integer;
    LastShimPath: String;
    FTask: ITask;

    PyEng: TPythonEngine;
    PyEnv: TPyEmbeddedResEnvironment39;
    PyIO: TPythonInputOutput;

    NumPy: TNumPy;
    SciPy: TSciPy;
    TorchVision: TTorchVision;

    FPySysFinishedEvent: TPySysFinishedEvent;
    FLogTarget: TMemo;

    procedure DoPySysFinishedEvent(const AStatus: Boolean);

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

    procedure DoReady(Sender: TObject; const APythonVersion: string);
    procedure ReActivate;
    procedure SetLogTarget(AStringContainer: TMemo);
    procedure SetupPackage(APackage: TPyManagedPackage; const AExtraURL: String = '');
    procedure ShimSysPath(const ShimPath: String);
    procedure ThreadedSetup;
  public
    Torch: TPyTorch;
    PSUtil: TPSUtil;
    AWS: TBoto3;

    modStyle: TModStyle;
    modTrain: TModTrain;
    modPyIO: TModPyIO;

    property LogTarget: TMemo read FLogTarget write SetLogTarget;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupSystem(OnSetupComplete: TPySysFinishedEvent = Nil);
    procedure RunTest;
    procedure RunSystem;
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

  Torch.Free;
  PSUtil.Free;
  NumPy.Free;
  SciPy.Free;
  TorchVision.Free;
  AWS.Free;

  PyEng.Free;
  PyEnv.Free;
  PyIO.Free;

  inherited;
end;

constructor TPySys.Create(AOwner: TComponent);
begin
  inherited;
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
//  SafeMaskFPUExceptions(True);
end;

procedure TPySys.PackageAfterInstall(Sender: TObject);
begin
//  Log('Installed ' + TPyPackage(Sender).PyModuleName);
  Inc(PyPackagesInstalled);
//  SafeMaskFPUExceptions(False);
end;

procedure TPySys.PackageInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log('Error for ' + TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
end;

procedure TPySys.PackageBeforeUnInstall(Sender: TObject);
begin
  Log('UnInstalling ' + TPyPackage(Sender).PyModuleName);
//  SafeMaskFPUExceptions(True);
end;

procedure TPySys.PackageAfterUnInstall(Sender: TObject);
begin
//  Log('UnInstalled ' + TPyPackage(Sender).PyModuleName);
  Dec(PyPackagesInstalled);
//  SafeMaskFPUExceptions(False);
end;

procedure TPySys.PackageUnInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log('Error for ' + TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
end;

procedure TPySys.PackageBeforeImport(Sender: TObject);
begin
  Log('Importing ' + TPyPackage(Sender).PyModuleName);
//  SafeMaskFPUExceptions(True);
end;

procedure TPySys.PackageAfterImport(Sender: TObject);
begin
//  Log('Imported ' + TPyPackage(Sender).PyModuleName);
//  SafeMaskFPUExceptions(False);
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
      TDirectory.Delete(PyEnv.EnvironmentPath, True);
    end;
end;

procedure TPySys.PyIOSendUniData(Sender: TObject; const Data: string);
begin
  Log(Data);
end;

procedure TPySys.ReActivate;
begin
  PyIsReactivating := True;
  PyEnv.Deactivate;
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

procedure TPySys.SetupSystem(OnSetupComplete: TPySysFinishedEvent = Nil);
begin
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
  // Python Engine

  PyEnv := TPyEmbeddedResEnvironment39.Create(Self);
  PyEnv.OnReady := DoReady;
  PyEnv.PythonEngine := PyEng;
  PyEnv.PythonVersion := pyver;

  // Python Environment

  PyEnv.AfterDeactivate := PyEnvAfterDeactivate;
  // Tidy up on exit (clean Python for testing)

  PyEnv.EnvironmentPath := IncludeTrailingPathDelimiter(AppHome) + 'python';
  {$ifdef MACOS64}
//  PyEnv.EnvironmentPath := '';
//  PyEng.DllPath := PyEnv.EnvironmentPath + '/' + pyver + '/lib/';
//  PyEng.DllName := 'libpython' + pyver + '.dylib';
  {$endif}

  // Create NumPy
  NumPy := TNumPy.Create(Self);
  SetupPackage(NumPy);

  // Create SciPy
  SciPy := TSciPy.Create(Self);
  SetupPackage(SciPy);

  // Create AWS
  AWS := TBoto3.Create(Self);
  SetupPackage(AWS);

  // Create PSUtil
  {$IFNDEF MACOS64}
  PSUtil := TPSUtil.Create(Self);
  SetupPackage(PSUtil);
  {$ENDIF}

  // Create Torch
  Torch := TPyTorch.Create(Self);
  SetupPackage(Torch, 'https://download.pytorch.org/whl/cu116');

  // Create TorchVision
  TorchVision := TTorchVision.Create(Self);
  SetupPackage(TorchVision, 'https://download.pytorch.org/whl/cu116');

  //  'Add Modules
  modStyle := TModStyle.Create(Self);
  modStyle.Engine := PyEng;
  modStyle.ModuleName := 'pstyle';

  modTrain := TModTrain.Create(Self);
  modTrain.Engine := PyEng;
  modTrain.ModuleName := 'ptrain';

  modPyIO := TModPyIO.Create(Self);
  modPyIO.Engine := PyEng;
  modPyIO.ModuleName := 'pinout';

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
    {$ifdef MACOS64}
      var EnvVar: String;
      EnvVar := GetEnvironmentVariable('DYLD_LIBRARY_PATH');
      if EnvVar = String.Empty then
        EnvVar := PyEnv.EnvironmentPath + '/' + pyver + '/lib'
      else
        EnvVar := PyEnv.EnvironmentPath + '/' + pyver + '/lib:' + EnvVar;
//      SetEnvironmentVariable('DYLD_LIBRARY_PATH', EnvVar);
      Log('Environment needs ' + EnvVar);
      {$endif}
      Log('Env Path = ' + PyEnv.EnvironmentPath);
      Log('Eng DLLPath = ' + PyEng.DllPath);
      Log('Eng DLLName = ' + PyEng.DllName);
      Log('pyver = ' + pyver);

      try
      // Activate Python
      if PyEnv.Activate(pyver) then
        Log('Python activate returned true')
      else
        Log('Python activate returned false');
      except
        on E: Exception do
          begin
            Log('Unhandled Exception');
            Log('Class : ' + E.ClassName);
            Log('Error : ' + E.Message);
          end;
      end;
    end
  );
  FTask.CheckCanceled();

  SafeMaskFPUExceptions(True);
  NumPy.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();

  SafeMaskFPUExceptions(True);
  SciPy.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();

  SafeMaskFPUExceptions(True);
  AWS.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();

  {$IFNDEF MACOS64}
  SafeMaskFPUExceptions(True);
  PSUtil.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();
  {$ENDIF}

  SafeMaskFPUExceptions(True);
  Torch.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();

  SafeMaskFPUExceptions(True);
  TorchVision.Install();
  SafeMaskFPUExceptions(False);
  FTask.CheckCanceled();

  TThread.Queue(nil,
    procedure()
    begin
      SafeMaskFPUExceptions(True);
//      Numpy.Import();
//      SciPy.Import();
//      AWS.Import();
      {$IFNDEF MACOS64}
      PSUtil.Import();
      {$ENDIF}
      Torch.Import();
//      TorchVision.Import();
      SafeMaskFPUExceptions(False);

      ShimSysPath(pyshim);
      RunSystem;
    end
    );

  TThread.Synchronize(nil,
    procedure()
    begin
      Log('All Done');
      FLogTarget.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'startup.log');
      SystemActive := True;
      FLogTarget := Nil;
      DoPySysFinishedEvent(True);
    end
  );
end;

procedure TPySys.ShutdownSystem;
begin
  PyEnv.Deactivate;
end;

procedure TPySys.ShimSysPath(const ShimPath: String);
var
  Shim: TStringList;
begin
  Shim := Nil;
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
  finally
    if not(Shim = Nil) then
      Shim.Free;
  end;
end;

procedure TPySys.RunTest;
var
  PythonCode: TStringList;
begin

  PythonCode := TStringList.Create;
  PythonCode.Add('import sys');
  PythonCode.Add('print("Hello World from ", sys.version)');
  PythonCode.Add('print("Python =", sys.executable)');
  PythonCode.Add('import torch');
  PythonCode.Add('print("Torch =", torch.__version__)');
  // A little script to check we're working as expected

  try
    SafeMaskFPUExceptions(True);
    PyEng.ExecStrings(PythonCode);
    SafeMaskFPUExceptions(False);
  except
    on E: EPyImportError do
      begin
        Log('Import Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Value : ' + E.EValue);
        Log('Name : ' + E.EName);
      end;
    on E: EPyIndentationError do
      begin
        Log('Indentation Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Line : ' + IntToStr(E.ELineNumber));
        Log('Offset : ' + IntToStr(E.EOffset));
      end;
    on E: EPyException do
      begin
        Log('Unhandled Python Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
    on E: Exception do
      begin
        Log('Unhandled Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
  end;

  Log('Ready');
end;

procedure TPySys.RunSystem;
begin
  try
    SafeMaskFPUExceptions(True);
    PyEng.ExecStrings(SystemCode);
    SafeMaskFPUExceptions(False);
  except
    on E: EPyImportError do
      begin
        Log('Import Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Value : ' + E.EValue);
        Log('Name : ' + E.EName);
      end;
    on E: EPyIndentationError do
      begin
        Log('Indentation Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
        Log('Line : ' + IntToStr(E.ELineNumber));
        Log('Offset : ' + IntToStr(E.EOffset));
      end;
    on E: EPyException do
      begin
        Log('Unhandled Python Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
    on E: Exception do
      begin
        Log('Unhandled Exception');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
  end;

end;

end.
