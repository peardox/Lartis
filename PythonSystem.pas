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

    FLogTarget: TMemo;
    procedure PackageConfigureInstall(Sender: TObject);
    procedure PackageBeforeInstall(Sender: TObject);
    procedure PackageAfterInstall(Sender: TObject);
    procedure PackageAfterImport(Sender: TObject);
    procedure PackageBeforeImport(Sender: TObject);
    procedure PackageInstallError(Sender: TObject; AErrorMessage: string);
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
    procedure SetupSystem;
    procedure RunTest;
    procedure RunSystem;
    procedure ShutdownSystem;
    procedure Log(const AMsg: String; const SameLine: Boolean = False);
    procedure LogClear;
  end;

var
  PySys: TPySys;
  FPUMASK: TArithmeticExceptionMask;

function EscapeBackslashForPython(const AStr: String): String;
procedure GetAllModels(const AltModelDir: String = String.Empty; const ModelSubDir: String = String.Empty);
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

procedure GetAllModels(const AltModelDir: String = String.Empty; const ModelSubDir: String = String.Empty);
var
  SearchRec: TSearchRec;
  modeldir: String;
  filespec: String;
  FileName: String;
begin
  if (AltModelDir = String.Empty) then
    Modeldir := IncludeTrailingPathDelimiter(AppHome) + 'models'
  else
    modeldir := AltModelDir;

  if (ModelSubDir = String.Empty) then
    filespec := IncludeTrailingPathDelimiter(modeldir)
  else
    filespec := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(modeldir) + ModelSubDir);

  {$ifdef MSWINDOWS}
  filespec := filespec + '*.*';
  {$ELSE}
  filespec := filespec + '*';
  {$ENDIF}

  if not Assigned(ModelList) then
    ModelList := TStringList.Create
  else if (ModelSubDir = String.Empty) then
    ModelList.Clear;

  if DirectoryExists(modeldir) then
    begin
      if (FindFirst(filespec, faAnyFile, SearchRec) = 0) then
        begin
          repeat
            FileName := SearchRec.Name;
            if ((SearchRec.Attr and faDirectory) = 0) then
            begin
              if FileName.ToLower.EndsWith('.pth') then
                begin
                  FileName := FileName.Remove(Length(Filename) - 4);
                  if (ModelSubDir <> String.Empty) then
                    FileName := ModelSubDir + System.IOUtils.TPath.DirectorySeparatorChar + FileName;
                  ModelList.Add(FileName);
                end;
            end
          else
            begin
              if (FileName <> '.') and (FileName <> '..') then
                begin
                  if (ModelSubDir = String.Empty) then
                    GetAllModels(modeldir, FileName)
                  else
                    GetAllModels(modeldir, ModelSubDir + System.IOUtils.TPath.DirectorySeparatorChar + FileName);
                end;
            end;
          until FindNext(SearchRec) <> 0;
          FindClose(SearchRec);
        end;
    end;
end;

function EscapeBackslashForPython(const AStr: String): String;
begin
  Result := StringReplace(AStr, '\', '\\', [rfIgnoreCase, rfReplaceAll]);
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

procedure TPySys.PackageConfigureInstall(Sender: TObject);
begin
  TPyManagedPackage(Sender).AutoImport := False;
  TPyManagedPackage(Sender).AutoInstall := False;

  TPyManagedPackage(Sender).BeforeInstall := PackageBeforeInstall;
  TPyManagedPackage(Sender).AfterInstall := PackageAfterInstall;
  TPyManagedPackage(Sender).OnInstallError := PackageInstallError;
  TPyManagedPackage(Sender).BeforeImport := PackageBeforeImport;
  TPyManagedPackage(Sender).AfterImport := PackageAfterImport;

  if TPyPackage(Sender).PyModuleName = 'torch' then
    begin
    Log('Installing ' + TPyPackage(Sender).PyModuleName + sLineBreak +
      'This will take quite some time, please be patient');
    end
  else
    begin
      Log('Installing ' + TPyPackage(Sender).PyModuleName);
    end;
end;

procedure TPySys.PackageBeforeInstall(Sender: TObject);
begin
  SafeMaskFPUExceptions(True);
end;

procedure TPySys.PackageAfterInstall(Sender: TObject);
begin
  SafeMaskFPUExceptions(False);
  Log('Installed ' + TPyPackage(Sender).PyModuleName);
  Inc(PyPackagesInstalled);
end;

procedure TPySys.PackageBeforeImport(Sender: TObject);
begin
  SafeMaskFPUExceptions(True);
  Log('Importing ' + TPyPackage(Sender).PyModuleName);
end;

procedure TPySys.PackageAfterImport(Sender: TObject);
begin
  SafeMaskFPUExceptions(False);
end;

procedure TPySys.PackageInstallError(Sender: TObject; AErrorMessage: string);
begin
  Log(TPyPackage(Sender).PyModuleName + ' : ' + AErrorMessage);
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
  APackage.BeforeInstall := PackageConfigureInstall;
  if (AExtraUrl <> '') then
    AddExtraUrl(APackage, AExtraUrl);
end;

procedure TPySys.DoReady(Sender: TObject; const APythonVersion: string);
begin
  Log('Ready Event');
end;

procedure TPySys.SetupSystem;
begin
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
  PyEng.DllPath := PyEnv.EnvironmentPath + '/' + pyver + '/lib/';
  PyEng.DllName := 'libpython' + pyver + '.dylib';
  {$endif}

  Log('Env Path = ' + PyEnv.EnvironmentPath);
  Log('Eng Lib = ' + PyEng.DllName);
  Log('Eng Libpath = ' + PyEng.DllPath);

//  Log('Numpy');
  NumPy := TNumPy.Create(Self);
  SetupPackage(NumPy);
  // Create NumPy

//  Log('Scipy');
  SciPy := TSciPy.Create(Self);
  SetupPackage(SciPy);
  // Create SciPy

//  Log('AWS');
  AWS := TBoto3.Create(Self);
  SetupPackage(AWS);
  // Create AWS

//  Log('PSUtil');
  PSUtil := TPSUtil.Create(Self);
  SetupPackage(PSUtil);
  // Create PSUtil

//  Log('Torch');
  Torch := TPyTorch.Create(Self);
  SetupPackage(Torch, 'https://download.pytorch.org/whl/cu116');
  // Create Torch

//  Log('TorchVision');
  TorchVision := TTorchVision.Create(Self);
  SetupPackage(TorchVision, 'https://download.pytorch.org/whl/cu116');
  // Create TorchVision

//  Log('Add Modules');
  modStyle := TModStyle.Create(Self);
  modStyle.Engine := PyEng;
  modStyle.ModuleName := 'pstyle';

  modTrain := TModTrain.Create(Self);
  modTrain.Engine := PyEng;
  modTrain.ModuleName := 'ptrain';

  modPyIO := TModPyIO.Create(Self);
  modPyIO.Engine := PyEng;
  modPyIO.ModuleName := 'pinout';

//  Log('Call Setup');
  FTask := TTask.Run(ThreadedSetup);

end;

procedure TPySys.ThreadedSetup;
begin
  PyEnv.Setup(pyver);
  FTask.CheckCanceled();
  // Install Python if required

  // Show some important stuff
  Log('PyEng Libpath = ' + PyEng.DllPath);
  Log('PyEng Lib = ' + PyEng.DllName);

  TThread.Synchronize(nil,
    procedure()
    begin
      if PyEnv.Activate(pyver) then
        Log('Python activate returned true')
      else
        Log('Python activate returned false');
    end
  );
  FTask.CheckCanceled();
  // Activate Python

  Log('Numpy Install');
  NumPy.Install();
  FTask.CheckCanceled();
  // Create NumPy

  Log('Scipy Install');
  SciPy.Install();
  FTask.CheckCanceled();
  // Create SciPy

  Log('AWS Install');
  AWS.Install();
  // Create AWS

  Log('PSUtil Install');
  PSUtil.Install();
  FTask.CheckCanceled();
  // Create PSUtil

  Log('Torch Install');
  Torch.Install();
  FTask.CheckCanceled();
  // Create Torch

  Log('TorchVision Install');
  TorchVision.Install();
  FTask.CheckCanceled();
  // Create TorchVision

  TThread.Queue(nil,
    procedure()
    begin
      Log('Import All');
{
      Log('Numpy Import');
      Numpy.Import();
      Log('SciPy Import');
      SciPy.Import();
      Log('AWS Import');
      AWS.Import();
      Log('PSUtil Import');
      PSUtil.Import();
      Log('Torch Import');
      Torch.Import();
      Log('TorchVision Import');
      TorchVision.Import();
}
      Log('All Imported');

      ShimSysPath(pyshim);
      RunSystem;
    end
    );

  TThread.Synchronize(nil,
    procedure()
    begin
      SystemActive := True;
      FLogTarget := Nil;
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

    Log('Shim');
    for var i := 0 to Shim.Count - 1 do
      Log(Shim[i]);

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

  Log('Done');
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

  LogTarget := Nil;
  frmSetup.Close;
end;

end.
