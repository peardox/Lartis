unit Settings;

// {$DEFINE CLEANSTART}

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Forms, FMX.Dialogs, StyleModel;

type
  TSettings = record
    WipeOnStart: Boolean;
    PythonInstalled: Boolean;
    LastOpenStyleDir: String;
    LastSaveStyleDir: String;
  end;

var
  InstallRequired: Boolean;

  AppHome: String;
  CachePath: String;
  ShaderPath: String;
  StylesPath: String;
  DataSetsPath: String;
  PreTrainedPath: String;
  TempPath: String;

  StyleModels: TStyleModels;
  SystemActive: Boolean;

  FrameCount: Integer;
  EnableGPU: Boolean;
  SystemStyle: String;
  SystemSettings: TSettings;

const
  appname: String = 'Lartis';
  pypath: String = 'python';
  pyver: String = '3.9';
  pyexe: String = 'python.exe';
  pyshim: String = 'pysrc';
  pycode: String = 'SystemCode.py';

  APIBase: String = 'https://peardox.com/Lartis/';

procedure CreateSettings;
procedure DestroySettings;
procedure LoadSystemSettings;
procedure SaveSystemSettings;
procedure InitialiseSystem;

implementation

uses
  JSON.Serializers;

procedure CreateSettings;
begin
  InstallRequired := False;
  EnableGPU := True;

  {$IF DEFINED(MACOS64)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSEIF DEFINED(LINUX)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ELSE}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ENDIF}
  // System agnostic path for data files + Python

  if FileExists(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json') then
    begin
      LoadSystemSettings;
    end;

    InitialiseSystem;
end;

procedure DestroySettings;
begin
  SaveSystemSettings;
end;

procedure LoadSystemSettings;
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  try
    JsonText := TFile.ReadAllText(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json');
  except
     on E : Exception do
       Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
  end;

  lSerializer := TJsonSerializer.Create;
  try
    try
      SystemSettings := lSerializer.Deserialize<TSettings>(JsonText);
    except
     on E : Exception do
       Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
    end;
  finally
    FreeAndNil(lSerializer);
  end;
end;

procedure SaveSystemSettings;
var
  lSerializer: TJsonSerializer;
  JsonText: String;
begin
  lSerializer := TJsonSerializer.Create;
  try
    try
      JsonText := lSerializer.Serialize<TSettings>(SystemSettings);
      try
        TFile.WriteAllText(IncludeTrailingPathDelimiter(AppHome) + 'Settings.json', JsonText);
      except
         on E : Exception do
           Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
            E.ClassName + ', Message = ' + E.Message);
      end;
    except
     on E : Exception do
     begin
       Raise Exception.Create('SaveSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
  end;
end;

procedure InitialiseSystem;
  begin
  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
      InstallRequired := True;
    end;

  if not SystemSettings.PythonInstalled then
    InstallRequired := True;

  TempPath := TPath.Combine(AppHome, 'temp');;
  if not DirectoryExists(TempPath) then
    begin
      ForceDirectories(TempPath);
    end;

  ShaderPath := TPath.Combine(AppHome, 'shaders');;
  if not DirectoryExists(ShaderPath) then
    begin
      ForceDirectories(ShaderPath);
    end;

  SystemStyle := '';
  StylesPath := TPath.Combine(AppHome, 'styles');;
  if not DirectoryExists(ShaderPath) then
    begin
      ForceDirectories(StylesPath);
    end;

  PreTrainedPath := TPath.Combine(AppHome, 'pretrained');;
  if not DirectoryExists(PreTrainedPath) then
    begin
      ForceDirectories(PreTrainedPath);
    end;

  DataSetsPath := TPath.Combine(AppHome, 'datasets');
  if not DirectoryExists(DataSetsPath) then
    begin
      ForceDirectories(DataSetsPath);
    end;

  CachePath := TPath.Combine(AppHome, 'cache');;
  if not DirectoryExists(CachePath) then
    begin
      ForceDirectories(CachePath);
    end;

  SaveSystemSettings;
end;


end.
