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
    AppHome: String;
  end;

var
  InstallRequired: Boolean;

  AppHome: String;
  SettingsHome: String;
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
var
  RealHome: String;
begin
  InstallRequired := False;
  EnableGPU := True;

  {$IF DEFINED(MACOS64)}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSEIF DEFINED(LINUX)}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ELSE}
  RealHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ENDIF}
  // System agnostic path for data files + Python

  AppHome := RealHome;
  SettingsHome := AppHome;

  if FileExists(IncludeTrailingPathDelimiter(SettingsHome) + 'Settings.json') then
    begin
      LoadSystemSettings;
      // If AppHome has been removed (e.g. USB) then it won't exist any more
      // so reset it back to default in that situation
      if not DirectoryExists(AppHome) then
        begin
          AppHome := RealHome;
          SystemSettings.AppHome := AppHome;
        end;
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
    JsonText := TFile.ReadAllText(IncludeTrailingPathDelimiter(SettingsHome) + 'Settings.json');
  except
     on E : Exception do
       Raise Exception.Create('LoadSystemSettings - Exception : Class = ' +
        E.ClassName + ', Message = ' + E.Message);
  end;

  lSerializer := TJsonSerializer.Create;
  try
    try
      SystemSettings := lSerializer.Deserialize<TSettings>(JsonText);
      if SystemSettings.AppHome <> String.Empty then
        AppHome := SystemSettings.AppHome;
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
        TFile.WriteAllText(IncludeTrailingPathDelimiter(SettingsHome) + 'Settings.json', JsonText);
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

  if not FileExists(TPath.Combine(AppHome, pycode)) then
    InstallRequired := True;

  SaveSystemSettings;
end;


end.
