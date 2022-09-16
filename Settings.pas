unit Settings;

// {$DEFINE CLEANSTART}

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Forms, FMX.Dialogs, StyleModel;

var
  InstallRequired: Boolean;

  AppHome: String;
  CachePath: String;
  ShaderPath: String;
  StylesPath: String;
  DataSetsPath: String;
  PreTrainedPath: String;

  ModelList: TStringList;
  StyleModels: TStyleModels;
  SystemActive: Boolean;

  FrameCount: Integer;
  EnableGPU: Boolean;
  SystemStyle: String;

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

implementation

procedure CreateSettings;
begin
  InstallRequired := False;
//  InstallRequired := True;

  {$IF DEFINED(MACOS64)}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetLibraryPath) + appname;
  {$ELSE}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ENDIF}
  // System agnostic path for data files + Python

  {$IFDEF CLEANSTART}
  if DirectoryExists(AppHome) then
    TDirectory.Delete(AppHome, True);
  {$ENDIF}

  if not DirectoryExists(AppHome) then
    begin
      ForceDirectories(AppHome);
      InstallRequired := True;
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

end;

procedure DestroySettings;
begin
  if Assigned(ModelList) then
    FreeAndNil(ModelList);
end;

end.
