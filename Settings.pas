unit Settings;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Dialogs;

var
  AppHome: String;
  ModelList: TStringList;
  SystemActive: Boolean;
  ShaderPath: String;
  StylesPath: String;
  DataSetsPath: String;
  OutImagePath: String;
  FrameCount: Integer;

const
  appname: String = 'Lartis';
  pypath: String = 'python';
  pyver: String = '3.9';
  pyexe: String = 'python.exe';
  pyshim: String = 'pysrc';
  pycode: String = 'SystemCode.py';

procedure CreateSettings;

implementation

procedure CreateSettings;
begin
  {$IFDEF MSWINDOWS}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  {$ELSE}
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + '.' + appname;
  {$ENDIF}
  // System agnostic path for data files + Python
  if not DirectoryExists(AppHome) then
    begin
      ShowMessage('No AppHome at : ' + AppHome);
    end;

  ShaderPath := TPath.Combine(AppHome, 'shaders');;
  if not DirectoryExists(ShaderPath) then
    begin
      ShowMessage('No Shaders at : ' + ShaderPath);
    end;

  StylesPath := TPath.Combine(AppHome, 'styles');;

{
  DataSetsPath := TPath.Combine(AppHome, 'datasets');
  if not DirectoryExists(DataSetsPath) then
    begin
      ForceDirectories(DataSetsPath);
    end;
}

  OutImagePath := TPath.Combine(AppHome, 'output-images');;
  if not DirectoryExists(OutImagePath) then
    begin
      ForceDirectories(OutImagePath);
    end;

end;

end.
