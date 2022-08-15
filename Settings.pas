unit Settings;

interface

uses
  System.SysUtils, System.IOUtils, System.Types, System.UITypes, System.Classes, System.Variants;

var
  AppHome: String;
  ModelList: TStringList;

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
  AppHome := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetHomePath) + appname;
  // System agnostic path for data files + Python
  if not DirectoryExists(AppHome) then
    ForceDirectories(AppHome);
end;

end.
