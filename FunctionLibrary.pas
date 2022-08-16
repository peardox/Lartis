unit FunctionLibrary;

interface

function LoadShader(const AShaderFile: String): String;
function FitInsideContainer(const AContainerWidth: Single; const AContentWidth: Single; const AContainerHeight: Single; const AContentHeight: Single): Single;
function BoolToInt(const AValue: Boolean): Integer;

implementation

uses
  Settings,
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, FMX.Forms, FMX.Dialogs,
  IOUtils, Math;

function FitInsideContainer(const AContainerWidth: Single;
  const AContentWidth: Single; const AContainerHeight: Single;
  const AContentHeight: Single): Single;
begin
  Result := Min(AContainerWidth / AContentWidth, AContainerHeight / AContentHeight);
end;

function BoolToInt(const AValue: Boolean): Integer;
begin
  Result := 0;
  if AValue then
    Result := 1;
end;

function LoadShader(const AShaderFile: string): String;
begin
  if FileExists(AShaderFile) then
    begin
      try
        Result := TFile.ReadAllText(AShaderFile).Replace(#9, #32);
      except
        ShowMessage('Can''t read shader ''' + AShaderFile + '''');
        Application.Terminate;
        Exit;
      end;
    end
  else
    begin
      ShowMessage('Can''t find shader ''' + AShaderFile + '''');
      Application.Terminate;
      Exit;
    end;
end;

end.
