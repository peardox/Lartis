unit FunctionLibrary;

interface

uses
  FMX.Graphics, VarPyth, PythonEngine;

type
  TAlphaMap = record
    HasAlphaMap: Boolean;
    AlphaMap: Array [0..255] of Integer;
    AlphasUsed: Integer;
    PixelCount: Integer;
    MinAlpha: Integer;
    MaxAlpha: Integer;
    MostAlpha: Integer;
    MostAlphaCount: Integer;
  end;

function ImageToPyBytes(ABitmap: TBitmap): Variant;
function LoadShader(const AShaderFile: String): String;
function FitInsideContainer(const AContainerWidth: Single; const AContentWidth: Single; const AContainerHeight: Single; const AContentHeight: Single): Single;
function BoolToInt(const AValue: Boolean): Integer;
function GetFileHash(const AFile: String): String;
function CreateAlphaMap(const ABitmap: TBitmap): TAlphaMap;

implementation

uses
  Settings, Math,
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, FMX.Forms, FMX.Dialogs,
  System.IOUtils, System.Hash;

function ImageToPyBytes(ABitmap: TBitmap): Variant;
var
  _stream : TMemoryStream;
  _bytes : PPyObject;
begin
  _stream := TMemoryStream.Create();
  try
    ABitmap.SaveToStream(_stream);
    // Form1.mmLog.Lines.Add('Bytes = ' + _stream.Size.ToString);
    _bytes := GetPythonEngine.PyBytes_FromStringAndSize(_stream.Memory, _stream.Size);
    Result := VarPythonCreate(_bytes);
    GetPythonEngine.Py_DECREF(_bytes);
  finally
    _stream.Free;
  end;
end;


function CreateAlphaMap(const ABitmap: TBitmap): TAlphaMap;
var
  Alpha: Integer;
  I: Integer;
  CurrentData: TBitmapData;
  color: TAlphaColor;
  txt: TStringList;
begin
  if Assigned(ABitmap) then
    begin
      Result.AlphasUsed := 0;
      Result.PixelCount := 0;
      Result.MostAlpha := 0;
      Result.MinAlpha := -1;
      Result.MaxAlpha := -1;
      Result.MostAlphaCount := 0;

      for I := 0 to 255 do
        Result.AlphaMap[I] := 0;

      try
        try
          if ABitmap.Map(TMapAccess.Read, CurrentData) then
            begin
              for var x := 0 to ABitmap.Width - 1 do
                for var y := 0 to ABitmap.Height - 1 do
                  begin
                    Inc(Result.PixelCount);
                    {$RANGECHECKS ON}
                    Color := CurrentData.GetPixel(x, y);
                    {$RANGECHECKS OFF}
                    Alpha := TAlphaColorRec(Color).A;
                    Inc(Result.AlphaMap[Alpha]);
                  end;
            end;
        except
          on E: Exception do
            begin
              ShowMessage('Unhandled Exception in CreateAlpha'
                + sLineBreak + 'Class : ' + E.ClassName
                + sLineBreak + 'Error : ' + E.Message);
            end;
        end;
      finally
        ABitmap.UnMap(CurrentData);
      end;

      for I := 0 to 255 do
        begin
          if Result.AlphaMap[I] > 0 then
            begin
              Inc(Result.AlphasUsed);
              if I > 0 then // Ignore Alpha #0
                if Result.AlphaMap[I] >= Result.MostAlphaCount then
                  begin
                    Result.MostAlpha := I;
                    Result.MostAlphaCount := Result.AlphaMap[I];
                  end;
              if Result.MinAlpha = -1 then
                Result.MinAlpha := I;
              Result.MaxAlpha := I;
            end;
        end;

      Result.HasAlphaMap := (Result.AlphasUsed <> 1);

      txt := TStringList.Create;

      for I := 0 to 255 do
        txt.Add(IntToStr(I) + ', ' + IntToStr(Result.AlphaMap[I]));
      txt.SaveToFile(IncludeTrailingPathDelimiter(CachePath) + 'alphamap.txt');
      FreeAndNil(txt);
    end;
end;


function GetFileHash(const AFile: String): String;
begin
  Result := String.Empty;
  if FileExists(AFile) then
    Result := System.Hash.THashMD5.GetHashStringFromFile(AFile);
end;

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
