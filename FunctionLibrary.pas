unit FunctionLibrary;

interface

uses
  FMX.Graphics;

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

function LoadShader(const AShaderFile: String): String;
function FitInsideContainer(const AContainerWidth: Single; const AContentWidth: Single; const AContainerHeight: Single; const AContentHeight: Single): Single;
function BoolToInt(const AValue: Boolean): Integer;
function GetFileHash(const AFile: String): String;
function CreateAlphaMap(const ABitmap: TBitmap): TAlphaMap;
procedure GetAllModels(const AltModelDir: String = ''; const ModelSubDir: String = '');
procedure GetModelJson(const AltModelDir: String = ''; const ModelSubDir: String = '');

implementation

uses
  Settings, Math,
  System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants, FMX.Forms, FMX.Dialogs,
  System.IOUtils, System.Hash;

function CreateAlphaMap(const ABitmap: TBitmap): TAlphaMap;
var
  Alpha: Integer;
  AlphaMap: Array [0..255] of Integer;
  I: Integer;
  CurrentData: TBitmapData;
  AlphasUsed: Integer;
  PixelCount: Integer;
  color: TAlphaColor;
  MinAlpha, MaxAlpha, MostAlpha, MostAlphaCount: Integer;
  txt: TStringList;
begin
  if Assigned(ABitmap) then
    begin
      AlphasUsed := 0;
      PixelCount := 0;
      MostAlpha := 0;
      MinAlpha := -1;
      MaxAlpha := -1;
      MostAlphaCount := 0;

      for I := 0 to 255 do
        AlphaMap[I] := 0;

      try
        try
          if ABitmap.Map(TMapAccess.Read, CurrentData) then
            begin
              for var x := 0 to ABitmap.Width - 1 do
                for var y := 0 to ABitmap.Height - 1 do
                  begin
                    Inc(PixelCount);
                    {$RANGECHECKS ON}
                    Color := CurrentData.GetPixel(x, y);
                    {$RANGECHECKS OFF}
                    Alpha := TAlphaColorRec(Color).A;
                    Inc(AlphaMap[Alpha]);
                  end;
            end;
        except
          on E: Exception do
            begin
              ShowMessage('Unhandled Exception'
                + sLineBreak + 'Class : ' + E.ClassName
                + sLineBreak + 'Error : ' + E.Message);
            end;
        end;
      finally
        ABitmap.UnMap(CurrentData);
      end;

      for I := 0 to 255 do
        begin
          if AlphaMap[I] > 0 then
            begin
              Inc(AlphasUsed);
              if I > 0 then // Ignore Alpha #0
                if AlphaMap[I] >= MostAlphaCount then
                  begin
                    MostAlpha := I;
                    MostAlphaCount := AlphaMap[I];
                  end;
              if MinAlpha = -1 then
                MinAlpha := I;
              MaxAlpha := I;
            end;
        end;
{
      ShowMessage('AlphasUsed = ' + IntToStr(AlphasUsed)
                 + sLineBreak + 'MinAlpha = ' + IntToStr(MinAlpha)
                 + sLineBreak + 'MaxAlpha = ' + IntToStr(MaxAlpha)
                 + sLineBreak + 'MostAlpha = ' + IntToStr(MostAlpha)
                 + ' (' + FormatFloat('##0.00', (MostAlphaCount / PixelCount) * 100) + '%)'
                 + sLineBreak + 'MostAlphaCount = ' + IntToStr(MostAlphaCount)
                 + sLineBreak + 'Pixels = ' + IntToStr(PixelCount)
      );
}
      txt := TStringList.Create;

      for I := 0 to 255 do
        txt.Add(IntToStr(I) + ', ' + IntToStr(AlphaMap[I]));
      txt.SaveToFile('c:\temp\alphamap.txt');

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

procedure GetAllModels(const AltModelDir: String = ''; const ModelSubDir: String = '');
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

procedure GetModelJson(const AltModelDir: String = ''; const ModelSubDir: String = '');
var
  SearchRec: TSearchRec;
  ModelDir: String;
  FileSpec: String;
  FileName: String;
begin
  if (AltModelDir = String.Empty) then
    Modeldir := IncludeTrailingPathDelimiter(AppHome) + 'models'
  else
    ModelDir := AltModelDir;

  if (ModelSubDir = String.Empty) then
    FileSpec := IncludeTrailingPathDelimiter(modeldir)
  else
    FileSpec := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(modeldir) + ModelSubDir);

  {$ifdef MSWINDOWS}
  FileSpec := filespec + '*.*';
  {$ELSE}
  filespec := filespec + '*';
  {$ENDIF}

  if not Assigned(JsonList) then
    JsonList := TStringList.Create
  else if (ModelSubDir = String.Empty) then
    JsonList.Clear;

  if DirectoryExists(modeldir) then
    begin
      if (FindFirst(filespec, faAnyFile, SearchRec) = 0) then
        begin
          repeat
            FileName := SearchRec.Name;
            if ((SearchRec.Attr and faDirectory) = 0) then
            begin
              if FileName.ToLower = 'styles.json' then
                begin
                  if (ModelSubDir <> String.Empty) then
                    FileName := ModelSubDir; // + System.IOUtils.TPath.DirectorySeparatorChar + FileName;
                  JsonList.Add(ModelDir  + System.IOUtils.TPath.DirectorySeparatorChar + FileName);
                end;
            end
          else
            begin
              if (FileName <> '.') and (FileName <> '..') then
                begin
                  if (ModelSubDir = String.Empty) then
                    GetModelJson(modeldir, FileName)
                  else
                    GetModelJson(modeldir, ModelSubDir + System.IOUtils.TPath.DirectorySeparatorChar + FileName);
                end;
            end;
          until FindNext(SearchRec) <> 0;
          FindClose(SearchRec);
        end;
    end;
end;

end.
