unit StyleModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, JSON.Serializers;

type
  TModelImage = record
    iTitle: String;
    iName: String;
    iDesc: String;
    iType: String;
    iWidth: Integer;
    iHeight: Integer;
    iHash: String;
    sGroup: String;
  end;

  TModelStyle = record
    mModel: String;
    mZoom: Integer;
    mType: String;
    mNet: String;
    mLimit: Integer;
    mEpochs: Integer;
    mDataset: String;
    mBatch: Integer;
    mImageSize: Integer;
    mStyleWeight: Single;
    mImageWeight: Single;
    mChannels: Integer;
    mHash: String;
    sGroup: String;
  end;
  TModelStyleArray = Array of TModelStyle;

  TModelStyleCollection = record
    fpath: String;
    image: TModelImage;
    models: TModelStyleArray;
  end;
  TModelStyleCollectionArray = Array of TModelStyleCollection;

  TStyleModels = class(TComponent)
    private
      FCollection: TModelStyleCollectionArray;
      function GetCollection(Index: Integer): TModelStyleCollection;
      procedure SetCollection(Index: Integer; Value: TModelStyleCollection);
    public
      constructor Create(AOwner: TComponent); override;
      procedure GetAllModels;
      function Count: Integer;
      property Collection[Index: Integer]: TModelStyleCollection read GetCollection write SetCollection;
  end;

function LoadJson(const AJsonFile: string): TModelStyleCollection;
function DecodeJsonStyle(const JsonStyleText: String): TModelStyleCollection;
procedure GetModelJson(var JsonList: TStringList; const AltModelDir: String = ''; const ModelSubDir: String = '');

implementation

uses
  Settings,
  System.IOUtils,
  FMX.Dialogs,
  PythonSystem;

constructor TStyleModels.Create(AOwner: TComponent);
begin
  inherited;
  SetLength(FCollection, 0);
end;

function TStyleModels.Count: Integer;
begin
  Result := Length(FCollection)
end;

function TStyleModels.GetCollection(Index: Integer): TModelStyleCollection;
begin
  if (Index >= 0 ) and (Index < Length(FCollection)) then
    begin
      Result := FCollection[Index];
    end;
end;

procedure TStyleModels.SetCollection(Index: Integer; Value: TModelStyleCollection);
begin
  if (Index >= 0 ) and (Index < Length(FCollection)) then
    begin
      FCollection[Index] := Value;
    end;
end;

procedure TStyleModels.GetAllModels;
var
  JSonList: TStringList;
  I: Integer;
  fn: String;
  RecCount: Integer;
begin
  RecCount := 0;
  JSonList := TStringList.Create;
  try
    GetModelJson(JsonList);
    if JsonList.Count > 0 then
      begin
        SetLength(FCollection, JsonList.Count);
        for I := 0 to JsonList.Count - 1 do
          begin
            fn := JsonList[I] + System.IOUtils.TPath.DirectorySeparatorChar + 'styles.json';
            if FileExists(fn) then
              begin
                FCollection[I] := LoadJson(fn);
                Inc(RecCount);
              end;
          end;
        SetLength(FCollection, RecCount);
      end;
  finally
    FreeAndNil(JsonList);
  end;
end;

function LoadJson(const AJsonFile: string): TModelStyleCollection;
var
  JsonText: String;
begin
  try
    JsonText := TFile.ReadAllText(AJsonFile);
    Result := DecodeJsonStyle(JsonText);
    Result.fpath := TPath.GetDirectoryName(AJsonFile);
  except
    ShowMessage('Can''t read model info ''' + AJsonFile + '''');
  end;
end;

function DecodeJsonStyle(const JsonStyleText: String): TModelStyleCollection;
var
  lSerializer: TJsonSerializer;
  obj: TModelStyleCollection;
begin

  lSerializer := TJsonSerializer.Create;
  try
    try
      obj := lSerializer.Deserialize<TModelStyleCollection>(JsonStyleText);
   except
     on E : Exception do
     begin
       PySys.Log('Exception class name = '+E.ClassName);
       PySys.Log('Exception message = '+E.Message);
       PySys.Log(JsonStyleText);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
  end;
  Result := obj;
end;

procedure GetModelJson(var JsonList: TStringList; const AltModelDir: String = ''; const ModelSubDir: String = '');
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
                    GetModelJson(JsonList, modeldir, FileName)
                  else
                    GetModelJson(JsonList, modeldir, ModelSubDir + System.IOUtils.TPath.DirectorySeparatorChar + FileName);
                end;
            end;
          until FindNext(SearchRec) <> 0;
          FindClose(SearchRec);
        end;
    end;
end;


end.
