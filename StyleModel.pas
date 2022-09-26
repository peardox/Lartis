unit StyleModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, JSON.Serializers,
  FMX.Layouts, FMX.Types, FMX.StdCtrls, FMX.Objects;

type
  TStyleProc = procedure(Sender: TObject; const APath: String; const AModel: String) of object;

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
    FPath: String;
    Image: TModelImage;
    Models: TModelStyleArray;
  private
    function GetModel(Index: Integer): TModelStyle;
    procedure SetModel(Index: Integer; Value: TModelStyle);
  public
    function ImageFilename: String;
    function Count: Integer;
    property StylePath: String read fpath write fpath;
    property Model[Index: Integer]: TModelStyle read GetModel write SetModel;
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

  TStyleSelector = class(TLayout)
    layInfo: TLayout;
    lblName: TLabel;
    lblScale: TLabel;
    imgThumb: TImage;
    trkScale: TTrackBar;
    procedure DoClick(Sender: TObject);
  private
    FRunStyle: TStyleProc;
    FScale: Integer;
    FStyleModel: TModelStyleCollection;
    procedure SetStyle(AStyle: TModelStyleCollection);
    procedure DoResize(Sender: TObject);
    procedure ChangeScale(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Style: TModelStyleCollection read FStyleModel write SetStyle;
    property RunStyle: TStyleProc read FRunStyle write FRunStyle;
  end;
  TStyleSelectorArray = Array of TStyleSelector;


function LoadJson(const AJsonFile: string): TModelStyleCollection;
function DecodeJsonStyle(const JsonStyleText: String): TModelStyleCollection;
procedure GetModelJson(var JsonList: TStringList; const AltModelDir: String = ''; const ModelSubDir: String = '');

implementation

uses
  Settings,
  Math,
  System.IOUtils,
  FMX.Dialogs, // REMOVE_ME
  DebugForm, // REMOVE_ME
  PythonSystem;

constructor TStyleSelector.Create(AOwner: TComponent);
begin
  inherited;
  Parent := TFmxObject(AOwner);
  Align := TAlignLayout.Top;

  Width := 160;
  Height := 128;
  Position.X := 0;
  Position.Y := 0;

  trkScale := TTrackBar.Create(Self);
  trkScale.Align := TAlignLayout.Bottom;
  trkScale.Parent := Self;
  trkScale.OnChange := ChangeScale;

  layInfo := TLayout.Create(Self);
  layInfo.Align := TAlignLayout.Bottom;
  layInfo.Height := 30;
  layInfo.Parent := Self;

  lblName := TLabel.Create(layInfo);
  lblName.TextSettings.HorzAlign := TTextAlign.Leading;
  lblName.Height := 30;
  lblName.Position.X := 8;
  lblName.Position.Y := 0;
  lblName.Parent := layInfo;

  lblScale := TLabel.Create(layInfo);
  lblScale.Position.Y := 0;
  lblScale.Margins.Right := 8;
  lblScale.Parent := layInfo;

  imgThumb := TImage.Create(Self);
  imgThumb.Align := TAlignLayout.Client;
  imgThumb.Parent := Self;
  imgThumb.OnClick := DoClick;

  FScale := 0;
  OnResize := DoResize;
end;

procedure TStyleSelector.ChangeScale(Sender: TObject);
var
  ScaleIndex: Integer;
  mdl: TModelStyle;
begin
  ScaleIndex := floor(trkScale.Value);
  mdl := FStyleModel.Models[ScaleIndex];

  lblScale.Text := FormatFloat('0.00', mdl.mZoom / 100);
end;

procedure TStyleSelector.DoClick(Sender: TObject);
var
  ScaleIndex: Integer;
  mdl: TModelStyle;
  path: String;
  model: String;
begin
  ScaleIndex := floor(trkScale.Value);
  mdl := FStyleModel.Models[ScaleIndex];
  path := FStyleModel.fpath;
  model := mdl.mModel + '-' + IntToStr(mdl.mZoom);
{
  frmDebug.Show;

  PySys.Log('Click3ed on ' + FStyleModel.Image.iTitle + ' - index = ' + IntToStr(ScaleIndex));
  PySys.Log(FStyleModel.fpath + ' has ' + Length(FStyleModel.models).ToString + ' models');
  PySys.Log(FStyleModel.image.iTitle);
  PySys.Log(FStyleModel.image.iName);
  PySys.Log(FStyleModel.image.iDesc);
  PySys.Log(FStyleModel.image.iType);
  PySys.Log(FStyleModel.image.iWidth.ToString);
  PySys.Log(FStyleModel.image.iHeight.ToString);
  PySys.Log(FStyleModel.image.iHash);
  PySys.Log(FStyleModel.image.sGroup);

  PySys.Log('mModel = ' + mdl.mModel);
  PySys.Log('mZoom = ' + IntToStr(mdl.mZoom));
  PySys.Log(path);
  PySys.Log(model);
}
  if Assigned(FRunStyle) then
    FRunStyle(Self, path, model);
end;

procedure TStyleSelector.DoResize(Sender: TObject);
begin
  if Assigned(Self.lblScale) then
    begin
      lblName.Width := floor((layInfo.Width * 8) / 10);
      lblScale.Position.X := lblName.Width;
      lblScale.Width := floor((layInfo.Width * 2) / 10);
      lblScale.Height := 30;
    end;
end;

procedure TStyleSelector.SetStyle(AStyle: TModelStyleCollection);
begin
  FStyleModel := AStyle;
  lblName.Text := AStyle.Image.iTitle;
  lblScale.Text := '1.00';
  var fn := TPath.Combine(AStyle.StylePath,
        AStyle.ImageFilename);
  imgThumb.Bitmap.LoadFromFile(fn);

  trkScale.Min := 0;
  trkScale.Max := AStyle.Count - 1;
  trkScale.Frequency := 1;

//  DoResize(Self);
end;

function TModelStyleCollection.ImageFilename: String;
begin
  Result := image.iName + '.' + image.iType;
end;

function TModelStyleCollection.Count: Integer;
begin
  Result := Length(Models)
end;

function TModelStyleCollection.GetModel(Index: Integer): TModelStyle;
begin
  if (Index >= 0 ) and (Index < Length(Models)) then
    begin
      Result := Models[Index];
    end;
end;

procedure TModelStyleCollection.SetModel(Index: Integer; Value: TModelStyle);
begin
  if (Index >= 0 ) and (Index < Length(Models)) then
    begin
      Models[Index] := Value;
    end;
end;

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
