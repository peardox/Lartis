unit StyleModel;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, JSON.Serializers;

type
  TModelImage = record
    iTitle: String;
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
    image: TModelImage;
    models: TModelStyleArray;
  end;

function LoadJson(const AJsonFile: string): String;
function DecodeJsonStyle(const JsonStyleText: String): TModelStyleCollection;

implementation

uses
  System.IOUtils,
  FMX.Dialogs,
  PythonSystem;

function LoadJson(const AJsonFile: string): String;
begin
  if FileExists(AJsonFile) then
    begin
      try
        Result := TFile.ReadAllText(AJsonFile);
      except
        ShowMessage('Can''t read shader ''' + AJsonFile + '''');
      end;
    end
  else
    begin
      ShowMessage('Can''t find json ''' + AJsonFile + '''');
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
      PySys.Log(JsonStyleText);
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

end.
