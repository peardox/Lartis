unit Modules;

interface
uses
  System.SysUtils, System.IOUtils, System.Threading, System.Types,
  System.UITypes, System.Classes, System.Variants, FMX.Dialogs,
  JSON.Serializers,
  PythonEngine, PyCommon, PyModule, VarPyth;

type
  TModProgressEvent = procedure(Sender: TObject; const AValue: Single) of object;
  TModFinishedEvent = procedure(Sender: TObject; const AFile: String) of object;
  TModErrorEvent = procedure(Sender: TObject; const AString: String) of object;

  TPyIOOptions = record
    TrainJsonLog: String;
    TrainAbortFlag: Boolean;
    TrainSampleFlag: Boolean;
    SampleFilename: String;
    StyleAbortFlag: Boolean;
    StyleFilename: String;
    StyleJsonLog: String;
  end;

  TTrainOptions = record
    dataset: String;
    style_image: String;
    model_name: String;
    model_dir: String;
    model_ext: String;
    checkpoint_model_dir: String;
    net: String;
    vgg16_path: String;
    vgg19_path: String;
    logfile: String;
    epochs: Integer;
    limit: Integer;
    batch_size: Integer;
    image_size: Integer;
    seed: Integer;
    content_weight: Single;
    style_weight: Single;
    lr: Single;
    style_scale: Single;
    channels: Integer;
    force_size: Boolean;
    ignore_gpu: Boolean;
    log_event_api: Boolean;
    calibrating: Boolean;
  end;

  TStyleOptions = record
    content_image: String;
    content_image_raw: String;
    output_image: String;
    model: String;
    model_dir: String;
    model_ext: String;
    logfile: String;
    content_scale: Single;
    ignore_gpu: Boolean;
    export_onnx: Boolean;
    add_model_ext: Boolean;
    log_event_api: Boolean;
    calibrating: Boolean;
  end;

  TTrainLog = record
    image_count: Integer;
    train_elapsed: Integer;
    train_interval: Single;
    content_loss: Integer;
    style_loss: Integer;
    total_loss: Integer;
    reporting_line: Integer;
    train_completion: Single;
    total_images: Integer;
    train_eta: Integer;
    train_left: Integer;
    train_delta: Single;
  end;

  TStyleLog = record
    event: Integer;
    subevent: Integer;
    time: Single;
  end;

  TModTrain = class(TPythonModule)
  private
    FTask: ITask;
    FOptions: TTrainOptions;
    FModProgressEvent: TModProgressEvent;
    FModFinishedEvent: TModFinishedEvent;
    FModErrorEvent: TModErrorEvent;
    procedure DoProgress(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure DoFinished(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure DoModProgressEvent(const AValue: Single);
    procedure DoModFinishedEvent(const AFile: String);
    procedure DoModErrorEvent(const AString: String);
  public
    ProgressCount: Integer;
    property Options: TTrainOptions read FOptions write FOptions;
    constructor Create(AOwner: TComponent); override;
    procedure CreateDefaultOptions;
    procedure InitializeModule(Sender: TObject);
    function GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
    procedure Train(const AFile: String; const AModel: String); overload;
    procedure Train(const AFile: String; const AModel: String; const StyleWeight: Double); overload;
    procedure TrainAll(const AFile: String);
  published
    property ModProgressEvent: TModProgressEvent read FModProgressEvent write FModProgressEvent;
    property ModFinishedEvent: TModFinishedEvent read FModFinishedEvent write FModFinishedEvent;
    property ModErrorEvent: TModErrorEvent read FModErrorEvent write FModErrorEvent;
  end;

  TModStyle = class(TPythonModule)
  private
    FTask: ITask;
    FOptions: TStyleOptions;
    FModProgressEvent: TModProgressEvent;
    FModFinishedEvent: TModFinishedEvent;
    FModErrorEvent: TModErrorEvent;
    procedure DoProgress(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure DoFinished(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
    procedure DoModProgressEvent(const AValue: Single);
    procedure DoModFinishedEvent(const AFile: String);
    procedure DoModErrorEvent(const AString: String);
  public
    ProgressCount: Integer;
    property Options: TStyleOptions read FOptions write FOptions;
    constructor Create(AOwner: TComponent); override;
    procedure CreateDefaultOptions;
    procedure InitializeModule(Sender: TObject);
    function GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
    procedure Stylize(const AFile: String; OnProgress: TModProgressEvent = Nil; OnFinished: TModFinishedEvent = Nil; OnError: TModErrorEvent = Nil);
    procedure StylizeAll(const AFile: String);
  published
    property ModProgressEvent: TModProgressEvent read FModProgressEvent write FModProgressEvent;
    property ModFinishedEvent: TModFinishedEvent read FModFinishedEvent write FModFinishedEvent;
    property ModErrorEvent: TModErrorEvent read FModErrorEvent write FModErrorEvent;
  end;

  TModPyIO = class(TPythonModule)
  private
    FOptions: TPyIOOptions;
  public
    property Options: TPyIOOptions read FOptions write FOptions;
    constructor Create(AOwner: TComponent); override;
    procedure CreateDefaultOptions;
    procedure InitializeModule(Sender: TObject);
    function GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
    function GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
  end;

  TModHelper = class helper for TPythonModule
    function PyType_CheckExact( obj : PPyObject ) : Boolean;
    function GetTypeAsString( obj : PPyObject ) : string;
  end;


implementation

uses
  Settings,
  StyleForm,
//  TrainForm,
  FMX.Forms,
  PythonSystem;

///// Helper for TPythonModule /////

function TModHelper.PyType_CheckExact( obj : PPyObject ) : Boolean;
begin
  Result := Assigned( obj ) and (obj^.ob_type = PPyTypeObject(Engine.PyType_Type));
end;

function TModHelper.GetTypeAsString( obj : PPyObject ) : string;
begin
  if PyType_CheckExact( obj ) then
    Result := string(PPyTypeObject(obj).tp_name)
  else
    Result := Engine.PyObjectAsString(obj);
end;

///// Training Module Definitions /////

constructor TModTrain.Create(AOwner: TComponent);
begin
  inherited;
  OnInitialization := InitializeModule;
  CreateDefaultOptions;
end;

procedure TModTrain.DoModProgressEvent(const AValue: Single);
begin
  if Assigned(FModProgressEvent) then
    FModProgressEvent(Self, AValue);
end;

procedure TModTrain.DoModFinishedEvent(const AFile: String);
begin
  if Assigned(FModFinishedEvent) then
    FModFinishedEvent(Self, AFile);
end;

procedure TModTrain.DoModErrorEvent(const AString: String);
begin
  if Assigned(FModErrorEvent) then
    FModErrorEvent(Self, AString);
end;

procedure TModTrain.CreateDefaultOptions;
begin
  FOptions.dataset := IncludeTrailingPathDelimiter(AppHome) + 'datasets/train/unsplash/lite/256';
  FOptions.style_image := IncludeTrailingPathDelimiter(AppHome) + 'style-images/wall_800x510.jpg';
  FOptions.model_name := 'test-model';
  FOptions.model_dir := 'models';
  FOptions.model_ext := '.pth';
  FOptions.checkpoint_model_dir := 'cache';
  FOptions.net := 'vgg16';
  Foptions.vgg16_path := IncludeTrailingPathDelimiter(AppHome) + 'pretrained/vgg16-397923af.pth';
  Foptions.vgg19_path := IncludeTrailingPathDelimiter(AppHome) + 'pretrained/vgg19-dcbb9e9d.pth';
  FOptions.logfile := '';
  FOptions.epochs := 2;
  FOptions.limit := 50000;
  FOptions.batch_size := 8;
  FOptions.image_size := 256;
  FOptions.seed := 42;
  FOptions.content_weight := 1e5;
  FOptions.style_weight := 1e10;
  FOptions.lr := 1e-3;
  FOptions.style_scale := 1;
  FOptions.channels := 32;
  FOptions.force_size := True;
  FOptions.ignore_gpu := False;
  FOptions.log_event_api := True;
  FOptions.calibrating := False;
end;

procedure TModTrain.InitializeModule(Sender: TObject);
var
  ev: TEventDef;
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'GetProperty', GetProperty, 'GetProperty(PropName) -> PropValue' );
      AddDelphiMethod( 'SetProperty', SetProperty, 'SetProperty(PropName, PropValue) -> None' );
      AddDelphiMethod( 'GetPropertyList', GetPropertyList, 'GetPropertyList() -> List of property names' );

      ev := TEventDef.Create(Events);
      ev.Name := 'TrainProgress';
      ev.OnExecute := DoProgress;

      ev := TEventDef.Create(Events);
      ev.Name := 'TrainFinished';
      ev.OnExecute := DoFinished;
    end;
end;

function TModTrain.GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:GetProperty',@key ) <> 0 then
      begin
        if key = 'dataset' then
          Result := VariantAsPyObject(FOptions.dataset)
        else if key = 'style_image' then
          Result := VariantAsPyObject(FOptions.style_image)
        else if key = 'logfile' then
          Result := VariantAsPyObject(FOptions.logfile)
        else if key = 'net' then
          Result := VariantAsPyObject(FOptions.net)
        else if key = 'vgg16_path' then
          Result := VariantAsPyObject(FOptions.vgg16_path)
        else if key = 'vgg19_path' then
          Result := VariantAsPyObject(FOptions.vgg19_path)
        else if key = 'checkpoint_model_dir' then
          Result := VariantAsPyObject(FOptions.checkpoint_model_dir)
        else if key = 'model_ext' then
          Result := VariantAsPyObject(FOptions.model_ext)
        else if key = 'model_dir' then
          Result := VariantAsPyObject(FOptions.model_dir)
        else if key = 'model_name' then
          Result := VariantAsPyObject(FOptions.model_name)
        else if key = 'epochs' then
          Result := VariantAsPyObject(FOptions.epochs)
        else if key = 'limit' then
          Result := VariantAsPyObject(FOptions.limit)
        else if key = 'batch_size' then
          Result := VariantAsPyObject(FOptions.batch_size)
        else if key = 'image_size' then
          Result := VariantAsPyObject(FOptions.image_size)
        else if key = 'seed' then
          Result := VariantAsPyObject(FOptions.seed)
        else if key = 'content_weight' then
          Result := VariantAsPyObject(FOptions.content_weight)
        else if key = 'style_weight' then
          Result := VariantAsPyObject(FOptions.style_weight)
        else if key = 'lr' then
          Result := VariantAsPyObject(FOptions.lr)
        else if key = 'style_scale' then
          Result := VariantAsPyObject(FOptions.style_scale)
        else if key = 'channels' then
          Result := VariantAsPyObject(FOptions.channels)
        else if key = 'force_size' then
          Result := VariantAsPyObject(FOptions.force_size)
        else if key = 'ignore_gpu' then
          Result := VariantAsPyObject(FOptions.ignore_gpu)
        else if key = 'log_event_api' then
          Result := VariantAsPyObject(FOptions.log_event_api)
        else if key = 'calibrating' then
          Result := VariantAsPyObject(FOptions.calibrating)
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModTrain.SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
  value : PPyObject;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:SetProperty',@key, @value ) <> 0 then
      begin
        if key = 'dataset' then
          begin
            FOptions.dataset := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'style_image' then
          begin
            FOptions.style_image := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_name' then
          begin
            FOptions.model_name := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_dir' then
          begin
            FOptions.model_dir := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_ext' then
          begin
            FOptions.model_ext := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'checkpoint_model_dir' then
          begin
            FOptions.checkpoint_model_dir := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'net' then
          begin
            FOptions.net := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'vgg16_path' then
          begin
            FOptions.vgg16_path := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'vgg19_path' then
          begin
            FOptions.vgg19_path := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'logfile' then
          begin
            FOptions.logfile := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'epochs' then
          begin
            FOptions.epochs := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'limit' then
          begin
            FOptions.limit := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'batch_size' then
          begin
            FOptions.batch_size := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'image_size' then
          begin
            FOptions.image_size := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'seed' then
          begin
            FOptions.seed := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'content_weight' then
          begin
            FOptions.content_weight := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'style_weight' then
          begin
            FOptions.style_weight := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'lr' then
          begin
            FOptions.lr := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'style_scale' then
          begin
            FOptions.style_scale := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'channels' then
          begin
            FOptions.channels := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'force_size' then
          begin
            FOptions.force_size := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'ignore_gpu' then
          begin
            FOptions.ignore_gpu := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'log_event_api' then
          begin
            FOptions.log_event_api := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'calibrating' then
          begin
            FOptions.calibrating := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModTrain.GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(24);
      PyList_SetItem(Result,  0, PyUnicodeFromString('dataset'));
      PyList_SetItem(Result,  1, PyUnicodeFromString('style_image'));
      PyList_SetItem(Result,  2, PyUnicodeFromString('model_name'));
      PyList_SetItem(Result,  3, PyUnicodeFromString('model_dir'));
      PyList_SetItem(Result,  4, PyUnicodeFromString('model_ext'));
      PyList_SetItem(Result,  5, PyUnicodeFromString('checkpoint_model_dir'));
      PyList_SetItem(Result,  6, PyUnicodeFromString('net'));
      PyList_SetItem(Result,  7, PyUnicodeFromString('vgg16_path'));
      PyList_SetItem(Result,  8, PyUnicodeFromString('vgg19_path'));
      PyList_SetItem(Result,  9, PyUnicodeFromString('logfile'));
      PyList_SetItem(Result, 10, PyUnicodeFromString('epochs'));
      PyList_SetItem(Result, 11, PyUnicodeFromString('limit'));
      PyList_SetItem(Result, 12, PyUnicodeFromString('batch_size'));
      PyList_SetItem(Result, 13, PyUnicodeFromString('image_size'));
      PyList_SetItem(Result, 14, PyUnicodeFromString('seed'));
      PyList_SetItem(Result, 15, PyUnicodeFromString('content_weight'));
      PyList_SetItem(Result, 16, PyUnicodeFromString('style_weight'));
      PyList_SetItem(Result, 17, PyUnicodeFromString('lr'));
      PyList_SetItem(Result, 18, PyUnicodeFromString('style_scale'));
      PyList_SetItem(Result, 19, PyUnicodeFromString('channels'));
      PyList_SetItem(Result, 20, PyUnicodeFromString('force_size'));
      PyList_SetItem(Result, 21, PyUnicodeFromString('ignore_gpu'));
      PyList_SetItem(Result, 22, PyUnicodeFromString('log_event_api'));
      PyList_SetItem(Result, 23, PyUnicodeFromString('calibrating'));
    end;
end;

///// Train Module Events /////

procedure TModTrain.DoProgress(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
  procedure HandleLogLine(const ALogLine: String);
  var
    lSerializer: TJsonSerializer;
    log: TTrainLog;
  begin
    lSerializer := TJsonSerializer.Create;
    try
      try
        log := lSerializer.Deserialize<TTrainLog>(ALogLine);
        PySys.Log(ALogLine);
//        Application.ProcessMessages;
      except
       on E : Exception do
       begin
         PySys.Log('Exception class name = '+E.ClassName);
         PySys.Log('Exception message = '+E.Message);
         PySys.Log(ALogLine);
       end;
      end;
    finally
      FreeAndNil(lSerializer);
    end;
  end;
var
  jstr: Variant;
begin
  jstr := Engine.PyObjectAsString(Args);
//  HandleLogLine(jstr);
  PySys.Log(jstr);
  Result := Engine.ReturnNone;
end;

procedure TModTrain.DoFinished(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
var
  jstr: Variant;
begin
  jstr := Engine.PyObjectAsString(Args);

  PySys.Log(jstr);
  Result := Engine.ReturnNone;
end;

///// Train Module Procs /////
procedure TModTrain.Train(const AFile: String; const AModel: String);
begin
  Train(AFile, AModel, 1e10);
end;

procedure TModTrain.Train(const AFile: String; const AModel: String; const StyleWeight: Double);
begin
  ProgressCount := 0;
  FOptions.style_image := AFile;
  FOptions.model_name := AModel;
  FOptions.model_dir := IncludeTrailingPathDelimiter(AppHome) + 'models';
  FOptions.limit := 0;
  FOptions.net := 'vgg19';
//  FOptions.dataset := IncludeTrailingPathDelimiter(AppHome) + 'datasets/train/coco/2017/256';
  FOptions.dataset := IncludeTrailingPathDelimiter(AppHome) + 'datasets/train/unsplash/lite/256';
  FOptions.force_size := False;
  FOptions.epochs := 1;
  FOptions.batch_size := 16;
  FOptions.image_size := 256;
  FOptions.style_scale := 1;
  FOptions.style_weight := StyleWeight;
//  FOptions.ignore_gpu := True;
//  FOptions.batch_size := 1;

//  FOptions.ignore_gpu := True
  PySys.LogClear;
  if Assigned(FTask) then
    Pysys.Log('Calling Training Python - Task ID = ' + IntToHex(FTask.GetId))
  else
    Pysys.Log('Calling Training Python - Task ID = UnAssigned');

  // _im :=
  FTask := TTask.Run(
    procedure()
      begin
        TThread.Synchronize(nil,
          procedure()
          begin
            MainModule.delphi_train();
            // FTask.CheckCanceled();
            PySys.Log('Training Task Finished');
          end
          )
      end
    );
  if Assigned(FTask) then
    Pysys.Log('Back from Python Training - Task ID = ' + IntToHex(FTask.GetId))
  else
    Pysys.Log('Back from Python Training - Task ID = UnAssigned');
//  Pysys.Log('_im (' + Result + ') is a ' + VarTypeAsText(VarType(_im)));
end;

procedure TModTrain.TrainAll(const AFile: String);
var
  _im: Variant;
  OutFile: String;
  batchdir: String;
begin
  ShowMessage('Not Safe Yet');
  Exit;

  batchdir := IncludeTrailingPathDelimiter(AppHome) + 'batch'; // /zoom';
  if not DirectoryExists(batchdir) then
    ForceDirectories(batchdir);

  if not Assigned(ModelList) then
    GetAllModels; // (zoom_models);

  FOptions.style_image := AFile;
//  FOptions.model_dir := 'zoom_models';
//  FOptions.ignore_gpu := True;

  for var I := 0 to ModelList.Count - 1 do
    begin
      FOptions.model_name := IncludeTrailingPathDelimiter(batchdir) + ModelList[I] + '.jpg';
      _im := MainModule.delphi_train();

      OutFile := _im;
      Pysys.Log('_im (' + OutFile + ') is a ' + VarTypeAsText(VarType(_im)));
    end;
end;

///// Style Module Definitions /////

constructor TModStyle.Create(AOwner: TComponent);
begin
  inherited;
  OnInitialization := InitializeModule;
  CreateDefaultOptions;
end;

procedure TModStyle.DoModProgressEvent(const AValue: Single);
begin
  if Assigned(FModProgressEvent) then
    FModProgressEvent(Self, AValue);
end;

procedure TModStyle.DoModFinishedEvent(const AFile: String);
begin
  if Assigned(FModFinishedEvent) then
    FModFinishedEvent(Self, AFile);
end;

procedure TModStyle.DoModErrorEvent(const AString: String);
begin
  if Assigned(FModErrorEvent) then
    FModErrorEvent(Self, AString);
end;

procedure TModStyle.CreateDefaultOptions;
begin
  ProgressCount := 0;
  FOptions.content_image := 'input-images/haywain.jpg';
  FOptions.content_image_raw := String.Empty;
  FOptions.output_image := 'output-images/lartis-mosaic-vgg19.jpg';
  FOptions.model := 'mosaic/mosaic-200';
  FOptions.model_dir := 'models';
  FOptions.model_ext := '.pth';
  FOptions.logfile := String.Empty;
  FOptions.content_scale := 1;
  FOptions.ignore_gpu := False;
  FOptions.export_onnx := False;
  FOptions.add_model_ext := True;
  FOptions.log_event_api := True;
  FOptions.calibrating := False;
end;

procedure TModStyle.InitializeModule(Sender: TObject);
var
  ev: TEventDef;
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'GetProperty', GetProperty, 'GetProperty(PropName) -> PropValue' );
      AddDelphiMethod( 'SetProperty', SetProperty, 'SetProperty(PropName, PropValue) -> None' );
      AddDelphiMethod( 'GetPropertyList', GetPropertyList, 'GetPropertyList() -> List of property names' );

      ev := TEventDef.Create(Events);
      ev.Name := 'StyleProgress';
      ev.OnExecute := DoProgress;

      ev := TEventDef.Create(Events);
      ev.Name := 'StyleFinished';
      ev.OnExecute := DoFinished;

    end;
end;

function TModStyle.GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:GetProperty',@key ) <> 0 then
      begin
        if key = 'content_image' then
          Result := VariantAsPyObject(FOptions.content_image)
        else if key = 'content_image_raw' then
          Result := VariantAsPyObject(FOptions.content_image_raw)
        else if key = 'output_image' then
          Result := VariantAsPyObject(FOptions.output_image)
        else if key = 'model' then
          Result := VariantAsPyObject(FOptions.model)
        else if key = 'model_dir' then
          Result := VariantAsPyObject(FOptions.model_dir)
        else if key = 'model_ext' then
          Result := VariantAsPyObject(FOptions.model_ext)
        else if key = 'logfile' then
          Result := VariantAsPyObject(FOptions.logfile)
        else if key = 'content_scale' then
          Result := VariantAsPyObject(FOptions.content_scale)
        else if key = 'ignore_gpu' then
          Result := VariantAsPyObject(FOptions.ignore_gpu)
        else if key = 'export_onnx' then
          Result := VariantAsPyObject(FOptions.export_onnx)
        else if key = 'add_model_ext' then
          Result := VariantAsPyObject(FOptions.add_model_ext)
        else if key = 'log_event_api' then
          Result := VariantAsPyObject(FOptions.log_event_api)
        else if key = 'calibrating' then
          Result := VariantAsPyObject(FOptions.calibrating)
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModStyle.SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
  value : PPyObject;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:SetProperty',@key, @value ) <> 0 then
      begin
        if key = 'content_image' then
          begin
            FOptions.content_image := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'content_image_raw' then
          begin
            FOptions.content_image_raw := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'output_image' then
          begin
            FOptions.output_image := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model' then
          begin
            FOptions.model := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_dir' then
          begin
            FOptions.model_dir := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'model_ext' then
          begin
            FOptions.model_ext := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'logfile' then
          begin
            FOptions.logfile := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'content_scale' then
          begin
            FOptions.content_scale := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'ignore_gpu' then
          begin
            FOptions.ignore_gpu := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'export_onnx' then
          begin
            FOptions.export_onnx := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'add_model_ext' then
          begin
            FOptions.add_model_ext := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'log_event_api' then
          begin
            FOptions.log_event_api := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'calibrating' then
          begin
            FOptions.calibrating := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModStyle.GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(13);
      PyList_SetItem(Result, 0, PyUnicodeFromString('content_image'));
      PyList_SetItem(Result, 1, PyUnicodeFromString('content_image_raw'));
      PyList_SetItem(Result, 2, PyUnicodeFromString('output_image'));
      PyList_SetItem(Result, 3, PyUnicodeFromString('model'));
      PyList_SetItem(Result, 4, PyUnicodeFromString('model_dir'));
      PyList_SetItem(Result, 5, PyUnicodeFromString('model_ext'));
      PyList_SetItem(Result, 6, PyUnicodeFromString('logfile'));
      PyList_SetItem(Result, 7, PyUnicodeFromString('content_scale'));
      PyList_SetItem(Result, 8, PyUnicodeFromString('ignore_gpu'));
      PyList_SetItem(Result, 9, PyUnicodeFromString('export_onnx'));
      PyList_SetItem(Result, 10, PyUnicodeFromString('add_model_ext'));
      PyList_SetItem(Result, 11, PyUnicodeFromString('log_event_api'));
      PyList_SetItem(Result, 12, PyUnicodeFromString('calibrating'));
    end;
end;

///// Style Module Events /////

procedure TModStyle.DoProgress(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
  procedure HandleLogLine(const ALogLine: String);
  var
    lSerializer: TJsonSerializer;
    log: TTrainLog;
  begin
    lSerializer := TJsonSerializer.Create;
    try
      try
        if ALogLine <> String.Empty then
          begin
            log := lSerializer.Deserialize<TTrainLog>(ALogLine);
            Inc(ProgressCount);
            DoModProgressEvent(ProgressCount / 45);
            Application.ProcessMessages;
          end;
      except
       on E : Exception do
       begin
         PySys.Log('Exception class name = '+E.ClassName);
         PySys.Log('Exception message = '+E.Message);
         PySys.Log('LogLine = ' + '"' + ALogLine + '"');
       end;
      end;
    finally
      FreeAndNil(lSerializer);
    end;
  end;
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TThread.Synchronize(nil,
      procedure()
      begin
        PySys.Log('InThread');
      end)
    else
      HandleLogLine(PySys.modPyIO.Options.StyleJsonLog);

//  PySys.Log('==> ' + );
  Result := Engine.ReturnNone;
end;

procedure TModStyle.DoFinished(Sender: TObject; PSelf, Args: PPyObject; var Result: PPyObject);
begin
  PySys.Log('FN > ' + PySys.modPyIO.Options.StyleFilename );
//  frmStyle.ShowStyledImage(Self, PySys.modPyIO.Options.StyleFilename);
  DoModFinishedEvent(PySys.modPyIO.Options.StyleFilename);
  Result := Engine.ReturnNone;
end;

///// Style Module Procs /////

procedure TModStyle.Stylize(const AFile: String; OnProgress: TModProgressEvent = Nil; OnFinished: TModFinishedEvent = Nil; OnError: TModErrorEvent = Nil);
begin
  ProgressCount := 0;

  FModProgressEvent := OnProgress;
  FModFinishedEvent := OnFinished;
  FModErrorEvent := OnError;

  DoModProgressEvent(0);

  FOptions.content_image := AFile;
  FOptions.output_image := IncludeTrailingPathDelimiter(AppHome) + 'output-images' + System.IOUtils.TPath.DirectorySeparatorChar + System.IOUtils.TPath.GetFileNameWithoutExtension(AFile) + '-tile_test2.jpg';
  FOptions.model := 'gothic-512/gothic-100';
  FOptions.ignore_gpu := False;
  PySys.LogClear;
  if Assigned(FTask) then
    Pysys.Log('Calling Python Stylize - Task ID = ' + IntToHex(FTask.GetId))
  else
    Pysys.Log('Calling Python Stylize - Task ID = UnAssigned');

  SafeMaskFPUExceptions(True);
  FTask := TTask.Run(
    procedure()
      begin
        TThread.Synchronize(nil,
          procedure()
          begin
            MainModule.delphi_style();
          end
          )
      end
    );
  SafeMaskFPUExceptions(False);

  if Assigned(FTask) then
    Pysys.Log('Back from Python Stylize - Task ID = ' + IntToHex(FTask.GetId))
  else
    Pysys.Log('Back from Python Stylize - Task ID = UnAssigned');

//  Pysys.Log('_im (' + Result + ') is a ' + VarTypeAsText(VarType(_im)));
end;

procedure TModStyle.StylizeAll(const AFile: String);
var
  _im: Variant;
  OutFile: String;
  batchdir: String;
begin
//  ShowMessage('Not Safe Yet');
//  Exit;

  batchdir := IncludeTrailingPathDelimiter(AppHome) + 'batch/haywain-1024x768'; // /zoom';
  if not DirectoryExists(batchdir) then
    ForceDirectories(batchdir);

  if not Assigned(ModelList) then
    GetAllModels('models/mosaic');

  FOptions.content_image := AFile;
  FOptions.model_dir := IncludeTrailingPathDelimiter(AppHome) +
    'models/mosaic';
//  FOptions.ignore_gpu := True;

  for var I := 0 to ModelList.Count - 1 do
    begin
      FOptions.output_image := IncludeTrailingPathDelimiter(batchdir) +
              ModelList[I] + '.jpg';
      FOptions.model := ModelList[I];
      _im := MainModule.delphi_style();

      OutFile := _im;
      Pysys.Log('_im (' + OutFile + ') is a ' + VarTypeAsText(VarType(_im)));
    end;
end;


///// PythonIO Module Definitions /////

constructor TModPyIO.Create(AOwner: TComponent);
begin
  inherited;
  OnInitialization := InitializeModule;
  CreateDefaultOptions;
end;

procedure TModPyIO.CreateDefaultOptions;
begin
  FOptions.TrainJsonLog := String.Empty;
  FOptions.TrainAbortFlag := False;
  FOptions.TrainSampleFlag := False;
  FOptions.SampleFilename:= String.Empty;
  FOptions.StyleAbortFlag := False;
  FOptions.StyleFilename := String.Empty;
  FOptions.StyleJsonLog := String.Empty;
end;

procedure TModPyIO.InitializeModule(Sender: TObject);
begin
  with Sender as TPythonModule do
    begin
      AddDelphiMethod( 'GetProperty', GetProperty, 'GetProperty(PropName) -> PropValue' );
      AddDelphiMethod( 'SetProperty', SetProperty, 'SetProperty(PropName, PropValue) -> None' );
      AddDelphiMethod( 'GetPropertyList', GetPropertyList, 'GetPropertyList() -> List of property names' );
    end;
end;

function TModPyIO.GetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 's:GetProperty',@key ) <> 0 then
      begin
        if key = 'TrainJsonLog' then
          Result := VariantAsPyObject(FOptions.TrainJsonLog)
        else if key = 'TrainAbortFlag' then
          Result := VariantAsPyObject(FOptions.TrainAbortFlag)
        else if key = 'TrainSampleFlag' then
          Result := VariantAsPyObject(FOptions.TrainSampleFlag)
        else if key = 'SampleFilename' then
          Result := VariantAsPyObject(FOptions.SampleFilename)
        else if key = 'StyleAbortFlag' then
          Result := VariantAsPyObject(FOptions.StyleAbortFlag)
        else if key = 'StyleFilename' then
          Result := VariantAsPyObject(FOptions.StyleFilename)
        else if key = 'StyleJsonLog' then
          Result := VariantAsPyObject(FOptions.StyleJsonLog)
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModPyIO.SetProperty(pSelf, Args : PPyObject) : PPyObject; cdecl;
var
  key : PAnsiChar;
  value : PPyObject;
begin
  with GetPythonEngine do
    if PyArg_ParseTuple( args, 'sO:SetProperty',@key, @value ) <> 0 then
      begin
        if key = 'TrainJsonLog' then
          begin
            FOptions.TrainJsonLog := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'TrainAbortFlag' then
          begin
            FOptions.TrainAbortFlag := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'TrainSampleFlag' then
          begin
            FOptions.TrainSampleFlag := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'SampleFilename' then
          begin
            FOptions.SampleFilename := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'StyleAbortFlag' then
          begin
            FOptions.StyleAbortFlag := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'StyleFilename' then
          begin
            FOptions.StyleFilename := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else if key = 'StyleJsonLog' then
          begin
            FOptions.StyleJsonLog := PyObjectAsVariant( value );
            Result := ReturnNone;
          end
        else
          begin
            PyErr_SetString (PyExc_AttributeError^, PAnsiChar(Format('Unknown property "%s"', [key])));
            Result := nil;
          end;
      end
    else
      Result := nil;
end;

function TModPyIO.GetPropertyList(pSelf, Args : PPyObject) : PPyObject; cdecl;
begin
  with GetPythonEngine do
    begin
      Result := PyList_New(7);
      PyList_SetItem(Result, 0, PyUnicodeFromString('TrainJsonLog'));
      PyList_SetItem(Result, 2, PyUnicodeFromString('TrainAbortFlag'));
      PyList_SetItem(Result, 3, PyUnicodeFromString('TrainSampleFlag'));
      PyList_SetItem(Result, 3, PyUnicodeFromString('SampleFilename'));
      PyList_SetItem(Result, 4, PyUnicodeFromString('StyleAbortFlag'));
      PyList_SetItem(Result, 5, PyUnicodeFromString('StyleFilename'));
      PyList_SetItem(Result, 6, PyUnicodeFromString('StyleJsonLog'));
    end;
end;

end.

