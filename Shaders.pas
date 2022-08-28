unit Shaders;

interface

uses
  System.Classes, System.Types, System.TypInfo, FMX.Types, FMX.Graphics,
  FMX.Controls, FMX.Objects, FMX.Layouts, Skia, Skia.FMX, FunctionLibrary;

type
  TLayerImageType = (Styled, Original);
  TLayerBitmap = record
    ImageFile: String;
    Bitmap: TBitmap;
  end;

  TLayerImages = Array[0..1] of TLayerBitmap;

  TAspectLayout = class(TRectangle)
  public
    ChildMaxImWidth: Integer;
    ChildMaxImHeight: Integer;
    ChildMaxImScale: Single;
    constructor Create(AOwner: TComponent); override;
    procedure FitToContainer;
    procedure Resize; override;
  end;

  TBaseShader = class(TSkPaintBox)
//  TBaseShader = class(TSkAnimatedPaintBox)
  protected
    Effect: ISkRuntimeEffect;
    Painter: ISkPaint;
    ShaderText: String;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGridShader = class(TBaseShader)
  private
    frame_count: Integer;
    procedure DoDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
  public
    iGridSize: Integer;
    cFront: TArray<Single>;
    cBack: TArray<Single>;
    constructor Create(AOwner: TComponent); override;
    procedure AddShader;
  end;

  TLayerShader = class(TBaseShader)
  private
    FOnChangeStyleWeight: TNotifyEvent;
    FStyleWeight: Single;
    FOnChangeAlphaThreshold: TNotifyEvent;
    FAlphaThreshold: Single;
    FOnChangeColorMode: TNotifyEvent;
    FColorMode: Integer;
    FOnChangePreserveTransparency: TNotifyEvent;
    FPreserveTransparency: Boolean;
    FInvertAlpha: Boolean;
    FOnChangeInvertAlpha: TNotifyEvent;

    bmImages: TLayerImages;
    ImWidth: Integer;
    ImHeight: Integer;
    fImScale: Single;
    fImOffsetX: Integer;
    fImOffsetY: Integer;
    procedure DoDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function MakeAlphaMap(const ABitmap: TBitmap): TAlphaMap;

    function GetStyleImage: String;
    function GetOriginalImage: String;
    Procedure SetStyleImage(const AFileName: String);
    procedure SetOriginalImage(const AFileName: String);
    function GetOriginalBitmap: TBitmap;
    function GetStyleBitmap: TBitmap;

    procedure SetStyleWeight(const AValue: Single);
    procedure SetAlphaThreshold(const AValue: Single);
    procedure SetColorMode(const AValue: Integer);
    procedure SetPreserveTransparency(const AValue: Boolean);
    procedure SetInvertAlpha(const AValue: Boolean);

    function GetStyleAlphaMap: TAlphaMap;
    function GetOriginalAlphaMap: TAlphaMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddShader;
    function AddImage(const ImageType: TLayerImageType; const AImageFile: String): Boolean;
    function AddBitmap(const ImageType: TLayerImageType; const ImageBitmap: TBitmap; const CopySrc: Boolean = False): Boolean;
    function IsImageStyled: Boolean;
  published
    property OnDraw;
    property StyleImage: String read GetStyleImage write SetStyleImage;
    property StyleBitmap: TBitmap read GetStyleBitmap;
    property OriginalImage: String read GetOriginalImage write SetOriginalImage;
    property OriginalBitmap: TBitmap read GetOriginalBitmap;
    property StyleAlphaMap: TAlphaMap read GetStyleAlphaMap;
    property OriginalAlphaMap: TAlphaMap read GetOriginalAlphaMap;
    property StyleWeight: Single read FStyleWeight write SetStyleWeight;
    property OnChangeStyleWeight: TNotifyEvent read FOnChangeStyleWeight write FOnChangeStyleWeight;
    property AlphaThreshold: Single read FAlphaThreshold write SetAlphaThreshold;
    property OnChangeAlphaThreshold: TNotifyEvent read FOnChangeAlphaThreshold write FOnChangeAlphaThreshold;
    property ColorMode: Integer read FColorMode write SetColorMode;
    property OnChangeColorMode: TNotifyEvent read FOnChangeColorMode write FOnChangeColorMode;
    property PreserveTransparency: Boolean read FPreserveTransparency write SetPreserveTransparency;
    property OnChangePreserveTransparency: TNotifyEvent read FOnChangePreserveTransparency write FOnChangePreserveTransparency;
    property InvertAlpha: Boolean read FInvertAlpha write SetInvertAlpha;
    property OnChangeInvertAlpha: TNotifyEvent read FOnChangeInvertAlpha write FOnChangeInvertAlpha;
  end;

  TProgressShader = class(TBaseShader)
  private
    fImScale: Single;
    fImOffsetX: Integer;
    fImOffsetY: Integer;
    fProgress: Single;
    bmImage: TBitmap;
    ImWidth: Integer;
    ImHeight: Integer;
    FImageFile: String;
    procedure DoDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
    function MakeAlphaMap(const ABitmap: TBitmap): TAlphaMap;
    function GetAlphaMap: TAlphaMap;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddShader;
    function AddImage(const AImageFile: String): Boolean;
    function AddBitmap(const AImageBitmap: TBitmap; const CopySrc: Boolean = False): Boolean;
  published
    property OnDraw;
    property Progress: Single read fProgress write fProgress;
    property ImageFile: String read FImageFile write FImageFile;
    property Bitmap: TBitmap read bmImage write bmImage;
    property AlphaMap: TAlphaMap read GetAlphaMap;
  end;


const
  LayerImageNames: Array [0..1] of String = ('Styled', 'Original');

implementation

uses
  Settings, System.SysUtils, System.UITypes, FMX.Dialogs,
  Math, IOUtils;

constructor TAspectLayout.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if Assigned(AOwner) then
    TFmxObject(AOwner).AddObject(Self);
  Fill.Color := TAlphaColors.Blue;
  ClipChildren := True;
  Width := 0;
  Height := 0;
end;

procedure TAspectLayout.Resize;
begin
  FitToContainer;
end;

procedure TAspectLayout.FitToContainer;
var
  Scale: Single;
begin
  if Assigned(Owner) and (Owner is TControl) and (ChildMaxImWidth > 0) and (ChildMaxImHeight > 0) then
    begin
      Scale := FitInsideContainer(TControl(Owner).Width, ChildMaxImWidth,
        TControl(Owner).Height, ChildMaxImHeight);
      if (Scale > 0) then
        begin
          ChildMaxImScale := Scale;
          Width := ChildMaxImWidth * Scale;
          Height := ChildMaxImHeight * Scale;
          Position.X := Floor((TControl(Owner).Width - Width) / 2);
          Position.Y := Floor((TControl(Owner).Height - Height) / 2);
        end;
    end;
end;

constructor TBaseShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Enabled := False;
  if Assigned(AOwner) then
    TFmxObject(AOwner).AddObject(Self);
  Align := TAlignLayout.Client;
end;

constructor TGridShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  iGridSize := 128;
  cBack := [0.333, 0.333, 0.333, 1];
  cFront := [0.666, 0.666, 0.666, 1];
  AddShader;
end;

procedure TGridShader.AddShader;
begin
  Enabled := False;
  ShaderText := LoadShader(TPath.Combine(ShaderPath,'grid.sksl'));

  var AErrorText := '';
  Effect := TSkRuntimeEffect.MakeForShader(ShaderText, AErrorText);
  if AErrorText <> '' then
    raise Exception.Create(AErrorText);

  Painter := TSkPaint.Create;
  Painter.Shader := Effect.MakeShader(True);

  OnDraw := DoDraw;

end;

procedure TGridShader.DoDraw(ASender: TObject; const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if Assigned(Effect) and Assigned(Painter) then
  begin
    if Effect.UniformExists('iResolution') then
    begin
      if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float3 then
        Effect.SetUniform('iResolution', [Single(TAspectLayout(Parent).ChildMaxImWidth), Single(TAspectLayout(Parent).ChildMaxImHeight), Single(0)])
      else if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float2 then
        Effect.SetUniform('iResolution', PointF(TAspectLayout(Parent).ChildMaxImWidth, TAspectLayout(Parent).ChildMaxImHeight))
      else
        Effect.SetUniform('iResolution', [TAspectLayout(Parent).ChildMaxImWidth, TAspectLayout(Parent).ChildMaxImHeight]);
    end;
    if Effect.UniformExists('sResolution') then
    begin
      if Effect.UniformType['sResolution'] = TSkRuntimeEffectUniformType.Float3 then
        Effect.SetUniform('sResolution', [Single(ADest.Width), Single(ADest.Height), Single(0)])
      else if Effect.UniformType['sResolution'] = TSkRuntimeEffectUniformType.Float2 then
        Effect.SetUniform('sResolution', PointF(ADest.Width, ADest.Height))
      else
        Effect.SetUniform('sResolution', [Int(ADest.Width), Int(ADest.Height)]);
    end;
    if Effect.UniformExists('iGridSize') then
      Effect.SetUniform('iGridSize', iGridSize ); // 64
    if Effect.UniformExists('cBack') then
      Effect.SetUniform('cBack', cBack); // [0.333, 0.333, 0.333, 1]
    if Effect.UniformExists('cFront') then
      Effect.SetUniform('cFront', cFront); // [0.666, 0.666, 0.666, 1]
    ACanvas.DrawRect(ADest, Painter);
  Inc(frame_count);
  end;
end;

constructor TLayerShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImScale := 1;
  fImOffsetX := 0;
  fImOffsetY := 0;
  FStyleWeight := 1;
  fAlphaThreshold := 0.95;
  FColorMode := 0;
  FPreserveTransparency := False;
  FInvertAlpha := False;
  AddShader;
end;

destructor TLayerShader.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(bmImages) - 1 do
    if Assigned(bmImages[I].Bitmap) then
      begin
        bmImages[I].ImageFile := String.Empty;
        FreeAndNil(bmImages[I].Bitmap);
      end;
  inherited;
end;

procedure TLayerShader.SetStyleWeight(const AValue: Single);
begin
  if FStyleWeight <> AValue then
    begin
      FStyleWeight := AValue;
      if Assigned(FOnChangeStyleWeight) then
        FOnChangeStyleWeight(Self);
    end;
end;

procedure TLayerShader.SetAlphaThreshold(const AValue: Single);
begin
  if FAlphaThreshold <> AValue then
    begin
      FAlphaThreshold := AValue;
      if Assigned(FOnChangeAlphaThreshold) then
        FOnChangeAlphaThreshold(Self);
    end;
end;

procedure TLayerShader.SetColorMode(const AValue: Integer);
begin
  if FColorMode <> AValue then
    begin
      FColorMode := AValue;
      if Assigned(FOnChangeColorMode) then
        FOnChangeColorMode(Self);
    end;
end;

procedure TLayerShader.SetPreserveTransparency(const AValue: Boolean);
begin
  if FPreserveTransparency <> AValue then
    begin
      FPreserveTransparency := AValue;
      if Assigned(FOnChangePreserveTransparency) then
        FOnChangePreserveTransparency(Self);
    end;
end;

procedure TLayerShader.SetInvertAlpha(const AValue: Boolean);
begin
  if FInvertAlpha <> AValue then
    begin
      FInvertAlpha := AValue;
      if Assigned(FOnChangeInvertAlpha) then
        FOnChangeInvertAlpha(Self);
    end;
end;

function TLayerShader.MakeAlphaMap(const ABitmap: TBitmap): TAlphaMap;
begin
  // SBDbg
end;

function TLayerShader.GetStyleAlphaMap: TAlphaMap;
begin
  if Assigned(bmImages[0].Bitmap) then
    Result := MakeAlphaMap(bmImages[0].Bitmap);
end;

function TLayerShader.GetOriginalAlphaMap: TAlphaMap;
begin
  if Assigned(bmImages[1].Bitmap) then
    Result := MakeAlphaMap(bmImages[1].Bitmap);
end;

function TLayerShader.GetStyleBitmap: TBitmap;
begin
  if Assigned(bmImages[0].Bitmap) then
    Result := bmImages[0].Bitmap;
end;

function TLayerShader.GetOriginalBitmap: TBitmap;
begin
  if Assigned(bmImages[1].Bitmap) then
    Result := bmImages[1].Bitmap;
end;

function TLayerShader.IsImageStyled: Boolean;
begin
  Result := Assigned(bmImages[0].Bitmap) and Assigned(bmImages[1].Bitmap);
end;

function TLayerShader.GetStyleImage: String;
begin
  Result := bmImages[0].ImageFile;
end;

function TLayerShader.GetOriginalImage: String;
begin
  Result := bmImages[1].ImageFile;
end;

Procedure TLayerShader.SetStyleImage(const AFileName: String);
begin
  bmImages[0].ImageFile := AFileName;
end;

procedure TLayerShader.SetOriginalImage(const AFileName: String);
begin
  bmImages[1].ImageFile := AFileName;
end;

function TLayerShader.AddImage(const ImageType: TLayerImageType; const AImageFile: String): Boolean;
begin
  Result := False;
  if FileExists(AImageFile) then
    begin
      var ImageIdentifier: String := LayerImageNames[Ord(ImageType)];
      bmImages[Ord(ImageType)].ImageFile := AImageFile;
      bmImages[Ord(ImageType)].Bitmap := TBitmap.Create;
      bmImages[Ord(ImageType)].Bitmap.LoadFromFile(AImageFile);
      Result := AddBitmap(ImageType, bmImages[Ord(ImageType)].Bitmap);
    end;
end;

function TLayerShader.AddBitmap(const ImageType: TLayerImageType;
  const ImageBitmap: TBitmap; const CopySrc: Boolean = False): Boolean;
var
  ImImageInfo: TSkImageInfo;
  HaveImage: Boolean;
  TexImage: ISkImage;
  LBitmap: TBitmap;
begin
  if CopySrc then
    begin
      LBitmap := TBitmap.Create;
      LBitmap.Assign(ImageBitmap);
      bmImages[Ord(ImageType)].Bitmap := LBitmap;
    end
  else
    LBitmap := ImageBitmap;

  HaveImage := False;
  var ImageIdentifier: String := LayerImageNames[Ord(ImageType)];

  if Assigned(LBitmap) and Effect.ChildExists(ImageIdentifier) then
  begin
    TexImage := LBitmap.ToSkImage;

    if Assigned(TexImage) then
    begin
      HaveImage := True;
      Effect.ChildrenShaders[ImageIdentifier] := TexImage.MakeShader(TSKSamplingOptions.High);
      if Effect.UniformExists(ImageIdentifier + 'Resolution') then
        case Effect.UniformType[ImageIdentifier + 'Resolution'] of
          TSkRuntimeEffectUniformType.Float2:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectFloat2.Create(TexImage.Width, TexImage.Height));
          TSkRuntimeEffectUniformType.Float3:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectFloat3.Create(TexImage.Width, TexImage.Height, 0));
          TSkRuntimeEffectUniformType.Int2:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectInt2.Create(TexImage.Width, TexImage.Height));
          TSkRuntimeEffectUniformType.Int3:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectInt3.Create(TexImage.Width, TexImage.Height, 0));
        end;

      ImImageInfo := TexImage.ImageInfo;
      ImWidth := ImImageInfo.Width;
      ImHeight := ImImageInfo.Height;

      //      MakeAlphaMap(TexImage.PeekPixels);

      if Owner is TAspectLayout then
        begin
          if (TAspectLayout(Parent).ChildMaxImHeight < ImHeight) or
             (TAspectLayout(Parent).ChildMaxImWidth < ImWidth) then
            begin
              if (TAspectLayout(Parent).ChildMaxImHeight < ImHeight) then
                TAspectLayout(Parent).ChildMaxImHeight := ImHeight;
              if (TAspectLayout(Parent).ChildMaxImWidth < ImWidth) then
                TAspectLayout(Parent).ChildMaxImWidth := ImWidth;
              TAspectLayout(Parent).FitToContainer;
            end;
        end;

    Painter := TSkPaint.Create;
    Painter.Shader := Effect.MakeShader(False);
    end;
  end;
  if Effect.UniformExists(ImageIdentifier + 'Valid') then
    Effect.SetUniform(ImageIdentifier + 'Valid', BoolToInt(HaveImage));
  Result := HaveImage;
end;

procedure TLayerShader.AddShader;
begin
  Enabled := False;
  ShaderText := LoadShader(TPath.Combine(ShaderPath,'original_colors.sksl'));

  var AErrorText := '';
  try
    Effect := TSkRuntimeEffect.MakeForShader(ShaderText, AErrorText);
    if AErrorText <> '' then
      raise Exception.Create('Error creating shader : ' + AErrorText);
    if Effect.UniformExists('StyledValid') then
      Effect.SetUniform('StyledValid', BoolToInt(False));
    if Effect.UniformExists('OriginalValid') then
      Effect.SetUniform('OriginalValid', BoolToInt(False));
  except
          on E: Exception do
            begin
              ShowMessage('Unhandled Exception'
                + sLineBreak + 'Class : ' + E.ClassName
                + sLineBreak + 'Error : ' + E.Message
                + sLineBreak + 'Message : ' + AErrorText);
            end;
  end;

  Enabled := True;

  OnDraw := DoDraw;
end;

procedure TLayerShader.DoDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if Assigned(Effect) and Assigned(Painter) then
  begin
    if Effect.UniformExists('iResolution') then
    begin
      if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float3 then
        Effect.SetUniform('iResolution', [Single(ADest.Width), Single(ADest.Height), Single(0)])
      else if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float2 then
        Effect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height))
      else
        Effect.SetUniform('iResolution', [Int(ADest.Width), Int(ADest.Height)]);
    end;
    if Effect.UniformExists('fStyleWeight') then
      Effect.SetUniform('fStyleWeight', FStyleWeight);
    if Effect.UniformExists('fAlphaThreshold') then
      Effect.SetUniform('fAlphaThreshold', FAlphaThreshold);

    if Effect.UniformExists('iColorMode') then
      Effect.SetUniform('iColorMode', FColorMode);
    if Effect.UniformExists('bPreserveTransparency') then
      Effect.SetUniform('bPreserveTransparency', BoolToInt(FPreserveTransparency));
    if Effect.UniformExists('bInvertAlpha') then
      Effect.SetUniform('bInvertAlpha', BoolToInt(FInvertAlpha));
    if Effect.UniformExists('fImScale') then
      begin
        if fImScale > 0 then
          Effect.SetUniform('fImScale', 1 / fImScale)
        else
          Effect.SetUniform('fImScale', 1);
      end;
    if Effect.UniformExists('fImOffset') then
      Effect.SetUniform('fImOffset', TSkRuntimeEffectFloat2.Create(fImOffsetX * (1 / fImScale), fImOffsetY * (1 / fImScale)));

    ACanvas.DrawRect(ADest, Painter);
  end;
end;

constructor TProgressShader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImScale := 1;
  fImOffsetX := 0;
  fImOffsetY := 0;
  fProgress := 1;
  FImageFile := String.Empty;

  AddShader;
end;

destructor TProgressShader.Destroy;
begin
  if Assigned(bmImage) then
    FreeAndNil(bmImage);
  inherited;
end;

procedure TProgressShader.AddShader;
begin
  Enabled := False;
  ShaderText := LoadShader(TPath.Combine(ShaderPath,'progress.sksl'));
//  ShaderText := LoadShader(TPath.Combine(ShaderPath,'clock.sksl'));

  var AErrorText := '';
  Effect := TSkRuntimeEffect.MakeForShader(ShaderText, AErrorText);
  if AErrorText <> '' then
    raise Exception.Create('Error creating shader : ' + AErrorText);

  Enabled := True;

  OnDraw := DoDraw;
end;

function TProgressShader.AddImage(const AImageFile: String): Boolean;
begin
  Result := False;
  if FileExists(AImageFile) then
    begin
      FImageFile := AImageFile;
      bmImage := TBitmap.Create;
      bmImage.LoadFromFile(FImageFile);
      Result := AddBitmap(bmImage);
    end
  else
    FImageFile := String.Empty;
end;

function TProgressShader.AddBitmap(const AImageBitmap: TBitmap; const CopySrc: Boolean = False): Boolean;
var
  ImImageInfo: TSkImageInfo;
  HaveImage: Boolean;
  TexImage: ISkImage;
  LBitmap: TBitmap;
begin
   if CopySrc then
    begin
      LBitmap := TBitmap.Create;
      LBitmap.Assign(AImageBitmap);
      bmImage := LBitmap;
    end
  else
    LBitmap := AImageBitmap;

  HaveImage := False;
  var ImageIdentifier: String := 'sImage';

  if Assigned(LBitmap) and Effect.ChildExists(ImageIdentifier) then
  begin
    TexImage := LBitmap.ToSkImage;

    if Assigned(TexImage) then
    begin
      HaveImage := True;
      Effect.ChildrenShaders[ImageIdentifier] := TexImage.MakeShader(TSKSamplingOptions.High);
      if Effect.UniformExists(ImageIdentifier + 'Resolution') then
        case Effect.UniformType[ImageIdentifier + 'Resolution'] of
          TSkRuntimeEffectUniformType.Float2:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectFloat2.Create(TexImage.Width, TexImage.Height));
          TSkRuntimeEffectUniformType.Float3:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectFloat3.Create(TexImage.Width, TexImage.Height, 0));
          TSkRuntimeEffectUniformType.Int2:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectInt2.Create(TexImage.Width, TexImage.Height));
          TSkRuntimeEffectUniformType.Int3:
            Effect.SetUniform(ImageIdentifier + 'Resolution', TSkRuntimeEffectInt3.Create(TexImage.Width, TexImage.Height, 0));
        end;

      ImImageInfo := TexImage.ImageInfo;
      ImWidth := ImImageInfo.Width;
      ImHeight := ImImageInfo.Height;

      if Owner is TAspectLayout then
        begin
          if (TAspectLayout(Parent).ChildMaxImHeight < ImHeight) or
             (TAspectLayout(Parent).ChildMaxImWidth < ImWidth) then
            begin
              if (TAspectLayout(Parent).ChildMaxImHeight < ImHeight) then
                TAspectLayout(Parent).ChildMaxImHeight := ImHeight;
              if (TAspectLayout(Parent).ChildMaxImWidth < ImWidth) then
                TAspectLayout(Parent).ChildMaxImWidth := ImWidth;
              TAspectLayout(Parent).FitToContainer;
            end;
        end;

    Painter := TSkPaint.Create;
    Painter.Shader := Effect.MakeShader(False);
    end;
  end;
  Result := HaveImage;
end;

procedure TProgressShader.DoDraw(ASender: TObject;
  const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single);
begin
  if Assigned(Effect) and Assigned(Painter) then
  begin
    if Effect.UniformExists('iResolution') then
    begin
      if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float3 then
        Effect.SetUniform('iResolution', [Single(ADest.Width), Single(ADest.Height), Single(0)])
      else if Effect.UniformType['iResolution'] = TSkRuntimeEffectUniformType.Float2 then
        Effect.SetUniform('iResolution', PointF(ADest.Width, ADest.Height))
      else
        Effect.SetUniform('iResolution', [Int(ADest.Width), Int(ADest.Height)]);
    end;
    if Effect.UniformExists('fPercentDone') then
      Effect.SetUniform('fPercentDone', fProgress);

    if Effect.UniformExists('fImScale') then
      begin
        if fImScale > 0 then
          Effect.SetUniform('fImScale', 1 / fImScale)
        else
          Effect.SetUniform('fImScale', 1);
      end;
    if Effect.UniformExists('fImOffset') then
      Effect.SetUniform('fImOffset', TSkRuntimeEffectFloat2.Create(fImOffsetX * (1 / fImScale), fImOffsetY * (1 / fImScale)));

    ACanvas.DrawRect(ADest, Painter);
  end;
end;

function TProgressShader.MakeAlphaMap(const ABitmap: TBitmap): TAlphaMap;
begin
  Result := CreateAlphaMap(Bitmap);
end;

function TProgressShader.GetAlphaMap: TAlphaMap;
begin
  if Assigned(Bitmap) then
    Result := MakeAlphaMap(Bitmap);
end;


end.


