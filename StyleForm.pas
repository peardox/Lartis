unit StyleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  Shaders, FMX.ListBox;

type
  TLayerArray = Array of TBaseShader;

  TfrmStyle = class(TEmbeddedForm)
    TopPanel: TPanel;
    Button1: TButton;
    ControlLayout: TLayout;
    StyleLayout: TLayout;
    OpenDialog1: TOpenDialog;
    prgStyleBatch: TProgressBar;
    vsbLayers: TFramedVertScrollBox;
    GridLayout1: TGridLayout;
    layControls: TLayout;
    btnAddLayer: TButton;
    btnStylize: TButton;
    cbxColourMode: TComboBox;
    expTransparency: TExpander;
    chkEnableTransparency: TCheckBox;
    lblAlphaThresholdKey: TLabel;
    trkAlphaThreshold: TTrackBar;
    lblAlphaThresholdValue: TLabel;
    chkInvertAlpha: TCheckBox;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    trkStyleWeight: TTrackBar;
    chkEnableGPU: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnStylizeClick(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure trkStyleWeightChange(Sender: TObject);
    procedure cbxColourModeChange(Sender: TObject);
    procedure chkEnableTransparencyChange(Sender: TObject);
    procedure chkInvertAlphaChange(Sender: TObject);
    procedure trkAlphaThresholdChange(Sender: TObject);
    procedure expTransparencyExpandedChanged(Sender: TObject);
    procedure ControlLayoutResize(Sender: TObject);
    procedure TopPanelResize(Sender: TObject);
    procedure chkEnableGPUChange(Sender: TObject);
  private
    { Private declarations }
    Grid: TGridShader;
    ImageLayer: TBaseShader;
    Container: TAspectLayout;
    Layers: TLayerArray;
    LayerCount: Integer;
    procedure AddLayer;
    procedure ProjectInitialise;
    procedure ShowStyleProgress(Sender: TObject; const AValue: Single);
    procedure ShowStyledImage(Sender: TObject; const AFileName: String);
  public
    { Public declarations }
  end;

var
  frmStyle: TfrmStyle;

const
  ControlMargin = 8;

implementation

uses
  Settings,
  Math,
  FunctionLibrary,
  PythonSystem;

{$R *.fmx}

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
  cbxColourMode.Items.Add('Use Styled Colors');
  cbxColourMode.Items.Add('Use Original (YUV)');
  cbxColourMode.Items.Add('Use Original (HLS)');
  cbxColourMode.ItemIndex := 0;

  chkEnableGPU.IsChecked := EnableGPU;

  LayerCount := 0;

  ProjectInitialise;
end;

procedure TfrmStyle.expTransparencyExpandedChanged(Sender: TObject);
begin
  ControlLayoutResize(Self);
end;

procedure TfrmStyle.ControlLayoutResize(Sender: TObject);
var
  ch: Single;
begin
  ch := lblStyleWeightKey.Size.Height +
    trkStyleWeight.Size.Height +
    cbxColourMode.Size.Height +

    expTransparency.Size.Height +

    btnAddLayer.Size.Height +
    btnStylize.Size.Height +
    (ControlMargin * 7);

  layControls.Height := ch;

  lblStyleWeightKey.Position.X := ControlMargin;
  lblStyleWeightKey.Position.Y := ControlMargin;

  lblStyleWeightValue.Position.X := layControls.Width - 56;
  lblStyleWeightValue.Position.Y := ControlMargin;

  trkStyleWeight.Position.X := ControlMargin;
  trkStyleWeight.Position.Y := lblStyleWeightKey.Position.Y + 24;
  trkStyleWeight.Size.Width := layControls.Width - (ControlMargin * 2);

  cbxColourMode.Position.X := ControlMargin;
  cbxColourMode.Position.Y := trkStyleWeight.Position.Y + 32;
  cbxColourMode.Size.Width := layControls.Width - (ControlMargin * 2);

  expTransparency.Position.X := ControlMargin - 2;
  expTransparency.Position.Y := cbxColourMode.Position.Y + 32;

  btnAddLayer.Position.X := ControlMargin;
  btnAddLayer.Position.Y := expTransparency.Position.Y + expTransparency.Size.Height + ControlMargin;
  btnAddLayer.Size.Width := layControls.Width - (ControlMargin * 2);

  btnStylize.Position.X := ControlMargin;
  btnStylize.Position.Y := btnAddLayer.Position.Y + 24;
  btnStylize.Size.Width := layControls.Width - (ControlMargin * 2);

  vsbLayers.Position.X := ControlMargin;
  vsbLayers.Position.Y := ControlMargin;
  vsbLayers.Size.Width := layControls.Width - (ControlMargin * 2);
end;

procedure TfrmStyle.ProjectInitialise;
begin
  trkStyleWeight.Max := 10000;
  trkAlphaThreshold.Max := 10000;
  trkStyleWeight.Value := trkStyleWeight.Max;
  trkAlphaThreshold.Value := trkAlphaThreshold.Max;
  lblAlphaThresholdValue.Text := FormatFloat('##0.00', 100.00);
  lblStyleWeightValue.Text := FormatFloat('##0.00', 100.00);
  expTransparency.Enabled := True;
end;

procedure TfrmStyle.ShowStyleProgress(Sender: TObject; const AValue: Single);
begin
  PySys.Log('Progress = ' + FloatToStr(AValue));
  if Assigned(ImageLayer) then
      if ImageLayer is TProgressShader then
        begin
          TProgressShader(ImageLayer).Progress := AValue;
          prgStyleBatch.Value := AValue;
        end;
end;

procedure TfrmStyle.StyleLayoutResize(Sender: TObject);
begin
  if Assigned(Container) then
    begin
      Container.FitToContainer;
    end;
end;

procedure TfrmStyle.TopPanelResize(Sender: TObject);
begin
  chkEnableGPU.Position.X := TopPanel.Width - chkEnableGPU.Size.Width - ControlMargin;
  chkEnableGPU.Position.Y := floor((TopPanel.Height - chkEnableGPU.Size.Height) / 2);
end;

procedure TfrmStyle.trkAlphaThresholdChange(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    begin
      if ImageLayer is TLayerShader then // Track StyleWeight for LayerShader
        with ImageLayer as TLayerShader do
          begin
            AlphaThreshold := (trkAlphaThreshold.Value / trkAlphaThreshold.Max);
            lblAlphaThresholdValue.Text := FormatFloat('##0.00', AlphaThreshold * 100);
          end;
      if ImageLayer is TProgressShader then // Fake StyleWeight for ProgressShader
        begin
          lblAlphaThresholdValue.Text := FormatFloat('##0.00', (trkAlphaThreshold.Value / trkAlphaThreshold.Max) * 100);
        end;
    end;
end;

procedure TfrmStyle.trkStyleWeightChange(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    begin
      if ImageLayer is TLayerShader then // Track StyleWeight for LayerShader
        with ImageLayer as TLayerShader do
          begin
            StyleWeight := (trkStyleWeight.Value / trkStyleWeight.Max);
            lblStyleWeightValue.Text := FormatFloat('##0.00', StyleWeight * 100);
          end;
      if ImageLayer is TProgressShader then // Fake StyleWeight for ProgressShader
        begin
          lblStyleWeightValue.Text := FormatFloat('##0.00', (trkStyleWeight.Value / trkStyleWeight.Max) * 100);
        end;
    end;
end;

procedure TfrmStyle.btnAddLayerClick(Sender: TObject);
begin
  if LayerCount = 0 then
    begin
    {
      if Assigned(Grid) then
        FreeAndNil(Grid);
      if Assigned(Container) then
        FreeAndNil(Container);
    }
      Container := TAspectLayout.Create(StyleLayout);
      Grid := TGridShader.Create(Container);
    end;

  Inc(LayerCount);
  SetLength(Layers, LayerCount);
  ImageLayer := Layers[LayerCount - 1];

  AddLayer;
end;

procedure TfrmStyle.AddLayer;
var
  LBitmap: TBitmap;
begin
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          if Assigned(ImageLayer) then
            begin
              if ImageLayer is TLayerShader then
                FreeAndNil(TLayerShader(ImageLayer));
              if ImageLayer is TProgressShader then
                FreeAndNil(TProgressShader(ImageLayer));
            end;

          ImageLayer := TProgressShader.Create(Container);

          with ImageLayer as TProgressShader do
            begin
              AddImage(OpenDialog1.FileName);
              trkStyleWeight.Value := 1.00;
              trkStyleWeight.Enabled := False;
              if Assigned(TProgressShader(ImageLayer).Bitmap) then
                TProgressShader(ImageLayer).AlphaMap;
            end;

        end;
    end;
end;

procedure TfrmStyle.ShowStyledImage(Sender: TObject; const AFileName: String);
var
  CurrentImage: String;
  CurrentBitMap: TBitmap;
begin
  CurrentBitMap := Nil;
  if Assigned(ImageLayer) then
    begin
      if ImageLayer is TProgressShader then
        begin
          PySys.Log('Removing ProgressShader');
          CurrentImage := TProgressShader(ImageLayer).ImageFile;
          if Assigned(TProgressShader(ImageLayer).Bitmap) then
            begin
              CurrentBitmap := TBitmap.Create;
              CurrentBitmap.Assign(TProgressShader(ImageLayer).Bitmap);
            end;
          if Assigned(CurrentBitmap) then
            begin
              TProgressShader(ImageLayer).Free;

              PySys.Log('Adding LayerShader');
              ImageLayer := TLayerShader.Create(Container);
              with ImageLayer as TLayerShader do
                begin
                  PySys.Log('Adding Original ' + CurrentImage);
                  OriginalImage := CurrentImage;
                  AddBitmap(Original, CurrentBitmap, True);
                  PySys.Log('Adding Styled ' + AFileName);
                  AddImage(Styled, AFileName);
                  PreserveTransparency := False;
                  trkStyleWeight.Value := 1.00;
                  trkStyleWeight.Enabled := True;
                  trkAlphaThreshold.Value := 0.95;
                  prgStyleBatch.Value := 0;
                end;
                FreeAndNil(CurrentBitMap);
            end;
        end;
    end;

end;

procedure TfrmStyle.btnStylizeClick(Sender: TObject);
var
  CurrentImage: String;
  CurrentBitMap: TBitmap;
begin
  if Assigned(PySys) and Assigned(ImageLayer) then
    begin

      if ImageLayer is TLayerShader then
        begin
          PySys.Log('Removing LayerShader');
          CurrentImage := TLayerShader(ImageLayer).OriginalImage;
          if Assigned(TLayerShader(ImageLayer).OriginalBitmap) then
            begin
              CurrentBitmap := TBitmap.Create;
              CurrentBitmap.Assign(TLayerShader(ImageLayer).OriginalBitmap);
            end;
          if Assigned(CurrentBitmap) then
            begin
              TLayerShader(ImageLayer).Free;

              PySys.Log('Adding ProgressShader');
              ImageLayer := TProgressShader.Create(Container);
              with ImageLayer as TProgressShader do
                begin
                  PySys.Log('Adding Original ' + CurrentImage);
                  ImageFile := CurrentImage;
                  AddBitmap(CurrentBitmap, True);
                end;
                FreeAndNil(CurrentBitMap);
            end;
        end;

      if ImageLayer is TProgressShader then
        with ImageLayer as TProgressShader do
          begin
            if ImageFile <> String.Empty then
              PySys.modStyle.Stylize(ImageFile, ShowStyleProgress, ShowStyledImage);
          end;
    end;
end;

procedure TfrmStyle.chkEnableGPUChange(Sender: TObject);
begin
   EnableGPU := chkEnableGPU.IsChecked;
end;

procedure TfrmStyle.chkEnableTransparencyChange(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).PreserveTransparency :=  chkEnableTransparency.IsChecked;
end;

procedure TfrmStyle.chkInvertAlphaChange(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).InvertAlpha :=  chkInvertAlpha.IsChecked;
end;

procedure TfrmStyle.cbxColourModeChange(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).ColorMode :=  cbxColourMode.ItemIndex;
end;

procedure TfrmStyle.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;


end.
