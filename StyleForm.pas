unit StyleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  Shaders, StyleProject, FMX.ListBox;

type
  TfrmStyle = class(TEmbeddedForm)
    TopPanel: TPanel;
    layStyleControl: TLayout;
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
    chkEnableGPU: TCheckBox;
    btnClearLayers: TButton;
    layStylePicker: TLayout;
    trkStyleWeight: TTrackBar;
    btnBack: TButton;
    procedure btnBackClick(Sender: TObject);
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
    procedure layStyleControlResize(Sender: TObject);
    procedure TopPanelResize(Sender: TObject);
    procedure chkEnableGPUChange(Sender: TObject);
    procedure btnClearLayersClick(Sender: TObject);
  private
    { Private declarations }
    Grid: TGridShader;
    ActiveLayer: TBaseShader;
    Container: TAspectLayout;
    Layers: TLayerArray;
    LayerCount: Integer;
    function  AddLayer: TBaseShader;
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
  layStyleControlResize(Self);
end;

procedure TfrmStyle.layStyleControlResize(Sender: TObject);
var
  ch: Single;
begin
  ch := lblStyleWeightKey.Size.Height +
    trkStyleWeight.Size.Height +
    cbxColourMode.Size.Height +

    expTransparency.Size.Height +

    btnAddLayer.Size.Height +
    btnStylize.Size.Height +
    btnClearLayers.Size.Height +
    (ControlMargin * 8);

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

  btnClearLayers.Position.X := ControlMargin;
  btnClearLayers.Position.Y := btnStylize.Position.Y + 24;
  btnClearLayers.Size.Width := layControls.Width - (ControlMargin * 2);

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
  if Assigned(ActiveLayer) then
      if ActiveLayer is TProgressShader then
        begin
          TProgressShader(ActiveLayer).Progress := AValue;
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
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TLayerShader then // Track StyleWeight for LayerShader
        with ActiveLayer as TLayerShader do
          begin
            AlphaThreshold := (trkAlphaThreshold.Value / trkAlphaThreshold.Max);
            lblAlphaThresholdValue.Text := FormatFloat('##0.00', AlphaThreshold * 100);
          end;
      if ActiveLayer is TProgressShader then // Fake StyleWeight for ProgressShader
        begin
          lblAlphaThresholdValue.Text := FormatFloat('##0.00', (trkAlphaThreshold.Value / trkAlphaThreshold.Max) * 100);
        end;
    end;
end;

procedure TfrmStyle.trkStyleWeightChange(Sender: TObject);
begin
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TLayerShader then // Track StyleWeight for LayerShader
        with ActiveLayer as TLayerShader do
          begin
            StyleWeight := (trkStyleWeight.Value / trkStyleWeight.Max);
            lblStyleWeightValue.Text := FormatFloat('##0.00', StyleWeight * 100);
          end;
      if ActiveLayer is TProgressShader then // Fake StyleWeight for ProgressShader
        begin
          lblStyleWeightValue.Text := FormatFloat('##0.00', (trkStyleWeight.Value / trkStyleWeight.Max) * 100);
        end;
    end;
end;

procedure TfrmStyle.btnAddLayerClick(Sender: TObject);
var
  NewLayer: TBaseShader;
begin
  if LayerCount = 0 then
    begin
      Container := TAspectLayout.Create(StyleLayout);
      Grid := TGridShader.Create(Container);
    end;

  NewLayer := AddLayer;
  if Assigned(NewLayer) then
    begin
      LayerCount := Length(Layers) + 1;
      SetLength(Layers, LayerCount);
      Layers[LayerCount - 1] := NewLayer;
      ActiveLayer := NewLayer;
    end;
end;

procedure TfrmStyle.btnClearLayersClick(Sender: TObject);
var
  I: Integer;
begin
  LayerCount := Length(Layers);
  for I := LayerCount - 1 downto 0 do
    begin
      if Assigned(Layers[I]) then
        begin
          if Layers[I] is TLayerShader then
            FreeAndNil(TLayerShader(Layers[I]))
          else if Layers[I] is TProgressShader then
            FreeAndNil(TProgressShader(Layers[I]))
          else
            begin
              ShowMessage('Releasing Unknown Layer! Class = ' + Layers[I].ClassName);
              Layers[I] := Nil;
            end;
        end
      else
        ShowMessage('Unassigned Layer in Layer List!');
      Dec(LayerCount);
    end;

  SetLength(Layers, LayerCount);
  if LayerCount > 0 then
    ActiveLayer := Layers[LayerCount - 1]
  else
    begin
      ActiveLayer := Nil;
      if Assigned(Grid) then
        FreeAndNil(Grid);
      if Assigned(Container) then
        FreeAndNil(Container);
    end;

end;

function TfrmStyle.AddLayer: TBaseShader;
var
  LBitmap: TBitmap;
  NewLayer: TBaseShader;
begin
  NewLayer := Nil;

  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          NewLayer := TProgressShader.Create(Container);

          with NewLayer as TProgressShader do
            begin
              AddImage(OpenDialog1.FileName);
              trkStyleWeight.Value := trkStyleWeight.Max;
              trkStyleWeight.Enabled := False;
              if Assigned(TProgressShader(NewLayer).Bitmap) then
                TProgressShader(NewLayer).AlphaMap;
            end;
        end;
    end;
    Result := NewLayer;
end;

procedure TfrmStyle.ShowStyledImage(Sender: TObject; const AFileName: String);
var
  CurrentImage: String;
  CurrentBitMap: TBitmap;
begin
  CurrentBitMap := Nil;
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TProgressShader then
        begin
          PySys.Log('Removing ProgressShader');
          CurrentImage := TProgressShader(ActiveLayer).ImageFile;
          if Assigned(TProgressShader(ActiveLayer).Bitmap) then
            begin
              CurrentBitmap := TBitmap.Create;
              CurrentBitmap.Assign(TProgressShader(ActiveLayer).Bitmap);
            end;
          if Assigned(CurrentBitmap) then
            begin
              TProgressShader(ActiveLayer).Free;

              PySys.Log('Adding LayerShader');
              ActiveLayer := TLayerShader.Create(Container);
              with ActiveLayer as TLayerShader do
                begin
                  PySys.Log('Adding Original ' + CurrentImage);
                  OriginalImage := CurrentImage;
                  AddBitmap(Original, CurrentBitmap, True);
                  PySys.Log('Adding Styled ' + AFileName);
                  AddImage(Styled, AFileName);
                  PreserveTransparency := False;
                  trkStyleWeight.Value := trkStyleWeight.Max;
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
  if Assigned(PySys) and Assigned(ActiveLayer) then
    begin

      if ActiveLayer is TLayerShader then
        begin
          PySys.Log('Removing LayerShader');
          CurrentImage := TLayerShader(ActiveLayer).OriginalImage;
          if Assigned(TLayerShader(ActiveLayer).OriginalBitmap) then
            begin
              CurrentBitmap := TBitmap.Create;
              CurrentBitmap.Assign(TLayerShader(ActiveLayer).OriginalBitmap);
            end;
          if Assigned(CurrentBitmap) then
            begin
              TLayerShader(ActiveLayer).Free;

              PySys.Log('Adding ProgressShader');
              ActiveLayer := TProgressShader.Create(Container);
              with ActiveLayer as TProgressShader do
                begin
                  PySys.Log('Adding Original ' + CurrentImage);
                  ImageFile := CurrentImage;
                  AddBitmap(CurrentBitmap, True);
                end;
                FreeAndNil(CurrentBitMap);
            end;
        end;

      if ActiveLayer is TProgressShader then
        with ActiveLayer as TProgressShader do
          begin
            if ImageFile <> String.Empty then
              PySys.modStyle.Stylize(ImageFile, ShowStyleProgress, ShowStyledImage);
//              PySys.modStyle.Stylize(Bitmap, ShowStyleProgress, ShowStyledImage);
          end;
    end;
end;

procedure TfrmStyle.chkEnableGPUChange(Sender: TObject);
begin
   EnableGPU := chkEnableGPU.IsChecked;
end;

procedure TfrmStyle.chkEnableTransparencyChange(Sender: TObject);
begin
  if Assigned(ActiveLayer) then
    if ActiveLayer is TLayerShader then
      TLayerShader(ActiveLayer).PreserveTransparency :=  chkEnableTransparency.IsChecked;
end;

procedure TfrmStyle.chkInvertAlphaChange(Sender: TObject);
begin
  if Assigned(ActiveLayer) then
    if ActiveLayer is TLayerShader then
      TLayerShader(ActiveLayer).InvertAlpha :=  chkInvertAlpha.IsChecked;
end;

procedure TfrmStyle.cbxColourModeChange(Sender: TObject);
begin
  if Assigned(ActiveLayer) then
    if ActiveLayer is TLayerShader then
      TLayerShader(ActiveLayer).ColorMode :=  cbxColourMode.ItemIndex;
end;

procedure TfrmStyle.btnBackClick(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;


end.
