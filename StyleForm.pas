unit StyleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  Shaders, FMX.ListBox;

type
  TfrmStyle = class(TEmbeddedForm)
    TopPanel: TPanel;
    Button1: TButton;
    ControlLayout: TLayout;
    btnAddLayer: TButton;
    StyleLayout: TLayout;
    OpenDialog1: TOpenDialog;
    btnStylize: TButton;
    prgStyleBatch: TProgressBar;
    trkStyleWeight: TTrackBar;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    expTransparency: TExpander;
    Splitter1: TSplitter;
    cbxColourMode: TComboBox;
    CheckBox1: TCheckBox;
    lblAlphaThresholdKey: TLabel;
    trkAlphaThreshold: TTrackBar;
    lblAlphaThresholdValue: TLabel;
    CheckBox2: TCheckBox;
    vsbLayers: TFramedVertScrollBox;
    GridLayout1: TGridLayout;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure btnStylizeClick(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure trkStyleWeightChange(Sender: TObject);
    procedure cbxColourModeChange(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure trkAlphaThresholdChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure expTransparencyExpandedChanged(Sender: TObject);
  private
    { Private declarations }
    Grid: TGridShader;
    ImageLayer: TBaseShader;
    Container: TAspectLayout;
  public
    { Public declarations }
    procedure ProjectInitialise;
    procedure ShowStyleProgress(Sender: TObject; const AValue: Single);
    procedure ShowStyledImage(Sender: TObject; const AFileName: String);
  end;

var
  frmStyle: TfrmStyle;

const
  ControlMargin = 8;

implementation

uses
  Settings,
  PythonSystem;

{$R *.fmx}

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
  ProjectInitialise;
end;

procedure TfrmStyle.expTransparencyExpandedChanged(Sender: TObject);
begin
  btnAddLayer.Position.X := ControlMargin;
  btnAddLayer.Position.Y := expTransparency.Position.Y + expTransparency.Size.Height + ControlMargin;
  btnAddLayer.Size.Width := ControlLayout.Width - (ControlMargin * 2);

  btnStylize.Position.X := ControlMargin;
  btnStylize.Position.Y := btnAddLayer.Position.Y + 24;
  btnStylize.Size.Width := ControlLayout.Width - (ControlMargin * 2);

  vsbLayers.Position.X := ControlMargin;
  vsbLayers.Position.Y := btnStylize.Position.Y + 24;
  vsbLayers.Size.Width := ControlLayout.Width - (ControlMargin * 2);

end;

procedure TfrmStyle.FormResize(Sender: TObject);
begin
  lblStyleWeightKey.Position.X := ControlMargin;
  lblStyleWeightKey.Position.Y := ControlMargin;

  lblStyleWeightValue.Position.X := ControlLayout.Width - 56;
  lblStyleWeightValue.Position.Y := ControlMargin;

  trkStyleWeight.Position.X := ControlMargin;
  trkStyleWeight.Position.Y := lblStyleWeightKey.Position.Y + 24;
  trkStyleWeight.Size.Width := ControlLayout.Width - (ControlMargin * 2);

  cbxColourMode.Position.X := ControlMargin;
  cbxColourMode.Position.Y := trkStyleWeight.Position.Y + 32;
  cbxColourMode.Size.Width := ControlLayout.Width - (ControlMargin * 2);

  expTransparency.Position.X := ControlMargin - 2;
  expTransparency.Position.Y := cbxColourMode.Position.Y + 32;

  expTransparencyExpandedChanged(Self);
end;

procedure TfrmStyle.ProjectInitialise;
begin
  cbxColourMode.Items.Add('Use Styled Colors');
  cbxColourMode.Items.Add('Use Original (YUV)');
  cbxColourMode.Items.Add('Use Original (HLS)');
  cbxColourMode.ItemIndex := 0;

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
          if Assigned(Grid) then
            FreeAndNil(Grid);
          if Assigned(Container) then
            FreeAndNil(Container);

          Container := TAspectLayout.Create(StyleLayout);
          Grid := TGridShader.Create(Container);
          ImageLayer := TProgressShader.Create(Container);

          with ImageLayer as TProgressShader do
            begin
              AddImage(OpenDialog1.FileName);
              trkStyleWeight.Value := 1.00;
              trkStyleWeight.Enabled := False;
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
begin
  if Assigned(PySys) and Assigned(ImageLayer) then
    begin
      if ImageLayer is TProgressShader then
        with ImageLayer as TProgressShader do
          begin
            if ImageFile <> String.Empty then
              PySys.modStyle.Stylize(ImageFile, ShowStyleProgress, ShowStyledImage);
          end;
    end;
end;

procedure TfrmStyle.CheckBox1Change(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).PreserveTransparency :=  CheckBox1.IsChecked;
end;

procedure TfrmStyle.CheckBox2Change(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).InvertAlpha :=  CheckBox2.IsChecked;
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
