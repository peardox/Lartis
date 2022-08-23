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
    btnOpenFile: TButton;
    StyleLayout: TLayout;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    trkStyleWeight: TTrackBar;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    Expander1: TExpander;
    Splitter1: TSplitter;
    ComboBox1: TComboBox;
    CheckBox1: TCheckBox;
    lblAlphaThresholdKey: TLabel;
    trkAlphaThreshold: TTrackBar;
    lblAlphaThresholdValue: TLabel;
    CheckBox2: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure trkStyleWeightChange(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure trkAlphaThresholdChange(Sender: TObject);
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

implementation

uses
  Settings,
  PythonSystem;

{$R *.fmx}

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
  ProjectInitialise;
end;

procedure TfrmStyle.ProjectInitialise;
begin
  ComboBox1.Items.Add('Use Styled Colors');
  ComboBox1.Items.Add('Use Original (YUV)');
  ComboBox1.Items.Add('Use Original (HLS)');
  ComboBox1.ItemIndex := 0;

  trkStyleWeight.Max := 10000;
  trkAlphaThreshold.Max := 10000;
  trkStyleWeight.Value := 1.00;
  trkAlphaThreshold.Value := 0.95;
  lblAlphaThresholdValue.Text := FormatFloat('##0.00', 100.00);
  lblStyleWeightValue.Text := FormatFloat('##0.00', 100.00);
  Expander1.Enabled := True;
end;

procedure TfrmStyle.ShowStyleProgress(Sender: TObject; const AValue: Single);
begin
  PySys.Log('Progress = ' + FloatToStr(AValue));
  if Assigned(ImageLayer) then
      if ImageLayer is TProgressShader then
        begin
          TProgressShader(ImageLayer).Progress := AValue;
          ProgressBar1.Value := AValue;
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

procedure TfrmStyle.btnOpenFileClick(Sender: TObject);
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
                  ProgressBar1.Value := 0;
                end;
                FreeAndNil(CurrentBitMap);
            end;
        end;
    end;

end;

procedure TfrmStyle.Button2Click(Sender: TObject);
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

procedure TfrmStyle.ComboBox1Change(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    if ImageLayer is TLayerShader then
      TLayerShader(ImageLayer).ColorMode :=  ComboBox1.ItemIndex;
end;

procedure TfrmStyle.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;


end.
