unit StyleForm;

interface

{$DEFINE NOLAYERS}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  StyleModel, System.Diagnostics,
  Shaders, StyleProject, FMX.ListBox, Skia, Skia.FMX, Skia.FMX.Graphics, FMX.Ani;

type
  TfrmStyle = class(TEmbeddedForm)
    TopPanel: TPanel;
    OpenImageDialog: TOpenDialog;
    prgStyleBatch: TProgressBar;
    chkEnableGPU: TCheckBox;
    btnBack: TButton;
    SaveImageDialog: TSaveDialog;
    lblInfo: TLabel;
    RectAnimation1: TRectAnimation;
    Layout1: TLayout;
    layStyleControl: TLayout;
    vsbLayers: TFramedVertScrollBox;
    layStyleThumb1: TLayout;
    imgStyleThumb1zzz: TImageControl;
    layControls: TLayout;
    btnAddLayer: TButton;
    cbxColourMode: TComboBox;
    expTransparency: TExpander;
    chkEnableTransparency: TCheckBox;
    lblAlphaThresholdKey: TLabel;
    trkAlphaThreshold: TTrackBar;
    lblAlphaThresholdValue: TLabel;
    chkInvertAlpha: TCheckBox;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    btnClearLayers: TButton;
    trkStyleWeight: TTrackBar;
    btnSave: TButton;
    Layout2: TLayout;
    layStylePicker: TLayout;
    vsbStyles: TFramedVertScrollBox;
    StyleLayout: TLayout;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    btnAbort: TButton;
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure btnSaveClick(Sender: TObject);
    procedure btnAddLayerClick(Sender: TObject);
    procedure SetSaveDialogExtension(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure imgStyleThumb1zzzClick(Sender: TObject);
  private
    { Private declarations }
    FSaveInNextPaint: Boolean;
    Grid: TGridShader;
    ActiveLayer: TBaseShader;
    Container: TAspectLayout;
    Layers: TLayerArray;
    StyleSelectors: TStyleSelectorArray;
    FrameCount: Integer;
    LastPath: String;
    LastModel: String;
    function  AddNewLayer: TBaseShader;
    procedure ProjectInitialise;
    procedure ShowStyleProgress(Sender: TObject; const AValue: Single);
    procedure ShowStyledImage(Sender: TObject; const AFileName: String; const ATime: Single);
    procedure HandleStyleError(Sender: TObject; const AMessage: String);
    procedure HandleStyleAbort(Sender: TObject);
    procedure DoSaveLayers;
    procedure Stylize(Sender: TObject; const APath: String; const AModel: String; const ByPassGPU: Boolean = False);
    procedure ClearLayers;
    procedure ResetLayerOptions;
    procedure UpdateLayerOptions;
  public
    { Public declarations }
    procedure SaveStyleImage;
    procedure AddStyleLayer;
    procedure MakeStyleSelectors;
    procedure UpdateDebugInfo;
  end;

var
  frmStyle: TfrmStyle;

const
  ControlMargin = 8;

implementation

uses
  Settings,
  Math,
  ErrorForm,
  System.IOUtils,
  FunctionLibrary,
  PythonSystem;

{$R *.fmx}

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
  {$IFDEF NOLAYERS}
  btnAddLayer.Visible := False;
  btnAddLayer.Height := 0;
  btnSave.Visible := False;
  btnSave.Height := 0;
  btnClearLayers.Visible := False;
  btnClearLayers.Height := 0;
  Splitter1.Enabled := False;
  Splitter1.Width := 0;
  Splitter2.Enabled := False;
  Splitter2.Width := 0;
  {$ENDIF}

  LastPath := String.Empty;
  LastModel := String.Empty;

  OpenImageDialog.Filter:='Images (*.png; *jpg)|*.png; *jpg';
//  OpenImageDialog.Filter:='PNG Images (*.png)|*.png|JPG Images (*.jpg)|*.jpg';
  SaveImageDialog.Filter:='PNG Images (*.png)|*.png|JPG Images (*.jpg)|*.jpg';
  SaveImageDialog.DefaultExt := '.png';
  SaveImageDialog.OnTypeChange := SetSaveDialogExtension;

  lblInfo.Text := '';
  FrameCount := 0;

  cbxColourMode.Items.Add('Use Styled Colors');
  cbxColourMode.Items.Add('Use Original (YUV)');
  cbxColourMode.Items.Add('Use Original (HLS)');
  cbxColourMode.ItemIndex := 0;

  chkEnableGPU.IsChecked := EnableGPU;

  SetLength(StyleSelectors, 0);
  SetLength(Layers, 0);

  ProjectInitialise;
  MakeStyleSelectors;
end;

procedure TfrmStyle.SetSaveDialogExtension(Sender: TObject);
begin
  case (Sender as TSaveDialog).FilterIndex of
    0: (Sender as TSaveDialog).DefaultExt := '.png';
    1: (Sender as TSaveDialog).DefaultExt := '.jpg';
  end;
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
    btnClearLayers.Size.Height +
    btnSave.Size.Height +
    (ControlMargin * 6);

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

  btnClearLayers.Position.X := ControlMargin;
  btnClearLayers.Position.Y := btnAddLayer.Position.Y + 24;
  btnClearLayers.Size.Width := layControls.Width - (ControlMargin * 2);

  btnSave.Position.X := ControlMargin;
  btnSave.Position.Y := btnClearLayers.Position.Y + 24;
  btnSave.Size.Width := layControls.Width - (ControlMargin * 2);

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

procedure TfrmStyle.HandleStyleAbort(Sender: TObject);
begin
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TProgressShader then
        begin
          TProgressShader(ActiveLayer).Progress := 1;
          prgStyleBatch.Value := 1;
//          ShowMessage('Aborted');
        end;
    end;
end;

procedure TfrmStyle.HandleStyleError(Sender: TObject; const AMessage: String);
var
  mr: TModalResult;
begin
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TProgressShader then
        begin
          TProgressShader(ActiveLayer).Progress := 1;
          prgStyleBatch.Value := 1;
          if AMessage = 'ERR_GPU_OOM' then
            begin
              frmErrorDialog.ErrorMode(AMessage);
              mr := frmErrorDialog.ShowModal();
              if mr = mrRetry then
                begin
                  Stylize(Self, LastPath, LastModel, True);
                end
              else
                begin
                  // May need some other stuff here
                end;
            end
          else
            begin
              //ShowMessage(AMessage);
            end;
        end;
    end;
end;

procedure TfrmStyle.imgStyleThumb1zzzClick(Sender: TObject);
begin
  imgStyleThumb1zzz.EnableOpenDialog := False;
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

procedure TfrmStyle.MakeStyleSelectors;
var
  I: Integer;
  AStyleModel: TModelStyleCollection;
begin
  if(Length(StyleSelectors) > 0) then
    begin
      for I := Length(StyleSelectors) - 1 downto 0 do
        FreeAndNil(StyleSelectors[I]);
      SetLength(StyleSelectors, 0);
    end;
  SetLength(StyleSelectors, StyleModels.Count);

  for I := 0 to StyleModels.Count - 1 do
    begin
      StyleSelectors[I] := TStyleSelector.Create(vsbStyles);
      StyleSelectors[I].Style := StyleModels.Collection[I];
      StyleSelectors[I].RunStyle := Stylize;
      vsbStyles.AddObject(StyleSelectors[I]);
    end;
end;

procedure TfrmStyle.btnAbortClick(Sender: TObject);
begin
  PySys.modPyIO.AbortStyle;
end;

procedure TfrmStyle.btnAddLayerClick(Sender: TObject);
begin
//  AddStyleLayer;
end;

procedure TfrmStyle.ResetLayerOptions;
begin
  chkEnableTransparency.IsChecked := False;
  chkInvertAlpha.IsChecked := False;
//  cbxColourMode.ItemIndex := 0;

// trkAlphaThresholdChange(Self);
// trkStyleWeightChange(Self);
//  chkEnableTransparencyChange(Self);
//  chkInvertAlphaChange(Self);
//  cbxColourModeChange(Self);
  if Assigned(ActiveLayer) and (ActiveLayer is TLayerShader) then
    begin
      TLayerShader(ActiveLayer).ColorMode :=  cbxColourMode.ItemIndex;
      TLayerShader(ActiveLayer).PreserveTransparency :=  chkEnableTransparency.IsChecked;
      TLayerShader(ActiveLayer).InvertAlpha :=  chkInvertAlpha.IsChecked;
    end;

end;

procedure TfrmStyle.UpdateLayerOptions;
begin
  if Assigned(ActiveLayer) and (ActiveLayer is TLayerShader) then
    begin
      TLayerShader(ActiveLayer).ColorMode :=  cbxColourMode.ItemIndex;
      TLayerShader(ActiveLayer).PreserveTransparency :=  chkEnableTransparency.IsChecked;
      TLayerShader(ActiveLayer).InvertAlpha :=  chkInvertAlpha.IsChecked;
    end;

end;

procedure TfrmStyle.AddStyleLayer;
var
  NewLayer: TBaseShader;
begin
  {$IFDEF NOLAYERS}
  ClearLayers;
  {$ENDIF}
  if Length(Layers) = 0 then
    begin
      Container := TAspectLayout.Create(StyleLayout);
      Container.OnPaint := FormPaint;
      Grid := TGridShader.Create(Container);
    end;

  // Add a new image as a TProgressShader
  NewLayer := AddNewLayer;
  if Assigned(NewLayer) then
    begin
      if NewLayer is TProgressShader then
        begin
          imgStyleThumb1zzz.Bitmap.LoadFromFile(TProgressShader(NewLayer).ImageFile)
        end;

      SetLength(Layers, Length(Layers) + 1);
      Layers[Length(Layers) - 1] := NewLayer;
      ActiveLayer := Layers[Length(Layers) - 1];
      ResetLayerOptions;
    end;
end;

procedure TfrmStyle.btnClearLayersClick(Sender: TObject);
begin
  ClearLayers;
end;

procedure TfrmStyle.ClearLayers;
var
  I: Integer;
begin
  for I := Length(Layers) - 1 downto 0 do
    begin
      if Assigned(Layers[I]) then
        begin
          if Layers[I] is TLayerShader then
            FreeAndNil(TLayerShader(Layers[I]))
          else if Layers[I] is TProgressShader then
            FreeAndNil(TProgressShader(Layers[I]))
          else
            begin
            //  ShowMessage('Releasing Unknown Layer! Class = ' + Layers[I].ClassName);
              Layers[I] := Nil;
            end;
        end;
//      else
//        ShowMessage('Unassigned Layer in Layer List!');
    end;

  SetLength(Layers, 0);
  ActiveLayer := Nil;
  if Assigned(Grid) then
    FreeAndNil(Grid);
  if Assigned(Container) then
    begin
      Container.OnPaint := Nil;
      FreeAndNil(Container);
    end;
end;

function TfrmStyle.AddNewLayer: TBaseShader;
var
  NewLayer: TBaseShader;
begin
  NewLayer := Nil;

  if Assigned(PySys) then
    begin
      OpenImageDialog.InitialDir := SystemSettings.LastOpenStyleDir;
      if OpenImageDialog.Execute then
        begin
          SystemSettings.LastOpenStyleDir := OpenImageDialog.InitialDir;

          NewLayer := TProgressShader.Create(Container);

          with NewLayer as TProgressShader do
            begin
              AddImage(OpenImageDialog.FileName);
              trkStyleWeight.Value := trkStyleWeight.Max;
              trkStyleWeight.Enabled := False;
              if Assigned(TProgressShader(NewLayer).Bitmap) then
                TProgressShader(NewLayer).AlphaMap;
            end;
        end;
    end;
    Result := NewLayer;
end;

procedure TfrmStyle.ShowStyledImage(Sender: TObject; const AFileName: String; const ATime: Single);
var
  CurrentImage: String;
  CurrentBitMap: TBitmap;
begin
  CurrentBitMap := Nil;
  if Assigned(ActiveLayer) then
    begin
      if ActiveLayer is TProgressShader then
        begin
          lblInfo.Text := 'Last Style : Model = ' +
            PySys.modStyle.Options.model + // Lazy - Really should return this as a param!!!
            ', Time = ' + FormatFloat('0.000', ATime) + 's';
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
                  UpdateLayerOptions;
                end;
                FreeAndNil(CurrentBitMap);
            end;
        end;
    end;

end;

procedure TfrmStyle.Stylize(Sender: TObject; const APath: String; const AModel: String; const ByPassGPU: Boolean = False);
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
              begin
                LastPath := APath;
                LastModel := AModel;
                PySys.Log('Path = ' + APath + ', Model = ' + AModel);
                PySys.modStyle.Stylize(ImageFile, APath, AModel, ByPassGPU, ShowStyleProgress, ShowStyledImage, HandleStyleError, HandleStyleAbort);
  //              PySys.modStyle.Stylize(Bitmap, APath, AModel, ByPassGPU, ShowStyleProgress, ShowStyledImage);
              end;
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

procedure TfrmStyle.btnSaveClick(Sender: TObject);
begin
  SaveStyleImage;
end;

procedure TfrmStyle.SaveStyleImage;
begin
  SaveImageDialog.InitialDir := SystemSettings.LastOpenStyleDir;
  SaveImageDialog.FileName := ExtractFilename(SaveImageDialog.FileName);
  if SaveImageDialog.Execute then
    begin
      SystemSettings.LastOpenStyleDir := SaveImageDialog.InitialDir;
      FSaveInNextPaint := True;
      Container.Repaint;
    end;
end;

procedure TfrmStyle.UpdateDebugInfo;
var
  txt: String;
  LLCount: Integer;
begin
  if Assigned(Container) then
    begin
      LLCount := 0;
      Inc(FrameCount);
      txt := '';
      if Assigned(Layers) then
        LLCount := Length(Layers);
      txt := 'Layers = ' + IntToStr(LLCount)
        + ', Frames = ' + IntToStr(FrameCount)
        ;

      lblInfo.Text := txt;
    end;
end;

procedure TfrmStyle.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//  UpdateDebugInfo;
  if FSaveInNextPaint then
    DoSaveLayers;
end;

procedure TfrmStyle.DoSaveLayers;
var
  AWidth, AHeight: Integer;
  LSurface: ISkSurface;
  IDX: Integer;
  sw: TStopWatch;
  rend: Single;
begin
  FSaveInNextPaint := False;
  sw := TStopWatch.StartNew;

  AWidth := Container.ChildMaxImWidth;
  AHeight := Container.ChildMaxImHeight;

  if (Container.Canvas is TGrCanvasCustom) and Assigned(TGrCanvasCustom(Container.Canvas).Context) then
    LSurface := TSkSurface.MakeRenderTarget(TGrCanvasCustom(Container.Canvas).Context, False, TSkImageInfo.Create(AWidth, AHeight))
  else
    LSurface := TSkSurface.MakeRaster(AWidth, AHeight);
  LSurface.Canvas.Clear(TAlphaColors.Null);

  if Assigned(Container) and (Container.ChildrenCount > 1) then
    begin
      for IDX := 0 to Container.ChildrenCount - 1 do
        begin
          if Assigned(Container.Children[IDX]) and (Container.Children[IDX] is TLayerShader) then
            begin
              var ThisLayer: TLayerShader := Container.Children[IDX] as TLayerShader;
              ThisLayer.OnDraw(ThisLayer, LSurface.Canvas, RectF(0, 0, AWidth, AHeight), 1);
            end
          else if Assigned(Container.Children[IDX]) and (Container.Children[IDX] is TProgressShader) then
            begin
              var ThisLayer: TProgressShader := Container.Children[IDX] as TProgressShader;
              ThisLayer.OnDraw(ThisLayer, LSurface.Canvas, RectF(0, 0, AWidth, AHeight), 1);
            end;
        end;
    end;

  rend := sw.ElapsedMilliseconds;

  LSurface.MakeImageSnapshot.EncodeToFile(SaveImageDialog.FileName);
  lblInfo.Text := 'Saving ' +
    ExtractFileName(SaveImageDialog.FileName) + ' took ' +
    FormatFloat('0.000s', rend / 1000) + ' to compose and ' +
    FormatFloat('0.000s', (sw.ElapsedMilliseconds - rend) / 1000) + ' to write';
end;

end.
