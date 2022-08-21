unit StyleForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts,
  Shaders;

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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure trkStyleWeightChange(Sender: TObject);
  private
    { Private declarations }
    Grid: TGridShader;
    ImageLayer: TBaseShader;
    ProgressLayer: TProgressShader;
    Container: TAspectLayout;
  public
    { Public declarations }
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
              trkStyleWeight.Value := 1;
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
              CurrentBitmap := TBitmap.Create(TProgressShader(ImageLayer).Bitmap.Width, TProgressShader(ImageLayer).Bitmap.Height);
              CurrentBitmap.CopyFromBitmap(TProgressShader(ImageLayer).Bitmap);
            end;
          if (CurrentImage <> String.Empty) and Assigned(CurrentBitmap) then
            begin
              TProgressShader(ImageLayer).Free;

              PySys.Log('Adding LayerShader');
              ImageLayer := TLayerShader.Create(Container);
              with ImageLayer as TLayerShader do
                begin
                  PySys.Log('Adding Original ' + CurrentImage);
                  OriginalImage := CurrentImage;
                  AddBitmap(Original, CurrentBitmap);
                  PySys.Log('Adding Styled ' + AFileName);
                  AddImage(Styled, AFileName);
                  PreserveTransparency := False;
                  trkStyleWeight.Value := 0.5;
                  trkStyleWeight.Enabled := True;
                  ProgressBar1.Value := 0;
                end;
            end;
        end;
    end;
//    FreeAndNil(CurrentBitMap);

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

procedure TfrmStyle.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
//      Container := TAspectLayout.Create(StyleLayout);
//      Grid := TGridShader.Create(Container);
end;


end.
