unit Unit2;

interface
 {$DEFINE ENABLE_PYTHON}
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation,
  Shaders, FMX.Objects;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    StyleLayout: TRectangle;
    VertScrollBox1: TVertScrollBox;
    btnOpenFile: TButton;
    Button2: TButton;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure StyleLayoutPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    { Private declarations }
  public
    { Public declarations }
    Grid: TGridShader;
    ImageLayer: TLayerShader;
    Container: TAspectLayout;
    procedure ShowStyleProgress(const AValue: Single);
    procedure AddStyledImage(Sender: TObject; const AFileName: String);
    procedure MakeAvailable(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}
uses
  Skia, Skia.FMX,
  Settings,
  Math,
  FunctionLibrary,
{$IFDEF ENABLE_PYTHON}
  PythonSystem,
  SetupForm,
{$ENDIF}
  OptionsForm,
  DebugForm;

procedure TForm2.MakeAvailable(Sender: TObject);
begin
{$IFDEF ENABLE_PYTHON}
  frmSetup.Close;
{$ENDIF}

  Container := TAspectLayout.Create(StyleLayout);
  Grid := TGridShader.Create(Container);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Caption := appname;
{$IFDEF ENABLE_PYTHON}
  frmSetup := TfrmSetup.Create(Application);
  frmSetup.Parent := Self;
  frmSetup.Show;
{$ELSE}
  MakeAvailable(Self);
{$ENDIF}
end;

procedure TForm2.ShowStyleProgress(const AValue: Single);
begin
  ProgressBar1.Value := AValue;
end;


procedure TForm2.StyleLayoutPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//  if Assigned(ImageLayer) then
//    ImageLayer.InvalidateRect(ARect);
//  InValidate;
end;

procedure TForm2.StyleLayoutResize(Sender: TObject);
begin
  if Assigned(Container) then
    begin
      Container.FitToContainer;
    end;
end;

procedure TForm2.TrackBar1Change(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    begin
      ImageLayer.fStyleWeight := (TrackBar1.Value / TrackBar1.Max);
      lblStyleWeightValue.Text := FormatFloat('##0.00', ImageLayer.fStyleWeight * 100);
    end;
end;

procedure TForm2.btnOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      if Assigned(ImageLayer) then
        FreeAndNil(ImageLayer);

      ImageLayer := TLayerShader.Create(Container);
      ImageLayer.AddImage(Original, OpenDialog1.FileName);
    end;
end;


procedure TForm2.Button2Click(Sender: TObject);
begin
{$IFDEF ENABLE_PYTHON}
  if Assigned(PySys) and Assigned(ImageLayer) then
    begin
      if ImageLayer.OriginalImage <> String.Empty then
        PySys.modStyle.Stylize(ImageLayer.OriginalImage);
    end;
{$ELSE}
      if Assigned(ImageLayer) then
        begin
          ImageLayer.AddImage(Styled, 'C:\Users\simon\AppData\Roaming\Lartis\output-images\fermin-tile_test.jpg');
          ImageLayer.iPreserveTransparency := False;
          ImageLayer.fStyleWeight := 1.0;
        end;
{$ENDIF}
end;

procedure TForm2.AddStyledImage(Sender: TObject; const AFileName: String);
begin
{$IFDEF ENABLE_PYTHON}
  if Assigned(PySys) then
    begin
      if Assigned(ImageLayer) then
        begin
          ImageLayer.AddImage(Styled, AFileName);
          ImageLayer.iPreserveTransparency := False;
          ImageLayer.fStyleWeight := 1.0;
        end;
    end;
{$ENDIF}
end;

end.
