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
    TrackBar1: TTrackBar;
    lblStyleWeightKey: TLabel;
    lblStyleWeightValue: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure StyleLayoutResize(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Grid: TGridShader;
    ImageLayer: TLayerShader;
    ClockLayer: TClockShader;
    Container: TAspectLayout;
    procedure ShowStyleProgress(Sender: TObject; const AValue: Single);
    procedure AddStyledImage(Sender: TObject; const AFileName: String);
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
  ProgressBar1.Value := AValue;
end;

procedure TfrmStyle.StyleLayoutResize(Sender: TObject);
begin
  if Assigned(Container) then
    begin
      Container.FitToContainer;
    end;
end;

procedure TfrmStyle.TrackBar1Change(Sender: TObject);
begin
  if Assigned(ImageLayer) then
    begin
      ImageLayer.fStyleWeight := (TrackBar1.Value / TrackBar1.Max);
      lblStyleWeightValue.Text := FormatFloat('##0.00', ImageLayer.fStyleWeight * 100);
    end;
end;

procedure TfrmStyle.btnOpenFileClick(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          if Assigned(ImageLayer) then
            FreeAndNil(ImageLayer);

          ImageLayer := TLayerShader.Create(Container);
          ImageLayer.AddImage(Original, OpenDialog1.FileName);
        end;
    end;
end;

procedure TfrmStyle.AddStyledImage(Sender: TObject; const AFileName: String);
begin
  if Assigned(PySys) then
    begin
      if Assigned(ImageLayer) then
        begin
          PySys.Log('Adding Styled Image ' + AFileName);
          ImageLayer.AddImage(Styled, AFileName);
          ImageLayer.iPreserveTransparency := False;
          ImageLayer.fStyleWeight := 1.0;
        end;
    end;
end;

procedure TfrmStyle.Button2Click(Sender: TObject);
begin
  if Assigned(PySys) and Assigned(ImageLayer) then
    begin
      if ImageLayer.OriginalImage <> String.Empty then
        PySys.modStyle.Stylize(ImageLayer.OriginalImage);
    end;
end;

procedure TfrmStyle.Button1Click(Sender: TObject);
begin
  if Assigned(CloseMyself) then
      CloseMyself(Self);
end;

procedure TfrmStyle.FormCreate(Sender: TObject);
begin
      Container := TAspectLayout.Create(StyleLayout);
      Grid := TGridShader.Create(Container);
end;


end.
