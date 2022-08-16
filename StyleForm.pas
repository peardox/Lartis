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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Grid: TGridShader;
    StyleLayer: TLayerShader;
    OriginalLayer: TLayerShader;
    Container: TAspectLayout;
  end;

var
  frmStyle: TfrmStyle;

implementation

uses
  Settings,
  PythonSystem;

{$R *.fmx}

procedure TfrmStyle.btnOpenFileClick(Sender: TObject);
begin
  if Assigned(PySys) then
    begin
      if OpenDialog1.Execute then
        begin
          if Assigned(OriginalLayer) then
            begin
              FreeAndNil(OriginalLayer);
              OriginalLayer := TLayerShader.Create(Container);
            end;

          OriginalLayer.AddImage(Original, OpenDialog1.FileName);
 //         PySys.modStyle.Stylize(OpenDialog1.Filename);
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
      Container := TAspectLayout.Create(StyleLayout);

      Grid := TGridShader.Create(Container);


      StyleLayer := TLayerShader.Create(Container);
//      StyleLayer.AddImage(Styled, TPath.Combine(MediaPath, 'haywain-wall.jpg'));
//      StyleLayer.AddImage(Original, TPath.Combine(MediaPath, 'haywain.jpg'));

      OriginalLayer := TLayerShader.Create(Container);
//      OriginalLayer.AddImage(Styled, TPath.Combine(MediaPath, 'fermin-6.jpg'));
end;


end.
