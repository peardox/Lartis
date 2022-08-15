unit OptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Skia, Skia.FMX,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Layouts, FMX.TabControl;

type
  TfrmOptions = class(TForm)
    Panel3: TPanel;
    Button1: TButton;
    Panel4: TPanel;
    TabControl1: TTabControl;
    StyleTab: TTabItem;
    TrainTab: TTabItem;
    SystemTab: TTabItem;
    SystermLayout: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    cbWipeOnStart: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.fmx}

uses
  Settings;

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
//  SystermLayout
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  Label1.Text := 'Lartis Home';
  Label2.Text := AppHome;
end;

end.
