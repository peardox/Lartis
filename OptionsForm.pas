unit OptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, Skia, Skia.FMX,
  {$IFDEF MSWINDOWS}
  ShellApi,
  {$ENDIF}
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
    lblHomeLabel: TLabel;
    lblHomeValue: TLabel;
    Label3: TLabel;
    cbWipeOnStart: TCheckBox;
    btnHomeOpen: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnHomeOpenClick(Sender: TObject);
    procedure cbWipeOnStartChange(Sender: TObject);
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
  {$IFNDEF LINUX}
  DW.OSDevice,
  {$ENDIF}
  Settings;

procedure TfrmOptions.btnHomeOpenClick(Sender: TObject);
begin
//  TOSDevice.ShowFilesInFolder([ExcludeTrailingPathDelimiter(AppHome)]);
{$IFDEF MSWINDOWS}
  ShellExecute(0, 'open', 'explorer.exe', PChar('"' + ExcludeTrailingPathDelimiter(AppHome) + '"'), nil, 1);
  ModalResult := mrOK;
{$ENDIF}
end;

procedure TfrmOptions.Button1Click(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TfrmOptions.cbWipeOnStartChange(Sender: TObject);
begin
  SystemSettings.WipeOnStart := cbWipeOnStart.IsChecked;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  {$IFNDEF MSWINDOWS}
  btnHomeOpen.Visible := False;
  {$ENDIF}
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  lblHomeLabel.Text := 'Lartis Home';
  lblHomeValue.Text := AppHome;

  cbWipeOnStart.IsChecked := SystemSettings.WipeOnStart;

end;

end.
