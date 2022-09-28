unit ZipLartis;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  System.Zip, Settings, PythonSystem,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TZipExtractForm = class(TForm)
    ProgressBar1: TProgressBar;
    Panel1: TPanel;
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure ExtractLartisZip;
    procedure ShowZipProgress(Sender: TObject; AFilename: String; AHeader: TZipHeader; APosition: Int64);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    ProgCount: Int64;
    PaintCount: Int64;
    ProgTick: Int64;
    ProgFile: String;
    ProgressBar: TProgressBar;
    procedure FormReset;
  public
    { Public declarations }
  end;

var
  ZipExtractForm: TZipExtractForm;

implementation

{$R *.fmx}

uses
  Math,
  System.IOUtils;

procedure TZipExtractForm.ShowZipProgress(Sender: TObject; AFilename: String; AHeader: TZipHeader; APosition: Int64);
var
  PosTick: Int64;
begin
  if Assigned(ProgressBar) then
    begin
      if(ProgFile <> AFilename) then
        begin
          ProgFile := AFilename;
          Inc(ProgCount);
          ProgressBar.Value := ProgCount;
          PosTick := floor((ProgCount / ProgressBar.Max) * ProgressBar.Width);
          if(ProgTick <> PosTick) then
            begin
              ProgTick := PosTick;
              Inc(PaintCount);
              ProgressBar.Repaint;
              Application.ProcessMessages;
            end;
        end;
    end;
end;

procedure TZipExtractForm.Button2Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TZipExtractForm.ExtractLartisZip;
var
  z: TZipFile;
  ZipOut: String;
  I, ZipCount: Int64;
begin
  z := Nil;

  if OpenDialog1.Execute then
    begin
      ProgFile := '';
      ProgTick := 0;
      PaintCount := 0;
      ZipOut := TPath.Combine(AppHome, 'models');
      {$IFDEF TESTING}
      if DirectoryExists(ZipOut) then
        TDirectory.Delete(ZipOut, True);
      {$ENDIF}
      Label1.TextSettings.HorzAlign := TTextAlign.Leading;

      if not DirectoryExists(ZipOut) then
        ForceDirectories(ZipOut);
      try
        try
          z := TZipFile.Create;
          z.Open(OpenDialog1.Filename, TZipMode.zmRead);


          ZipCount := Length(z.FileNames);
          PySys.Log('Files = ' + IntToStr(ZipCount));

          ProgressBar.Min := 0;
          ProgressBar.Max := ZipCount - 1;
          ProgressBar.Value := 0;
          z.OnProgress := ShowZipProgress;

          for I := 0 to ZipCount - 1 do
            begin
              Label1.Text := 'Extracting ' + z.FileName[I]
                + ' (' + IntToStr(I+1)
                + ' of ' + IntToStr(ZipCount) + ')';
              z.Extract(i, ZipOut);
            end;

        except
          on E: Exception do
            begin
              PySys.Log('Unhandled Exception in RunShim');
              PySys.Log('Class : ' + E.ClassName);
              PySys.Log('Error : ' + E.Message);
            end;
        end;
      finally
        z.Free;
      end;

      Label1.TextSettings.HorzAlign := TTextAlign.Center;
      Label1.Text := 'Imported All Styles (You can delete the archive if you want)';
      Button1.Text := 'Close';
      Button1.Enabled := True;

      ProgressBar.Value := 0;
    end
  else
    ModalResult := mrCancel;
end;

procedure TZipExtractForm.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  Button2.Enabled := False;
  if Button1.Text = 'Close' then
    begin
      ModalResult := mrClose
    end
  else
    ExtractLartisZip;
end;

procedure TZipExtractForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if ModalResult <> mrClose then
    ModalResult := mrCancel;
end;

procedure TZipExtractForm.FormCreate(Sender: TObject);
var
  DownPath: String;
  DocPath: String;
begin
  Label1.Text := 'Import a Lartis Style Archive';
  Label1.TextSettings.HorzAlign := TTextAlign.Center;
  ProgressBar := ProgressBar1;
  DocPath := TPath.GetDocumentsPath;
  DownPath := ExpandFileName(TPath.Combine(TPath.Combine(DocPath, '..'), 'Downloads'));
  if not DirectoryExists(DownPath) then
    begin
      if DirectoryExists(DocPath) then
        DownPath := DocPath
      else
        DownPath := '';
    end;

  OpenDialog1.Filter:='Lartis Style Archives (*.lartis.zip)|*.lartis.zip';
  OpenDialog1.InitialDir := DownPath;
end;

procedure TZipExtractForm.FormShow(Sender: TObject);
begin
  FormReset;
end;

procedure TZipExtractForm.FormActivate(Sender: TObject);
begin
  FormReset;
end;

procedure TZipExtractForm.FormReset;
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  ModalResult := mrNone;
end;

end.
