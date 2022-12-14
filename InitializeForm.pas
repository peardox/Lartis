unit InitializeForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Layouts, FMX.Memo.Types,
  System.IOUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Threading, System.Net.HttpClientComponent,
  LartisTypes,  EmbeddedForm,
  System.Diagnostics, FMX.ScrollBox, FMX.Memo, Skia, Skia.FMX;

type
  TInstallFinishedEvent = procedure(Sender: TObject; const AStatus: Boolean) of object;

  TfrmInit = class(TEmbeddedForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    StatusBar: TStatusBar;
    ProgressBar1: TProgressBar;
    layInstall: TLayout;
    laySplash: TLayout;
    btnAbort: TButton;
    btnInstall: TButton;
    Image1: TImage;
    Layout3: TLayout;
    Memo1: TMemo;
    layBlurb: TLayout;
    Blurb: TSkLabel;
    btnHomeLocation: TButton;
    Timer1: TTimer;
    procedure btnAbortClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure laySplashResize(Sender: TObject);
    procedure btnHomeLocationClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    DidRunInstall: Boolean;
    SystemError: Boolean;
    SettingUp: Boolean;
    FHaveGPU: Boolean;
    FInstallFinishedEvent: TInstallFinishedEvent;
    SetupInfo: String;
    procedure DoInstallFinished(Sender: TObject; const AStatus: Boolean);
    procedure AllDone;
    procedure DoGPUWarmup;
    function DoSystemTest: Boolean;
    procedure FinishInit(Sender: TObject; const AStatus: Boolean);
    function HandleFileList(const AFileList: String): TJSONFileArray;
    procedure MultiThreadedMediaDownload(const outpath: String);
    procedure SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
      const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
      Logger: TMemo = Nil; Progress: TProgressBar = Nil);
    procedure SetupComplete(Sender: TObject; const AStatus: Boolean);
    procedure ToggleBlurb(const Force: TComponent = Nil);
    procedure CheckPaths;
    procedure Log(const AMsg: String);
    procedure GetVersionUpdate;
  public
    { Public declarations }
    procedure CheckWipe;
  published
    property OnInstallFinished: TInstallFinishedEvent read FInstallFinishedEvent write FInstallFinishedEvent;
  end;

var
  frmInit: TfrmInit;
  AbortFlag: Boolean;

implementation

uses
  Settings,
  PythonSystem,
  JSON.Serializers,
  Unit1 {frmMain},
  OptionsForm {frmOptions},
  DebugForm {frmDebug},
  Downloader,
  Math;

{$R *.fmx}

procedure TfrmInit.Log(const AMsg: String);
begin
  Memo1.Lines.Add(AMsg);
end;

procedure TfrmInit.GetVersionUpdate;
var
  SysCode: String;
begin
  RESTClient1.BaseURL := APIBase;
  RESTRequest1.Resource := 'SystemCode.py';
  RESTRequest1.AcceptEncoding := 'gzip, deflate';
  RESTRequest1.Execute;
  if RestResponse1.StatusCode = 200 then
    begin
      SysCode := RestResponse1.Content;
      try
        TFile.WriteAllText(IncludeTrailingPathDelimiter(AppHome) + 'SystemCode.py', SysCode);
      except
         on E : Exception do
           ShowMessage('Couldn''t update system code, ' +
            'Exception : Class = ' + E.ClassName +
            ', Message = ' + E.Message);
      end;
    end;
end;

procedure TfrmInit.FormCreate(Sender: TObject);
begin
  DidRunInstall := False;
  SettingUp := True;
  layInstall.Visible := True;
  StatusBar.Visible := True;
  FHaveGPU := True;
  AbortFlag := False;
  Caption := 'System Setup';

  Memo1.Align := TAlignLayout.Client;
  layBlurb.Align := TAlignLayout.Client;
  Blurb.TextSettings.FontColor := TAlphaColorRec.Azure;
  ToggleBlurb(layBlurb);

  btnAbort.Visible := False;
  btnAbort.Text := 'Abort';
  btnInstall.Text := 'Install';

  RESTClient1.BaseURL := APIBase;
  RESTRequest1.Resource := 'system.json';
  RESTRequest1.AcceptEncoding := 'gzip, deflate';
  RESTRequest1.Execute;
//  Log('JSON = ' + RestResponse1.StatusCode.ToString);
  if RestResponse1.StatusCode = 200 then
    begin
      Log(SetupInfo);
      SetupInfo := RestResponse1.Content;
    end
  else
    begin
      ShowMessage('Something has gone horribly wrong');
    end;

    if SystemSettings.WipeOnStart then
       begin

       end
    else
      begin
      if not InstallRequired and SystemSettings.PythonInstalled then
        begin
          layInstall.Visible := False;
          StatusBar.Visible := False;
          CheckPaths;
          if VersionUpdate then
            GetVersionUpdate;
          AllDone;
        end;
      end;
end;

procedure TfrmInit.CheckPaths;
begin
  try

    TempPath := TPath.Combine(AppHome, 'temp');;
    if not DirectoryExists(TempPath) then
      begin
        ForceDirectories(TempPath);
      end;

    ShaderPath := TPath.Combine(AppHome, 'shaders');;
    if not DirectoryExists(ShaderPath) then
      begin
        ForceDirectories(ShaderPath);
      end;

    SystemStyle := '';
    StylesPath := TPath.Combine(AppHome, 'styles');;
    if not DirectoryExists(ShaderPath) then
      begin
        ForceDirectories(StylesPath);
      end;

    PreTrainedPath := TPath.Combine(AppHome, 'pretrained');;
    if not DirectoryExists(PreTrainedPath) then
      begin
        ForceDirectories(PreTrainedPath);
      end;

    DataSetsPath := TPath.Combine(AppHome, 'datasets');
    if not DirectoryExists(DataSetsPath) then
      begin
        ForceDirectories(DataSetsPath);
      end;

    CachePath := TPath.Combine(AppHome, 'cache');;
    if not DirectoryExists(CachePath) then
      begin
        ForceDirectories(CachePath);
      end;

    SaveSystemSettings;
    Log('Updated System Settings');
    Log('Checked paths');

  except
    on E: Exception do
      begin
        Log('Unhandled Exception in CheckPaths');
        Log('Class : ' + E.ClassName);
        Log('Error : ' + E.Message);
      end;
  end;
end;

procedure TfrmInit.CheckWipe;
begin
  if SystemSettings.WipeOnStart then
    begin
      MessageDlg('The system is marked for wiping - do you really want to do this?',
        TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0, TMsgDlgBtn.mbNo,
          procedure(const AResult: TModalResult)
            begin
              if AResult = mrYes then
                begin
                  if DirectoryExists(AppHome) then
                    begin
                      layInstall.Visible := True;
                      StatusBar.Visible := True;
                      Log('Wiping system, please wait...');
                      TDirectory.Delete(AppHome, True);
                      InitialiseSystem;
                      Log('System wiped.');
                    end;
                end
              else
                begin
                  layInstall.Visible := False;
                  StatusBar.Visible := False;
                  AllDone;
                end;
              SystemSettings.WipeOnStart := False;
            end);
    end;
end;

// Center the logo
procedure TfrmInit.laySplashResize(Sender: TObject);
begin
  Image1.Width := laySplash.Width;
  Image1.Height := Floor(Image1.Width * (640 / 1920));
  Image1.Position.X := 0;
  Image1.Position.Y := 0;

  if Image1.Height > (laySplash.Height * 0.69) then
    begin
      Image1.Height := Floor(laySplash.Height * 0.6848);
      Image1.Width := Image1.Height * (1920 / 640);
      Image1.Position.X := (laySplash.Width - Image1.Width) / 2;
      Image1.Position.Y := (laySplash.Height - Image1.Height) / 2;
    end;
end;

procedure TfrmInit.Timer1Timer(Sender: TObject);
begin
  Application.ProcessMessages;
end;

procedure TfrmInit.ToggleBlurb(const Force: TComponent = Nil);
begin
  if Force is TMemo then
    begin
      layBlurb.Visible := False;
      Memo1.Visible := True;
    end
  else  if Force is TLayout then
    begin
      layBlurb.Visible := True;
      Memo1.Visible := False;
    end
  else if Memo1.Visible then
    begin
      layBlurb.Visible := True;
      Memo1.Visible := False;
    end
  else
    begin
      layBlurb.Visible := False;
      Memo1.Visible := True;
    end;
end;

procedure TfrmInit.DoInstallFinished(Sender: TObject; const AStatus: Boolean);
begin
  if Assigned(FInstallFinishedEvent) then
    FInstallFinishedEvent(Self, AStatus);
end;

procedure TfrmInit.btnAbortClick(Sender: TObject);
begin
  Log('Aborting');
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
  btnAbort.Enabled := False;
  AbortFlag := True;
  Application.Terminate;
end;

procedure TfrmInit.btnHomeLocationClick(Sender: TObject);
var
  LPath: String;
begin
  if FMX.Dialogs.SelectDirectory('Select Data Directory Location', AppHome, LPath) then
    begin
      AppHome := LPath;
      SystemSettings.AppHome := AppHome;
      SaveSystemSettings;
      Log('Updated System Settings');
    end;
end;

function TfrmInit.HandleFileList(const AFileList: String): TJSONFileArray;
var
  lSerializer: TJsonSerializer;
  data: TJSONFileList;
  Idx, I, F: Integer;
  Res: TJSONFileArray;
begin
  lSerializer := TJsonSerializer.Create;
  Idx := 0;
  SetLength(Res, Idx);
  try
    try
      data := lSerializer.Deserialize<TJSONFileList>(AFileList);
      for I := 0 to Length(data.Content) - 1 do
        begin
          for F := 0 to Length(data.Content[I].Data) - 1 do
            begin
              Inc(Idx);
              SetLength(Res, Idx);
              Res[Idx - 1] := data.Content[I].Data[F];
            end;
        end;
    except
      on E: Exception do
        begin
          Log('Unhandled Exception in MultiThreadedMediaDownload');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;
  finally
    FreeAndNil(lSerializer);
    Result := Res;
  end;
end;

procedure TfrmInit.AllDone;
begin
  btnAbort.Enabled := False;
  btnInstall.Enabled := False;
  ToggleBlurb(Memo1);
  PySys.LogTarget := Memo1;
  if SettingUp then
    begin
      SettingUp := False;
      PySys.Log('Starting Python Subsystem');
      Timer1.Enabled := True;
      PySys.SetupSystem(SetupComplete, FHaveGPU, True);
      Timer1.Enabled := False;
    end;
end;

procedure TfrmInit.DoGPUWarmup;
begin
  PySys.Log('One moment please, warming up GPU');
  PySys.modCalibrate.CalibrateStyle(True, 1, 256);
end;

function TfrmInit.DoSystemTest: Boolean;
begin
  SystemError := PySys.RunShim(pyshim);

  if not SystemError then
    SystemError := PySys.RunTest;
  if not SystemError then
    SystemError := PySys.RunSystem;

  Result := not SystemError;
end;

procedure TfrmInit.SetupComplete(Sender: TObject; const AStatus: Boolean);
begin
  if AStatus and EnableGPU then
    begin
      DoGPUWarmup;
    end;
  if DidRunInstall then
    begin
      btnInstall.Enabled := True;
      FinishInit(Self, AStatus);
      btnInstall.Text := 'Continue';
    end
  else
    begin
      FinishInit(Self, AStatus);
    end;
end;

procedure TfrmInit.FinishInit(Sender: TObject; const AStatus: Boolean);
begin
  if InstallRequired then
    begin
      Log('Installation Complete');
      Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
    end
  else
    begin
      Log('Setup Complete');
      Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'setup.log');
    end;
  PySys.LogTarget := Nil;
  if Assigned(CloseMyself) then
      CloseMyself(Self);
  DoInstallFinished(Self, AStatus);
end;

procedure TfrmInit.btnInstallClick(Sender: TObject);
begin
  if not DidRunInstall then
    begin
      btnHomeLocation.Enabled := False;
      CheckPaths;
      ToggleBlurb(Memo1);
      btnInstall.Enabled := False;
      DidRunInstall := True;
      Log('Starting installation');
      MultiThreadedMediaDownload(AppHome);
      AllDone;
    end
  else
    begin
      FinishInit(Self, True);
    end;
end;

procedure TfrmInit.MultiThreadedMediaDownload(const outpath: String);
var
  FileList: TJSONFileArray;
  I: Integer;
  DirList: TStringList;
  FullSize: Integer;
begin
  Memo1.Lines.Clear;
  FullSize := 0;
  try
    try
      FileList := HandleFileList(SetupInfo);
      DirList := TStringList.Create;
      DirList.Sorted := True;
      DirList.Duplicates := dupIgnore;

      for I := 0 to Length(FileList) - 1 do
        begin
          FullSize := FullSize + FileList[I].Size;
          DirList.Add(TPath.GetDirectoryName(TPath.Combine(outpath, UnixToDos(FileList[I].Name))));
        end;
      for I := 0 to DirList.Count - 1 do
        begin
          if not DirectoryExists(DirList[I]) then
            begin
              if ForceDirectories(DirList[I]) then
                Log('Created ' + DirList[I])
              else
                Log('Failed to created ' + DirList[I])
            end;
        end;

      DirList.Free;
      Log('Got filelist');

      Log('Downloading');
      SingleThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1, ProgressBar1);

    except
      on E: Exception do
        begin
          Log('Unhandled Exception in MultiThreadedMediaDownload');
          Log('Class : ' + E.ClassName);
          Log('Error : ' + E.Message);
        end;
    end;

  finally

  end;
end;

procedure TfrmInit.SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
  const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
  Logger: TMemo = Nil; Progress: TProgressBar = Nil);
var
  sw: TStopWatch;
  Downer: TDownerClient;
  I: Integer;
begin
  if Assigned(Logger) then
    begin
      Logger.Lines.Add('Downloader');
    end;
  if Assigned(Progress) then
    begin
      Progress.Min := 0;
      Progress.Max := FullSize;
    end;
  sw := TStopWatch.StartNew;

  for I := 0 to ImageCount - 1 do
    begin
      if AbortFlag then
        Exit;

      Downer := TDownerClient.Create(Self);
      var infilerec := AFileList[I];
      var outfile := TPath.Combine(ADestPath, UnixToDos(infilerec.Name));
      try
        if Assigned(Logger) then
          begin
            Logger.Lines.Add('Downloading ' + infilerec.Name);
            Logger.GoToTextEnd;
            Logger.Repaint;
          end;
        Downer.Download(ABaseURL, ADestPath, infilerec, I, Progress);
        Application.ProcessMessages;
      except
        on E: Exception do
          begin
            if Assigned(Logger) then
              begin
                Logger.Lines.Add('Unhandled Exception in SingleThreadDownload');
                Logger.Lines.Add('Class : ' + E.ClassName);
                Logger.Lines.Add('Error : ' + E.Message);
                Logger.Lines.Add('Vars : I = ' + i.ToString + ', OutFile = ' + outfile + ', ImageCount = ' + ImageCount.ToString);
                Logger.GoToTextEnd;
                Logger.Repaint;
              end;
          end;

      end;
      FreeAndNil(Downer);
    end;

  if Assigned(Logger) then
      Logger.Lines.Add('Finished ' + sw.ElapsedMilliseconds.ToString);
  if Assigned(Progress) then
      Progress.Value := 0;
  AllDone;
end;

end.
