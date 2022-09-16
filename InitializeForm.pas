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
  System.Diagnostics, FMX.ScrollBox, FMX.Memo;

type
  TInstallFinishedEvent = procedure(Sender: TObject; const AStatus: Boolean) of object;

  TfrmInit = class(TEmbeddedForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Layout2Resize(Sender: TObject);
  private
    { Private declarations }
    FInstallFinishedEvent: TInstallFinishedEvent;
    SetupInfo: String;
    procedure DoInstallFinished(Sender: TObject; const AStatus: Boolean);
    procedure AllDone;
    function HandleFileList(const AFileList: String): TJSONFileArray;
    procedure MultiThreadedMediaDownload(const outpath: String);
    procedure MultiThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
      const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
      Logger: TMemo = Nil; Progress: TProgressBar = Nil);
    procedure SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
      const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
      Logger: TMemo = Nil; Progress: TProgressBar = Nil);
    procedure SetupComplete(Sender: TObject; const AStatus: Boolean);
  public
    { Public declarations }
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

procedure TfrmInit.DoInstallFinished(Sender: TObject; const AStatus: Boolean);
begin
  if Assigned(FInstallFinishedEvent) then
    FInstallFinishedEvent(Self, AStatus);
end;

procedure TfrmInit.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Aborting');
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
  Button1.Enabled := False;
  AbortFlag := True;
  Application.Terminate;
end;

function TfrmInit.HandleFileList(const AFileList: String): TJSONFileArray;
var
  lSerializer: TJsonSerializer;
  log: TJSONFileList;
  Idx, I, F: Integer;
  Res: TJSONFileArray;
begin
  lSerializer := TJsonSerializer.Create;
  Idx := 0;
  SetLength(Res, Idx);
  try
    try
      log := lSerializer.Deserialize<TJSONFileList>(AFileList);
      for I := 0 to Length(log.Content) - 1 do
        begin
          for F := 0 to Length(log.Content[I].Data) - 1 do
            begin
              Inc(Idx);
              SetLength(Res, Idx);
              Res[Idx - 1] := log.Content[I].Data[F];
            end;
        end;
    except
     on E : Exception do
     begin
       Memo1.Lines.Add('Exception class name = '+E.ClassName);
       Memo1.Lines.Add('Exception message = '+E.Message);
       Memo1.Lines.Add(AFileList);
     end;
    end;
  finally
    FreeAndNil(lSerializer);
    Result := Res;
  end;
end;

// Center the logo
procedure TfrmInit.Layout2Resize(Sender: TObject);
begin
  Image1.Width := Layout2.Width;
  Image1.Height := Floor(Image1.Width * (640 / 1920));
  Image1.Position.X := 0;
  Image1.Position.Y := 0;

  if Image1.Height > (Layout2.Height * 0.69) then
    begin
      Image1.Height := Floor(Layout2.Height * 0.6848);
      Image1.Width := Image1.Height * (1920 / 640);
      Image1.Position.X := (Layout2.Width - Image1.Width) / 2;
      Image1.Position.Y := (Layout2.Height - Image1.Height) / 2;
    end;
end;

procedure TfrmInit.SetupComplete(Sender: TObject; const AStatus: Boolean);
begin
  PySys.LogTarget := Nil;
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'python-install.log');
  Memo1.Lines.Add('Installation Complete');
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
  if Assigned(CloseMyself) then
      CloseMyself(Self);
  DoInstallFinished(Self, True);
end;

procedure TfrmInit.AllDone;
begin
  PySys.LogTarget := Memo1;
  PySys.SetupSystem(SetupComplete);
end;

procedure TfrmInit.Button2Click(Sender: TObject);
begin
  Button2.Enabled := False;
  Memo1.Lines.Add('Starting installation');
  MultiThreadedMediaDownload(AppHome);
  AllDone;
end;

procedure TfrmInit.FormCreate(Sender: TObject);
begin
  AbortFlag := False;
  Caption := 'System Setup';
  Memo1.Lines.Add('Ready');

  Button1.Text := 'Abort';
  Button2.Text := 'Continue';

  RESTClient1.BaseURL := APIBase;
  RESTRequest1.Resource := 'system.json';
  RESTRequest1.AcceptEncoding := 'gzip, deflate';
  RESTRequest1.Execute;
  Memo1.Lines.Add('JSON = ' + RestResponse1.StatusCode.ToString);
  if RestResponse1.StatusCode = 200 then
    begin
      Memo1.Lines.Add(SetupInfo);
      SetupInfo := RestResponse1.Content;
    end
  else
    begin
      ShowMessage('Something has gone horribly wrong');
    end;

end;

procedure TfrmInit.MultiThreadedMediaDownload(const outpath: String);
var
  FileList: TJSONFileArray;
  I: Integer;
  DirList: TStringList;
  LDirectory: String;
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
                Memo1.Lines.Add('Created ' + DirList[I])
              else
                Memo1.Lines.Add('Failed to created ' + DirList[I])
            end;
        end;

      DirList.Free;
      Memo1.Lines.Add('Got filelist');

      {$IFNDEF MACOS64}
      Memo1.Lines.Add('Downloading');
      SingleThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1, ProgressBar1);
//      MultiThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1, ProgressBar1);
      {$ELSE}
      Memo1.Lines.Add('Downloading');
      SingleThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1, ProgressBar1);
      {$ENDIF}

    except
      on E: Exception do
        begin
          Memo1.Lines.Add('Unhandled Exception # 270');
          Memo1.Lines.Add('Class : ' + E.ClassName);
          Memo1.Lines.Add('Error : ' + E.Message);
        end;
    end;

  finally

  end;
end;

procedure TfrmInit.MultiThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
  const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
  Logger: TMemo = Nil; Progress: TProgressBar = Nil);
var
  sw: TStopWatch;
  Downer: TUnSplashClient;
  tp: TThreadPool;
  ThreadsToUse: Integer;
  TotalImageCount: Integer;
  TotalDone: Integer;
begin
  if Assigned(Progress) then
    begin
      Progress.Min := 0;
      Progress.Max := FullSize;
    end;
  TotalDone := 0;
  TotalImageCount := Length(AFileList);
  tp := TThreadPool.Create;
  ThreadsToUse := 4;
  if Assigned(Logger) then
    begin
      Logger.Lines.Add('CPUs : ' + CPUCount.ToString);
      Logger.Lines.Add('ProcesserThreads : ' + TThread.ProcessorCount.ToString);
      Logger.Lines.Add('MinThreads : ' + tp.MinWorkerThreads.ToString);
      Logger.Lines.Add('Threadpool MaxThreads : ' + tp.MaxWorkerThreads.ToString);
      Logger.Lines.Add('Threadpool MinThreads : ' + tp.MinWorkerThreads.ToString);
    end;
  if tp.SetMaxWorkerThreads(ThreadsToUse) then
    if Assigned(Logger) then
      begin
        Logger.Lines.Add('Threadpool set to ' + ThreadsToUse.ToString);
      end
  else
    if Assigned(Logger) then
      begin
        Logger.Lines.Add('Threadpool failed : ' + tp.MaxWorkerThreads.ToString);
      end;
  sw := TStopWatch.StartNew;

	TThread.CreateAnonymousThread(
		procedure
		begin
			TParallel.For(1, 0, ImageCount - 1,
				procedure(I: Integer)
				begin
					var Downer := TUnSplashClient.Create(Self);
          var infilerec := AFileList[I];
          var DownSize := AFileList[I].Size;
          var outfile := TPath.Combine(ADestPath, UnixToDos(infilerec.Name));
          try
            Downer.Download(ABaseURL, ADestPath, infilerec, I, Progress);
          except
            on E: Exception do
              begin
                TThread.Synchronize(nil,
                  procedure
                    begin
                      if Assigned(Logger) then
                        begin
                          Logger.Lines.Add('Unhandled Exception # 544');
                          Logger.Lines.Add('Class : ' + E.ClassName);
                          Logger.Lines.Add('Error : ' + E.Message);
                          Logger.Lines.Add('Vars : I = ' + i.ToString + ', OutFile = ' + outfile + ', ImageCount = ' + ImageCount.ToString);
                        end;
                    end);
              end;

          end;
          FreeAndNil(Downer);
				end
			, tp);
			TThread.Synchronize(nil,
				procedure
				begin
          if Assigned(Logger) then
            begin
					    Logger.Lines.Add('Finished ' + sw.ElapsedMilliseconds.ToString);
            end;
          tp.Free;
          AllDone;
				end
			);
		end
	).Start;
end;

procedure TfrmInit.SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
  const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
  Logger: TMemo = Nil; Progress: TProgressBar = Nil);
var
  sw: TStopWatch;
  Downer: TUnSplashClient;
  TotalImageCount: Integer;
  I, TotalDone: Integer;
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
  TotalDone := 0;
  TotalImageCount := Length(AFileList);
  sw := TStopWatch.StartNew;

  for I := 0 to ImageCount - 1 do
    begin
      if AbortFlag then
        Exit;

      Downer := TUnSplashClient.Create(Self);
      var infilerec := AFileList[I];
      var DownSize := AFileList[I].Size;
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
                Logger.Lines.Add('Unhandled Exception # 416');
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
