unit InitializeForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, REST.Types, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope, FMX.Layouts, FMX.Memo.Types,
  System.IOUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Threading, System.Net.HttpClientComponent,
  System.Diagnostics, FMX.ScrollBox, FMX.Memo;

type
  TJSONFile = Record
    Name: String;
    Size: Integer;
    Time: Integer;
  End;
  TJSONFileArray = Array of TJSONFile;

  TJSONMeta = Record
    Content: String;
    Size: Integer;
    Version: String;
  End;

  TJSONContent = Record
    Name: String;
    Desc: String;
    Inst: String;
    Items: Integer;
    Size: Integer;
    Data: TJSONFileArray;
  End;
  TJSONContentArray = Array of TJSONContent;

  TJSONFileList = Record
    Meta: TJSONMeta;
    Content: TJSONContentArray;
  End;

  TUnSplashClient = class(TObject)
  private
    FDownloadIndex: Integer;
    FDownloadStream: TStream;
    FClient: THTTPClient;
    FProgress: TProgressBar;
    FBytesSoFar: Int64;
    procedure ReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var AAbort: Boolean);
    procedure NeedClientCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificateList: TCertificateList;
      var AnIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Download(const ABaseURL: String; const Outpath: String; const AFile: TJSONFile; const ADownloadIndex: Integer; Progress: TProgressBar); overload;
  end;

  TfrmInit = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    StatusBar1: TStatusBar;
    ProgressBar1: TProgressBar;
    Layout1: TLayout;
    Layout2: TLayout;
    Button1: TButton;
    Memo1: TMemo;
    StyleBook1: TStyleBook;
    Button2: TButton;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    SetupInfo: String;
    procedure AllDone;
    function HandleFileList(const AFileList: String): TJSONFileArray;
    procedure MultiThreadedMediaDownload(const outpath: String);
    procedure MultiThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
      const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
      Logger: TStrings = Nil; Progress: TProgressBar = Nil);
    procedure SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
      const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
      Logger: TStrings = Nil; Progress: TProgressBar = Nil);
  public
    { Public declarations }
  end;

var
  frmInit: TfrmInit;
  AbortFlag: Boolean;

function UnixToDos(const Filename: String): String; Inline;

implementation

uses
  Settings,
  JSON.Serializers,
  Math;

{$R *.fmx}

constructor TUnSplashClient.Create;
begin
    FClient := THTTPClient.Create;
end;

destructor TUnSplashClient.Destroy;
begin
    FreeAndNil(FClient);
    inherited;
end;


procedure TUnSplashClient.ReceiveData(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  if Assigned(FProgress) then
    begin
      AAbort := AbortFlag;
      if TThread.CurrentThread.ThreadID <> MainThreadID then
        TThread.Synchronize(nil,
          procedure()
          begin
            FProgress.Value := FProgress.Value + (AReadCount - FBytesSoFar);
            Application.ProcessMessages;
          end
          )
      else
        begin
          FProgress.Value := FProgress.Value + (AReadCount - FBytesSoFar);
          Application.ProcessMessages;
        end;
    end;
  FBytesSoFar := AReadCount;
end;

function UnixToDos(const Filename: String): String;
begin
  Result := Filename{$IFDEF MSWINDOWS}.Replace('/','\\'){$ENDIF};
end;

procedure TUnSplashClient.NeedClientCertificate(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificateList: TCertificateList;
  var AnIndex: Integer);
begin
  ShowMessage('Cert requested');
end;

procedure TUnSplashClient.Download(const ABaseURL: String; const Outpath: String; const AFile: TJSONFile; const ADownloadIndex: Integer; Progress: TProgressBar);
var
  URL: string;
  LResponse: IHTTPResponse;
  LFileName: string;
  LDirectory: String;
  LSize: Int64;
begin
  LFileName := TPath.Combine(Outpath, UnixToDos(AFile.Name));
  try
    URL := ABaseUrl + AFile.Name;
    FDownloadIndex := ADownloadIndex;

    FClient.OnNeedClientCertificate := NeedClientCertificate;

    if Assigned(Progress) then
      begin
        FProgress := Progress;
        FClient.OnReceiveData := ReceiveData;
      end;

    // Create the file that is going to be dowloaded
    FDownloadStream := TFileStream.Create(LFileName, fmCreate);
    FDownloadStream.Position := 0;

    // Start the download process
    LResponse := FClient.Get(URL, FDownloadStream);

  finally
    FreeandNil(FDownloadStream);
  end;
end;

procedure TfrmInit.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Add('Aborting');
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
  Button1.Enabled := False;
  AbortFlag := True;
  ModalResult := mrAbort;
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

procedure TfrmInit.AllDone;
begin
  Memo1.Lines.Add('All done');
  Memo1.Lines.SaveToFile(IncludeTrailingPathDelimiter(AppHome) + 'install.log');
  ModalResult := mrClose;
end;

procedure TfrmInit.Button2Click(Sender: TObject);
begin
  Button2.Enabled := False;
  Memo1.Lines.Add('Starting installation');
  MultiThreadedMediaDownload(AppHome);
end;

procedure TfrmInit.FormCreate(Sender: TObject);
begin
  AbortFlag := False;
  Timer1.Enabled := False;
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
      Timer1.Interval := 10000;
      Timer1.Enabled := True;
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
      SingleThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1.Lines, ProgressBar1);
//      MultiThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1.Lines, ProgressBar1);
      {$ELSE}
      Memo1.Lines.Add('Downloading');
      SingleThreadDownload(Length(FileList), FileList, APIBase, outpath, FullSize, Memo1.Lines, ProgressBar1);
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
  Logger: TStrings = Nil; Progress: TProgressBar = Nil);
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
  Logger.Add('CPUs : ' + CPUCount.ToString);
  Logger.Add('ProcesserThreads : ' + TThread.ProcessorCount.ToString);
  Logger.Add('MinThreads : ' + tp.MinWorkerThreads.ToString);
  Logger.Add('Threadpool MaxThreads : ' + tp.MaxWorkerThreads.ToString);
  Logger.Add('Threadpool MinThreads : ' + tp.MinWorkerThreads.ToString);
  ThreadsToUse := 4;
  if tp.SetMaxWorkerThreads(ThreadsToUse) then
    Logger.Add('Threadpool set to ' + ThreadsToUse.ToString)
  else
    Logger.Add('Threadpool failed : ' + tp.MaxWorkerThreads.ToString);
  sw := TStopWatch.StartNew;

	TThread.CreateAnonymousThread(
		procedure
		begin
			TParallel.For(1, 0, ImageCount - 1,
				procedure(I: Integer)
				begin
					var Downer := TUnSplashClient.Create;
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
                    Logger.Add('Unhandled Exception # 544');
                    Logger.Add('Class : ' + E.ClassName);
                    Logger.Add('Error : ' + E.Message);
                    Logger.Add('Vars : I = ' + i.ToString + ', OutFile = ' + outfile + ', ImageCount = ' + ImageCount.ToString);
                    end);
              end;

          end;
          TThread.Synchronize(nil,
            procedure
            begin
              if Assigned(Logger) then
                begin
                  Logger.Add('Downloaded (' + I.ToString + ') ' + outfile + ' at ' + SW.ElapsedMilliseconds.ToString);
                end;
            end
          );
          FreeAndNil(Downer);
				end
			, tp);
			TThread.Synchronize(nil,
				procedure
				begin
					Logger.Add('Finished ' + sw.ElapsedMilliseconds.ToString);
          tp.Free;
          AllDone;
				end
			);
		end
	).Start;
end;

procedure TfrmInit.SingleThreadDownload(const ImageCount: Integer; AFileList: TJSONFileArray;
  const ABaseURL: String; const ADestPath: String; const FullSize: Integer;
  Logger: TStrings = Nil; Progress: TProgressBar = Nil);
var
  sw: TStopWatch;
  Downer: TUnSplashClient;
  TotalImageCount: Integer;
  I, TotalDone: Integer;
begin
  Logger.Add('Downloader');
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

      Downer := TUnSplashClient.Create;
      var infilerec := AFileList[I];
      var DownSize := AFileList[I].Size;
      var outfile := TPath.Combine(ADestPath, UnixToDos(infilerec.Name));
      try
        Logger.Add('Downloading ' + infilerec.Name);
        Memo1.GoToTextEnd;
        Memo1.Repaint;
        Downer.Download(ABaseURL, ADestPath, infilerec, I, Progress);
        Application.ProcessMessages;
      except
        on E: Exception do
          begin
            Logger.Add('Unhandled Exception # 416');
            Logger.Add('Class : ' + E.ClassName);
            Logger.Add('Error : ' + E.Message);
            Logger.Add('Vars : I = ' + i.ToString + ', OutFile = ' + outfile + ', ImageCount = ' + ImageCount.ToString);
            Memo1.GoToTextEnd;
            Memo1.Repaint;
          end;

      end;
      if Assigned(Logger) then
        begin
          Logger.Add('Downloaded ' + infilerec.Name + ' at ' + SW.ElapsedMilliseconds.ToString);
          Memo1.GoToTextEnd;
          Memo1.Repaint;
        end;
      FreeAndNil(Downer);
    end;

  if Assigned(Logger) then
      Logger.Add('Finished ' + sw.ElapsedMilliseconds.ToString);
  if Assigned(Progress) then
      Progress.Value := 0;
  AllDone;
end;

procedure TfrmInit.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  Button2Click(Self);
end;

end.
