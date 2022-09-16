unit Downloader;

interface
uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo.Types,
  LartisTypes,
  System.IOUtils, System.Net.URLClient, System.Net.HttpClient,
  System.Threading, System.Net.HttpClientComponent,
  System.Diagnostics, FMX.ScrollBox, FMX.Memo;

type
  TUnSplashClient = class(TComponent)
  private
    FDownloadIndex: Integer;
    FDownloadStream: TStream;
    FClient: TNetHTTPClient;
    FProgress: TProgressBar;
    FBytesSoFar: Int64;
    FAbortFlag: Boolean;
    procedure ReceiveData(const Sender: TObject; AContentLength,
      AReadCount: Int64; var AAbort: Boolean);
    procedure NeedClientCertificate(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificateList: TCertificateList;
      var AnIndex: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Download(const ABaseURL: String; const Outpath: String; const AFile: TJSONFile; const ADownloadIndex: Integer; Progress: TProgressBar); overload;
  end;

function UnixToDos(const Filename: String): String; Inline;

implementation

constructor TUnSplashClient.Create(AOwner: TComponent);
begin
  inherited;
  FClient := TNetHTTPClient.Create(Self);
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
      AAbort := FAbortFlag;
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


end.
