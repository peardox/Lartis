unit ChoiceForm;

interface

uses
  System.SysUtils, System.Types, System.IOUtils, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  EmbeddedForm, FMX.StdCtrls, FMX.Layouts;

type
  TSelectImage = class(TImage)
  public
    TargetForm: TEmbeddedForm;
  end;

  TImageArray = Array of TSelectImage;

  TfrmChoice = class(TEmbeddedForm)
    Text1: TText;
    Layout1: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure Layout1Resize(Sender: TObject);
  private
    { Private declarations }
    GridCols: Integer;
    GridRows: Integer;
    SectionImages: TImageArray;
    procedure ImageClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmChoice: TfrmChoice;

const
  Sections: Array [0..3] of String = ('style.png', 'train.png', 'evolve.png', 'movie.png');


implementation

{$R *.fmx}

uses
  Math,
  EvolveForm,
  MovieForm,
  StyleForm,
  TrainForm
  ;


procedure TfrmChoice.FormCreate(Sender: TObject);
var
  BaseDir: String;
  ImgDir: String;
  I: Integer;
begin
  Text1.Visible := False;

  GridCols := 2;
  GridRows := 2;

  SetLength(SectionImages, Length(Sections));

  {$IFDEF ANDROID}
  ImgDir := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetDocumentsPath);
  {$ELSE}
  ImgDir := '';
  {$ENDIF}
  {$IFNDEF ANDROID}
  {$IFDEF MACOS}
  BaseDir := IncludeTrailingPathDelimiter(System.IOUtils.TPath.GetFullPath('../Resources/'));
  {$ELSE}
  BaseDir := '';
  {$ENDIF}

  if DirectoryExists(BaseDir + 'images') then
      ImgDir := BaseDir + 'images'
  else if DirectoryExists(BaseDir + '../../images') then
      ImgDir := BaseDir + '../../images'
  else
    ShowMessage('Can''t find images - ' + BaseDir + sLineBreak + ImgDir);

  if DirectoryExists(ImgDir + '/licensed') then
      ImgDir := ImgDir + '/licensed'
  else if DirectoryExists(ImgDir + '/free') then
      ImgDir := ImgDir + '/free'
  else
    ShowMessage('Can''t find image assets');
  ImgDir := IncludeTrailingPathDelimiter(ImgDir);

  for I := 0 to Length(Sections) - 1 do
    begin
      if not FileExists(ImgDir + Sections[I]) then
        begin
          ShowMessage('Missing image ' + ImgDir + Sections[I]);
          Application.Terminate;
        end;
    end;
  {$ENDIF}

  for I := 0 to Length(Sections) - 1 do
    begin
      if FileExists(ImgDir + Sections[I]) then
        begin
          SectionImages[I] := TSelectImage.Create(Self);
          SectionImages[I].Bitmap.LoadFromFile(ImgDir + Sections[I]);
          SectionImages[I].Parent := Layout1;
          case I of
            0: begin
              SectionImages[I].TargetForm := frmStyle;
            end;
            1: begin
              SectionImages[I].TargetForm := frmTrain;
            end;
            2: begin
              SectionImages[I].TargetForm := frmEvolve;
            end;
            3: begin
              SectionImages[I].TargetForm := frmMovie;
            end;
          end;
          SectionImages[I].OnClick := ImageClick;
        end;
    end;

end;

procedure TfrmChoice.Layout1Resize(Sender: TObject);
var
  AvailableHeight: Single;
  AvailableWidth: Single;
  CellSize: Single;
  CellMargin: Integer;
  I, X, Y: Integer;
  GridTop, GridLeft: Single;
  TxtDebug: String;
begin
  if (GridRows > 0) and (GridCols > 0) then
    begin
      AvailableHeight := Layout1.Height / GridRows;
      if AvailableHeight > SectionImages[0].Bitmap.Height then
        AvailableHeight := SectionImages[0].Bitmap.Height;

      AvailableWidth := Layout1.Width / GridCols;
      if AvailableWidth > SectionImages[0].Bitmap.Width then
        AvailableWidth := SectionImages[0].Bitmap.Width;

      CellSize := Min(AvailableHeight, AvailableWidth);
      var OCS: Single := CellSize;

      GridLeft := (Layout1.Width - (CellSize * GridCols)) / 2;
      GridTop := (Layout1.Height - (CellSize * GridRows)) / 2;
      TxtDebug := '';

      if(CellSize > 16) then
        begin
          CellMargin := floor(CellSize / 32);
          CellSize := CellSize - (CellMargin * 2);

          for I := 0 to Length(SectionImages) - 1 do
            begin
              X := I mod GridCols;
              Y := I div GridCols;


              SectionImages[I].Width := CellSize;
              SectionImages[I].Height := CellSize;

              SectionImages[I].Position.X := (X * CellSize) + (CellMargin * ((X * 2) + 1)) + GridLeft;
              SectionImages[I].Position.Y := (Y * CellSize) + (CellMargin * ((Y * 2) + 1)) + GridTop;

              TxtDebug := TxtDebug + IntToStr(I) + ' / ' + IntToStr(X) + ' / '  + IntToStr(Y) + sLineBreak;
              TxtDebug := TxtDebug + FloatToStr(SectionImages[I].Position.X) + ' / ' + FloatToStr(SectionImages[I].Position.Y) + sLineBreak;

            end;

          TxtDebug := TxtDebug + 'Cellsize / CellMargin = ' + FloatToStr(CellSize) + ' / ' + IntToStr(CellMargin) + ' (' + FloatToStr(OCS) + ')' + sLineBreak +
                        'Layout = ' + FloatToStr(Layout1.Width) + ' x ' + FloatToStr(Layout1.Height) + sLineBreak +
                        'Position = ' + FloatToStr(GridLeft) + ' x ' + FloatToStr(GridTop) + ' + SectionImages = ' + IntToStr(Length(SectionImages));
          Text1.Text := TxtDebug;
          Text1.BringToFront;
        end;
    end;
end;

procedure TfrmChoice.ImageClick(Sender: TObject);
begin
  if Assigned(SwitchToOther) then
    begin
      if Sender Is TSelectImage then
        SwitchToOther(TSelectImage(Sender).TargetForm);
    end;
end;

end.
