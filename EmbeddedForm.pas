unit EmbeddedForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects;

type
  TEmbeddedForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    ParentLayout: TControl;
    CloseMyself: TNotifyEvent;
    SwitchToOther: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    procedure HideForm;
    procedure ShowForm;
    procedure Paint;
  end;

implementation

constructor TEmbeddedForm.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TEmbeddedForm.Paint;
begin
  if Assigned(OnPaint) then
    DoPaint(Canvas, TRect.Create(0, 0, Width, Height));
end;

procedure TEmbeddedForm.HideForm;
begin
  if Assigned(ParentLayout) then
    if ParentLayout.Visible then
      ParentLayout.Visible := False;
end;

procedure TEmbeddedForm.ShowForm;
begin
  if Assigned(ParentLayout) then
    if not ParentLayout.Visible then
      ParentLayout.Visible := True;
end;

end.
