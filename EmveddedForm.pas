unit EmveddedForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Layouts, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Objects;

type
  TEmbeddedForm = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    ParentLayout: TControl;
    procedure HideForm(Sender: TObject);
    procedure ShowForm(Sender: TObject);
  end;

implementation

procedure TEmbeddedForm.HideForm(Sender: TObject);
begin
  if Assigned(ParentLayout) then
    ParentLayout.Visible := False;
end;

procedure TEmbeddedForm.ShowForm(Sender: TObject);
begin
  if Assigned(ParentLayout) then
    ParentLayout.Visible := True;
end;

end.
