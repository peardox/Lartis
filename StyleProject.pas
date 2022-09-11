unit StyleProject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  Shaders;

type
  TLayerArray = Array of TBaseShader;

  TStyleProject = class(TComponent)
  private
    FLayers: TLayerArray;
    function GetLayer(Index: Integer): TBaseShader;
    procedure SetLayer(Index: Integer; Value: TBaseShader);
  public
    property Layers[Index: Integer]: TBaseShader read GetLayer write SetLayer;
  end;

  TStyleProjects = Array of TStyleProject;

implementation

function TStyleProject.GetLayer(Index: Integer): TBaseShader;
begin
  if (Index > 0 ) and (Index < Length(FLayers)) then
    begin
      Result := FLayers[Index];
    end;
end;

procedure TStyleProject.SetLayer(Index: Integer; Value: TBaseShader);
begin

end;

end.
