unit LartisPackage;

interface

uses
  System.Classes, PyPackage, PyPackage.Model, PythonEngine;

type
  [ComponentPlatforms(pidAllPlatforms)]
  TAnyPackage = class(TPyManagedPackage)
  private
    function AsVariant: variant;
  protected
    procedure Prepare(const AModel: TPyPackageModel); override;
    procedure ImportModule; override;
  public
    ModuleName: String;
    constructor Create(AOwner: TComponent); override;
    constructor CreatePackage(AOwner: TComponent; const AModuleName: String);
    property np: variant read AsVariant;
  end;

implementation
{
  pio := TAnyPackage.CreatePackage(Self, 'torchvision');
  pio.BeforeInstall := PackageConfigureInstall;
  pio.PyEnvironment := PyEnv;
  pio.PythonEngine := PyEng;
  pio.Install();
}

uses
  System.Variants,
  PyPackage.Manager.ManagerKind,
  PyPackage.Manager.Pip;

{ TAnyPackage }

constructor TAnyPackage.Create(AOwner: TComponent);
begin
  inherited;
  AutoImport := False;
  AutoInstall := False;
end;

constructor TAnyPackage.CreatePackage(AOwner: TComponent; const AModuleName: String);
begin
  ModuleName := AModuleName;
  inherited Create(AOwner);
end;

function TAnyPackage.AsVariant: variant;
begin
  Result := inherited;
end;

procedure TAnyPackage.ImportModule;
begin
  MaskFPUExceptions(true);
  try
    inherited;
  finally
    MaskFPUExceptions(false);
  end;
end;

procedure TAnyPackage.Prepare(const AModel: TPyPackageModel);
begin
  inherited;
  with AModel do begin
    PackageManagers.Add(
      TPyPackageManagerKind.pip,
      TPyPackageManagerPip.Create(ModuleName)
      );
  end;
end;

end.
