{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit kyoukai_ideintf;

{$warn 5023 off : no warning about unused units}
interface

uses
  KyfrmNewProject, kyoukai_projectutil, kyoukai_projectconsts, 
  kyoukai_dirstructures, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('kyoukai_projectutil', @kyoukai_projectutil.Register);
end;

initialization
  RegisterPackage('kyoukai_ideintf', @Register);
end.
