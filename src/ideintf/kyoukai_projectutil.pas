unit kyoukai_projectutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, LazIDEIntf, ProjectIntf, Controls, Forms, PackageIntf,
  MenuIntf, KyfrmNewProject;
type
  { TKyoukaiWebAppDescriptor }

  TKyoukaiWebAppDescriptor = class(TProjectDescriptor)
  private
    IsCreateProject: Boolean;
  public
    constructor Create; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function InitProject(AProject: TLazProject): TModalResult; override;
    function CreateStartFiles({%H-}AProject: TLazProject): TModalResult; override;
  end;

{ TFileControllerDesc }

  TFileControllerDesc = class(TFileDescPascalUnit)
  public
    constructor Create; override;
    function GetInterfaceUsesSection: string; override;
    function GetInterfaceSource(const aFilename, aSourceName, aResourceName: string
      ): string; override;
    function GetLocalizedName: string; override;
    function GetLocalizedDescription: string; override;
    function GetUnitDirectives: string; override;
    function GetImplementationSource(const Filename, SourceName,
                                     ResourceName: string): string; override;
  end;


procedure Register;

implementation

procedure StartMyTool;
begin
  ShowMessage('halo');
end;


procedure Register;
begin
  //RegisterProjectFileDescriptor(TFileControllerDesc.Create,
  //                              FileDescGroupName);
  //RegisterProjectDescriptor(TKyoukaiWebAppDescriptor.Create);
  RegisterIDEMenuCommand(mnuProject, 'newkyoukaifile', 'New Kyoukai Project',
   @frmNewProject.StartDlg);

end;

function FileDescriptorUnit() : TProjectFileDescriptor;
begin
  Result:= ProjectFileDescriptors.FindByName('Kyoukai Web App Controller');
end;

{ TFileDescPascalUnitWithlpForm }

constructor TFileControllerDesc.Create;
begin
  inherited Create;
  Name:='Kyoukai Web App Controller';
  UseCreateFormStatements := False;
end;

function TFileControllerDesc.GetInterfaceUsesSection: string;
begin
  Result :=
    'Classes, SysUtils,'+LineEnding+
    '  Kyoukai.Standard.WebView,'+LineEnding+
    '  Kyoukai.Standard.WebRouter,'+LineEnding+
    '  Kyoukai.Standard.Controller';
end;

function TFileControllerDesc.GetLocalizedName: string;
begin
  Result:='Kyoukai Web App Controller';
end;

function TFileControllerDesc.GetLocalizedDescription: string;
begin
  Result:='Create a new blank Kyoukai Web App Controller';
end;

function TFileControllerDesc.GetUnitDirectives: string;
begin
  result := inherited GetUnitDirectives();
end;

function TFileControllerDesc.GetInterfaceSource(const aFilename, aSourceName, aResourceName: string
      ): string;
begin
  Result :=
    'type'+LineEnding+
    '  T'+aSourceName+' = class()'+LineEnding+
    '  end;'+LineEnding+

    LineEnding;
end;

function TFileControllerDesc.GetImplementationSource(const Filename,
  SourceName, ResourceName: string): string;
begin
  //Result:='{$R *.dfm}'+LineEnding+LineEnding;
end;

{ TProjectApplicationDescriptor }

constructor TKyoukaiWebAppDescriptor.Create;
begin
  inherited;
  Name := 'A Kyoukai Web App';
end;

function TKyoukaiWebAppDescriptor.CreateStartFiles(
  AProject: TLazProject): TModalResult;
begin
  if IsCreateProject then
    Result:=LazarusIDE.DoNewEditorFile(FileDescriptorUnit, 'MainController.pas',
      'MainController', [nfIsPartOfProject,nfOpenInEditor,nfCreateDefaultSrc])
  else
    Result := mrCancel;
end;

function TKyoukaiWebAppDescriptor.GetLocalizedDescription: string;
begin
  Result := 'Kyoukai Web Application'+LineEnding+LineEnding
           +'Web Service application written in Free Pascal.'+LineEnding
           +'This files will be automatically maintained by Lazarus.';
end;

function TKyoukaiWebAppDescriptor.GetLocalizedName: string;
begin
  Result := 'Kyoukai Web App';
end;

function TKyoukaiWebAppDescriptor.InitProject(
  AProject: TLazProject): TModalResult;
var
  NewSource: String;
  KyoukaiPath: String;
  Pkg: TIDEPackage;
  MainFile: TLazProjectFile;
  PrjDlg: TfrmNewProject;
begin
  Result:=inherited InitProject(AProject);
  Pkg := PackageEditingInterface.FindPackageWithName('kyoukai_ideintf');
  KyoukaiPath := Pkg.DirectoryExpanded;

  //MainFile:=AProject.CreateProjectFile('project.lpr');
  //MainFile.IsPartOfProject:=true;
  //AProject.AddFile(MainFile,false);
  //AProject.MainFileID:=0;

  PrjDlg := TfrmNewProject.Create(nil);
  with PrjDlg do
  begin
    if ShowModal <> mrOK then
    begin
      AProject.Title := edProjectName.Text;
      //AProject.MainFile. := edProjectPath.Text + AProject.Title.Replace(' ', '',
      //  [rfReplaceAll]);
    end
    else
    begin
      IsCreateProject := False;
    end;
  end;
  FreeAndNil(PrjDlg);


  //// create program source
  //NewSource:='program Project;'+LineEnding+
  //  LineEnding+
  //  '{$mode objfpc}{$H+}'+LineEnding+
  //  LineEnding+
  //  'uses'+LineEnding+
  //  '  {$IFNDEF WINDOWS}'+LineEnding+
  //  '  cthreads,'+LineEnding+
  //  '  {$ENDIF}'+LineEnding+
  //  '  Classes, SysUtils,'+LineEnding+
  //  '  // New CGI Support'+LineEnding+
  //  '  Kyoukai.Standard.CGIApplication,'+LineEnding+
  //  '  Kyoukai.Standard.WebRouter,'+LineEnding+
  //  '  {You must place your module units here or Kyoukai can''t register anything!}'+LineEnding+
  //  '  MainController'+LineEnding+
  //  '  { you can add units after this };'+LineEnding+
  //  LineEnding+
  //  LineEnding+
  //  'begin'+LineEnding+
  //  '  KyoukaiApp.Run;'+LineEnding+
  //  'end.'+LineEnding;
  //AProject.MainFile.SetSourceText(NewSource,true);
  //
  //// add lcl pp/pas dirs to source search path
  //AProject.AddPackageDependency('FCL');
  //AProject.AddPackageDependency('kyoukai_standard');
  //
  //AProject.LazCompilerOptions.Win32GraphicApp:=false;
  //AProject.LazCompilerOptions.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  //AProject.LazCompilerOptions.TargetFilename:='project';


  LazarusIDE.DoOpenProjectFile(
    KyoukaiPath+
    'projecttemplates\cgi_template\website_template.lpi',
    [ofProjectLoading]);

  //if IsCreateProject then
  //  Result := mrOK
  //else
    Result := mrAbort;

end;

end.
