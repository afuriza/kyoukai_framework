unit KyfrmNewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ComCtrls, ExtCtrls, StdCtrls, EditBtn, PackageIntf, LazIDEIntf, ProjectIntf;

type

  { TfrmNewProject }

  TfrmNewProject = class(TForm)
    btnPrevWizard: TButton;
    btnNextWizard: TButton;
    btnPrevWizard1: TButton;
    btnPrevWizard2: TButton;
    CheckBox1: TCheckBox;
    edProjectPath: TDirectoryEdit;
    edWebRootPath: TDirectoryEdit;
    edOutputFile: TLabeledEdit;
    imgHeaderLogo: TImage;
    Label3: TLabel;
    lblVersion: TLabel;
    lblDialogName: TLabel;
    Label2: TLabel;
    edProjectName: TLabeledEdit;
    nbStepWizard: TNotebook;
    SecondStep: TPage;
    pgFirstStep: TPage;
    RadioGroup1: TRadioGroup;
    ScrollBox1: TScrollBox;
    ValueListEditor1: TValueListEditor;
    procedure btnNextWizardClick(Sender: TObject);
    procedure edProjectPathChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure CreatePrj;
    procedure PreparePrj(TargetPath: string);
  public
    procedure StartDlg(Sender: TObject);
  end;

var
  frmNewProject: TfrmNewProject;

implementation

{$R *.lfm}

{ TfrmNewProject }

procedure TfrmNewProject.FormCreate(Sender: TObject);
begin

end;

procedure TfrmNewProject.btnNextWizardClick(Sender: TObject);
begin
  CreatePrj;
  Close;
end;

procedure TfrmNewProject.edProjectPathChange(Sender: TObject);
var
  AppName: string;
begin
  AppName := StringReplace(edProjectName.Text, ' ', '_', [rfReplaceAll]).ToLower;
  edWebRootPath.Text := edProjectPath.Text + AppName +
    PathDelim + 'public';
end;

procedure TfrmNewProject.PreparePrj(TargetPath: string);
begin
  RenameFile(TargetPath + PathDelim + 'control.lpi.kptemplate',
    TargetPath + PathDelim + 'control.lpi');
  RenameFile(TargetPath + PathDelim + 'control.lps.kptemplate',
    TargetPath + PathDelim + 'control.lps');
  RenameFile(TargetPath + PathDelim + 'control.lpr.kptemplate',
    TargetPath + PathDelim + 'control.lpr');
  RenameFile(TargetPath + PathDelim + 'controllers' + PathDelim +
    'maincontroller.pas.kptemplate',
    TargetPath + PathDelim + 'controllers' + PathDelim +
    'maincontroller.pas');
end;

procedure TfrmNewProject.CreatePrj;
var
  KyoukaiPath: String;
  Pkg: TIDEPackage;
  AppName: string;
begin
  Pkg := PackageEditingInterface.FindPackageWithName('kyoukai_ideintf');
  KyoukaiPath := Pkg.DirectoryExpanded;
  AppName := StringReplace(edProjectName.Text, ' ', '_', [rfReplaceAll]).ToLower;


  CopyDirTree(KyoukaiPath + 'projecttemplates' + PathDelim + 'simple_template',
    edProjectPath.Text + PathDelim + AppName, [cffCreateDestDirectory]
  );

  PreparePrj(edProjectPath.Text + PathDelim + AppName);

  LazarusIDE.DoOpenProjectFile(edProjectPath.Text + PathDelim + AppName + PathDelim +
    'control.lpi', [ofProjectLoading]);

end;

procedure TfrmNewProject.StartDlg(Sender: TObject);
begin
  Show;
end;

initialization
  frmNewProject := TfrmNewProject.Create(nil);

finalization
  FreeAndNil(frmNewProject);

end.

