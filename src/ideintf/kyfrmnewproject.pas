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
    rgType: TRadioGroup;
    ScrollBox1: TScrollBox;
    ValueListEditor1: TValueListEditor;
    procedure btnNextWizardClick(Sender: TObject);
    procedure edProjectNameChange(Sender: TObject);
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
  if DirectoryExists(IncludeTrailingBackslash(edProjectPath.Text)) and
    (edProjectName.Text <> '') then
  begin
    CreatePrj;
    Close;
  end;
end;

procedure TfrmNewProject.edProjectNameChange(Sender: TObject);
begin
  if edProjectPath.Text <> '' then
    edProjectPathChange(Sender);
end;

procedure TfrmNewProject.edProjectPathChange(Sender: TObject);
var
  AppName: string;
begin
  AppName := StringReplace(edProjectName.Text, ' ', '_', [rfReplaceAll]).ToLower;
  edWebRootPath.Text := IncludeTrailingBackslash(edProjectPath.Text) + AppName +
    PathDelim + 'public';
end;

procedure TfrmNewProject.PreparePrj(TargetPath: string);
var
  SL: TStringList;
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

  SL := TStringList.Create;
  SL.LoadFromFile(TargetPath + PathDelim + 'control.lpr');
  if rgType.ItemIndex = 0 then
    SL.Text := StringReplace(SL.Text, 'Kyoukai.Base.CGIApplication,',
      '{Kyoukai.Base.CGIApplication,}', [rfReplaceAll])
  else
    SL.Text := StringReplace(SL.Text, 'Kyoukai.Base.HTTPApplication,',
      '{Kyoukai.Base.HTTPApplication,}', [rfReplaceAll]);
  SL.SaveToFile(TargetPath + PathDelim + 'control.lpr');
  SL.Free;
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

  if
    DirectoryExists(
    IncludeTrailingBackslash(edProjectPath.Text) + PathDelim + AppName) then
  begin
    ShowMessage('Folder already exists');
    Abort;
  end;



  CopyDirTree(KyoukaiPath + 'projecttemplates' + PathDelim + 'simple_template',
    IncludeTrailingBackslash(edProjectPath.Text) + PathDelim + AppName, [cffCreateDestDirectory]
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

