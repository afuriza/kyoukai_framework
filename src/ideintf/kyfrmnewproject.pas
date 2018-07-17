unit KyfrmNewProject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ComCtrls, ExtCtrls, StdCtrls, EditBtn;

type

  { TfrmNewProject }

  TfrmNewProject = class(TForm)
    btnPrevWizard: TButton;
    btnNextWizard: TButton;
    btnPrevWizard1: TButton;
    btnPrevWizard2: TButton;
    CheckBox1: TCheckBox;
    DirectoryEdit1: TDirectoryEdit;
    imgHeaderLogo: TImage;
    lblVersion: TLabel;
    lblDialogName: TLabel;
    Label2: TLabel;
    LabeledEdit1: TLabeledEdit;
    nbStepWizard: TNotebook;
    SecondStep: TPage;
    pgFirstStep: TPage;
    RadioGroup1: TRadioGroup;
    ScrollBox1: TScrollBox;
    ValueListEditor1: TValueListEditor;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmNewProject: TfrmNewProject;

implementation

{$R *.lfm}

{ TfrmNewProject }

procedure TfrmNewProject.FormCreate(Sender: TObject);
begin

end;

end.

