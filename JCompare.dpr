program JCompare;

uses
  Forms,
  Controls,
  UMain in 'UMain.pas' {frmMain},
  UFileCompare in 'UFileCompare.pas',
  UCRC32 in 'UCRC32.pas',
  UFindText in 'UFindText.pas' {frmFindText},
  UFileOpen in 'UFileOpen.pas' {frmFileOpen},
  UTextFile in 'UTextFile.pas',
  DiffEdit in 'EditControl\DiffEdit.pas',
  UProcessDlg in 'UProcessDlg.pas' {frmProcess};

{$R *.res}
var
  fileName1, fileName2 : string;
  isIgnoreCase, isIgnoreWhiteSpace : Boolean;
begin
  Application.Initialize;

  frmFileOpen := TfrmFileOpen.Create(Application);
  if frmFileOpen.ShowModal = mrCancel then
  begin
    exit;
  end;
  fileName1 := frmFileOpen.GetFileName1;
  fileName2 := frmFileOpen.GetFileName2;
  isIgnoreCase := frmFileOpen.IsIgnoreCase;
  isIgnoreWhiteSpace := frmFileOpen.IsIgnoreWhiteSpace;
  frmFileOpen.Free;

  Application.Title := 'JCompare';
  Application.CreateForm(TfrmMain, frmMain);
  frmMain.StartCompareFile(fileName1, fileName2, isIgnoreCase, isIgnoreWhiteSpace);

  Application.Run;
end.
