unit UProcessDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmProcess = class(TForm)
    Label1: TLabel;
    btnCancel: TButton;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FCancelProcess : Boolean;
  public
    { Public declarations }
  published
    property CancelProcess : Boolean read FCancelProcess;
  end;

var
  frmProcess: TfrmProcess;

implementation

{$R *.dfm}

procedure TfrmProcess.FormCreate(Sender: TObject);
begin
  FCancelProcess := False;
end;

procedure TfrmProcess.btnCancelClick(Sender: TObject);
begin
  if MessageBox (Handle, 'Do you want to abort the job?', 'JCompare',
    MB_YESNO or MB_ICONQUESTION) = IDYES then
  begin
    FCancelProcess := True;
  end;
end;

end.
