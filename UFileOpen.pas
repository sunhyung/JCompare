unit UFileOpen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmFileOpen = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    eFile1: TEdit;
    eFile2: TEdit;
    btnOpen1: TButton;
    btnOpen2: TButton;
    OpenDialog1: TOpenDialog;
    cboxIgnoreCase: TCheckBox;
    cboxIgnoreWhiteSpace: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnOpen1Click(Sender: TObject);
    procedure btnOpen2Click(Sender: TObject);
  private
    { Private declarations }
    FCanClose : Boolean;
  public
    { Public declarations }
    function GetFileName1 : string;
    function GetFileName2 : string;
    function IsIgnoreCase : Boolean;
    function IsIgnoreWhiteSpace : Boolean;
  end;

var
  frmFileOpen: TfrmFileOpen;

implementation

{$R *.dfm}

procedure TfrmFileOpen.FormCreate(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmFileOpen.btnCancelClick(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmFileOpen.btnOkClick(Sender: TObject);
begin
  if eFile1.Text = '' then
  begin
    FCanClose := False;
    MessageBox (Handle, 'Input file path', 'JCompare', MB_ICONINFORMATION or MB_OK);
    eFile1.SetFocus;
    exit;
  end;

  if eFile2.Text = '' then
  begin
    FCanClose := False;
    MessageBox (Handle, 'Input file path', 'JCompare', MB_ICONINFORMATION or MB_OK);
    eFile2.SetFocus;
    exit;
  end;

  if not FileExists (eFile1.Text) then
  begin
    FCanClose := False;
    MessageBox (Handle, PChar(eFile1.Text + ' doesn''t exist.'), 'JCompare', MB_ICONINFORMATION or MB_OK);
    eFile1.SetFocus;
    exit;
  end;

  if not FileExists (eFile2.Text) then
  begin
    FCanClose := False;
    MessageBox (Handle, PChar(eFile2.Text + ' doesn''t exist.'), 'JCompare', MB_ICONINFORMATION or MB_OK);
    eFile2.SetFocus;
    exit;
  end;

  FCanClose := True;
end;

procedure TfrmFileOpen.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

function TfrmFileOpen.GetFileName1: string;
begin
  Result := eFile1.Text;
end;

function TfrmFileOpen.GetFileName2: string;
begin
  Result := eFile2.Text;
end;

function TfrmFileOpen.IsIgnoreCase: Boolean;
begin
  Result := cboxIgnoreCase.Checked;
end;

function TfrmFileOpen.IsIgnoreWhiteSpace: Boolean;
begin
  Result := cboxIgnoreWhiteSpace.Checked;
end;

procedure TfrmFileOpen.btnOpen1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    eFile1.Text := OpenDialog1.FileName;
  end;
end;

procedure TfrmFileOpen.btnOpen2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    eFile2.Text := OpenDialog1.FileName;
  end;
end;

end.
