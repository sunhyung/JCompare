unit UFindText;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, StrUtils;

type
  TfrmFindText = class(TForm)
    Label1: TLabel;
    eFindKeyword: TEdit;
    btnFind: TButton;
    cboxCaseSensitive: TCheckBox;
    cboxWholeWord: TCheckBox;
    btnClose: TButton;
    rgFindDir: TRadioGroup;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    FCanClose : Boolean;
  public
    { Public declarations }
    function GetFindKeyword : string;
    procedure SetFindKeyword (value : string);
    function GetFindOptions : TStringSearchOptions;
    procedure SetFindOptions (options : TStringSearchOptions);
  end;

var
  frmFindText: TfrmFindText;

implementation

{$R *.dfm}

{ TfrmFindText }

function TfrmFindText.GetFindKeyword: string;
begin
  Result := eFindKeyword.Text;
end;

function TfrmFindText.GetFindOptions: TStringSearchOptions;
begin
  Result := [];
  if cboxCaseSensitive.Checked then
  begin
    Result := Result + [soMatchCase];
  end;

  if cboxWholeWord.Checked then
  begin
    Result := Result + [soWholeWord];
  end;

  if rgFindDir.ItemIndex = 1 then
  begin
    Result := Result + [soDown];
  end;
end;

procedure TfrmFindText.SetFindKeyword(value: string);
begin
  eFindKeyword.Text := value;
end;

procedure TfrmFindText.SetFindOptions (options : TStringSearchOptions);
begin
  if soMatchCase in options then
  begin
    cboxCaseSensitive.Checked := True;
  end;

  if soWholeWord in options then
  begin
    cboxWholeWord.Checked := True;
  end;

  if soDown in options then
  begin
    rgFindDir.ItemIndex := 1;
  end
  else
  begin
    rgFindDir.ItemIndex := 0;
  end;
end;

procedure TfrmFindText.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN : btnFind.Click;
    VK_ESCAPE : btnClose.Click;
  end;
end;

procedure TfrmFindText.FormCreate(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmFindText.btnFindClick(Sender: TObject);
begin
  FCanClose := (eFindKeyword.Text <> '');
  if not FCanClose then
  begin
    MessageBox (Handle, 'Input a keyword', 'Find text', MB_ICONINFORMATION or MB_OK);
    eFindKeyword.SetFocus;
  end;
end;

procedure TfrmFindText.btnCloseClick(Sender: TObject);
begin
  FCanClose := True;
end;

procedure TfrmFindText.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := FCanClose;
end;

end.
