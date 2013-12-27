unit UMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, DiffEdit, XPMan, ExtCtrls, Clipbrd, ToolWin,
  Math, ImgList, Menus, StrUtils;

type
  TfrmMain = class(TForm)
    ToolBar1: TToolBar;
    tbtnCopyToClipbord: TToolButton;
    tbtnFind: TToolButton;
    tbtnFindNext: TToolButton;
    ToolButton4: TToolButton;
    tbtnFindPrev: TToolButton;
    tbtnAddBookmark: TToolButton;
    tbtnNextBookmark: TToolButton;
    tbtnPrevBookmark: TToolButton;
    ToolButton9: TToolButton;
    tbtnDelBookmark: TToolButton;
    tbtnPrevDiff: TToolButton;
    XPManifest1: TXPManifest;
    tbtnNextDiff: TToolButton;
    StatusBar1: TStatusBar;
    ilToolbar: TImageList;
    ToolButton1: TToolButton;
    pmEdit: TPopupMenu;
    NCopyToClipboard: TMenuItem;
    DiffText1: TDiffText;
    pmMain: TPopupMenu;
    pmiFind: TMenuItem;
    pmiFindNext: TMenuItem;
    pmiCopyToClipboard: TMenuItem;
    pmiFindPrev: TMenuItem;
    pmiAddBookmark: TMenuItem;
    pmiNextBookmark: TMenuItem;
    pmiPrevBookmark: TMenuItem;
    pmiDelBookmark: TMenuItem;
    pmiPrevDiff: TMenuItem;
    pmiNextDiff: TMenuItem;
    procedure tbtnCopyToClipbordClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar;
      Panel: TStatusPanel; const Rect: TRect);
    procedure tbtnAddBookmarkClick(Sender: TObject);
    procedure tbtnDelBookmarkClick(Sender: TObject);
    procedure tbtnNextBookmarkClick(Sender: TObject);
    procedure tbtnPrevBookmarkClick(Sender: TObject);
    procedure DiffText1CaretMove(Sender: TObject; lineNum, colNum: Integer;
      availPrevDiff, availNextDiff: Boolean);
    procedure tbtnPrevDiffClick(Sender: TObject);
    procedure tbtnNextDiffClick(Sender: TObject);
    procedure pmEditPopup(Sender: TObject);
    procedure NCopyToClipboardClick(Sender: TObject);
    procedure DiffText1PaneChanged(Sender: TObject; lineNum,
      colNum: Integer);
    procedure tbtnFindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbtnFindNextClick(Sender: TObject);
    procedure tbtnFindPrevClick(Sender: TObject);
    procedure DiffText1TextSelected(Sender: TObject; isSelected: Boolean);
    procedure pmiCopyToClipboardClick(Sender: TObject);
    procedure pmiFindClick(Sender: TObject);
    procedure pmiFindNextClick(Sender: TObject);
    procedure pmiFindPrevClick(Sender: TObject);
    procedure pmiAddBookmarkClick(Sender: TObject);
    procedure pmiNextBookmarkClick(Sender: TObject);
    procedure pmiPrevBookmarkClick(Sender: TObject);
    procedure pmiDelBookmarkClick(Sender: TObject);
    procedure pmiPrevDiffClick(Sender: TObject);
    procedure pmiNextDiffClick(Sender: TObject);
  private
    { Private declarations }
    FFindOptions : TStringSearchOptions;
    FFindKeyword : string;
  public
    { Public declarations }
    procedure StartCompareFile (fileName1, fileName2 : string;
      isIgnoreCase, isIgnoreWhiteSpace : Boolean);
  end;

var
  frmMain: TfrmMain;

implementation

uses UFileCompare, UFindText, UTextFile, UProcessDlg;

{$R *.dfm}

procedure TfrmMain.tbtnCopyToClipbordClick(Sender: TObject);
begin
  DiffText1.CopyToClipboard;
end;

procedure TfrmMain.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  rt : TRect;
  defFontColor : TColor;
begin
  rt := Rect;
  defFontColor := StatusBar1.Canvas.Font.Color;
  case Panel.Index of
    0 :
    begin
      StatusBar1.Canvas.Font.Color := clRed;
      StatusBar1.Canvas.TextOut (Rect.Left, Rect.Top, 'Deleted Text');
    end;

    1 :
    begin
      StatusBar1.Canvas.Font.Color := clBlue;
      StatusBar1.Canvas.TextOut (Rect.Left, Rect.Top, 'Changed Text');
    end;

    2 :
    begin
      StatusBar1.Canvas.Font.Color := clGreen;
      StatusBar1.Canvas.TextOut (Rect.Left, Rect.Top, 'Inserted Text');
    end;
  end;
  StatusBar1.Canvas.Font.Color := defFontColor;
end;

procedure TfrmMain.tbtnAddBookmarkClick(Sender: TObject);
begin
  DiffText1.ToggleBookmark;
  tbtnNextBookmark.Enabled := (DiffText1.GetBookmarkCount > 0);
  tbtnPrevBookmark.Enabled := tbtnNextBookmark.Enabled;
  tbtnDelBookmark.Enabled := tbtnNextBookmark.Enabled;
end;

procedure TfrmMain.tbtnDelBookmarkClick(Sender: TObject);
begin
  DiffText1.ClearBookmark;
  tbtnNextBookmark.Enabled := False;
  tbtnPrevBookmark.Enabled := False;
  tbtnDelBookmark.Enabled := False;
end;

procedure TfrmMain.tbtnNextBookmarkClick(Sender: TObject);
begin
  DiffText1.MoveNextBookmark;
end;

procedure TfrmMain.tbtnPrevBookmarkClick(Sender: TObject);
begin
  DiffText1.MovePrevBookmark;
end;

procedure TfrmMain.DiffText1CaretMove(Sender: TObject; lineNum,
  colNum: Integer; availPrevDiff, availNextDiff: Boolean);
begin
  StatusBar1.Panels[3].Text := 'Ln ' + IntToStr(lineNum) + ', Col ' + IntToStr(colNum);
  tbtnPrevDiff.Enabled := availPrevDiff;
  tbtnNextDiff.Enabled := availNextDiff;
end;

procedure TfrmMain.tbtnPrevDiffClick(Sender: TObject);
begin
  DiffText1.MovePrevDifferent;
end;

procedure TfrmMain.tbtnNextDiffClick(Sender: TObject);
begin
  DiffText1.MoveNextDifferent;
end;

procedure TfrmMain.pmEditPopup(Sender: TObject);
begin
  NCopyToClipboard.Enabled := DiffText1.HasSelText;
end;

procedure TfrmMain.NCopyToClipboardClick(Sender: TObject);
begin
  DiffText1.CopyToClipboard;
end;

procedure TfrmMain.DiffText1PaneChanged(Sender: TObject; lineNum,
  colNum: Integer);
begin
  tbtnCopyToClipbord.Enabled := DiffText1.HasSelText;
  tbtnNextBookmark.Enabled := (DiffText1.GetBookmarkCount > 0);
  tbtnPrevBookmark.Enabled := tbtnNextBookmark.Enabled;
  tbtnDelBookmark.Enabled := tbtnNextBookmark.Enabled;
end;

procedure TfrmMain.tbtnFindClick(Sender: TObject);
begin
  frmFindText := TfrmFindText.Create(nil);
  frmFindText.SetFindKeyword(FFindKeyword);
  frmFindText.SetFindOptions (FFindOptions);

  if frmFindText.ShowModal = mrCancel then
  begin
    exit;
  end;

  tbtnFindNext.Enabled := True;
  tbtnFindPrev.Enabled := True;
  FFindKeyword := frmFindText.GetFindKeyword;
  FFindOptions := frmFindText.GetFindOptions;

  frmFindText.Free;
  frmFindText := nil;

  DiffText1.FindKeyword(FFindKeyword, FFindOptions);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFindKeyword := '';
  FFindOptions := [soDown];
end;

procedure TfrmMain.tbtnFindNextClick(Sender: TObject);
begin
  if FFindKeyword = '' then
  begin
    exit;
  end;
  DiffText1.FindKeyword(FFindKeyword, FFindOptions + [soDown]);
end;

procedure TfrmMain.tbtnFindPrevClick(Sender: TObject);
begin
  if FFindKeyword = '' then
  begin
    exit;
  end;
  DiffText1.FindKeyword(FFindKeyword, FFindOptions - [soDown]);
end;

procedure TfrmMain.DiffText1TextSelected(Sender: TObject;
  isSelected: Boolean);
begin
  tbtnCopyToClipbord.Enabled := isSelected;
end;

procedure TfrmMain.StartCompareFile(fileName1, fileName2: string;
  isIgnoreCase, isIgnoreWhiteSpace: Boolean);
var
  i, j : Integer;
  fileComp : TFileCompare;
  priorLine1, priorLine2 : Integer;
  diffLine1, diffLine2 : Integer;
  diffLarge : Integer;
  compResult : PCompareLine;
  tmpStr : string;
  txtFile1 : TTextFileStream;
  txtFile2 : TTextFileStream;
  fileCount1 : Integer;
  fileCount2 : Integer;
  linePos : DWord;
  dwTick : DWord;
  isCancel : Boolean;
begin
  isCancel := False;
  frmProcess := TfrmProcess.Create(nil);
  frmProcess.Show;
  try
    txtFile1 := TTextFileStream.Create(fileName1, fmOpenRead);
    txtFile2 := TTextFileStream.Create(fileName2, fmOpenRead);

    fileCount1 := 0;
    fileCount2 := 0;
    fileComp := TFileCompare.Create;
    while not txtFile1.Eof or not txtFile2.Eof do
    begin
      if frmProcess.CancelProcess then
      begin
        isCancel := True;
        exit;
      end;

      if not txtFile1.Eof then
      begin
        linePos := txtFile1.ReadLn(tmpStr);
        if isIgnoreCase then
        begin
          tmpStr := UpperCase (tmpStr);
        end;

        if isIgnoreWhiteSpace then
        begin
          tmpStr := Trim (tmpStr);
        end;
        fileComp.SetLine (0, tmpStr, linePos);
        Inc(fileCount1);
      end;

      if not txtFile2.Eof then
      begin
        linePos := txtFile2.ReadLn(tmpStr);
        if isIgnoreCase then
        begin
          tmpStr := UpperCase (tmpStr);
        end;

        if isIgnoreWhiteSpace then
        begin
          tmpStr := Trim (tmpStr);
        end;
        fileComp.SetLine (1, tmpStr, linePos);
        Inc(fileCount2);
      end;

      Application.ProcessMessages;
    end;

    fileComp.Compare;

    DiffText1.SetFileInfos(txtFile1, txtFile2);
    DiffText1.BeginUpdate;
    //Processing resuts...
    priorLine1 := -1;
    priorLine2 := -1;
    for i := 0 to fileComp.GetResultCount - 1 do
    begin
      if frmProcess.CancelProcess then
      begin
        isCancel := True;
        exit;
      end;
      
      compResult := fileComp.GetResult(i);
      diffLine1 := compResult.srcLineNum - priorLine1;
      diffLine2 := compResult.dstLineNum - priorLine2;

      if (diffLine1 = 1) and (diffLine2 > 1) then
      begin//Added line
        DiffText1.SetDifferentInfo (DiffText1.Compare1Count);
        for j := priorLine2 + 1 to compResult.dstLineNum - 1 do
        begin
          DiffText1.SetLineText(0, 0, ltDummy, False);
          DiffText1.SetLineText(1, fileComp.GetLinePos(1, j), ltInserted, True);
        end;
      end
      else if (diffLine1 > 1) and (diffLine2 = 1) then
      begin//Removed line
        DiffText1.SetDifferentInfo (DiffText1.Compare1Count);
        for j := priorLine1 + 1 to compResult.srcLineNum - 1 do
        begin
          DiffText1.SetLineText(0, fileComp.GetLinePos(0, j), ltDeleted, True);
          DiffText1.SetLineText(1, 0, ltDummy, False);
        end;
      end
      else
      begin//Changed line
        diffLarge := Max (diffLine1, diffLine2);
        if diffLarge > 1 then
        begin
          DiffText1.SetDifferentInfo (DiffText1.Compare1Count);
          for j := 1 to diffLarge - 1 do
          begin
            if priorLine1 + j < compResult.srcLineNum then
            begin
              DiffText1.SetLineText(0, fileComp.GetLinePos(0, priorLine1 + j), ltChanged, True);
            end
            else
            begin
              DiffText1.SetLineText(0, 0, ltDummy, False);
            end;

            if priorLine2 + j < compResult.dstLineNum then
            begin
              DiffText1.SetLineText(1, fileComp.GetLinePos(1, priorLine2 + j), ltChanged, True);
            end
            else
            begin
              DiffText1.SetLineText(1, 0, ltDummy, False);
            end;
          end;
        end;
      end;

      for j := 0 to compResult.sameLen - 1 do
      begin
        DiffText1.SetLineText(0, fileComp.GetLinePos(0, compResult.srcLineNum + j), ltNormal, True);
        DiffText1.SetLineText(1, fileComp.GetLinePos(1, compResult.dstLineNum + j), ltNormal, True);
      end;

      priorLine1 := compResult.srcLineNum + compResult.sameLen - 1;
      priorLine2 := compResult.dstLineNum + compResult.sameLen - 1;
      Application.ProcessMessages;
    end;

    //set the differencies between two files
    if fileComp.GetResultCount > 0 then
    begin
      if compResult.srcLineNum + compResult.sameLen < fileCount1 then
      begin
        DiffText1.SetDifferentInfo (DiffText1.Compare1Count);
      end;

      for i := compResult.srcLineNum + compResult.sameLen to fileCount1 - 1 do
      begin
        DiffText1.SetLineText (0, fileComp.GetLinePos(0, i), ltDeleted, True);
        DiffText1.SetLineText (1, 0, ltDummy, False);
      end;

      if compResult.dstLineNum + compResult.sameLen < fileCount2 then
      begin
        DiffText1.SetDifferentInfo (DiffText1.Compare1Count);
      end;

      for i := compResult.dstLineNum + compResult.sameLen to fileCount2 - 1 do
      begin
        DiffText1.SetLineText (0, 0, ltDummy, False);
        DiffText1.SetLineText (1, fileComp.GetLinePos(1, i), ltInserted, True);
      end;
      Application.ProcessMessages;
    end;

    DiffText1.EndUpdate;

  finally
    fileComp.Free;
    frmProcess.Free;

    if isCancel then
    begin
      Close;
    end;
  end;
end;

procedure TfrmMain.pmiCopyToClipboardClick(Sender: TObject);
begin
  if tbtnCopyToClipbord.Enabled then
  begin
    tbtnCopyToClipbordClick(nil);
  end;
end;

procedure TfrmMain.pmiFindClick(Sender: TObject);
begin
  tbtnFindClick(nil);
end;

procedure TfrmMain.pmiFindNextClick(Sender: TObject);
begin
  if tbtnFindNext.Enabled then
  begin
    tbtnFindNextClick(nil);
  end;
end;

procedure TfrmMain.pmiFindPrevClick(Sender: TObject);
begin
  if tbtnFindPrev.Enabled then
  begin
    tbtnFindPrevClick(nil);
  end;
end;

procedure TfrmMain.pmiAddBookmarkClick(Sender: TObject);
begin
  tbtnAddBookmarkClick(nil);
end;

procedure TfrmMain.pmiNextBookmarkClick(Sender: TObject);
begin
  if tbtnNextBookmark.Enabled then
  begin
    tbtnNextBookmarkClick(nil);
  end;
end;

procedure TfrmMain.pmiPrevBookmarkClick(Sender: TObject);
begin
  if tbtnPrevBookmark.Enabled then
  begin
    tbtnPrevBookmarkClick(nil);
  end;
end;

procedure TfrmMain.pmiDelBookmarkClick(Sender: TObject);
begin
  if tbtnDelBookmark.Enabled then
  begin
    tbtnDelBookmarkClick(nil);
  end;
end;

procedure TfrmMain.pmiPrevDiffClick(Sender: TObject);
begin
  if tbtnPrevDiff.Enabled then
  begin
    tbtnPrevDiffClick(nil);
  end;
end;

procedure TfrmMain.pmiNextDiffClick(Sender: TObject);
begin
  if tbtnNextDiff.Enabled then
  begin
    tbtnNextDiffClick(nil);
  end;
end;

end.
