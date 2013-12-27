unit DiffEdit;

interface

uses Windows, Controls, SysUtils, Classes, Messages, Graphics,
  Math, Dialogs, StdCtrls, Forms, StrUtils, Clipbrd, ExtCtrls,
  UTextFile;

type
  TDiffEditCaretMovedEvent = procedure (Sender : TObject; lineNum,
    colNum : Integer) of object;
  TDiffTextCaretMovedEvent = procedure (Sender : TObject; lineNum, colNum : Integer;
    availPrevDiff, availNextDiff : Boolean) of object;
  TPaneFocusedEvent = TDiffEditCaretMovedEvent;
  TTextSelectedEvent = procedure (Sender : TObject; isSelected : Boolean) of object;
  TEditScrollEvent = procedure (Sender : TObject; scrollPos : Integer) of object;

  //해당 라인 타입...
  TLineType = (ltNormal = 0, ltDummy, ltDeleted, ltChanged, ltInserted);

  TSelectInfo = record
    charPos : Integer;
    linePos : Integer;
  end;

  TLineInfo = record
    lineNum : Integer;
    lineFilePointer : DWord;
    contentType : TLineType;
    hasBookmark : Boolean;
  end;
  PLineInfo = ^TLineInfo;

  TDiffStrings = class
  private
    FLastLineNum : Integer;
    FLineData : TList;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    procedure SetLineText (filePos : DWord; contentType : TLineType;
      hasLineNum : Boolean);
    procedure Clear;
  end;

  TDiffEdit = class(TCustomControl)
  private
    FCaretX : Integer;
    FCaretY : Integer;
    FCaretPos : TSelectInfo;
    FSelBegin : TSelectInfo;
    FSelEnd : TSelectInfo;
    FFileInfo : TTextFileStream;
    FLineCountInPage : Integer;

    FLines : TDiffStrings;
    FBookmarkList : TList;

    FTxtHeight : Integer;
    FLineNumWidth : Integer;

    FHScrollPos : Integer;
    FVScrollPos : Integer;
    FIsMouseCaptured : Boolean;
    FPaintCS : Integer;

    //파일 내용을 그리기 위한 메모리 DC관련 변수
    FMemDC : HDC;
    FMemBitmap : HBitmap;
    FMemCanvas : TCanvas;

    //Events...
    FOnCaretMove : TDiffEditCaretMovedEvent;
    FOnPaneFocused : TPaneFocusedEvent;
    FOnTextSelected : TTextSelectedEvent;
    FOnHScroll : TEditScrollEvent;
    FOnVScroll : TEditScrollEvent;

  private
    procedure WMCreate(var Msg : TWMCreate); message WM_CREATE;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    procedure WMHScroll(var Msg: TWMScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMScroll); message WM_VSCROLL;
    procedure CMWantSpecialKey (var Message : TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CNKeyUp (var Message : TWMKey); message CN_KEYUP;

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

  protected
    procedure InitializeCaret;
    procedure HideCaret;
    procedure ShowCaret;
    procedure Paint; override;
    procedure PaintLineNumber (index, lineNum : Integer);
    procedure PaintBookmark (index : Integer);
    procedure PaintContent (index : Integer; pLine : PLineInfo);
    procedure PaintCurrentPosMark;
    procedure PaintSelectedText;
    procedure InitScrollBars;
    procedure SetScrollBarInfo;
    function CalcCaretPos (mouseX, mouseY : Integer) : TSelectInfo;
    function GetSelText : string;
    procedure UpdateCursor;
    procedure UpdateCaret;
    procedure MoveTo (line, col : Integer; isRepaint : Boolean = True);
    function ReadLine (pLine : PLineInfo) : string;
    procedure SetMemDC;
    function HasSelText : Boolean;
    function GetColumnEndPos (lineNum : Integer) : Integer;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;

    procedure SetLineText (filePos : DWord; contentType : TLineType; hasLineNum : Boolean);
    procedure CopyToClipboard;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    function GetCurrentPos : TPoint;
    //북마크 관련 함수들...
    function GetBookmarkCount : Integer;
    procedure ToggleBookmark;
    procedure ClearBookmark;
    procedure MoveNextBookmark;
    procedure MovePrevBookmark;
    //찾기관련 함수
    function FindKeyword (keyword : string; searchOptions : TStringSearchOptions;
      var findInfo : TSelectInfo) : Boolean;
    procedure SetTextSelectInLine (lineNum, colNum, length : Integer);

    procedure SetFileInfo (fileInfo : TTextFileStream);
    procedure SetHScrollPos (scrollPos : Integer);
    procedure SetVScrollPos (scrollPos : Integer);


  published
    property Align;
    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Hint;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnCaretMove : TDiffEditCaretMovedEvent read FOnCaretMove write FOnCaretMove;
    property OnPaneFocused : TPaneFocusedEvent read FOnPaneFocused write FOnPaneFocused;
    property OnTextSelected : TTextSelectedEvent read FOnTextSelected write FOnTextSelected;
    property OnHScroll : TEditScrollEvent read FOnHScroll write FOnHScroll;
    property OnVScroll : TEditScrollEvent read FOnVScroll write FOnVScroll;
  end;

  TDiffText = class(TCustomControl)
  private
    FLeftPane : TPanel;
    FRightPane : TPanel;
    FLeftTopPane : TPaintBox;
    FRightTopPane : TPaintBox;
    FCompare1 : TDiffEdit;
    FCompare1Count : Integer;
    FCompare2 : TDiffEdit;
    FCompare2Count : Integer;
    FSplitter : TSplitter;
    FCompareResult : TList;
    FFileName1 : string;
    FFileName2 : string;

    //Events...
    FOnCaretMove : TDiffTextCaretMovedEvent;
    FOnPaneChanged : TPaneFocusedEvent;
    FOnTextSelected : TTextSelectedEvent;
    procedure SetFileNames(fileName1, fileName2: string);

  private
    procedure WMCreate(var Msg : TWMCreate); message WM_CREATE;

  protected
    procedure Paint; override;

  protected
    procedure ClearCompareResult;
    function GetPrevDiff (lineIndex : Integer) : Integer;
    function GetNextDiff (lineIndex : Integer) : Integer;
    procedure OnDiffEditCaretMove (Sender : TObject; lineNum, colNum : Integer);
    procedure OnDiffEditFocuseChanged (Sender : TObject; lineNum, colNum : Integer);
    procedure OnDiffEditTextSelected(Sender : TObject; isSelected : Boolean);
    procedure OnDiffEditHScroll (Sender : TObject; scrollPos : Integer);
    procedure OnDiffEditVScroll (Sender : TObject; scrollPos : Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Free;

    procedure SetLineText (paneIndex : Integer; filePos : DWord;
      contentType : TLineType; hasLineNum : Boolean);
    procedure SetDifferentInfo (diffIndex : Integer);
    procedure CopyToClipboard;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    //Procedures and functions for bookmark...
    function GetBookmarkCount : Integer;
    procedure ToggleBookmark;
    procedure ClearBookmark;
    procedure MoveNextBookmark;
    procedure MovePrevBookmark;
    //Procedures and functions in order to move to changed line
    procedure MoveNextDifferent;
    procedure MovePrevDifferent;
    function HasSelText : Boolean;
    function FindKeyword (keyword : string; searchOptions : TStringSearchOptions) : Boolean;
    procedure SetFileInfos (file1, file2 : TTextFileStream);

  published
    property Align;
    property Left;
    property Top;
    property Width;
    property Height;
    property Cursor;
    property Hint;
    property Font;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Compare1Count : Integer read FCompare1Count;
    property Compare2Count : Integer read FCompare2Count;
    property OnCaretMove : TDiffTextCaretMovedEvent read FOnCaretMove write FOnCaretMove;
    property OnPaneChanged : TPaneFocusedEvent read FOnPaneChanged write FOnPaneChanged;
    property OnTextSelected : TTextSelectedEvent read FOnTextSelected write FOnTextSelected;
  end;

  procedure Register;
  function TabbedTextWidth (canvas : TCanvas; text : string) : Integer;
  function UltraPos (str, substr : string; startStrIndex : Integer;
    options : TStringSearchOptions) : Integer;
  function MakeShortText (Canvas : TCanvas; dispText : string;
    areaWidth : Integer) : string;

implementation

const
  LineNumContentGap = 10;//Pixel value of gap between line number and text
  BackColors : array [TLineType] of TColor =
    (clWhite, clInactiveBorder, clInactiveBorder, clInactiveBorder, clInactiveBorder);
  FontColors : array [TLineType] of TColor =
    (clWindowText, clGrayText, clRed, clBlue, clGreen);
  MaxHScroll : Integer = 5000;

var
  TabWidth : Integer = 32;

procedure Register;
begin
	RegisterComponents('SNComponent', [TDiffEdit]);
	RegisterComponents('SNComponent', [TDiffText]);
end;

function TabbedTextWidth (canvas : TCanvas; text : string) : Integer;
var
  idx : Integer;
  totalTabWidth : Integer;
begin
  //Basically, Tab letter doesn't affect to TextWidth
  //but in this case, it need to be calculated.
  //HIWORD is height, LOWORD is width.
  Result := LoWord(GetTabbedTextExtent (canvas.Handle, PChar(text), Length(text), 1, TabWidth));
end;

function UltraPos (str, substr : string; startStrIndex : Integer;
  options : TStringSearchOptions) : Integer;
var
  findIndex : Integer;
begin
  Result := 0;
  if Length (substr) > Length(str) then
  begin
    exit;
  end;

  if not (soDown in options) then
  begin//Reverse find...
    str := ReverseString (str);
    substr := ReverseString(substr);
    startStrIndex := Length(str) - startStrIndex + 1;
  end;

  if not (soMatchCase in options) then
  begin
    str := UpperCase (str);
    substr := UpperCase (substr);
  end;

  try
    //If the length of start position + keyword length is longer than document length,
    //there is no matching text.
    while startStrIndex + Length(substr) - 1 < Length(str) do
    begin
      try
        findIndex := PosEx (substr, str, startStrIndex);
        if (findIndex > 0) and not (soWholeWord in options) then
        begin
          Result := findIndex;
          exit;
        end
        else if (findIndex > 0) and (soWholeWord in options) then
        begin
          //Whole word option is on.
          //If found keyword is a part of word, ignore this keyword.
          if findIndex - 1 > 0 then
          begin//Check right before the found keyword
            if not (str[findIndex - 1] in [#9, #32..#47, #58..#64, #91..#96, #123..#127]) then
            begin
              continue;
            end;
          end;

          if findIndex + Length(substr) <= Length(str) then
          begin//Check right after the found keyword
            if not (str[findIndex + Length(substr)] in [#9, #32..#47, #58..#64, #91..#96, #123..#127]) then
            begin
              continue;
            end;
          end;

          Result := findIndex;
          exit;
        end;
      finally
        Inc (startStrIndex, Length(substr));
      end;
    end;
  finally
    if (Result > 0) and not (soDown in options) then
    begin
      Result := Length(str) - Result + 1 - Length(substr) + 1
    end;
  end;
end;

function MakeShortText (Canvas : TCanvas; dispText : string; areaWidth : Integer) : string;
var
	txtWidth : Integer;
  i : Integer;
  tmpText : WideString;
begin
	Result := dispText;

  if Canvas.TextWidth(dispText) <= areaWidth then
  begin
  	exit;
  end;

  tmpText := dispText;
  for i := Length (tmpText) - 1 downto 1 do
  begin
  	tmpText := LeftStr (tmpText, i);

    if Canvas.TextWidth(tmpText + '...') <= areaWidth then
    begin
    	Result := tmpText + '...';
      exit;
    end;
  end;
end;

{ TDiffStrings }

procedure TDiffStrings.Clear;
var
  i : Integer;
begin
  if FLineData <> nil then
  begin
    for i := 0 to FLineData.Count - 1 do
    begin
      Dispose (FLineData[i]);
    end;
  end;
  FLineData.Clear;
  FLastLineNum := 0;
end;

constructor TDiffStrings.Create;
begin
  FLineData := TList.Create;
  FLastLineNum := 0;
end;

destructor TDiffStrings.Destroy;
begin
  Free;
  inherited;
end;

procedure TDiffStrings.Free;
begin
  if FLineData <> nil then
  begin
    Clear;
    FLineData.Free;
    FLineData := nil;
  end;
  FLastLineNum := 0;
end;

procedure TDiffStrings.SetLineText(filePos : DWord; contentType : TLineType;
  hasLineNum : Boolean);
var
  newLine : PLineInfo;
begin
  New (newLine);

  newLine.lineNum := -1;
  if hasLineNum then
  begin
    Inc (FLastLineNum);
    newLine.lineNum := FLastLineNum;
  end;
//  newLine.content := content;
  newLine.lineFilePointer := filePos;
  newLine.contentType := contentType;
  newLine.hasBookmark := False;

  FLineData.Add(newLine); 

//  FLineNum.Add(lineNum);
//  FLines.Add(content);
end;

{ TDiffEdit }

procedure TDiffEdit.BeginUpdate;
begin
  Inc(FPaintCS);
end;


function TDiffEdit.CalcCaretPos (mouseX, mouseY : Integer) : TSelectInfo;
var
  horzLine : Integer;
  vertLine : Integer;
  curLine : PLineInfo;
  i : Integer;
  pos : Integer;
  tmpWidth : Integer;
  content : WideString;
begin
  if FLines.FLineData.Count < 1 then
  begin
    horzLine := FLineNumWidth + LineNumContentGap - FHScrollPos;
    vertLine := 0;
    exit;
  end;

  horzLine := 0;
  mouseX := mouseX - (FLineNumWidth + LineNumContentGap) + FHScrollPos;

  //Calc new caret position
  if mouseY + FVScrollPos < 0 then
  begin
    vertLine := 0;
  end
  else
  begin
    vertLine := (mouseY div FTxtHeight) + FVScrollPos;
  end;

  if vertLine >= FLines.FLineData.Count then
  begin
    vertLine := FLines.FLineData.Count - 1;
  end;
  //Basically, vertLine is assigned a value of current line count - 1 so that make + 1
  Result.linePos := vertLine + 1;

  //Calc the horz position of caret
  curLine := FLines.FLineData[vertLine];
  content := ReadLine(curLine);
  if mouseX >= TabbedTextWidth(Canvas, content) then
  begin
    //When horz position of caret is out of end position of text
    Result.charPos := Length (content);
  end
  else if mouseX < 0 then
  begin
    Result.charPos := 0;
  end
  else
  begin
    horzLine := mouseX;
    pos := 0;
    for i := 1 to Length (content) do
    begin
      if content[i] = #9 then
      begin
        //When the TAB letter is in the middle of line sentence, length may be changed,
        //Extract the length of TAB letter which will be drawn from a value of
        //the length of sentence without the TAB letter.
        pos := TabbedTextWidth(Canvas, LeftStr(content, i)) -
               TabbedTextWidth(Canvas, LeftStr(content, i - 1));
      end
      else
      begin
        pos := TabbedTextWidth (Canvas, content[i]);
      end;

      if mouseX >= pos then
      begin
        mouseX := mouseX - pos;
      end
      else
      begin
        //If mouse X position is bigger then the middle of letter, put the caret to next position.
        if mouseX >= (pos div 2) then
        begin
          Result.charPos := i;
        end
        else
        begin//Less than the middle of letter, put the caret to prior letter.
          Result.charPos := i - 1;
        end;

        if Result.charPos = 0 then
        begin
          horzLine := FLineNumWidth + LineNumContentGap;
        end
        else
        begin
          horzLine := TabbedTextWidth (Canvas, LeftStr (content, Result.charPos)) +
                      FLineNumWidth + LineNumContentGap;
        end;
        break;
      end;
    end;
  end;

  FCaretPos := Result;
  MoveTo (Result.linePos - 1, Result.charPos, False);
end;

procedure TDiffEdit.Clear;
begin
  ClearBookmark;
  FHScrollPos := 0;
  FVScrollPos := 0;
  InitScrollBars;
  FLines.Clear;
end;

procedure TDiffEdit.CopyToClipboard;
var
  clipMem : HGLOBAL;
  pTarget : PChar;
  copyContent : string;
  contentLen : Integer;
begin
  //Copy the selected text to clipboard
  copyContent := GetSelText;
  contentLen := Length (copyContent);

  //If there is no selected, just exit the procedure.
  if contentLen = 0 then
  begin
    exit;
  end;

  Clipboard.Clear;
  Clipboard.SetTextBuf(PChar (copyContent));
end;

constructor TDiffEdit.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse,csClickEvents,csSetCaption..csDoubleClicks,csNeedsBorderPaint];
  ParentFont := True;
  TabStop := True;
  FLines := TDiffStrings.Create;
  FBookmarkList := TList.Create;
  FMemCanvas := TCanvas.Create;
  FFileInfo := nil;

  FCaretX := 0;
  FCaretY := 0;
  FCaretPos.linePos := 0;
  FCaretPos.charPos := 0;
  FHScrollPos := 0;
  FVScrollPos := 0;
  FLineCountInPage := 0;
  
  FMemDC := 0;
  FMemBitmap := 0;

  FIsMouseCaptured := False;

  ZeroMemory (@FSelBegin, sizeof (TSelectInfo));
  ZeroMemory (@FSelEnd, sizeof (TSelectInfo));

  FPaintCS := 0;

  DoubleBuffered := True;

  //Initialize event
  FOnCaretMove := nil;
end;

destructor TDiffEdit.Destroy;
begin
  Free;
  inherited;
end;

procedure TDiffEdit.EndUpdate;
begin
  Dec (FPaintCS);
  SetScrollBarInfo;
  Invalidate;
end;

procedure TDiffEdit.Free;
begin
  if FMemCanvas <> nil then
  begin
    FMemCanvas.Free;
    FMemCanvas := nil;
  end;

  if FMemBitmap <> 0 then
  begin
    DeleteObject(FMemBitmap);
    FMemBitmap := 0;
  end;

  if FMemDC <> 0 then
  begin
    DeleteDC(FMemDC);
    FMemDC := 0;
  end;

  if FBookmarkList <> nil then
  begin
    ClearBookmark;
    FBookmarkList.Free;
    FBookmarkList := nil;
  end;

  if FLines <> nil then
  begin
    FLines.Free;
    FLines := nil;
  end;

  if FFileInfo <> nil then
  begin
    FFileInfo.Free;
    FFileInfo := nil;
  end;

  DestroyCaret;
end;

function TDiffEdit.GetSelText: string;
var
  tmpSel : TSelectInfo;
  tmpSelBegin : TSelectInfo;
  tmpSelEnd : TSelectInfo;
  curLine : PLineInfo;
  i : Integer;
  content : WideString;
begin
  Result := '';
  //If there is no selected or out of bound, exit function.
  if not HasSelText or
     (FSelEnd.linePos >= FLines.FLineData.Count) then
  begin
    exit;
  end;

  //Makes tmpSelBegin smaller than tmpSelEnd
  tmpSelBegin := FSelBegin;
  tmpSelEnd := FSelEnd;
  if (tmpSelBegin.linePos > tmpSelEnd.linePos) or
     ((tmpSelBegin.linePos = tmpSelEnd.linePos) and
     (tmpSelBegin.charPos > tmpSelEnd.charPos)) then
  begin
    tmpSel := tmpSelBegin;
    tmpSelBegin := tmpSelEnd;
    tmpSelEnd := tmpSel;
  end;

  if tmpSelBegin.linePos = tmpSelEnd.linePos then
  begin//If selected lines are same
    curLine := FLines.FLineData[tmpSelBegin.linePos - 1];
    content := ReadLine(curLine);
    Result := MidStr (content, tmpSelBegin.charPos + 1,
      tmpSelEnd.charPos - tmpSelBegin.charPos);
  end
  else
  begin
    for i := tmpSelBegin.linePos - 1 to tmpSelEnd.linePos - 1 do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      if i = tmpSelBegin.linePos - 1 then
      begin//First line
        Result := RightStr (PWideChar(@content[tmpSelBegin.charPos + 1]),
                  Length (content) - tmpSelBegin.charPos);
        Result := Result + #13#10;
      end
      else if i = tmpSelEnd.linePos - 1 then
      begin//Last line
        Result := Result + LeftStr (content, tmpSelEnd.charPos);
      end
      else
      begin//Middle lines
        Result := Result + content + #13#10;
      end;
    end;// end for
  end;
end;

procedure TDiffEdit.HideCaret;
begin
  Windows.HideCaret(Handle);
end;

procedure TDiffEdit.InitializeCaret;
begin
  CreateCaret(Handle, 0, 2, FTxtHeight);
end;

procedure TDiffEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  scrollInfo : TScrollInfo;
  linePos : Integer;
  charPos : Integer;
begin
  linePos := FCaretPos.linePos - 1;
  charPos := FCaretPos.charPos;

  case Key of
    VK_HOME : charPos := 0;
    VK_END : charPos := GetColumnEndPos(linePos);
    VK_LEFT : Dec(charPos);
    VK_RIGHT : Inc(charPos);
    VK_UP : Dec(linePos);
    VK_DOWN : Inc(linePos);

    VK_PRIOR :
    begin
      Dec(linePos, FLineCountInPage);
      if FLines.FLineData.Count > FLineCountInPage then
      begin
        SetVScrollPos (FVScrollPos - FLineCountInPage);
      end;
    end;
    VK_NEXT :
    begin
      Inc(linePos, FLineCountInPage);
      if FLines.FLineData.Count > FLineCountInPage then
      begin
        SetVScrollPos (FVScrollPos + FLineCountInPage);
      end;
    end;

    else
      inherited;
      exit;
  end;

  if not HasSelText and (ssShift in Shift) then
  begin
    FSelBegin := FCaretPos;
  end;

  MoveTo(linePos, charPos, False);

  if ssShift in Shift then
  begin
    FSelEnd := FCaretPos;
  end
  else
  begin
    FSelEnd := FSelBegin;
  end;
  Invalidate;

  if Assigned(FOnVScroll) then
  begin
    FOnVScroll(Self, FVScrollPos);
  end;

  if Assigned (FOnTextSelected) then
  begin
    FOnTextSelected (Self, HasSelText);
  end;
end;

procedure TDiffEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  isFocused : Boolean;
begin
  inherited;

  if Button = mbLeft then
  begin
    SetCapture (Handle);
    FIsMouseCaptured := True;
    FSelBegin := CalcCaretPos (X, Y);
    FSelEnd := FSelBegin;
    Invalidate;
  end;

  isFocused := Focused;
  SetFocus;
  if not isFocused then
  begin
    if Assigned (FOnPaneFocused) then
    begin
      FOnPaneFocused (Self, FCaretPos.linePos, FCaretPos.charPos + 1);
    end;
  end;

  if Assigned (FOnTextSelected) then
  begin
    FOnTextSelected (Self, False);
  end;
end;

procedure TDiffEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (ssLeft in Shift) and FIsMouseCaptured then
  begin
    FSelEnd := CalcCaretPos (X, Y);

    if Assigned (FOnTextSelected) then
    begin
      FOnTextSelected (Self, HasSelText);
    end;
    Invalidate;
  end;
end;

procedure TDiffEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if FIsMouseCaptured then
  begin
    ReleaseCapture;
    FIsMouseCaptured := False;
    FSelEnd := CalcCaretPos (X, Y);

    if Assigned (FOnTextSelected) then
    begin
      FOnTextSelected (Self, HasSelText);
    end;
    Invalidate;
  end;
end;

procedure TDiffEdit.Paint;
var
  i : Integer;
  curLine : PLineInfo;
  defFontColor : TColor;
  defPenColor : TColor;
  defBrushColor : TColor;
begin
  if FPaintCS > 0 then
  begin
    exit;
  end;

  Canvas.Rectangle(-1, -1, Width+1, Height+1);
  FMemCanvas.Rectangle(-1, -1, Width - FLineNumWidth - LineNumContentGap + 1, Height + 1);

  if FTxtHeight = 0 then
  begin
    FTxtHeight := Canvas.TextHeight('뷁');
  end;

  if FLines.FLineData.Count < 1 then
  begin
    exit;
  end;

  defFontColor := Canvas.Font.Color;
  defPenColor := Canvas.Pen.Color;
  defBrushColor := Canvas.Brush.Color;

  SetTextAlign (FMemCanvas.Handle, GetTextAlign(Canvas.Handle) and TA_UPDATECP);
  for i := FVScrollPos to Min (FVScrollPos + FLineCountInPage + 1, FLines.FLineData.Count) - 1 do
  begin
    curLine := FLines.FLineData[i];
    //Draw line number
    PaintLineNumber (i, curLine.lineNum);

    //Draw bookmark
    if curLine.hasBookmark then
    begin
      PaintBookmark (i);
    end;

    //Draw line text
    PaintContent (i, curLine);
  end;

  PaintCurrentPosMark;
  PaintSelectedText;

  BitBlt (Canvas.Handle, FLineNumWidth + LineNumContentGap, 0,
    Width - FLineNumWidth - LineNumContentGap, Height, FMemCanvas.Handle, 0, 0, SRCCOPY);

  Canvas.Font.Color := defFontColor;
  Canvas.Pen.Color := defPenColor;
  Canvas.Brush.Color := defBrushColor;
end;

procedure TDiffEdit.PaintSelectedText;
var
  tmpSel : TSelectInfo;
  tmpSelBegin : TSelectInfo;
  tmpSelEnd : TSelectInfo;
  i, j : Integer;
  prevBackColor : COLORREF;
  prevFontColor : TColor;
  curLine : PLineInfo;
  nonSelWidth : Integer;
  content : WideString;
begin
  if (FMemDC = 0) or (FMemBitmap = 0) then
  begin
    exit;
  end;

  if not Focused or not HasSelText then
  begin
    exit;
  end;

  //Makes tmpSelBegin smaller than tmpSelEnd
  tmpSelBegin := FSelBegin;
  tmpSelEnd := FSelEnd;
  if (tmpSelBegin.linePos > tmpSelEnd.linePos) or
     ((tmpSelBegin.linePos = tmpSelEnd.linePos) and
     (tmpSelBegin.charPos > tmpSelEnd.charPos)) then
  begin
    tmpSel := tmpSelBegin;
    tmpSelBegin := tmpSelEnd;
    tmpSelEnd := tmpSel;
  end;

  //Prepare font
  prevFontColor := FMemCanvas.Font.Color;
  FMemCanvas.Font.Color := clHighlightText;
  prevBackColor := SetBkColor (FMemCanvas.Handle, ColorToRGB(clHighlight));

  if tmpSelBegin.linePos <> tmpSelEnd.linePos then
  begin
    for i := tmpSelBegin.linePos - 1 to tmpSelEnd.linePos - 1 do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      if i = tmpSelBegin.linePos - 1 then
      begin
        nonSelWidth := TabbedTextWidth (Canvas, LeftStr (content, tmpSelBegin.charPos));
        TabbedTextOutW (FMemCanvas.Handle, nonSelWidth - FHScrollPos, FTxtHeight * (i - FVScrollPos),
                PWideChar (@content[tmpSelBegin.charPos + 1]),
                Length (content) - tmpSelBegin.charPos,
                1, TabWidth, -FHScrollPos);
      end
      else if i = tmpSelEnd.linePos - 1 then
      begin
        TabbedTextOutW (FMemCanvas.Handle, -FHScrollPos,
                FTxtHeight * (i - FVScrollPos),
                PWideChar (content),
                tmpSelEnd.charPos,
                1, TabWidth, -FHScrollPos);
      end
      else
      begin
        TabbedTextOutW (FMemCanvas.Handle, -FHScrollPos,
                FTxtHeight * (i - FVScrollPos),
                PWideChar (content),
                Length (content),
                1, TabWidth, -FHScrollPos);
      end;
    end;
  end
  else
  begin
    //If the start and end are same
    curLine := FLines.FLineData[tmpSelBegin.linePos - 1];
    content := ReadLine(curLine);
    nonSelWidth := TabbedTextWidth (Canvas, LeftStr (content, tmpSelBegin.charPos));
    TabbedTextOutW (FMemCanvas.Handle, nonSelWidth - FHScrollPos, (tmpSelBegin.linePos - 1 - FVScrollPos) * FTxtHeight,
            PWideChar (@content[tmpSelBegin.charPos + 1]),
            tmpSelEnd.charPos - tmpSelBegin.charPos,
            1, TabWidth, -FHScrollPos);
  end;

  //Restore font
  FMemCanvas.Font.Color := prevFontColor;
  SetBkColor (FMemCanvas.Handle, prevBackColor);
end;

procedure TDiffEdit.SetLineText(filePos : DWord; contentType : TLineType;
  hasLineNum: Boolean);
begin
  FLines.SetLineText(filePos, contentType, hasLineNum);
  FLineNumWidth := Canvas.TextWidth(IntToStr (FLines.FLastLineNum));
  if FPaintCS > 0 then
  begin
    exit;
  end;

  if FPaintCS = 0 then
  begin
    SetScrollBarInfo;
  end;
end;

procedure TDiffEdit.SetScrollBarInfo;
var
  scrollInfo : TScrollInfo;
begin
  if FTxtHeight < 1 then
  begin
    exit;
  end;

  FLineCountInPage := (ClientRect.Bottom - ClientRect.Top) div FTxtHeight;
  with scrollInfo do
  begin
    cbSize := SizeOf (TScrollInfo);
    fMask := SIF_PAGE or SIF_RANGE or SIF_TRACKPOS;
    nMin := 0;
    nMax := FLines.FLineData.Count - 1;
    nPage := FLineCountInPage;
    nPos := 0;
    nTrackPos := 0;
  end;
  SetScrollInfo (Handle, SB_VERT, scrollInfo, True);

  with scrollInfo do
  begin
    cbSize := SizeOf (TScrollInfo);
    fMask := SIF_PAGE or SIF_RANGE or SIF_TRACKPOS;
    nMin := 0;
    nMax := MaxHScroll;
    nPage := Width div 2;
    nPos := 0;
    nTrackPos := 0;
  end;
  SetScrollInfo (Handle, SB_HORZ, scrollInfo, True);
end;

procedure TDiffEdit.InitScrollBars;
var
  scInfo : TScrollInfo;
begin
  ZeroMemory (@scInfo, sizeof (TScrollInfo));
  scInfo.cbSize := SizeOf(TScrollInfo);
  scInfo.fMask := SIF_ALL;

  ShowScrollBar(Handle, SB_BOTH, True);
  SetScrollInfo (Handle, SB_HORZ, scInfo, True);
  SetScrollInfo (Handle, SB_VERT, scInfo, True);
end;

procedure TDiffEdit.ShowCaret;
begin
  PaintCurrentPosMark;
  if Focused then
  begin
    Windows.SetCaretPos(FCaretX, FCaretY);
    Windows.ShowCaret(Handle);
  end;
end;

procedure TDiffEdit.UpdateCursor;
var
  pt : TPoint;
  rt : TRect;
begin
  pt := Self.ScreenToClient(Mouse.CursorPos);
  if pt.X <= FLineNumWidth + LineNumContentGap then
  begin
    SetCursor (Screen.Cursors[crDefault]);
  end
  else
  begin
    rt := GetClientRect;
    if (pt.X >= rt.Left) and (pt.X <= rt.Right) and
       (pt.Y >= rt.Top) and (pt.Y <= rt.Bottom) then
    begin
      SetCursor (Screen.Cursors[crIBeam]);
    end
    else
    begin
      SetCursor (Screen.Cursors[crDefault]);
    end;
  end;
end;

procedure TDiffEdit.WMCreate(var Msg: TWMCreate);
begin
  Canvas.Font := Font;
  InitScrollBars;
  SetMemDC;
  if FTxtHeight = 0 then
  begin
    FTxtHeight := Canvas.TextHeight('뷁');
    FLineCountInPage := (ClientRect.Bottom - ClientRect.Top) div FTxtHeight;
  end;
end;

procedure TDiffEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TDiffEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  HideCaret;
  Invalidate;
end;

procedure TDiffEdit.WMMouseWheel(var Msg: TMessage);
var
  scrollInfo : TScrollInfo;
  scrollPos : Integer;
begin
  if FLines.FLineData.Count <= FLineCountInPage then
  begin
    exit;
  end;

  ZeroMemory (@scrollInfo, SizeOf (TScrollInfo));
  scrollInfo.cbSize := SizeOf (TScrollInfo);
  scrollInfo.fMask := SIF_ALL;
  GetScrollInfo (Handle, SB_VERT, scrollInfo);

  if Integer (Msg.WParam) < 0 then
  begin
    scrollPos := Min (FVScrollPos + 3, scrollInfo.nMax);
  end
  else
  begin
    scrollPos := Max (FVScrollPos - 3, scrollInfo.nMin);
  end;

  SetVScrollPos(scrollPos);
  UpdateCaret;
  Invalidate;

  if Assigned(FOnVScroll) then
  begin
    FOnVScroll(Self, FVScrollPos);
  end;
end;

procedure TDiffEdit.WMSetCursor(var Msg: TWMSetCursor);
begin
  UpdateCursor;
  Msg.Result := 1;
end;

procedure TDiffEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  InitializeCaret;
  ShowCaret;
  Invalidate;
end;

procedure TDiffEdit.WMSize(var Msg: TWMSize);
begin
  SetMemDC;
  SetScrollBarInfo;
end;

procedure TDiffEdit.WMHScroll(var Msg: TWMScroll);
var
  scrollInfo : TScrollInfo;
  scrollPos : Integer;
begin
  ZeroMemory (@scrollInfo, SizeOf (TScrollInfo));
  scrollInfo.cbSize := SizeOf (TScrollInfo);
  scrollInfo.fMask := SIF_ALL;
  GetScrollInfo (Handle, SB_HORZ, scrollInfo);

  case Msg.ScrollCode of
    SB_RIGHT : scrollPos := MaxHScroll;
    SB_LINERIGHT : scrollPos := Min (FHScrollPos + 1, MaxHScroll);
    SB_LINELEFT : scrollPos := Max (FHScrollPos - 1, 0);
    SB_PAGERIGHT : scrollPos := Min (FHScrollPos + scrollInfo.nPage, MaxHScroll);
    SB_PAGELEFT : scrollPos := Max (FHScrollPos - scrollInfo.nPage, 0);
    SB_THUMBPOSITION,
    SB_THUMBTRACK : scrollPos := scrollInfo.nTrackPos;
    SB_LEFT : scrollPos := 0;
    SB_ENDSCROLL : exit;
  end;

  SetHScrollPos (scrollPos);
  Msg.Result := 1;

  if Assigned (FOnHScroll) then
  begin
    FOnHScroll(Self, FHScrollPos);
  end;
end;

procedure TDiffEdit.WMVScroll(var Msg: TWMScroll);
var
  scrollInfo : TScrollInfo;
  scrollPos : Integer;
begin
  ZeroMemory (@scrollInfo, SizeOf (TScrollInfo));
  scrollInfo.cbSize := SizeOf (TScrollInfo);
  scrollInfo.fMask := SIF_ALL;
  GetScrollInfo (Handle, SB_VERT, scrollInfo);

  case Msg.ScrollCode of
    SB_BOTTOM : scrollPos := scrollInfo.nMax;
    SB_LINEDOWN : scrollPos := Min (FVScrollPos + 1, scrollInfo.nMax);
    SB_LINEUP : scrollPos := Max (FVScrollPos - 1, 0);
    SB_PAGEDOWN : scrollPos := Min (FVScrollPos + scrollInfo.nPage, scrollInfo.nMax);
    SB_PAGEUP : scrollPos := Max (FVScrollPos - scrollInfo.nPage, 0);
    SB_THUMBPOSITION,
    SB_THUMBTRACK : scrollPos := scrollInfo.nTrackPos;
    SB_TOP : scrollPos := scrollInfo.nMin;
    SB_ENDSCROLL : exit;
  end;

  SetVScrollPos(scrollPos);
  Msg.Result := 1;

  if Assigned(FOnVScroll) then
  begin
    FOnVScroll(Self, FVScrollPos);
  end;
end;

function TDiffEdit.GetCurrentPos: TPoint;
begin
  Result := TPoint(FCaretPos);
end;

procedure TDiffEdit.ToggleBookmark;
var
  i : Integer;
  bookmarkLine : Integer;
  curLine : PLineInfo;
  findNewPos : Integer;
begin
  if FLines.FLineData.Count = 0 then
  begin
    exit;
  end;

  //Add a bookmark on current position
  if FBookmarkList.Count = 0 then
  begin//If there is no bookmark, just add one
    FBookmarkList.Add(Pointer(FCaretPos.linePos - 1));
    curLine := FLines.FLineData[FCaretPos.linePos - 1];
    curLine.hasBookmark := True;
  end
  else
  begin//If there are bookmarks
    curLine := FLines.FLineData[FCaretPos.linePos - 1];
    if curLine.hasBookmark then
    begin//If current line already has a bookmark, remove it.
      curLine.hasBookmark := False;
      FBookmarkList.Remove(Pointer(FCaretPos.linePos - 1));
    end
    else
    begin
      //If there is no bookmark on current line, add it
      //Add a bookmark into bookmark list as ascending order
      //If it made binary search, it'd be faster.
      //Now I made just it as sequencial search
      findNewPos := 0;
      for i := 0 to FBookmarkList.Count - 1 do
      begin
        bookmarkLine := Integer(FBookmarkList[i]);
        if FCaretPos.linePos - 1 < bookmarkLine then
        begin
          break;
        end;
        Inc(findNewPos);
      end;

      FBookmarkList.Insert(findNewPos, Pointer(FCaretPos.linePos - 1));
      curLine.hasBookmark := True;
    end;
  end;
  Invalidate;
end;

procedure TDiffEdit.ClearBookmark;
var
  i : Integer;
  bookmarkLine : Integer;
  curLine : PLineInfo;
begin
  //Remove all bookmarks like VSS
  for i := 0 to FBookmarkList.Count - 1 do
  begin
    bookmarkLine := Integer(FBookmarkList[i]);
    curLine := FLines.FLineData[bookmarkLine];
    curLine.hasBookmark := False;
  end;
  FBookmarkList.Clear;
  Invalidate;
end;

function TDiffEdit.GetBookmarkCount: Integer;
begin
  Result := FBookmarkList.Count;
end;

procedure TDiffEdit.MoveNextBookmark;
var
  i : Integer;
  bookmarkLine : Integer;
begin
  //Go to the nearest bookmark after the current position of caret
  if FBookmarkList.Count = 0 then
  begin
    exit;
  end;

  for i :=  0 to FBookmarkList.Count - 1 do
  begin
    bookmarkLine := Integer(FBookmarkList[i]);
    if bookmarkLine > FCaretPos.linePos - 1 then
    begin
      MoveTo (bookmarkLine, 0);
      exit;
    end;
  end;

  //If it couldn't move on for loop, just go to first bookmark.
  bookmarkLine := Integer(FBookmarkList.First);
  MoveTo (bookmarkLine, 0);
end;

procedure TDiffEdit.MovePrevBookmark;
var
  i : Integer;
  bookmarkLine : Integer;
begin
  //Go to the nearest bookmark before the current position of caret
  if FBookmarkList.Count = 0 then
  begin
    exit;
  end;

  for i :=  FBookmarkList.Count - 1 downto 0 do
  begin
    bookmarkLine := Integer(FBookmarkList[i]);
    if bookmarkLine < FCaretPos.linePos - 1 then
    begin
      MoveTo (bookmarkLine, 0);
      exit;
    end;
  end;

  //If it couldn't move on for loop, just go to last bookmark.
  bookmarkLine := Integer(FBookmarkList.Last);
  MoveTo (bookmarkLine, 0);
end;

procedure TDiffEdit.MoveTo(line, col: Integer; isRepaint : Boolean);
var
  curLine : PLineInfo;
  content : WideString;
  scrollPos : Integer;
begin
  if line < 0 then
  begin
    line := 0;
  end
  else if line >= FLines.FLineData.Count then
  begin
    line := FLines.FLineData.Count - 1;
  end;

  curLine := FLines.FLineData[line];
  content := ReadLine (curLine);
  if col < 0 then
  begin
    col := 0;
  end
  else if col > Length(content) then
  begin
    col := Length(content);
  end;

  FCaretPos.linePos := line + 1;
  FCaretPos.charPos := col;

  //If the line that will show is not in the screen, scroll the document vertically
  if line < FVScrollPos then
  begin
    scrollPos := line;
    SetVScrollPos (scrollPos);
    if Assigned(FOnVScroll) then
    begin
      FOnVScroll(Self, FVScrollPos);
    end;
  end
  else if line > FVScrollPos + FLineCountInPage - 1 then
  begin//If the line is out of the current visibles
    scrollPos := line - (FLineCountInPage - 1);
    SetVScrollPos (scrollPos);
    if Assigned(FOnVScroll) then
    begin
      FOnVScroll(Self, FVScrollPos);
    end;
  end;

  //If the columns that will show is not in the screen, scroll the document horizontally
  if TabbedTextWidth (Canvas, LeftStr (content, FCaretPos.charPos)) -
      FHScrollPos < 0 then
  begin//too left
    scrollPos := TabbedTextWidth (Canvas, LeftStr (content, FCaretPos.charPos));
    SetHScrollPos (scrollPos);
    if Assigned(FOnHScroll) then
    begin
      FOnHScroll(Self, FHScrollPos);
    end;
  end
  else if TabbedTextWidth (Canvas, LeftStr (content, FCaretPos.charPos)) +
    FLineNumWidth + LineNumContentGap - FHScrollPos >
    (ClientRect.Right - ClientRect.Left) then
  begin//too right
    scrollPos := (TabbedTextWidth (Canvas, LeftStr (content, FCaretPos.charPos)) +
                FLineNumWidth + LineNumContentGap) -
                (ClientRect.Right - ClientRect.Left) + 2;
    //line - (FLineCountInPage - 1);
    SetHScrollPos (scrollPos);
    if Assigned(FOnHScroll) then
    begin
      FOnHScroll(Self, FHScrollPos);
    end;
  end;

  UpdateCaret;

  if isRepaint then
  begin
    Invalidate;
  end;

  //Shot the event of OnCaretMove
  if Assigned (FOnCaretMove) then
  begin
    FOnCaretMove (Self, FCaretPos.linePos, FCaretPos.charPos + 1);
  end;
end;

procedure TDiffEdit.UpdateCaret;
var
  visibleLineCnt : Integer;
  curLine : PLineInfo;
  content : WideString;
begin
  if FCaretPos.linePos = 0 then
  begin
    exit;
  end;
  curLine := FLines.FLineData[FCaretPos.linePos - 1];
  content := ReadLine(curLine);
  FCaretX := TabbedTextWidth (Canvas, LeftStr (content, FCaretPos.charPos)) +
            FLineNumWidth + LineNumContentGap - FHScrollPos;
  FCaretY := (FCaretPos.linePos - 1 - FVScrollPos) * FTxtHeight;

  ShowCaret;
end;

procedure TDiffEdit.PaintBookmark (index : Integer);
begin
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clAqua;
  Canvas.RoundRect(FLineNumWidth, FTxtHeight * (index - FVScrollPos), FLineNumWidth + LineNumContentGap,
            FTxtHeight * (index - FVScrollPos + 1), 5, 5);
end;

procedure TDiffEdit.PaintLineNumber(index, lineNum: Integer);
var
  txtLineNum : string;
  lineNumRt : TRect;
begin
  if lineNum = -1 then
  begin
    txtLineNum := '';
  end
  else
  begin
    txtLineNum := IntToStr (lineNum);
  end;
  lineNumRt := Rect (0, FTxtHeight * (index - FVScrollPos), FLineNumWidth,
              FTxtHeight * (index - FVScrollPos + 1));

  Canvas.Font.Color := clBlack;
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clWhite;
  DrawText (Canvas.Handle, PChar (txtLineNum), Length (txtLineNum),
            lineNumRt, DT_RIGHT or DT_SINGLELINE);
end;

procedure TDiffEdit.PaintContent(index: Integer; pLine: PLineInfo);
var
  contentRt : TRect;
  content : WideString;
  defFontColor : TColor;
  defPenColor : TColor;
  defBrushColor : TColor;
begin
  if (FMemDC = 0) or (FMemBitmap = 0) then
  begin
    exit;
  end;

  if pLine.contentType <> ltDummy then
  begin
    content := ReadLine(pLine);
  end
  else
  begin
    content := '';
  end;

  defFontColor := FMemCanvas.Font.Color;
  defPenColor := FMemCanvas.Pen.Color;
  defBrushColor := FMemCanvas.Brush.Color;

  contentRt := Rect (0,
                      FTxtHeight * (index - FVScrollPos),
                      Width,
                      FTxtHeight * (index - FVScrollPos + 1));

  FMemCanvas.Pen.Color := BackColors[pLine.contentType];
  FMemCanvas.Brush.Color := BackColors[pLine.contentType];
  FMemCanvas.Rectangle(contentRt);

  FMemCanvas.Font.Color := FontColors[pLine.contentType];
  SetBkMode (FMemCanvas.Handle, TRANSPARENT);
  TabbedTextOutW (FMemCanvas.Handle, -FHScrollPos,
          FTxtHeight * (index - FVScrollPos), PWideChar (content),
          Length (content), 1, TabWidth, -FHScrollPos);
  SetBkMode (FMemCanvas.Handle, OPAQUE);

  FMemCanvas.Font.Color := defFontColor;
  FMemCanvas.Pen.Color := defPenColor;
  FMemCanvas.Brush.Color := defBrushColor;
end;

procedure TDiffEdit.PaintCurrentPosMark;
var
  rectTop : Integer;
  defPenColor : TColor;
  defBrushColor : TColor;
  caretLine : Integer;
  visibleLineCnt : Integer;
begin
  if not Focused then
  begin
    exit;
  end;

  if FCaretPos.linePos < 1 then
  begin
    exit;
  end;

  visibleLineCnt := Height div FTxtHeight;
  if (FCaretPos.linePos - 1 < FVScrollPos) or
     (FCaretPos.linePos > FVScrollPos + visibleLineCnt) then
  begin
    exit;
  end;
  
  caretLine := FCaretPos.linePos - 1;
  defPenColor := Canvas.Pen.Color;
  defBrushColor := Canvas.Brush.Color;
  Canvas.Pen.Color := clRed;
  Canvas.Brush.Color := clRed;
  rectTop := (FTxtHeight * (caretLine - FVScrollPos)) + (FTxtHeight div 2);
  Canvas.Rectangle(FLineNumWidth, rectTop - 2, FLineNumWidth + LineNumContentGap,
            rectTop + 2);
  Canvas.Pen.Color := defPenColor;
  Canvas.Brush.Color := defBrushColor;
end;

function TDiffEdit.FindKeyword(keyword: string; searchOptions : TStringSearchOptions;
  var findInfo : TSelectInfo): Boolean;
var
  i : Integer;
  startIndex : Integer;
  colIndex : Integer;
  findColIndex : Integer;
  curLine : PLineInfo;
  content : WideString;
  tmpSelBegin : TSelectInfo;
  tmpSelEnd : TSelectInfo;
  tmpSel : TSelectInfo;
begin
  Result := False;
  if FLines.FLineData.Count < 1 then
  begin
    exit;
  end;

  if not HasSelText then
  begin
    //If there is no selected text, make the current position of caret as a start position
    startIndex := Max (0, FCaretPos.linePos - 1);
    colIndex := Max (1, FCaretPos.charPos);
  end
  else
  begin
    //Makes tmpSelBegin smaller than tmpSelEnd
    tmpSelBegin := FSelBegin;
    tmpSelEnd := FSelEnd;
    if (tmpSelBegin.linePos > tmpSelEnd.linePos) or
       ((tmpSelBegin.linePos = tmpSelEnd.linePos) and
       (tmpSelBegin.charPos > tmpSelEnd.charPos)) then
    begin
      tmpSel := tmpSelBegin;
      tmpSelBegin := tmpSelEnd;
      tmpSelEnd := tmpSel;
    end;

    if soDown in searchOptions then
    begin
      //If downward search, makes the end of selected area as a start possition
      startIndex := Max (0, tmpSelEnd.linePos - 1);
      colIndex := Max (1, tmpSelEnd.charPos);
    end
    else
    begin
      //If upward search, makes the beginning of selected area as a start possition
      startIndex := Max (0, tmpSelBegin.linePos - 1);
      colIndex := Max (1, tmpSelBegin.charPos);
    end;
  end;


  findColIndex := -1;

  if not (soDown in searchOptions) then
  begin
    for i := startIndex downto 0 do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      findColIndex := UltraPos (content, keyword, colIndex, searchOptions);
      if findColIndex > 0 then
      begin
        findInfo.linePos := i + 1;
        findInfo.charPos := findColIndex - 1;
        Result := True;
        exit;
      end;

      if i > 0 then
      begin
        curLine := FLines.FLineData[i - 1];
        content := ReadLine(curLine);
        colIndex := Length(string(content));
      end;
    end;

    //If nothing found from the current position to the top, try to search from the end
    for i := FLines.FLineData.Count - 1 downto startIndex do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      findColIndex := UltraPos (content, keyword, colIndex, searchOptions);
      if findColIndex > 0 then
      begin
        findInfo.linePos := i + 1;
        findInfo.charPos := findColIndex - 1;
        Result := True;
        exit;
      end;

      if i > 0 then
      begin
        curLine := FLines.FLineData[i - 1];
        content := ReadLine(curLine);
        colIndex := Length(string(content));
      end;
    end;
  end
  else
  begin
    for i := startIndex to FLines.FLineData.Count - 1 do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      findColIndex := UltraPos (content, keyword, colIndex, searchOptions);
      if findColIndex > 0 then
      begin
        findInfo.linePos := i + 1;
        findInfo.charPos := findColIndex - 1;
        Result := True;
        exit;
      end;
      colIndex := 1;
    end;

    //If nothing found from the current position to the end, try to search from the beginning
    for i := 0 to startIndex do
    begin
      curLine := FLines.FLineData[i];
      content := ReadLine(curLine);
      findColIndex := UltraPos (content, keyword, colIndex, searchOptions);
      if findColIndex > 0 then
      begin
        findInfo.linePos := i + 1;
        findInfo.charPos := findColIndex - 1;
        Result := True;
        exit;
      end;
      colIndex := 1;
    end;
  end;
end;

procedure TDiffEdit.SetTextSelectInLine(lineNum, colNum, length: Integer);
begin
  FSelBegin.linePos := lineNum + 1;
  FSelBegin.charPos := colNum;
  FSelEnd.linePos := lineNum + 1;;
  FSelEnd.charPos := colNum + length;

  if Assigned (FOnTextSelected) then
  begin
    FOnTextSelected (Self, True);
  end;
end;

procedure TDiffEdit.SetFileInfo(fileInfo: TTextFileStream);
begin
  FFileInfo := fileInfo;
end;

function TDiffEdit.ReadLine(pLine: PLineInfo): string;
begin
  Result := '';
  if pLine.contentType <> ltDummy then
  begin
    FFileInfo.ReadLn(pLine.lineFilePointer, Result);
  end;
end;

procedure TDiffEdit.SetHScrollPos (scrollPos : Integer);
begin
  if scrollPos < 0 then
  begin
    scrollPos := 0;
  end
  else if scrollPos > MaxHScroll then
  begin
    scrollPos := MaxHScroll;
  end;

  FHScrollPos := scrollPos;
  SetScrollPos (Handle, SB_HORZ, FHScrollPos, True);
  UpdateCaret;
  Invalidate;
end;

procedure TDiffEdit.SetVScrollPos(scrollPos: Integer);
begin
  if scrollPos < 0 then
  begin
    scrollPos := 0;
  end
  else if scrollPos > FLines.FLineData.Count then
  begin
    scrollPos := FLines.FLineData.Count - 1;
  end;

  FVScrollPos := scrollPos;
  SetScrollPos (Handle, SB_VERT, FVScrollPos, True);
  UpdateCaret;
  Invalidate;
end;

procedure TDiffEdit.SetMemDC;
begin
  //Create partial memory dc of text data
  if FMemBitmap <> 0 then
  begin
    DeleteObject (FMemBitmap);
    FMemBitmap := 0;
  end;

  if FMemDC <> 0 then
  begin
    DeleteDC (FMemDC);
    FMemDC := 0;
  end;

  FMemDC := CreateCompatibleDC (Canvas.Handle);
  FMemBitmap := CreateCompatibleBitmap (Canvas.Handle,
        Width - FLineNumWidth - LineNumContentGap, Height);
  SelectObject (FMemDC, FMemBitmap);
  FMemCanvas.Handle := FMemDC;
  FMemCanvas.Font := Canvas.Font;
end;

procedure TDiffEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  Message.Result := 1;
end;

procedure TDiffEdit.CNKeyUp(var Message: TWMKey);
begin
  if not (csDesigning in ComponentState) then
  begin
    with Message do
      case CharCode of
      VK_TAB, VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
      VK_RETURN, VK_EXECUTE, VK_ESCAPE, VK_CANCEL:
      begin
        if Perform(CM_WANTSPECIALKEY, CharCode, 0) = 0 then
        begin
          Result := 1;
        end
        else
        begin
          Result := 0;
        end;
      end;
    end;
  end;
end;

function TDiffEdit.HasSelText: Boolean;
begin
  Result := not ( ((FSelBegin.linePos = FSelEnd.linePos) and
    (FSelBegin.charPos = FSelEnd.charPos)) or
    (FSelBegin.linePos = 0) or (FSelEnd.linePos = 0) );
end;

function TDiffEdit.GetColumnEndPos (lineNum : Integer) : Integer;
var
  curLine : PLineInfo;
  content : WideString;
begin
  curLine := FLines.FLineData[lineNum];
  content := ReadLine (curLine);
  Result := Length(content);
end;

{ TDiffText }

procedure TDiffText.BeginUpdate;
begin
  FCompare1.BeginUpdate;
  FCompare2.BeginUpdate;
end;

procedure TDiffText.ClearCompareResult;
begin
  FCompareResult.Clear;
end;

procedure TDiffText.Clear;
begin
  ClearCompareResult;
  FCompare1Count := 0;
  FCompare2Count := 0;
  FCompare1.Clear;
  FCompare2.Clear;
end;

procedure TDiffText.CopyToClipboard;
begin
  if FCompare1.Focused then
  begin
    FCompare1.CopyToClipboard;
  end
  else if FCompare2.Focused then
  begin
    FCompare2.CopyToClipboard;
  end;
end;

constructor TDiffText.Create(AOwner: TComponent);
begin
  inherited Create (AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse,csClickEvents,csSetCaption..csDoubleClicks,csNeedsBorderPaint];
  TabStop := True;

  FLeftPane := TPanel.Create(Self);
  FRightPane := TPanel.Create(Self);

  FLeftTopPane := TPaintBox.Create(Self);
  FRightTopPane := TPaintBox.Create(Self);

  FCompare1 := TDiffEdit.Create (Self);
  FCompare2 := TDiffEdit.Create (Self);

  FCompareResult := TList.Create;

  FSplitter := TSplitter.Create(Self);
end;

destructor TDiffText.Destroy;
begin
  Free;
  inherited;
end;

procedure TDiffText.EndUpdate;
begin
  FCompare1.EndUpdate;
  FCompare2.EndUpdate;
end;

procedure TDiffText.Free;
begin
  if FCompare1 <> nil then
  begin
    FCompare1.Free;
    FCompare1 := nil;
  end;

  if FCompare2 <> nil then
  begin
    FCompare2.Free;
    FCompare2 := nil;
  end;

  if FSplitter <> nil then
  begin
    FSplitter.Free;
    FSplitter := nil;
  end;

  if FLeftTopPane <> nil then
  begin
    FLeftTopPane.Free;
    FLeftTopPane := nil;
  end;

  if FRightTopPane <> nil then
  begin
    FRightTopPane.Free;
    FRightTopPane := nil;
  end;

  if FLeftPane <> nil then
  begin
    FLeftPane.Free;
    FLeftPane := nil;
  end;

  if FRightPane <> nil then
  begin
    FRightPane.Free;
    FRightPane := nil;
  end;
end;

procedure TDiffText.SetDifferentInfo(diffIndex : Integer);
begin
  FCompareResult.Add(Pointer(diffIndex));
end;

procedure TDiffText.SetLineText(paneIndex : Integer; filePos : DWord;
  contentType: TLineType; hasLineNum: Boolean);
begin
  case paneIndex of
    0 :
    begin
      FCompare1.SetLineText(filePos, contentType, hasLineNUm);
      Inc (FCompare1Count);
    end;

    1 :
    begin
      FCompare2.SetLineText(filePos, contentType, hasLineNUm);
      Inc (FCompare2Count);
    end;
  end;
end;

procedure TDiffText.ClearBookmark;
begin
  if FCompare1.Focused then
  begin
    FCompare1.ClearBookmark;
  end
  else if FCompare2.Focused then
  begin
    FCompare2.ClearBookmark;
  end;
end;

procedure TDiffText.ToggleBookmark;
begin
  if FCompare1.Focused then
  begin
    FCompare1.ToggleBookmark;
  end
  else if FCompare2.Focused then
  begin
    FCompare2.ToggleBookmark;
  end;
end;

procedure TDiffText.MoveNextBookmark;
begin
  if FCompare1.Focused then
  begin
    FCompare1.MoveNextBookmark;
  end
  else if FCompare2.Focused then
  begin
    FCompare2.MoveNextBookmark;
  end;
end;

procedure TDiffText.MovePrevBookmark;
begin
  if FCompare1.Focused then
  begin
    FCompare1.MovePrevBookmark;
  end
  else if FCompare2.Focused then
  begin
    FCompare2.MovePrevBookmark;
  end;
end;

procedure TDiffText.MoveNextDifferent;
var
  curLineNum : Integer;
  nextDiffLine : Integer;
begin
  curLineNum := -1;
  if FCompare1.Focused then
  begin
    curLineNum := FCompare1.FCaretPos.linePos - 1;
  end
  else if FCompare2.Focused then
  begin
    curLineNum := FCompare2.FCaretPos.linePos - 1;
  end
  else
  begin
    exit;
  end;

  nextDiffLine := GetNextDiff (curLineNum);
  if nextDiffLine > 0 then
  begin
    FCompare1.MoveTo(nextDiffLine, 0);
    FCompare2.MoveTo(nextDiffLine, 0); 
  end;
end;

procedure TDiffText.MovePrevDifferent;
var
  curLineNum : Integer;
  prevDiffLine : Integer;
begin
  curLineNum := -1;
  if FCompare1.Focused then
  begin
    curLineNum := FCompare1.FCaretPos.linePos - 1;
  end
  else if FCompare2.Focused then
  begin
    curLineNum := FCompare2.FCaretPos.linePos - 1;
  end
  else
  begin
    exit;
  end;

  prevDiffLine := GetPrevDiff (curLineNum);
  if prevDiffLine > -1 then
  begin
    FCompare1.MoveTo(prevDiffLine, 0);
    FCompare2.MoveTo(prevDiffLine, 0);
  end;
end;

procedure TDiffText.OnDiffEditCaretMove(Sender: TObject; lineNum,
  colNum: Integer);
begin
  if Assigned (FOnCaretMove) then
  begin
    FOnCaretMove (Sender, lineNum, colNum, (GetPrevDiff(lineNum-1) <> -1),
      (GetNextDiff(lineNum-1) <> -1));
  end;
end;

function TDiffText.GetNextDiff(lineIndex: Integer): Integer;
var
  i : Integer;
  diffIndex : Integer;
begin
  Result := -1;
  for i := 0 to FCompareResult.Count - 1 do
  begin
    diffIndex := Integer (FCompareResult[i]);
    if diffIndex > lineIndex then
    begin
      Result := diffIndex;
      exit;
    end;
  end;
end;

function TDiffText.GetPrevDiff(lineIndex: Integer): Integer;
var
  i : Integer;
  diffIndex : Integer;
begin
  Result := -1;
  for i := FCompareResult.Count - 1 downto 0 do
  begin
    diffIndex := Integer (FCompareResult[i]);
    if diffIndex < lineIndex then
    begin
      Result := diffIndex;
      exit;
    end;
  end;
end;

function TDiffText.HasSelText: Boolean;
begin
  Result := False;
  if FCompare1.Focused then
  begin
    Result := (FCompare1.GetSelText <> '');
  end
  else if FCompare2.Focused then
  begin
    Result := (FCompare2.GetSelText <> '');
  end;
end;

function TDiffText.GetBookmarkCount: Integer;
begin
  Result := 0;
  if FCompare1.Focused then
  begin
    Result := FCompare1.GetBookmarkCount;
  end
  else if FCompare2.Focused then
  begin
    Result := FCompare2.GetBookmarkCount;
  end;
end;

procedure TDiffText.OnDiffEditFocuseChanged(Sender: TObject; lineNum,
  colNum: Integer);
begin
  if Assigned (FOnPaneChanged) then
  begin
    FOnPaneChanged (Self, lineNum, colNum);
  end;
end;

procedure TDiffText.WMCreate(var Msg: TWMCreate);
begin
  DoubleBuffered := True;

  FLeftPane.Parent := Self;
  FLeftPane.BorderStyle := bsNone;
  FLeftPane.Left := 0;
  FLeftPane.Top := 0;
  FLeftPane.Width := Width div 2;
  FLeftPane.Height := Height;
  FLeftPane.Align := alLeft;
  FLeftPane.BevelOuter := bvNone;
  FLeftPane.Visible := True;
  FLeftPane.ParentFont := True;

  FRightPane.Parent := Self;
  FRightPane.BorderStyle := bsNone;
  FRightPane.Left := FCompare1.Left + FCompare1.Width;
  FRightPane.Top := 0;
  FRightPane.Width := Width div 2;
  FRightPane.Height := Height;
  FRightPane.Align := alClient;
  FRightPane.BevelOuter := bvNone;
  FRightPane.Visible := True;
  FRightPane.ParentFont := True;

  FLeftTopPane.Parent := FLeftPane;
  FLeftTopPane.Left := 0;
  FLeftTopPane.Top := 0;
  FLeftTopPane.Width := Width div 2;
  FLeftTopPane.Height := 25;
  FLeftTopPane.Align := alTop;
  FLeftTopPane.Visible := True;
  FLeftTopPane.ParentFont := True;
  FLeftTopPane.Canvas.Font := FLeftTopPane.Font;

  FRightTopPane.Parent := FRightPane;
  FRightTopPane.Left := 0;
  FRightTopPane.Top := 0;
  FRightTopPane.Width := Width div 2;
  FRightTopPane.Height := 25;
  FRightTopPane.Align := alTop;
  FRightTopPane.Visible := True;
  FRightTopPane.ParentFont := True;
  FRightTopPane.Canvas.Font := FRightTopPane.Font;

  FCompare1.Parent := FLeftPane;
  FCompare1.OnCaretMove := OnDiffEditCaretMove;
  FCompare1.OnPaneFocused := OnDiffEditFocuseChanged;
  FCompare1.OnTextSelected := OnDiffEditTextSelected;
  FCompare1.OnHScroll := OnDiffEditHScroll;
  FCompare1.OnVScroll := OnDiffEditVScroll;
  FCompare1.Align := alClient;
  FCompare1.Visible := True;
  FCompare1Count := 0;

  FCompare2.Parent := FRightPane;
  FCompare2.OnCaretMove := OnDiffEditCaretMove;
  FCompare2.OnPaneFocused := OnDiffEditFocuseChanged;
  FCompare2.OnTextSelected := OnDiffEditTextSelected;
  FCompare2.OnHScroll := OnDiffEditHScroll;
  FCompare2.OnVScroll := OnDiffEditVScroll;
  FCompare2.Align := alClient;
  FCompare2.Visible := True;
  FCompare2Count := 0;

  FSplitter.Parent := Self;
  FSplitter.Left := FLeftPane.Width + 10;
  FSplitter.Align := alLeft;
end;

function TDiffText.FindKeyword(keyword: string;
  searchOptions: TStringSearchOptions): Boolean;
var
  findInfo : TSelectInfo;
begin
  Result := False;
  if FCompare1.Focused then
  begin
    Result := FCompare1.FindKeyword (keyword, searchOptions, findInfo);
    if Result then
    begin
      //Put the caret on after the found keyword
      FCompare1.MoveTo(findInfo.linePos - 1, findInfo.charPos + Length(Keyword));
      //Make the found keyword selected
      FCompare1.SetTextSelectInLine(findInfo.linePos - 1, findInfo.charPos, Length(Keyword));
    end;
  end
  else if FCompare2.Focused then
  begin
    Result := FCompare2.FindKeyword (keyword, searchOptions, findInfo);
    if Result then
    begin
      FCompare2.MoveTo(findInfo.linePos - 1, findInfo.charPos + Length(Keyword));
      FCompare2.SetTextSelectInLine(findInfo.linePos - 1, findInfo.charPos, Length(Keyword));
    end;
  end;
end;

procedure TDiffText.OnDiffEditTextSelected(Sender: TObject;
  isSelected: Boolean);
begin
  if Assigned (FOnTextSelected) then
  begin
    FOnTextSelected (Sender, isSelected);
  end;
end;

procedure TDiffText.Paint;
var
  txtRt : TRect;
  dispTxt : string;
begin
  inherited;

  SetBkMode (FLeftTopPane.Canvas.Handle, TRANSPARENT);
  txtRt := Rect (0, 0, FLeftTopPane.Width, FLeftTopPane.Height);
  dispTxt := MakeShortText (FLeftTopPane.Canvas, FFileName1, FLeftTopPane.Width);
  DrawText (FLeftTopPane.Canvas.Handle, PAnsiChar(dispTxt), Length(dispTxt),
    txtRt, DT_SINGLELINE or DT_VCENTER);
  SetBkMode (FLeftTopPane.Canvas.Handle, OPAQUE);

  SetBkMode (FRightTopPane.Canvas.Handle, TRANSPARENT);
  txtRt := Rect (0, 0, FRightTopPane.Width, FRightTopPane.Height);
  dispTxt := MakeShortText (FRightTopPane.Canvas, FFileName2, FRightTopPane.Width);
  DrawText (FRightTopPane.Canvas.Handle, PAnsiChar(dispTxt), Length(dispTxt),
    txtRt, DT_SINGLELINE or DT_VCENTER);
  SetBkMode (FRightTopPane.Canvas.Handle, OPAQUE);
end;

procedure TDiffText.SetFileNames(fileName1, fileName2: string);
var
  txtRt : TRect;
begin
  FFileName1 := filename1;
  FLeftTopPane.Hint := FFileName1;
  FLeftTopPane.ShowHint := True;
  FFileName2 := filename2;
  FRightTopPane.Hint := FFileName2;
  FRightTopPane.ShowHint := True;

  txtRt := Rect (0, 0, Width, 25);
  InvalidateRect (Handle, @txtRt, True);
end;

procedure TDiffText.SetFileInfos(file1, file2: TTextFileStream);
var
  txtRt : TRect;
begin
  FFileName1 := file1.FileName;
  FLeftTopPane.Hint := FFileName1;
  FLeftTopPane.ShowHint := True;
  FFileName2 := file2.FileName;
  FRightTopPane.Hint := FFileName2;
  FRightTopPane.ShowHint := True;

  txtRt := Rect (0, 0, Width, 25);
  InvalidateRect (Handle, @txtRt, True);

  FCompare1.SetFileInfo(file1);
  FCompare2.SetFileInfo(file2);
end;

procedure TDiffText.OnDiffEditHScroll(Sender: TObject; scrollPos: Integer);
begin
  if Sender = FCompare1 then
  begin
    FCompare2.SetHScrollPos(scrollPos);
  end
  else
  begin
    FCompare1.SetHScrollPos(scrollPos);
  end;
end;

procedure TDiffText.OnDiffEditVScroll(Sender: TObject; scrollPos: Integer);
begin
  if Sender = FCompare1 then
  begin
    FCompare2.SetVScrollPos(scrollPos);
  end
  else
  begin
    FCompare1.SetVScrollPos(scrollPos);
  end;
end;

end.
