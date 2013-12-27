unit UFileCompare;

interface

uses Windows, SysUtils, Classes, Math, StrUtils, Dialogs;

type
  THashLine = record
    lineCRC : DWord;
    linePos : DWord;
  end;
  PHashLine = ^THashLine;

  TCompareLine = record
    srcLineNum : Integer;
    dstLineNum : Integer;
    sameLen : Integer;
  end;
  PCompareLine = ^TCompareLine;

  PPoint = ^TPoint;

  TFileCompare = class
  private
    FHashLineList1 : TList;
    FHashLineList2 : TList;
    FCompareResult : TList;

  protected
    procedure AddLineData (list : TList; content : string; linePos : DWord);
    procedure CompareLMS (srcStart, srcEnd, dstStart, dstEnd : Integer);
    procedure ClearCompareResult;
    function GetCompareLine (srcStart, srcEnd, dstStart,
        dstEnd : Integer; var outMatchLen : Integer) : TCompareLine;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Free;

    procedure SetLine (listIdx : Integer; content : string; linePos : DWord);
    procedure Compare;
    function GetResultCount : Integer;
    function GetResult (index : Integer) : PCompareLine;
    function GetLinePos (listIdx, itemIndex : Integer) : DWord;
  end;

implementation

uses UCRC32;


{ TFileCompare }

procedure TFileCompare.AddLineData(list: TList; content: string; linePos : DWord);
var
  newLine : PHashLine;
begin
  New (newLine);
  newLine.lineCRC := GetStringCheckSum (content);
  newLine.linePos := linePos;
  list.Add (newLine);
end;

procedure TFileCompare.ClearCompareResult;
var
  i : Integer;
begin
  for i := 0 to FCompareResult.Count - 1 do
  begin
    Dispose (FCompareResult[i]);
  end;
  FCompareResult.Clear;
end;

constructor TFileCompare.Create;
begin
  FHashLineList1 := TList.Create;
  FHashLineList2 := TList.Create;
  FCompareResult := TList.Create;
end;

destructor TFileCompare.Destroy;
begin
  Free;
  inherited;
end;

procedure TFileCompare.Free;
var
  i : Integer;
begin
  if FHashLineList1 <> nil then
  begin
    for i := 0 to FHashLineList1.Count - 1 do
    begin
      Dispose (FHashLineList1[i]);
    end;

    FHashLineList1.Free;
    FHashLineList1 := nil;
  end;

  if FHashLineList2 <> nil then
  begin
    for i := 0 to FHashLineList2.Count - 1 do
    begin
      Dispose (FHashLineList2[i]);
    end;

    FHashLineList2.Free;
    FHashLineList2 := nil;
  end;

  if FCompareResult <> nil then
  begin
    ClearCompareResult;
    FCompareResult.Free;
    FCompareResult := nil;
  end;
end;

procedure TFileCompare.SetLine(listIdx: Integer; content: string; linePos : DWord);
begin
  case listIdx of
    0 : AddLineData (FHashLineList1, content, linePos);
    1 : AddLineData (FHashLineList2, content, linePos);
  end;
end;

procedure TFileCompare.Compare;
var
  i, j : Integer;
  prior, cur : PCompareLine;
  tmp : TCompareLine;
begin
  CompareLMS (0, FHashLineList1.Count - 1, 0, FHashLineList2.Count - 1);

  for i := 0 to FCompareResult.Count - 2 do
  begin
    prior := FCompareResult[i];
    for j := i + 1 to FCompareResult.Count - 1 do
    begin
      cur := FCompareResult[j];

      if prior.srcLineNum > cur.srcLineNum then
      begin
        tmp := prior^;
        prior^ := cur^;
        cur^ := tmp;
      end;
    end;
  end;
end;

function TFileCompare.GetResultCount: Integer;
begin
  Result := FCompareResult.Count;
end;

function TFileCompare.GetResult(index: Integer): PCompareLine;
begin
  Result := nil;
  if (index < 0) or (index >= FCompareResult.Count) then
  begin
    exit;
  end;
  Result := FCompareResult[index];
end;

procedure TFileCompare.CompareLMS (srcStart, srcEnd, dstStart, dstEnd : Integer);
var
  curLine1 : PHashLine;
  curLine2 : PHashLine;
  compareInfo : PCompareLine;
  curCompInfo : TCompareLine;
  compValue : DWord;
  possSrcLen, possDstLen : Integer;
  maxLen : Integer;
  curMatchLen : Integer;
  i : Integer;
  bestLen : Integer;
  bestDstIndex : Integer;
  bestSrcIndex : Integer;

  upperDstStart : Integer;
  upperSrcStart : Integer;
  srcIndex : Integer;
  dstIndex : Integer;
begin
  bestLen := -1;
  curMatchLen := 0;
  bestSrcIndex := -1;
  bestDstIndex := -1;
  srcIndex := srcStart;
  dstIndex := dstStart;

  while dstIndex < dstEnd + 1 do
  begin
    possDstLen := dstEnd - dstIndex + 1;
    if possDstLen < bestLen then
    begin
      break;
    end;

    curCompInfo := GetCompareLine (srcStart, srcEnd, dstIndex, dstEnd, curMatchLen);
    if (curCompInfo.srcLineNum <> -1) and (curCompInfo.dstLineNum <> -1) then
    begin
      if curMatchLen > bestLen then
      begin
        bestSrcIndex := curCompInfo.srcLineNum;
        bestDstIndex := dstIndex;
        bestLen := curMatchLen;
      end;
      dstIndex := dstIndex + curMatchLen;
    end
    else
    begin
      Inc(dstIndex);
    end;
  end;// end while dstIndex

  if bestDstIndex < 0 then
  begin
    exit;
  end
  else
  begin
    srcIndex := bestSrcIndex;
    New(compareInfo);
    compareInfo.srcLineNum := bestSrcIndex;
    compareInfo.dstLineNum := bestDstIndex;
    compareInfo.sameLen := bestLen;
    FCompareResult.Add(compareInfo);

    if dstStart < bestDstIndex then
    begin
      if srcStart < srcIndex then
      begin
        CompareLMS (srcStart, srcIndex - 1, dstStart, bestDstIndex - 1);
      end;
    end;

    upperDstStart := bestDstIndex + bestLen;
    upperSrcStart := srcIndex + bestLen;
    if dstEnd > upperDstStart then
    begin
      if srcEnd > upperSrcStart then
      begin
        CompareLMS (upperSrcStart, srcEnd, upperDstStart, dstEnd);
      end;
    end;
  end;
end;

function TFileCompare.GetCompareLine(srcStart, srcEnd, dstStart,
  dstEnd : Integer; var outMatchLen : Integer): TCompareLine;
var
  curMatchLen : Integer;
  maxLen : Integer;
  longestLen : Integer;
  maxDstLen : Integer;

  function GetMatchLength (srcIndex, dstIndex, maxLen : Integer) : Integer;
  var
    i : Integer;
  begin
    Result := 0;
    for i := 0 to maxLen - 1 do
    begin
      if PHashLine(FHashLineList1[srcIndex + i]).lineCRC <>
         PHashLine(FHashLineList2[dstIndex + i]).lineCRC then
      begin
        break;
      end;
      Inc(Result);
    end;
  end;
begin
  Result.srcLineNum := -1;
  Result.dstLineNum := -1;

  longestLen := 0;
  curMatchLen := 0;
  maxDstLen := dstEnd - dstStart + 1;

  while srcStart < srcEnd + 1 do
  begin
    maxLen := Min(srcEnd - srcStart + 1,
                  maxDstLen);
    curMatchLen := GetMatchLength (srcStart, dstStart, maxLen);

    if curMatchLen > longestLen then
    begin
      Result.srcLineNum := srcStart;
      Result.dstLineNum := dstStart;
      longestLen := curMatchLen;
      srcStart := srcStart + curMatchLen;
    end
    else
    begin
      Inc(srcStart);
    end;
  end;

  outMatchLen := longestLen;
end;

function TFileCompare.GetLinePos(listIdx, itemIndex: Integer): DWord;
begin
  Result := 0;
  case listIdx of
    0 :
    begin
      if (itemIndex < 0) or (itemIndex >= FHashLineList1.Count) then
      begin
        exit;
      end;
      Result := PHashLine(FHashLineList1[itemIndex]).linePos
    end;

    1 :
    begin
      if (itemIndex < 0) or (itemIndex >= FHashLineList2.Count) then
      begin
        exit;
      end;
      Result := PHashLine(FHashLineList2[itemIndex]).linePos
    end;
  end;
end;

end.
