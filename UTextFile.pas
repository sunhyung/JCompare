unit UTextFile;

interface

uses
  Windows, SysUtils, Classes, StrUtils, RTLConsts;

type
  TTextFileStream = class(THandleStream)
  private
    FFileName : string;

  public
    constructor Create(const FileName: string; Mode: Word); overload;
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal); overload;
    destructor Destroy; override;

    procedure ReadLn(linePos : DWord; var outStr : string); overload;
    function ReadLn(var outStr : string): DWord; overload;
    function Eof : Boolean;
    procedure GotoSof;
    function GetNextLinePos : DWord;

  published
    property FileName : string read FFileName;
  end;

implementation

const
  BUF_SIZE = 255;
  
{ TTextFileStream }

constructor TTextFileStream.Create(const FileName: string; Mode: Word);
begin
  Create(Filename, Mode, 0);
end;

constructor TTextFileStream.Create(const FileName: string; Mode: Word;
  Rights: Cardinal);
begin
  if Mode = fmCreate then
  begin
    inherited Create(FileCreate(FileName, Rights));
    if FHandle < 0 then
      raise EFCreateError.CreateResFmt(@SFCreateErrorEx, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  end
  else
  begin
    inherited Create(FileOpen(FileName, Mode));
    if FHandle < 0 then
      raise EFOpenError.CreateResFmt(@SFOpenErrorEx, [ExpandFileName(FileName), SysErrorMessage(GetLastError)]);
  end;
  FFileName := FileName
end;

destructor TTextFileStream.Destroy;
begin
  if FHandle >= 0 then FileClose(FHandle);
  inherited Destroy;
end;

function TTextFileStream.Eof: Boolean;
begin
  Result := (Position = Size);
end;

function TTextFileStream.GetNextLinePos: DWord;
var
  str : string;
begin
  Result := ReadLn(str);
end;

procedure TTextFileStream.GotoSof;
begin
  Position := 0;
end;

function TTextFileStream.ReadLn(var outStr: string): DWord;
var
  buffer : array [0..BUF_SIZE + 1] of AnsiChar;
  readByte : LongInt;
  curPos : Int64;
  lineLen : Int64;
  pScanPos : PAnsiChar;
  scanPos : Integer;
begin
  outStr := '';

  lineLen := 0;
  //Save current position
  curPos := Position;
  while Position < Size do
  begin
    //Read 255 byte into a buffer
    readByte := Read(buffer, BUF_SIZE * SizeOf(AnsiChar));
    buffer[readByte] := AnsiChar(#0);
    pScanPos := StrScan (buffer, AnsiChar(#10));
    if pScanPos <> nil then
    begin
      scanPos := pScanPos - buffer;
      buffer[scanPos] := #0;
      outStr := outStr + buffer;
      Inc(lineLen, scanPos + 1);
      break;
    end
    else
    begin
      outStr := outStr + buffer;
      Inc(lineLen, readByte);
    end;
  end;

  if outStr[Length(outStr)] = #13 then
  begin
    outStr := Copy(outStr, 1, Length(outStr) - 1);
  end;
  Position := curPos + lineLen;
  Result := curPos;
end;

procedure TTextFileStream.ReadLn(linePos: DWord; var outStr: string);
begin
  Position := linePos;
  ReadLn(outStr);
end;

end.

