object frmProcess: TfrmProcess
  Left = 244
  Top = 219
  Caption = 'Processing...'
  ClientHeight = 98
  ClientWidth = 419
  Color = clBtnFace
  Font.Charset = HANGEUL_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #44404#47548
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 56
    Top = 15
    Width = 257
    Height = 12
    Alignment = taCenter
    Caption = 'Loading and processing the requested files...'
  end
  object Label2: TLabel
    Left = 56
    Top = 33
    Width = 316
    Height = 12
    Alignment = taCenter
    Caption = 'Depends on the file size and your system performance,'
  end
  object Label3: TLabel
    Left = 56
    Top = 51
    Width = 295
    Height = 12
    Alignment = taCenter
    Caption = 'It will take from several seconds to several minutes.'
  end
  object btnCancel: TButton
    Left = 336
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = btnCancelClick
  end
end
