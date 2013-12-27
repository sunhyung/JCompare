object frmFindText: TfrmFindText
  Left = 348
  Top = 259
  BorderStyle = bsDialog
  Caption = 'Find text'
  ClientHeight = 96
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = HANGEUL_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #44404#47548
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 8
    Top = 11
    Width = 50
    Height = 12
    Caption = 'Keyword'
  end
  object eFindKeyword: TEdit
    Left = 61
    Top = 8
    Width = 172
    Height = 20
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 0
  end
  object btnFind: TButton
    Left = 239
    Top = 7
    Width = 71
    Height = 21
    Caption = 'Next'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnFindClick
  end
  object cboxCaseSensitive: TCheckBox
    Left = 8
    Top = 43
    Width = 105
    Height = 17
    Caption = 'Case sensitive'
    TabOrder = 2
  end
  object cboxWholeWord: TCheckBox
    Left = 8
    Top = 67
    Width = 97
    Height = 17
    Caption = 'Whole word'
    TabOrder = 3
  end
  object btnClose: TButton
    Left = 239
    Top = 67
    Width = 71
    Height = 21
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 4
    OnClick = btnCloseClick
  end
  object rgFindDir: TRadioGroup
    Left = 120
    Top = 39
    Width = 105
    Height = 49
    Caption = 'Direction'
    ItemIndex = 1
    Items.Strings = (
      'Up'
      'Down')
    TabOrder = 5
  end
end
