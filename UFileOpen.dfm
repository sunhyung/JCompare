object frmFileOpen: TfrmFileOpen
  Left = 408
  Top = 388
  Caption = #54028#51068#50676#44592
  ClientHeight = 106
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = HANGEUL_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #44404#47548
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 12
  object Label1: TLabel
    Left = 16
    Top = 12
    Width = 52
    Height = 12
    Caption = #48708#44368' '#54028#51068
  end
  object Label2: TLabel
    Left = 16
    Top = 35
    Width = 52
    Height = 12
    Caption = #45824#49345' '#54028#51068
  end
  object eFile1: TEdit
    Left = 80
    Top = 8
    Width = 201
    Height = 20
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 0
  end
  object eFile2: TEdit
    Left = 80
    Top = 32
    Width = 201
    Height = 20
    ImeName = 'Microsoft Office IME 2007'
    TabOrder = 1
  end
  object btnOpen1: TButton
    Left = 288
    Top = 8
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 2
    OnClick = btnOpen1Click
  end
  object btnOpen2: TButton
    Left = 288
    Top = 32
    Width = 25
    Height = 21
    Caption = '...'
    TabOrder = 3
    OnClick = btnOpen2Click
  end
  object cboxIgnoreCase: TCheckBox
    Left = 16
    Top = 64
    Width = 153
    Height = 17
    Caption = #45824#49548#47928#51088' '#47924#49884#54616#44256' '#48708#44368
    TabOrder = 4
  end
  object cboxIgnoreWhiteSpace: TCheckBox
    Left = 16
    Top = 88
    Width = 193
    Height = 17
    Caption = #51460' '#45149#51032' '#44277#48177#47928#51088' '#47924#49884#54616#44256' '#48708#44368
    TabOrder = 5
  end
  object btnOk: TButton
    Left = 328
    Top = 8
    Width = 75
    Height = 25
    Caption = #48708#44368#49884#51089
    ModalResult = 1
    TabOrder = 6
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 328
    Top = 40
    Width = 75
    Height = 25
    Caption = #45803#44592
    ModalResult = 2
    TabOrder = 7
    OnClick = btnCancelClick
  end
  object OpenDialog1: TOpenDialog
    Left = 256
    Top = 8
  end
end
