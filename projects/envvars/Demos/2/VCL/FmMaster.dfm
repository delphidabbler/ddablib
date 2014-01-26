object MasterForm: TMasterForm
  Left = 200
  Top = 118
  Width = 455
  Height = 652
  Caption = 'MasterForm'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 390
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  DesignSize = (
    439
    614)
  PixelsPerInch = 96
  TextHeight = 14
  object lblPrompt: TLabel
    Left = 12
    Top = 16
    Width = 349
    Height = 14
    Caption = 'Enter some environment variables in Name=Value format below'
  end
  object btnExecSlave: TButton
    Left = 11
    Top = 545
    Width = 414
    Height = 60
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Execute slave program'
    TabOrder = 0
    OnClick = btnExecSlaveClick
  end
  object edNewEnvVars: TMemo
    Left = 10
    Top = 40
    Width = 415
    Height = 467
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Foo=Lorem'
      'Bar=Dipsum'
      'Raboof=Delorum')
    TabOrder = 1
  end
  object chkIncludeCurrentBlock: TCheckBox
    Left = 10
    Top = 512
    Width = 415
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Include current environment block'
    TabOrder = 2
  end
end
