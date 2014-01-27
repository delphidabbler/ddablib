object VCLDemo1Form: TVCLDemo1Form
  Left = 449
  Top = 177
  Width = 700
  Height = 545
  Caption = 'VCL Demo 1'
  Color = clBtnFace
  Constraints.MinHeight = 545
  Constraints.MinWidth = 700
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  DesignSize = (
    684
    507)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 56
    Width = 30
    Height = 13
    Caption = 'Value:'
  end
  object edName: TEdit
    Left = 8
    Top = 27
    Width = 338
    Height = 21
    TabOrder = 0
  end
  object edValue: TEdit
    Left = 8
    Top = 75
    Width = 338
    Height = 21
    TabOrder = 1
  end
  object lbEnvVars: TListBox
    Left = 360
    Top = 8
    Width = 317
    Height = 491
    TabStop = False
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object btnGetValue: TButton
    Left = 8
    Top = 106
    Width = 80
    Height = 25
    Caption = 'GetValue()'
    TabOrder = 3
    OnClick = btnGetValueClick
  end
  object btnSetValue: TButton
    Left = 94
    Top = 106
    Width = 80
    Height = 25
    Caption = 'SetValue()'
    TabOrder = 4
    OnClick = btnSetValueClick
  end
  object btnDelete: TButton
    Left = 180
    Top = 106
    Width = 80
    Height = 25
    Caption = 'Delete()'
    TabOrder = 5
    OnClick = btnDeleteClick
  end
  object btnExpand: TButton
    Left = 266
    Top = 106
    Width = 80
    Height = 25
    Caption = 'Expand()'
    TabOrder = 6
    OnClick = btnExpandClick
  end
  object btnExists: TButton
    Left = 8
    Top = 137
    Width = 80
    Height = 25
    Caption = 'Exists()'
    TabOrder = 7
    OnClick = btnExistsClick
  end
  object btnCount: TButton
    Left = 94
    Top = 137
    Width = 80
    Height = 25
    Caption = 'Count()'
    TabOrder = 8
    OnClick = btnCountClick
  end
  object btnBlockSize: TButton
    Left = 180
    Top = 137
    Width = 80
    Height = 25
    Caption = 'BlockSize()'
    TabOrder = 9
    OnClick = btnBlockSizeClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 170
    Width = 338
    Height = 141
    TabOrder = 10
    object btnGetAllNames1: TButton
      Left = 11
      Top = 8
      Width = 153
      Height = 25
      Caption = 'GetAllNames(TStrings)'
      TabOrder = 0
      OnClick = btnGetAllNames1Click
    end
    object btnGetAll1: TButton
      Left = 172
      Top = 8
      Width = 153
      Height = 25
      Caption = 'GetAll(TStrings)'
      TabOrder = 1
      OnClick = btnGetAll1Click
    end
    object btnGetAllNames2: TButton
      Left = 11
      Top = 39
      Width = 153
      Height = 25
      Caption = 'GetAllNames: array'
      TabOrder = 2
      OnClick = btnGetAllNames2Click
    end
    object btnGetAll2: TButton
      Left = 172
      Top = 39
      Width = 153
      Height = 25
      Caption = 'GetAll: array'
      TabOrder = 3
      OnClick = btnGetAll2Click
    end
    object btnEnumNames1: TButton
      Left = 11
      Top = 70
      Width = 153
      Height = 25
      Caption = 'EnumNames() - method'
      TabOrder = 4
      OnClick = btnEnumNames1Click
    end
    object btnEnumVars1: TButton
      Left = 172
      Top = 70
      Width = 153
      Height = 25
      Caption = 'EnumVars() - method'
      TabOrder = 5
      OnClick = btnEnumVars1Click
    end
    object btnEnumNames2: TButton
      Left = 11
      Top = 101
      Width = 153
      Height = 25
      Caption = 'EnumNames() - closure'
      TabOrder = 6
      OnClick = btnEnumNames2Click
    end
    object btnEnumVars2: TButton
      Left = 172
      Top = 101
      Width = 153
      Height = 25
      Caption = 'EnumVars() - closure'
      TabOrder = 7
      OnClick = btnEnumVars2Click
    end
  end
  object Panel2: TPanel
    Left = 8
    Top = 320
    Width = 338
    Height = 137
    TabOrder = 11
    object Label3: TLabel
      Left = 11
      Top = 39
      Width = 301
      Height = 13
      Caption = 'New block will include the following new environment variables:'
    end
    object btnCreateBlock: TButton
      Left = 11
      Top = 8
      Width = 113
      Height = 25
      Caption = 'CreateBlock()'
      TabOrder = 0
      OnClick = btnCreateBlockClick
    end
    object lbNewEnv: TListBox
      Left = 24
      Top = 58
      Width = 299
      Height = 47
      ItemHeight = 13
      Items.Strings = (
        'Foo=Lorem'
        'Bar=Ispum'
        'Raboof=Delorum')
      TabOrder = 1
    end
    object chkIncludeCurrent: TCheckBox
      Left = 11
      Top = 111
      Width = 312
      Height = 17
      Caption = 'Include current environment in new block'
      TabOrder = 2
    end
  end
  object btnEnumerator: TButton
    Left = 8
    Top = 468
    Width = 338
    Height = 29
    Caption = 'Enumerate names using TPJEnvVarsEnumerator'
    TabOrder = 12
    OnClick = btnEnumeratorClick
  end
end
