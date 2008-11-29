object TestForm: TTestForm
  Left = 213
  Top = 114
  Width = 870
  Height = 500
  Caption = 'Test Form'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CheckBox1: TCheckBox
    Left = 96
    Top = 16
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 0
  end
  object CheckBox2: TCheckBox
    Left = 96
    Top = 56
    Width = 97
    Height = 17
    Caption = 'CheckBox2'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
  object btnClose: TButton
    Left = 88
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 2
    OnClick = btnCloseClick
  end
  object Edit1: TEdit
    Left = 96
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 3
    Text = 'Edit1'
  end
  object ListBox1: TListBox
    Left = 328
    Top = 48
    Width = 249
    Height = 169
    ItemHeight = 13
    TabOrder = 4
  end
  object btnAdd: TButton
    Left = 104
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 5
  end
end
