object Form1: TForm1
  Left = 200
  Top = 108
  Caption = 'Example 1 : Using a DelphiVar with a Dictionary'
  ClientHeight = 441
  ClientWidth = 812
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 305
    Top = 0
    Height = 441
    ExplicitHeight = 446
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 305
    Height = 441
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object GroupBox1: TGroupBox
      Left = 16
      Top = 8
      Width = 273
      Height = 377
      Caption = 'Properties'
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 16
        Width = 23
        Height = 13
        Caption = 'Title:'
      end
      object Label2: TLabel
        Left = 16
        Top = 64
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object Label3: TLabel
        Left = 128
        Top = 176
        Width = 22
        Height = 13
        Caption = 'Age:'
      end
      object edName: TEdit
        Left = 16
        Top = 80
        Width = 241
        Height = 21
        TabOrder = 0
        Text = 'Smith'
      end
      object cbInformatician: TCheckBox
        Left = 16
        Top = 120
        Width = 97
        Height = 17
        Caption = 'Informatician?'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object rgSex: TRadioGroup
        Left = 16
        Top = 168
        Width = 97
        Height = 65
        Caption = 'Sex'
        ItemIndex = 0
        Items.Strings = (
          'Male'
          'Female')
        TabOrder = 2
      end
      object cbPythonUser: TCheckBox
        Left = 16
        Top = 136
        Width = 97
        Height = 17
        Caption = 'Python user?'
        TabOrder = 3
      end
      object cbTitle: TComboBox
        Left = 16
        Top = 32
        Width = 241
        Height = 21
        TabOrder = 4
        Text = 'Mr'
        Items.Strings = (
          'Mr'
          'Mrs'
          'Miss'
          'Dr')
      end
      object edAge: TEdit
        Left = 128
        Top = 192
        Width = 121
        Height = 21
        TabOrder = 5
        Text = '35'
      end
    end
    object Button1: TButton
      Left = 88
      Top = 408
      Width = 129
      Height = 25
      Caption = 'Execute Script'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object Panel2: TPanel
    Left = 308
    Top = 0
    Width = 504
    Height = 441
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 153
      Width = 504
      Height = 3
      Cursor = crVSplit
      Align = alTop
      ExplicitWidth = 380
    end
    object Memo1: TMemo
      Left = 0
      Top = 156
      Width = 504
      Height = 285
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      Lines.Strings = (
        '# Define some constants'
        'Male = 0'
        'Female = 1'
        ''
        '# Display the Properties content'
        'print (Properties)'
        '# Get one of the properties'
        'print (Properties.Value['#39'Title'#39'])'
        '# Change one of the properties'
        'Properties.Value['#39'Age'#39'] = 55'
        '# By reassigning the same object, we force the OnChange event'
        '# That will update the Delphi controls'
        'Properties.Value = Properties.Value'
        ''
        '# We can simplify it with a new class'
        'class TProperties:'
        '  def __init__(Self, DelphiVar):'
        '    Self.__DelphiVar__ = DelphiVar'
        ''
        '  def __getattr__(Self, Key):'
        '    return Self.__dict__['#39'__DelphiVar__'#39'].Value[Key]'
        ''
        '  def __setattr__(Self, Key, Value):'
        '    if Key == "__DelphiVar__":'
        '      Self.__dict__['#39'__DelphiVar__'#39'] = Value'
        '    else:'
        '      Self.__DelphiVar__.Value[Key] = Value'
        '      Self.__DelphiVar__.Value = Self.__DelphiVar__.Value'
        ''
        '  def __repr__(Self):'
        '    return str(Self.__DelphiVar__.Value)'
        ''
        '  def __str__(Self):'
        '    return str(Self.__DelphiVar__.Value)'
        ''
        '# Instantiate our new class'
        'Props = TProperties( Properties )'
        '# Use this instance to read/write the properties'
        'print ("Name:", Props.Name)'
        'Props.Name = "Watson"'
        'Props.PythonUser = True'
        'if Props.Sex == Male:'
        '  print (Props.Name, "is male!")'
        'else:'
        '  print (Props.Name, "is female!")'
        'print (Props)')
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Memo2: TMemo
      Left = 0
      Top = 0
      Width = 504
      Height = 153
      Align = alTop
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 8
    Top = 400
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 40
    Top = 400
  end
  object PythonDelphiVar1: TPythonDelphiVar
    Engine = PythonEngine1
    Module = '__main__'
    VarName = 'Properties'
    OnExtGetData = PythonDelphiVar1ExtGetData
    OnExtSetData = PythonDelphiVar1ExtSetData
    OnChange = PythonDelphiVar1Change
    Left = 240
    Top = 400
  end
end
