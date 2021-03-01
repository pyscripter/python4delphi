object Form1: TForm1
  Left = 193
  Top = 110
  Caption = 'Form1'
  ClientHeight = 336
  ClientWidth = 528
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
    Left = 0
    Top = 145
    Width = 528
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 536
  end
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 528
    Height = 145
    Align = alTop
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Memo2: TMemo
    Left = 0
    Top = 148
    Width = 528
    Height = 147
    Align = alClient
    Lines.Strings = (
      'import spam'
      'p = spam.CreatePoint(2, 5)'
      'print (p)'
      'p.OffsetBy( 3, 3 )'
      'print (p.x, p.y)'
      'print (dir(spam))'
      'print (spam.Point)'
      'print ("p = ", p, "  --> ",)'
      'if type(p) is spam.Point:'
      '  print ("p is a Point")'
      'else:'
      '  print ("p is not a point")'
      'p = 2'
      'print ("p = ", p, "  --> ",)'
      'if type(p) is spam.Point:'
      '  print ("p is a Point")'
      'else:'
      '  print ("p is not a point")'
      'p = spam.CreatePoint(2, 5)'
      'try:'
      '  print ("raising an error of class EBadPoint")'
      '  p.RaiseError() # it will raise spam.EBadPoint'
      
        'except spam.PointError as what: #it shows you that you can inter' +
        'cept a parent class'
      '  print ("Caught an error derived from PointError")'
      
        '  print ("Error class = ", what.__class__, "     a =", what.a, "' +
        '   b =", what.b, "   c =", what.c)'
      ''
      '# You can raise errors from a Python script too!'
      
        'print ("--------------------------------------------------------' +
        '----------")'
      'print ("Errors in a Python script")'
      'try:'
      '  raise spam.EBadPoint("this is a test !")'
      'except:'
      '  pass'
      ''
      'try:'
      '  err = spam.EBadPoint()'
      '  err.a = 1'
      '  err.b = 2'
      '  err.c = 3'
      '  raise err'
      
        'except spam.PointError as what: #it shows you that you can inter' +
        'cept a parent class'
      '  print ("Caught an error derived from PointError")'
      
        '  print ("Error class = ", what.__class__, "     a =", what.a, "' +
        '   b =", what.b, "   c =", what.c)'
      ''
      'if p == spam.CreatePoint(2, 5):'
      '  print ("Equal")'
      'else:'
      '  print ("Not equal")'
      '')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 295
    Width = 528
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 105
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object PythonEngine1: TPythonEngine
    AutoLoad = False
    IO = PythonGUIInputOutput1
    Left = 16
    Top = 32
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo1
    Left = 48
    Top = 32
  end
  object PythonModule1: TPythonModule
    Engine = PythonEngine1
    ModuleName = 'spam'
    Errors = <
      item
        Name = 'PointError'
        ErrorType = etClass
      end
      item
        Name = 'EBadPoint'
        ErrorType = etClass
        ParentClass.Name = 'PointError'
      end>
    Left = 80
    Top = 32
  end
  object PythonType1: TPythonType
    Engine = PythonEngine1
    OnInitialization = PythonType1Initialization
    TypeName = 'Point'
    Prefix = 'Create'
    Module = PythonModule1
    Services.Basic = [bsGetAttr, bsSetAttr, bsRepr, bsStr]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = []
    Left = 112
    Top = 32
  end
end
