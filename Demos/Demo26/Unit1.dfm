object Form1: TForm1
  Left = 214
  Top = 174
  Width = 592
  Height = 422
  VertScrollBar.Range = 210
  ActiveControl = Memo1
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 169
    Width = 576
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Memo1: TMemo
    Left = 0
    Top = 172
    Width = 576
    Height = 170
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      'import spam'
      ''
      'class MyPoint(spam.Point):'
      '  def Foo(Self, v):'
      '    Self.OffsetBy(v, v)'
      ''
      '# old way to create a type instance'
      'p = spam.CreatePoint(2, 5)'
      'print (p, type(p))'
      'p.OffsetBy( 3, 3 )'
      'print (p.x, p.y)'
      'print ("Name =", p.Name)'
      'p.Name = '#39'Hello world!'#39
      'print ("Name =", p.Name)'
      ''
      '# new way to create a type instance'
      'p = spam.Point(2, 5) # no need to use CreateXXX anymore'
      'print (p, type(p))'
      'p.OffsetBy( 3, 3 )'
      'print (p.x, p.y)'
      ''
      '# create a subtype instance'
      'p = MyPoint(2, 5)'
      'print (p, type(p))'
      'p.OffsetBy( 3, 3 )'
      'print (p.x, p.y)'
      'p.Foo( 4 )'
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
      
        'except spam.PointError as what: # this shows you that you can in' +
        'tercept a parent class'
      '  print ("Caught an error derived from PointError")'
      
        '  print ("Error class = ", what.__class__, "     a =", what.a, "' +
        '   b =", what.b, "   c =", what.c)'
      ''
      '# You can raise errors from a Python script too!'
      
        'print ("--------------------------------------------------------' +
        '----------")'
      'print ("Errors in a Python script")'
      'try:'
      '  raise spam.EBadPoint("this is a test!")'
      'except:'
      '  pass'
      ''
      'try:'
      '  err = spam.EBadPoint()'
      '  err.a = 1'
      '  err.b = 2'
      '  err.c = 3'
      '  raise err'
      
        'except spam.PointError as what: # this shows you that you can in' +
        'tercept a parent class'
      '  print ("Caught an error derived from PointError")'
      
        '  print ("Error class = ", what.__class__, "     a =", what.a, "' +
        '   b =", what.b, "   c =", what.c)'
      ''
      'if p == spam.CreatePoint(2, 5):'
      '  print ("Equal")'
      'else:'
      '  print ("Not equal")')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 342
    Width = 576
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Execute'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object Memo2: TMemo
    Left = 0
    Top = 0
    Width = 576
    Height = 169
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 16
    Top = 16
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
    Left = 56
    Top = 16
  end
  object PythonType1: TPythonType
    Engine = PythonEngine1
    OnInitialization = PythonType1Initialization
    TypeName = 'Point'
    Prefix = 'Create'
    Module = PythonModule1
    Services.Basic = [bsRepr, bsStr, bsGetAttrO, bsSetAttrO]
    Services.InplaceNumber = []
    Services.Number = []
    Services.Sequence = []
    Services.Mapping = []
    Left = 96
    Top = 16
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 136
    Top = 16
  end
end
