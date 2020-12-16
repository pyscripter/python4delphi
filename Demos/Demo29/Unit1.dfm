object Form1: TForm1
  Left = 218
  Top = 18
  Caption = 'Form1'
  ClientHeight = 701
  ClientWidth = 668
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    668
    701)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 657
    Height = 289
    AutoSize = True
  end
  object Button1: TButton
    Left = 8
    Top = 312
    Width = 97
    Height = 25
    Caption = 'Open Picture...'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 196
    Top = 312
    Width = 105
    Height = 25
    Caption = 'Execute'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 0
    Top = 472
    Width = 668
    Height = 229
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      '#  This demo requires  the module pillow (PIL)'
      
        '#  You can use "pip install Pillow" from a command prompt to ins' +
        'tall Pillow'
      ''
      'from io import BytesIO'
      'from PIL import Image'
      'import sys'
      ''
      'def ProcessImage(data):'
      '  print(sys.version)'
      '  stream = BytesIO(data)'
      '  im = Image.open(stream)'
      
        '  print ("Processing image %s of %d bytes" % (im.format, len(dat' +
        'a)))'
      '  new_im = im.rotate(90, expand=True)'
      '  new_im.format = im.format'
      '  return new_im'
      '  '
      'def ImageToBytes(image):'
      '  stream = BytesIO()'
      '  image.save(stream, image.format)'
      '  return stream.getvalue()')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 8
    Top = 352
    Width = 657
    Height = 114
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 3
  end
  object chkUseDC: TCheckBox
    Left = 336
    Top = 316
    Width = 193
    Height = 17
    Caption = 'Use Device Context'
    TabOrder = 4
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 104
    Top = 336
  end
  object PythonEngine1: TPythonEngine
    AutoUnload = False
    IO = PythonGUIInputOutput1
    Left = 168
    Top = 624
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 208
    Top = 624
  end
end
