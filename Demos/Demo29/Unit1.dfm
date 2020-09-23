object Form1: TForm1
  Left = 218
  Top = 18
  Caption = 'Form1'
  ClientHeight = 743
  ClientWidth = 668
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
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
    Left = 8
    Top = 471
    Width = 657
    Height = 330
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
      'def ImageToString(image):'
      '  stream = BytesIO()'
      '  image.save(stream, image.format)'
      '  return stream.getvalue()')
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object Memo2: TMemo
    Left = 8
    Top = 352
    Width = 657
    Height = 113
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
    DllName = 'python36.dll'
    APIVersion = 1013
    RegVersion = '3.6'
    UseLastKnownVersion = False
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
