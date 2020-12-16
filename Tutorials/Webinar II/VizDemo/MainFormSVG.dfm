object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Python Chats to Svg'
  ClientHeight = 613
  ClientWidth = 1035
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 489
    Top = 0
    Height = 613
    ExplicitLeft = 528
    ExplicitTop = 280
    ExplicitHeight = 100
  end
  object SVGIconImage1: TSVGIconImage
    Left = 960
    Top = 520
    Width = 100
    Height = 41
    AutoSize = False
  end
  object SVGIconImage: TSVGIconImage
    Left = 492
    Top = 0
    Width = 543
    Height = 613
    AutoSize = False
    ParentDoubleBuffered = False
    DoubleBuffered = True
    Align = alClient
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 489
    Height = 613
    ActivePage = TabSheet1
    Align = alLeft
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'matplotlib'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 481
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 1
          Top = 445
          Width = 479
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 10
          ExplicitWidth = 492
        end
        object SynEdit1: TSynEdit
          Left = 1
          Top = 1
          Width = 479
          Height = 444
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Highlighter = SynPythonSyn1
          Lines.Strings = (
            'from delphi_module import svg_image'
            'from io import StringIO'
            'import numpy as np'
            'import matplotlib.pyplot as plt'
            'import matplotlib.cbook as cbook'
            ''
            
              '# Load a numpy record array from yahoo csv data with fields date' +
              ', open, close,'
            
              '# volume, adj_close from the mpl-data/example directory. The rec' +
              'ord array'
            
              '# stores the date as an np.datetime64 with a day unit ('#39'D'#39') in t' +
              'he date column.'
            
              'price_data = (cbook.get_sample_data('#39'goog.npz'#39', np_load=True)['#39'p' +
              'rice_data'#39']'
            '              .view(np.recarray))'
            
              'price_data = price_data[-250:]  # get the most recent 250 tradin' +
              'g days'
            ''
            
              'delta1 = np.diff(price_data.adj_close) / price_data.adj_close[:-' +
              '1]'
            ''
            '# Marker size in units of points^2'
            'volume = (15 * price_data.volume[:-2] / price_data.volume[0])**2'
            
              'close = 0.003 * price_data.close[:-2] / 0.003 * price_data.open[' +
              ':-2]'
            ''
            'fig, ax = plt.subplots()'
            
              'ax.scatter(delta1[:-1], delta1[1:], c=close, s=volume, alpha=0.5' +
              ')'
            ''
            'ax.set_xlabel(r'#39'$\Delta_i$'#39', fontsize=15)'
            'ax.set_ylabel(r'#39'$\Delta_{i+1}$'#39', fontsize=15)'
            'ax.set_title('#39'Volume and percent change'#39')'
            ''
            'ax.grid(True)'
            'fig.tight_layout()'
            ''
            'figfile = StringIO()'
            'fig.savefig(figfile, format='#39'svg'#39')'
            'figdata_svg = figfile.getvalue()'
            'svg_image.SvgText = figdata_svg'
            ''
            '#plt.show()')
        end
        object Panel2: TPanel
          Left = 1
          Top = 543
          Width = 479
          Height = 41
          Align = alBottom
          TabOrder = 1
          object Button1: TButton
            Left = 24
            Top = 6
            Width = 75
            Height = 25
            Caption = 'Run'
            TabOrder = 0
            OnClick = Button1Click
          end
        end
        object Memo1: TMemo
          Left = 1
          Top = 448
          Width = 479
          Height = 95
          Align = alBottom
          TabOrder = 2
        end
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'seaborn'
      ImageIndex = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 481
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter6: TSplitter
          Left = 1
          Top = 377
          Width = 479
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 151
          ExplicitWidth = 433
        end
        object SynEdit2: TSynEdit
          Left = 1
          Top = 1
          Width = 479
          Height = 376
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Consolas'
          Font.Style = []
          Font.Quality = fqClearTypeNatural
          TabOrder = 0
          UseCodeFolding = False
          Gutter.Font.Charset = DEFAULT_CHARSET
          Gutter.Font.Color = clWindowText
          Gutter.Font.Height = -11
          Gutter.Font.Name = 'Consolas'
          Gutter.Font.Style = []
          Highlighter = SynPythonSyn1
          Lines.Strings = (
            'from delphi_module import svg_image'
            'from io import StringIO'
            'import seaborn as sns'
            'import matplotlib.pyplot as plt'
            ''
            'df = sns.load_dataset("penguins")'
            'sns.pairplot(df, hue="species")'
            ''
            'figfile = StringIO()'
            'plt.savefig(figfile, format='#39'svg'#39')'
            'figdata_svg = figfile.getvalue()'
            'svg_image.SvgText = figdata_svg'
            ''
            '#plt.show()'
            '')
        end
        object Panel6: TPanel
          Left = 1
          Top = 543
          Width = 479
          Height = 41
          Align = alBottom
          TabOrder = 1
          object Button3: TButton
            Left = 24
            Top = 6
            Width = 75
            Height = 25
            Caption = 'Run'
            TabOrder = 0
            OnClick = Button1Click
          end
        end
        object Memo2: TMemo
          Left = 1
          Top = 380
          Width = 479
          Height = 163
          Align = alBottom
          TabOrder = 2
        end
      end
    end
  end
  object SynPythonSyn1: TSynPythonSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 632
    Top = 40
  end
  object SynEditPythonBehaviour1: TSynEditPythonBehaviour
    Editor = SynEdit2
    Left = 628
    Top = 89
  end
  object PythonEngine1: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 632
    Top = 136
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Left = 632
    Top = 184
  end
  object PythonModule: TPythonModule
    Engine = PythonEngine1
    ModuleName = 'delphi_module'
    Errors = <>
    Left = 632
    Top = 232
  end
  object PyDelphiWrapper: TPyDelphiWrapper
    Engine = PythonEngine1
    Module = PythonModule
    Left = 632
    Top = 280
  end
end
