object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Interactive visualizations - Edge browser'
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
  object Splitter3: TSplitter
    Left = 545
    Top = 0
    Height = 613
    ExplicitLeft = 368
    ExplicitTop = 240
    ExplicitHeight = 100
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 545
    Height = 613
    ActivePage = TabSheet2
    Align = alLeft
    TabOrder = 0
    object TabSheet2: TTabSheet
      Caption = 'matplotlib with mpld3'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 537
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter4: TSplitter
          Left = 1
          Top = 413
          Width = 535
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitLeft = 0
          ExplicitTop = 386
          ExplicitWidth = 727
        end
        object SynEdit1: TSynEdit
          Left = 1
          Top = 1
          Width = 535
          Height = 412
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
            'from delphi_module import edge_browser'
            'import matplotlib.pyplot as plt'
            'import numpy as np'
            'import pandas as pd'
            'import mpld3'
            'from mpld3 import plugins'
            'from io import StringIO'
            ''
            'np.random.seed(9615)'
            ''
            '# generate df'
            'N = 100'
            
              'df = pd.DataFrame((.1 * (np.random.random((N, 5)) - .5)).cumsum(' +
              '0),'
            '                  columns=['#39'a'#39', '#39'b'#39', '#39'c'#39', '#39'd'#39', '#39'e'#39'],)'
            ''
            '# plot line + confidence interval'
            'fig, ax = plt.subplots()'
            'ax.grid(True, alpha=0.3)'
            ''
            'for key, val in df.iteritems():'
            '    l, = ax.plot(val.index, val.values, label=key)'
            '    ax.fill_between(val.index,'
            '                    val.values * .5, val.values * 1.5,'
            '                    color=l.get_color(), alpha=.4)'
            ''
            '# define interactive legend'
            ''
            
              'handles, labels = ax.get_legend_handles_labels() # return lines ' +
              'and labels'
            
              'interactive_legend = plugins.InteractiveLegendPlugin(zip(handles' +
              ','
            
              '                                                         ax.coll' +
              'ections),'
            '                                                     labels,'
            
              '                                                     alpha_unsel' +
              '=0.5,'
            
              '                                                     alpha_over=' +
              '1.5,'
            
              '                                                     start_visib' +
              'le=True)'
            'plugins.connect(fig, interactive_legend)'
            ''
            'ax.set_xlabel('#39'x'#39')'
            'ax.set_ylabel('#39'y'#39')'
            'ax.set_title('#39'Interactive legend'#39', size=20)'
            ''
            'figfile = StringIO()'
            'mpld3.save_html(fig, figfile)'
            
              'html = '#39'<html><head><title>mpld3 plot</title></head><body>\n'#39' + ' +
              'figfile.getvalue() + '#39'</body></html>'#39
            'edge_browser.NavigateToString(html)'
            '##mpld3.show()')
        end
        object Panel4: TPanel
          Left = 1
          Top = 543
          Width = 535
          Height = 41
          Align = alBottom
          TabOrder = 1
          object Button2: TButton
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
          Top = 416
          Width = 535
          Height = 127
          Align = alBottom
          TabOrder = 2
        end
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'altair'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 537
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 1
          Top = 421
          Width = 535
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 151
          ExplicitWidth = 433
        end
        object SynEdit2: TSynEdit
          Left = 1
          Top = 1
          Width = 535
          Height = 420
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
            'from delphi_module import edge_browser'
            'from io import StringIO'
            'import altair as alt'
            'from vega_datasets import data'
            ''
            'cars = data.cars.url'
            ''
            '# define selection'
            'click = alt.selection_multi(encodings=['#39'color'#39'])'
            ''
            '# scatter plots of points'
            'scatter = alt.Chart(cars).mark_circle().encode('
            '    x='#39'Horsepower:Q'#39','
            '    y='#39'Miles_per_Gallon:Q'#39','
            '    size=alt.Size('#39'Cylinders:O'#39','
            '        scale=alt.Scale(range=(20,100))'
            '    ),'
            '    color=alt.Color('#39'Origin:N'#39', legend=None),'
            '    tooltip=['#39'Name:N'#39','#39'Horsepower:Q'#39','#39'Miles_per_Gallon:Q'#39','
            '             '#39'Cylinders:O'#39','#39'Origin:N'#39'],'
            ').transform_filter('
            '    click'
            ').interactive()'
            ''
            '# legend'
            'legend = alt.Chart(cars).mark_rect().encode('
            '    y=alt.Y('#39'Origin:N'#39', axis=alt.Axis(title='#39'Select Origin'#39')),'
            '    color=alt.condition(click, '#39'Origin:N'#39', '
            '                        alt.value('#39'lightgray'#39'), legend=None),'
            '    size=alt.value(250)'
            ').properties('
            '    selection=click'
            ')'
            ''
            'chart = (scatter | legend)'
            ''
            'figfile = StringIO()'
            'chart.save(figfile, '#39'html'#39')'
            'edge_browser.NavigateToString(figfile.getvalue())')
        end
        object Panel2: TPanel
          Left = 1
          Top = 543
          Width = 535
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
        object Memo2: TMemo
          Left = 1
          Top = 424
          Width = 535
          Height = 119
          Align = alBottom
          TabOrder = 2
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'bokeh'
      ImageIndex = 3
      object Splitter7: TSplitter
        Left = 534
        Top = 0
        Height = 585
        Align = alRight
        ExplicitLeft = 512
        ExplicitTop = 240
        ExplicitHeight = 100
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 534
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter8: TSplitter
          Left = 1
          Top = 437
          Width = 532
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitTop = 151
          ExplicitWidth = 433
        end
        object SynEdit3: TSynEdit
          Left = 1
          Top = 1
          Width = 532
          Height = 436
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
            'from delphi_module import edge_browser'
            'from bokeh.resources import INLINE'
            'from bokeh.embed import file_html'
            ''
            'import numpy as np'
            ''
            'from bokeh.layouts import column, row'
            'from bokeh.models import CustomJS, Slider'
            
              'from bokeh.plotting import ColumnDataSource, figure, output_file' +
              ', show'
            ''
            'x = np.linspace(0, 10, 500)'
            'y = np.sin(x)'
            ''
            'source = ColumnDataSource(data=dict(x=x, y=y))'
            ''
            
              'plot = figure(y_range=(-10, 10), plot_width=400, plot_height=400' +
              ')'
            ''
            'plot.line('#39'x'#39', '#39'y'#39', source=source, line_width=3, line_alpha=0.6)'
            ''
            
              'amp_slider = Slider(start=0.1, end=10, value=1, step=.1, title="' +
              'Amplitude")'
            
              'freq_slider = Slider(start=0.1, end=10, value=1, step=.1, title=' +
              '"Frequency")'
            
              'phase_slider = Slider(start=0, end=6.4, value=0, step=.1, title=' +
              '"Phase")'
            
              'offset_slider = Slider(start=-5, end=5, value=0, step=.1, title=' +
              '"Offset")'
            ''
            
              'callback = CustomJS(args=dict(source=source, amp=amp_slider, fre' +
              'q=freq_slider, phase=phase_slider, offset=offset_slider),'
            '                    code="""'
            '    const data = source.data;'
            '    const A = amp.value;'
            '    const k = freq.value;'
            '    const phi = phase.value;'
            '    const B = offset.value;'
            '    const x = data['#39'x'#39']'
            '    const y = data['#39'y'#39']'
            '    for (var i = 0; i < x.length; i++) {'
            '        y[i] = B + A*Math.sin(k*x[i]+phi);'
            '    }'
            '    source.change.emit();'
            '""")'
            ''
            'amp_slider.js_on_change('#39'value'#39', callback)'
            'freq_slider.js_on_change('#39'value'#39', callback)'
            'phase_slider.js_on_change('#39'value'#39', callback)'
            'offset_slider.js_on_change('#39'value'#39', callback)'
            ''
            'layout = row('
            '    plot,'
            
              '    column(amp_slider, freq_slider, phase_slider, offset_slider)' +
              ','
            ')'
            ''
            'html = file_html(layout, INLINE, "my plot")'
            'edge_browser.NavigateToString(html)')
        end
        object Panel8: TPanel
          Left = 1
          Top = 543
          Width = 532
          Height = 41
          Align = alBottom
          TabOrder = 1
          object Button4: TButton
            Left = 24
            Top = 6
            Width = 75
            Height = 25
            Caption = 'Run'
            TabOrder = 0
            OnClick = Button1Click
          end
        end
        object Memo3: TMemo
          Left = 1
          Top = 440
          Width = 532
          Height = 103
          Align = alBottom
          TabOrder = 2
        end
      end
    end
  end
  object Panel5: TPanel
    Left = 548
    Top = 0
    Width = 487
    Height = 613
    Align = alClient
    BevelInner = bvLowered
    BevelKind = bkFlat
    TabOrder = 1
    object EdgeBrowser: TEdgeBrowser
      Left = 2
      Top = 2
      Width = 479
      Height = 605
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 4
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
    Output = Memo2
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
