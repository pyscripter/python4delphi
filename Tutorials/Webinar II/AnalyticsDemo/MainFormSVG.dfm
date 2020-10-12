object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Analytics Demo (Covid-19 Analysis)'
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
      Caption = 'Covid-19 Analysis'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 481
        Height = 585
        Align = alClient
        TabOrder = 0
        object Splitter2: TSplitter
          Left = 1
          Top = 421
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
            'from delphi_module import svg_image'
            'from io import StringIO'
            'import numpy as np'
            'import pandas as pd'
            'import matplotlib.pyplot as plt'
            'from sklearn.linear_model import  BayesianRidge'
            
              'from sklearn.model_selection import RandomizedSearchCV, train_te' +
              'st_split'
            'from sklearn.preprocessing import PolynomialFeatures'
            
              'from sklearn.metrics import mean_squared_error, mean_absolute_er' +
              'ror'
            'from sklearn.utils import parallel_backend'
            'parallel_backend('#39'threading'#39')'
            'import datetime'
            'import warnings'
            'warnings.filterwarnings("ignore")'
            ''
            
              'confirmed_df = pd.read_csv('#39'https://raw.githubusercontent.com/CS' +
              'SEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_ti' +
              'me_series/time_series_covid19_confirmed_global.csv'#39')'
            ''
            'parallel_backend('#39'threading'#39')'
            ''
            'cols = confirmed_df.keys()'
            'confirmed = confirmed_df.loc[:, cols[4]:cols[-1]]'
            ''
            'dates = confirmed.keys()'
            'world_cases = []'
            ''
            'for i in dates:'
            '    confirmed_sum = confirmed[i].sum()'
            '    world_cases.append(confirmed_sum)'
            ''
            '# window size'
            'window = 7'
            ''
            
              'days_since_1_22 = np.array([i for i in range(len(dates))]).resha' +
              'pe(-1, 1)'
            'world_cases = np.array(world_cases).reshape(-1, 1)'
            ''
            'days_in_future = 10'
            
              'future_forcast = np.array([i for i in range(len(dates)+days_in_f' +
              'uture)]).reshape(-1, 1)'
            ''
            'start = '#39'1/22/2020'#39
            'start_date = datetime.datetime.strptime(start, '#39'%m/%d/%Y'#39')'
            'future_forcast_dates = []'
            'for i in range(len(future_forcast)):'
            
              '    future_forcast_dates.append((start_date + datetime.timedelta' +
              '(days=i)).strftime('#39'%m/%d/%Y'#39'))'
            ''
            
              'X_train_confirmed, X_test_confirmed, y_train_confirmed, y_test_c' +
              'onfirmed = train_test_split(days_since_1_22[50:], world_cases[50' +
              ':], test_size=0.05, shuffle=False)'
            ''
            '# transform our data for polynomial regression'
            'bayesian_poly = PolynomialFeatures(degree=5)'
            
              'bayesian_poly_X_train_confirmed = bayesian_poly.fit_transform(X_' +
              'train_confirmed)'
            
              'bayesian_poly_X_test_confirmed = bayesian_poly.fit_transform(X_t' +
              'est_confirmed)'
            
              'bayesian_poly_future_forcast = bayesian_poly.fit_transform(futur' +
              'e_forcast)'
            ''
            '# bayesian ridge polynomial regression'
            'tol = [1e-6, 1e-5, 1e-4, 1e-3, 1e-2]'
            'alpha_1 = [1e-7, 1e-6, 1e-5, 1e-4, 1e-3]'
            'alpha_2 = [1e-7, 1e-6, 1e-5, 1e-4, 1e-3]'
            'lambda_1 = [1e-7, 1e-6, 1e-5, 1e-4, 1e-3]'
            'lambda_2 = [1e-7, 1e-6, 1e-5, 1e-4, 1e-3]'
            'normalize = [True, False]'
            ''
            
              'bayesian_grid = {'#39'tol'#39': tol, '#39'alpha_1'#39': alpha_1, '#39'alpha_2'#39' : alp' +
              'ha_2, '#39'lambda_1'#39': lambda_1, '#39'lambda_2'#39' : lambda_2,'
            '                 '#39'normalize'#39' : normalize}'
            ''
            'bayesian = BayesianRidge(fit_intercept=True)'
            
              'bayesian_search = RandomizedSearchCV(bayesian, bayesian_grid, sc' +
              'oring='#39'neg_mean_squared_error'#39', cv=3, return_train_score=True, n' +
              '_jobs=-1, n_iter=40, verbose=1)'
            
              'bayesian_search.fit(bayesian_poly_X_train_confirmed, y_train_con' +
              'firmed)'
            ''
            'print(bayesian_search.best_params_)'
            ''
            'bayesian_confirmed = bayesian_search.best_estimator_'
            
              'test_bayesian_pred = bayesian_confirmed.predict(bayesian_poly_X_' +
              'test_confirmed)'
            
              'bayesian_pred = bayesian_confirmed.predict(bayesian_poly_future_' +
              'forcast)'
            
              'print('#39'MAE:'#39', mean_absolute_error(test_bayesian_pred, y_test_con' +
              'firmed))'
            
              'print('#39'MSE:'#39',mean_squared_error(test_bayesian_pred, y_test_confi' +
              'rmed))'
            ''
            'plt.plot(y_test_confirmed)'
            'plt.plot(test_bayesian_pred)'
            
              'plt.legend(['#39'Test Data'#39', '#39'Bayesian Ridge Polynomial Predictions'#39 +
              '])'
            ''
            'figfile = StringIO()'
            'plt.savefig(figfile, format='#39'svg'#39')'
            'figdata_svg = figfile.getvalue()'
            'svg_image.SvgText = figdata_svg'
            ''
            '#plt.show()')
          ExplicitLeft = 3
          ExplicitTop = -2
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
          Top = 424
          Width = 479
          Height = 119
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
    Left = 628
    Top = 89
  end
  object PythonEngine1: TPythonEngine
    InitThreads = True
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
