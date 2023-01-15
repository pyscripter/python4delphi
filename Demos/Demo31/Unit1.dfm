object Form1: TForm1
  Left = 246
  Top = 10
  Width = 737
  Height = 647
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
  Visible = True
  OnCreate = FormCreate
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 169
    Width = 715
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Memo1: TMemo
    Left = 0
    Top = 172
    Width = 715
    Height = 378
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Pitch = fpVariable
    Font.Style = []
    Lines.Strings = (
      
        'from spam import DelphiVersion, MainForm, DVar, CreateComponent,' +
        ' Application, Screen, mrOk, Component, Form, Button, CheckBox, O' +
        'penDialog, caFree'
      
        'from spam import Point, Monitor, DrawGrid, gdSelected, clBlue, s' +
        'sCtrl, PageControl, TabSheet'
      'if DelphiVersion >= 6:'
      '  from spam import mdNearest'
      'if DelphiVersion >= 15: '
      '  from spam import rtti_var'
      ''
      'import unittest'
      'import sys'
      'py_major, py_minor = sys.version_info[:2]'
      ''
      'class MyForm(Form):'
      '  def __init__(self, Owner):'
      '    self.Caption = '#39'Subclassed form'#39
      '    self.btnClose = Button(self)'
      '    self.btnClose.Parent = self'
      '    self.btnClose.Caption = '#39'Close'#39
      '    self.btnClose.SetBounds(10, 10, 120, 30)'
      '    self.btnClose.OnClick = self.btnCloseClick'
      ''
      '    self.chkCanClose = CheckBox(self)'
      '    self.chkCanClose.Parent = self'
      '    self.chkCanClose.Caption = '#39'Can close?'#39
      '    self.chkCanClose.SetBounds(10, 50, 120, 30)'
      ''
      '    self.grdTest = DrawGrid(self)'
      '    self.grdTest.Parent = self'
      '    self.grdTest.SetBounds(10, 100, 300, 250)'
      '    self.grdTest.OnDrawCell = self.grdTestDrawCell'
      '    self.grdTest.OnSelectCell = self.grdTestSelectCell'
      ''
      '    self.OnCloseQuery = self.MyFormCloseQuery'
      '    self.OnClose = self.MyFormClose'
      '    self.Width = 400'
      '    self.Height = 400'
      ''
      '  def btnCloseClick(self, Sender):'
      '    print("Close!")'
      '    self.Close()'
      ''
      '  def MyFormCloseQuery(self, Sender, CanClose):'
      '    CanClose.Value = self.chkCanClose.Checked'
      ''
      '  def MyFormClose(self, Sender, Action):'
      '    Action.Value = caFree'
      ''
      '  def grdTestDrawCell(self, Sender, Col, Row, Rect, State):'
      '    if gdSelected in State:'
      '      Sender.Canvas.Brush.Color = clBlue # 0x00ff0000 # blue'
      
        '      print("Cell[%d, %d] is selected, Rect=%s, State=%s" % (Col' +
        ', Row, Rect, State))'
      
        '    Sender.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, "%d @ ' +
        '%d" % (Col, Row))'
      ''
      '  def grdTestSelectCell(self, Sender, Col, Row, CanSelect):'
      '    if Col == 2 and Row == 2:'
      '      CanSelect.Value = False'
      ''
      'class TTestForm(Form):'
      '  def __init__(self, Owner):'
      
        '    self.Caption = self.Caption + '#39' - changed by Python subclass' +
        #39
      '    self.BindMethodsToEvents()'
      ''
      '  def handle_btnAdd_OnClick(self, Sender):'
      '    self.ListBox1.Items.Add(self.Edit1.Text)'
      ''
      'class TestDelphiWrapper(unittest.TestCase):'
      ''
      '    def testReadWriteProperties(self):'
      '        DVar.SValue = '#39'Weight'#39
      '        self.assertEqual(DVar.SValue, '#39'Weight'#39')'
      '        DVar.IValue = 70'
      '        self.assertEqual(DVar.IValue, 70)'
      
        '        MainForm.Caption = '#39'PyDelphi rocks!'#39'  #setting propertie' +
        's'
      '        self.assertEqual(MainForm.Caption, '#39'PyDelphi rocks!'#39')'
      '        print()'
      
        '        print('#39'MainForm.ActiveControl='#39',  MainForm.ActiveControl' +
        ')  # class properties'
      
        '        MainForm.BorderStyle = '#39'bsSizeable'#39' #enumeration propert' +
        'y'
      '        MainForm.Anchors = ['#39'akLeft'#39', '#39'akTop'#39'] #set property'
      '        self.assertEqual(MainForm.Anchors, ['#39'akLeft'#39', '#39'akTop'#39'])'
      ''
      '    def testTObject(self):'
      '        self.assertEqual(MainForm.ClassName, '#39'TForm1'#39')'
      '        self.assertTrue(MainForm.InheritsFrom('#39'TObject'#39'))'
      ''
      '    def testMethodCall(self):'
      '        if DelphiVersion >= 7:'
      '            DVar.SetMeUp('#39'Age'#39', 25)'
      '            self.assertEqual(DVar.SValue, '#39'Age'#39')'
      '            self.assertEqual(DVar.IValue, 25)'
      '            print()'
      '            print(DVar.DescribeMe()) #method calls'
      ''
      '    def testRepr(self):'
      '        print()'
      '        print('#39'Representation of Delphi objects'#39')'
      '        print(DVar)'
      '        print(MainForm)'
      '        if DelphiVersion >= 7:'
      '            print(DVar.DescribeMe)  # method object'
      '        print(DVar.SL) # TStrings'
      ''
      '    def testTStrings(self):'
      '        SL = DVar.SL'
      '        self.assertEqual(len(SL), 2)'
      '        print()'
      '        for i in DVar.SL: print(i, '#39' contains '#39', DVar.SL[i])'
      '        SL.Add('#39'New String'#39')'
      '        self.assertEqual(len(SL), 3)'
      '        self.assertEqual(SL.IndexOf('#39'New String'#39'), 2)'
      '        self.assertEqual(SL[0], '#39'Form1'#39')'
      '        self.assertEqual(SL.Objects[0], MainForm)'
      '        self.assertEqual(MainForm in SL.Objects, True)'
      '        SL[2] = '#39'Changed'#39
      '        self.assertEqual(SL[2], '#39'Changed'#39')'
      '        self.assertEqual(SL.Objects[2], None)'
      '        SL.Objects[2] = MainForm'
      '        self.assertEqual(SL.Objects[2], MainForm)'
      '        self.assertEqual(MainForm in SL.Objects, True)'
      '        SL.Delete(2)'
      '        self.assertEqual(len(SL), 2)'
      '        print("str(SL) =", str(SL))'
      '        SL.Assign([1, 2, 3])'
      '        self.assertEqual(len(SL), 3)'
      '        self.assertEqual('#39'2'#39' in SL, True)'
      '        self.assertEqual(SL[1], '#39'2'#39')'
      '        tmp = ['#39'a'#39', '#39'b'#39', '#39'c'#39']'
      '        SL.Assign(tmp)'
      '        self.assertEqual(SL.ToList(), tmp)'
      '        tmp = ('#39'a'#39', '#39'b'#39', '#39'c'#39')'
      '        SL.Assign(tmp)'
      '        self.assertEqual(SL.ToTuple(), tmp)'
      ''
      '    def testTComponent(self):'
      '        self.assertEqual(MainForm.Name, '#39'Form1'#39')'
      '        count = MainForm.ComponentCount'
      '        def SetComponentCount():'
      '            MainForm.ComponentCount = 3'
      '        if py_major == 2 and py_minor < 5:'
      '            self.assertRaises(TypeError, SetComponentCount)'
      '        else:'
      '            self.assertRaises(AttributeError, SetComponentCount)'
      '        self.assertEqual(MainForm.__owned__, False)'
      ''
      '    def testSubComponents(self):'
      '        MainForm.Button1.Caption = '#39'Click me!!'#39
      '        self.assertEqual(MainForm.Button1.Caption, '#39'Click me!!'#39')'
      '       # test alternate mapping notation'
      '        MainForm['#39'Button1'#39'].Caption = '#39'Click me!!!!'#39
      
        '        self.assertEqual(MainForm['#39'Button1'#39'].Caption, '#39'Click me!' +
        '!!!'#39')'
      
        '        self.assertEqual(MainForm.Button1.Caption, '#39'Click me!!!!' +
        #39')'
      '        # Test Owner property and DelphiObject comparison'
      '        self.assertTrue(MainForm.Button1.Owner == MainForm)'
      '        print([i.Name for i in MainForm.Components])'
      ''
      '    def testCreateComponent(self):'
      '        NewButton = CreateComponent('#39'TButton'#39', None)'
      '        self.assertEqual(NewButton.__bound__, True)'
      '        self.assertEqual(NewButton.__owned__, True)'
      '        NewButton.Free()'
      '        self.assertEqual(NewButton.__bound__, False)'
      ''
      '    def testWinControls(self):'
      '        self.assertEqual(MainForm.Panel1.Parent, MainForm)'
      '        print()'
      
        '        print('#39'MainForm contains '#39', MainForm.ControlCount, '#39' con' +
        'trols'#39')'
      '        print([i.Name for i in MainForm.Controls])'
      ''
      '    def testForm(self):'
      '        MyForm = CreateComponent('#39'TForm'#39', None)'
      '        MyForm.Name = '#39'MyForm'#39
      '        MyForm.Caption = '#39'Python Generated Form'#39
      '        MyForm.Height = 300'
      '        MyForm.Width = 310'
      '        MyForm.Position = '#39'poDefaultPosOnly'#39
      '        PC = PageControl(MyForm)'
      '        PC.Name = "MyPageControl"'
      '        PC.Parent = MyForm'
      '        PC.Align = "alClient"'
      '        P1 = TabSheet(MyForm)'
      '        P1.PageControl = PC'
      '        P1.Caption = "Page 1"'
      '        LB = CreateComponent('#39'TListBox'#39', MyForm)'
      '        B1 = CreateComponent('#39'TButton'#39', MyForm)'
      '        B2 = CreateComponent('#39'TButton'#39', MyForm)'
      '        Edit = CreateComponent('#39'TEdit'#39', MyForm)'
      '        LB.Name = '#39'ListBox1'#39
      '        LB.Parent = P1'
      '        LB.Left = 20'
      '        LB.Top = 14'
      '        LB.Width = 121'
      '        LB.Height = 97'
      '        LB.TabOrder = 0'
      '        B1.Name = '#39'Button1'#39
      '        B1.Parent = P1'
      '        B1.Left = 185'
      '        B1.Top = 60'
      '        B1.Width = 75'
      '        B1.Height = 25'
      '        B1.Caption = '#39'Add to list'#39
      '        B1.TabOrder = 1'
      '        Edit.Name = '#39'Edit1'#39
      '        Edit.Parent = P1'
      '        Edit.Left = 168'
      '        Edit.Top = 27'
      '        Edit.Width = 121'
      '        Edit.Height = 21'
      '        Edit.TabOrder = 2'
      '        Edit.Text = '#39'Add me to List'#39
      '        B2.Name = '#39'Button2'#39
      '        B2.Parent = P1'
      '        B2.Left = 121'
      '        B2.Top = 125'
      '        B2.Width = 75'
      '        B2.Height = 25'
      '        B2.Caption = '#39'Close'#39
      '        B2.ModalResult = mrOk'
      '        B2.TabOrder = 3'
      '        def ClickHandler(Sender):'
      '            print(Sender.Name, '#39' was clicked'#39')'
      '            LB.Items.Add(Edit.Text)'
      '        B1.OnClick = ClickHandler'
      '        def KeyPressHandler(Sender, Key):'
      '           if Key.Value == '#39'a'#39':'
      '              Key.Value = '#39'z'#39
      '        Edit.OnKeyPress = KeyPressHandler'
      '        def KeyDownHandler(Sender, Key, Shift):'
      '           # forbid Ctrl+Home'
      '           if Key.Value == 36 and ssCtrl in Shift:'
      '              Key.Value = 0'
      '        Edit.OnKeyDown = KeyDownHandler'
      '        BtnNextPage = Button(MyForm)'
      '        BtnNextPage.Parent = P1'
      '        BtnNextPage.SetBounds(10, 200, 150, 24)'
      '        BtnNextPage.Caption = "Select Next Page"'
      '        def NextPageClick(Sender):'
      '           Sender.Owner.MyPageControl.SelectNextPage(True, True)'
      '        BtnNextPage.OnClick = NextPageClick'
      '        P2 = TabSheet(MyForm)'
      '        P2.PageControl = PC'
      '        P2.Caption = "Page 2"'
      '        BtnGotoPage3 = Button(MyForm)'
      '        BtnGotoPage3.Parent = P2'
      '        BtnGotoPage3.SetBounds(10, 10, 150, 24)'
      '        BtnGotoPage3.Caption = "Goto Page 3"'
      '        def GotoPage3Click(Sender):'
      
        '           Sender.Owner.MyPageControl.ActivePage = Sender.Owner.' +
        'Page3'
      '        BtnGotoPage3.OnClick = GotoPage3Click'
      ''
      '        P3 = TabSheet(MyForm)'
      '        P3.PageControl = PC'
      '        P3.Caption = "Page 3"'
      '        P3.Name = "Page3"'
      '        BtnGotoFirstPage = Button(MyForm)'
      '        BtnGotoFirstPage.Parent = P3'
      '        BtnGotoFirstPage.SetBounds(10, 10, 150, 24)'
      '        BtnGotoFirstPage.Caption = "Goto First Page"'
      '        def GotoFirstPageClick(Sender):'
      '           PC.ActivePageIndex = 0'
      '        BtnGotoFirstPage.OnClick = GotoFirstPageClick'
      '        chkAllowChange = CheckBox(MyForm)'
      '        chkAllowChange.Parent = P3'
      '        chkAllowChange.SetBounds(10, 40, 150, 24)'
      '        chkAllowChange.Caption = "Allow Page Change?"'
      '        chkAllowChange.Checked = True'
      ''
      '        def PCChanging(Sender, AllowChange):'
      '           AllowChange.Value = chkAllowChange.Checked'
      '        PC.OnChanging = PCChanging'
      ''
      '        MyForm.ShowModal()'
      ''
      '    def testFormSubclass(self):'
      '        f = MyForm(Application)'
      '        try:'
      '          print(f.ShowModal())'
      '        finally:'
      '          f.Free()'
      ''
      '    def testFormSubclass2(self):'
      '        f = TTestForm(Application)'
      '        try:'
      '          print(f.ShowModal())'
      '        finally:'
      '          f.Free()'
      ''
      '    def testPointConversions(self):'
      '      p1 = Point(10, 10)'
      '      p = MainForm.ClientToScreen(p1)'
      '      print(p)'
      '      p2 = MainForm.ScreenToClient(p)'
      '      self.assertEqual(p2.X, p1.X)'
      '      self.assertEqual(p2.Y, p1.Y)'
      ''
      '    def testObjectNotification(self):'
      '        DVar.IValue = 0'
      '        def ChangeHandler(Sender):'
      '            print(Sender)'
      '            Sender.IValue = 55'
      '        DVar.OnChange = ChangeHandler'
      '        if DelphiVersion >= 7:'
      '            DVar.TriggerChange()'
      '            self.assertEqual(DVar.IValue, 55)'
      '            DVar.OnChange = None'
      '            DVar.IValue = 0'
      '            DVar.TriggerChange()'
      '            self.assertEqual(DVar.IValue, 0)'
      ''
      '    def testActions(self):'
      '        self.assertEqual(MainForm.ActionList1.ActionCount, 1)'
      
        '        self.assertEqual(MainForm.ActionList1.Actions[0], MainFo' +
        'rm.actTest)'
      
        '        self.assertEqual(MainForm.actTest in MainForm.ActionList' +
        '1, True)'
      '        DVar.IValue = 0'
      '        MainForm.actTest.Execute()'
      '        self.assertEqual(DVar.IValue, 1)'
      '        def ActionHandler(Sender):'
      '            print("Action", Sender.Name, "executed from Python")'
      '            DVar.IValue = 2'
      '        self.assertEqual(MainForm.actTest.OnExecute, None)'
      '        MainForm.actTest.OnExecute = ActionHandler'
      
        '        self.assertEqual(MainForm.actTest.OnExecute, ActionHandl' +
        'er)'
      '        DVar.IValue = 0'
      '        MainForm.actTest.Execute()'
      '        self.assertEqual(DVar.IValue, 2)'
      '        MainForm.actTest.OnExecute = None'
      '        self.assertEqual(MainForm.actTest.OnExecute, None)'
      ''
      '    def testScreen(self):'
      '        self.assertEqual(Screen.DataModuleCount, 0)'
      '        self.assertEqual(Screen.FormCount > 0, True)'
      '        self.assertEqual(Screen.CustomFormCount > 0, True)'
      '        idx = -1'
      '        for i, f in enumerate(Screen.Forms):'
      '          if f == MainForm:'
      '            idx = i'
      '            break;'
      '        self.assertEqual(idx > -1, True)'
      '        self.assertEqual(Screen.Forms[idx], MainForm)'
      '        self.assertEqual(Screen.CustomForms[idx], MainForm)'
      '        self.assertEqual(Screen.Width > 0, True)'
      '        self.assertEqual(Screen.Height > 0, True)'
      '        MainForm.Button1.SetFocus()'
      '        self.assertEqual(Screen.ActiveControl, MainForm.Button1)'
      '        def ActiveControlChangeHandler(Sender):'
      '            print("ActiveControlChangeHandler fired")'
      
        '            print("New active constrol is", Sender.ActiveControl' +
        '.Name)'
      '            DVar.IValue = 2'
      '        self.assertEqual(Screen.OnActiveControlChange, None)'
      
        '        Screen.OnActiveControlChange = ActiveControlChangeHandle' +
        'r'
      
        '        self.assertEqual(Screen.OnActiveControlChange, ActiveCon' +
        'trolChangeHandler)'
      '        DVar.IValue = 0'
      '        self.assertEqual(DVar.IValue, 0)'
      '        MainForm.Memo2.SetFocus()'
      '        self.assertEqual(DVar.IValue, 2)'
      '        Screen.OnActiveControlChange = None'
      '        self.assertEqual(Screen.OnActiveControlChange, None)'
      '        DVar.IValue = 0'
      '        MainForm.Button1.SetFocus()'
      '        self.assertEqual(DVar.IValue, 0)'
      '        if DelphiVersion >= 6:'
      
        '            m = Screen.MonitorFromPoint(Point(10, 10), mdNearest' +
        ')'
      '            self.assertEqual(isinstance(m, Monitor), True)'
      ''
      '    def testOpenDialog(self):'
      '        print("OpenDialog test:")'
      '        open_dialog = CreateComponent("TOpenDialog", None)'
      '        if open_dialog.Execute():'
      '            print("FileName:", open_dialog.FileName)'
      '        open_dialog.Free()'
      ''
      '    def testClassProperty(self):'
      
        '        MainForm.ActiveControl = MainForm.Memo2 # Class property' +
        '!'
      '        self.assertEqual(MainForm.ActiveControl, MainForm.Memo2)'
      '    '
      '    def testDir(self):'
      '        L = dir(MainForm)'
      '        self.assertTrue('#39'ComponentCount'#39' in L)'
      '        self.assertTrue('#39'__owned__'#39' in L)'
      '        self.assertTrue('#39'ShowModal'#39' in L)'
      ''
      '    def testRttiAccess(self):'
      '        if DelphiVersion >= 15:'
      '          rtti_var.Fruit = '#39'Apple'#39
      '          self.assertTrue(rtti_var.Fruit == '#39'Apple'#39')'
      '          rtti_var.Fruits = ['#39'Apple'#39', '#39'Banana'#39']'
      
        '          self.assertTrue(rtti_var.Fruits == ['#39'Apple'#39', '#39'Banana'#39']' +
        ')'
      '          rtti_var.BuyFruits(['#39'Apple'#39', '#39'Orange'#39'])'
      
        '          self.assertTrue(rtti_var.Fruits == ['#39'Apple'#39', '#39'Orange'#39']' +
        ')'
      
        '          rtti_var.SetFormCaption(MainForm, '#39'From TTestRTTIAcces' +
        's'#39')'
      
        '          self.assertEqual(MainForm.Caption, '#39'From TTestRTTIAcce' +
        'ss'#39')'
      '          rtti_var.FruitField = '#39'Apple'#39
      '          self.assertTrue(rtti_var.FruitField == '#39'Apple'#39')'
      '          rtti_var.FruitsField = ['#39'Apple'#39', '#39'Banana'#39']'
      
        '          self.assertTrue(rtti_var.FruitsField == ['#39'Apple'#39', '#39'Ban' +
        'ana'#39'])'
      '          rtti_var.StringField = '#39'Hi'#39
      '          self.assertTrue(rtti_var.StringField == '#39'Hi'#39')'
      '          rtti_var.DoubleField = 3.14'
      '          self.assertTrue(rtti_var.DoubleField == 3.14)'
      '          rtti_var.ObjectField = MainForm'
      '          MainForm.Caption = '#39'From TTestRTTIAccess.ObjectField'#39
      
        '          self.assertTrue(MainForm.Caption == '#39'From TTestRTTIAcc' +
        'ess.ObjectField'#39')'
      ''
      '    def testInheritance(self):'
      '         self.assertTrue(issubclass(Form, Component))'
      '         self.assertTrue(issubclass(Button, Component))'
      ''
      'if __name__ == '#39'__main__'#39':'
      '    try:'
      '        unittest.main()'
      '    except SystemExit:'
      '        pass')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 550
    Width = 715
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
    Width = 715
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
  object PyEngine: TPythonEngine
    IO = PythonGUIInputOutput1
    Left = 16
    Top = 16
  end
  object PythonModule: TPythonModule
    Engine = PyEngine
    Events = <
      item
        Name = 'TestFunction'
        OnExecute = PythonModuleEvents0Execute
        DocString.Strings = (
          'doc for TestFunction')
      end>
    ModuleName = 'spam'
    Errors = <>
    Left = 72
    Top = 16
  end
  object PythonGUIInputOutput1: TPythonGUIInputOutput
    UnicodeIO = True
    RawOutput = False
    Output = Memo2
    Left = 168
    Top = 17
  end
  object ActionList1: TActionList
    Left = 16
    Top = 80
    object actTest: TAction
      Caption = 'Test'
      OnExecute = actTestExecute
    end
  end
end
