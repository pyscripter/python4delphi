from DelphiVCL import *

class MainForm(Form):

    def __init__(self, Owner):
        self.Caption = "A VCL Form..."
        self.SetBounds(10, 10, 500, 400)
        
        self.lblHello = Label(self)
        self.lblHello.SetProps(Parent=self, Caption="Hello Python")
        self.lblHello.SetBounds(10, 10, 300, 24)

        self.edit1 = Edit(self)
        self.edit1.SetProps(Parent=self)
        self.edit1.SetBounds(10, 30, 250, 24)

        self.button1 = Button(self)
        self.button1.Parent = self
        self.button1.SetBounds(270,24,100,30)
        self.button1.Caption = "Add"
        self.button1.OnClick = self.Button1Click

        self.lb1 = ListBox(self)
        self.lb1.Parent = self
        self.lb1.SetBounds(10,60,300,300)

        self.OnClose = self.MainFormClose

    def MainFormClose(self, Sender, Action):
        Action.Value = caFree

    def Button1Click(self, Sender):
        self.lb1.Items.Add(self.edit1.Text)
        self.edit1.Text = ""

def main():
    Application.Initialize()
    Application.Title = "MyDelphiApp"
    f = MainForm(Application)
    f.Show()
    FreeConsole()
    Application.Run()

main()

