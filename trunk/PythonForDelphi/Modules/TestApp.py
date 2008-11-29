from Delphi import *

class MainForm(Form):

    def __init__(self, Owner):
        self.Caption = "A Delphi Form..."
        self.SetBounds(10, 10, 500, 400)
        self.lblHello = Label(self)
        self.lblHello.SetProps(Parent=self, Caption="Hello World")
        self.lblHello.SetBounds(10, 10, 300, 24)
        self.OnClose = self.MainFormClose

    def MainFormClose(self, Sender, Action):
        Action.Value = caFree

def main():
    Application.Initialize()
    Application.Title = "MyDelphiApp"
    f = MainForm(Application)
    f.Show()
    FreeConsole()
    Application.Run()

main()

