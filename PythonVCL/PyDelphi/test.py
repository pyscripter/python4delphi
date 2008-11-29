# open the file SetPath.py and adjust the path
import SetPath
# use the same name of units as you use in the USES instruction of a unit.
from Forms import *
from StdCtrls import *

# Misc functions
def GetColor( r, g, b ):
  return r + (g<<8) + (b<<16)

def inherited(obj):
  return obj.__class__.__bases__[0]
  
# subclass TForm
class TMyForm(TForm):

  # Always use the constructor CreateNew for the Forms, otherwise you'll get an exception
  # because Delphi tries to load the associated ressource, but it doesn't exist.
  def CreateNew(Self, AOwner):
    # call inherited
    inherited(Self).CreateNew( Self, AOwner) # or TForm.CreateNew( Self, AOwner)
    # init form properties
    Self.FormStyle = fsMDIChild
    Self.BorderIcons = [biSystemMenu, biMinimize]
    Self.Caption = "New form"
    Self.Visible = True
    # init form events. Delegation is possible, as in Delphi !
    Self.OnClose = Self.DoClose
    # create a GroupBox
    Self.g = TGroupBox().Create(Self)
    Self.g.Caption = "Buttons"
    Self.g.Height = 60
    Self.g.Align = alTop
    Self.g.Parent = Self
    # Create a Memo
    Self.m = TMemo().Create(Self)
    Self.m.Align = alClient
    Self.m.ScrollBars = ssVertical
    Self.m.Font.Size = 16
    Self.m.Parent = Self
    Self.m.Color = GetColor(200,200,255)
    Self.m.Font.Color = GetColor(0,0,240)
    Self.m.Lines.LoadFromFile( "c:\\autoexec.bat")
    # Create several buttons
    Self.L=[]
    for i in range(4):
      b = TButton().Create(Self)
      b.Caption = "Button "+str(i)
      b.Left = 10+120*i
      b.Top = 20
      b.Right = 100
      b.Bottom = 30
      b.Parent = Self.g
      b.OnClick = Self.DoButtonClick
      Self.L.append(b)
    # Create a TEdit
    Self.e = TEdit().Create(Self)
    Self.e.Left = 500
    Self.e.Top = 20
    Self.e.Parent = Self.g
    # You can use strings or integers for defining chars:
    #Self.e.PasswordChar = "#"
    #Self.e.PasswordChar = 43
    Self.e.OnKeyPress = Self.DoEditKeyPress
    # Don't forget to return Self !
    return Self

  def __del__( Self ):
     print "deleting ", Self

  def DoClose(Self, Sender, Action ):
    #Note: Action is a var arg in Delphi, but it does not exist in Python !
    #so we use an object (VarArg) that only stores a Python object in an 
    #attribute called Value. So, change this attribute and not the argument itself.
    print "Close from: ", Sender.Caption, "   Action = ", Action
    Action.Value = caFree

  def DoButtonClick( Self, Sender ):
    #There's no need (and no way) to cast the Sender, because it is the true object.
    print "Button '", Sender.Caption, "' clicked !"
    Self.m.Text = "Button '" + Sender.Caption + "' clicked !"
    if Sender.Caption == "Button 0":
      Application.MessageBox( "Button '" + Sender.Caption + "' clicked !", "Message from Python", 1 )
    
  def DoEditKeyPress( Self, Sender, Key ):
    print "Key ["+Key.Value+"] pressed in TEdit"
    if Key.Value == " ":
      Key.Value = "_"
    
f = TMyForm().CreateNew(Application)
if not(biMaximize in f.BorderIcons):
  print "biMaximize not defined in the Form"


#if you want to delete the form from Python, just type:
#f.Free()
#But the Python instance remains alive. If you want to destroy it,, type:
#del f
#But if you only do a "del f", then you loose contact to the form, and it must be closed
#by the user. That's usually what we want !!!

