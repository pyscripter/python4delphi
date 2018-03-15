# open the file SetPath.py and adjust the path
import SetPath
# use the same name of units as you use in the USES instruction of a unit.
from Forms import *
from StdCtrls import *

# Create a class with a Click event
class Test:
  def Click( Self, Sender ):
    print "You clicked the ", Sender.Caption, " menu item !"

# Instantiate this class    
t = Test()

# Get the MainMenu from the MainForm
main = Application.MainForm.FindComponent( "MainMenu1")

# Create a new menu
new = TMenuItem().Create(main)
new.Caption = "New menu"

# Create a submenu item in the new menu
sub1 = TMenuItem().Create(main)
sub1.Caption = "Click-me !"
sub1.OnClick = t.Click # connect this menu item to the instance created, containing our event code
new.Add( sub1 )

# Add the new menu
main.Items.Add( new )

# Try it !!!
