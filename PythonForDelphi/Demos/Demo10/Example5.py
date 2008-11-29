import DBTables
import pyio

True = 1
False = 0

# Misc
def DisplayTypesStats(*args):
  for i in apply(pyio.GetTypesStats, args): # Requested types
  #for i in pyio.GetTypesStats('TTable', 'TField'): # Only the types TTable and TField
    print "--------------------------------------------------------------------------------"
    print "TYPE ", i[0]
    print "    Instance count :", i[1]
    print "    Create Hits :", i[2]
    print "    Delete Hits :", i[3]
  print "--------------------------------------------------------------------------------"

# Event functions

def AfterScroll(Dataset):
  print "---------> Scrolled to rec #"+str(Dataset.RecNo)+" <--------------"

# Event methods

# WARNING :
# ---------
# if you store the TTable in your C instance, and if you use events,
# you must be aware of the reference counting mechanism, and its
# failure on circular references:
# c = C(): c refcount = 1
# c.T = CreateTTable(): c.T refcount = 1
# c.T.OnAfterOpen = c.OnAfterOpen: c refcount = 2
# del c: c refcount = 1 and you loose any reference on your c instance
# which still contains a TTable.
# so if you want to avoid the leaking, then you must clear the events
# before you delete the instance:
# c.T.OnAfterOpen = None: c refcount = 1
# del c: c refcount = 0 -> c is deleted
#
# Try to use try..finally constructs, in order to avoid
# the leaking if an exception is raised before you can free the
# resources.
#
# Don't forget to close the TTable, if you want to get the Before/After
# close events, because when destroying a TTable, we first clear all
# events. But that's the case with the Delphi behaviour too.

class C:
  def __init__(Self):
    # Create and open the table
    Self.T = DBTables.CreateTTable()
    Self.T.DatabaseName   = "DBDemos"
    Self.T.TableName      = "customer.db"
    Self.T.AfterScroll    = AfterScroll         # a global function
    Self.T.BeforeOpen     = Self.BeforeOpen     # a method of the c instance of class C
    Self.T.AfterOpen      = Self.AfterOpen      # a method of the c instance of class C
    Self.T.BeforeClose    = Self.BeforeClose    # a method of the c instance of class C
    Self.T.AfterClose     = Self.AfterClose     # a method of the c instance of class C
    Self.T.OnFilterRecord = Self.OnFilterRecord # a method of the c instance of class C
    Self.T.Filtered = True
    Self.T.Open()

  def __del__(Self):
    print "c instance deleted"

  def Free(Self):
    Self.T.Close()
    del Self.T

  def BeforeOpen(Self, Dataset):
    Self.Name = Dataset.DatabaseName + ":" + Dataset.TableName
    print "Before Open of ", Self.Name

  def AfterOpen(Self, Dataset):
    print "After Open of ", Self.Name

  def BeforeClose(Self, Dataset):
    print "Before close of ", Self.Name

  def AfterClose(Self, Dataset):
    print "After Close of ", Self.Name

  def OnFilterRecord(Self, Dataset, Accept):
    # Accept is var argument in Delphi, but in Python you can't
    # change a function's argument. So we use a special object
    # TVarArg which contains a single property : Value
    Accept.Value = Dataset.FieldByName("Company").AsString > "E"
    if not Accept.Value:
      print "$$$$$ Rejected record:", Dataset.FieldsAsTuple()

DisplayTypesStats()
c = C()
DisplayTypesStats()

# don't use a "for i in c.T" expression with a filtered table,
# because the BDE will filter the whole table for each movement,
# in order to get the RecordCount.

try:
  c.T.First()
  while not c.T.EOF:
    print c.T.FieldsAsTuple()
    c.T.Next()

finally:
  c.Free()
  del c

DisplayTypesStats()
#DisplayTypesStats('TField', 'TTable')


