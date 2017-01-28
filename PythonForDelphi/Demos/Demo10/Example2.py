import DBTables
True = 1
False = 0

# Get the TTable object associated to
# the delphi table displayed on right
# It is done when you click on Execute
T = DBTables.T

# Display columns
print "Columns: "
for i in T.FieldNamesAsTuple():
  print "   ", i

T.CancelRange()
# For each record of the table
for i in T:
  print "Rec.", T.RecNo, ":",  i

# Print some fields by their names
print T.FieldByName("Company")

# check state
if not T.State in [DBTables.dsEdit, DBTables.dsInsert]:
  print "Table is not edited"

# access the table like an array
print T[10]  # Row 11
print T[10] [1] # second field of row 11

# locate a record
if T.Locate( "City;State", ["Largo","FL"], [] ):
  print "Found rec:", T.RecNo
if T.Locate( "Company", "BLUE SPORTS", [DBTables.loCaseInsensitive] ):
  print "Found rec:", T.RecNo
if T.Locate( "Company", "ISLAND", [DBTables.loCaseInsensitive, DBTables.loPartialKey] ):
  print "Found rec:", T.RecNo

# lookup a record
print T.Lookup( "City;State", ["Largo","FL"], "CustNo;Company" )

# define a range
print "-----------------------------------------------------------"
print "Indexes:", T.GetIndexNames()
T.IndexName = "ByCompany"
T.SetRange( ["Unisco"], ["Unisco"] )
for i in T:
  print "Rec.", T.RecNo, ":",  i
T.CancelRange()

print "-----------------------------------------------------------"
# Find a record
if T.FindKey( ['Unisco'] ):
  print "Unisco found !"
else:
  print "Could not find Unisco !"

# Find the nearest record
T.FindNearest( ['Ocean'] )
print "Find nearest Ocean :", T.FieldsAsTuple()

# Print all doc strings of an instance's methods
def PrintDocOf( inst ):
  print "--------------------- Type ", type(inst).__name__, "---------------------"
  if type(inst).__doc__:
    print type(inst).__doc__
  print "Methods:"
  for i in inst.__members__:
    doc = getattr(inst, i).__doc__
    if doc:
      print "    ", doc
  print
  print "Properties:"
  for i in inst.__properties__:
    print "    ", i
print "-----------------------------------------------------------"
print "Documentation:"
print
print "Module DBTables:"
print DBTables.__doc__
PrintDocOf(T)
print "-----------------------------------------------------------"
print "Property values of", T
for i in T.__members__: # or T.__properties__
  print i, "=", getattr(T, i)


