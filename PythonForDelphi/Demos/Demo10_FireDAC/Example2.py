import DBFireDac

# Get the TTable object associated to
# the delphi table displayed on right
# It is done when you click on Execute
T = DBFireDac.T

# Display TableName
print ("TableName: ", T.TableName)

# Display columns
print ("Columns: ")
for i in T.FieldNamesAsTuple():
  print ("   ", i)

T.CancelRange()
# For each record of the table
for i in T:          # <- required ContainerAccess !!!
  print ("Rec.", T.RecNo, ":",  i)

# Print some fields by their names
print ("Rec.", T.RecNo, ": ", "Company: ", T.FieldByName("Company").Value)

# check state
if not T.State in [DBFireDac.dsEdit, DBFireDac.dsInsert]:
  print ("Table is not edited")

# access the table like an array
print ("Index 10; Row 11: ", T[10])    # Row 11   <- required ContainerAccess !!!
print ("Index 10; Row 11 - First field: ", T[10][1])  # second field of row 11   <- required ContainerAccess !!!

# locate a record
if T.Locate( "City;State", ["Largo","FL"], [] ):
  print ("Found RecNo:", T.RecNo, " for Locate(Key: 'City;State', Value: [Largo,FL])")
if T.Locate( "Company", "BLUE SPORTS", [DBFireDac.loCaseInsensitive] ):
  print ("Found RecNo:", T.RecNo, " for Locate(Key: 'Company', Value: [BLUE SPORTS])")
if T.Locate( "Company", "ISLAND", [DBFireDac.loCaseInsensitive, DBFireDac.loPartialKey] ):
  print ("Found RecNo:", T.RecNo, " for Locate(Key: 'Company', Value: [ISLAND])")

# lookup a record
print ("Lookup 'CustNo;Company' for Key: 'City;State', Value: [Largo,FL] -> ", T.Lookup( "City;State", ["Largo","FL"], "CustNo;Company" ))

# define a range
print ("-----------------------------------------------------------")
print ("Names of Indexes:", T.GetIndexNames())
T.IndexName = "SK1_CUSTOMER"
T.SetRange( ["Unisco"], ["Unisco"] )
for i in T:
  print ("Rec.", T.RecNo, ":",  i)
T.CancelRange()

print ("-----------------------------------------------------------")
# Find a record
if T.FindKey( ['Unisco'] ):
  print ("Unisco found !")
else:
  print ("Could not find Unisco !")

# Find the nearest record
T.FindNearest( ['Ocean'] )
print ("Find nearest Ocean :", T.FieldsAsTuple())

# Print all doc strings of an instance's methods
def PrintDocOf( inst ):
  print ("--------------------- Type ", type(inst).__name__, "---------------------")
  if type(inst).__doc__:
    print (type(inst).__doc__)
print ("-----------------------------------------------------------")
print ("Documentation:")
print
print ("Module DBFireDac:")
print (DBFireDac.__doc__)
PrintDocOf(T)
print ("-----------------------------------------------------------")


