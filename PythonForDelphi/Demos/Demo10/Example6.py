import DBTables
True = 1
False = 0

# Here's how you can use a Proxy class
# in order to inherit from any Python object
# like our TTable or TQuery !

class Proxy:
  def __init__(Self, Object):
    Self.__object__ = Object

  def __getattr__(Self, Key):
    return getattr(Self.__dict__['__object__'], Key)

  def __setattr__(Self, Key, Value):
    if Key == "__object__":
      Self.__dict__[Key] = Value
    elif hasattr(Self.__object__, Key):
      return setattr(Self.__object__, Key, Value)
    else:
      Self.__dict__[Key] = Value

  def __len__(Self):
    return len(Self.__object__)

  def __getitem__(Self, Item):
    return Self.__object__[Item]

class TMyQuery(Proxy):
  def __init__(Self):
    Proxy.__init__(Self, DBTables.CreateTQuery() )

class TMySubQuery(TMyQuery):
  def First(Self):
    print "First requested on Query"
    Self.__object__.First()
    

# Create and open the table
T = TMySubQuery()
T.DatabaseName = "DBDemos"
T.SQL = [
  "SELECT *",
  "  FROM customer.db"
        ]
T.Open()

# Display columns
print "Columns: ", T.FieldNamesAsTuple()

# Test of method override
T.Last()
print "RecNo =", T.RecNo
T.First()
print "RecNo =", T.RecNo

# For each record of the table
T.First()
while not T.EOF:
  # Get all the fields in a list
  A = []
  for i in xrange( T.FieldCount ):
    A.append( T.Fields(i).AsString )
  # Print the current record number and the list
  print "Rec.", T.RecNo, ":",  A
  # Get next record
  T.Next()

# Print some fields by their names
print T.FieldByName("Company")

# Print the fields as a dictionary
D = T.FieldsAsDict()
print D["Company"]

# Iterate our class
print T
for i in T:
  print i

print T

# Define new instance vars
T.Test1 = 3
print T.Test1, T.DatabaseName

# delete the table
del T

