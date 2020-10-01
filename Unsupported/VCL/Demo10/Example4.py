import DBTables
True = 1
False = 0

# Create and open the table
T = DBTables.CreateTQuery()
T.DatabaseName = "DBDemos"
T.SQL = [
  "SELECT *",
  "  FROM customer.db"
        ]
T.Open()

# Display columns
print "Columns: ", T.FieldNamesAsTuple()

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
print D
print D["Company"]

# delete the table
del T

