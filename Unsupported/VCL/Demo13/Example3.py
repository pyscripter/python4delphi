import DB
import DBTables
True = 1
False = 0

# Get the TTable object created by Delphi 
# and connected to a Delphi Datasource
# It is done when you click on Execute
T = DBTables.T
# Select and open a table
T.DatabaseName = "DBDemos"
T.TableName = "customer.db"
T.Open()

# Display columns
print "Columns: "
for i in T.FieldNamesAsTuple():
  print "   ", i

# For each record of the table
T.First()
while not T.EOF:
  # Get all the fields in a tuple
  A = T.FieldsAsTuple()
  # Print the current record number and the tuple
  print "Rec.", T.RecNo, ":",  A
  # Get next record
  T.Next()

# Print some fields by their names
print T.FieldByName("Company")

# check state
if not T.State in [DB.dsEdit, DB.dsInsert]:
  print "Table is not edited"

# clean up
del T
del DBTables.T
