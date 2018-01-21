import DBFireDac

# Create and open the Query
Q = DBFireDac.TFDQuery(None)
Q.ConnectionDefName = "SQLite_DB"
Q.SQL.Text = """select *
from ORDERS
"""

Q.Open()  # or Q.Active = True

# Display columns
print ("Columns: ", Q.FieldNamesAsTuple())
# For each record of the table
Q.First()
while not Q.EOF:
  # Get all the fields in a list
  A = []
  for i in range( 0, Q.FieldCount ):
    A.append( Q.Fields(i).AsString )
    # Print the current record number and the list
  print ("Rec.", Q.RecNo, ":",  A)
  # Get next record
  Q.Next()
#
Q.Close()

# delete the Query
del Q

