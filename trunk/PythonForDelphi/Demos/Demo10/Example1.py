import DBTables
True = 1
False = 0

# Create and open the table
T = DBTables.CreateTTable()
T.DatabaseName = "DBDemos"
T.TableName = "customer.db"
T.Active = True  #or use T.Open()

# Display columns
print "Columns: ", T.FieldNamesAsTuple()

LastInvoiceDate = T.FieldByName("LastInvoiceDate")

# For each record of the table
T.First()
while not T.EOF:
  # Get all the fields in a list
  A = []
  for i in xrange( T.FieldCount ):
    A.append( T.Fields(i).Value )
  # Print the current record number and the list
  print "Rec.", T.RecNo, ":",  A
  # Edit record
  T.Edit()
  T.FieldByName("TaxRate").Value = 2.5
  # Increments date by one day
  D = LastInvoiceDate.Value
  if D[2] < 28:
    D2 = D[:2]+(D[2]+1,)+D[3:] # this is tuple arithmetic !
  else:
    D2 = D[:2]+(1,)+D[3:] # this is tuple arithmetic !
  LastInvoiceDate.Value = D2
  T.Post()
  # Get next record
  T.Next()

# Print some fields by their names
F = T.FieldByName("Company")
print F, F.FieldName, "=", F.Value

# Use the TFields and print the list of companies
T.First()
while not T.EOF:
  print F.AsString
  T.Next()

# Print the fields as a dictionary
D = T.FieldsAsDict()
print D
print D["Company"]

T.Close()
T.TableName = "Dummy.db"
try:
  T.Open()
except DBTables.DBError:
  print "could not open table ", T.TableName

#delete fields
del F
del LastInvoiceDate

# delete the table
del T
