import DBFireDac

# Create and open the table
T = DBFireDac.TFDTable(None)
T.ConnectionDefName = "SQLite_DB"
T.TableName = "Customer"
T.Active = True  #or use T.Open()

# Display columns
print ("Columns: ", T.FieldNamesAsTuple())

LastInvoiceDate = T.FieldByName("LastInvoiceDate")

# For each record of the table
T.First()
while not T.EOF:
  # Get all the fields in a list
  A = []
  for i in range( 0, T.FieldCount ):
    A.append( T.Fields(i).Value )
  # Print the current record number and the list
  print ("Rec.", T.RecNo, ":",  A)
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
print (F, F.FieldName, "=", F.Value)

# Use the TFields and print the list of companies
T.First()
while not T.EOF:
  print (F.AsString)
  T.Next()

# Print the fields as a dictionary
D = T.FieldsAsDict()
print (D)
print ("COMPANY:", D["COMPANY"])    # <- case-sensitive

T.Close()

T.TableName = "Dummy"
try:
  T.Open()
except DBFireDac.DBError:
  print ("could not open table ", T.TableName)

#delete fields
del F
del LastInvoiceDate

# delete the table
del T
