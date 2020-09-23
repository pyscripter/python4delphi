import DBFireDac
from datetime import datetime

# Get the TTable object associated to
# the delphi table displayed on right
# It is done when you click on Execute
T = DBFireDac.T

# Display columns
print ("Columns: ")
for i in T.FieldNamesAsTuple():
  print ("   ", i)

# For each record of the table
print ("Company name for each record of table : ")
T.First()
while not T.EOF:
  # Print the current record number and the Company
  print ("Rec.", T.RecNo, "; Company: ", T.FieldByName("Company").Value)
  # Get next record
  T.Next()

# check state
if not T.State in [DBFireDac.dsEdit, DBFireDac.dsInsert]:
  print ("Table is not edited")

# Find and edit a record
T.IndexName = "SK1_CUSTOMER"
if T.FindKey( ['Unisco'] ):
  print ("Unisco found !")
  T.Edit()
  T.FieldByName('ADDR2').AsString = 'Egal'
  T.FieldByName('LASTINVOICEDATE').AsDateTime = datetime.today()
  T.Post()
  print ("New values for ADDR2='", T.FieldByName('ADDR2').AsString, "'and LASTINVOICEDATE=", T.FieldByName('LASTINVOICEDATE').AsString)
else:
  print ("Could not find Unisco !")

# New Company: Append or Delete
if T.FindKey( ['Test-Company'] ):
  # Delete record
  T.Delete()
  print ("New Company 'Test-Company' deleted !")
else:
  # New record
  T.Append()  
  T.FieldByName('COMPANY').AsString = 'Test-Company'
  T.FieldByName('ADDR1').AsString = 'Marktplatz 1'
  T.FieldByName('CITY').AsString = 'Köln'
  T.FieldByName('LASTINVOICEDATE').AsDateTime = datetime.today()
  T.Post()
  print ("New Company 'Test-Company' created !")

 
