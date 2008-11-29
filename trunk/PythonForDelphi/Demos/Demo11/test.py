from SortModule import getvalue,swap
 
def SortFunc1(handle, low, high):
  for i in range(low, high):
    for j in range(low+1,high):
      if getvalue(handle,j-1) > getvalue(handle,j):
        swap(handle,j-1,j)

def SortFunc2(handle,low,high):
  for i in range(low,high-1):
    for j in range(i+1,high):
      if getvalue(handle,i) > getvalue(handle,j):
        swap(handle,i,j)

def SortFunc3(handle,low,high):
  Lo = low
  Hi = high-1
  Mid = getvalue(handle,(Lo+Hi) / 2)
  while 1:
    while getvalue(handle,Lo) < Mid: 
      Lo = Lo + 1
    while getvalue(handle,Hi) > Mid:
      Hi = Hi - 1
    if Lo <= Hi:
      swap(handle,Lo,Hi)
      Lo = Lo + 1
      Hi = Hi - 1
    if Lo > Hi:
      break
  if Hi > low:
    SortFunc3(handle,low,Hi+1)
  if Lo < high-1:
    SortFunc3(handle,Lo,high)
