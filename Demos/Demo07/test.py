import sys
print "Win version:", sys.winver
import spam
print (spam.foo('hello world', 1))
p = spam.CreatePoint( 10, 25 )
print ("Point:", p)
p.x = 58
print (p.x, p)
p.OffsetBy( 5, 5 )
print (p)
print ("Current value of var test is: ", test)
test.Value = "New value set by Python"
print (spam.getdouble())