##########################################################
#     A Lisp engine made by Morgan Martinet (C) 1999                              #
##########################################################

True = 1
False = 0

ELispError = "ELispError"

def inherited(inst):
	return inst.__class__.__bases__[0]


##################################################################
# Types of TLispObject:

tlAnyType = 0
tlInteger = 1
tlFloat = 2
tlNumber = 3
tlString = 4
tlChar = 5
tlBoolean = 6
tlAtom = 7
tlList = 8
tlAssoc = 9

##################################################################

class TLispObject:

	OjectCount = 0

	def __del__(Self):
		TLispObject.OjectCount = TLispObject.OjectCount - 1

	def IsAtom(Self):
		return False

	def IsList(Self):
		return False

	def IsAssoc(Self):
		return False

	def IsNil(Self):
		return False
	
	def IsInteger(Self):
		return False
		
	def IsFloat(Self):
		return False

	def IsNumber(Self):
		return Self.IsInteger() or Self.IsFloat()
		
	def IsString(Self):
		return False

	def IsSymbol(Self):
		return False
		
	def IsFunction(Self):
		return False
		
	def IsTrue(Self):
		return False
		
	def Clone(Self):
		raise ELispError, "Abstract method"

	def Eval(Self, Lisp, StackFrame=None):
		# By default we do nothing !
		return Self
		
##################################################################""
class TAtom(TLispObject):

	def __init__(Self, data ):
		Self._data = data
		TLispObject.OjectCount = TLispObject.OjectCount + 1
		
	def IsAtom(Self):
		return True

	def __str__(Self):
		return str(Self._data)
		
	def IsInteger(Self):
		return type(Self._data) == type(1)
		
	def IsFloat(Self):
		return type(Self._data) == type(1.0)

	def IsString(Self):
		return type(Self._data) == type("")

	def IsTrue(Self):
		return True
		
	def Clone(Self):
		#return Self.__class__( Self._data )
		return Self

class TSymbol(TAtom):
	def IsSymbol(Self):
		return True

	def Eval(Self, Lisp, StackFrame=None):
		return Lisp.FindSymbol( Self._data )
	
class TQString(TAtom):
	def __str__(Self):
		return '"' + str(Self._data) + '"'
		
class TBoolean(TAtom):
	pass
	
class TBoolTrue(TBoolean):
	def __init__(Self):	
		Self._data = 1
		TLispObject.OjectCount = TLispObject.OjectCount + 1

	def __str__(Self):	return "T"
		
class TBoolFalse(TBoolean):
	def __init__(Self):	
		Self._data = 0
		TLispObject.OjectCount = TLispObject.OjectCount + 1

	def __str__(Self):	return "False"

##################################################################""
class TList(TLispObject):

	def __init__(Self, data = None, next = None ):
		Self._data = data
		Self._next = next
		TLispObject.OjectCount = TLispObject.OjectCount + 1

	def IsAtom(Self):
		return Self.IsNil()

	def IsList(Self):
		return True

	def IsAssoc(Self):
		return Self._data != None and Self._next != None and Self._next.IsAtom()

	def IsNil(Self):
		return Self._data ==  Self._next == None

	def Car(Self):
		return Self._data
	
	def Cdr(Self):
		return Self._next

	def Length(Self):
		cpt = 0
		current = Self
		while current != None:
			if current._data != None:
				cpt = cpt + 1
			current = current._next
		return cpt
		
	def IsTrue(Self):
		return not Self.IsNil()
			
	def __str__(Self):
		start = True
		tmp = ""
		current = Self
		while current != None:
			if not start:
				tmp =  tmp + " "
			else:
				start = False
			if current.IsAssoc():
				tmp = tmp + str(current._data) + ' . ' + str(current._next)
				current = None
			elif current._data != None:
				tmp = tmp + str(current._data)
				current = current._next
			else:
				break
		return "("+tmp+")"

	def Clone(Self):
		first = None
		prev = None
		current = Self
		while current != None:
			tmp = TList()
			if current._data != None:
				tmp._data = current._data.Clone()
			if first == None:
				first = tmp
			if prev != None:
				prev._next = tmp
			prev = tmp
			current = current._next
		if first == None:
			return TList()
		else:
			return first
			
	def Eval(Self, Lisp, StackFrame = None):
		if Self.IsNil():	return Self
		if not Self.Car().IsSymbol():
			raise ELispError, str(Self.Car()) + " is not a symbol"
		func = Lisp.FindSymbol( Self.Car()._data )
		if not func.IsFunction():
			raise ELispError, str(Self.Car()._data) + " is not a function"
		sf = TStackFrame()
		sf._Expr = Self
		sf._Func = func
		sf._CurrentArg = Self.Cdr()
		if StackFrame:
			Lisp.AddStackFrame( StackFrame )
		Lisp.AddStackFrame( sf )
		return None
			
class TListNil(TList):
	def __str__(Self):	return "nil"
		
def Nil():	return TListNil()
	
##################################################################""
class TFunction(TAtom):
	
	def __init__(Self):
		Self._data = None
		Self.MinArgs = 0
		Self.MaxArgs = -1 #It means that there's no maximum
		Self.Name = Self.__class__.__name__
		Self.Aliases = [] #other names for this function
		Self.ArgsType = tlAnyType  # a type for all arguments
		Self.ArgsTypeList = [] # a list containing the type for each argument
		Self.Expr = None
		TLispObject.OjectCount = TLispObject.OjectCount + 1
		Self.Init()

	def __str__(Self):
		if Self.IsUserFunc():
			return str(Self.Expr)
		else:
			return "<builtin function " + Self.Name + ">"

	def IsFunction(Self):
		return True

	def Init(Self):
		pass

	def PreProcess(Self, Lisp, StackFrame):
		if not StackFrame._WaitingResult:
			if Lisp._TraceCalls:
				Self.TraceExpr( Lisp, StackFrame )
			Self.CheckArgCount(StackFrame)
	
	def Execute(Self, Lisp, StackFrame):
		Self.PreProcess( Lisp, StackFrame )
		if Self.PrepareArgs(Lisp, StackFrame):
			return
		Self.CheckArgs(StackFrame._Args)
		if Lisp._TraceCalls:
			Self.TraceCall( Lisp, StackFrame )
		Lisp._Result = Self.SelectApply( Lisp, StackFrame )
		if Lisp._TraceCalls:
			Self.TraceResult( Lisp, StackFrame )

	def ContinueArgsEvaluation(Self, StackFrame):
		return True
		
	def SkipArgEvaluation(Self, Lisp, StackFrame, Arg):
		return False
		
	def TraceExpr( Self, Lisp, StackFrame ):
		print "Eval:", StackFrame._Expr
			
	def TraceCall( Self, Lisp, StackFrame ):
		print "Call:", Self.CallAsString( StackFrame._Args )

	def TraceResult( Self, Lisp, StackFrame ):
		print "  -->", Lisp._Result
	
	def CallAsString( Self, Args ):
		tmp = ""
		first = True
		for i in Args:
			if not first:
				tmp = tmp + " "
			tmp = tmp + str(i)
			first = False
		tmp = "(" + Self.Name + " " + tmp + ")"
		return tmp

	def IsUserFunc(Self):
		return False
		
	def IsUserExprFunc(Self):
		return False
		
	def CheckArgCount( Self, StackFrame ):
		if StackFrame._Expr.Cdr():
			length = StackFrame._Expr.Cdr().Length()
		else:
			length = 0
		if Self.MinArgs == Self.MaxArgs:
			if length != Self.MinArgs:
				raise ELispError, Self.Name + " needs " + str(Self.MaxArgs) + " arg(s)"
		
			return None
		if length < Self.MinArgs:
			raise ELispError, Self.Name + " needs at least " + str(Self.MinArgs) + " arg(s)"
		if Self.MaxArgs >= 0:
			if length > Self.MaxArgs:
				raise ELispError, Self.Name + " needs " + str(Self.MaxArgs) + " arg(s)"

	def EvalArg( Self, Lisp, StackFrame, Arg ):
		if not Self.ContinueArgsEvaluation(StackFrame):
			return False
		car = Arg.Car()
		if Self.SkipArgEvaluation( Lisp, StackFrame, Arg ):
			# if we must skip the evaluation of this argument,
			# we simply add it to the list of the evaluated arguments
			StackFrame._Args.append( car )
			Lisp._Result = car
			return False
		result = car.Eval(Lisp, StackFrame)
		if result:
			StackFrame._Args.append( result )
			Lisp._Result = result
			return False
		else:
			StackFrame._WaitingResult = True
			return True
	
	def PrepareArgs( Self, Lisp, StackFrame ):
		if StackFrame._WaitingResult:
			StackFrame._Args.append( Lisp._Result )
		while StackFrame._CurrentArg != None:
			current = StackFrame._CurrentArg
			StackFrame._CurrentArg = current.Cdr()
			if Self.EvalArg( Lisp, StackFrame, current ):
				return True
		return False

	def CheckArg( Self, Arg, ArgType, index ):
		if ArgType == tlAnyType or Arg == None: 
			return
		elif ArgType == tlInteger:
			if not Arg.IsInteger():
				raise ELispError, "%s: argument %d must be of type Integer" % (Self.Name, index+1)
		elif ArgType == tlFloat:
			if not Arg.IsFloat():
				raise ELispError, "%s: argument %d must be of type Float" % (Self.Name, index+1)
		elif ArgType == tlNumber:
			if not Arg.IsNumber():
				raise ELispError, "%s: argument %d must be of type Number" % (Self.Name, index+1)
		elif ArgType == tlString:
			if not Arg.IsString():
				raise ELispError, "%s: argument %d must be of type String" % (Self.Name, index+1)
		elif ArgType == tlChar:
			if not Arg.IsString() or len(Arg._data) != 1:
				raise ELispError, "%s: argument %d must be of type Char" % (Self.Name, index+1)
		elif ArgType == tlBoolean:
			if not Arg.IsNumber() or not (Arg._data in [0,1,0.0,1.0]):
				raise ELispError, "%s: argument %d must be of type Number" % (Self.Name, index+1)
		elif ArgType == tlAtom:
			if not Arg.IsAtom():
				raise ELispError, "%s: argument %d must be of type Atom" % (Self.Name, index+1)
		elif ArgType == tlList:
			if not Arg.IsList():
				raise ELispError, "%s: argument %d must be of type List" % (Self.Name, index+1)
		elif ArgType == tlAssoc:
			if not Arg.IsAssoc():
				raise ELispError, "%s: argument %d must be of type Assoc" % (Self.Name, index+1)

	def CheckAllArgsWith( Self, Args, ArgsType ):
		cpt = 0
		for i in Args:
			Self.CheckArg( i, ArgsType, cpt )
			cpt = cpt + 1
			
	def CheckEachArgWith( Self, Args, ArgsTypeList ):
		cpt = 0
		for i in Args:
			if cpt < len(ArgsTypeList):
				Self.CheckArg( i, ArgsTypeList[cpt], cpt )
			else:
				break
			cpt = cpt + 1
				
	def CheckArgs( Self, Args ):
		if Self.ArgsType != tlAnyType:
			Self.CheckAllArgsWith( Args, Self.ArgsType )
		if len(Self.ArgsTypeList) > 0:
			Self.CheckEachArgWith( Args, Self.ArgsTypeList )

	def SelectApply( Self, Lisp, StackFrame ):
		if Self.MinArgs == Self.MaxArgs:
			args = StackFrame._Args
			if Self.MinArgs == 0:
				return Self.Apply0( Lisp )
			elif Self.MinArgs == 1:
				return Self.Apply1( Lisp,  args[0] )
			elif Self.MinArgs == 2:
				return Self.Apply2( Lisp,  args[0],  args[1] )
			elif Self.MinArgs == 3:
				return Self.Apply3( Lisp,  args[0],  args[1],  args[2] )
			elif Self.MinArgs == 4:
				return Self.Apply4( Lisp,  args[0],  args[1],  args[2],  args[3] )
			elif Self.MinArgs == 5:
				return Self.Apply5( Lisp,  args[0],  args[1],  args[2],  args[3],  args[4] )
			else:
				return Self.ApplyN( Lisp, StackFrame._Args )
		elif Self.MaxArgs == -1: # n arguments
			return Self.ApplyN( Lisp, StackFrame._Args )
		else:
			return Self.Apply( Lisp, StackFrame )
			
	def Apply( Self, Lisp, StackFrame ):
		return Lisp.Nil

	def ApplyN( Self, Lisp, Args ):
		return Lisp.Nil

	def Apply0( Self, Lisp ):
		return Lisp.Nil

	def Apply1( Self, Lisp, A1 ):
		return Lisp.Nil
		
	def Apply2( Self, Lisp, A1, A2 ):
		return Lisp.Nil
		
	def Apply3( Self, Lisp, A1, A2, A3 ):
		return Lisp.Nil
		
	def Apply4( Self, Lisp, A1, A2, A3, A4 ):
		return Lisp.Nil
		
	def Apply5( Self, Lisp, A1, A2, A3, A4 ):
		return Lisp.Nil
		
##################################################################""

class TFuncQuote( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "quote"

	def PrepareArgs( Self, Lisp, StackFrame ):
		StackFrame._Args.append( StackFrame._CurrentArg.Car() )
		
	def Apply1( Self, Lisp, A1 ):
		return A1
		
class TFuncCar( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "car"
		Self.Aliases = ["first"]
		Self.ArgsTypeList = [tlList]

	def Apply1( Self, Lisp, A1 ):
		if A1.Car() == None:
			return Lisp.Nil
		else:
			return A1.Car()
		
class TFuncCdr( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "cdr"
		Self.Aliases = ["rest"]
		Self.ArgsTypeList = [tlList]

	def Apply1( Self, Lisp, A1 ):
		if A1.Cdr() == None:
			return Lisp.Nil
		else:
			return A1.Cdr()
		
class TFuncCons( TFunction ):

	def Init(Self):
		Self.MinArgs = 2
		Self.MaxArgs = 2
		Self.Name = "cons"

	def Apply2( Self, Lisp, A1, A2 ):
		tmp = TList(A1)
		if not A2.IsNil():
			tmp._next = A2
		return tmp
		
#------------------------------------------------------------------------------------------------------------------------------------		
class TFuncOperation( TFunction ):

	def Init(Self):
		Self.MinArgs = 2
		Self.ArgsType = tlNumber

	def ApplyN( Self, Lisp, Args ):
		tmp = Args[0]._data
		for i in Args[1:]:
			tmp = Self.ApplyOp( tmp, i._data )
		return TAtom(tmp)
	
	def ApplyOp( Self, Val1, Val2 ):
		raise ELispError, "abstract method"
		
class TFuncPlus( TFuncOperation ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "+"

	def ApplyOp( Self, Val1, Val2 ):
		return Val1 + Val2
		
class TFuncMinus( TFuncOperation ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "-"

	def ApplyOp( Self, Val1, Val2 ):
		return Val1 - Val2
		
class TFuncMul( TFuncOperation ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "*"

	def ApplyOp( Self, Val1, Val2 ):
		return Val1 * Val2
		
class TFuncDiv( TFuncOperation ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "/"

	def ApplyOp( Self, Val1, Val2 ):
		return Val1 / Val2
		
#------------------------------------------------------------------------------------------------------------------------------------		
class TFuncIf( TFunction ):

	def Init(Self):
		Self.MinArgs = 3
		Self.MaxArgs = 3
		Self.Name = "if"

	def SkipArgEvaluation(Self, Lisp, StackFrame, Arg):
		# We must only evaluate the condition
		# Then we'll choose to continue with the 2nd or the 3rd argument
		# according to condition.
		return len(StackFrame._Args) >= 1

	def Apply3(Self, Lisp, A1, A2, A3):
		result = A1.IsTrue()
		if Lisp._TraceCalls:
			print "Call: (if", A1, A2, A3, ")"
			if result:
				print "  --> Condition is True, choosing ", A2
			else:
				print "  --> Condition is False, choosing ", A3
		if result:
			return A2.Eval(Lisp)
		else:
			return A3.Eval(Lisp)
	
class TFuncWhen( TFunction ):

	def Init(Self):
		Self.MinArgs = 2
		Self.MaxArgs = 2
		Self.Name = "when"

	def SkipArgEvaluation(Self, Lisp, StackFrame, Arg):
		# We must only evaluate the condition
		# Then we'll choose to continue with the 2nd 
		# according to condition.
		return len(StackFrame._Args) >= 1

	def Apply2(Self, Lisp, A1, A2):
		result = A1.IsTrue()
		if Lisp._TraceCalls:
			print "Call: (if", A1, A2, ")"
			if result:
				print "  --> Condition is True, choosing ", A2
			else:
				print "  --> Condition is False, doing nothing "
		if result:
			return A2.Eval(Lisp)
		else:
			return Lisp.Nil
	
class TFuncProgn( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.Name = "progn"

	def ApplyN( Self, Lisp, Args ):
		# nothing to do, because evaluation of the arguments has already been done !
		return Lisp._Result

class TFuncEval( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "eval"

	def Apply1(Self, Lisp, A1):
		return A1.Eval( Lisp )

class TFuncDefun( TFunction ):

	def Init(Self):
		Self.MinArgs = 3
		Self.Name = "defun"

	def Execute(Self, Lisp, StackFrame):
		Self.PreProcess( Lisp, StackFrame )
		e = StackFrame._Expr
		f = TFuncUser()
		f.Expr = e
		e = e.Cdr() # we skip "defun"
		if not e.Car().IsString():
			raise ELispError, "defun: needs a string for the function name"
		f.Name = e.Car()._data
		e = e.Cdr() # we skip the func name
		if not e.Car().IsList():
			raise ELispError, "defun: needs a list of argument after the function name"
		# check if the arguments are strings
		tmp = e.Car()
		while tmp != None:
			if (tmp.Car() != None) and (not tmp.Car().IsSymbol()):
				raise ELispError, "defun: Arguments of a function must be symbols"
			tmp = tmp.Cdr()
		f.MinArgs = e.Car().Length()
		f.MaxArgs = f.MinArgs
		Lisp.AddFunc( f )
		Lisp._Result = TAtom(f.Name)

class TFuncUser( TFunction ):

	def IsUserFunc(Self):
		return True

	def SelectApply( Self, Lisp, StackFrame ):
		# we force the selection of the n arguments evaluation
		return Self.ApplyN( Lisp, StackFrame._Args )
		
	def ApplyN( Self, Lisp, Args ):
		# create a new special function that will handle the evaluation of the user function body
		f = TFuncUserExpr()
		f.Name = "Eval content of " + Self.Name
		f.Expr = Self.Expr
		# prepare the stackframe for this special function
		sf = TStackFrame()
		sf._Func = f
		sf._CurrentArg = Self.Expr.Cdr().Cdr().Cdr() # first function of the body (after the arguments)
		# define the local arguments of the function
		sf._Symb = TSymbols()
		cpt = 0
		current = Self.Expr.Cdr().Cdr().Car() # get the first argument
		# for each argument
		while current != None:
			if current.Car() != None:
				# add it to the local symbols of the function
				sf._Symb.Add( current.Car()._data, Args[cpt] )
				cpt = cpt + 1
			current = current.Cdr()
		# Continue evaluation with our function body
		Lisp.AddStackFrame( sf )
		# Return last result
		return Lisp._Result

class TFuncUserExpr( TFunction ):

	def IsUserExprFunc(Self):
		return True

	def Execute( Self, Lisp, StackFrame ):
		# simply evaluate each item of the function's body
		current = StackFrame._CurrentArg
		if current != None:
			StackFrame._CurrentArg = current.Cdr()
			Lisp._Result = current.Car().Eval( Lisp, StackFrame  )
		
class TFuncList( TFunction ):

	def Init(Self):
		Self.Name = "list"

	def ApplyN( Self, Lisp, Args ):
		first = None
		prev = None
		for i in Args:
			tmp = TList( i )
			if first == None:
				first = tmp
			if prev:
				prev._next = tmp
			prev = tmp
		if first:
			return first
		else:
			return Lisp.Nil
		
class TFuncSet( TFunction ):

	def Init(Self):
		Self.MinArgs = 2
		Self.MaxArgs = 2
		Self.Name = "set"
		Self.ArgsTypeList = [tlString, tlAnyType]

	def Apply2( Self, Lisp, A1, A2 ):
		Lisp.AddObject( A1._data, A2 )
		return A2
		
class TFuncSetq( TFuncSet ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "setq"

	def SkipArgEvaluation(Self, Lisp, StackFrame, Arg):
		# we skip the evaluation of the first argument
		# to avoid quotation of the string symbol
		return len(StackFrame._Args) == 0
		
class TFuncPrint( TFunction ):

	def Init(Self):
		Self.Name = "print"

	def ApplyN( Self, Lisp, Args ):
		i = Lisp.Nil
		for i in Args:
			if i.IsString():
				print i._data,
			else:
				print i,
		print
		return i
		
class TFuncForeach( TFunction ):

	def Init(Self):
		Self.Name = "foreach"

	def ApplyN( Self, Lisp, Args ):
		return Lisp.Nil
#------------------------------------------------------------------------------------------------------------------------------------		
class TFuncLogicalOp( TFunction ):

	def Init(Self):
		Self.MinArgs = 1

	def ApplyOp( Self, Value ):
		raise ELispError, "abstract method"
		
	def ContinueArgsEvaluation(Self, StackFrame):
		lastArg = len(StackFrame._Args)
		if lastArg > 0:
			return Self.ApplyOp( StackFrame._Args[lastArg-1].IsTrue() )
		else:
			return True
		
	def ApplyN( Self, Lisp, Args ):
		lastArg = len(Args)
		return Lisp.BoolAsAtom( lastArg > 0 and Args[lastArg-1].IsTrue() )
		
class TFuncOr( TFuncLogicalOp ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "or"

	def ApplyOp( Self, Value ):
		return not(Value == True)
		
class TFuncAnd( TFuncLogicalOp ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "and"

	def ApplyOp( Self, Value ):
		return Value == True
		
#------------------------------------------------------------------------------------------------------------------------------------		
class TFuncComparison(TFunction):

	def Init(Self):
		Self.MinArgs = 2
		Self.MaxArgs = 2

	def CompareAtoms( Self, A1, A2 ):
		raise ELispError, "abstract method"
		
	def CompareLists( Self, A1, A2 ):
		return False
		
	def Apply2( Self, Lisp, A1, A2 ):
		if A1.IsAtom() and A2.IsAtom():
			return Lisp.BoolAsAtom( Self.CompareAtoms( A1, A2 ) )
		if A1.IsList() and A2.IsList():
			return Lisp.BoolAsAtom( Self.CompareLists( A1, A2 ) )
		else:
			return Lisp.BoolAsAtom( False )

class TFuncEq( TFuncComparison ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "="
		Self.Aliases = ["eq"]

	def CompareAtoms( Self, A1, A2 ):
		return A1._data == A2._data

	def CompareLists( Self, A1, A2 ):
		return A1 is A2
		
class TFuncLt( TFuncComparison ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "<"

	def CompareAtoms( Self, A1, A2 ):
		return A1._data < A2._data
		
class TFuncLe( TFuncComparison ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = "<="

	def CompareAtoms( Self, A1, A2 ):
		return A1._data <= A2._data
		
class TFuncGt( TFuncComparison ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = ">"

	def CompareAtoms( Self, A1, A2 ):
		return A1._data > A2._data
		
class TFuncGe( TFuncComparison ):

	def Init(Self):
		inherited(Self).Init(Self)
		Self.Name = ">="

	def CompareAtoms( Self, A1, A2 ):
		return A1._data >= A2._data
		
#------------------------------------------------------------------------------------------------------------------------------------		
class TFuncAtom( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "atom"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsAtom() )
		
class TFuncNumberp( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "numberp"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsNumber() )
		
class TFuncListp( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "listp"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsList() )
		
class TFuncStringp( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "stringp"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsString() )
		
class TFuncZerop( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "zerop"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsNumber() and (A1._data == 0) )
		
class TFuncNull( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "null"

	def Apply1( Self, Lisp, A1 ):
		return Lisp.BoolAsAtom( A1.IsNil() )
		
class TFuncLength( TFunction ):

	def Init(Self):
		Self.MinArgs = 1
		Self.MaxArgs = 1
		Self.Name = "length"
		Self.ArgsTypeList = [tlList]

	def Apply1( Self, Lisp, A1 ):
		return TAtom( A1.Length() )
		

#------------------------------------------------------------------------------------------------------------------------------------		
# créer cond, equal, map, apply, lamda, let, while...
# (dotimes (symbol value) ...) -> Nil  = for symbol = 0 to value - 1 do ...
#(loop ...) / (return value)
#(dolist (symbol list) ...)
#(do ((var1 init1 step1) (var2 init2 step2) ...) (end-test result) statement1 ...)
# count, reverse, append

##################################################################"" 06 16 05 69 15
class TSymbols:

	def __init__(Self):
		Self._Dict = {}
	
	def Add( Self, Name, Value ):
		Self._Dict[Name] = Value
		
	def Find( Self, Name ):
		try:
			return Self._Dict[Name]
		except:
			raise ELispError, "Undefined symbol '%s'" % (Name,)
		
##################################################################""
class TStackFrame:
	def __init__(Self):
		Self._Func = None
		Self._Args = []
		Self._Symb = None
		Self._Expr = None
		Self._WaitingResult = False

	def Print(Self):
		print Self._Expr
		
##################################################################""
class TStack:
	def __init__(Self):
		Self._List = []
	
	def Push( Self, Frame ):
		Self._List.append( Frame )
	
	def Pop( Self ):
		x = Self._List[len(Self._List)-1]
		del Self._List[len(Self._List)-1]
		return x

	def IsEmpty(Self):
		return len(Self._List) == 0

	def Clear(Self):
		Self._List = []

	def Length(Self):
		return len(Self._List)

##################################################################
EParserError = "EParserError"
DIGITS = ['0','1','2','3','4','5','6','7','8','9']
SEPARATORS = ['(',')',' ',"'","\n","\t",';']

class TParser:
	def __init__(Self, Text):
		Self._Text = Text
		Self._CurrentPos = 0
		Self._Line = 1
		Self._Col = 1
		Self._ParsingString = False

	def EOF(Self):
		return Self._CurrentPos > len(Self._Text)-1
		
	def PeekChar(Self):
		if Self.EOF():
			return ""
		else:
			return Self._Text[Self._CurrentPos]
			
	def GetChar(Self):
		if Self.EOF():
			raise EParserError, "EOF"
		else:
			tmp = Self._Text[Self._CurrentPos]
			if tmp == "\n":
				Self._Line = Self._Line + 1
				Self._Col = 1
			else:
				Self._Col = Self._Col + 1
			Self._CurrentPos = Self._CurrentPos + 1
			return tmp

	def SkipComment(Self):
		while Self.PeekChar() != "\n":
			Self.GetChar()
		Self.GetChar() # skip the CR-LF
	
	def SkipBlank(Self):
		while Self.PeekChar() in [" ", "\t", "\n", ";"]:
			c = Self.GetChar()
			if c == ";":
				Self.SkipComment()
			
	def Parse(Self):
		Self.SkipBlank()
		if Self.EOF():
			raise EParserError, "EOF"
		c = Self.PeekChar()
		if c == "(":
			return Self.ParseList()
		elif c == "'":
			return Self.ParseQuote()
		elif c == '"':
			return Self.ParseQString()
		else:
			return Self.ParseAtom()

	def ParseList(Self):
		Self.GetChar() # pass the (
		first = None
		prev = None
		Self.SkipBlank()
		while (not Self.EOF()) and (Self.PeekChar() != ')'):
			Self.SkipBlank()
			try:
				tmp = TList(Self.Parse())
			except:
				raise EParserError, "EOF while parsing a list"
			if first == None:
				first = tmp
			if prev != None:
				prev._next = tmp
			prev = tmp
		if Self.EOF():
			raise EParserError, "EOF while parsing a list"
		Self.GetChar() # we skip the )
		Self.SkipBlank()
		if first != None:
			return first
		else:
			return TList()

	def ParseQuote(Self):
		Self.GetChar() # pass the quote
		expr = Self.Parse()
		return TList( TSymbol("quote"), TList(expr) )
		
	def ParseQString(Self):
		Self.GetChar() # pass the "
		tmp = ""
		while  Self.PeekChar() != '"':
			tmp = tmp + Self.GetChar()
		Self.GetChar() # pass the "
		return TList( TSymbol("quote"), TList(TQString(tmp)) )
		
	def ParseAtom(Self):
		c = Self.PeekChar()
		if c == '-':
			Self.GetChar()
			if Self.PeekChar() in DIGITS:
				return Self.ParseNumber(True)
			else:
				return TAtom( "-" )
		elif c in DIGITS:
			return Self.ParseNumber()
		else:
			return Self.ParseString()

	def ParseNumber(Self, Minus = False):
		if Minus:
			tmp = "-"
		else:
			tmp = ""
		while (not Self.EOF()) and (not (Self.PeekChar() in SEPARATORS)):
			tmp = tmp + Self.GetChar()
		if '.' in tmp:
			val = float(tmp)
		else:
			val = int(tmp)
		return TAtom(val)
		
	def ParseString(Self):
		tmp = ""
		while (not Self.EOF()) and (not (Self.PeekChar() in SEPARATORS)):
			tmp = tmp + Self.GetChar()
		Self.SkipBlank()
		return TSymbol(tmp)
	
##################################################################
class TLisp:

	def __init__(Self):
		Self._Stack = TStack()
		Self._CurrentStackFrame = None
		Self._Symbols = TSymbols()
		Self._Result = None
		Self._TraceCalls = False
		Self._DumpStack = True
		Self._MaxStackDump = 50
		Self.MaxStackSize = 10000
		Self.InitSymbols()

	def AddFunc( Self, Func ):
		Self._Symbols.Add( Func.Name, Func )
		for i in Func.Aliases:
			Self._Symbols.Add( i, Func )

	def AddObject( Self, Name, Value ):
		Self._Symbols.Add( Name, Value )
	
	def InitSymbols(Self):
		# Init procedures
		Self.AddFunc( TFuncQuote() )
		Self.AddFunc( TFuncCar() )
		Self.AddFunc( TFuncCdr() )
		Self.AddFunc( TFuncCons() )
		Self.AddFunc( TFuncPlus() )
		Self.AddFunc( TFuncMinus() )
		Self.AddFunc( TFuncMul() )
		Self.AddFunc( TFuncDiv() )
		Self.AddFunc( TFuncIf() )
		Self.AddFunc( TFuncWhen() )
		Self.AddFunc( TFuncProgn() )
		Self.AddFunc( TFuncEval() )
		Self.AddFunc( TFuncDefun() )
		Self.AddFunc( TFuncList() )
		Self.AddFunc( TFuncSet() )
		Self.AddFunc( TFuncSetq() )
		Self.AddFunc( TFuncPrint() )
		Self.AddFunc( TFuncForeach() )
		Self.AddFunc( TFuncOr() )
		Self.AddFunc( TFuncAnd() )
		Self.AddFunc( TFuncEq() )
		Self.AddFunc( TFuncLt() )
		Self.AddFunc( TFuncLe() )
		Self.AddFunc( TFuncGt() )
		Self.AddFunc( TFuncGe() )
		Self.AddFunc( TFuncAtom() )
		Self.AddFunc( TFuncNumberp() )
		Self.AddFunc( TFuncListp() )
		Self.AddFunc( TFuncStringp() )
		Self.AddFunc( TFuncZerop() )
		Self.AddFunc( TFuncNull() )
		Self.AddFunc( TFuncLength() )
		# Init globals
		Self.Nil = Nil()
		Self.True = TBoolTrue()
		Self.False = Self.Nil
		Self.AddObject( "nil", Self.Nil )
		Self.AddObject( "NIL", Self.Nil )
		Self.AddObject( "True", Self.True )
		Self.AddObject( "T", Self.True )
		Self.AddObject( "t", Self.True )
		Self.AddObject( "False", Self.False )

	def AddStackFrame( Self, StackFrame ):
		Self.CheckStackSize()
		Self._Stack.Push( StackFrame )

	def GetStackFrame( Self ):
		return Self._Stack.Pop()

	def EvalLoop(Self):
		while not Self._Stack.IsEmpty():
			Self._CurrentStackFrame = Self.GetStackFrame()
			Self._CurrentStackFrame._Func.Execute( Self, Self._CurrentStackFrame )
	
	def EvalLispExpr(Self, expr):
		Self._Result = None
		Self._CurrentStackFrame = None
		Self._Stack.Clear()
		Self._Result = expr.Eval(Self)
		Self.EvalLoop()
		return Self._Result
	
	def Eval(Self, expr):
		result = None
		P = TParser(expr)
		while not P.EOF():
			try:
				tmp = P.Parse()
				P.SkipBlank()
			except:
				print "While parsing: ", expr
				raise
			try:
				result = Self.EvalLispExpr(tmp)
			except:
				if Self._DumpStack:
					Self.PrintStack()
				raise
		return result

	def FindSymbolInStackFrame(Self, Name, StackFrame):
		result = None
		if StackFrame and StackFrame._Symb != None:
			try:
				result = StackFrame._Symb.Find(Name)
			except:
				result = None
		return result
		
	def FindLocalSymbol(Self, Name):
		result = Self.FindSymbolInStackFrame( Name, Self._CurrentStackFrame )
		if result: return result
		cpt = Self._Stack.Length() - 1
		while cpt >= 0:
			sf = Self._Stack._List[cpt]
			if sf._Symb != None:
				result = Self.FindSymbolInStackFrame( Name, sf )
				if result:
					break
			if sf._Func.IsUserExprFunc():
				break
			cpt = cpt - 1
		return result
		
	def FindSymbol(Self, Name):
		tmp = Self.FindLocalSymbol( Name )
		if tmp == None:
			tmp = Self._Symbols.Find( Name )
		return tmp

	def CheckStackSize(Self):
		if Self._Stack.Length() > Self.MaxStackSize:
			raise ELispError, "Lisp stack overflow"

	def PrintStack(Self):
		print "<============= Stack dump ====================>"
		if Self._CurrentStackFrame != None:
			Self._CurrentStackFrame.Print()
		cpt = 0
		idx = len(Self._Stack._List)-1
		while idx >= 0:
			i = Self._Stack._List[idx]
			i.Print()
			cpt = cpt + 1
			idx = idx - 1
			if cpt > Self._MaxStackDump:
				print "..."
				break
		print "<============= End of Stack dump ==============>"

	def BoolAsAtom(Self, Value):
		if Value:
			return Self.True
		else:
			return Self.False
			
##################################################################
L = TLisp()
#L._TraceCalls = True
print L.Eval( "(/ 2.0 (+ 3 1 (+ T 1)) 2)" )
print L.Eval( "(cdr '(1 2 nil 3))" )
print L.Eval( "(if (+ 0 0 0) True False)" )
print L.Eval( "(if T (progn (+ 2 2) (+ 1 1)) False)" )
print L.Eval( "(defun foo (a b c) (+ a b c))" )
print L.Eval( "(foo 1 2 3)" )
print L.Eval( "(cons '(1 2) '(3 4))" )
tmp = """
; merge takes two lists as argument and concatenates them
(defun merge (L1 L2) 
    (if L1 
        (cons (car L1) (merge (cdr L1) L2)) 
        L2
    )
)

; reverse takes a list as argument and reverses its content
(defun reverse (L) 
    (if L 
        (merge (reverse (cdr L)) (list (car L))) 
        nil
    )
)
"""
print L.Eval( tmp )
print L.Eval( "(reverse '(1 2 3))" )
print L.Eval( "(merge '(1 2) '(3 4))" )
print L.Eval( "(rest (first (cons (cons 1 2) 3)))" )
print L.Eval( "(setq z 33)" )
print L.Eval( "(set 'test '(+ 1 z 3))" )
print L.Eval( "(set 'test2 test)" )
print L.Eval( "(eval test)" )
print L.Eval( """ '(1 "Hello world !" T)""" )
print L.Eval( '(print "and is " (and "1" (+ 2 2) 3))' )
print L.Eval( "(> 3 4)" )
#print L.Eval( '(foreach (i test) (print "i =" i))' )
del L
print "Lisp objects : ", TLispObject.OjectCount

# To do:
#parse assoc (1 . 2)
