unit PythonException;

interface

uses
  SysUtils;

type
//#######################################################
//##                                                   ##
//##         New exception classes                     ##
//##                                                   ##
//#######################################################

  // Components' exceptions
  EPythonComponent = class(Exception)
  end;
  EDLLLoadError  = class(EPythonComponent);
  EDLLImportError = class(EPythonComponent)
    public
      WrongFunc : AnsiString;
      ErrorCode : Integer;
  end;

  // Python's exceptions
  EPythonError   = class(Exception)
    public
      EName : string;
      EValue : string;
  end;
  EPyExecError   = class(EPythonError);

// Standard exception classes of Python

{ Hierarchy of Python exceptions, Python 2.3, copied from <INSTALL>\Python\exceptions.c

Exception\n\
 |\n\
 +-- SystemExit\n\
 +-- StopIteration\n\
 +-- StandardError\n\
 |    |\n\
 |    +-- KeyboardInterrupt\n\
 |    +-- ImportError\n\
 |    +-- EnvironmentError\n\
 |    |    |\n\
 |    |    +-- IOError\n\
 |    |    +-- OSError\n\
 |    |         |\n\
 |    |         +-- WindowsError\n\
 |    |         +-- VMSError\n\
 |    |\n\
 |    +-- EOFError\n\
 |    +-- RuntimeError\n\
 |    |    |\n\
 |    |    +-- NotImplementedError\n\
 |    |\n\
 |    +-- NameError\n\
 |    |    |\n\
 |    |    +-- UnboundLocalError\n\
 |    |\n\
 |    +-- AttributeError\n\
 |    +-- SyntaxError\n\
 |    |    |\n\
 |    |    +-- IndentationError\n\
 |    |         |\n\
 |    |         +-- TabError\n\
 |    |\n\
 |    +-- TypeError\n\
 |    +-- AssertionError\n\
 |    +-- LookupError\n\
 |    |    |\n\
 |    |    +-- IndexError\n\
 |    |    +-- KeyError\n\
 |    |\n\
 |    +-- ArithmeticError\n\
 |    |    |\n\
 |    |    +-- OverflowError\n\
 |    |    +-- ZeroDivisionError\n\
 |    |    +-- FloatingPointError\n\
 |    |\n\
 |    +-- ValueError\n\
 |    |    |\n\
 |    |    +-- UnicodeError\n\
 |    |        |\n\
 |    |        +-- UnicodeEncodeError\n\
 |    |        +-- UnicodeDecodeError\n\
 |    |        +-- UnicodeTranslateError\n\
 |    |\n\
 |    +-- ReferenceError\n\
 |    +-- SystemError\n\
 |    +-- MemoryError\n\
 |\n\
 +---Warning\n\
      |\n\
      +-- UserWarning\n\
      +-- DeprecationWarning\n\
      +-- PendingDeprecationWarning\n\
      +-- SyntaxWarning\n\
      +-- RuntimeWarning\n\
      +-- FutureWarning"
}
   EPyException = class (EPythonError);
   EPyStandardError = class (EPyException);
   EPyArithmeticError = class (EPyStandardError);
   EPyLookupError = class (EPyStandardError);
   EPyAssertionError = class (EPyStandardError);
   EPyAttributeError = class (EPyStandardError);
   EPyEOFError = class (EPyStandardError);
   EPyFloatingPointError = class (EPyArithmeticError);
   EPyEnvironmentError = class (EPyStandardError);
   EPyIOError = class (EPyEnvironmentError);
   EPyOSError = class (EPyEnvironmentError);
   EPyImportError = class (EPyStandardError);
   EPyIndexError = class (EPyLookupError);
   EPyKeyError = class (EPyLookupError);
   EPyKeyboardInterrupt = class (EPyStandardError);
   EPyMemoryError = class (EPyStandardError);
   EPyNameError = class (EPyStandardError);
   EPyOverflowError = class (EPyArithmeticError);
   EPyRuntimeError = class (EPyStandardError);
   EPyNotImplementedError = class (EPyRuntimeError);
   EPySyntaxError = class (EPyStandardError)
   public
      EFileName: UnicodeString;
      ELineStr: UnicodeString;
      ELineNumber: Integer;
      EOffset: Integer;
   end;
   EPyIndentationError = class (EPySyntaxError);
   EPyTabError = class (EPyIndentationError);
   EPySystemError = class (EPyStandardError);
   EPySystemExit = class (EPyException);
   EPyTypeError = class (EPyStandardError);
   EPyUnboundLocalError = class (EPyNameError);
   EPyValueError = class (EPyStandardError);
   EPyUnicodeError = class (EPyValueError);
   UnicodeEncodeError = class (EPyUnicodeError);
   UnicodeDecodeError = class (EPyUnicodeError);
   UnicodeTranslateError = class (EPyUnicodeError);
   EPyZeroDivisionError = class (EPyArithmeticError);
   EPyStopIteration = class(EPyException);
   EPyWarning = class (EPyException);
   EPyUserWarning = class (EPyWarning);
   EPyDeprecationWarning = class (EPyWarning);
   PendingDeprecationWarning = class (EPyWarning);
   FutureWarning = class (EPyWarning);
   EPySyntaxWarning = class (EPyWarning);
   EPyRuntimeWarning = class (EPyWarning);
   EPyReferenceError = class (EPyStandardError);
 {$IFDEF MSWINDOWS}
   EPyWindowsError = class (EPyOSError);
 {$ENDIF}

implementation

end.
