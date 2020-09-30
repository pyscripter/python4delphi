# Create Python extension modules using P4D

You can use P4D to create Python extension modules that expose your own classes and functions to the Python interpreter. You can package your extension with setuptools and distribute it through [PyPi](https://pypi.org/).

- The project in the subdirectory Delphi generates a Python extension module (a dynamic link library with extension "pyd" in Windows) that allows you to create user interfaces using  delphi from within python.  The whole VCL (almost) is wrapped with a few lines of code! The small demo TestApp.py gives you a flavour of what is possible.   The machinery by which this is achieved is the WrapDelphi unit.  

 - The subdirectory DemoModule demonstrates how to create Python extension modules using Delphi, that allow you to use in Python, functions defined in Delphi.  Compile the project and run test.py from the command prompt (e.g. py test.py). The generated pyd file should be in the same directory as the python file.  This project should be easily adapted to use with Lazarus and FPC.

 - The subdirectory RttiModule contains a project that does the same as the DemoModule, but using extended RTTI to export Delphi functions.  This currently is not supported by FPC.  