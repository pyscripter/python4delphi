You should replace D? by the Delphi version you're using : D3 or D4

1 ***********************************************************************************
If you want to try the VCL encapsulation inside Python:
- if you don't use the default python.dpk package, install the component
  PythonVclStd.pas from "Components\sources\VCL\D?" 
- check if the lib path contains a reference to the folder "...\Components\sources\VCL\D?"
- Open the project "PyDelphi"
- execute it
- Open the file "D?\SetPath.py"
- edit the path to reflect your folder "...\lib\Delphi\D?"
- close it
- Open the file "D?\test.py"
- execute it and look at the form !
- Now examine the source code and see how close to Delphi we are !
- Have a look at the file log.txt, it gives you informations about methods that could
  not be implemented.

2 ***********************************************************************************
If you want to use the VCL encapsulation inside your apps, then just drop
the component PythonVclStd on your form and connect it to the PythonEngine.
Be sure that the Python path has an entry on the directory containing the
Delphi classes for Python ("...\lib\Delphi\D?"). You can do what the SetPath.py does,
or update the registry key:
HKEY_LOCAL_MACHINE\SOFTWARE\Python\PythonCore\1.5.0\PythonPath.
Or you can copy the "...\lib\Delphi\D?" in the lib directory of Python.

3 ***********************************************************************************
If you want to rebuild the VCL stuff, then execute the "VCL Generator",
load the "Standard_d?.prj" file.
Go in "Edit\Settings" and check the folders.
At last, click on "Execute\Generate".
In Delphi, recompile the Python components.

You can use this programm to generate an interface for your own components.
Create a new project, go in "Edit\Settings" and define the name of the component that
you will drop on your form (like PythonVclStd) and define the name of the unit that
will host this component.
Add the needed delphi units with "Edit\Add Delphi unit" (note: you must add all the
units that are used by your interface, as Classes.pas, forms.pas...)
Save your project and generate it.
In Delphi, add the new component to your Python package and compile it.
