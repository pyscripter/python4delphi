##############################################################
##          This is the Python for Delphi project           ##
##############################################################

AUTHORS:
~~~~~~~~
Dr. Dietmar Budelsky
    dbudelsky@web.de

Morgan Martinet
    4723, Brebeuf
    H2J 3L2 MONTREAL (QC)
    CANADA

    e-mail: p4d@mmm-experts.com
            

This project is the result of the merging of their two separate projects.

PURPOSE:
~~~~~~~~
  Provide an interface to the Python language in Delphi.

COPYRIGHT:
~~~~~~~~~~
  This source code is distributed with no WARRANTY, for no reason or use.
  Everyone is allowed to use and change this code free for his own tasks
  and projects, as long as this header and its copyright text is intact.
  For changed versions of this code, which are public distributed the
  following additional conditions have to be fullfilled:
  1) The header has to contain a comment on the change and the author of it.
  2) A copy of the changed source has to be sent to one of the above E-Mail
     addresses or our then valid address, if this is possible to the
     author.
  The second condition has the target to maintain an up to date central
  version of the components. If this condition is not acceptable for
  confidential or legal reasons, everyone is free to derive components
  or to generate a diff file to our or other original sources.

FILES:
~~~~~~
  Readme.txt                   This file.
  Python.txt                   Infos about Python, and further references.
  Changes.txt                  List of all changes since the first release.
  Tutorial.txt                 A simple tutorial to use the PythonEngine
  To do.txt                    A to do list.
  Deploying P4D.pdf            Notes on the Deployment of your applications using Python for Delphi.
  C++ Builder Notes.txt        Notes on using C++Builder with the Python for Delphi components.
  PythonAtom.hlp               A help file explaining the use of TPythonAtom
  Demos                        A folder containing several demos of Python for Delphi.
  Components\Python.*          The "Python for Delphi" packages.
  Components\Sources\Core      The source folder of the core "Python for Delphi".
  Lib                          Library of Python modules.
  PythonIDE                    A Python developpment environment written in Delphi.
                               See PythonIDE\Readme.txt for the required components.
  Modules                      Contains the Delphi\Delphi.dpr  project that creates the Modules\Delphi.pyd Python module
                               that allows you to interact with Delphi VCL objects from Python.

INSTALLATION:
~~~~~~~~~~~~~
  install the Python for Windows distribution (http://www.python.org/).

  1) Install the core components
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  For recent versions of Delphi, install the "Python_d" package located in the 
  Components folder and add the folder "...\Components\Sources\Core".

  For Delphi 3, install the "Python_d3" package located in the Components folder
  and add the folder "...\Components\Sources\Core".

  For Delphi 4, install the "Python_d4" package located in the Components folder
  and add the folder "...\Components\Sources\Core".

  For Delphi 5, install the "Python_d5" package located in the Components folder
  and add the folder "...\Components\Sources\Core".

  For Delphi 6, install the "Python_d6" package located in the Components folder
  and add the folder "...\Components\Sources\Core".

  For Delphi 7, install the "Python_d7" package located in the Components folder
  and add the folder "...\Components\Sources\Core".

  2) Install the VCL components (this is optional)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  For recent versions of Delphi, install the "PythonVCL_d" package located in the 
  Components folder and add the folder "...\Components\Sources\Core".

  For Delphi 3, install the "PythonVCL_d3" package located in the Components folder
  and add the folder "...\Components\Sources\VCL".

  For Delphi 4, install the "PythonVCL_d4" package located in the Components folder
  and add the folder "...\Components\Sources\VCL".

  For Delphi 5, install the "PythonVCL_d5" package located in the Components folder
  and add the folder "...\Components\Sources\VCL".

  For Delphi 6, install the "PythonVCL_d6" package located in the Components folder
  and add the folder "...\Components\Sources\VCL".

  For Delphi 7, install the "PythonVCL_d7" package located in the Components folder
  and add the folder "...\Components\Sources\VCL".


  3) Build Modules\Delphi\Delphi.dpr (This is optional and unsupported)
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Once the project is build you can either extend the Python path with ..\Modules or
  copy ..Modules\Delphi.pyd to C:\Python24\DLLs, to be able to import the Delphi module
  from Python.

  Note that you can try this module by invoking the ..\Modules\TestApp.py script.

NOTE:
~~~~~
  Don't forget to look at the tutorial file (tutorial.txt) which introduces the
  demos.

DISTRIBUTION:
~~~~~~~~~~~~~
  You are free to distribute your applications built with Python for Delphi,
  but don't forget to install the python15.dll in the window\system folder of
  your client, or install the Python for windows distribution (it needs more
  space on the harddisk, but it's better because there are all Python modules
  and the help).
