
# Webinar: – Python for Delphi Developers – Part II

![P4D Logo](https://github.com/pyscripter/python4delphi/wiki/Images/Python4Delphi-Libraries.png)

- [Webinar Info](https://blogs.embarcadero.com/python-for-delphi-developers-webinar/)

- [Video replay](https://blogs.embarcadero.com/combining-the-strengths-of-delphi-and-python/)

- [Slides](https://www.slideshare.net/embarcaderotechnet/python-for-delphi-developers-part-2)

- Source code is included in this folder.  [Demo33](https://github.com/pyscripter/python4delphi/tree/master/Demos/Demo33) is in the Demos directory and the python extension [module demos](https://github.com/pyscripter/python4delphi/tree/master/Modules/DemoModule) are in the Modules directory.

Please note that to compile the source code you need to install the [SVGIconLibrary](https://github.com/EtheaDev/SVGIconImageList).  
PyChartHTML uses the [TEdgeBrowser](http://docwiki.embarcadero.com/RADStudio/Sydney/en/Using_TEdgeBrowser_Component_and_Changes_to_the_TWebBrowser_Component) control.  So you need to use Delphi 10.4 or later and install the [WebView2 Runtime](https://developer.microsoft.com/en-us/microsoft-edge/webview2/).  The required WebView2Loader.dll is already in the output folder.  Finally the visualization and analytics demos make use of a range of python modules that need to be installed in python using the python package installer pip. These needed modules are listed below:

- numpy
- pandas
- matplotlib
- seaborn
- mpld3
- bokeh
- altair
- scikit-learn
