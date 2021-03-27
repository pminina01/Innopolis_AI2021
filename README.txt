HOW TO RUN THE CODE:
Preconditions: 
	you need to have SWI-Prolog on your computer:
	- download https://www.swi-prolog.org/download/stable
	- put it in PATH using tutorial https://docs.microsoft.com/en-us/previous-versions/office/developer/sharepoint-2010/ee537574(v=office.14)
	- if you use VSCode, then put PATH 'C:\Program Files\swipl\bin\swipl-win.exe' 
	into file->settings->parameters->extentions->VCS-Prolog-Prolog Executable PATH)

1) Open command line in the same directory as source code
   or use VS Code terminal with installed SWI-Prolog.
2) In command line (usual terminal or VSCode terminal) type: 
	swipl main.pl
3) Then type one of the next commands:

	start("no_random").
		- Use data for map from input.txt file (you can change it).

	start("random").
		- Create random map and save it in program as facts.

HOW TO INPUT DATA:
Initially input.txt have next data:
	size(9).
	home(7,4).
	covid(3,7).
	covid(5,4).
	doctor(2,5).
	mask(1,6).
You can change data in the input.txt file, but respecting the format described below and in the assignment:
	size(n). - n>=6 
	home(a,b). - 1<a<n and 1<b<n
	covid(c,d). - 1<c<n and 1<d<n
	covid(e,f). - 1<e<n and 1<f<n
	doctor(g,h). - 1<g<n and 1<h<n
	mask(k,t). - 1<k<n and 1<t<n