! Replace S:\Core\Programs\Web
 
	autoLibrary
 
	dim msgline$(3)*60,response$(5)*1,cap$*128
 
	cap$="ACS User's Website"
	msgline$(1)="Do you wish open the web site:"
	msgline$(2)="http://planetacs.net/user"
	msgline$(3)="in your default browser?"
	fnmsgbox(mat msgline$,resp$,cap$,3)
	if resp$="No" or resp$="Cancel" then goto Xit
	execute "sy start http://planetacs.net/user/index.htm"
Xit: fnXit("")
 
include: Ertn
