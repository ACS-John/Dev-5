! Replace work\signature
autoLibrary
dim docname$*30
dim msgline$(5)*200
dim cddrive$*1
dim response$(5)*30
open #1: "Name=S:\Core\Docnames,KFName=S:\Core\DocIndex,USE,RecL=39,KPS=1,KLN=30",i,outIn,k
! read #1,using 60:docname$,docline,docposition,docsigchoice,cddrive$
 
MAINSCREEN: ! r:
	dim comboa$(99)*30
	mat comboa$(99)
	fnTos
	mylen = 17 : myalign = 1
	fnLbl(1,1,"Document Name:",mylen,myalign)
	a=0
	restore #1:
	do
		read #1,using L60: docname$,docline,docposition,docsigchoice,cddrive$ eof L105
		L60: form pos 1,c 30,n 3,n 3,n 2,c 1
		a+=1
		comboa$(a)=docname$
	loop
	L105: !
	a=max(1,a)
	mat comboa$(a)
	fncomboa("DocNames",1,18,mat comboa$,'',25)
	fnLbl(2,1,"Number of Copies:",mylen,myalign)
	fnTxt(2,18,2)
	response$(2)=str$(1)
	fnCmdSet(14)
	fnAcs(mat response$,ckey)
	docname$=response$(1)
	copies=max(1,val(response$(2)))
	if ckey=5 then goto Xit
	if ckey=10 then goto Xit
	docline=docposition=0
	docsigchoice=1
	if ckey=1 then
		docname$="": goto EDITSCREEN
	else if ckey=2 then
		read #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey EDITSCREEN
		goto EDITSCREEN
	else if ckey=4 then
		read #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey EDITSCREEN
		goto PRINTSIGNATURE
	end if
goto EDITSCREEN ! /r
 
EDITSCREEN: ! r:
	fnTos
	mylen=17 : myalign=1
	
	fnLbl(1,1,"Document Name:",mylen,myalign)
	fnTxt(1,mylen+1,30,30,0,'',0,"Choose a name that you will remember, such as Payroll Check" )
	response$(1)=docname$
	fnLbl(2,1,"Line on form:",mylen,myalign)
	fnTxt(2,mylen+1,4,3,0,'30',0,"This is the distance from the top of the form where the signature shold print. (Normally about 6 lines per inch)" )
	response$(2)=str$(docline)
	fnLbl(3,1,"Position on form:",mylen,myalign)
	fnTxt(3,mylen+1,4,3,0,'30',0,"This is the number of characters from the left side of the form.       (Normally about 10 characters per inch)" )
	response$(3)=str$(docposition)
	fnLbl(4,1,"Signature Choice:",mylen,myalign)
	fnTxt(4,mylen+1,3,2,0,'30' ,0,"You can have up to 10 different signatures.  Choose the one you want printed on this document." )
	response$(4)=str$(docsigchoice)
	fnLbl(5,1,"CD Drive:",mylen,myalign)
	fnTxt(5,mylen+1,1,1,0,'',0,"Your signature is stored on a cd.  What is the drive designation used on this computer for the cd drive?" )
	response$(5)=cddrive$
	fnCmdSet(4)
	fnAcs(mat response$,ckey)
	if ckey=5 then goto MAINSCREEN
	docname$=response$(1)(1:30)
	docline     =val(response$(2)) conv BADLINE
	docposition =val(response$(3)) conv BADPOSITION
	docsigchoice=val(response$(4)) conv BADSIGCHOICE
	cddrive$    =uprc$(response$(5))
	if cddrive$<"A" or cddrive$>"Z" then
		mat msgline$(2): msgline$(1)="The drive designation you used is invalid!"
		msgline$(2) =" Normal designations would be D or E, but can be other letters"
		fnmsgbox(mat msgline$,response$,"Bad CD Drive designation",48)
		goto EDITSCREEN
	end if
	rewrite #1,using L60,key=rpad$(docname$,30): docname$,docline,docposition,docsigchoice,cddrive$ nokey L415
goto MAINSCREEN ! /r
L415: ! r:
	write #1,using L60: docname$,docline,docposition,docsigchoice,cddrive$
goto MAINSCREEN ! /r
BADLINE: ! r:
	mat msgline$(1)
	msgline$(1)="You have entered an invalid line #. Must be a number from 1 to 200!"
	fnmsgbox(mat msgline$,response$,"Bad line number",48)
goto EDITSCREEN ! /r
BADPOSITION: ! r:
	mat msgline$(1)
	msgline$(1)="You have entered an invalid position. Your answer must be from 1 to 200!"
	fnmsgbox(mat msgline$,response$,"Bad position",48)
goto EDITSCREEN ! /r
BADSIGCHOICE: ! r:
	mat msgline$(1)
	msgline$(1)="You can have up to 10 different signatures.  You may just one.  Choose an answer from 1 to 10"
	fnmsgbox(mat msgline$,response$,"Bad position",48)
goto EDITSCREEN ! /r
PRINTSIGNATURE: ! r:
	fnopenprn
	if docline = 0 then docline = 1
	if docposition = 0 then docposition = 1
	for j=1 to copies
		pr #255,using L1050: " "
		L1050: form skip docline,pos docposition,c 50,skip 1
		pr #255: "*Insert File:Z:\Signature1.acs"
		pr #255: newpage
	next j
	fncloseprn
	pr newpage
goto MAINSCREEN ! /r
Xit: ! chain "menu"
