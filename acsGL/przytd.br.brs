! Replace S:\acsGL\PRZYTD
! Zero Year To Date and Quarter To Date Information
 
	autoLibrary
	fnTop(program$,"Zero YTD Payroll Information")
	on error goto Ertn
 
	dim k$(3)*25,ss$*11,m(36),adr(2),n(2),d(14),ml$(3)*80
 
	if fnprocess=1 then goto L300
L120: fnTos
	mylen=42: mypos=mylen+3 : right=2
	fnLbl(1,1,"* * * * *   WARNING   * * * * *",mylen,right)
	fnLbl(3,1,"This program zeroes all year to date",mylen,right)
	fnLbl(4,1,"information. It should be run at",mylen,right)
	fnLbl(5,1,"at the end of each year after all",mylen,right)
	fnLbl(6,1,"quarterly and annual reports have been run.",mylen,right)
	fnLbl(8,1,"Enter 'ZERO' to continue:",mylen,right)
	fnTxt(8,mypos,4,0,right,"",0,"You must type the word 'Zero' to indicate that you for sure want to zero the year.",0 ) : _
	resp$(1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
 
	if ckey=5 then goto Xit
	pas$=uprc$(resp$(1))
	if pas$="ZERO" then goto L300
MSGBOX1: !
	mat ml$(2)
	ml$(1)="          Incorrect password! "
	ml$(2)="Click OK to try again; else Cancel to stop."
	fnmsgbox(mat ml$,resp$,'',49)
if resp$="OK" then goto L120 else goto Xit
 
L300: !
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed
	L310: !
		read #1,using L320: x eof L350
		L320: form pos 91,36*pd 5.2,2*n 5
		rewrite #1,using L320: mat m,mat adr
	goto L310
L350: !
close #1:
open #1: "Name=[Q]\GLmstr\ACPRCKS.h[cno],size=0,RecL=110,Replace",internal,output
close #1:
goto Xit
Xit: fnXit
include: Ertn
 
