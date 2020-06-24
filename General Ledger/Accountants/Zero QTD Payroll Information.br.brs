! formerly S:\acsGL\PRZQTD
! Zero Quarterly Information (After-Fact-Payroll)
autoLibrary
on error goto Ertn

dim n(2),k$(3)*25,ss$*11,m(36),adr(2),ml$(3)*80

fnTop(program$,"Zero QTD Payroll Information")
if fnprocess=1 then goto ZeroIt
Screen1: !
	fnTos
	mylen=40: mypos=mylen+3 : right=2
	fnLbl(1,1,"* * * * *   WARNING   * * * * *",mylen,right)
	fnLbl(3,1,"This program zeroes all quarterly",mylen,right)
	fnLbl(4,1,"information. It should be run at",mylen,right)
	fnLbl(5,1,"the end of each quarter after all",mylen,right)
	fnLbl(6,1,"quarterly reports have been run.",mylen,right)
	fnLbl(8,1,"Enter 'ZERO' to continue:",mylen,right)
	fnTxt(8,mypos,4,0,right,"",0,"You must type the word 'Zero' to indicate that you for sure want to zero the quarter.",0 ) : _
	resp$(1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	pas$=uprc$(resp$(1))
	if pas$="ZERO" then goto ZeroIt
	pr bell;
	goto Screen1

ZeroIt: ! r:
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno],Shr",internal,outIn,keyed
	L300: !
		read #1,using 'Form POS 91,36*PD 5.2': mat m eof L340
		for j=2 to 36 step 2 : m(j)=0 : next j
		rewrite #1,using 'Form POS 91,36*PD 5.2': mat m
	goto L300
	L340: close #1:
goto Xit ! /r

Xit: fnXit
include: Ertn

