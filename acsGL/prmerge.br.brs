! Replace S:\acsGL\PRMerge
! ACCOUNTANTS P/R MERGE (Posts payroll checks entered directly from G/L to the after-the-fact payroll records in G/L)
 
	autoLibrary
	on error goto Ertn
 
	dim k$(3)*25,ss$*11,d(22),m(36),adr(2),n(2),fb$(4),en$*4,cap$*128,tr(7)
L80: dim tr$*12,td$*30,jv$(3)*6,resp$(10)*80
 
	fnTop(program$,cap$="Post Payroll Checks")
	fncno(cno)
	if exists("[Q]\glmstr\PRmstr.h[cno]")=0 then goto Xit
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],NoShr",internal,outIn,relative
	if lrec(2)=0 then goto Xit
	open #3: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Shr",internal,outIn,relative
READ_ENTRIES: !
L160: read #2,using L180: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,ven$,mat jv$,key$ eof L500
	rec2=rec(2)
	if tr(7)=7 or tr(7)=16 then goto L160 ! already posted (7 pr only, 16 both)
L180: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,pos 93,c 12
	if tr(6)<>4 then goto L160 ! must be payroll check transaction
	en$=lpad$(rtrm$(ven$),4) soflow L870
	if trim$(olden$)<>"" and olden$<>en$ or holdtr$<>"" and holdtr$<>tr$ then gosub REWRITE_RECORD
L220: if olden$<>en$ or holdtr$<>tr$ then read #1,using L230,key=en$: eno,mat m,mat adr nokey L870
L230: form pos 1,n 4,pos 91,36*pd 5.2,2*n 5
	j=val(jv$(2)) ! pull code from Jv$
	if j=0 then j=17 ! put in miscellaneous if breakdown code lost
	if j=1 or j=17 or j=18 then m(j*2-1)=m(j*2-1)+tr(5) : d(j+3)=tr(5) else m(j*2-1)=m(j*2-1)-tr(5) : d(j+3)=-tr(5)
	if j=1 or j=17 or j=18 then m(j*2)=m(j*2)+tr(5) else m(j*2)=m(j*2)-tr(5)
	if j=17 then goto L310 ! skip weeks worked
	d(22)+=tr(5) ! add net
L310: olden$=en$: holdtr4=tr(4): holdtr$=tr$: rewrite #2,using L470,rec=rec2: tr(7)+7: goto READ_ENTRIES
REWRITE_RECORD: !
L330: r9=lrec(3)+1
	nca=0
	d(1)=eno: d(2)=holdtr4 : d(3)=val(holdtr$)
	write #3,using ' Form POS 1,N 4,2*PD 4,19*PD 5.2,PD 3',rec=r9,reserve: mat d,nca duprec L330
	mat d=(0)
	if adr(2)=0 then goto L430
	read #3,using L400,rec=adr(2),reserve: nca
L400: form pos 108,pd 3
	nca=r9
	rewrite #3,using L400,rec=adr(2),reserve: nca
L430: if adr(1)=0 then adr(1)=r9
	adr(2)=r9
	if trim$(olden$)="" then goto L480
	rewrite #1,using L230,key=olden$: eno,mat m,mat adr nokey L460
L460: rewrite #2,using L470: tr(7)+7 ! add to posting code of 9 from gl merge
L470: form pos 27,n 2
L480: if eofcode=1 then goto L500 ! write last record and quit
return
L500: ! EOJ
	if eofcode=0 then eofcode=1: goto REWRITE_RECORD
	close #1:
	close #2:
	close #3:
	if print1=1 then let fncloseprn
	goto Xit
 
	if fnprocess=1 then goto L800
	pr newpage
	pr f "4,10,c 60": "The Following Employee is not on File."
	pr f "7,2,c 60": "    Emp Number    Check Number    Date   Gross-Pay"
	pr f "9,1,PIC(ZZZZZZZZZZ#),N": d(1)
	pr f "9,20,PIC(ZZZZZZ#),N": d(3)
	pr f "9,32,PIC(ZZ/ZZ/ZZ),N": d(2)
	pr f "9,40,PIC(--------.##),N": d(4)
	pr f "20,1,C 66,N": "Enter 0 to Add this Employee or Enter Correct Employee Number"
L670: input fields "20,70,N 4,UE,N": numb conv L670
	if numb>0 then d(1)=numb: goto L220
	pr newpage
	pr f "5,10,c 25": "Employee Name" : _
	pr f "7,10,c 25": "Address" : _
	pr f "9,10,c 25": "City, State, Zip Code" : _
	pr f "11,10,c 25": "Social Security Number"
	fb$(1)="5,40,C 25,UT,N" : _
	fb$(2)="7,40,C 25,UT,N" : _
	fb$(3)="9,40,C 25,UT,N" : _
	fb$(4)="11,40,C 11,UT,N"
L720: input fields mat fb$: k$(1),k$(2),k$(3),ss$ conv L720
L730: mat m=(0)
	mat adr=(0)
	write #1,using L760: d(1),mat k$,ss$,mat m,mat adr
L760: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
	goto L220
 
 
L800: fnopenprn
	if print1=0 then pr #255: "The Employees listed here were not previously on file." : _
		pr #255: "Use Payroll Employee File maintenance to enter" : _
		pr #255: "their Name and Address" : _
		pr #255: "_______________________________________________________________________________"
	pr #255: d(1)
	print1=1
	mat k$=(" ")
	ss$=" "
	goto L730
L870: fnTos
	mylen=40: mypos=mylen+3 : right=1
	fnLbl(1,10,"  Employee Number: "&ven$,mylen,left)
	fnLbl(2,10,"   Check Number: "&tr$,mylen,left)
	fnLbl(3,10, "           Date: "&cnvrt$("pic(zz/zz/zz)",tr(4)),mylen,left)
	fnLbl(4,10, "  Gross Pay: "&cnvrt$("pic(-------.zz)",tr(5)) ,mylen,left)
	fnLbl(7,5, "This employee number does not exist!" ,60,0)
	fnOpt(8,10,"Add this Employee",0,0) : _
	resp$(1)="True"
	fnOpt(9,10,"Change Employee Number",0,0) : _
	resp$(2)="False"
	fnCmdKey("&Next",1,1,0,"Allows you to either add the employee or change the employee #.")
	fnAcs(mat resp$,ckey)
	if resp$(1)="True" then gosub ADD : goto L220
	if resp$(2)="True" then gosub CHANGE_EMPLOYEE_NUMBER : goto L220
	goto L80
 
ADD: !
	fnTos
	mylen=15: mypos=mylen+3 : right=1: rc=0
	fnLbl(1,1,"Name:",mylen,right)
	fnTxt(1,mypos,30,0,left,"",0,"Enter the employee information.",0 ) : _
	resp$(rc+=1)=k$(1)
	fnLbl(2,1,"Address:",mylen,right)
	fnTxt(2,mypos,30,0,left,"",0,"Enter the employee information.",0 ) : _
	resp$(rc+=1)=k$(2)
	fnLbl(3,1,"City, St Zip:",mylen,right)
	fnTxt(3,mypos,30,0,left,"",0,"",0 ) : _
	resp$(rc+=1)=k$(3)
	fnLbl(4,1,"SS Number:",mylen,right)
	fnTxt(4,mypos,11,0,left,"",0,"Enter the employee social security number.",0 ) : _
	resp$(rc+=1)=ss$
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto L220
	k$(1)=resp$(1) : _
	k$(2)=resp$(2) : _
	k$(3)=resp$(3) : _
	ss$=resp$(4)
	mat m=(0)
	mat adr=(0)
	write #1,using L1180: en$,mat k$,ss$,mat m,mat adr
L1180: form pos 1,c 4,3*c 25,c 11,36*pd 5.2,2*n 5
return
 
CHANGE_EMPLOYEE_NUMBER: !
	fnTos
	mylen=18: mypos=mylen+3 : right=1: rc=0
	fnLbl(1,1,"Employee Number:",mylen,right)
	fncombof("PRmstr",1,mypos,27,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,'',0,pas, "Choose from the list of employees.",0) : _
	resp$(1)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto L220
	en$=lpad$(rtrm$(resp$(1)(1:4)),4)
	goto L220
 
Xit: fnXit
 
include: ertn
 
