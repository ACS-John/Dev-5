! Replace S:\acsPR\newjcPctCmp
! Enter Percent Complete
 
autoLibrary
on error goto Ertn
 
dim cap$*128,ml$(2)*60,resp$(5)*30
dim jn$*6,jno$*6,n$*40,cn$*11,k$*25
 
fnTop("S:\acsPR\jcPctCmp",cap$="Enter Percent Complete")
 
open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed
open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed
TRANSACTION_ENTRY: !
	cn=l10=l12=l13=0
	fnTos(sn$="Percentinput")
	respc=0 : frac=0
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Job Number:",mylen,1)
	fncmbjob(1,mypos)
	resp$(respc+=1)=jn$
	fnLbl(2,1,"Category:",mylen,1)
	fncmbcategory(2,mypos)
	resp$(respc+=1)=str$(cn)
	fnLbl(3,1,"Percent Complete - Labor:",mylen,1)
	fnTxt(3,mypos,3,3,0,"30",0,"Enter whole numbers. For example:  10% would be entered as 10")
	resp$(respc+=1)=str$(l12)
	fnLbl(4,1,"Percent Complete - Other:",mylen,1)
	fnTxt(4,mypos,3,3,0,"30",0,"Enter whole numbers. For example:  10% would be entered as 10")
	resp$(respc+=1)=str$(l13)
	fnLbl(5,1,"Units Completed:",mylen,1)
	fnTxt(5,mypos,7,7,0,"30",0,"If you are tracking units on the category, enter the units completed.")
	resp$(respc+=1)=str$(l10)
	fnCmdKey("&Update Job",1,1,0,"Posts these percentages immediately to this job.")
	fnCmdKey("&Complete",5,0,1,"Returns you to main menu.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	jn$=lpad$(trim$(resp$(1)(1:6)),6) ! job number
	if trim$(jn$)="" then goto L340 else goto L350
L340: mat ml$(2)
	ml$(1)="You failed to enter a job number. You can not continue"
	ml$(2)="without a job number."
	fnmsgbox(mat ml$,resp$,cap$,0): goto TRANSACTION_ENTRY
L350: cn=val(resp$(2)(1:5)) ! category
	if cn=0 then goto L370 else goto L380
L370: mat ml$(2)
	ml$(1)="You failed to enter a category number. You cannot continue"
	ml$(2)="without a category number."
	fnmsgbox(mat ml$,resp$,cap$,0)
	goto TRANSACTION_ENTRY
L380: cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
	read #2,using L540,key=cn$: k$,rl10,rl12,rl13 nokey L410
	goto L420
L410: mat ml$(2)
	ml$(1)="There is no job # "&trim$(jn$)&" with a category # "&str$(cn)&"."
	ml$(2)="You must enter a valid job or category number."
	fnmsgbox(mat ml$,resp$,cap$,0)
	goto TRANSACTION_ENTRY
L420: l12=val(resp$(3)) ! labor %
	l13=val(resp$(4)) ! other %
	l10=val(resp$(5)) ! units
	cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
	read #2,using L540,key=cn$: k$,rl10,rl12,rl13
	if l10=0 then goto L480 else goto L490
	L480: l10=rl10
	L490: if l12=0 then goto L500 else goto L510
	L500: l12=rl12
	L510: if l13=0 then goto L520 else goto L530
	L520: l13=rl13
	L530: rewrite #2,using L540,key=cn$: k$,l10,l12,l13
	L540: form pos 12,c 25,pos 100,pd 7.2,pos 114,2*pd 2
goto TRANSACTION_ENTRY
 
DONE: !
	close #2:
goto Xit
 
Xit: fnXit
include: Ertn
