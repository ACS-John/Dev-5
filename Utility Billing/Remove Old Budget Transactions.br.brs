! Replace S:\acsUB\RmBudget
! -- Remove Old Budget Transactions
 
autoLibrary
on error goto Ertn
fnTop(program$)
 
dim ba(13),bt1(14,2)
dim tr(2)
 
gosub BUD1
if bud1=0 then goto Xit
 
fnTos(sn$:="RmBudget" )
fnLbl(1,1,"All paid budget records with a date prior" ,44,2)
fnLbl(2,1,"to this date will be removed." ,44,2)
fnLbl(4,1,"Oldest Date to Retain (MMDDYY):" ,33,1)
fnTxt(4,35,8,0,0,"1")
resp$(1)=""
fnCmdSet(2)
ckey=fnAcs(mat resp$)
rd1=val(resp$(1))
if ckey=5 then goto Xit
open #2: "Name="&env$('temp')&'\'&"Work1.dat,Size=0,RecL=149,Replace",i,outi,r
L370: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
do
	read #81,using 'form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3': z$,mat ba,mat tr eof END1
	adr=tr(1)
	mat tr=(0)
	L300: !
	if adr<>0 then
		read #82,using L370,rec=adr,release: z$,mat bt1,nba noRec L450
		d1=bt1(1,1) ! transaction date
		d2=rd1      ! cutoff date
		if sum(bt1)=0 then goto L440
		if bt1(14,1) then
			d1=fndate_mmddyy_to_ccyymmdd(d1)
			d2=fndate_mmddyy_to_ccyymmdd(d2)
			if d1<d2 then goto L440
		end if
		lr2=lrec(2)+1
		write #2,using L370,rec=lr2: z$,mat bt1,0
		if tr(2)>0 then rewrite #2,using 'form pos 147,pd 3',rec=tr(2): lr2
		if tr(1)=0 then tr(1)=lr2
		tr(2)=lr2
		L440: !
		adr=nba
		goto L300
	end if
	L450: !
	rewrite #81,using L460: mat tr
	L460: form pos 75,2*pd 3
loop
 
END1: !
close #2:
close #81:
close #82:
execute 'Copy "'&env$('temp')&'\'&'Work1.dat" "'&"[Q]\UBmstr\BudTrans.h[cno]"&'" -n'
execute "Index [Q]\UBmstr\BudMstr.h[cno]"&' '&"[Q]\UBmstr\BudIdx1.h[cno] 1 10 Replace DupKeys -n"
execute "Free "&env$('temp')&'\'&"Work1.dat -n"
goto Xit
 
Xit: fnXit
 
BUD1: ! r:
	bud1=0
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",i,outIn,k ioerr L630
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
	bud1=1
	L630: !
return ! /r
 
include: ertn
