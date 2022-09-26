! formerly S:\acsUB\RmBudget
! -- Remove Old Budget Transactions

autoLibrary
on error goto Ertn
fnTop(program$)

! r: BUD1: !
	bud1=0
	open #hBudgetMstr=fnH: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr',i,outIn,k ioerr Xbud1
	FbudgetMstr:  form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	FbudMTrAddr: form pos 75,2*pd 3
	open #hBudgetTrans=fnH: 'Name=[Q]\UBmstr\BudTrans.h[cno],Shr',i,outi,r
	FbudgetTrans: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	bud1=1
Xbud1: ! /r
if bud1=0 then goto Xit

fnTos
fnLbl(1,1,'All paid budget records with a date prior' ,44,2)
fnLbl(2,1,'to this date will be removed.' ,44,2)
fnLbl(4,1,'Oldest Date to Retain (MMDDYY):' ,33,1)
fnTxt(4,35,8,0,0,'1')
resp$(1)=''
fnCmdSet(2)
ckey=fnAcs(mat resp$)
rd1=val(resp$(1))
if ckey=5 then goto Xit
open #hTemp=fnH: 'Name=[temp]\Work1.dat,RecL=149,Replace',i,outi,r

do
	dim ba(13)
	dim tr(2)
	read #hBudgetMstr,using FbudgetMstr: z$,mat ba,mat tr eof Finis
	adr=tr(1)
	mat tr=(0)
	do while adr
		dim bt1(14,2)
		read #hBudgetTrans,using FbudgetTrans,rec=adr,release: z$,mat bt1,nba noRec L450
		d1=bt1(1,1) ! transaction date
		d2=rd1      ! cutoff date
		if sum(bt1)=0 then goto L440
		if bt1(14,1) then
			d1=fndate_mmddyy_to_ccyymmdd(d1)
			d2=fndate_mmddyy_to_ccyymmdd(d2)
			if d1<d2 then goto L440
		end if
		lr2=lrec(hTemp)+1
		write #hTemp,using FbudgetTrans,rec=lr2: z$,mat bt1,0
		if tr(2)>0 then rewrite #hTemp,using 'form pos 147,pd 3',rec=tr(2): lr2
		if tr(1)=0 then tr(1)=lr2
		tr(2)=lr2
		L440: !
		adr=nba
	loop
	L450: !
	rewrite #hBudgetMstr,using FbudMTrAddr: mat tr
loop

Finis: !
close #hTemp:
close #hBudgetMstr:
close #hBudgetTrans:
fnCopy('[temp]\Work1.dat','[Q]\UBmstr\BudTrans.h[cno]')
fnIndex('[Q]\UBmstr\BudMstr.h[cno]','[Q]\UBmstr\BudIdx1.h[cno]','1 10')
fnFree('[temp]\Work1.dat')
goto Xit
Xit: fnXit
include: ertn
