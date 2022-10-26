! Replace S:\acsGL\BudInpt
! used to enter new budget figures at beginning of new year
!
	autoLibrary
	on error goto Ertn
	fnTop(program$,'Budget Amounts')
!
	dim dat$*20,bm(13),io1$(4),in1(4),fl2$(2),sc2$(2)*50
	dim revb(13),resp$(10)*256
	dim heading$*70,form$*80,numeric_format$*20,selection$*70
!
!
	open #1: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
	read #1,using 'form pos 384,N 2',rec=1: nap
	close #1:
!
	fnTos
	mylen=50: mypos=mylen+3 : right=1
	fnFra(1,1,2,55,'Method of Budget Entry',' ',0)
	fnOpt(1,3,'Enter new Budget amounts',0,1)
	resp$(rc+=1)='True'
	fnOpt(2,3,'Pull Budget from Budget Management System',0,1)
	resp$(rc+=1)='False'
	fnFra(5,1,3,55,'Method of Allocating Budget',' ',0)
	fnOpt(1,3,'Divide new budget evenly between months',0,2)
	resp$(rc+=1)='True'
	fnOpt(2,3,'Allocate full amount to month 12',0,2)
	resp$(rc+=1)='False'
	fnOpt(3,3,'Allocate full amount to month 1',0,2)
	resp$(rc+=1)='False'
	fnCmdSet(2)
!
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then method=1 else method=2
	if resp$(3)='True' then method_to_allocate=1 ! divide evenly
	if resp$(4)='True' then method_to_allocate=2 ! all to month 12
	if resp$(5)='True' then method_to_allocate=3 ! all to month 1
	if method=2 then gosub L790
	if method=2 then gosub BUDGET_FILE_NUM
	open #1: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr',i,outIn,k
	fnopenprn
!
	pr #255: '   GL Number    New Budget  Old Budget'
	pr #255: '--------------  ----------  ----------'
ENTER_BUDGET: !
	if method=2 then goto L1090
	fnTos(sn$='BudgetAmount2')
	mylen=25: mypos=mylen+3 : right=1
	k$='': read #1,using L440: k$ ioerr L450 eof L450 ! try to read next account
L440: form pos 1,c 12
L450: fnLbl(1,1,' General Ledger Number:',mylen,right)
	fnQgl(1,mypos,0,2)
	resp$(1)=fnrgl$(k$)
	fnLbl(2,1,' Budget Amount:',mylen,right)
	fnTxt(2,mypos,12,0,1,'10',0,'Enter the total budget amount for this account.  Use negative amounts on revenues (any negative balance accounts).')
	resp$(2)=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	k$=fnagl$(resp$(1))
	budgetamt=val(resp$(2)) ! new budget amount
	read #1,using L550,key=k$: mat bm,mat revb
L550: form pos 249,13*pd 6.2,pos 339,13*pd 6.2
L560: o1=sum(bm)
	mat bm=(0)
	if method_to_allocate=2 then bm(12)=budgetamt: goto L650 ! allocate all to month 12
	if method_to_allocate=3 then bm(1)=budgetamt: goto L650 ! allocate all to month 1
	m1=round(budgetamt/nap,2)
	for j=1 to nap-1
		bm(j)=m1
	next j
	bm(j)=budgetamt-sum(bm)
L650: mat revb=bm
	pr #255,using L670: mat in1,o1
L670: form pos 1,pic(zzz),n 7,pic(zzzz),2*n 12.2,skip 1
	if budyear=1 then rewrite #1,using L690,key=k$: mat revb: goto L710 ! only rewrite the revised budget figures if record only changes
L690: form pos 339,13*pd 6.2
	rewrite #1,using L550,key=k$: mat bm,mat revb
L710: goto ENTER_BUDGET

EOF2: !
	fncloseprn

Xit: fnXit

L790: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)

	open #1: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
	read #1,using 'form pos 384,N 2',rec=1: nap
	close #1:
	pr newpage
	fnTos(sn$='BudgetAmount3')
	mylen=50: mypos=mylen+3 : right=1
	fnFra(1,1,2,55,'Update Current Budget or New Budget',' ',0)
	fnOpt(1,3,'Update Current Budget for Changes',0,1)
	resp$(rc+=1)='True'
	fnOpt(2,3,'Update for New Budget Year',0,1)
	resp$(rc+=1)='False'

	fnCmdSet(2)

	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then budyear=1 else budyear=2
	if budyear=1 then p1=37 else p1=43 ! CURRENT YEARS BUDGET OR NEXT YEARS BUDGET
return

BUDGET_FILE_NUM: ! r:
	fnTos(sn$='BudgetAmount4')
	mylen=50: mypos=mylen+3 : right=1
	fnLbl(1,1,' Budget File Number to Pull:',mylen,right)
	fnTxt(1,mypos,3,0,1,'30',0,'You can have different budget files in the budget management system.  Enter the budget file you wish to pull.')
	resp$(1)=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	bud=val(resp$(1)) ! budget file number to pull
	open #2: 'Name=[Q]\GLmstr\Budget'&str$(bud)&'.h[cno],KFName=[Q]\GLmstr\BgIndx'&str$(bud)&'.h[cno],Shr',i,outIn,k ioerr BUDGET_FILE_NUM
return ! /r

L1090: ! PULL FROM BUDGET MANAGEMENT SYSTEM  (select budget #)
L1100: !
	read #2,using 'form pos 1,N 3,N 6,N 3,pos P1,PD 6.2,pos 149,C 1': mat in1,cd$ eof EOF2
	if cd$<>'B' then goto L1100
	k$=cnvrt$('N 3',in1(1))&cnvrt$('N 6',in1(2))&cnvrt$('N 3',in1(3))
	rewrite #2,using L1140: 0 ! SET CHANGES TO ZERO IN BUDGET WORKFILE
	L1140: form pos 31,pd 6.2
	read #1,using L550,key=k$: mat bm,mat revb nokey L1100
goto L560

include: ertn
