! Bank reconciliation routines
 
autoLibrary
on error goto Ertn

dim dat$*20,adr(2)

dim tr$(5)*35,de$*30,bn$*40,sn$*50
dim line$*132,resp$(10)*35,ml$(4)*60
dim item1$(6)*40,item3$(9)*40
dim chdr$(5)*15,cmask$(5)*10,flxitm$(5)*15
dim w(4),hdw$(4)*40
dim io7$(9),dpt(3),io8$(25),dpd(5,5)

fnTop(program$)
fndat(dat$)

open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,i,r
read #20,using 'form pos 150,X 2,N 2',rec=1,release: wbc
close #20:
open #bankmstr:=12: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',internal,outIn,keyed
open #paymstr1:=13: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',internal,outIn,keyed
open #paymstr2:=14: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr',internal,outIn,keyed
open #trmstr1:=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',internal,outIn,keyed
open #trmstr2:=2: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr',internal,outIn,keyed
open #tralloc:=3: 'Name=[Q]\CLmstr\TrAlloc.h[cno],Shr',i,outi,r
read #bankmstr,using 'form pos 3,C 40',key=cnvrt$('N 2',wbc),release: bn$
restore #bankmstr:

Menu1: ! r:
	close #81: ioerr ignore
	open #81: 'Name=[Q]\CLmstr\Bank'&str$(wbc)&'.h[cno],Use,RecL=32',i,outi,r
	if lrec(81)>0 then
		read #81,using 'form pos 1,N 6,2*PD 5.2,N 6',rec=1: stmtdt,bgbal,stmtbal,codt conv L320 : goto BANK_STMT_INFO
	end if
	L320: !
	if lrec(81)>0 then
		read #81,using 'form pos 1,N 6,2*PD 7.2,N 6',rec=1: stmtdt,bgbal,stmtbal,codt
	else
		write #81,using 'form pos 1,N 6,2*PD 7.2,N 6': stmtdt,bgbal,stmtbal,codt
	end if
BANK_STMT_INFO: !
	fnTos
	respc=0
	mylen=30 : mypos=mylen+2
	fnLbl(1,1,'Bank Code:',mylen,1)
	fncombof('Bankmstr',1,mypos,35,'[Q]\CLmstr\bankmstr.h[cno]',1,2,3,30,'[Q]\CLmstr\BankIdx1.h[cno]',0,0, '',0)
	resp$(respc+=1)=str$(wbc)
	fnLbl(3,1,'Statement Date:',mylen,1)
	fnTxt(3,mypos,10,0,1,'3',0,'We suggest you always use the month end date.')
	resp$(respc+=1)=str$(stmtdt)
	fnLbl(4,1,'Previous Statement Balance:',mylen,1)
	fnTxt(4,mypos,14,0,1,'10',0,'Pull this information directly from your current bank statement. ')
	resp$(respc+=1)=str$(bgbal)
	fnLbl(5,1,'Current Statement Balance:',mylen,1)
	fnTxt(5,mypos,14,0,1,'10',0,' ')
	resp$(respc+=1)=str$(stmtbal)
	fnLbl(6,1,'Reconciliation Cutoff Date:',mylen,1)
	fnTxt(6,mypos,10,0,1,'3',0,'The cutoff date would normally be the last day of the month.')
	resp$(respc+=1)=str$(codt)
	fnLbl(8,1,'Reconciliation Options:',28,1)
	item1$(1)='Enter Cleared Checks'
	item1$(2)='Enter Cleared Deposits'
	item1$(3)='Clear All Adjustments'
	item1$(4)='Calculate Bank Totals'
	item1$(5)='Print Reconciliation Listing'
	item1$(6)='Make Corrections'
	fncomboa('bankrec-1',8,30,mat item1$,'Select the funtion you would like to perform.  Normally you would clear the checks, deposits, adjustments and then calculate the bank totals')
	resp$(respc+=1)=item1$(1)
	fnCmdKey('&Display Balances',3,0,0,'Displays previous balances for this bank account.')
	fnCmdKey('&Next',1,1,0,'Proceed to next options')
	fnCmdKey('&Cancel',5,0,1,'Returns to main menu')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	wbc=val(resp$(1)(1:2)) ! working bank code
	if ckey=3 then goto Menu1 ! redisplay balances
	read #bankmstr,using 'form pos 3,C 40',key=cnvrt$('N 2',wbc),release: bn$
	stmtdt=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! convert date back to mmddyy format
	bgbal=val(resp$(3))
	stmtbal=val(resp$(4))
	codt=val(resp$(5)(5:6))*10000+val(resp$(5)(7:8))*100+val(resp$(5)(3:4)) ! convert date back to mmddyy format
	for j=1 to 6
		if resp$(6)=item1$(j) then ti3=j: goto L610
	next j
	L610: !
	if ti3=6 then fnchain('S:\Checkbook\Transaction')
	close #81: ioerr ignore
	open #81: 'Name=[Q]\CLmstr\Bank'&str$(wbc)&'.h[cno],Use,RecL=32',i,outi,r
	if lrec(81)=0 then write #81,using 'form pos 1,N 6,2*PD 7.2,N 6',rec=1: stmtdt,bgbal,stmtbal,codt : goto L660
	rewrite #81,using 'form pos 1,N 6,2*PD 7.2,N 6',rec=1: stmtdt,bgbal,stmtbal,codt
	L660: !
	close #81: ioerr ignore
	if ti3<3 then wtt=ti3
on ti3 goto ClearingOptions,ClearingOptions,ClearingAdjustments,CalculateTotals,PrintListings none Menu1
! /r 
ClearingAdjustments: ! r:
	fnTos
	respc=0
	fnLbl(1,60,'',1,0)
	fnLbl(1,1,'If the adjustments have been previously cleared',50,0)
	fnLbl(2,1,'with a wrong date, then enter the previously',50,0)
	fnLbl(3,1,'cleared date (mmddyy) (otherwise leave blank):',50,0)
	fnTxt(3,48,8,0,1,'1',0,'')
	resp$(respc+=1)=''
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto Menu1
	pcd1=val(resp$(1)) ! clear old adjustments
	restore #trmstr1,key>=cnvrt$('N 2',wbc)&'3        ': nokey Menu1
	L810: !
	read #trmstr1,using 'form pos 1,N 2,N 1,pos 12,N 6,pos 72,N 6': bank_code,tcde,d1,clr eof Menu1
	if tcde><3 then goto Menu1
	if bank_code><wbc then goto Menu1
	if fndate_mmddyy_to_ccyymmdd(d1)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L810
	if clr><pcd1 then goto L810
	rewrite #trmstr1,using 'form pos 1,N 2,N 1,pos 12,N 6,pos 72,N 6': bank_code,tcde,d1,stmtdt eof Menu1
goto L810 ! /r
 
ClearingOptions: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'Reference Number to Clear:',28,1)
	fnTxt(1,31,8,0,1,'',0,'If you wish to clear one transaction at a time, enter the reference # here.')
	resp$(respc+=1)=''
	fnLbl(2,1,'Cleared Amount:',28,1)
	fnTxt(2,31,12,0,1,'10',0,'You do not have to enter the amount.  If you do, it will compare the amount you enter to your check history and warn you of any differences. ')
	resp$(respc+=1)=''
	if ti3=1 then fnButton(1,50,'Clear &Range of Checks',60,'This option allows you to clear a range of checks without having to enter each check number',0,20)
	if ti3=2 then fnButton(1,50,'Clear Deposits by &Amount',61,'This option allows you to clear deposits by entering the amount without having to know the reference number.',0,30)
	if ti3=2 then fnButton(3,50,'Clear Deposits by Date &Range',62,'This option allows you to clear deposits by entering a date range.',0,30)
	if ti3=2 then fnButton(5,50,'Clear Deposits from &List',63,'This option allows you to clear deposits from a listing of all outstanding deposits.',0,30)
	if ti3=1 then fnButton(3,50,'Clear Checks from &List',64,'This option allows you to clear checks from a listing of all outstanding checks.',0,20)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Menu1
	k$=resp$(1) ! check # to clear
	if ckey=61 then
		ti=2: tcde=wtt=ti3
		tr3=pcde=0
		goto ClearDepositsByAmount
	end if
	if ti3=1 and ckey=60 then goto L2790 ! clear range of checks
! If TI3=2 AND ckey=3 Then Goto 4440 ! clear deposits by amount
	if ti3=2 and ckey=62 then goto L2790 ! deposits by date range
	if ti3=2 and ckey=63 then goto CLEAR_TRANSACTIONS_FROM_LIST ! deposits from listing of all outstanding deposits
	if ti3=1 and ckey=64 then goto CLEAR_TRANSACTIONS_FROM_LIST ! clear checks from listing
	if ckey=5 or ckey=99 then goto Menu1
	if rtrm$(k$)='' then goto ClearingOptions
	checkNumber$=lpad$(rtrm$(k$),8)
	k$=cnvrt$('N 2',wbc)&str$(ti3)&checkNumber$
	read #trmstr1,using 'form pos 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',key=k$,release: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd nokey L1190
	tr$(3)=str$(tx3)
	if val(tr$(3))=val(resp$(2)) or val(resp$(2))=0 then goto L1210
	mat ml$(3)
	ml$(1)='Cleared amount does not agree with your check history!'
	ml$(2)='Any corrections to your data must be done through'
	ml$(3)='the check history option. OK to clear; Cancel to skip'
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$='OK' then goto L1210
	if resp$='Cancel' then goto ClearingOptions
	L1190: !
	mat ml$(1)
	ml$(1)='Reference # '&k$(4:11)&' could not be found.  Retry!'
	fnmsgbox(mat ml$,resp$,'',48)
	goto ClearingOptions
L1210: if clr>0 then goto ALREADY_CLEARED
	rewrite #trmstr1,using 'form pos 72,N 6',key=k$: stmtdt
	goto ClearingOptions
ALREADY_CLEARED: !
	fnTos
	respc=0
	fnLbl(1,1,'Check # '&trim$(k$(4:11))&' was previously cleared on '&cnvrt$('PIC(ZZ/ZZ/ZZ)',clr)&'!',60,1)
	fnLbl(2,1,'Correct cleared date:',30,1)
	fnTxt(2,33,10,0,1,'1',0,'Enter the new date if cleared in error on another date.  Use the old cleared date if correct.  Leave blank to unclear. ')
	resp$(respc+=1)=str$(stmtdt)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto ClearingOptions
	cdte=val(resp$(1)) ! statement date cleared
	! if cdte=0 then goto L1330
	! L1330: !
	rewrite #trmstr1,using 'form pos 72,N 6',key=k$: cdte
goto ClearingOptions ! /r

CalculateTotals: ! r:
	restore #trmstr1,key>=cnvrt$('N 2',wbc)&'         ': nokey Menu1
	mat t1=(0)
	L1380: !
	read #trmstr1,using 'form pos 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',release: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof L1600
	tr$(3)=str$(tx3)
	
	if bank_code><wbc then goto L1600
	amt=val(tr$(3)) conv L1380
	if amt=0 then goto L1380
	if codt=0 then goto L1450
	x=val(tr$(2)) conv L1380
	if fndate_mmddyy_to_ccyymmdd(x)>fndate_mmddyy_to_ccyymmdd(codt) then goto L1380
	L1450: !
	if stmtdt=clr then goto L1530
	if clr=0 then goto L1480
	if fndate_mmddyy_to_ccyymmdd(clr)<fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L1380
	L1480: !
	on tcde goto L1490,L1500,L1510,L1490 none L1580 ! NOT CLEARED

	L1490: !
		t1(8)=t1(8)+1 : t1(9)=t1(9)+amt : w0=2 
	goto L1580 ! NOT CLEARED CHECKS
	L1500: !
		t1(6)=t1(6)+1 : t1(7)=t1(7)+amt : w0=1 
	goto L1580 ! NOT CLEARED DEPOSITS
	L1510: !
	if amt>0 then goto L1500
	amt=-amt
	goto L1490
	L1530: !
	on tcde goto L1540,L1550,L1560,L1540 none L1580
	L1540: !
	t1(1)=t1(1)+1 : t1(2)=t1(2)+amt : w0=4
	goto L1580 ! CLEARED CHECKS
	
	L1550: !
		t1(3)=t1(3)+1 : t1(4)=t1(4)+amt : w0=3
	goto L1580 ! CLEARED DEPOSTIS
	
	L1560: !
		if amt>0 then goto L1550
		amt=-amt
	goto L1540
	
	L1580: !
	if ti3=5 then gosub L2550
goto L1380
	L1600: !
	if ti3><5 then goto BankTotalScreen
	fnopenprn
	for j=1 to 4
		wp=j+50
		if w(j)=0 then goto WPNEXTJ
		pr #wp,using L1660: t2(j)
		L1660: form pos 19,'  ----------',skip 1,pos 19,n 12.2,skip 1
		! If NW=1 Then pr #WP: NEWPAGE
		close #wp:
		open #wp: 'Name=[Temp]\RPT'&str$(j)&'.'&wsid$,display,input
		do
			linput #wp: line$ eof EO_WP
			pr #255: line$
		loop
		EO_WP: !
		pr #255: newpage
		close #wp:
		WPNEXTJ: !
	next j
	fncloseprn
goto Menu1 ! /r
BankTotalScreen: ! r:
	fnTos
	respc=0
	fnLbl(1,1, rtrm$(bn$(1:30)),45,2)
	fnLbl(3,1,'   Statement Date:      '&ltrm$(cnvrt$('PIC(##/##/##)',stmtdt)),45,1)
	fnLbl(4,1,'Previous Statement Balance:'&cnvrt$('pic(----,---,---.##)',bgbal),45,1)
	fnLbl(5,1,cnvrt$('N 5',t1(3))&' Deposit(s) Cleared:'&cnvrt$('pic(----,---,---.##)',t1(4)),45,1)
	fnLbl(6,1, cnvrt$('N 5',t1(1))&' Withdrawal(s) Cleared:'&cnvrt$('pic(----,---,---.##)',t1(2)),45,1)
	t1(5)=bgbal+t1(4)-t1(2)
	fnLbl(7,1, 'Calculated Statement Balance:'&cnvrt$('pic(----,---,---.##)',t1(5)),45,1)
	fnLbl(8,1, 'Bank Statement Balance:'& cnvrt$('pic(----,---,---.##)',stmtbal),45,1)
	fnLbl(9,1, 'Out of Balance Amount:'&cnvrt$('pic(----,---,---.##)',stmtbal-t1(5)),45,1)
	fnLbl(10,1, 'Uncleared Deposit(s):'&cnvrt$('pic(----,---,---.##)',t1(7)),45,1)
	fnLbl(11,1, cnvrt$('N 5',t1(8))&' Uncleared Withdrawal(s):'&cnvrt$('pic(----,---,---.##)',t1(9)) ,45,1)
	t1(10)=bgbal+t1(7)+t1(4)-t1(9)-t1(2)
	fnLbl(12,1,'Calculated Book Balance:'&cnvrt$('pic(----,---,---.##)',t1(10)) ,45,1)
	fncloseprn
	cutoffbal=bal
	restore #trmstr1,key>=cnvrt$('N 2',wbc)&'         ': nokey EO_ADDING_BALANCE
L1970: read #trmstr1,using 'form pos 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof EO_ADDING_BALANCE
	tr$(3)=str$(tx3)
	if bank_code><wbc then goto L1970
	x=val(tr$(2)) conv L1970
	if fndate_mmddyy_to_ccyymmdd(x)=<fndate_mmddyy_to_ccyymmdd(codt) then goto L1970
	if tcde=1 then cutoffbal=cutoffbal+val(tr$(3)) ! add checks back
	if tcde=2 then cutoffbal=cutoffbal-val(tr$(3)) ! subtract deposits
	if tcde=3 then cutoffbal=cutoffbal-val(tr$(3)) ! adjustments
	goto L1970
EO_ADDING_BALANCE: !
	fnLbl(11,1,'Book Balance as of'&cnvrt$('PIC(ZZ/ZZ/ZZ)',codt)&':'&cnvrt$('pic(----,---,---.##)',cutoffbal),45,1)
	fnCmdSet(3): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Menu1
	if ckey=1 then gosub L2120 : goto BankTotalScreen
goto BankTotalScreen ! /r
 
L2120: ! r:
	fnopenprn
	pr #255,using L2140: env$('cnam')
	pr #255,using L2140: bn$(1:30)
	L2140: form pos 20,cc 40
	pr #255,using L2140: 'Bank Reconciliation'
	pr #255,using L2140: 'Statement Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',stmtdt)
	pr #255: ''
	pr #255: 'Previous Statement Balance: '&cnvrt$('N 11.2',bgbal)
	pr #255: cnvrt$('N 5',t1(3))&' Deposits Total:   '&cnvrt$('N 15.2',t1(4))
	pr #255: cnvrt$('N 5',t1(1))&' Withdrawals Total: '&cnvrt$('N 14.2',t1(2))
	pr #255: 'Bank Statement Balance:'&cnvrt$('N 16.2',stmtbal)
	pr #255: 'Calculated Statement Bal:'&cnvrt$('N 14.2',t1(5))
	pr #255: 'Out of Balance Amount:'&cnvrt$('N 17.2',stmtbal-t1(5))
	pr #255: ''
	pr #255: cnvrt$('N 5',t1(6))&' Uncleared Deposits:'&cnvrt$('N 14.2',t1(7))
	pr #255: cnvrt$('N 5',t1(8))&' Uncleared Withdrawals:'&cnvrt$('N 11.2',t1(9))
	pr #255: 'Actual Bank Balance:'&cnvrt$('N 19.2',t1(10))
return ! /r
 
PrintListings: ! r:
	hdw$(1)='Deposits not cleared as of '&cnvrt$('PIC(ZZ/ZZ/ZZ)',stmtdt)
	hdw$(2)='Withdrawals not cleared as of '&cnvrt$('PIC(ZZ/ZZ/ZZ)',stmtdt)
	hdw$(3)='Deposits cleared on '&cnvrt$('PIC(ZZ/ZZ/ZZ)',stmtdt)
	hdw$(4)='Withdrawals cleared on '&cnvrt$('PIC(ZZ/ZZ/ZZ)',stmtdt)
	fnTos
	respc=0 : mat resp$=('')
	! fnFra(1,1,4,60,'Select Listings to Print','Choose any of the four listing',0)
	fnChk(1,3,hdw$(1),0)
	resp$(respc+=1)=''
	fnChk(2,3,hdw$(2),0)
	resp$(respc+=1)=''
	fnChk(3,3,hdw$(3),0)
	resp$(respc+=1)=''
	fnChk(4,3,hdw$(4),0)
	resp$(respc+=1)=''
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto Menu1
	if resp$(1)(1:1)='T' then w(1)=1 else w(1)=0
	if resp$(2)(1:1)='T' then w(2)=1 else w(2)=0
	if resp$(3)(1:1)='T' then w(3)=1 else w(3)=0
	if resp$(4)(1:1)='T' then w(4)=1 else w(4)=0
	fncloseprn
	for j=1 to 4
		if w(j)=0 then goto L2520
		wp=j+50
		open #wp: 'Name=[Temp]\RPT'&str$(j)&'.'&wsid$&',Size=0,RecL=132,Replace',d,o
	L2520: !
	next j
	mat t2=(0)
	goto CalculateTotals
	L2550: !
	if w(w0)=0 then goto L2610
	wp=w0+50
	if w(w0)=1 then gosub WP_HEADER
	pr #wp,using L2590: tr$(1),val(tr$(2)),amt,tr$(5),clr pageoflow WP_PGOF
	L2590: form pos 1,c 10,pic(zz/zz/zz),n 12.2,x 2,c 37,pic(zz/zz/zz),skip 1
	t2(w0)=t2(w0)+amt
	L2610: !
return ! /r
WP_PGOF: ! r:
	pr #wp: newpage
	gosub WP_HEADER
	continue  ! /r
WP_HEADER: ! r:
	pr #wp,using L2710: date$('mm/dd/yy'),env$('cnam')&' - Bank Account # '&str$(wbc),time$,' Reconciliation Listing',hdw$(w0),'Page',w(w0),dat$
	L2710: form skip 3,pos 1,c 8,cc 70,skip 1,pos 1,c 8,cc 70,skip 1,pos 9,cc 70,skip 1,pos 1,c 4,n 4,cc 70,skip 2
	pr #wp: 'Check or                                                              Date'
	pr #wp: 'Ref Numb    Date       Amount   Name or Description                  Cleared'
	pr #wp: '________  ________  __________  ___________________________________  ________'
	w(w0)=w(w0)+1
return  ! /r
 
L2790: r1=2
SCR_CLEAR_BY_RANGE: ! r: clear by range of check numbers or by date
	fnTos
	respc=0
	clear_from_range=1 ! return here from grid
	fnLbl(1,43,'',1,0)
	if ti3=2 then goto L2900
	fnLbl(1,1,'Lowest Check Number to Clear:',30,1)
	fnTxt(1,32,8,0,1,'',0,'If your bank statement lists checks in the order cleared and indicate breaks in the check numbers, it is very easy to clear checks using this feature.')
	resp$(respc+=1)=''
	fnLbl(2,1,'Highest Check Number to Clear:',30,1)
	fnTxt(2,32,8,0,1,'',0,'Enter the last check number in the series.')
	resp$(respc+=1)=''
	goto L2940
	L2900: !
	fnLbl(1,1,'Lowest Date to Clear:',30,1)
	fnTxt(1,32,10,0,1,'1',0,'Normally you would use beginning of the month, but you could back the date up a few days to get any uncleared from the previous month.')
	resp$(respc+=1)=''
	fnLbl(2,1,'Highest Date to Clear:',30,1)
	fnTxt(2,32,10,0,1,'1',0,'Normally you would use the last day of the month, but if the bank''s cutoff date is different, you may want to use it.')
	resp$(respc+=1)=''
	L2940: !
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto Menu1
	l1=val(resp$(1)) ! either lowest date or lowest check #
	h1=val(resp$(2)) ! either highest date or highest check #
	if ti3=2 then
		l1=fncd(l1)
		h1=fncd(h1)
	end if
	if ti3=1 then k$=cnvrt$('N 2',wbc)&str$(ti3)&lpad$(trim$(resp$(1)),8)
	if ti3=2 then k$=cnvrt$('N 2',wbc)&str$(ti3)&lpad$(str$(0),8)
	restore #trmstr1,key>=k$: nokey NOTHING_IN_RANGE
goto L3060 ! /r
NOTHING_IN_RANGE: ! r:
	mat ml$(1)
	ml$(1)='Nothing found in this range.'
	fnmsgbox(mat ml$,resp$,'',48)
goto SCR_CLEAR_BY_RANGE ! /r
L3060: ! pr f '1,2,C 8,R,N': ' Check #'   ! do I want some kind of grid here???? kj
! pr f '1,11,C 10,R,N': '  Amount'
DO_CLEAR_BY_RANGE: ! r:
	read #trmstr1,using 'form pos 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof FINIS_DCBR
	tr$(3)=str$(tx3)
	if bank_code><wbc or tcde><ti3 then goto SCR_CLEAR_BY_RANGE
	if ti3=1 then goto L3150
	if clr>0 then goto DO_CLEAR_BY_RANGE
	tr2=fncd(val(tr$(2))) conv DO_CLEAR_BY_RANGE
	if tr2<l1 or tr2>h1 then goto DO_CLEAR_BY_RANGE
	goto L3170
	L3150: !
	h2=val(tr$(1)) conv DO_CLEAR_BY_RANGE
	if h2>h1 then displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST ! Goto 3710
	L3170: !
	rewrite #trmstr1,using 'form pos 72,N 6': stmtdt
	goto DO_CLEAR_BY_RANGE
	FINIS_DCBR: !
goto DISPLAY_CLEARED_SO_FAR ! /r
DISPLAY_CLEARED_SO_FAR: ! r: show what has been cleared
	displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST
	if ti><2 then goto DCSF_FINIS ! don't know what the following logic is; probably can delete
	if rtrm$(tr$(4))='' then goto DCSF_FINIS
	read #paymstr1,using 'form pos 9,C 30,pos P1,PD 3',key=tr$(4),release: sn$,ta1 nokey DCSF_FINIS
	if rtrm$(tr$(4))='' then adr=ta1=0 else adr=ta1
	dim aa(2)
	mat aa=(0)
	gl$=salgl$=''
	L3350: !
	if adr=0 then goto DCSF_FINIS
	read #payeeglbreakdown,using L3370,rec=adr: gl$,pct,de$,nta noRec DCSF_FINIS
	L3370: form pos 9,c 12,pd 3.2,c 30,pd 3
	amt=round(val(tr$(3))*pct*.01,2)
	if rtrm$(de$)='' then de$=tr$(5)(1:30)
	lr5=lrec(tralloc)+1
	write #tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,X 3,C 12',rec=lr5: bank_code,tcde,tr$(1),gl$,amt,de$,0,''
	tac=tac+amt
	if aa(1)=0 then aa(1)=lr5
	! If AA(2)>0 Then  Rewrite #TRALLOC,Using 'form pos 65,X 3',Rec=AA(2): LR5
	aa(2)=lr5
	! Mat TR=AA
	! rewrite #TRMSTR1,Using 'form pos 79,2*PD 3',Rec=CR1: MAT TR
	ti=3
	pr f '7,46,C 18,R,N': cnvrt$('N 16.2',tac)
	adr=nta
	goto L3350
	DCSF_FINIS: !
return  ! /r
FREE_DPAMNTS: ! r:
	fnFree('[Q]\CLmstr\dpAmnts.h[cno]')
continue  ! /r
 
ClearDepositsByAmount: ! r: clear deposits by amounts
	if cda=1 then goto DPAMENU
	L3600: !
	open #hDpAmnts=fnH: 'Name=[Q]\CLmstr\dpAmnts.h[cno],Use,RecL=20',i,outi,r ioerr FREE_DPAMNTS
	open #hDpDates=fnH: 'Name=[Q]\CLmstr\dpDates.h[cno],Use,RecL=150',i,outi,r
	read #hDpDates,using L3630,rec=1: mat dpd noRec L3650
	L3630: form pos 1,25*n 6
	goto L3660
	L3650: !
		write #hDpDates,using L3630,rec=1: mat dpd
	L3660: !
	open #hDpTypes=fnH: 'Name=[Q]\CLmstr\dpTypes.h[cno],Use,RecL=3',i,outi,r
	read #hDpTypes,using FdpTypes,rec=1: mat dpt noRec L3700
	FdpTypes: form pos 1,3*n 1
	goto L3710
	L3700: !
	write #hDpTypes,using FdpTypes,rec=1: mat dpt
	L3710: !
	cda=1
goto DPAMENU ! /r

DPAMENU: ! r:
	fnTos
	respc=0
	fnFra(1,1,9,60,'Reconciliation Options','Used to clear deposita by amounts and not by reference number',0)
	frame=1
	fnOpt(1,3,'Erase Previous Amounts & Dates',0,frame)
	if sel_code=1 or sel_code=0 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(2,3,'Enter Deposit Amounts',0,frame)
	if sel_code=2 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(3,3,'Enter Types to Add Together',0,frame)
	if sel_code=3 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(4,3,'Enter Dates to Add Together',0,frame)
	if sel_code=4 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(5,3,'Clear Matches',0,frame)
	if sel_code=5 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(6,3,'Print listing of Un-Matched',0,frame)
	if sel_code=6 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(7,3,'Print Listing of All Entries',0,frame)
	if sel_code=7 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(8,3,'Make Corrections',0,frame)
	if sel_code=8 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
! fnCOMBOA('bankrec-3',3,30,MAT ITEM3$,'Select the funtion you would like to perform.  Normally you would Erase Previous, Enter Amounts, Clear Matches and check your totals')!
	resp$(respc+=1)=item3$(1)
	resp$(2)='True'
	if t1 =0 and t2=0 then goto L3890
	fnLbl(12,1,'Deposits Entered: '&cnvrt$('pic(ZZ,ZZZ,ZZZ.##)',t1),40,0)
	fnLbl(13,1,'Deposits Cleared: '&cnvrt$('pic(ZZ,ZZZ,ZZZ.##)',t2),40,0)
L3890: fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto Menu1
	for j=1 to 8
		if resp$(j)(1:1)='T' then sel_code=ti1=j: goto L3940
	next j
L3940: if ti1=3 or ti1=4 then goto DPAMENU ! these two options not finished  kj
on ti1 goto L3960,L4000,DPTYPES,EnterDpDates,L5260,PRINT_EDITS,PRINT_EDITS,COR1 none DPAMENU
L3960: ! r:
	close #hDpAmnts,free: ioerr ignore
	close #hDpDates,free: ioerr ignore
	close #hDpTypes: ioerr ignore
goto L3600 ! /r

L4000: !
	if lrec(hDpAmnts)=0 then goto ENTER_DEPOSITS_CLEARED
	read #hDpAmnts,using L4120,rec=lrec(hDpAmnts): tr$,am1,ti$
ENTER_DEPOSITS_CLEARED: !
	fnTos
	respc=0
	fnLbl(1,1,'Amount to Clear:',25,1)
	fnTxt(1,28,12,0,1,'10',0,'Enter each deposit amount that shows as cleared on the bank statement.')
	resp$(respc+=1)=''
	if am1>0 then fnLbl(2,1,'Last Amount Entered:'&cnvrt$('N 13.2',am1),38,1)
	fnCmdSet(2) : ckey=fnAcs(mat resp$)
	if ckey=5 then goto PRINT_EDITS
	am1=val(resp$(1)) ! deposit amount entered
	if am1=0 then goto PRINT_EDITS
	write #hDpAmnts,using L4120: '',am1,''
L4120: form pos 1,c 8,pd 10.2,c 2
	goto ENTER_DEPOSITS_CLEARED
PRINT_EDITS: t1=t2=0
	fnopenprn
	if ti1=6 then pr #255,using 'form pos 10,cc 60': 'Uncleared Entries'
	if ti1=7 then pr #255,using 'form pos 10,cc 60': 'Listing of All Entries Entered For Clearing'
	for j=1 to lrec(hDpAmnts)
		read #hDpAmnts,using L4120,rec=j: tr$,am1,ti$
		if am1=0 then goto L4270
		if ti1<6 then goto L4250
		if rtrm$(tr$)><'' and ti1=6 then goto L4270
		pr #255,using L4240: j,tr$,am1,ti$
L4240: form pos 1,n 4,x 2,c 10,n 10.2,x 2,c 2,skip 1
L4250: t1=t1+am1
		if rtrm$(tr$)><'' then t2=t2+am1
L4270: next j
	if ti1<6 then goto DPAMENU
	pr #255,using L4300: '__________',t1,'=========='
L4300: form pos 17,c 10,skip 1,pos 17,n 10.2,skip 1,pos 17,c 10,skip 1
	fncloseprn
goto DPAMENU ! /r
DPTYPES: ! r: ENTER TYPES TO TOTAL   ! fix  KJ
	for j=1 to 3
		io7$(j)=str$(j*2+11)&',40,N 1,UT,N'
	next j
	close #101: ioerr ignore
	open #101: 'SROW=04,SCOL=10,EROW=19,ECOL=73,Border=Sr,CAPTION=<Deposit Types to Total',display,outIn
	pr f '5,11,C 62': 'This is designed to take care of two or more types of Credit'
	pr f '6,11,C 62': 'Cards that are totaled together by the card company before'
	pr f '7,11,C 62': 'they are transmitted to your bank for deposit.'
	pr f '09,11,c 62': 'credit card deposits begin with a prefix of c1,c2, or c3.'
	pr f '10,11,c 62': 'place a 1 by the types to be added together for 1 bank deposit'
	for j=1 to 3
		pr f str$(j*2+11)&',37,C 3': 'C'&str$(j)
	next j
	pr f '20,30,C 09,B,1': 'Next (F1)'
	L4480: !
	rinput fields mat io7$: mat dpt conv CONV7
	if ce>0 then io7$(ce)(ce1:ce2)='U': ce=0
	if ckey>0 then goto L4570 else ce=curfld
	L4510: !
	ce=ce+1: if ce>udim(io7$) then ce=1
	L4520: !
	io7$(ce)=rtrm$(uprc$(io7$(ce))) : ce1=pos(io7$(ce),'U',10) : if ce1=0 then goto L4510
	ce2=ce1+1 : io7$(ce)(ce1:ce1)='UC' : goto L4480
	CONV7: !
	if ce>0 then io7$(ce)(ce1:ce2)='U'
	ce=cnt+1
	pr f '24,78,C 1': bell : goto L4520
	L4570: !
	rewrite #hDpTypes,using FdpTypes,rec=1: mat dpt
goto DPAMENU ! /r
 
EnterDpDates: ! r: ENTER DATES TO TOTAL
	close #108: ioerr L4620
L4620: open #108: 'SROW=03,SCOL=10,EROW=23,ECOL=73,Border=Sr,CAPTION=<Credit Card Dates to be Totaled',display,outIn
	pr #108: newpage
	for j=1 to 5
		io8$(j)='14,'&str$(j*9+15)&',N 6,UT,N'
		io8$(j+5)='16,'&str$(j*9+15)&',N 6,UT,N'
		io8$(j+10)='18,'&str$(j*9+15)&',N 6,UT,N'
		io8$(j+15)='20,'&str$(j*9+15)&',N 6,UT,N'
		io8$(j+20)='22,'&str$(j*9+15)&',N 6,UT,N'
	next j
	pr f '4,11,c 62': 'This is designed to take care of two or more dates on Credit'
	pr f '5,11,c 62': 'Card batched totaled together by the credit card company'
	pr f '6,11,c 62': 'before they are transmitted to your bank for deposit.'
	pr f '08,11,c 62': 'this usually occurs on weekends or when a batch is transmitted'
	pr f '09,11,c 62': 'after 8pm (Eastern time) one day and before 8pm the next.'
	pr f '11,11,C 62': 'You may have 5 different entries with up to 5 Dates per entry'
	for j=1 to 5
		pr f str$(j*2+12)&',16,C 50': 'Entry '&str$(j)&'        +        +        +        +'
	next j
	pr f '13,24,C 42,N': '         (use only mmddyy format)'
	pr f '24,35,C 09,B,1': 'Next (F1)'
L4820: rinput fields mat io8$: mat dpd conv ERR8
	if ce>0 then io8$(ce)(ce1:ce2)='U': ce=0
	if ckey>0 then goto L4910 else ce=curfld
L4850: ce=ce+1: if ce>udim(io8$) then ce=1
L4860: io8$(ce)=rtrm$(uprc$(io8$(ce))) : ce1=pos(io8$(ce),'U',10) : if ce1=0 then goto L4850
	ce2=ce1+1 : io8$(ce)(ce1:ce1)='UC' : goto L4820
	if ce>0 then io8$(ce)(ce1:ce2)='U'
	ce=cnt+1
ERR8: pr f '24,78,C 1': bell : goto L4860
L4910: dpd$=cnvrt$('PIC(######)',stmtdt)
	mo2=val(dpd$(1:2))
! da2=val(dpd$(3:4))
	yr2=val(dpd$(5:6))
	for ce=1 to 25
		j1=int((ce-1)/5)+1
		j2=ce-(j1-1)*5
		if dpd(j1,j2)=0 then goto L5090
		dpd$=cnvrt$('PIC(######)',dpd(j1,j2))
		mo1=val(dpd$(1:2))
		da1=val(dpd$(3:4))
		yr1=val(dpd$(5:6))
		if mo1<01 or mo1>12 then goto ERR8
		if mo1=mo2 and yr1=yr2 then goto L5080 ! SAME MONTH SAME YEAR
		if yr1=yr2 and mo1+1=mo2 then goto L5080 ! PREVIOUS MONTH SAME YEAR
		if yr1+1><yr2 then goto ERR8
		if mo2=1 and mo1=12 then goto L5080 else goto ERR8
L5080: if da1<01 or da1>31 then goto ERR8
L5090: next ce
	ce=0
	rewrite #hDpDates,using L3630,rec=1: mat dpd
	goto DPAMENU ! /r
 
COR1: ! r:
	pr newpage
	pr f '10,10,c 50': 'Item Number to correct:'
	pr f '12,35,C 10,B,99': 'Stop (Esc)'
L5170: input fields '10,50,Nz 4,UT,N': cr1 conv L5170
	if cr1=0 or ckey=5 then goto DPAMENU
	if cr1<1 or cr1>lrec(hDpAmnts) then goto L5170
	read #hDpAmnts,using L4120,rec=cr1: tr$,am1,ti$
	pr f '10,10,C 60': 'Correct Amount (0 to Delete):'
	rinput fields '10,50,N 10.2,UT,N': am1
	rewrite #hDpAmnts,using L4120,rec=cr1: tr$,am1,ti$
goto COR1 ! /r
 
L5260: ! r:
	restore #trmstr1,key>=lpad$(str$(wbc),2)&'2        ': nokey DPAMENU
L5270: read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6': k$,tr2,tr3,clr eof L5980
	if val(k$(1:2))><wbc then goto L5980
	if val(k$(3:3))><2 then goto L5980
	if clr><0 then goto L5270
	if fndate_mmddyy_to_ccyymmdd(tr2)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L5270
	for r1=1 to lrec(hDpAmnts)
		read #hDpAmnts,using L4120,rec=r1: tr$,am1,ti$
		if am1=0 then goto L5960
		if rtrm$(tr$)><'' then goto L5960
		if k$(4:5)<'C1' or k$(4:5)>'C3' then goto L5910
		ct1=val(k$(5:5))
		! CCADD: !
		ckd=am3=0
		ckd1=val(k$(6:11)) conv L5910
		for j1=1 to 5
			for j2=1 to 5
				if dpd(j1,j2)=0 then goto L5450
				if dpd(j1,j2)=ckd1 then ckd=j1: goto L5470
				L5450: !
			next j2
		next j1
		L5470: !
		if dpt(ct1)=0 and ckd=0 then goto L5910
		if dpt(ct1)=1 then goto L5520
		am3=tr3
		k2$=k$
		goto L5570
		
		L5520: !
		for j=1 to 3
			if dpt(j)=0 then goto L5650
			k2$=k$(1:4)&str$(j)&k$(6:11)
			read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5570
			if clr=0 then am3=am3+am2
			L5570: !
			if ckd=0 then goto L5650
			for j1=1 to 5
				if dpd(ckd,j1)=0 then goto L5640
				if j1=j2 then goto L5640
				k2$=k2$(1:5)&cnvrt$('PIC(######)',dpd(ckd,j1))
				read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5640
				if clr=0 then am3=am3+am2
				L5640: !
			next j1
			L5650: !
			if dpt(ct1)=0 then goto L5670
		next j
L5670: if am3><am1 then goto L5880
		if dpt(ct1)=1 then goto L5720
		rewrite #trmstr1,using 'form pos 72,N 6',key=k$: stmtdt
		k2$=k$
		goto L5770
L5720: for j=1 to 3
			if dpt(j)=0 then goto L5850
			k2$=k$(1:4)&str$(j)&k$(6:11)
			read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5850
			if clr=0 then
				rewrite #trmstr1,using 'form pos 72,N 6',key=k2$: stmtdt
			end if
L5770: if ckd=0 then goto L5850
			for j1=1 to 5
				if dpd(ckd,j1)=0 then goto L5840
				if j1=j2 then goto L5840
				k2$=k2$(1:5)&cnvrt$('PIC(######)',dpd(ckd,j1))
				read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6',key=k2$: k2$,tr2,am2,clr nokey L5840
				if clr=0 then
					rewrite #trmstr1,using 'form pos 72,N 6',key=k2$: stmtdt
				end if
L5840: next j1
L5850: if dpt(ct1)=0 then goto L5870
		next j
L5870: rewrite #hDpAmnts,using L4120,rec=r1: k$(4:11)
L5880: read #trmstr1,using 'form pos 1,C 11',key=k$: k$
		if am3=am1 then goto L5270
		goto L5960
L5910: if tr3><am1 then goto L5960
		rewrite #hDpAmnts,using L4120,rec=r1: k$(4:11)
		rewrite #trmstr1,using 'form pos 72,N 6',key=k$: stmtdt
		goto L5270
L5960: next r1
	goto L5270
L5980: !
goto DPAMENU ! /r
Xit: fnXit
CLEAR_TRANSACTIONS_FROM_LIST: ! r:
	lastrec=nextrec=total=0
	displayattop$='True'
	if ti3=1 then type$='Checks' else type$='Deposits'
	close #clearing: ioerr ignore
	open #clearing=89: 'Name=[Q]\CLmstr\clearing.H'&wsid$&',replace,RecL=33',i,outi,r
	if ti3=2 then
		restore #trmstr1,key>=lpad$(str$(wbc),2)&'2        ': nokey DISPLAY_GRID
	else
		restore #trmstr1,key>=lpad$(str$(wbc),2)&'1        ': nokey DISPLAY_GRID
	end if
L6140: !
	read #trmstr1,using 'form pos 1,C 11,G 6,pd 10.2,pos 72,N 6': k$,tr2,tr3,clr eof DISPLAY_GRID
	if tr3=0 then goto L6140 ! don't display zero amounts
	if val(k$(1:2))><wbc then goto L6140
	if ti3=1 and val(k$(3:3))><1 then goto L6140 ! only disbursments
	if displaycleared=1 and clr=stmtdt then goto L6200 ! display only cleared on this date
	if displaycleared=1 and clr<>stmtdt then goto L6140 ! if only cleared this clearing date, don't allow others to list
	if clr><0 then goto L6140
L6200: !
	write #clearing,using 'form pos 1,C 11,G 6,pd 10.2,N 6': k$,tr2,tr3,clr
	goto L6140
DISPLAY_GRID: !
	mat chdr$(5)
	mat cmask$(5)
	mat flxitm$(5)
	chdr$(1)='Rec'
	chdr$(2)='Reference #'
	chdr$(3)='Date'
	chdr$(4)='Amount'
	chdr$(5)='Cleared'
	cmask$(1)='30'
	cmask$(2)=''
	cmask$(3)='1'
	cmask$(4)='10'
	cmask$(5)='1'
L6240: !
	fnTos
	respc=0 : mat resp$=('')
	fnLbl(1,1,trim$(bn$(1:30))&'-'&type$,65,2)
	fnChk(3,49,'Display at Top:',1)
	resp$(respc+=1)=displayattop$ : respc_display_top=respc : pr 'set respc_display_top to ';respc_display_top
!
	fnflexinit1('Deposit-1',5,3,15,55,mat chdr$,mat cmask$,1) : pr '__________init___________ displayattop$='&displayattop$&', nextrec='&str$(nextrec)
	restore #clearing:
	if nextrec>0 and displayattop$='True' then
		fn_ctfl_add_grid_items(1)
		fn_ctfl_add_grid_items(2)
	else ! display them all in indexed order
		fn_ctfl_add_grid_items
	end if
!
	respc_grid=2
!
	fnLbl(2,30,'Total Cleared:',16,1)
	fnLbl(2,49,cnvrt$('pic($$$,$$$,$$$,$$#.##)',total),1)
!
	fnCmdKey('&Display Cleared',3,0,0,'Displays all transactions cleared on this clearing date')
	fnCmdKey('Display &Uncleared',2,0,0,'Displays all remaining uncleared transactions')
	if clear_from_range=1 then
		fnCmdKey('&Clear by &Range',6,1,0,'Enter another range of reference numbers')
	else if clear_from_range=0 then
		fnCmdKey('C&lear',1,1,0,'Clear the highlited transaction')
	end if
	fnCmdKey('&Complete',5,0,1,'Return to Bank Reconciliation menu')
	ckey=fnAcs(mat resp$)
	displaycleared=total= clear_from_range=0
	if ckey=5 or ckey=99 then goto BANK_STMT_INFO
	displayattop$=resp$(respc_display_top) ! do you want next uncleared check at the top of the screen
	if ckey=2 then ! redisplay on uncleared
		goto CLEAR_TRANSACTIONS_FROM_LIST
	else if ckey=3 then ! displays only cleared on this date
		displaycleared=1
		goto CLEAR_TRANSACTIONS_FROM_LIST
	else if ckey=6 then ! return to clearing by range of reference numbers
		goto SCR_CLEAR_BY_RANGE
	end if
	read #clearing,using 'form pos 1,C 11,G 6,pd 10.2,N 6',rec=val(resp$(respc_grid)): k$,tr2,tr3,clr
	if clr=0 then newclr=stmtdt else newclr=0 ! if no previous clearing date, use new one; if it has a date, unclear it
	rewrite #clearing,using 'form pos 28,n 6',rec=val(resp$(respc_grid)): newclr
	rewrite #trmstr1,using 'form pos 72,N 6',key=k$: newclr ! update the transaction history
	lastrec=val(resp$(respc_grid))
	if lastrec+1 <= lrec(clearing) then nextrec=lastrec+1 else nextrec=1
	if lrec(clearing)=1 then nextrec=0
	goto L6240
! /r CLEAR_TRANSACTIONS_FROM_LIST
def fn_ctfl_add_grid_items(; cagi_type)
	! cagi_type 1 = uncleared only
	! cagi_type 2 = cleared only
	! cagi_type 0 = (default) all
	restore #clearing:
	do
		read #clearing,using 'form pos 1,C 11,G 6,pd 10.2,c 6': flxitm$(2),flxitm$(3),amount,flxitm$(5) eof EO_CAGI_CLEARING
		if cagi_type=1 and trim$(flxitm$(5))<>'0' then goto CAGI_NEXT
		if cagi_type=2 and trim$(flxitm$(5))='0' then goto CAGI_NEXT
		flxitm$(1)=str$(rec(clearing))
		flxitm$(4)=str$(amount)
		if val(flxitm$(5))=stmtdt then total+=amount
		fnflexadd1(mat flxitm$) ! pr 'CAGI TYPE '&str$(cagi_type)&'  '&flxitm$(1)&'-'&flxitm$(4)&'-'&flxitm$(5)
		CAGI_NEXT: !
	loop
	EO_CAGI_CLEARING: !
fnend
include: ertn
