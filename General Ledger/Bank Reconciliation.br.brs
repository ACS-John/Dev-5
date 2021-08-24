! formerly S:\acsGL\BankRec
! Bank reconciliation routines for general ledger

autoLibrary
on error goto Ertn

dim dat$*20,adr(2),gl(3),sf1$*28,pr$(4)*30,whgl$(5)*12
dim sendto$*80,xck$(22),aa(2),prtr(99),k$*21
dim tr$(5)*35,tr(2),de$*30,bn$*40,sn$*50
dim aa(2),whgl(5,3),line$*132,resp$(10)*35,ml$(4)*60,desc$(6)*14
dim item1$(6)*40,item3$(9)*40,chdr$(5)*15
dim cmask$(5)*10,flxitm$(5)*21
dim w(4),hdw$(4)*40,miscgl$(10)*12,misc$(10)*20
dim io7$(9),dpt(3),io8$(25),dpd(5,5)

fnTop(program$, "Reconcile Bank")
fndat(dat$)
open #bankrec1:=1: "Name=[Q]\GLmstr\bankrec.h[cno],KFName=[Q]\GLmstr\Bankrec-idx.h[cno],Shr",internal,outIn,keyed
! Open #BANKREC2:=2: "Name=[Q]\GLmstr\bankrec.h[cno],KFName=[Q]\GLmstr\bankx2.h[cno],Shr",Internal,outIn,Keyed  ! ????? ken
close #82: ioerr ignore
open #82: "Name=[Q]\GLmstr\Bank2"&wsid$&".h[cno],Use,RecL=35",internal,outIn,relative
read #82,using "Form pos 1,c 12",rec=1: wbc$ noRec L270
goto MENU1
L270: !
write #82,using "Form pos 1,c 12",rec=1: wbc$
MENU1: !
close #81: ioerr ignore
L300: open #81: "Name=[Q]\GLmstr\Bank"&wbc$&".h[cno],Use,RecL=32",internal,outIn,relative
	if lrec(81)>0 then
		read #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt
	else
		write #81,using "Form POS 1,N 6,2*PD 5.2,N 6": stmtdt,bgbal,stmtbal,codt
	end if
BANK_STMT_INFO: !
	fnTos
	respc=0
	mylen=30 : mypos=mylen+2
	fnLbl(1,1,"Bank Account:",mylen,1)
	fnqgl(1,mypos,0,2)
	resp$(respc+=1)=fnrgl$(wbc$)
	fnLbl(3,1,"Statement Date:",mylen,1)
	fnTxt(3,mypos,10,0,1,"3",0,"We suggest you always use the month end date.")
	resp$(respc+=1)=str$(stmtdt)
	fnLbl(4,1,"Previous Statement Balance:",mylen,1)
	fnTxt(4,mypos,12,0,1,"10",0,"Pull this information directly from your current bank statement. ")
	resp$(respc+=1)=str$(bgbal)
	fnLbl(5,1,"Current Statement Balance:",mylen,1)
	fnTxt(5,mypos,12,0,1,"10",0," ")
	resp$(respc+=1)=str$(stmtbal)
	fnLbl(6,1,"Reconciliation Cutoff Date:",mylen,1)
	fnTxt(6,mypos,10,0,1,"3",0,"The cutoff date would normally be the last day of the month.")
	resp$(respc+=1)=str$(codt)
	fnLbl(8,1,"Reconciliation Options:",28,1)
	item1$(1)="Enter Cleared Checks"
	item1$(2)="Enter Cleared Deposits"
	item1$(3)="Clear All Adjustments"
	item1$(4)="Calculate Bank Totals"
	item1$(5)="Print Reconciliation Listing"
	item1$(6)="Make Corrections"
	fncomboa("bankrec-1",8,30,mat item1$,"Select the funtion you would like to perform.  Normally you would clear the checks, deposits, adjustments and then calculate the bank totals")
	if selection=0 then selection=1 : resp$(respc+=1)=item1$(selection)
	fnCmdKey("&Display Balances",3,0,0,"Displays previous balances for this bank account.")
	fnCmdKey("&Next",1,1,0,"Proceed to next options")
	fnCmdKey("&Cancel",5,0,1,"Returns to main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	wbc$=fnagl$(resp$(1)) ! working bank account
	rewrite #82,using "Form pos 1,c 12",rec=1: resp$(1)
	if ckey=3 then goto MENU1 ! redisplay balances
	bn$=resp$(1)(13:30)
	stmtdt=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! convert date back to mmddyy format
	bgbal=val(resp$(3))
	stmtbal=val(resp$(4))
	codt=val(resp$(5)(5:6))*10000+val(resp$(5)(7:8))*100+val(resp$(5)(3:4)) ! convert date back to mmddyy format
	for j=1 to 6
		if resp$(6)=item1$(j) then ti3=j: selection=j: goto L620
	next j
L620: if ti3=6 then fnchain("S:\General Ledger\Access Bank Rec File")
	close #81: ioerr L640
L640: open #81: "Name=[Q]\GLmstr\Bank"&wbc$&".h[cno],Use,RecL=32",internal,outIn,relative
	if lrec(81)=0 then write #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt : goto L670
	rewrite #81,using "Form POS 1,N 6,2*PD 5.2,N 6",rec=1: stmtdt,bgbal,stmtbal,codt
L670: close #81: ioerr L680
L680: if ti3<3 then wtt=ti3
	on ti3 goto CLEARING_OPTIONS,CLEARING_OPTIONS,CLEARING_ADJUSTMENTS,CALCULATE_TOTALS,PRINT_LISTINGS none MENU1

CLEARING_ADJUSTMENTS: !
	fnTos
	respc=0
	fnLbl(1,60,"",1,0)
	fnLbl(1,1,"If the adjustments have been previously cleared",50,0)
	fnLbl(2,1,"with a wrong date, then enter the previously",50,0)
	fnLbl(3,1,"cleared date (mmddyy) (otherwise leave blank):",50,0)
	fnTxt(3,48,8,0,1,"1",0,"")
	resp$(respc+=1)=""
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto MENU1
	pcd1=val(resp$(1)) ! clear old adjustments
	restore #bankrec1,key>=wbc$&"3        ": nokey MENU1
L820: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 2,N 1,POS 12,N 6,POS 72,N 6': bankgl$,tcde,d1,clr eof MENU1
	if tcde><3 then goto MENU1
	if bankgl$><wbc$ then goto MENU1
	if fndate_mmddyy_to_ccyymmdd(d1)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L820
	if clr><pcd1 then goto L820
	rewrite #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,POS 12,N 6,POS 72,N 6': bankgl$,tcde,d1,stmtdt eof MENU1
	goto L820

CLEARING_OPTIONS: !
	fnTos
	respc=0
	fnLbl(1,1,"Reference Number to Clear:",28,1)
	fnTxt(1,31,8,0,1,"",0,"If you wish to clear one transaction at a time, enter the reference # here.")
	resp$(respc+=1)=""
	fnLbl(2,1,"Cleared Amount:",28,1)
	fnTxt(2,31,12,0,1,"10",0,"You do not have to enter the amount.  If you do, it will compare the amount you enter to your check history and warn you of any differences. ")
	resp$(respc+=1)=""
	if ti3=1 then let fnButton(1,50,"Clear Range of Checks",60,"This option allows you to clear a range of checks without having to enter each check number",0,20)
	if ti3=2 then let fnButton(1,50,"Clear Deposits by Amount",61,"This option allows you to clear deposits by entering the amount without having to know the reference number.",0,30)
	if ti3=2 then let fnButton(3,50,"Clear Deposits by Date Range",62,"This option allows you to clear deposits by entering a date range.",0,30)
	if ti3=2 then let fnButton(5,50,"Clear Deposits from List",63,"This option allows you to clear deposits from a listing of all outstanding deposits.",0,30)
	if ti3=1 then let fnButton(3,50,"Clear Checks from List",64,"This option allows you to clear checks from a listing of all outstanding checks.",0,20)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto MENU1
	k$=resp$(1) ! check # to clear
	if ckey=61 then
		ti=2: tcde=wtt=ti3: xck$=k$
		hamt=tr3=pcde=0
		goto CLEAR_DEPOSITS_BY_AMOUNT
	end if
	if ti3=1 and ckey=60 then goto L2810 ! clear range of checks
! If TI3=2 AND ckey=3 Then Goto 4440 ! clear deposits by amount
	if ti3=2 and ckey=62 then goto L2810 ! deposits by date range
	if ti3=2 and ckey=63 then goto CLEAR_TRANSACTIONS_FROM_LIST ! deposits from listing of all outstanding deposits
	if ti3=1 and ckey=64 then goto CLEAR_TRANSACTIONS_FROM_LIST ! clear checks from listing
	if ckey=5 or ckey=99 then goto MENU1
	if rtrm$(k$)="" then goto CLEARING_OPTIONS
	xck$=lpad$(rtrm$(k$),8)
	k$=wbc$&str$(ti3)&xck$
	read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',key=k$,release: bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd nokey L1190
	tr$(3)=str$(tx3)
	ckamt=val(resp$(2)) : if val(tr$(3))=ckamt or ckamt=0 then goto L1210
	mat ml$(3)
	ml$(1)="Cleared amount does not agree with your check history!"
	ml$(2)="Any corrections to your data must be done through"
	ml$(3)="the check history option. OK to clear; Cancel to skip"
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then goto L1210
	if resp$="Cancel" then goto CLEARING_OPTIONS
L1190: !
	mat ml$(1)
	ml$(1)="Reference # "&k$(4:11)&" could not be found.  Retry!"
	fnmsgbox(mat ml$,resp$,'',48)
goto CLEARING_OPTIONS
L1210: !
	if clr>0 then goto ALREADY_CLEARED
	rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: stmtdt
goto CLEARING_OPTIONS
ALREADY_CLEARED: !
	fnTos
	respc=0
	fnLbl(1,1,"Check # "&trim$(k$(4:11))&" was previously cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",clr)&"!",60,1)
	fnLbl(2,1,"Correct cleared date:",30,1)
	fnTxt(2,33,10,0,1,"1",0,"Enter the new date if cleared in error on another date.  Use the old cleared date if correct.  Leave blank to unclear. ")
	resp$(respc+=1)=str$(stmtdt)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto CLEARING_OPTIONS
	cdte=val(resp$(1)) ! statement date cleared
	if cdte=0 then goto L1330
L1330: rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: cdte
	goto CLEARING_OPTIONS
CALCULATE_TOTALS: !
	restore #bankrec1,key>=wbc$&"         ": nokey MENU1
	mat t1=(0)
L1380: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',release: bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof L1600
	tr$(3)=str$(tx3)
	if bankgl$><wbc$ then goto L1600
	amt=val(tr$(3)) conv L1380
	if amt=0 then goto L1380
	if codt=0 then goto L1450
	x=val(tr$(2)) conv L1380
	if fndate_mmddyy_to_ccyymmdd(x)>fndate_mmddyy_to_ccyymmdd(codt) then goto L1380
L1450: if stmtdt=clr then goto L1530
	if clr=0 then goto L1480
	if fndate_mmddyy_to_ccyymmdd(clr)<fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L1380
L1480: on tcde goto L1490,L1500,L1510,L1490 none L1580 ! NOT CLEARED
L1490: t1(8)=t1(8)+1: t1(9)=t1(9)+amt : w0=2 : goto L1580 ! NOT CLEARED CHECKS
L1500: t1(6)=t1(6)+1: t1(7)=t1(7)+amt : w0=1 : goto L1580 ! NOT CLEARED DEPOSITS
L1510: if amt>0 then goto L1500
	amt=-amt : goto L1490
L1530: on tcde goto L1540,L1550,L1560,L1540 none L1580
L1540: t1(1)=t1(1)+1: t1(2)=t1(2)+amt : w0=4: goto L1580 ! CLEARED CHECKS
L1550: t1(3)=t1(3)+1: t1(4)=t1(4)+amt : w0=3: goto L1580 ! CLEARED DEPOSTIS
L1560: if amt>0 then goto L1550
	amt=-amt: goto L1540
L1580: if ti3=5 then gosub L2570
	goto L1380
L1600: if ti3><5 then goto BANKTOTALSCREEN
	fnopenprn
	for j=1 to 4
		wp=j+50
		if w(j)=0 then goto WPNEXTJ
		pr #wp,using L1660: t2(j)
L1660: form pos 19,"  ----------",skip 1,pos 19,n 12.2,skip 1
! If NW=1 Then pr #WP: NEWPAGE
		close #wp:
		open #wp: "Name=RPT"&str$(j)&"."&wsid$,display,input
L1700: linput #wp: line$ eof EO_WP
		pr #255: line$
		goto L1700
EO_WP: !
		pr #255: newpage
		close #wp:
! If SENDTO$(1:3)<>"PRN" Then 	! Execute "Sy Copy "&SENDTO$&"+RPT"&STR$(J)&"."&WSID$&" "&SENDTO$
WPNEXTJ: next j
	fncloseprn
	goto MENU1
BANKTOTALSCREEN: !
	pr newpage
	fnTos
	respc=0
	fnLbl(1,1, rtrm$(bn$),45,2)
	fnLbl(2,1,"   Statement Date:      "&ltrm$(cnvrt$("PIC(##/##/##)",stmtdt)),45,1)
	fnLbl(3,1,"Previous Statement Balance:"&cnvrt$("pic(----,---,---.##)",bgbal),45,1)
	fnLbl(4,1,cnvrt$("N 5",t1(3))&" Deposit(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(4)),45,1)
	fnLbl(5,1, cnvrt$("N 5",t1(1))&" Withdrawal(s) Cleared:"&cnvrt$("pic(----,---,---.##)",t1(2)),45,1)
	t1(5)=bgbal+t1(4)-t1(2)
	fnLbl(6,1, "Calculated Statement Balance:"&cnvrt$("pic(----,---,---.##)",t1(5)),45,1)
	fnLbl(7,1, "Bank Statement Balance:"& cnvrt$("pic(----,---,---.##)",stmtbal),45,1)
	fnLbl(8,1, "Out of Balance Amount:"&cnvrt$("pic(----,---,---.##)",stmtbal-t1(5)),45,1)
	fnLbl(9,1, "Uncleared Deposit(s):"&cnvrt$("pic(----,---,---.##)",t1(7)),45,1)
	fnLbl(10,1, cnvrt$("N 5",t1(8))&" Uncleared Withdrawal(s):"&cnvrt$("pic(----,---,---.##)",t1(9)) ,45,1)
	fnLbl(10,1,"Calculated Book Balance:"&cnvrt$("pic(----,---,---.##)",t1(10)) ,45,1)
	fncloseprn
	t1(10)=bgbal+t1(7)+t1(4)-t1(9)-t1(2)
	cutoffbal=bal
	restore #bankrec1,key>=wbc$&"         ": nokey EO_ADDING_BALANCE
L1990: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof EO_ADDING_BALANCE
	tr$(3)=str$(tx3)
	if bankgl$><wbc$ then goto L1990
	x=val(tr$(2)) conv L1990
	if fndate_mmddyy_to_ccyymmdd(x)=<fndate_mmddyy_to_ccyymmdd(codt) then goto L1990
	if tcde=1 then cutoffbal=cutoffbal+val(tr$(3)) ! add checks back
	if tcde=2 then cutoffbal=cutoffbal-val(tr$(3)) ! subtract deposits
	if tcde=3 then cutoffbal=cutoffbal-val(tr$(3)) ! adjustments
	goto L1990
EO_ADDING_BALANCE: !
	fnLbl(11,1,"Book Balance as of"&cnvrt$("PIC(ZZ/ZZ/ZZ)",codt)&":"&cnvrt$("pic(----,---,---.##)",cutoffbal),45,1)
	fnCmdSet(3): ckey=fnAcs(mat resp$)
	if ckey=5 then goto MENU1
	if ckey=1 then gosub L2140 : goto BANKTOTALSCREEN
	goto BANKTOTALSCREEN

L2140: fnopenprn
	pr #255,using L2160: bn$(1:30)
L2160: form pos 20,cc 40
	pr #255,using L2160: "Bank Reconciliation"
	pr #255,using L2160: "Statement Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
	pr #255: ""
	pr #255: "Previous Statement Balance: "&cnvrt$("N 11.2",bgbal)
	pr #255: cnvrt$("N 5",t1(3))&" Deposits Total:   "&cnvrt$("N 15.2",t1(4))
	pr #255: cnvrt$("N 5",t1(1))&" Withdrawals Total: "&cnvrt$("N 14.2",t1(2))
	pr #255: "Bank Statement Balance:"&cnvrt$("N 16.2",stmtbal)
	pr #255: "Calculated Statement Balance:"&cnvrt$("N 10.2",t1(5))
	pr #255: "Out of Balance Amount:"&cnvrt$("N 17.2",stmtbal-t1(5))
	pr #255: ""
	pr #255: cnvrt$("N 5",t1(6))&" Uncleared Deposits:"&cnvrt$("N 14.2",t1(7))
	pr #255: cnvrt$("N 5",t1(8))&" Uncleared Withdrawals:"&cnvrt$("N 11.2",t1(9))
	pr #255: "Actual Bank Balance:"&cnvrt$("N 19.2",t1(10))
return

PRINT_LISTINGS: !
	hdw$(1)="Deposits not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
	hdw$(2)="Withdrawals not cleared as of "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
	hdw$(3)="Deposits cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
	hdw$(4)="Withdrawals cleared on "&cnvrt$("PIC(ZZ/ZZ/ZZ)",stmtdt)
	fnTos
	respc=0 : mat resp$=('')
! fnFra(1,1,4,60,"Select Listings to Print","Choose any of the four listing",0)
	fnChk(1,3,hdw$(1),0)
	resp$(respc+=1)=""
	fnChk(2,3,hdw$(2),0)
	resp$(respc+=1)=""
	fnChk(3,3,hdw$(3),0)
	resp$(respc+=1)=""
	fnChk(4,3,hdw$(4),0)
	resp$(respc+=1)=""
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto MENU1
	if resp$(1)(1:1)="T" then w(1)=1 else w(1)=0
	if resp$(2)(1:1)="T" then w(2)=1 else w(2)=0
	if resp$(3)(1:1)="T" then w(3)=1 else w(3)=0
	if resp$(4)(1:1)="T" then w(4)=1 else w(4)=0
	fnopenprn
	for j=1 to 4
		if w(j)=0 then goto L2540
		wp=j+50
		open #wp: "Name=Rpt"&str$(j)&"."&wsid$&",Size=0,RecL=132,Replace",display,output
L2540: next j
	mat t2=(0)
	goto CALCULATE_TOTALS
L2570: if w(w0)=0 then goto L2630
	wp=w0+50
	if w(w0)=1 then gosub L2690
	pr #wp,using L2610: tr$(1),val(tr$(2)),amt,tr$(5),clr pageoflow L2650
L2610: form pos 1,c 10,pic(zz/zz/zz),n 12.2,x 2,c 37,pic(zz/zz/zz),skip 1
	t2(w0)=t2(w0)+amt
L2630: return

L2650: pr #wp: newpage
	gosub L2690
	continue

L2690: !

	hd2=46-int(len(rtrm$(hdw$(w0)))/2)
	pr #wp,using L2730: date$('mm/dd/yy'),rtrm$(env$('cnam'))&" - Bank Account # "&str$(wbc),time$," Reconciliation Listing",hdw$(w0),"Page",w(w0),dat$
L2730: form skip 3,pos 1,c 8,cc 56,skip 1,pos 1,c 8,cc 56,skip 1,pos 9,cc 56,skip 1,pos 1,c 4,n 4,cc 56,skip 2
	pr #wp: "Check or                                                              Date"
	pr #wp: "Ref Numb    Date       Amount   Name or Description                  Cleared"
	pr #wp: "________  ________  __________  ___________________________________  ________"
	w(w0)=w(w0)+1
return


L2810: c1=r1=2
CLEAR_BY_RANGE: ! clear by range of check numbers or by date
	fnTos
	respc=0
	clear_from_range=1 ! return here from grid
	fnLbl(1,43,"",1,0)
	if ti3=2 then goto L2920
	fnLbl(1,1,"Lowest Check Number to Clear:",30,1)
	fnTxt(1,32,8,0,1,"",0,"If your bank statement lists checks in the order cleared and indicate breaks in the check numbers, it is very easy to clear checks using this feature.")
	resp$(respc+=1)=""
	fnLbl(2,1,"Highest Check Number to Clear:",30,1)
	fnTxt(2,32,8,0,1,"",0,"Enter the last check number in the series.")
	resp$(respc+=1)=""
	goto L2960
L2920: fnLbl(1,1,"Lowest Date to Clear:",30,1)
	fnTxt(1,32,10,0,1,"1",0,"Normally you would use beginning of the month, but you could back the date up a few days to get any uncleared from the previous month.")
	resp$(respc+=1)=""
	fnLbl(2,1,"Highest Date to Clear:",30,1)
	fnTxt(2,32,10,0,1,"1",0,"Normally you would use the last day of the month, but if the bank's cutoff date is different, you may want to use it.")
	resp$(respc+=1)=""
L2960: fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto MENU1
	l1=val(resp$(1)) ! either lowest date or lowest check #
	h1=val(resp$(2)) ! either highest date or highest check #
	if ti3><2 then goto L3030
	l1=fn_cd(l1)
	h1=fn_cd(h1)
L3030: if ti3=1 then k$=wbc$&str$(ti3)&lpad$(str$(l1),8)
	if ti3=2 then k$=wbc$&str$(ti3)&lpad$(str$(0),8)
	restore #bankrec1,key>=k$: nokey L3070
	goto L3080
L3070: mat ml$(1)
	ml$(1)="Nothing found in this range."
	fnmsgbox(mat ml$,resp$,'',48)
	goto CLEAR_BY_RANGE
L3080: ! pr f "1,2,C 8,R,N": " Check #"   ! do I want some kind of grid here???? kj
! pr f "1,11,C 10,R,N": "  Amount"
L3100: read #bankrec1,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd eof DISPLAYCLEAREDSOFAR
	tr$(3)=str$(tx3)
	if bankgl$><wbc$ or tcde><ti3 then goto CLEAR_BY_RANGE
	if ti3=1 then goto L3170
	if clr>0 then goto L3100
	tr2=fn_cd(val(tr$(2))) conv L3100
	if tr2<l1 or tr2>h1 then goto L3100
	goto L3190
L3170: h2=val(tr$(1)) conv L3100
	if h2>h1 then displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST ! Goto 3710
L3190: rewrite #bankrec1,using 'Form POS 72,N 6': stmtdt
! pr f STR$(R1)&","&STR$(C1)&",C 8,N": TR$(1)  ! also some kind of grid here
! pr f STR$(R1)&","&STR$(C1+9)&",C 10,N": TR$(3)
! r1=R1+1
! If R1=8 Then r1=14
! If R1>23 Then r1=2: c1=C1+20 Else Goto 4230
! If C1>62 Then c1=2
! pr f "1,"&STR$(C1)&",C 8,R,N": " Check #"
! pr f "1,"&STR$(C1+9)&",C 10,R,N": "  Amount"
	goto L3100
DISPLAYCLEAREDSOFAR: ! show what has been cleared
	displaycleared=1 : goto CLEAR_TRANSACTIONS_FROM_LIST
FREE_DPAMNTS: !
	fnFree("[Q]\GLmstr\DpAmnts.h[cno]")
	continue

CLEAR_DEPOSITS_BY_AMOUNT: ! clear deposits by amounts
	if cda=1 then goto DPAMENU
L3370: open #dpamnts=91: "Name=[Q]\GLmstr\DpAmnts.h[cno],Use,RecL=20",internal,outIn,relative ioerr FREE_DPAMNTS
	open #92: "Name=[Q]\GLmstr\DpDates.h[cno],Use,RecL=150",internal,outIn,relative
	read #92,using L3400,rec=1: mat dpd noRec L3420
L3400: form pos 1,25*n 6
	goto L3430
L3420: write #92,using L3400,rec=1: mat dpd
L3430: open #93: "Name=[Q]\GLmstr\DPTypes.h[cno],Use,RecL=3",internal,outIn,relative
	read #93,using L3450,rec=1: mat dpt noRec L3470
L3450: form pos 1,3*n 1
	goto L3480
L3470: write #93,using L3450,rec=1: mat dpt
L3480: cda=1

DPAMENU: !
	fnTos
	respc=0
	fnFra(1,1,9,60,"Reconciliation Options","Used to clear deposita by amounts and not by reference number",0)
	frame=1
	fnOpt(1,3,"Erase Previous Amounts & Dates",0,frame)
	if sel_code=1 or sel_code=0 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(2,3,"Enter Deposit Amounts",0,frame)
	if sel_code=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(3,3,"Enter Types to Add Together",0,frame)
	if sel_code=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(4,3,"Enter Dates to Add Together",0,frame)
	if sel_code=4 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(5,3,"Clear Matches",0,frame)
	if sel_code=5 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(6,3,"Print listing of Un-Matched",0,frame)
	if sel_code=6 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(7,3,"Print Listing of All Entries",0,frame)
	if sel_code=7 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(8,3,"Make Corrections",0,frame)
	if sel_code=8 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
! fnCOMBOA("bankrec-3",3,30,MAT ITEM3$,"Select the funtion you would like to perform.  Normally you would Erase Previous, Enter Amounts, Clear Matches and check your totals")!: _
	resp$(respc+=1)=item3$(1)
	resp$(2)="True"
	if t1 =0 and t2=0 then goto L3660
	fnLbl(12,1,"Deposits Entered: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t1),40,0)
	fnLbl(13,1,"Deposits Cleared: "&cnvrt$("pic(ZZ,ZZZ,ZZZ.##)",t2),40,0)
L3660: fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=99 then goto MENU1
	for j=1 to 8
		if resp$(j)(1:1)="T" then sel_code=ti1=j: goto L3710
	next j
L3710: on ti1 goto L3720,L3760,DPTYPES,DPDATES,L5060,PRINT_EDITS,PRINT_EDITS,COR1 none DPAMENU
L3720: close #dpamnts,free: ioerr L3730
L3730: close #92,free: ioerr L3740
L3740: close #93: ioerr L3750
L3750: goto L3370
L3760: if lrec(91)=0 then goto ENTER_DEPOSITS_CLEARED
	read #dpamnts,using L3880,rec=lrec(91): tr$,am1,ti$
ENTER_DEPOSITS_CLEARED: !
	fnTos
	respc=0
	fnLbl(1,1,"Amount to Clear:",25,1)
	fnTxt(1,28,12,0,1,"10",0,"Enter each deposit amount that shows as cleared on the bank statement.")
	resp$(respc+=1)=""
	if am1>0 then let fnLbl(2,1,"Last Amount Entered:"&cnvrt$("N 13.2",am1),38,1)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto PRINT_EDITS
	am1=val(resp$(1)) ! deposit amount entered
	if am1=0 then goto PRINT_EDITS
	write #dpamnts,using L3880: "",am1,""
L3880: form pos 1,c 8,pd 10.2,c 2
	goto ENTER_DEPOSITS_CLEARED
PRINT_EDITS: t1=t2=0
	fnopenprn
	fncloseprn
	if ti1=6 then pr #255,using "form pos 10,cc 60": "Uncleared Entries"
	if ti1=7 then pr #255,using "form pos 10,cc 60": "Listing of All Entries Entered For Clearing"
	for j=1 to lrec(91)
		read #dpamnts,using L3880,rec=j: tr$,am1,ti$
		if am1=0 then goto L4040
		if ti1<6 then goto L4020
		if rtrm$(tr$)><"" and ti1=6 then goto L4040
		pr #255,using L4010: j,tr$,am1,ti$
L4010: form pos 1,n 4,x 2,c 10,n 10.2,x 2,c 2,skip 1
L4020: t1=t1+am1
		if rtrm$(tr$)><"" then t2=t2+am1
L4040: next j
	if ti1<6 then goto DPAMENU
	pr #255,using L4070: "__________",t1,"=========="
L4070: form pos 17,c 10,skip 1,pos 17,n 10.2,skip 1,pos 17,c 10,skip 1
	fncloseprn
	goto DPAMENU
DPTYPES: ! ENTER TYPES TO TOTAL   ! fix  KJ
	for j=1 to 3
		io7$(j)=str$(j*2+11)&",40,N 1,UT,N"
	next j
	pr newpage
	close #101: ioerr L4160
L4160: open #101: "SROW=04,SCOL=10,EROW=19,ECOL=73,Border=Sr,CAPTION=<Deposit Types to Total",display,outIn
	pr f "5,11,C 62": "This is designed to take care of two or more types of Credit"
	pr f "6,11,C 62": "Cards that are totaled together by the card company before"
	pr f "7,11,C 62": "they are transmitted to your bank for deposit."
	pr f "09,11,c 62": "credit card deposits begin with a prefix of c1,c2, or c3."
	pr f "10,11,c 62": "place a 1 by the types to be added together for 1 bank deposit"
	for j=1 to 3
		pr f str$(j*2+11)&",37,C 3": "C"&str$(j)
	next j
	pr f "20,30,C 09,B,1": "Next (F1)"
L4260: rinput fields mat io7$: mat dpt conv CONV7
	if ce>0 then io7$(ce)(ce1:ce2)="U": ce=0
	if ckey>0 then goto L4350 else ce=curfld
L4290: ce=ce+1: if ce>udim(io7$) then ce=1
L4300: io7$(ce)=rtrm$(uprc$(io7$(ce))) : ce1=pos(io7$(ce),"U",10) : if ce1=0 then goto L4290
	ce2=ce1+1 : io7$(ce)(ce1:ce1)="UC" : goto L4260
CONV7: if ce>0 then io7$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR7: pr f "24,78,C 1": bell : goto L4300
L4350: rewrite #93,using L3450,rec=1: mat dpt
	goto DPAMENU

DPDATES: ! ENTER DATES TO TOTAL
	pr newpage
	close #108: ioerr L4410
L4410: open #108: "SROW=03,SCOL=10,EROW=23,ECOL=73,Border=Sr,CAPTION=<Credit Card Dates to be Totaled",display,outIn
	pr #108: newpage
	for j=1 to 5
		io8$(j)="14,"&str$(j*9+15)&",N 6,UT,N"
		io8$(j+5)="16,"&str$(j*9+15)&",N 6,UT,N"
		io8$(j+10)="18,"&str$(j*9+15)&",N 6,UT,N"
		io8$(j+15)="20,"&str$(j*9+15)&",N 6,UT,N"
		io8$(j+20)="22,"&str$(j*9+15)&",N 6,UT,N"
	next j
	pr f "4,11,c 62": "This is designed to take care of two or more dates on Credit"
	pr f "5,11,c 62": "Card batched totaled together by the credit card company"
	pr f "6,11,c 62": "before they are transmitted to your bank for deposit."
	pr f "08,11,c 62": "this usually occurs on weekends or when a batch is transmitted"
	pr f "09,11,c 62": "after 8pm (Eastern time) one day and before 8pm the next."
	pr f "11,11,C 62": "You may have 5 different entries with up to 5 Dates per entry"
	for j=1 to 5
		pr f str$(j*2+12)&",16,C 50": "Entry "&str$(j)&"        +        +        +        +"
	next j
	pr f "13,24,C 42,N": "         (use only mmddyy format)"
	pr f "24,35,C 09,B,1": "Next (F1)"
L4610: rinput fields mat io8$: mat dpd conv ERR8
	if ce>0 then io8$(ce)(ce1:ce2)="U": ce=0
	if ckey>0 then goto L4700 else ce=curfld
L4640: ce=ce+1: if ce>udim(io8$) then ce=1
L4650: io8$(ce)=rtrm$(uprc$(io8$(ce))) : ce1=pos(io8$(ce),"U",10) : if ce1=0 then goto L4640
	ce2=ce1+1 : io8$(ce)(ce1:ce1)="UC" : goto L4610
CONV8: if ce>0 then io8$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR8: pr f "24,78,C 1": bell : goto L4650
L4700: dpd$=cnvrt$("PIC(######)",stmtdt)
	mo2=val(dpd$(1:2))
	da2=val(dpd$(3:4))
	yr2=val(dpd$(5:6))
	for ce=1 to 25
		j1=int((ce-1)/5)+1
		j2=ce-(j1-1)*5
		if dpd(j1,j2)=0 then goto L4880
		dpd$=cnvrt$("PIC(######)",dpd(j1,j2))
		mo1=val(dpd$(1:2))
		da1=val(dpd$(3:4))
		yr1=val(dpd$(5:6))
		if mo1<01 or mo1>12 then goto ERR8
		if mo1=mo2 and yr1=yr2 then goto L4870 ! SAME MONTH SAME YEAR
		if yr1=yr2 and mo1+1=mo2 then goto L4870 ! PREVIOUS MONTH SAME YEAR
		if yr1+1><yr2 then goto ERR8
		if mo2=1 and mo1=12 then goto L4870 else goto ERR8
L4870: if da1<01 or da1>31 then goto ERR8
L4880: next ce
	ce=0
	rewrite #92,using L3400,rec=1: mat dpd
	goto DPAMENU

COR1: pr newpage
	pr f "10,10,c 50": "Item Number to correct:"
	pr f "12,35,C 10,B,99": "Stop (Esc)"
L4960: input fields "10,50,Nz 4,UT,N": cr1 conv L4960
	if cr1=0 or ckey=5 then goto DPAMENU
	if cr1<1 or cr1>lrec(91) then goto L4960
	read #dpamnts,using L3880,rec=cr1: tr$,am1,ti$
	pr newpage
	pr f "10,10,C 60": "Correct Amount (0 to Delete):"
	rinput fields "10,50,N 10.2,UT,N": am1
	rewrite #dpamnts,using L3880,rec=cr1: tr$,am1,ti$
	goto COR1

L5060: restore #bankrec1,key>=wbc$&"2        ": nokey DPAMENU
	t2=0
L5080: read #bankrec1,using 'Form POS 79,c 12,POS 3,N 1,pos 4,c 8,G 6,pd 10.2,POS 72,N 6': bankgl$,tcde,tr$(1),tr2,tr3,clr eof L5260
	if bankgl$><wbc$ then goto L5260
	if tcde><2 then goto L5260
	if clr><0 then goto L5080
	if fndate_mmddyy_to_ccyymmdd(tr2)>fndate_mmddyy_to_ccyymmdd(stmtdt) then goto L5080
	for r1=1 to lrec(91)
		read #dpamnts,using L3880,rec=r1: tr$,am1,ti$
		if am1=0 then goto L5240
		if rtrm$(tr$)><"" then goto L5240
		if tr3><am1 then goto L5240
		rewrite #dpamnts,using L3880,rec=r1: tr$(1)
		k$=lpad$(rtrm$(bankgl$),2)&str$(tcde)&tr$(1)
		rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: stmtdt
		form pos 72,n 6
		t2+=tr3
		goto L5080
L5240: next r1
	goto L5080
L5260: goto DPAMENU

include: ertn
CLEAR_TRANSACTIONS_FROM_LIST: !
	lastrec=nextrec=total=0
	displayattop$="True"
	if ti3=1 then type$="Checks" else type$="Deposits"
	close #clearing: ioerr L5400
L5400: open #clearing=89: "Name=[Q]\GLmstr\clearing.H"&wsid$&",replace,RecL=43",internal,outIn,relative
	if ti3=2 then 
		restore #bankrec1,key>=wbc$&"2        ": nokey DISPLAY_GRID 
	else
		restore #bankrec1,key>=wbc$&"1        ": nokey DISPLAY_GRID
	end if
L5420: read #bankrec1,using 'Form POS 79,c 12,POS 3,N 1,pos 4,c 8,G 6,pd 10.2,POS 72,N 6': bankgl$,tcde,tr$(1),tr2,tr3,clr eof DISPLAY_GRID
	if bankgl$><wbc$ then goto L5420
	if ti3=1 and tcde><1 then goto L5420 ! only disbursments
	if displaycleared=1 and clr=stmtdt then goto L5480 ! display only cleared on this date
	if displaycleared=1 and clr<>stmtdt then goto L5420 ! if only cleared this clearing date, don't allow others to list
	if clr><0 then goto L5420
L5480: k$=lpad$(rtrm$(bankgl$),12)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
	write #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6': k$,tr2,tr3,clr
	goto L5420
DISPLAY_GRID: !
	mat chdr$(5) : mat cmask$(5) : mat flxitm$(5)
	chdr$(1)="Rec"
	chdr$(2)="Reference #" : chdr$(3)="Date"
	chdr$(4)="Amount" : chdr$(5)="Cleared"
	cmask$(1)='30' : cmask$(2)='' : cmask$(3)='1'
	cmask$(4)='10' : cmask$(5)='1'
L5530: fnTos
	respc=0 : mat resp$=('')
	fnLbl(1,1,trim$(bn$(1:30))&"-"&type$,65,2)
	fnflexinit1('Deposit-1',2,3,15,55,mat chdr$,mat cmask$,1)
	restore #clearing:
	x=lrec(89): if nextrec>0 and x>1 and displayattop$="True" then goto L5580 else goto L5670
L5580: for j=nextrec to lrec(clearing) ! read starting with next record
		read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6',rec=j: flxitm$(2),flxitm$(3),amount,flxitm$(5) noRec L5610
		flxitm$(1)=str$(rec(clearing))
		flxitm$(4)=str$(amount)
		fnflexadd1(mat flxitm$)
L5610: next j
	for j=1 to max(nextrec-1,1) ! read records previously cleared or skipped
		read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6',rec=j: flxitm$(2),flxitm$(3),amount,flxitm$(5) noRec L5650
		flxitm$(1)=str$(rec(clearing))
		flxitm$(4)=str$(amount)
		if val(flxitm$(5))=stmtdt then total+=amount
		fnflexadd1(mat flxitm$)
L5650: next j
	goto L5690
L5670: read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,c 6': flxitm$(2),flxitm$(3),amount,flxitm$(5) eof L5690
	flxitm$(1)=str$(rec(clearing))
	flxitm$(4)=str$(amount)
	if val(flxitm$(5))=stmtdt then total+=amount
	fnflexadd1(mat flxitm$) : goto L5670
L5690: fnLbl(17,30,"Total Cleared:",16,1)
	fnTxt(17,49,12,0,1,"10",0," ")
	resp$(respc+=1)=str$(total)
	if val(flxitm$(5))=stmtdt then total+=amount
	fnChk(18,49,"Display at Top:",1)
	resp$(respc+=1)=displayattop$
	fnCmdKey("&Display Cleared",3,0,0,"Displays all transactions cleared on this clearing date")
	fnCmdKey("&Display Uncleared",2,0,0,"Displays all remaining uncleared transactions")
	if clear_from_range=1 then let fnCmdKey("&Clear by Range",6,1,0,"Enter another range of reference numbers")
	if clear_from_range=0 then let fnCmdKey("&Clear",1,1,0,"Clear the highlited transaction")
	fnCmdKey("C&ancel",5,0,1,"Return to Bank Reconciliation menu")
	ckey=fnAcs(mat resp$)
	displaycleared=total= clear_from_range=0
	if ckey=5 or ckey=99 then goto BANK_STMT_INFO
	displayattop$=resp$(3)
	! do you want next uncleared check at the top of the screen
	if ckey=2 then goto CLEAR_TRANSACTIONS_FROM_LIST ! redisplay on uncleared
	if ckey=3 then displaycleared=1: goto CLEAR_TRANSACTIONS_FROM_LIST 	! displays only cleared on this date
	if ckey=6 then goto CLEAR_BY_RANGE ! return to clearing by range of reference numbers
	read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6',rec=val(resp$(1)): k$,tr2,tr3,clr noRec L5900
! k$(1:12)=LPAD$(RTRM$(K$(1:12)),12)
	if clr=0 then newclr=stmtdt else newclr=0 ! if no previous clearing date, use new one; if it has a date, unclear it
	rewrite #clearing,using 'Form POS 38,n 6',rec=val(resp$(1)): newclr
	read #clearing,using 'Form POS 1,C 21,G 6,pd 10.2,N 6',rec=val(resp$(1)): k$,tr2,tr3,clr noRec L5900
	rewrite #bankrec1,using 'Form POS 72,N 6',key=k$: newclr ! update the transaction history
	lastrec=val(resp$(1)) : if lastrec+1 <= lrec(clearing) then nextrec=lastrec+1 else nextrec=1
L5900: goto L5530

Xit: fnXit

execute "Index [Q]\GLmstr\bankrec.h[cno]"&' '&"[Q]\GLmstr\bankrec-idx.h[cno]" &" 79/3/4 12/1/8 Replace,DupKeys"

def fn_cd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
