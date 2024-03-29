! PR Check History (Transaction) Journals

autoLibrary
on error goto Ertn

fnTop(program$)
dim ti$(3)*20
ti$(1)='Checks'
ti$(2)='Deposits'
ti$(3)='Adjustments'

open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,i
read #20,using 'form pos 152,N 2': wbc
close #20:

MAIN: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'Beginning Date:',38,1)
	fnTxt(1,40,10,0,1,'3',0,'Earliest transation date to be shown on journals!')
	resp$(respc+=1)=''
	fnLbl(2,1,'Ending Date:',38,1)
	fnTxt(2,40,10,0,1,'3',0,'Last transation date to be shown on journals!')
	resp$(respc+=1)=''
	fnLbl(4,1,'Information to Print:',38,1)
	dim item2$(2)*15
	item2$(1)='Details'
	item2$(2)='Totals Only'
	fnComboA('claims-act',4,40,mat item2$)
	resp$(respc+=1)=item2$(1)
	fnChk(7,40,'Print Disbursments Journal:',1)
	resp$(respc+=1)='True'
	fnChk(8,40,'Print Receipts Journal:',1)
	resp$(respc+=1)='True'
	fnChk(9,40,'Print Adjustments Journal:',1)
	resp$(respc+=1)='False'
	fnLbl(11,1,'Bank Account:',38,1)
	fnComboF('Bankmstr',11,40,20,'[Q]\CLmstr\bankmstr.h[cno]',1,2,3,15,'[Q]\CLmstr\Bankidx1.h[cno]',1,0, 'Select bank account for printing')
	resp$(respc+=1)=str$(wbc)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dt1=val(resp$(1)) ! beginning date
	dt2=val(resp$(2)) ! ending date
	td1yn$=resp$(3)(1:1) !  detail
	dim slt(3),sltyn$(3)*1
	if resp$(4)(1:1)='T' then sltyn$(1)='Y' : slt(1)=1 else sltyn$(1)='N' ! disb jrn
	if resp$(5)(1:1)='T' then sltyn$(2)='Y' : slt(2)=1 else sltyn$(2)='N' ! rec jrn
	if resp$(6)(1:1)='T' then sltyn$(3)='Y' : slt(3)=1 else sltyn$(3)='N' ! adj jrn
	wbc=val(resp$(7)(1:2))
! FNWAIT
	open #trmstr=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,i,k
	open #tralloc=2: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr',i,i,k
	open #glmstr=4: 'Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr',i,i,k
	open #work=3: 'Name=[temp]\WORK,KFName=[temp]\ADDR,RecL=40,KPS=1,KLN=12,Replace',i,outIn,k ! this file is used to total Amounts by General Ledger Number
	open #bankmstr=12: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',i,outIn,k
	dim bn$*30
	read #bankmstr,using 'form pos 3,C 30,C 12,PD 6.2',key=cnvrt$('N 2',wbc),release: bn$ nokey MAIN
	close #bankmstr:
	bn$=rtrm$(bn$)
	fnOpenPrn
END1: !
	if wcd=0 or td1yn$='T' then goto HERE
	pr #255,using 'form pos 52,G 12.2': '  __________'
	pr #255,using 'form pos 52,G 12.2': t1(wcd)
	npg=1
HERE: !
	if wcd>2 then goto Finis
	wcd+=1
	if slt(wcd)<>1 then goto HERE
	if npg=1 then pr #255: newpage
	npg=0
	if td1yn$='D' then gosub PrHdr
	restore #trmstr,key>=lpad$(str$(wbc),2)&str$(wcd)&'        ': nokey END1
READ_TRMSTR: !
	dim de$*35
	read #trmstr,using 'form pos 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35': bank_code,tcde,checkNumber$,tr2,amt,vn$,de$ eof Finis
	if tcde=1 then de$=rpad$(ltrm$(vn$),8)&' '&de$(1:26)
	if bank_code><wbc then goto Finis
	if tcde><wcd then goto END1
	if fndate_mmddyy_to_ccyymmdd(tr2)<dt1 or (fndate_mmddyy_to_ccyymmdd(tr2)>dt2 and dt2<>0) then
		goto READ_TRMSTR
	end if
	sq$=' '
	if tcde><1 then goto L750
	ck1=val(checkNumber$) conv L750
	if ck2=0 then goto L740
	if ck2+1><ck1 then sq$='*'
L740: !
	ck2=ck1
L750: !
	if td1yn$='D' then
		pr #255,using 'form pos 1,C 2,C 10,PIC(ZZ/ZZ/ZZBB),C 29,N 12.2': sq$,checkNumber$,tr2,de$(1:29),amt pageoflow NEWPGE
	end if
	dim t1(3)
	t1(wcd)+=amt
RESTORE_TRALLOC: !
	totalalloc=0 ! kj 52307
	key$=cnvrt$('Pic(zz)',bank_code)&str$(tcde)&checkNumber$
	restore #tralloc,key>=key$: nokey PRINT_D_NEWPAGE
READ_TRALLOC: !
	dim tr5$*30
	read #tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,am2,tr5$,ivd$ eof PRINT_D_NEWPAGE
	if newkey$<>key$ then
		goto PRINT_D_NEWPAGE
	else if am2=0 then
		goto READ_TRALLOC
	else
		totalalloc+=am2 ! kj 52307
		if wcd=2 then am2=-am2
		if td1yn$='D' then
			pr #255,using 'form pos 66,C 12,N 11.2,X 2,C 32,c 6': gl$,am2,tr5$,ivd$ pageoflow NEWPGE
		end if
	end if
goto SUMMARY_MAYBE ! /r

PRINT_D_NEWPAGE: ! r:
	if amt<>totalalloc then pr #255,using 'form pos 1,c 80': 'The allocations on the above transaction do not agree with total transaction' ! kj 52307
	if td1yn$='D' then
		pr #255: pageoflow NEWPGE
		! if condition was left off and printed many blank pages if chose
		! totals only 4/04/01
	end if
goto READ_TRMSTR ! /r

NEWPGE: pr #255: newpage: gosub PrHdr : continue

PrHdr: ! r:
	pr #255,using 'form pos 1,C 8,Cc 74': date$,env$('cnam')
	if end3=0 then
		pr #255,using 'form pos 1,C 8,pos 24,CC 40': time$,'Bank # '&str$(wbc)&' '&bn$
		pr #255,using 'form pos 24,Cc 40': ti$(wcd)&' Journal'
	end if
	if end3=1 then
		pr #255,using 'form pos 1,C 8,pos 24,CC 40': time$,'Bank # '&str$(wbc)&' '&bn$
		pr #255,using 'form pos 24,Cc 40': ' General Ledger Recap'
	end if
	pr #255,using 'form pos 1,C 26,C 60': 'Page '&str$(pg+=1),'Date From: '&cnvrt$('PIC(ZZZZ/ZZ/ZZ)',dt1)&' Date To: '&cnvrt$('PIC(ZZZZ/ZZ/ZZ)',dt2)
	if end3=1 then goto EOPrHdr
	pr #255: '                                                                                  Item                                    Invoice '
	if wcd=1 then ref$='   Check # ' else ref$='   Ref #   '
	pr #255: ref$& '   Date    Payee/Description                  Amount   GL Number      Amount   Item Description                  Date  '
	pr #255: '  ________  ________  _______________________________ __________ ____________ __________  _______________________________ ________'
EOPrHdr: return ! /r

Finis: ! r:
	if slt(wcd) then
		if td1yn$='D' then
			pr #255,using 'form pos 52,G 12.2': '  __________'
			pr #255,using 'form pos 52,G 12.2': t1(wcd)
		else if td1yn$='T' then
			end3=1 : gosub PrHdr
			pr #255: ''
			pr #255,using 'form pos 1,C 60': '______________________________________'
			pr #255: ''
		end if
	end if
	for j=1 to 3
		pr #255,using 'form pos 7,C 18,N 12.2': 'Total '&ti$(j),t1(j)
	next j
	if td1yn$='T' then
		pr #255: ''
		pr #255,using 'form pos 1,C 60': '______________________________________'
		pr #255: ''
	end if
	gosub RESTORE_WORK
	fnClosePrn
goto Xit ! /r

Xit: fnXit

SUMMARY_MAYBE: ! r:
	dim glt(3)
	mat glt=(0)
	read #work,using 'form pos 1,C 12,3*PD 6.2',key=gl$: gl$,mat glt nokey READ_WORK_NOKEY
	glt(wcd)+=am2
	rewrite #work,using 'form pos 1,C 12,3*PD 6.2': gl$,mat glt
goto READ_TRALLOC ! /r was restore_tralloc

READ_WORK_NOKEY: ! r:
	glt(wcd)=am2
	write #work,using 'form pos 1,C 12,3*PD 6.2': gl$,mat glt
goto READ_TRALLOC ! /r was RESTORE_TRALLOC (2)

RESTORE_WORK: ! r:
	restore #work,key>='            ': nokey END3
	if td1yn$<>'T' then
		pr #255: newpage
		end3=1 : gosub PrHdr
	end if
	pr #255: '  GL Number   Disbursments    Receipts     Adjustments'
	pr #255: '____________  ____________  ____________  ____________'
	do
		read #work,using 'form pos 1,C 12,3*PD 6.2': gl$,mat glt eof END3
		if gl$(1:3)=hgl$(1:3) or val(hgl$)=0 then goto L1350

		L1330: !
		pr #255,using 'form pos 1,C 60': '____________  ____________  ____________  ____________'
		pr #255,using 'form pos 13,3*N 14.2': mat glts
		pr #255: ''
		mat glts=(0)

		L1350: !
		hgl$=gl$
		dim glts(3)
		mat glts=glts+glt
		if subcode=1 then goto END3B

		dim des$*30
		des$=''
		read #glmstr,using 'form pos 13,C 30',key=gl$: des$ nokey L1390
		L1390: !
		pr #255,using 'form pos 1,C 12,3*N 14.2,X 2,C 30': gl$,mat glt,des$
	loop

	END3: !
	if val(hgl$(1:3))<>0 then
		subcode=1
		goto L1330
	end if

	END3B: !
	pr #255: '____________  ____________  ____________  ____________'
	pr #255,using 'form pos 1,C 12,3*N 14.2,X 2,C 30': '   Totals',mat t1
return ! /r

include: ertn
