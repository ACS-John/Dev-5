! GL Distribution Report (fnPostCheckbookToGl    )
! Post to General Ledger (fnPostCheckbookToGl(1) )

def library fnPostCheckbookToGl(; enablePost,___,pg)
	autoLibrary
	on error goto Ertn

	! enablePost: 0/notpassed=Print Only   else    Post

	if enablePost then
		! cap$='Post to General Ledger'
		stopable$='NO'
	else
		! cap$='GL Distribution Report'
		stopable$='YES'
	end if

	dim ml$(0)*256

	dim optCashOrAccrual$(2)*12
	optCashOrAccrual$(1)='Cash'
	optCashOrAccrual$(2)='Accrual'

! r: determine if cash or accrual by checking for any accounts payable numbers in the general ledger control file
		up1$='C'
		open #hGlControl=fnH: 'Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Shr',i,i,k
		FglControlGw: form pos 52,C 12
		FglControl2: form pos 34,2*c 9,c 12
		do
			read #hGlControl,using FglControlGw: gw$ eof EoFund
			accrual=val(gw$) conv L230
			if accrual>0 then
				up1$='A'
				goto EoFund
			end if
			L230: !
		loop
		EoFund: ! /r
	fnTos
	fnLbl(1,60,'',0,1)
	fnLbl(1,1,'Starting Date:',44,1)
	fnTxt(1,46,10,0,1,'3',0,'Normally this would be the first day of the month.  If you post more often than once a month, it would be the first day of the period you are posting.')
	resp$(1)=''
	fnLbl(2,1,'Ending Date:',44,1)
	fnTxt(2,46,10,0,1,'3',0,'Normally this would be the last day of the month, unless you post more often than once a month!')
	resp$(2)=date$('ccyymmdd') ! ''
	fnChk(4,47,'Include previously posted transactions:',1)
	resp$(3)='False'
	fnLbl(5,1,'Basis for Accounting:',44,1)
	fnComboA('CashOrAccrual',5,46,mat optCashOrAccrual$,'If you record expenses as they are paid, you are on a cash basis.  If you wish to record unpaid invoices (accounts payable) as well as paid expenses, you are on an accrual basis.')
	resp$(4)=optCashOrAccrual$(1)
	fnChk(6,47,'Combine Payroll Entries:',1)
	resp$(5)='True'
	fnChk(7,47,'Print General Ledger Distribution Listing:',1)
	resp$(6)='True'
	fnChk(8,47,'Update After the Fact Payroll records:',1)
	resp$(7)='False'
	fnLbl(10,1,'Post to General Ledger Company Number:',44,1)
	fnTxt(10,46,5,0,1,'30',1,'Only change this default answer if wish to post to a different company than the one you are assigned to.')
	! protected this option on 5/19/2020 - i don't think they should ever change this.  if i am wrong i'll put it back in - john bowman  (to put it back list:   [cno]' ! &str$(gl2)   )
	resp$(8)=env$('cno')
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dt1=val(resp$(1)) ! beginning date
	dt2=val(resp$(2)) ! ending date
	d1=val(resp$(1)(5:6))*10000+val(resp$(1)(7:8))*100+val(resp$(1)(3:4)) ! beginning date
	d2=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! ending date  ! convert dates back to mmddyy
	if resp$(3)(1:1)='T' then include_prev_posted$='Y' else include_prev_posted$='N' ! include previously posted entries
	if resp$(4)(1:1)='C' then up1$='C' else up1$='A' ! cash or accrual
	if resp$(5)(1:1)='T' then enableCombinePrEntries=1 else enableCombinePrEntries=0  ! combine payroll entries
	if resp$(6)(1:1)='T' then enableDistributionListing=1 else enableDistributionListing=0 ! pr distribution listing
	if resp$(7)(1:1)='T' then pr2$='Y' else pr2$='N' ! update after fact payroll
	! gl2=val(resp$(8)) ! GL company to post
	if pr2$='Y' then fnProcess(4)
	if ~enablePost then enableDistributionListing=1
	! gl2=fnPutCno(gl2)
	!   pr f '13,34,C 12,B,99': 'Cancel (Esc)'
	!   on fkey 99 goto Xit
	fnOpenPrn
	open #hTran=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,outIn,k
	open #hAloc=fnH: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\tralloc-idx.h[cno],Shr',i,outIn,k
	open #hBank=fnH: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',i,outIn,k
	open #hWork=fnH: 'Name=[Temp]\WORK.[session],RecL=76,Replace',internal,output
	if ~fn_breakdownsAddUp then goto Xit ! gosub CHECK_BREAKDOWNS
	hGlwk=fn_gLBucketStuff(enablePost,glb,pr2$,glwk2wsid)
	goto ReadTran

	NoAllocations: ! r:
			! pr 'tran rejected due to no allocations'
			! pause
	goto ReadTran ! /r

	ReadTran: ! r: main loop
		dim de$*30
		read #hTran,using 'form pos 1,n 2,n 1,C 8,N 6,PD 10.2,pos 28,C 8,C 30,pos 71,N 1,X 6,N 1': trbank_code,trtcde,checkNumber$,pd,ca1,vn$,de$,pcde,scd eof End1

		! pr 'Read a Transaction: '&checkNumber$&' - '&date$(days(pd,'mmddyy'),'mm/dd/ccyy'),ca1

		if scd=4 and fndate_mmddyy_to_ccyymmdd(pd)>dt2 then
			! pr 'tran rejected from post - cause AA' ! pause
			goto ReadTran
		else if fndate_mmddyy_to_ccyymmdd(pd)<dt1 then
			! pr 'tran rejected from post - cause BB' ! pause
			goto ReadTran
		end if

		restore #hAloc,key>=cnvrt$('pic(zz)',trbank_code)&cnvrt$('pic(#)',trtcde)&checkNumber$: nokey NoAllocations
		! pr 'tran passed test AA and BB and has Allocations' : pause
		ReadNextAlloc: !
		read #hAloc,using 'form pos 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,C 6,pos 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd$,gde eof ReadTran
		ivd=val(ivd$) conv ignore
		if up1$='C' and gde=2 then gde=1 ! don't allow old accrual codes mess up cash basis
		if ivd=0 then ivd=pd ! kJ   10/02/06   skipping receipts w/o invoice numbers

		if bank_code<>trbank_code or trtcde<>tcde or checkNumber$<>trck$ then
			goto RewritePcde ! thru, allocation doesn/t belong to this transaction
		else if amt=0 then
			goto ReadNextAlloc ! skip 0
		end if

		if scd=4 then gosub PrdBld
		if scd=4 or fndate_mmddyy_to_ccyymmdd(ivd)>fndate_mmddyy_to_ccyymmdd(pd) then ivd=pd
		! gde=1  never in ap - entered and paid same month
		! gde=2  posted to ap from unpaid file
		! gde=3  previously posted to ap  - now posted from check history
		if include_prev_posted$='N' and (gde=1 or gde=3) then
			goto ReadNextAlloc
		end if

		if scd=4 then
			if fndate_mmddyy_to_ccyymmdd(ivd)>=dt1 and fndate_mmddyy_to_ccyymmdd(ivd)<=dt2 then
				goto WriteWork
			else
				goto ReadNextAlloc
			end if
		end if

		if ivd<=0 then
			if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then
				goto ReadNextAlloc
			else
				ivd=pd
				goto WriteWork ! Manual Check
			end if
		end if

		if up1$='C' then ivd=pd

		if fndate_mmddyy_to_ccyymmdd(ivd)>dt2 then
			goto ReadNextAlloc
		else if up1$='A' and gde=2 and fndate_mmddyy_to_ccyymmdd(ivd)<dt1 then
			goto L900
		else if up1$='A' and include_prev_posted$='Y' and gde=3 and fndate_mmddyy_to_ccyymmdd(ivd)<dt1 then
			goto L900
		else if up1$='C' or gde<2 then
			goto L920
		end if

		L900: !
			if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto L920
			gosub ReverseApEntries
		goto AfterWriteWork

		L920: !
		if include_prev_posted$='Y' and gde=1 and fndate_mmddyy_to_ccyymmdd(pd)<=dt2 then
			goto WriteWork
		else if gde=0 and fndate_mmddyy_to_ccyymmdd(pd)<=dt2 then
			goto WriteWork
		else if fndate_mmddyy_to_ccyymmdd(ivd)<dt1 or fndate_mmddyy_to_ccyymmdd(ivd)>dt2 then
			goto ReadNextAlloc ! DONT POST ANY INVOICE AS EXPENSE IF OUTSIDE POSTING DATE
		else if include_prev_posted$='N' and gde=2 then
			goto ReadNextAlloc
		end if

		if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then
			write #hWork,using 'form pos 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,ltrm$(iv$)(1:8),vn$,de$,amt, 0, 4, 0
		else
			write #hWork,using 'form pos 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,checkNumber$,vn$,de$,amt,0,tcde,scd
		end if
		gde=2
	goto RewriteGde

	RewriteGde: ! r:
		if enablePost then
			rewrite #hAloc,using 'form pos 80,N 1': gde
		end if
	goto ReadNextAlloc ! /r

	WriteWork: ! r:
		! pr 'Writting a Transaction: '&checkNumber$&' - '&date$(days(ivd,'mmddyy'),'mm/dd/ccyy'),amt
		write #hWork,using 'form pos 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,ivd,checkNumber$,vn$,de$,amt,bank_code,tcde,scd
	AfterWriteWork: !
		if pcde=0 or pcde=2 then pcde+=1
		if gde=0 or gde=2 then gde+=1
	goto RewriteGde ! /r

	RewritePcde: !
		if enablePost then
			rewrite #hTran,using 'form pos 71,N 1': pcde
		end if
		if scd=4 then gosub PrdWrite
	goto ReadTran ! /r

	End2: ! r:
		close #hTran:
		close #hAloc:

		! pr 'entries in work: '&str$(lrec(hWork)) : pause

		if lrec(hWork)=0 then goto EndAll

		close #hWork:
		open #hControl=fnH: 'Name=[Temp]\Control.[wsid],RecL=128,Replace',internal,output
		write #hControl,using Fsort: '! SORT FOR '&env$('program_caption')&' LIST IN PROCESS'
		write #hControl,using Fsort: 'FILE [Temp]\WORK.[Session],,,[Temp]\Addr.[Session],,,,,A,N'
		write #hControl,using Fsort: 'MASK 1,26,C,A'
		Fsort: form pos 1,c 128
		close #hControl:
		fnFree('[Temp]\Addr.[session]')
		execute 'Sort [Temp]\Control.[wsid] -n'
		open #hAddr=fnH: 'Name=[Temp]\Addr.[session]',i,i ioerr EndAll
		open #hWork=fnH: 'Name=[Temp]\WORK.[session]',i,i,r
		if enableDistributionListing then
			if ~pg then gosub Hdr ! f1
			pr #255: '____________  ________  ________  ________  Regular GL Postings___________  __________  __________' pageoflow NewPge
		end if
		
		ReadWork: !
		
		read #hAddr,using 'form pos 1,PD 3': r5 eof EndAll
		read #hWork,using 'form pos 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1',rec=r5: gl$,ivd,checkNumber$,vn$,de$,amt,bank_code,tcde,scd noRec ReadWork
		if amt=0 then goto ReadWork

		if gl$(1:3)='  0' then gl$(1:3)='   '
		if gl$(10:12)='  0' then gl$(10:12)='   '
		if hgl$=gl$ then goto L1410
		if enableCombinePrEntries and sc2=4 then 
			gosub PRGL
			if enableDistributionListing then
				dim pde$*30
				pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd,'  ','  ',pde$,pa1,pa2 pageoflow NewPge
			end if
			pa1=pa2=sc2=0
		end if
		
		if tc1 or tc2 then
			if enableDistributionListing then
				pr #255: '                                            ______________________________  __________  __________' pageoflow NewPge
				pr #255,using 'form pos 45,C 30,2*N 12.2': 'GL   '&hgl$&' TOTAL',tc1,tc2
				pr #255: '                                            ______________________________  __________  __________' pageoflow NewPge
			end if
			tc1=tc2=0
		end if
		L1410: !
		p1=75 : cl=1
		if tcde=1 and amt<0 then p1=87 : cl=2
		if tcde=2 and amt>0 then p1=87 : cl=2
		if tcde=3 and amt>0 then p1=87 : cl=2
		if tcde=4 and amt<0 then p1=87 : cl=2
		if ~enableCombinePrEntries then goto L1500
		gosub CombinePR
		if scd<>4 then
			L1500: !
			gosub REGGL
			if enableDistributionListing then
				pr #255,using L1530: gl$,ivd,checkNumber$,vn$,de$,abs(amt) pageoflow NewPge
				L1530: form pos 1,c 14,pic(zz/zz/zz),x 2,2*c 10,c 30,pos p1,g 12.2,skip 1
			end if
		end if
		if cl=2 then
			tc2=tc2+abs(amt)
			gc2=gc2+abs(amt)
		else
			tc1=tc1+abs(amt)
			gc1=gc1+abs(amt)
		end if
		if tcde=3 then
			p1=75
			if enableDistributionListing then
				pr #255,using L1530: bgl$,ivd,checkNumber$,vn$,de$,abs(amt) pageoflow NewPge
			end if
			if cl<>2 then
				tc2=tc2+abs(amt)
				gc2=gc2+abs(amt)
			else
				tc1=tc1+abs(amt)
				gc1=gc1+abs(amt)
			end if
		end if
		if bank_code then
			dim tbc(99,2)
			if tcde=1 then tbc(bank_code,1)=tbc(bank_code,1)+amt
			if tcde=2 then tbc(bank_code,2)=tbc(bank_code,2)+amt
			! IF TCDE=3 AND AMT>0 THEN tBC(Bank_Code,2)=TBC(Bank_Code,2)+AMT
			! IF TCDE=3 AND AMT<0 THEN tBC(Bank_Code,1)=TBC(Bank_Code,1)+ABS(AMT)
			if tcde=4 then tbc(bank_code,1)=tbc(bank_code,1)+amt
		else
			ap1=val(gl$(1:3))
			if ap1=0 then
				j=99
			else
				dim apc(99,3)
				for j=1 to 98
					if apc(j,1)=0 then goto L1720
					if apc(j,1)=ap1 then goto L1720
				next j
			end if
			L1720: !
			apc(j,1)=ap1
			apc(j,2)=apc(j,2)+(amt)
		end if
		hgl$=gl$

	goto ReadWork ! /r

	NewPge: ! r:
		pr #255: newpage
		gosub Hdr
	continue  ! /r
	Hdr: ! r:
		! f1=1
		pr #255,using 'form pos 1,c 8,cc 76': date$,env$('cnam')
		pr #255,using 'form pos 1,c 8,cc 76': time$,'General Ledger Distribution Listing'
		pr #255,using 'form pos 1,c 4,n 4,cc 76': 'Page',pg+=1,'From: '&cnvrt$('PIC(zz/ZZ/ZZ)',d1)&'   To: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2)
		pr #255: ''
		pr #255: '                        Check/Ref                                          '
		pr #255: '  GL Number     Date     Number    Vendor   Description                       Debits      Credits'
		pr #255: '____________  ________  ________  ________  ______________________________  __________  __________' pageoflow NewPge
	return  ! /r

	EndAll: ! r:
		EndAll=1 : enableDistributionListing=1  ! pr TOTALS
		if sc2=4 then gosub PRGL
		if enableDistributionListing then
			if sc2=4 then
				pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd,' ',' ',pde$,pa1,pa2 pageoflow NewPge
			end if
			pr #255: '                                            ______________________________  __________  __________' pageoflow NewPge
			pr #255,using 'form pos 45,C 30,2*N 12.2': 'GL # '&hgl$&' Total',tc1,tc2
			pr #255: '                                            ______________________________  __________  __________'
		end if
		for j=1 to 99
			gl$=''
			if tbc(j,1)=0 and tbc(j,2)=0 then goto L2130
			read #hBank,using 'form pos 33,C 12', key=lpad$(str$(j),2): gl$ nokey L2100
			if gl$(1:3)='  0' then gl$(1:3)='   '
			if gl$(10:12)='  0' then gl$(10:12)='   '
			L2100: !
			gosub BankGL
			if enableDistributionListing then
				pr #255,using 'form pos 45,C 30,2*N 12.2': 'Bank   '&gl$,tbc(j,2),tbc(j,1) pageoflow NewPge
			end if
			gc1=gc1+tbc(j,2): gc2=gc2+tbc(j,1)
			L2130: !
		next j
		if enableDistributionListing then
			pr #255: '                                            ______________________________  __________  __________' pageoflow NewPge
		end if
		for j=1 to 99
			gl$=''
			if apc(j,2)=0 and apc(j,3)=0 then goto L2240
			read #hGlControl,using FglControlGw,key=lpad$(str$(apc(j,1)),3): gl$ nokey L2210
			if gl$(1:3)='  0' then gl$(1:3)='   '
			if gl$(10:12)='  0' then gl$(10:12)='   '
			L2210: !
			gosub APGL
			if enableDistributionListing then
				pr #255,using 'form pos 45,C 30,2*N 12.2': 'A/P    '&gl$,apc(j,3),apc(j,2) pageoflow NewPge
			end if
			gc1=gc1+apc(j,3)
			gc2=gc2+apc(j,2)
			L2240: !
		next j
		if ~enableDistributionListing then goto L2300
		pr #255: '                                            ______________________________  __________  __________' pageoflow NewPge
		pr #255,using 'form pos 45,C 30,2*N 12.2': 'Final Total',gc1,gc2 pageoflow NewPge
		pr #255: '                                            ======================================================' pageoflow NewPge
		fnClosePrn
		L2300: !

		! pr 'at the end' : pause

		if ~enablePost then goto Xit
		if glb=2 then
			goto Xit
		else
			! gl2=fnPutCno(gl2)
			fnChain('S:\General Ledger\Merge')
		end if
	! /r
	End1: ! r:
		if scd=4 and pa1+pa2<>0 then gosub CombinePR
		if up1$='C' then goto End2
		open #paytrans=6: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,outIn,k
		open #unpdaloc=7: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr',i,outIn,k
		open #paymstr=8: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,i,k
		READ_PAYTRANS: !
		read #paytrans,using 'form pos 1,C 8,C 12,N 6,pos 45,C 18,pos 96,N 1,N 6': vn$,iv$,dd,de$,pcde,pdte eof L2610
		if include_prev_posted$='Y' then goto L2450
		if pcde=2 then goto READ_PAYTRANS
		L2450: !
		if include_prev_posted><1 then goto L2500
		if pdte=0 then goto L2500
		if fndate_mmddyy_to_ccyymmdd(pdte)<dt1 then goto READ_PAYTRANS
		if fndate_mmddyy_to_ccyymmdd(pdte)>dt2 then goto READ_PAYTRANS
		goto L2510
		L2500: !
		if fndate_mmddyy_to_ccyymmdd(dd)>dt2 then goto READ_PAYTRANS
		L2510: !
		read #paymstr,using 'form pos 9,C 30',key=vn$: de$ nokey L2520
		L2520: !
		restore #unpdaloc,key>=vn$&iv$: nokey READ_PAYTRANS
		L2540: !
		read #unpdaloc,using 'form pos 1,c 8,c 12,C 12,PD 5.2': trvn$,triv$,gl$,amt eof READ_PAYTRANS
		if vn$<>trvn$ or iv$<>triv$ then goto READ_PAYTRANS
		if amt=0 then goto L2580
		write #hWork,using 'form pos 1,C 12,N 6,2*C 8,C 30,PD 5.2,N 2,2*N 1': gl$,dd,ltrm$(iv$)(1:8),vn$,de$,amt,0,4,0
		L2580: !
		if enablePost then
			rewrite #paytrans,using 'form pos 96,N 1,N 6': 2,d2 ioerr L2590
		end if
		L2590: !
		goto L2540
		L2610: !
		close #paytrans:
		close #unpdaloc:
		close #paymstr:
	goto End2 ! /r
	CombinePR: ! r: (scd,pgl$,gl$,sc2,
		if scd><4 then goto L2750
		if pgl$=gl$ and pivd=ivd then 
			goto L2730
		else
			if sc2=4 then
				gosub PRGL
				if enableDistributionListing then
					pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,2*C 10,C 30,2*N 12.2': pgl$,pivd,'  ','  ',pde$,pa1,pa2 pageoflow NewPge
				end if
				L2720: !
			end if
			pa1=pa2=0
		end if
		L2730: !
		sc2=scd : pgl$=gl$ : pivd=ivd : pde$='Payroll Total' : pbank_code=bank_code
		if amt<0 then pa2+=abs(amt) else pa1+=amt
		L2750: !
	return  ! /r
	

	REGGL: ! r:
		gw$=gl$ : wbank_code=bank_code
		REGGL2: !
		tr4=ivd : tr5=amt : tr6=tcde
		if tr6=2 or tr6=3 then tr5=-tr5
		if scd=4 then tr6=1
		dim td$*30
		tr$=checkNumber$ : td$=de$ : ven$='' ! VN$
		if tr6=3 then
			gosub Pc2GlFinis
			tr5=-tr5
			gw$=bgl$
			wbank_code=bank_code
		end if
	goto Pc2GlFinis ! /r
	PRGL: ! r: (&gw$,pgl$,&tr4,pivd,x)
		gw$=pgl$ : tr4=pivd : x=pivd : pivd=x
		tr$='PR '&cnvrt$('PIC(ZZ/ZZ/ZZ)',pivd) : td$=pde$ : ven$='' : wbank_code=pbank_code
		if pa1<>0 then
			tr5=pa1
			tr6=1
			gosub Pc2GlFinis
		end if
		if pa2=0 then
			goto Areturn
		else
			tr5=-pa2 : tr6=1 : ven$=''
			goto Pc2GlFinis
		end if
	! /r
	BankGL: ! r:
		gw$=gl$ : wbank_code=bank_code : tr4=d2 : x=d2 : d2=x
		tr$='BK '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2)
		td$='Bank #:'&str$(j)&' Total' : ven$=''
		if tbc(j,1)<>0 then
			tr5=-tbc(j,1)
			tr6=1
			gosub Pc2GlFinis
		end if
		if tbc(j,2)=0 then
			goto Areturn
		else
			tr5=tbc(j,2)
			tr6=2
			goto Pc2GlFinis
		end if
	! /r
	APGL: ! r:
		gw$=gl$ : wbank_code=bank_code : tr4=d2 : x=d2 : d2=x
		tr$='AP '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2) : td$='AP Total'
		ven$=''
		if apc(j,2)<>0 then
			tr5=-apc(j,2)
			tr6=4
			gosub Pc2GlFinis
		end if
	goto Areturn ! /r
	Pc2GlFinis: ! r:
		if tcde=4 and rtrm$(gw$(1:3))<>'' then
			read #hGlControl,using FglControlGw,key=gw$(1:3): bgl$ nokey P2gfReadBank
		else
			P2gfReadBank: !
			read #hBank,using 'form pos 33,C 12', key=lpad$(str$(wbank_code),2): bgl$ nokey ignore
		end if
		if enablePost then
			! fn_checkGlnumber('bank Pc2GlFinis ',gw$)
			write #hGlwk,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gw$,tr4,tr5,tr6,0,tr$,td$,'','','','',bgl$
		end if
		gosub CreateFundTransfers
	goto Areturn ! /r
	Areturn: !
	return  !
	Xit: !
fnend
	def fn_gLBucketStuff(enablePost,&glb,pr2$,&glwk2wsid; ___,hGlBucket,glwk$*256,returnN)
		if enablePost then
			open #hGlBucket=fnH: 'Name=[Q]\GLmstr\GLBucket.h[cno]',i,i,r ioerr ignore
			read #hGlBucket,using 'form pos 1,N 1',rec=1: glb ioerr ignore 
			close #hGlBucket: ioerr ignore
			if glb=2 then
				glwk$='[Q]\GLmstr\GL'&cnvrt$('PIC(######)',d2)&'.h[cno]'
				open #returnN=fnH: 'Name='&glwk$&',RecL=104,Use',internal,output
			else
				glwk$='[Q]\GLmstr\GL_Work_[acsUserId].h[cno]'
				open #returnN=fnH: 'Name='&glwk$&',RecL=104,Replace',internal,output
			end if
		end if
		if pr2$<>'N' then
			open #glwk2wsid=fnH: 'Name=[Q]\GLmstr\GLWK2[acsUserId].h[cno],RecL=110,Replace',internal,output ! [cno]' ! &str$(gl2)
		end if
		fn_gLBucketStuff=returnN
	fnend
ReverseApEntries: ! r: Reverse AP Entries
	if fndate_mmddyy_to_ccyymmdd(pd)>dt2 then goto L3470
	ap1=val(gl$(1:3))
	if ap1=0 then
		j=99
		goto L3300
	end if
	for j=1 to 98
		if apc(j,1)=0 or apc(j,1)=ap1 then goto L3300
	next j
	L3300: !
	apc(j,1)=ap1
	apc(j,3)+=amt
	p1=75 : gw$=''
	read #hGlControl,using FglControlGw,key=lpad$(str$(ap1),3): gw$ nokey L3350
	goto L3360
	L3350: !
	read #hGlControl,using FglControlGw,key=lpad$(str$(0),3): gw$ nokey L3360
	L3360: !
	gosub REGGL2
	tbc(bank_code,1)=tbc(bank_code,1)+amt
	if ~enableDistributionListing then goto L3470
	if ~pg then gosub Hdr
	if ap2=0 then
		pr #255: '____________  ________  ________  Reduce Accounts Payable for Previously Posted Invoices  ________'
		ap2=1
	end if
	p1=75 : gw$=''
	read #hGlControl,using FglControlGw,key=lpad$(str$(ap1),3): gw$ nokey L3430
	L3430: !
	pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 12,C 8,C 30,pos P1,N 12.2': gw$,pd,checkNumber$,'','Reverse AP',amt pageoflow NewPge
	p1=87: gw$=''
	read #hBank,using 'form pos 33,C 12', key=lpad$(str$(bank_code),2): gw$ nokey L3460
	L3460: !
	pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 12,C 8,C 30,pos P1,N 12.2': gw$,pd,checkNumber$,'','Take Out of Bank',amt pageoflow NewPge
	L3470: !
return  ! /r
PrdBld: ! r:
	dim prd(23)
	if pr2$<>'N' then
		tr5=amt
		if ivd=1 then prd(4)+=tr5
		if ivd>1 and ivd<5 then prd(ivd+3)=-tr5
		if ivd=15 then prd(8)=-tr5
		if ivd>4 and ivd<15 and dedcode(ivd-4)=2 then tr5=-tr5
		if ivd>4 and ivd<15 then prd(ivd+4)=-tr5
		if ivd=16 then prd(19)=-tr5
		if fp(ivd*.01)=.19 then prd(20)=int(ivd*.01)
	end if
return  ! /r
PrdWrite: ! r:
	if pr2$='N' then goto L3850
	prd(1)=val(vn$) conv L3850
	prd(2)=pd
	prd(3)=val(checkNumber$) conv ignore
	prd(22)=ca1
	write #glwk2wsid,using 'form pos 1,N 4,2*PD 4,19*PD 5.2,PD 3': mat prd
	L3850: !
	mat prd=(0)
return  ! /r
CreateFundTransfers: ! r:
	if EndAll=1 then goto EoFundTr
	if uprc$(gw$(1:3))=uprc$(bgl$(1:3)) then goto EoFundTr
	if val(gw$(1:3))=0 then goto EoFundTr
	td$='Fund Transfer'
	gl1$=gl2$=gl3$='  0     0  0'
	read #hGlControl,using FglControl2,key=gw$(1:3): gl1$,gl2$,gl3$ nokey EoFundTr
	gl1$=lpad$(rtrm$(gl1$),9)
	gl2$=lpad$(rtrm$(gl2$),9)
	if val(gl1$(1:3))=0 and val(gl1$(4:9))=0 and val(gl1$(10:12))=0 then
		goto EoFundTr ! no interfund entries if no gl # in i/f file
	else if val(gl2$(1:3))=0 and val(gl2$(4:9))=0 and val(gl2$(10:12))=0 then
		goto EoFundTr ! no interfund entries if no gl # in i/f file
	end if
	read #hGlControl,using FglControlGw,key=bgl$(1:3): bankgl3$ nokey EoFundTr
	if gde>1 and gl3$=bankgl3$ then goto EoFundTr ! skip as check if previously posted interfund transfers in the Unpaid Invoice File (will post in unpaid file if AP numbers same in fund file
	if enablePost then
		! fn_checkGlnumber('gw&gl1',gw$(1:3)&gl1$)
		write #hGlwk,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gw$(1:3)&gl1$,tr4,-tr5,tr6,0,tr$,td$,'','','','',bgl$
		! fn_checkGlnumber('bgl&gl2',bgl$(1:3)&gl2$)
		write #hGlwk,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$(1:3)&gl2$,tr4,tr5,tr6,0,tr$,td$,'','','','',bgl$
	end if
	if enableDistributionListing then
		pr #255,using 'form pos 1,X 14,C 60': 'Transferred from '&gw$(1:3)&gl1$&' to '&bgl$(1:3)&gl2$&cnvrt$('N 10.2',tr5) pageoflow NewPge
	end if
	EoFundTr: !
return  ! /r

def fn_breakdownsAddUp(; ___,returnN)
	returnN=1
	if ~fn_cb_trmstr_test then returnN=0
	if ~fn_cb_unpaid_test then returnN=0
	fn_breakdownsAddUp=returnN
fnend  !
! r: functions subordionate to fn_breakdownsAddUp
	def fn_cb_unpaid_test ! CHECK_UNPAIDS: !
		cb_cu_return=1
		restore #hTran:
		open #paymstr=8: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,i,k
		open #unpdaloc=7: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr',i,outIn,k
		open #paytrans=6: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,outIn,k
		CB_CU_READ: !
		read #paytrans,using 'form pos 1,C 8,C 12,N 6,pos 45,C 18,pos 96,N 1,N 6,pos 63,g 10.2': vn$,iv$,dd,de$,pcde,pdte,upa eof EoPaytransTest
		invalloc=0
		if include_prev_posted$='Y' then goto L4440
		if pcde=2 then goto CB_CU_READ
		L4440: ! If include_prev_posted><1 Then Goto 4490
		! If PDTE=0 Then Goto 4400
		if fndate_mmddyy_to_ccyymmdd(dd)<dt1 then goto CB_CU_READ
		if fndate_mmddyy_to_ccyymmdd(dd)>dt2 then goto CB_CU_READ
		read #paymstr,using 'form pos 9,C 30',key=vn$: de$ nokey CB_CU_READ
		restore #unpdaloc,key>=vn$&iv$: nokey CB_CU_READ
		do
			read #unpdaloc,using 'form pos 1,c 8,c 12,C 12,PD 5.2': trvn$,triv$,gl$,amt eof CB_CU_FINIS
			if vn$<>trvn$ or iv$<>triv$ then goto CB_CU_FINIS
			invalloc+=amt
		loop
		CB_CU_FINIS: !
		if upa<>invalloc then
			mat ml$(3)
			ml$(1)='The allocations ('&trim$(cnvrt$('pic(---,---.##)',invalloc))&') does not match the total'
			ml$(2)='transaction amount ('&trim$(cnvrt$('pic(---,---.##)',upa))&').  You must fix this unpaid '
			ml$(3)='invoice # '&trim$(x$)&' in the unpaid invoice file before you can continue. '
			fnMsgBox(mat ml$,ok$,'',48)
			cb_cu_return=0
			goto EoPaytransTest
		else
			goto CB_CU_READ
		end if
		EoPaytransTest: !
		fn_cb_unpaid_test=cb_cu_return
		close #paymstr:
		close #paytrans:
		close #unpdaloc:
	fnend
	def fn_cb_trmstr_test ! TEST_CHECKHISTORY: !
		cb_tt_return=1
		do
			totalloc=0
			CB_TT_READ: !
			read #hTran,using 'form pos 1,n 2,n 1,C 8,N 6,PD 10.2,pos 28,C 8,C 30,pos 71,N 1,X 6,N 1': trbank_code,trtcde,checkNumber$,pd,ca1,vn$,de$,pcde,scd eof EO_TRMSTR_TEST
			if scd=4 and fndate_mmddyy_to_ccyymmdd(pd)>dt2 then
				goto CB_TT_READ
			else if fndate_mmddyy_to_ccyymmdd(pd)<dt1 then
				goto CB_TT_READ
			end if
			restore #hAloc,key>=cnvrt$('pic(zz)',trbank_code)&cnvrt$('pic(#)',trtcde)&checkNumber$: nokey CB_TT_READ
			do
				read #hAloc,using 'form pos 1,N 2,N 1,c 8,C 12,PD 5.2,C 12,X 18,C 6,pos 80,N 1': bank_code,tcde,trck$,gl$,amt,iv$,ivd$,gde eof CB_TT_FINIS ! eof EO_TRMSTR_TEST
				ivd=val(ivd$) conv ignore ! ivd$ logic added 8/12/2015 to prevent merriam wood's error here from using characters in this field
				if up1$='C' and gde=2 then gde=1 ! don't allow old accrual codes mess up cash basis
				if ivd=0 then ivd=pd ! kJ   10/02/06   skipping receipts w/o invoice numbers
				if bank_code<>trbank_code or trtcde<>tcde or checkNumber$<>trck$ then goto CB_TT_FINIS ! thru, allocation doesn/t belong to this transaction
				totalloc+=amt
			loop
			CB_TT_FINIS: !

			if ca1<>totalloc then
				mat ml$(3)
				ml$(1)='The allocations ('&cnvrt$('pic(---,---.##)',totalloc)&' does not match the total'
				ml$(2)='transaction amount ('&cnvrt$('pic(---,---.##)',ca1)&'.  You must fix this '
				ml$(3)='transaction ('&checkNumber$&') (bank '&str$(trbank_code)&') check history before you can continue. '
				fnMsgBox(mat ml$,ok$,'',48)
				cb_tt_return=0
				goto EO_TRMSTR_TEST
			end if
		loop
		EO_TRMSTR_TEST: !
		fn_cb_trmstr_test=cb_tt_return
	fnend
! /r
! def fn_checkGlnumber(source$*128,gln$*12) r:
! 	pr gln$
! 	if gln$(1:9)<>fnCleanGl$(gln$)(1:9) then 
! 	 pr source$&' gln$='&gln$&' changed to fnCleanGl$(gln$)='&fnCleanGl$(gln$)
! 	! pause
! 	end if
! fnend /r
include: ertn stopable$

