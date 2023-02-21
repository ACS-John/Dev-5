autoLibrary
on error goto Ertn
fnTop(program$)

gosub ASK_TI1
goto ASK_PP1

ASK_SORT: ! r:
	if fund=1 then ty1$='Vendor Sequence' else ty1$='Fund Sequence'
	if coded=1 then ty2$='All Invoices' else ty2$='Selected Invoices'
	open #paytrans=4: 'Name=[Q]\CLmstr\PayTrans.h[cno],Shr',i,i,r
	execute 'Index [Q]\CLmstr\unpdaloc.h[cno] [Q]\CLmstr\Uaidx2.h[cno] 1 20 Replace DupKeys -n' ! index in vendor, reference order
	open #unpdaloc=8: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr',i,i,k
	ReadPayTrans: !
	dim de$*50
	read #paytrans,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,g 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof L420
	if coded=2 and cde=0 then goto ReadPayTrans
	if fndate_mmddyy_to_ccyymmdd(dd)>fndate_mmddyy_to_ccyymmdd(d2) then goto ReadPayTrans
	dim ti1$*1
	if ti1$<>'C' and (fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1) then
		goto ReadPayTrans ! Purchases Only
	end if
	ivnum+=1 ! Unique Number for each Invoice
	restore #unpdaloc,key>=vn$&'            ':
	READ_UNPDALOC: !
	dim gl(3)
	dim ade$*50
	read #unpdaloc,using 'form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': alvn$,aliv$,mat gl,amt,ade$ noRec ReadPayTrans eof ReadPayTrans
	if alvn$<>vn$ then goto ReadPayTrans
	if trim$(aliv$)<>trim$(iv$) then goto READ_UNPDALOC
	if fund=2 then de$=ade$(1:18)
	if sum(gl)=0 and amt=0 then goto L410 ! don't write zero records
	write #clwork,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum
	L410: !
	goto READ_UNPDALOC
	L420: !
	close #paytrans: ioerr ignore
	close #unpdaloc: ioerr ignore
	close #clwork: ioerr ignore
	upa=0 ! this sort is ok. it sorts a temporary work file. leave in
	open #tmp=9: 'Name=[Temp]\Control,Size=0,RecL=128,Replace',internal,output
	write #tmp,using 'form pos 1,C 128': 'File CLWork[acsuserid].h[cno],[Q]\CLmstr,,[Temp]\Addr,,,,,A,N'
	if fund=2 then
		write #tmp,using 'form pos 1,C 128': 'Mask 74,12,N,A' ! 'Mask 74,3,N,A,1,20,C,A,86,4,N,A'
	else
		write #tmp,using 'form pos 1,C 128': 'Mask 1,20,C,A,86,4,N,A'
	end if
	close #tmp:
	execute 'Free [Temp]\Addr -n' ioerr ignore
	execute 'Sort [Temp]\Control -n'
	open #addr:=9: 'Name=[Temp]\Addr',i,i
	open #paymstr:=13: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,outIn,k
	open #rpmstr:=23: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k ioerr L550
	prcode=1
	L550: !
	open #clwork:=fnH: 'Name=[Q]\CLmstr\CLWork[acsuserid].h[cno],Shr',i,i,r
	open #glmstr:=fnH: 'Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr',i,outIn,k
	open #work:=fnH: 'Name=[Temp]\Work,Size=0,RecL=22,Replace',internal,output
	close #work:
	execute 'Free [Temp]\Indx -n' ioerr ignore
	execute 'Index [Temp]\Work,[Temp]\Indx,1,12,Replace,DupKeys -n'
	open #work=fnH: 'Name=[Temp]\Work,KFName=[Temp]\Indx',i,outIn,k
	open #hFund=fnH: 'Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Shr',i,i,k
	notused=1: open #11: 'Name=[Q]\CLmstr\DptMstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno]',i,i,k ioerr L640 : notused=0
	L640: !
	fnOpenPrn
	vn$='': iv$=''
	do
		read #addr,using 'form pos 1,PD 3': r4 eof END1
		if r4 then 
			read #clwork,using 'form pos 86,N 4',rec=r4: ivnum
			if hivnum=0 then upa=0 : goto L700 else goto L760
			L700: !
			read #clwork,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum
			upa=upa+amt
			hivnum=ivnum
			gl$=cnvrt$('PIC(ZZ#)',gl(1))&cnvrt$('PIC(ZZZZZZ)',gl(2))&cnvrt$('PIC(ZZ#)',gl(3))
			gosub L1050 ! ACCUMULATE G/L TOTALS
		end if
	loop
	L760: !
	if vn$=hvn$ then goto L810
	if fund=2 then goto L810 ! no vendor totals in fund sequence
	if vc<2 then goto L800
	pr #255: tab(87);'__________'
	pr #255,using L3070: 'Vendor Total',v3
	L800: !
	vc=v1=v2=v3=0
	L810: ! pr #255: PAGEOFLOW NEWPGE
	vc=vc+1
	hvn$=vn$
	if nofx=0 then gosub HDR
	! If GL$(3:3)<>'0' Then Pause
	if pos1=0 and pos2=0 then goto L880
	if notused =0 and dpt$><gl$(1:3)&cnvrt$('pic(zz)',val(gl$(pos1:pos2))) then gosub TotalDepartment
	L880: !
	if fund=2 then
		if fund$><gl$(1:3) then gosub TotalFund : pr #255: newpage : gosub HDR
		f2=(val(gl$(1:3)))
		if f2=0 then f2=1000
		dim ft2(1000)
		ft2(f2)=ft2(f2)+upa
	end if
	dim ft(3)
	dim dp(3)
	if cde=1 then
		p1= 97 : v1+=upa : t1+=upa : ft(1)+=upa: dp(1)+=upa
	else
		p1=109 : v2+=upa : t2+=upa : ft(2)+=upa : dp(2)+=upa
	end if
	v3=v3+upa
	t3=t3+upa : if rtrm$(ltrm$(iv$))='Received' or rtrm$(ltrm$(iv$))='Adjustment' then goto L970 else ft(3)=ft(3)+upa : dp(3)=dp(3)+upa
	L970: !
	dim vnam$*30
	vnam$=''
	read #paymstr,using 'form pos 9,C 30',key=vn$,release: vnam$ nokey L1000
	goto L1010
	L1000: !
	if prcode=1 then
		read #rpmstr,using 'form pos 9,C 30',key=vn$: vnam$ nokey ignore
	end if
	L1010: !
	if trim$(vnam$)='CRIS PERRY' then pr 'vn$='&vn$ : pause
	pr #255,using 'form pos 1,C 32,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,pos 87,N 10.2,pos 99,C 4,C 7,C 3': vnam$,iv$,ivd,dd,ade$(1:18),upa,gl$(1:3),gl$(4:9),gl$(10:12) pageoflow NEWPGE
	upa=0
	if endcode=1 then goto L1360
	goto L700
	L1050: !
	if gl$(3:3)=' ' then gl$(3:3)='0'
	if gl$(12:12)=' ' then gl$(12:12)='0'
	read #work,using 'form pos 13,pd 10.2',key=gl$: gla nokey L1120
	if amt<-20202020 then amt=0
	gla=gla+amt
	rewrite #work,using 'form pos 13,pd 10.2': gla
	goto L1150
	L1120: !
	if amt<-20202020 then amt=0
	if amt<>0 then
		write #work,using 'form pos 1,C 12,pd 10.2': gl$,amt
	end if
	L1150: !
return ! /r
NEWPGE: pr #255: newpage: gosub HDR : continue

END1: ! r:
	if r4=0 then goto Xit
	endcode=1
goto L760 ! /r
L1360: ! r:
	if vc<2 then goto L1390
	! pr #255: TAB(87);'__________'
	! pr #255,Using 1800: 'Vendor Total',V3 Pageoflow NEWPGE
	L1390: !
	gosub TotalFund
	pr #255: tab(87);'__________'
	pr #255,using 'form pos 67,C 18,N 12.2': 'Final Total',t3 pageoflow NEWPGE
	pr #255: tab(87);'=========='
	restore #work,key>='            ': nokey Finis
	L1440: !
	read #work,using 'form pos 1,C 12,pd 10.2': gl$,gla eof Finis
	if hf$='' or hf$=gl$(1:3) then goto L1470
	gosub TOTNOFX
	L1470: !
	hf$=gl$(1:3)
	de$=''
	read #glmstr,using 'form pos 13,C 50',key=gl$: de$ nokey L1500
	L1500: !
	pr #255,using 'form pos 12,C 14,C 50,N 12.2': gl$,de$,gla pageoflow NEWPGE
	tnofx+=gla
goto L1440 ! /r

TOTNOFX: ! r:
	pr #255: tab(78);'__________'
	! If FUND<>2 Then Goto 1550
	dim fd$*30
	if val(hf$)>0 then fd$='Total for Fund #: '&ltrm$(hf$) else fd$='Total'
	pr #255,using 'form pos 12,C 14,C 50,N 12.2': '',fd$,tnofx pageoflow NEWPGE
	pr #255: pageoflow NEWPGE
	tnofx=0
return ! /r

Finis: ! r:
	gosub TOTNOFX
	if fund=2 then gosub SUB_FT2
	fnClosePrn
	close #work,free: ioerr ignore
goto Xit ! /r
Xit: fnXit

TotalFund: ! r:
	pr #255: tab(87);'__________'
	pr #255,using 'form pos 67,C 18,N 12.2': 'Fund   Total',ft(3) pageoflow NEWPGE
	mat ft=(0)
return  ! /r

ASK_PP1: ! r:
	open #clwork=10: 'Name=[Q]\CLmstr\CLWork[acsuserid].h[cno], Size=0, RecL=93, Replace', internal,outIn
	open #trmstr=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,i,k
	open #tralloc=2: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr',i,i,k
	dim pp1yn$*1
	if pp1yn$='N' then goto END8
	ld1=fndate_mmddyy_to_ccyymmdd(ld1) : hd1=fndate_mmddyy_to_ccyymmdd(hd1)
	ReadTrMstr: !
	dim tr$(5)*35
	read #trmstr,using 'form pos 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,pos 78,n 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),scd eof END8
	tr$(3)=str$(tr3)
	if tcde=2 then goto ReadTrMstr ! skip receipts
	if pr$='N' and scd=4 then goto ReadTrMstr ! skip payroll checks
	pd1=fndate_mmddyy_to_ccyymmdd(val(tr$(2))) conv ReadTrMstr
	if ld1=0 and hd1=0 then goto L2150
	if pd1<ld1 or pd1>hd1 then goto ReadTrMstr
	L2150: !
	ck1=val(tr$(1)) conv NextIvnum
	goto RestoreTrAlloc
	NextIvnum: !
	ivnum+=1 : ck1=ivnum
	RestoreTrAlloc: !
	key$=cnvrt$('Pic(zz)',bank_code)&str$(tcde)&tr$(1)
	restore #tralloc,key=key$: nokey ReadTrMstr
	READ_TRALLOC: !
	read #tralloc,using 'form pos 1,C 11,N 3,N 6,N 3,PD 5.2,C 30,C 6,X 3,C 12,N 1': newkey$,mat gl,amt,de$,ivd$,po$,pc eof ReadTrMstr
	ivd=val(ivd$) conv ignore
	if newkey$<>key$ then goto ReadTrMstr
	if ivd=0 then goto L2270
	if pc=0 or pc=1 then goto WRITE_CLWORK ! Never been in AP
	if fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1 then goto EO_TRALLOC_LOOP
	L2270: !
	if tcde=2 then
		paid$='Received'
	else if tcde=3 then
		paid$='Adjustment'
	else
		paid$='Paid'
	end if
	WRITE_CLWORK: !
	write #clwork,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': tr$(4),paid$,ivd,0,po$,de$(1:18),amt,0,mat gl,ck1
	EO_TRALLOC_LOOP: !
goto READ_TRALLOC ! /r

END8: ! r:
	close #trmstr:
	close #tralloc:
goto ASK_SORT ! /r

SUB_FT2: ! r:
	fd$='Total for all Funds'
	pr #255: newpage : gosub HDR
	pr #255: 'Fund   Amount'
	pr #255: '____  __________'
	if ft2(1000)<>0 then
		pr #255,using 'form pos 1,N 4,N 12.2': 0,ft2(1000) pageoflow NEWPGE
	end if
	for j=1 to 999
		if ft2(j)<>0 then
			pr #255,using 'form pos 1,N 4,N 12.2': j,ft2(j) pageoflow NEWPGE
		end if
	next j
return ! /r

ASK_TI1: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'Cutoff Date:',38,1)
	fnTxt(1,40,8,0,1,'1',0,'No invoices past this date will be listed!')
	resp$(respc+=1)=''
	fnLbl(2,1,'Type of Report:',38,1)
	dim item1$(2)*15
	item1$(1)='Claims'
	item1$(2)='Purchases'
	fnComboA('claims-srt',2,40,mat item1$,tt$)
	resp$(respc+=1)=item1$(1)
	fnChk(3,41,'Include previously paid Invoices:',1)
	resp$(respc+=1)='False'
	fnLbl(5,1,'Starting Date:',38,1)
	fnTxt(5,40,8,0,1,'1',0,'Only applicable if including previously paid invoices!')
	resp$(respc+=1)=''
	fnLbl(6,1,'Ending Date:',38,1)
	fnTxt(6,40,8,0,1,'1',0,'Only applicable if including previously paid invoices!')
	resp$(respc+=1)=''
	fnLbl(8,1,'Sort by:',38,1)
	dim item2$(2)*35
	item2$(1)='Fund Number'
	item2$(2)='Vendor Number'
	fnComboA('claims-act',8,40,mat item2$)
	resp$(respc+=1)=item2$(1)
	fnLbl(10,1,'Show Invoices:',38,1)
	item2$(1)='All Invoices'
	item2$(2)='Coded for Payment'
	fnComboA('claims-3',10,40,mat item2$,'You have a choice of listing all unpaid invoices on the report, or just those that have been selected for payment')
	resp$(respc+=1)=item2$(1)
	fnChk(11,41,'Include payroll checks:',1)
	resp$(respc+=1)='False'
	fnLbl(13,1,'Beginning Position of Department Number:',43,1)
	fnTxt(13,46,2,0,1,'30',0,'If you have departmental breakdowns within a fund, you must identify the first digit of the department # within the general ledger number')
	resp$(respc+=1)=' '
	fnLbl(14,1,'Ending Position of Department Number:',43,1)
	fnTxt(14,46,2,0,1,'30',0,'Last digit representing department #. Example: GL # "001001600000" The beginning position would be 6 and the ending 7 if department number was the 16.')
	resp$(respc+=1)=' '
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d2=val(resp$(1)) ! cutoff date
	ti1$=resp$(2)(1:1)
	if resp$(3)(1:1)='T' then pp1yn$='Y' else pp1yn$='N'
	ld1=val(resp$(4))
	hd1=val(resp$(5))
	if resp$(6)(1:1)='V' then fund=1 else fund=2 ! vendor # or fund #
	if resp$(7)(1:1)='A' then coded=1 else coded=2 ! all invoices or only coded for payment
	if resp$(8)(1:1)='T' then pr$='Y' else pr$='N'
	pos1=val(resp$(9))
	pos2=val(resp$(10))
return ! /r

TotalDepartment: ! r: DPT TOTAL   ! special modifications for Ed Horton clients - Must have the departmental file set up under files for this to run
	! If GL$(3:3)<>'0' Then Pause
	if fund<>2 then goto TD_XIT
	if dp(3)<>0 then
		dim dp$*30
		dp$=''
		if pos1=0 and pos2=0 then goto TD_XIT
		dpt$=gl$(1:3)&cnvrt$('pic(zz)',val(gl$(pos1:pos2)))
		read #11,using 'form pos 1,n 3,n 2,c 30',key=dpt$: fund1,dept1,dp$ nokey TD_XIT
		if dept1=0 then goto TD_XIT ! don't do department totals if department file only has fund numbers.
		pr #255: tab(87);'----------'
		pr #255,using L3070: 'DEPT   TOTAL',dp(3) pageoflow NEWPGE
		L3070: form pos 67,c 18,n 12.2,skip 1
	end if
	mat dp=(0)
	if gl$(1:3)><fund$ then goto TD_XIT
	hp3=51-int(len(rtrm$(dp$))/2)
	pr #255,using L3120: dp$ pageoflow NEWPGE
	L3120: form pos hp3,c 30,skip 1
	TD_XIT: !
return  ! /r
HDR: ! r:
	fd$=''
	fund$=gl$(1:3)
	read #hFund,using 'form pos 4,C 25',key=fund$: fd$ nokey ignore ! changed from 'nokey ignore' in an attempt to fix error 201
	nofx=1 : pg+=1
	pr #255,using 'form pos 1,C 8,CC 86': date$,env$('cnam')
	dim tmp$*40
	if ti1$='C' then tmp$='Claims' else tmp$='Purchases'
	pr #255,using 'form pos 1,C 8,pos 30,C 50': time$,tmp$&' Report-'&rtrm$(ty1$)&'-'&rtrm$(ty2$)
	pr #255,using 'form pos 1,C 4,N 4,CC 86': 'Page',pg,date$('Month DD, CCYY')
	if fund<>2 then fd$=''
	pr #255,using 'form pos 1,Cc 102': fd$
	pr #255: ''
	pr #255: '                                              Invoice     Due                           Total          GL    '
	pr #255: 'Payee Name                      Invoice Numb    Date      Date    Description            Due          Number'
	pr #255: '______________________________  ____________  ________  ________  __________________  __________  ______________'
	nofx=1
	!  a nokey on the read #hFund line above can cause and err 201 (return without gosub)  the nokey is the last error and the continue goes to the line after the unsuccessful read   Build the general ledger control file and put the fund names in it!!!!!!!!
return  ! /r
include: ertn
