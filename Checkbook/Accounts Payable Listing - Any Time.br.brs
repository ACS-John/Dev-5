autoLibrary
on error goto Ertn
fnTop(program$)

fnTos
	respc=0
	fnLbl(1,1,'Period Ending Date:',23,1)
	fnTxt(1,25,10,0,1,'3')
	resp$(respc+=1)=str$(ped)
	fnLbl(2,1,'Print Order:',23,1)
	dim item1$(2)*15
	item1$(1)='General Ledger'
	item1$(2)='Vendor'
	fnComboA('ubnamlst-srt',2,25,mat item1$,tt$)
	resp$(respc+=1)=item1$(1)
	fnLbl(3,1,'Fund Number to Print:',23,1)
	fnTxt(3,25,3,0,1,'30')
	resp$(respc+=1)=str$(fund)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ped=val(resp$(1))
	dim sq1$*1
	sq1$=resp$(2)(1:1)
	fund=val(resp$(3))

	open #hPayee=fnH: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,outIn,k
	open #hTrans=fnH: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,outIn,k ! #1
	open #hTransAlloc=fnH: 'Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr',i,i,k ! #3
	open #hPayTrans=fnH: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,i,k ! #4
	open #hUnpaidAlloc=fnH: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],Version=2,KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr',i,i,k ! #6
	open #hWork=fnH: 'Name=[Temp]\Work,Size=0,RecL=66,Replace',internal,output

ReadPayTrans: !
	read #hPayTrans,using 'form pos 1,C 8,C 12,2*G 6',release: vn$,iv$,ivd,dd eof EoPayTrans
	if fndate_mmddyy_to_ccyymmdd(ivd)>ped then goto ReadPayTrans
	dim de$*30
	de$=''
	read #hPayee,using 'form pos 9,C 30',key=vn$: de$ nokey ignore
	restore #hUnpaidAlloc,key>=vn$&iv$:
READ_UNPDALOC: !
	read #hUnpaidAlloc,using 'form pos 1,c 8,c 12,C 12,PD 5.2': alocvn$,alociv$,gl$,upa noRec ReadPayTrans eof ReadPayTrans
	if alocvn$<>vn$ or alociv$<>iv$ then goto ReadPayTrans
	if fund>0 and fund<>val(gl$(1:3)) then goto READ_UNPDALOC
	if upa then ! if upa=0 then goto L470 ! don't pr 0 breakdowns
		write #hWork,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,ltrm$(rtrm$(iv$))&' '&de$(1:17),upa
	end if ! L470: !
goto READ_UNPDALOC

EoPayTrans: !
	close #hPayTrans:
ReadTrans: !
	read #hTrans,using 'form pos 1,n 2,n 1,c 8,N 6,pos 28,C 8,pos 36,C 30,pos 78,N 1': bank_code,tcde,tr$(1),pd,vn$,de$,scd eof EoTrans
	if scd=4 then goto ReadTrans
	if tcde>1 then goto ReadTrans
	if ped=>fndate_mmddyy_to_ccyymmdd(pd) then goto ReadTrans
	adr=ta(1)
ReadTransAlloc: !
	key$=cnvrt$('Pic(zz)',bank_code)&str$(tcde)&tr$(1)
	restore #hTransAlloc,key=key$: nokey ReadTrans
	do
		read #hTransAlloc,using 'form pos 1,C 11,C 12,PD 5.2,X 30,N 6': newkey$,gl$,amt,ivd eof ReadTrans
		if key$<>newkey$ then goto ReadTrans
		if ped<fndate_mmddyy_to_ccyymmdd(ivd) or ivd=0 or (fund>0 and fund<>val(gl$(1:3))) then goto EoTrAlloc
		if amt=0 then goto EoTrAlloc ! don't allow 0 amounts to show
		write #hWork,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,de$,amt
		EoTrAlloc: !
	loop

EoTrans: !
	close #hTrans:
	close #hTransAlloc:
	if lrec(hWork)=0 then goto Xit
	close #hWork:
	open #tmp=1: 'Name=[Temp]\Control,Size=0,RecL=128,Replace',internal,output
	write #tmp,using 'form pos 1,C 128': '! Sorting Accounts Payable Invoice List...'
	write #tmp,using 'form pos 1,C 128': 'File [Temp]\Work,,,[Temp]\Addr,,,,,A,N'
	if sq1$='G' then
		write #tmp,using 'form pos 1,C 128': 'Mask 1,26,C,A'
	else if sq1$='V' then
		write #tmp,using 'form pos 1,C 128': 'Mask 19,8,C,A,13,6,C,A'
	end if
	close #tmp:
	fnFree(env$('Temp')&'\ADDR')
	execute 'Sort [Temp]\Control'
	open #addr=1: 'Name=[Temp]\ADDR',i,i ioerr Xit
	open #hWork=fnH: 'Name=[Temp]\WORK',i,i,r
	fnOpenPrn
	gosub PrHeader
ReadAddr: !
	read #addr,using 'form pos 1,PD 3': r5 eof Finis
	read #hWork,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2',rec=r5: gl$,ivd,vn$,de$,amt noRec ReadAddr
	if t1=0 then goto PrLine
	if sq1$='G' then
		if hgl$=gl$ then goto PrLine
		if t1=0 then goto PrLine
		pr #255: '                                  ______________________________  __________'
		pr #255,using 'form pos 35,CR 30,N 12.2': 'GL # '&hgl$&' Total',t1
	else 
		if vn$=hvn$ then goto PrLine
		pr #255: '                                  ______________________________  __________'
		pr #255,using 'form pos 35,CR 30,N 12.2': 'Vendor #: '&ltrm$(hvn$)&' Total',t1
	end if
	pr #255: '                                  ______________________________  __________' pageoflow PgOf
	t1=0
	if amt=0 then goto ReadAddr
PrLine: !
	pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 10,C 30,N 12.2': gl$,ivd,vn$,de$,amt pageoflow PgOf
	t1+=amt : t2+=amt : hgl$=gl$ : hvn$=vn$
goto ReadAddr

PgOf: pr #255: newpage: gosub PrHeader : continue

PrHeader: ! r:
	pr #255,using 'form pos 1,C 8,CC 78': date$,env$('cnam')
	pr #255,using 'form pos 1,C 8,pos 32,C 40': time$,env$('program_caption')
	pr #255,using 'form pos 1,C 4,N 4,pos 38,C 40': 'Page',pg+=1,'as of '&cnvrt$('PIC(zzZZ/ZZ/ZZ)',ped)
	pr #255: ''
	pr #255: '               Invoice   Vendor                                            '
	pr #255: ' G/L Number     Date     Number   Description                         Amount  '
	pr #255: '____________  ________  ________  ______________________________  __________'
return ! /r

Finis: ! r:
	pr #255: '                                  ______________________________  __________'
	if sq1$='G' then
		pr #255,using 'form pos 35,CR 30,N 12.2': 'GL # '&hgl$&' Total',t1
	end if
	if sq1$='V' then
		pr #255,using 'form pos 35,CR 30,N 12.2': '  Vendor: '&ltrm$(hvn$)&' Total',t1
	end if
	pr #255: '                                  ______________________________  __________' pageoflow PgOf
	pr #255,using 'form pos 35,CR 30,N 12.2': 'Final Total',t2
	pr #255: '                                  =========================================='
	fnClosePrn
goto Xit ! /r
Xit: fnXit
include: ertn
