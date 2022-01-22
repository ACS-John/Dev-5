autoLibrary
on error goto Ertn
fnTop(program$)

dim vnam$*30,de$*30,sq1$*1,item1$(2)*15

fnTos
	respc=0
	fnLbl(1,1,'Period Ending Date:',23,1)
	fnTxt(1,25,10,0,1,'3')
	resp$(respc+=1)=str$(ped)
	fnLbl(2,1,'Print Order:',23,1)
	item1$(1)='General Ledger'
	item1$(2)='Vendor'
	fncomboa('ubnamlst-srt',2,25,mat item1$,tt$)
	resp$(respc+=1)=item1$(1)
	fnLbl(3,1,'Fund Number to Print:',23,1)
	fnTxt(3,25,3,0,1,'30')
	resp$(respc+=1)=str$(fund)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ped=val(resp$(1))
	sq1$=resp$(2)(1:1)
	fund=val(resp$(3))

	open #paymstr=13: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,outIn,k
	open #trmstr=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,outIn,k
	open #tralloc=3: 'Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr',i,i,k
	open #paytrans=4: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,i,k
	open #unpdaloc=6: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],Version=2,KFName=[Q]\CLmstr\UAIdx2.h[cno],Shr',i,i,k
	open #work=5: 'Name=[Temp]\Work,Size=0,RecL=66,Replace',internal,output
READ_PAYTRANS: !
	read #paytrans,using 'form pos 1,C 8,C 12,2*G 6',release: vn$,iv$,ivd,dd eof END1
	if fndate_mmddyy_to_ccyymmdd(ivd)>ped then goto READ_PAYTRANS
	de$=''
	read #paymstr,using 'form pos 9,C 30',key=vn$: de$ nokey RESTORE_UNPDALOC
RESTORE_UNPDALOC: !
	restore #unpdaloc,key>=vn$&iv$:
READ_UNPDALOC: !
	read #unpdaloc,using 'form pos 1,c 8,c 12,C 12,PD 5.2': alocvn$,alociv$,gl$,upa noRec READ_PAYTRANS eof READ_PAYTRANS
	if alocvn$<>vn$ or alociv$<>iv$ then goto READ_PAYTRANS
	if fund>0 and fund<>val(gl$(1:3)) then goto READ_UNPDALOC
	if upa=0 then goto L470 ! don't pr 0 breakdowns
	write #work,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,ltrm$(rtrm$(iv$))&' '&de$(1:17),upa
L470: goto READ_UNPDALOC

END1: close #paytrans:
READ_TRMSTR: !
	read #trmstr,using 'form pos 1,n 2,n 1,c 8,N 6,pos 28,C 8,pos 36,C 30,pos 78,N 1': bank_code,tcde,tr$(1),pd,vn$,de$,scd eof END2
	if scd=4 then goto READ_TRMSTR
	if tcde>1 then goto READ_TRMSTR
	if ped=>fndate_mmddyy_to_ccyymmdd(pd) then goto READ_TRMSTR
	adr=ta(1)
READ_TRALLOC: !
	key$=cnvrt$('Pic(zz)',bank_code)&str$(tcde)&tr$(1)
	restore #tralloc,key=key$: nokey READ_TRMSTR
L580: !
	read #tralloc,using 'form pos 1,C 11,C 12,PD 5.2,X 30,N 6': newkey$,gl$,amt,ivd eof READ_TRMSTR
	if key$<>newkey$ then goto READ_TRMSTR
	if ped<fndate_mmddyy_to_ccyymmdd(ivd) or ivd=0 or (fund>0 and fund<>val(gl$(1:3))) then goto EO_LOOP
	if amt=0 then goto EO_LOOP ! don't allow 0 amounts to show
	write #work,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2': gl$,ivd,vn$,de$,amt
EO_LOOP: goto L580

END2: close #1:
	close #tralloc:
	if lrec(work)=0 then goto Xit
	close #work:
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
	open #addr=1: 'Name=[Temp]\ADDR',internal,input ioerr Xit
	open #work=5: 'Name=[Temp]\WORK',i,i,r
	fnopenprn
	gosub HDR
READ_ADDR: !
	read #addr,using 'form pos 1,PD 3': r5 eof ENDALL
	read #work,using 'form pos 1,C 12,N 6,C 8,C 30,N 10.2',rec=r5: gl$,ivd,vn$,de$,amt noRec READ_ADDR
	if t1=0 then goto PRINT_LINE
	if sq1$='G' then goto L890
	if vn$=hvn$ then goto PRINT_LINE
	pr #255: '                                  ______________________________  __________'
	pr #255,using 'form pos 35,CR 30,N 12.2': 'Vendor #: '&ltrm$(hvn$)&' Total',t1
	goto L930
L890: if hgl$=gl$ then goto PRINT_LINE
	if t1=0 then goto PRINT_LINE
	pr #255: '                                  ______________________________  __________'
	pr #255,using 'form pos 35,CR 30,N 12.2': 'GL # '&hgl$&' Total',t1
L930: pr #255: '                                  ______________________________  __________' pageoflow NEWPGE
	t1=0
	if amt=0 then goto READ_ADDR
PRINT_LINE: !
	pr #255,using 'form pos 1,C 14,PIC(ZZ/ZZ/ZZ),X 2,C 10,C 30,N 12.2': gl$,ivd,vn$,de$,amt pageoflow NEWPGE
	t1+=amt : t2+=amt : hgl$=gl$ : hvn$=vn$
goto READ_ADDR
 
NEWPGE: pr #255: newpage: gosub HDR : continue
 
HDR: !
	pr #255,using 'form pos 1,C 8,CC 78': date$,env$('cnam')
	pr #255,using 'form pos 1,C 8,pos 32,C 40': time$,env$('program_caption')
	pr #255,using 'form pos 1,C 4,N 4,pos 38,C 40': 'Page',pg+=1,'as of '&cnvrt$('PIC(zzZZ/ZZ/ZZ)',ped)
	pr #255: ''
	pr #255: '               Invoice   Vendor                                            '
	pr #255: ' G/L Number     Date     Number   Description                         Amount  '
	pr #255: '____________  ________  ________  ______________________________  __________'
return
 
ENDALL: !
	pr #255: '                                  ______________________________  __________'
	if sq1$='G' then
		pr #255,using 'form pos 35,CR 30,N 12.2': 'GL # '&hgl$&' Total',t1
	end if
	if sq1$='V' then
		pr #255,using 'form pos 35,CR 30,N 12.2': '  Vendor: '&ltrm$(hvn$)&' Total',t1
	end if
	pr #255: '                                  ______________________________  __________' pageoflow NEWPGE
	pr #255,using 'form pos 35,CR 30,N 12.2': 'Final Total',t2
	pr #255: '                                  =========================================='
	fncloseprn
Xit: fnXit
include: ertn
