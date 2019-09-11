
	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fnAcs,fnTos,fnLbl,fnTxt,fncomboa,fnChk,fnCmdSet
	library 'S:\Core\Library': fndat
	library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd
	on error goto ERTN

	dim vnam$*30,de$*50,fd$*30,ft(3),aa(2),gl(3),ade$*50
	dim ft2(1000)
	dim ti1$*1
	dim tr$(5)*35,tr(2)
	dim pp1yn$*1
	dim item2$(2)*35
	dim item1$(2)*15
	dim tmp$*40
	dim dp(3)
	dim dp$*30

	fntop(program$)
	cancel=99

	gosub ASK_TI1
	goto ASK_PP1

ASK_SORT: ! 
	if fund=1 then ty1$="Vendor Sequence" else ty1$="Fund Sequence"
	if coded=1 then ty2$="All Invoices" else ty2$="Selected Invoices"
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.H[cno],Shr",internal,input,relative 
	execute "Index [Q]\CLmstr\unpdaloc.H[cno]"&' '&"[Q]\CLmstr\Uaidx2.H[cno] 1 20 Replace DupKeys -n" ! index in vendor, reference order
	open #unpdaloc=8: "Name=[Q]\CLmstr\UnPdAloc.H[cno],KFName=[Q]\CLmstr\Uaidx2.H[cno],Shr",internal,input,keyed 
	READ_PAYTRANS: ! 
	read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,g 10.2,G 1': vn$,iv$,ivd,dd,po$,de$,upa,cde eof L420
	if coded=2 and cde=0 then goto READ_PAYTRANS
	if fndate_mmddyy_to_ccyymmdd(dd)>fndate_mmddyy_to_ccyymmdd(d2) then goto READ_PAYTRANS
	if ti1$<>"C" and (fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1) then 
		goto READ_PAYTRANS ! Purchases Only
	end if
	ivnum+=1 ! Unique Number for each Invoice
	restore #unpdaloc,key>=vn$&"            ": 
	READ_UNPDALOC: ! 
	read #unpdaloc,using 'Form pos 1,c 8,c 12,N 3,N 6,N 3,PD 5.2,C 30': alvn$,aliv$,mat gl,amt,ade$ noRec READ_PAYTRANS eof READ_PAYTRANS
	if alvn$<>vn$ then goto READ_PAYTRANS
	if trim$(aliv$)<>trim$(iv$) then goto READ_UNPDALOC
	if fund=2 then de$=ade$(1:18)
	if sum(gl)=0 and amt=0 then goto L410 ! don't write zero records
	write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': vn$,iv$,ivd,dd,po$,de$(1:18),amt,cde,mat gl,ivnum
	L410: !
	goto READ_UNPDALOC
	L420: !
	close #paytrans: : close #unpdaloc: : close #clwork: 
	upa=0 ! this sort is ok. it sorts a temporary work file. leave in
	open #tmp=9: "Name="&env$('temp')&"\Control,Size=0,RecL=128,Replace",internal,output 
	write #tmp,using 'Form POS 1,C 128': "File CLWork"&wsid$&".H[cno],[Q]\CLmstr,,"&env$('temp')&"\Addr,,,,,A,N"
	if fund=2 then 
		write #tmp,using 'Form POS 1,C 128': "Mask 74,12,N,A" ! "Mask 74,3,N,A,1,20,C,A,86,4,N,A"
	else
		write #tmp,using 'Form POS 1,C 128': "Mask 1,20,C,A,86,4,N,A"
	end if
	close #tmp: 
	execute "Free "&env$('temp')&"\Addr -n" ioerr ignore
	execute "Sort "&env$('temp')&"\Control -n"
	open #addr:=9: "Name="&env$('temp')&"\Addr",internal,input 
	open #paymstr:=13: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed 
	open #rpmstr:=23: "Name=[Q]\PRmstr\rpMstr.H[cno],KFName=[Q]\PRmstr\rpIndex.H[cno],Shr",internal,input,keyed ioerr L550
	prcode=1
	L550: !
	open #clwork:=10: "Name=[Q]\CLmstr\CLWork"&wsid$&".H[cno],Shr",internal,input,relative 
	open #glmstr:=5: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed 
	open #work:=6: "Name="&env$('temp')&"\Work,Size=0,RecL=22,Replace",internal,output 
	close #work: 
	execute "Free "&env$('temp')&"\Indx -n" ioerr ignore
	execute "Index "&env$('temp')&"\Work,"&env$('temp')&"\Indx,1,12,Replace,DupKeys -n"
	open #work=6: "Name="&env$('temp')&"\Work,KFName="&env$('temp')&"\Indx",internal,outIn,keyed 
	open #fundmstr=7: "Name=[Q]\CLmstr\FundMstr.H[cno],KFName=[Q]\CLmstr\FundIdx1.H[cno],Shr",internal,input,keyed 
	notused=1: open #11: "Name=[Q]\CLmstr\dptmSTR.H[cno],KFName=[Q]\CLmstr\dptidx1.H[cno]",internal,input,keyed ioerr L640 : notused=0
	L640: !
	fnopenprn
	vn$="": iv$=""
L660: read #addr,using 'Form POS 1,PD 3': r4 eof END1
	if r4=0 then goto L660
	read #clwork,using 'Form POS 86,N 4',rec=r4: ivnum
	if hivnum=0 then upa=0 : goto L700 else goto L760
	L700: !
	read #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8',rec=r4: vn$,iv$,ivd,dd,po$,ade$,amt,cde,mat gl,ivnum
	upa=upa+amt
	hivnum=ivnum
	gl$=cnvrt$("PIC(ZZ#)",gl(1))&cnvrt$("PIC(ZZZZZZ)",gl(2))&cnvrt$("PIC(ZZ#)",gl(3))
	gosub L1050 ! ACCUMULATE G/L TOTALS
	goto L660
	L760: !
	if vn$=hvn$ then goto L810
	if fund=2 then goto L810 ! no vendor totals in fund sequence
	if vc<2 then goto L800
	pr #255: tab(87);"__________"
	pr #255,using L3070: "Vendor Total",v3
	L800: !
	vc=v1=v2=v3=0
	L810: ! pr #255: PAGEOFLOW NEWPGE
	vc=vc+1
	hvn$=vn$
	if nofx=0 then gosub HDR
	! If GL$(3:3)<>"0" Then Pause
	if pos1=0 and pos2=0 then goto L880
	if notused =0 and dpt$><gl$(1:3)&cnvrt$("pic(zz)",val(gl$(pos1:pos2))) then gosub TOTAL_DEPARTMENT
	L880: !
	if fund<>2 then goto L930
	if fund$><gl$(1:3) then gosub TOTAL_FUND : pr #255: newpage : gosub HDR
	f2=(val(gl$(1:3)))
	if f2=0 then f2=1000
	ft2(f2)=ft2(f2)+upa
	L930: !
	if cde=1 then p1=97: v1=v1+upa: t1=t1+upa : ft(1)=ft(1)+upa: dp(1)=dp(1)+upa else p1=109: v2=v2+upa : t2=t2+upa : ft(2)=ft(2)+upa : dp(2)=dp(2)+upa
	v3=v3+upa
	t3=t3+upa : if rtrm$(ltrm$(iv$))="Received" or rtrm$(ltrm$(iv$))="Adjustment" then goto L970 else ft(3)=ft(3)+upa : dp(3)=dp(3)+upa
	L970: !
	vnam$=""
	read #paymstr,using 'Form POS 9,C 30',key=vn$,release: vnam$ nokey L1000
	goto L1010
	L1000: !
	if prcode=1 then 
		read #rpmstr,using 'Form POS 9,C 30',key=vn$: vnam$ nokey ignore
	end if
	L1010: ! 
	if trim$(vnam$)='CRIS PERRY' then pr 'vn$='&vn$ : pause 
	pr #255,using 'Form POS 1,C 32,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS 87,N 10.2,POS 99,C 4,C 7,C 3': vnam$,iv$,ivd,dd,ade$(1:18),upa,gl$(1:3),gl$(4:9),gl$(10:12) pageoflow NEWPGE
	upa=0
	if endcode=1 then goto L1360
	goto L700
	L1050: !
	if gl$(3:3)=" " then gl$(3:3)="0"
	if gl$(12:12)=" " then gl$(12:12)="0"
	read #work,using 'Form POS 13,pd 10.2',key=gl$: gla nokey L1120
	if amt<-20202020 then amt=0
	gla=gla+amt
	rewrite #work,using 'Form POS 13,pd 10.2': gla
	goto L1150
	L1120: ! 
	if amt<-20202020 then amt=0
	if amt<>0 then 
		write #work,using 'Form POS 1,C 12,pd 10.2': gl$,amt
	end if 
L1150: ! 
return ! /r
NEWPGE: pr #255: newpage: gosub HDR : continue 

END1: ! r:
	if r4=0 then goto XIT
	endcode=1
goto L760 ! /r
L1360: ! r:
	if vc<2 then goto L1390
	! pr #255: TAB(87);"__________"
	! pr #255,Using 1800: "Vendor Total",V3 Pageoflow NEWPGE
	L1390: !
	gosub TOTAL_FUND
	pr #255: tab(87);"__________"
	pr #255,using 'Form POS 67,C 18,N 12.2': "Final Total",t3 pageoflow NEWPGE
	pr #255: tab(87);"=========="
	restore #work,key>="            ": nokey Finis
	L1440: !
	read #work,using 'Form POS 1,C 12,pd 10.2': gl$,gla eof Finis
	if hf$="" or hf$=gl$(1:3) then goto L1470
	gosub TOTNOFX
	L1470: !
	hf$=gl$(1:3)
	de$=""
	read #glmstr,using 'Form POS 13,C 50',key=gl$: de$ nokey L1500
	L1500: !
	pr #255,using 'Form POS 12,C 14,C 50,N 12.2': gl$,de$,gla pageoflow NEWPGE
	tnofx+=gla
goto L1440 ! /r

TOTNOFX: ! r:
	pr #255: tab(78);"__________"
	! If FUND<>2 Then Goto 1550
	if val(hf$)>0 then fd$="Total for Fund #: "&ltrm$(hf$) else fd$="Total"
	pr #255,using 'Form POS 12,C 14,C 50,N 12.2': "",fd$,tnofx pageoflow NEWPGE
	pr #255: pageoflow NEWPGE
	tnofx=0
return ! /r

Finis: ! r:
	gosub TOTNOFX
	if fund=2 then gosub SUB_FT2
	fncloseprn
	close #work,free: ioerr ignore
XIT: fnxit

TOTAL_FUND: ! r:
	pr #255: tab(87);"__________"
	pr #255,using 'Form POS 67,C 18,N 12.2': "Fund   Total",ft(3) pageoflow NEWPGE
	mat ft=(0)
return  ! /r

ASK_PP1: ! r:
	open #clwork=10: "Name=[Q]\CLmstr\CLWork"&wsid$&".H[cno], Size=0, RecL=93, Replace", internal,outIn 
	open #trmstr=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno],Shr",internal,input,keyed 
	open #tralloc=2: "Name=[Q]\CLmstr\TrAlloc.H[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.H[cno],Shr",internal,input,keyed 
	if pp1yn$="N" then goto END8
	ld1=fndate_mmddyy_to_ccyymmdd(ld1) : hd1=fndate_mmddyy_to_ccyymmdd(hd1)
	READ_TRMSTR: ! 
	read #trmstr,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,pos 78,n 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),scd eof END8 
	tr$(3)=str$(tr3)
	if tcde=2 then goto READ_TRMSTR ! skip receipts
	if pr$="N" and scd=4 then goto READ_TRMSTR ! skip payroll checks
	pd1=fndate_mmddyy_to_ccyymmdd(val(tr$(2))) conv READ_TRMSTR
	if ld1=0 and hd1=0 then goto L2150
	if pd1<ld1 or pd1>hd1 then goto READ_TRMSTR
	L2150: !
	ck1=val(tr$(1)) conv NEXT_IVNUM
	goto RESTORE_TRALLOC
	NEXT_IVNUM: ! 
	ivnum+=1 : ck1=ivnum
	RESTORE_TRALLOC: ! 
	key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&tr$(1) 
	restore #tralloc,key=key$: nokey READ_TRMSTR
	READ_TRALLOC: ! 
	read #tralloc,using 'Form Pos 1,C 11,N 3,N 6,N 3,PD 5.2,C 30,C 6,X 3,C 12,N 1': newkey$,mat gl,amt,de$,ivd$,po$,pc eof READ_TRMSTR 
	ivd=val(ivd$) conv ignore
	if newkey$<>key$ then goto READ_TRMSTR
	if ivd=0 then goto L2270
	if pc=0 or pc=1 then goto WRITE_CLWORK ! Never been in AP
	if fndate_mmddyy_to_ccyymmdd(ivd)<ld1 or fndate_mmddyy_to_ccyymmdd(ivd)>hd1 then goto EO_TRALLOC_LOOP
	L2270: !
	if tcde=2 then 
		paid$="Received" 
	else if tcde=3 then 
		paid$="Adjustment" 
	else 
		paid$="Paid"
	end if
	WRITE_CLWORK: ! 
	write #clwork,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,pd 10.2,G 1,N 3,N 6,N 3,N 8': tr$(4),paid$,ivd,0,po$,de$(1:18),amt,0,mat gl,ck1
	EO_TRALLOC_LOOP: !
goto READ_TRALLOC ! /r

END8: ! r:
	close #trmstr: 
	close #tralloc: 
goto ASK_SORT ! /r

SUB_FT2: ! r:
	fd$="Total for all Funds"
	pr #255: newpage : gosub HDR
	pr #255: "Fund   Amount" 
	pr #255: "____  __________"
	if ft2(1000)<>0 then 
		pr #255,using 'Form POS 1,N 4,N 12.2': 0,ft2(1000) pageoflow NEWPGE
	end if
	for j=1 to 999
		if ft2(j)<>0 then 
			pr #255,using 'Form POS 1,N 4,N 12.2': j,ft2(j) pageoflow NEWPGE
		end if
	next j
return ! /r

ASK_TI1: ! r:
	fnTos(sn$="claims") 
	respc=0
	fnLbl(1,1,"Cutoff Date:",38,1)
	fnTxt(1,40,8,0,1,"1",0,"No invoices past this date will be listed!") 
	resp$(respc+=1)=""
	fnLbl(2,1,"Type of Report:",38,1)
	item1$(1)="Claims" 
	item1$(2)="Purchases"
	fncomboa("claims-srt",2,40,mat item1$,tt$) 
	resp$(respc+=1)=item1$(1)
	fnChk(3,41,"Include previously paid Invoices:",1) 
	resp$(respc+=1)="False"
	fnLbl(5,1,"Starting Date:",38,1)
	fnTxt(5,40,8,0,1,"1",0,"Only applicable if including previously paid invoices!") 
	resp$(respc+=1)=""
	fnLbl(6,1,"Ending Date:",38,1)
	fnTxt(6,40,8,0,1,"1",0,"Only applicable if including previously paid invoices!") 
	resp$(respc+=1)=""
	fnLbl(8,1,"Sort by:",38,1)
	item2$(1)="Fund Number" 
	item2$(2)="Vendor Number"
	fncomboa("claims-act",8,40,mat item2$) 
	resp$(respc+=1)=item2$(1)
	fnLbl(10,1,"Show Invoices:",38,1)
	item2$(1)="All Invoices" 
	item2$(2)="Coded for Payment"
	fncomboa("claims-3",10,40,mat item2$,"You have a choice of listing all unpaid invoices on the report, or just those that have been selected for payment") 
	resp$(respc+=1)=item2$(1)
	fnChk(11,41,"Include payroll checks:",1) 
	resp$(respc+=1)="False"
	fnLbl(13,1,"Beginning Position of Department Number:",43,1)
	fnTxt(13,46,2,0,1,"30",0,"If you have departmental breakdowns within a fund, you must identify the first digit of the department # within the general ledger number") 
	resp$(respc+=1)=" "
	fnLbl(14,1,"Ending Position of Department Number:",43,1)
	fnTxt(14,46,2,0,1,"30",0,"Last digit representing department #. Example: GL # '001001600000' The beginning position would be 6 and the ending 7 if department number was the 16.") 
	resp$(respc+=1)=" "
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT
	d2=val(resp$(1)) ! cutoff date
	ti1$=resp$(2)(1:1)
	if resp$(3)(1:1)="T" then pp1yn$="Y" else pp1yn$="N"
	ld1=val(resp$(4))
	hd1=val(resp$(5))
	if resp$(6)(1:1)="V" then fund=1 else fund=2 ! vendor # or fund #
	if resp$(7)(1:1)="A" then coded=1 else coded=2 ! all invoices or only coded for payment
	if resp$(8)(1:1)="T" then pr$="Y" else pr$="N"
	pos1=val(resp$(9))
	pos2=val(resp$(10))
return ! /r

TOTAL_DEPARTMENT: ! r: DPT TOTAL   ! special modifications for monticello and other Ed clients - Must have the departmental file set up under files for this to run
	! If GL$(3:3)<>"0" Then Pause
	if fund<>2 then goto TD_XIT
	if dp(3)<>0 then 
		dp$=""
		if pos1=0 and pos2=0 then goto TD_XIT
		dpt$=gl$(1:3)&cnvrt$("pic(zz)",val(gl$(pos1:pos2)))
		read #11,using 'form pos 1,n 3,n 2,c 30',key=dpt$: fund1,dept1,dp$ nokey TD_XIT
		if dept1=0 then goto TD_XIT ! don't do department totals if department file only has fund numbers.
		pr #255: tab(87);"----------"
		pr #255,using L3070: "DEPT   TOTAL",dp(3) pageoflow NEWPGE
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
	fd$=""
	fund$=gl$(1:3)
	read #fundmstr,using 'Form POS 4,C 25',key=fund$: fd$ nokey ignore ! changed from "nokey ignore" in an attempt to fix error 201
	nofx=1 : pg+=1
	pr #255,using 'Form POS 1,C 8,CC 86': date$,env$('cnam')
	if ti1$="C" then tmp$="Claims" else tmp$="Purchases"
	pr #255,using 'Form POS 1,C 8,POS 30,C 50': time$,tmp$&" Report-"&rtrm$(ty1$)&"-"&rtrm$(ty2$)
	pr #255,using 'Form POS 1,C 4,N 4,CC 86': "Page",pg,date$("Month DD, CCYY")
	if fund<>2 then fd$=""
	pr #255,using 'Form POS 1,Cc 102': fd$
	pr #255: ""
	pr #255: "                                              Invoice     Due                           Total          GL    "
	pr #255: "Payee Name                      Invoice Numb    Date      Date    Description            Due          Number"
	pr #255: "______________________________  ____________  ________  ________  __________________  __________  ______________"
	nofx=1
	!  a nokey on the read #fundmstr line above can cause and err 201 (return without gosub)  the nokey is the last error and the continue goes to the line after the unsuccessful read   Build the general ledger control file and put the fund names in it!!!!!!!!
return  ! /r
include: ertn