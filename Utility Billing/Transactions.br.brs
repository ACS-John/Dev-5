! formerly S:\acsUB\FlexTran
fn_setup
fntop(program$)
fn_transfile
XIT: fnxit
! <Updateable Region: ERTN>
ERTN: fnerror(program$,err,line,act$,"xit")
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fntop,fnTos,fnAcs,fnCmdKey,fnerror,fnFra,fnButton,fnChk,fnCmdSet,fnOpt,fnLbl,fnTxt,fncmbact
		library 'S:\Core\Library': fngethandle,fncreg_read,fncreg_write
		library 'S:\Core\Library': fnopenprn,fncloseprn,fnmsgbox,fnxit
		library 'S:\Core\Library': fnget_services
		library 'S:\Core\Library': fnflexinit1,fnflexadd1
		on error goto ERTN
		transtype$(1)="Charge"
		transtype$(2)="Penalty"
		transtype$(3)="Collection"
		transtype$(4)="Credit Memo"
		transtype$(5)="Debit Memo"
		fnget_services(mat serviceName$, mat srv$)
	end if
fnend
def library fntrans_total_as_of(; customer_key$,date_ccyymmdd,trans_type)
	if ~setup then let fn_setup
	fntrans_total_as_of=fn_trans_total_as_of(customer_key$,date_ccyymmdd,trans_type)
fnend
def fn_trans_total_as_of(;customer_key$,date_ccyymmdd, trans_type)
	! transaction_type or blank for all
	if ~ttao_setup then 
		ttao_setup=1
		open #ttao_h_trans:=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
		dim ttao_key$*19
	end if
	ttao_return=0
	customer_key$=lpad$(customer_key$,10)
! ttao_key$=rpad$(customer_key$,kln(ttao_h_trans)) 
	ttao_key$=customer_key$&lpad$(str$(date_ccyymmdd),8)&cnvrt$('pic(z)',trans_type)
	restore #ttao_h_trans,key>=ttao_key$: nokey TTAO_FINIS
	do
		read #ttao_h_trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2": ttao_customer_read$,ttao_date,ttao_code,ttao_amount eof TTAO_FINIS
		if lpad$(ttao_customer_read$,10)=lpad$(customer_key$,10) or trim$(ttao_customer_read$)<>'' then 
			if ~date_ccyymmdd or ttao_date=>date_ccyymmdd then 
				if trans_type=0 or trans_type=ttao_code then
					ttao_return+=ttao_amount
				end if
			end if
		end if
	loop until customer_key$<>'' and ttao_customer_read$<>customer_key$
	TTAO_FINIS: !
	fn_trans_total_as_of=ttao_return
fnend 
def library fntransfile(; hact$*81)
	if ~setup then fn_setup
	fntransfile=fn_transfile( hact$)
fnend
def fn_transfile(; hact$*81)
	dim resp$(10)*80,printlineform$*1024
	dim totalalloc(10),totalusage(3),usage(3)
	! ___________________________________________________________________
	SCREEN1: ! 
	gosub ASKTRANSET ! fnASKTRANSET(CKEY,SEL_CODE,BEG_DATE,END_DATE,Z$,HACT$)
	if ckey=2 and trim$(z$)="" then goto SCREEN1 ! don't allow pr to work if no customer selected
	! If CKEY=2 AND TRIM$(Z$)="[All]" Then Goto SCREEN1
	if ckey=2 then let fn_PRINTTRANS : goto SCREEN1 ! pr report of charges
	if ckey=5 then goto Tf_XIT else goto SCREEN_TRANS_GRID
	! ___________________________________________________________________
	SCREEN_TRANS_GRID: ! r:
	fnTos(sn$="Transaction-2")
	stgFlexLine=0
	fnButton(stgFlexLine+=1,1,'Columns',opt_columns:=6)
	if z$<>'[All]' then
		fnLbl(stgFlexLine+=1,1,'Account:',8,1)
		fnTxt(stgFlexLine,10,10,0,0,'',1)
		resp$(1)=z$
	end if
	fn_flextran (stgFlexLine+=1,1,0,z$,beg_date,end_date,sel_code)
	fnCmdKey('Print',opt_print:=4,0,0)
	fnCmdKey('Edit',opt_edit:=1,1,0)
	fnCmdKey('Back',opt_back:=2,0,0,'Return to filter selection')
	fnCmdKey('Close',5,0,1)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=opt_back then 
		goto SCREEN1
	else if ckey=5 then 
		goto Tf_XIT
	else if opt_columns and ckey=opt_columns then 
		fn_columnSelect
	else if ckey=opt_print then 
		fn_PRINTTRANS
	else if ckey=opt_edit then 
		if z$='[All]' then editrec=val(resp$(1)) else editrec=val(resp$(2))
		fn_TransactionEdit(editrec)
	end if 
	goto SCREEN_TRANS_GRID ! /r
	ASKTRANSET: ! r: Def FNASKTRANSET(&CKEY,&SEL_CODE,&BEG_DATE,&END_DATE,&Z$,HACT$*80)
		fnTos(sn$="Transaction-1")
		rc=cf=0
		fnFra(1,1,6,23,"Transaction Type","You can review all transactions or any specific type of transaction",0)
		cf+=1 : fratype=cf
		fnOpt(1,3,"[All]",0,fratype)
		if sel_code=1 or sel_code=0 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(2,3,"Charges",0,fratype)
		if sel_code=2 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(3,3,"Penalties",0,fratype)
		if sel_code=3 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(4,3,"Collections",0,fratype)
		if sel_code=4 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(5,3,"Credit Memos",0,fratype)
		if sel_code=5 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(6,3,"Debit Memos",0,fratype)
		if sel_code=6 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnFra(1,30,3,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
		cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
		fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
		fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
		if beg_date=0 then beg_date=date('mm')*10000+100+date('yy')-1
		resp$(rc+=1)=str$(beg_date)
		fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
		fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(end_date)
		fnFra(6,30,2,60,"Account","You review transactions for all accounts or for an individual.")
		cf+=1 : fraaccount=cf
		fnLbl(1,1,"Account:",8,1,0,fraaccount)
		if trim$(hact$)='' then 
			fncmbact(1,10,1,fraaccount)
			rc+=1
			if resp$(rc)="" then resp$(rc)="[All]"
		else 
			fnTxt(1,10,10,0,1,'',1,'',fraaccount) ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
			resp$(rc+=1)=hact$
		end if 
		if trim$(hact$)<>"" then resp$(rc)=hact$ else if resp$(rc)="" then resp$(rc)="[All]"
		fnCmdKey("Next",1,1,0,"Displays a list of transactions on the screen")
		fnCmdKey("Print",2,0,0,"Prints a transaction listing. (To get totals, you can only select one type of transaction at a time.")
		fnCmdKey("Cancel",5,0,1,"Returns to customer record")
		fnAcs(sn$,0,mat resp$,ckey)
		if ckey=5 then goto L810
		if resp$(1)="True" then 
			sel_code=1
		else if resp$(2)="True" then 
			sel_code=2
		else if resp$(3)="True" then 
			sel_code=3
		else if resp$(4)="True" then 
			sel_code=4
		else if resp$(5)="True" then 
			sel_code=5
		else if resp$(6)="True" then 
			sel_code=6
		end if 
		beg_date=val(resp$(7))
		end_date=val(resp$(8))
		z$=resp$(9)(1:10)
		L810: !
	return  ! /r
	Tf_XIT: ! 
fnend 
def fn_TransactionEdit(editrec)
	open #trans=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],Shr",internal,outIn,relative 
	read #trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2",rec=editrec: p$,tdate,tcode,tamount
	fnTos(sn$="Transaction-3")
	lc=rc=0 : mylen=20 : mypos=mylen+2
	fnLbl(lc+=1,1,"Record:",mylen)
	fnTxt(lc,mypos,10,0,0,empty$,1)
	resp$(rc+=1)=str$(editrec)
	fnLbl(lc+=1,1,"Customer:",mylen)
	fnTxt(lc,mypos,10,0,0,empty$,1)
	resp$(rc+=1)=p$
	fnLbl(lc+=1,1,"Date:",mylen)
	fnTxt(lc,mypos,10,0,0,"3")
	resp$(respc_tDate:=rc+=1)=str$(tdate)
	fnLbl(lc+=1,1,"Type:",mylen)
	fnTxt(lc,mypos,10,0,0,empty$,1)
	resp$(rc+=1)=transtype$(tcode)
	fnLbl(lc+=1,1,"Amount:",mylen)
	fnTxt(lc,mypos,10,0,0,"10",1)
	resp$(rc+=1)=str$(tamount)
	fnCmdKey('Save',1,1,0)
	fnCmdKey('Cancel',5,0,1)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=1 then 
		tdate=val(resp$(respc_tDate))
		rewrite #trans,using "Form pos 11,N 8",rec=editrec: tdate
	end if 
	close #trans: 
fnend
def fn_PRINTTRANS ! very local function - lots of inherritance
	dim scr1$(10)*30,alloc(10),nam$*30
	dim r(20,4),hd1$*255,serviceName$(10)*20,tg(11),metraddr$*30
	dim srv$(10)*2
	dim name$(10)*20
	! r: ask print_balance
	dim msgbox$(3)*128
	if env$('client')="White Hall" then 
		msgbox_default=0
	else 
		msgbox_default=256
	end if 
	msgbox$(1)="Include balance column?"
	msgbox$(2)="The balances listed were the account balance at the time the transaction completed"
	msgbox$(3)="and will be misleading if transactions were processed out of date sequence."
	fnmsgbox(mat msgbox$,resp$,cap$,32+3+msgbox_default)
	if resp$='Cancel' then goto PT_XIT
	if resp$='Yes' then 
		print_balance=1
	else 
		print_balance=0
	end if 
	! /r
	fnopenprn
	if trim$(serviceName$(3))<>"Electric" and srv$(3)="EL" then ptShowElecUsed=1 ! electric readings are being used for a reduction meter
	if trim$(serviceName$(4))<>"Gas" and srv$(4)="GA" then ptShowGasUsed=1 ! gas readings are being used for a reduction meter
	if trim$(z$)="[All]" then hd1$="    {\ul Account        Date   }" else hd1$="    {\ul    Date   }"
	sz1=0
	x=0
	for j=1 to 10
		if j=3 and ptShowElecUsed=1 then goto L1010 ! skp heading is electric field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
		if j=4 and ptShowGasUsed=1 then goto L1010 ! skp heading is gas field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
		x2=pos(trim$(serviceName$(j))," ",1)
		if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&"-"&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
		if trim$(serviceName$(j))<>"" then 
			scr1$(sz1+=1)=serviceName$(j)
			hd1$=hd1$&"  {\ul "&lpad$(rtrm$(serviceName$(j)(1:6)),6)&"}" : name$(x+=1)=serviceName$(j)
		end if  ! trim$(serviceName$(j))<>"" then
		L1010: ! 
	next j
	hd1$=hd1$&"{\ul     Total}"
	if print_balance then 
		hd1$=hd1$&"  {\ul   Balance }" 
	else
		if trim$(serviceName$(1))="Water" then 
			hd1$=hd1$&"  {\ul   Wa Used }" 
			water=1
		end if 
		if trim$(serviceName$(3))="Electric" then 
			hd1$=hd1$&"  {\ul   El Used }" 
			electric=1 
		else if ptShowElecUsed=1 then 
			hd1$=hd1$&"  {\ul      Used }" 
			electric=1
		end if 
		if trim$(serviceName$(4))="Gas" then 
			hd1$=hd1$&"  {\ul   Ga Used }" 
			gas=1 
		else if ptShowGasUsed=1 then 
			hd1$=hd1$&"  {\ul      Used }" 
			gas=1
		end if
	end if
	mat scr1$(sz1)
	mat alloc(sz1) : mat totalalloc(sz1)
	mat totalalloc=(0) : mat totalusage=(0) : totaltamount=0
	close #trans: ioerr ignore
	open #trans=2: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed 
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
	if trim$(z$)="[All]" then restore #trans: : goto L1160
	read #h_customer,using 'Form POS 11,c 30,C 28,pos 292,PD 4.2',key=lpad$(rtrm$(z$),10),release: metraddr$,nam$,account_balance nokey PT_NO_CUSTOMER
	restore #trans,key>=lpad$(rtrm$(z$),10)&"         ": nokey PT_FINIS
	L1160: !
	gosub HDR
	do 
		PT_TRANS_READ: ! 
		read #trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PT_FINIS
		if trim$(z$)="[All]" then goto L1200 ! skip verifying customer if all selected
		if p$<>lpad$(rtrm$(z$),10) then goto PT_FINIS
		L1200: !
		if beg_date<>0 and tdate<beg_date then goto PT_TRANS_READ
		if end_date<>0 and tdate>end_date then goto PT_TRANS_READ
		if tamount=0 then goto PT_TRANS_READ
		if sel_code>1 and tcode<>sel_code-1 then goto PT_TRANS_READ
		! 
		if tcode=3 then ti2=1 ! REG.COLLECTION
		if tcode=4 then ti2=2 ! CREDIT MEMO
		if tcode=5 then ti2=3 ! DEBIT MEMO
		if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
		r(1,ti2+1)+=tamount
		x=0
		for j=1 to 10
			if trim$(serviceName$(j))="" then goto L1370
			if j=3 and (trim$(serviceName$(j))<>"Electric" or trim$(serviceName$(j))<>"Lawn Meter") and srv$(j)="EL" then goto L1370 ! electic being used for reduction meter
			if j=4 and trim$(serviceName$(j))<>"Gas" and srv$(j)="GA" then goto L1370 ! gas being used for reduction meter
			alloc(x+=1)=tg(j)
			if ti2=3 then r(x+3,1)-=tg(j) else r(x+3,1)+=tg(j)
			r(x+3,ti2+1)+=tg(j)
			L1370: ! 
		next j
		c$=" "
		if tcode=1 then c$="CHG"
		if tcode=2 then c$="PN"
		if tcode=3 then c$="COL"
		if tcode=4 then c$="CM"
		if tcode=5 then c$="DM"
		service=0
		if water=1 then service+=1: usage(service)=wu ! water
		if electric=1 then service+=1: usage(service)=eu ! Electric
		if gas=1 then service+=1: usage(service)=gu ! Gas
		if print_balance then 
			printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(--------.--),x 1"
			usage(1)=tbal
		else 
			printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(zzzzzzzzzzz),x 1"
		end if 
		if trim$(z$)="[All]" then 
			pr #255,using 'Form POS 1,c 10,x 1,'&printlineform$: p$,c$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
		else 
			pr #255,using 'Form POS 1,'&printlineform$: c$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
		end if  ! trim$(z$)="[All]"   /   else 
		if tcode=1 then mat totalalloc=totalalloc+alloc: totaltamount+=tamount: mat totalusage=totalusage+usage ! charges
		if tcode=2 then mat totalalloc=totalalloc+alloc: totaltamount+=tamount ! penalties
		if tcode=3 then mat totalalloc=totalalloc+alloc: totaltamount+=tamount ! collections
		if tcode=4 then mat totalalloc=totalalloc-alloc: totaltamount-=tamount ! credit memos
		if tcode=5 then mat totalalloc=totalalloc+alloc: totaltamount+=tamount ! debit memos
	loop 
	PGOF: ! r:
		pr #255: newpage
		gosub HDR
	continue  ! /r
	HDR: ! r:
	! need date$,time$
		pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&" }"
		pr #255: "\qc  {\f181 \fs20 \b "&trim$(nam$)&" }"
		pr #255: "\qc  {\f181 \fs20 \b "&trim$(metraddr$)&" }"
		pr #255: "\qc  {\f181 \fs20 \b "&trim$(z$)&" }"
		pr #255: "\qc  {\f181 \fs28 \b Transaction List }"
		if beg_date<>0 and end_date<>0 then 
			pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)& "  To "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
		end if  ! beg_date<>0 and end_date<>0
		pr #255: ""
		pr #255: "\ql "
		pr #255: hd1$
	return  ! /r
	PT_FINIS: ! 
	pr #255,using "form skip 1,pos 10,c 20": "Totals"
	for j=1 to udim(alloc)
		pr #255,using "form pos 1,c 20,pic(---,---,---.##)": name$(j),totalalloc(j)
	next j
	pr #255,using "form pos 1,c 20,pic(---,---,---.##)": "Total Amount",totaltamount
	if water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Water Usage",totalusage(1)
	if electric=1 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(2) ! electric 2nd metered service
	if electric=1 and water=0 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(1) ! electric is 1st metered service
	if gas=1 and electric=1 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(3) ! gas is third service
	if gas=1 and electric=0 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(2) ! gas is second metered service
	if gas=1 and electric=0 and water=0 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(1) ! gas is first  metered service
	pr #255,using "form skip 1,pos 1,cr 18,pic(-,---,---,--#.##)": "Current Balance:",account_balance
	PT_NO_CUSTOMER: ! 
	close #h_customer: ioerr ignore
	close #trans: ioerr ignore
	fncloseprn
	PT_XIT: ! 
fnend
def fn_flextran(myline,mypos; hTrans,z$,begdate,enddate,selcode)
	! ___________________________________________
	dim colmask$(30),colhdr$(30)*20,item$(25)*70,tg(11)
	dim srv$(10)*2,serviceName$(10)*20
	! ______________________________________________________________________
	if hTrans=0 then 
		close_hTrans=1
		open #hTrans:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,input,keyed 
	end if 
	hTrans_lrec_len=len(str$(lrec(hTrans)))
	fn_columnGet(mat colhdr$,mat colmask$,ftShowElecUsed,ftShowGasUsed)
	fn_columnEnabledGet(mat colEnabled) 
	forceAllColumnsOn=0
	if forceAllColumnsOn then mat colEnabled(25) : mat colEnabled=(1)
	if trim$(z$)="[All]" then 
		z$=""
		colEnabled(2)=1
	else if trim$(z$)<>"" then 
		z$=lpad$(trim$(z$),10)
		colEnabled(2)=0
	end if
	dim colHdr_enabled$(0)*20
	dim colMask_enabled$(0)
	colHeaderEnabledCount=0
	for hdrItem=1 to headerCount
		if colEnabled(hdrItem) then
			colHeaderEnabledCount+=1
			mat colHdr_enabled$(colHeaderEnabledCount)
			mat colMask_enabled$(colHeaderEnabledCount)
			colHdr_enabled$(colHeaderEnabledCount)=colhdr$(hdrItem)
			colMask_enabled$(colHeaderEnabledCount)=colmask$(hdrItem)
		end if
	nex hdrItem
	if trim$(z$)='' then 
		restore #hTrans: 
	else 
		restore #hTrans,key>=lpad$(z$,10)&"         ": nokey FlexTranFinis
	end if 
	fnflexinit1("ubtrans_b",myline,mypos,25,100,mat colHdr_enabled$,mat colMask_enabled$,1)
	do
		READ_UBTRANSVB: ! 
		read #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexTranFinis
		if lpad$(p$,10)<>lpad$(z$,10) and trim$(z$)<>'' then goto FlexTranFinis ! .     ! not same account
		if selcode>1 and tcode<>selcode-1 then goto READ_UBTRANSVB
		if begdate>20000000 and tdate<begdate then goto READ_UBTRANSVB
		if enddate>20000000 and tdate>enddate then goto READ_UBTRANSVB
		! if tcode=0 then tcode=1 ! temporary to prevent bad transaction codes
		items=0
		item$(items+=1)=lpad$(str$(rec(hTrans)),hTrans_lrec_len,'0')
		if colEnabled(2) then
			 item$(items+=1)=p$
		end if
		item$(items+=1)=str$(tdate)
		if tcode<1 or tcode>udim(mat transtype$) then 
			item$(items+=1)='(invalid)'
		else
			item$(items+=1)=transtype$(tcode)
		end if
		item$(items+=1)=str$(tamount)
		colEnabledItem=items ! +1
		for j=1 to 10
			if j=3 and ftShowElecUsed=1 then goto L440
			if j=4 and ftShowGasUsed=1 then goto L440
			if trim$(serviceName$(j))<>"" then 
				if colEnabled(colEnabledItem+=1) then
					! pr colhdr$(colEnabledItem) : pause
					item$(items+=1)=cnvrt$("pic(-------.zz)",tg(j))
				end if 
			end if 
			L440: ! 
		next j
		if colEnabled(colEnabledItem+=1) then
			! pr colhdr$(colEnabledItem) : pause
			item$(items+=1)=cnvrt$("pic(-------.zz)",tg(11)) ! net
		end if
		if trim$(serviceName$(1))<>"" then 
			if colEnabled(colEnabledItem+=1) then
				! pr colhdr$(colEnabledItem) : pause
				item$(items+=1)=str$(wr)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(wu)
			end if
		end if 
		if trim$(serviceName$(3))="Electric" or trim$(srv$(3))="EL" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(er)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(eu)
			end if
		end if 
		if trim$(serviceName$(3))="Lawn Meter" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(er)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(eu)
			end if
		end if 
		if trim$(serviceName$(4))="Gas" or trim$(srv$(4))="GA" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(gr) 
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(gu)
			end if
		end if 
		if colEnabled(colEnabledItem+=1) then
			item$(items+=1)=str$(tbal)
		end if
		fnflexadd1(mat item$) 
	loop
	FlexTranFinis: ! 
	if close_hTrans=1 then close #hTrans: : close_hTrans=0
fnend 
def fn_columnGet(mat colhdr$,mat colmask$,&ftShowElecUsed,&ftShowGasUsed)
	mat colhdr$(30)
	mat colmask$(30)
	colhdr$(1)="Rec"
	colhdr$(2)="Account"
	colhdr$(3)="Date"
	colhdr$(4)="Type"
	colhdr$(5)="Amount"
	colmask$(1)=""
	colmask$(2)=""
	colmask$(3)="3"
	colmask$(4)=""
	colmask$(5)="10"
	headerCount=5
	if trim$(serviceName$(3))<>"Electric" and srv$(3)="EL" then ftShowElecUsed=1
	if trim$(serviceName$(4))<>"Gas" and srv$(4)="GA" then ftShowGasUsed=1
	for j=1 to 10
		if j=3 and ftShowElecUsed=1 then goto L220
		if j=4 and ftShowGasUsed=1 then goto L220
		if trim$(serviceName$(j))<>"" then 
			colhdr$(headerCount+=1)=trim$(serviceName$(j))(1:min(8,len(trim$(serviceName$(j)))))
			colmask$(headerCount)="10"
		end if 
		L220: ! 
	next j
	colhdr$(headerCount+=1)="Net" : colmask$(headerCount)="10"
	for j=1 to 4
		if trim$(serviceName$(j))<>"" and j=1 then 
			colhdr$(headerCount+=1)="Water Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Water Used"
			colmask$(headerCount)="20"
		end if 
		if trim$(serviceName$(j))="Electric" and j=3 then 
			colhdr$(headerCount+=1)="Elec Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Elec Used"
			colmask$(headerCount)="20"
		else if trim$(srv$(j))="EL" and j=3 then 
			colhdr$(headerCount+=1)=" 2nd Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)=" 2nd Used"
			colmask$(headerCount)="20"
		end if 
		if trim$(serviceName$(j))="Lawn Meter" and j=3 then 
			colhdr$(headerCount+=1)="Lawn Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Lawn Used"
			colmask$(headerCount)="20"
		end if 
		if uprc$(trim$(serviceName$(j)))="GAS" and j=4 then 
			colhdr$(headerCount+=1)="Gas Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Gas Used"
			colmask$(headerCount)="20"
		else if uprc$(trim$(srv$(j)))="GA" and j=4 then 
			colhdr$(headerCount+=1)="3nd Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="3rd Used"
			colmask$(headerCount)="20"
		end if 
	next j
	colhdr$(headerCount+=1)="Balance"
	colmask$(headerCount)="10"
	mat colhdr$(headerCount)
	mat colmask$(headerCount)
fnend
def fn_columnEnabledGet(mat colenabled) ! requires local: headerCount
	mat colenabled(headerCount)
	mat colenabled=(0)
	for hdrItem=1 to 5
		colenabled(hdrItem)=1
	nex hdrItem
	for hdrItem=6 to headerCount
		fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',tmp$,'True')
		! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&tmp$
		if tmp$='True' then 
			colenabled(hdrItem)=1
		end if
	nex hdrItem
! pause
fnend
def fn_columnSelect
	dim csHeader$(30)*20
	fnTos(sn$='ubTrColSel') : respc=0 : csLine=0
	fn_columnGet(mat csHeader$,mat unusedColMask$,unusedShowElecUsed,unusedShowGasUsed)
	for hdrItem=6 to udim(mat csHeader$)
		fnChk(csLine+=1,25,csHeader$(hdrItem), 1)
		fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1),'True')
		! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(respc)
	nex hdrItem
! pause
	fnCmdSet(4)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey<>5 then
		respc=0
		for hdrItem=6 to udim(mat csHeader$)
			fncreg_write('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1))
			! pr 'wrote: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(hdrItem)
		nex hdrItem
	end if
! pause
fnend
