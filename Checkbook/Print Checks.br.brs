autoLibrary
on error goto Ertn
fnTop(program$)
! r: dims, opens, set constants and defaults (i.e. prd)
	dim amt(15,3),iv$(15,3)
	dim de2$(15,3)*13,ivdate(15,3)
	
	dim de$*50,allocde$*30
	dim in3$(150)*30,bn$*30,b$(4)*30
	dim ade$*30,agl(3)
	dim t1(5),arec(100)
	dim pr$(4)*30,myact$*20
	dim resp$(92)*50
	dim contact$*30,email$*50
	dim inl$(4)*50,ml$(5)*90
	dim allockey$*20
	dim eng$*128,wording$(27)*9,amount(11)
	dim tr$(5)*35,sn$*30,dtr$(5)*35,payeegl$*12,gldesc$*30
	
	prd=val(date$(4:5)&date$(7:8)&date$(1:2))
	open #bankmstr:=12: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',i,outIn,k
	open #h_paymstr1:=13: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,outIn,k
	open #paymstr2:=14: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr',i,outIn,k
	open #trmstr1:=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,outIn,k
	open #trmstr2:=2: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr',i,outIn,k
	open #tralloc:=3: 'Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\tralloc-idx.h[cno],Shr',i,outIn,k
	open #hPayTrans:=4: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,outIn,k
	open #hUnPdAlloc:=fnH: 'Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\uaidx2.h[cno],Shr',i,outIn,k
	open #hGlAccount:=fnH: 'Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr',i,outIn,k
	open #hGlControl:=fnH: 'Name=[Q]\CLmstr\Fundmstr.h[cno],KFName=[Q]\CLmstr\Fundidx1.h[cno],Shr',i,i,k
	open #ivpaid:=fnH: 'Name=[Q]\CLmstr\IvPaid.h[cno],KFName=[Q]\CLmstr\IVIndex.h[cno],Shr',i,outIn,k ! #6
	open #payeegl:=fnH: 'Name=[Q]\CLmstr\payeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Shr',i,outIn,k ! 17
	! r: get_coinfo
		open #company=fnH: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,outi,r
		dim whgl(5,3)
		dim dedcode(10)
		dim xd(2)
		dim misc$(10)*20
		dim miscgl$(10)*12
		read #company,using 'form pos 150,2*N 1,N 2,pos 418,10*C 20,pos 668,10*C 12,pos 298,15*PD 4,pos 618,10*N 1,pos 406,N 1',rec=1,release: mat xd,bankcode ,mat misc$,mat miscgl$,mat whgl,mat dedcode,prenum
		method$='C' ! temporary kJ  ! Read #COMPANY,Using 'form pos 789,c 1',Rec=1,Release: method$
		close #company:
		do
			read #hPayTrans,using 'form pos 63,N 10.2,N 1',release: upa,pcde eof EO_PAYTRANS_1
			if pcde=1 then t1(2)+=upa else t1(4)+=upa
			t1(5)+=upa
		loop
		EO_PAYTRANS_1: !
	! /r
! /r
MENU1: ! r: main loop
	dim lcn$*8
	read #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
	t1(1)=bal
	upi=t1(5)
	t1(3)=t1(1)-t1(2)
	ckn=val(lcn$)+1 conv ignore
	bn$=rtrm$(bn$)
MAIN_QUESTIONS: !
	if fn_scr_main_questions=5 then goto Xit

	tac=0
	h_vf1=h_paymstr1
	if ckoption=1 then h_vf1=13
	allign=0
	if ckoption=3 then goto ReprintChecks
	open #company=fnH: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,outi,r
	rewrite #company,using 'form pos 152,N 2',rec=1: bankcode
	close #company:
	read #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
	mat in3$=('')
	bn$=rtrm$(bn$)
	if ckoption=1 or ckoption=3 then goto CKOPTION1_CHECK_ENTRY
	restore #hPayTrans,key>=lpad$(rtrm$(begvn$),8)&'            ': nokey MENU1
	amt=arec=x=y=0
	mat amt=(0) : mat de2$=('')
READ_4: !
	dim vn$*8
	dim disamt(15,3)
	dim up$(4)
	read #hPayTrans,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,n 1,n 6,n 10.2,n 8',release: vn$,iv$,mat up$,upa,pcde,bc,checkNumber$,dp,gde,pdate,disamt,ddate eof EOF_ROUTINE
	vn$=lpad$(rtrm$(vn$),8)
	if rtrm$(vn$)='' then goto READ_4
	if pcde=0 then goto READ_4
	if bc=0 then bc=bankcode ! if coded for payment with no bank code, pay from the  current bank account
	if bc<>bankcode then goto READ_4
	lr4=rec(hPayTrans) ! what to do
	dim holdvn$*8
	if holdvn$=vn$ or rtrm$(holdvn$)='' then 
		goto L1130 
	else 
		gosub SubPrintCheck
	end if
	L1130: !
	if arec>30 then gosub SubPrintCheck
	fn_checkDiscount
	if iv$<>hiv$ then
		y=y+1
		if y>2 then y=1
		if y=1 then x+=1
		if x>15 then gosub SubPrintCheck
		iv$(x,y)=iv$(1:12)
		de2$(x,y)=up$(4)(1:13) ! this one for printing from unpaid file
	end if
	amt(x,y)=amt(x,y)+upa
	ivdate(x,y)=dp
	disamt(x,y)=disamt
	arec+=1
	arec(arec)=lr4
	holdvn$=vn$
	hiv$=iv$
	amt=sum(amt)
	st1=1
goto READ_4
EOF_ROUTINE: !
	if st1=1 then gosub SubPrintCheck
	fnClosePrn
	mat amt=(0) : mat de2$=('') : mat iv$=('') : mat de2$=('') : x=y=1
	st1=0 : holdvn$=''
goto ScrFinal ! /r

SubPrintCheck: ! r:
	fn_cknum
	fnOpenPrn
	ckn1=ckn
	if amt<=0 then goto L2420
	if scc$='CSS' then fn_portion_check : fn_portion_stub(1) : fn_portion_stub(2)
	if scc$='SCS' then fn_portion_stub(1) : fn_portion_check : fn_portion_stub(2)
	if scc$='SSC' then fn_portion_stub(1) : fn_portion_stub(2) : fn_portion_check
	if scc$='SCC' then fn_portion_stub(1) : fn_portion_check : fn_portion_check
	gosub UpdateInvoice
return ! /r

UpdateInvoice: ! r:
	for j=1 to arec
		rewrite #hPayTrans,using 'form pos 76,N 8,N 6',rec=arec(j): ckn,prdmmddyy
	next j
	idx=1
	if allign=3 then pr #255: newpage : goto ALIGN_COMPLETED
	pr #255: newpage
	fnClosePrn
	! if env$('client')='Washington Parrish' then fnProcess(0)
	dim holdpayee$*50
	holdpayee$=''
	if ckoption=1 then allign=2 : goto L2300 ! skip the continue routine when entering and printing checks
	if ~allign then
		if ckoption=1 or ckoption=3 then
			mat inl$(4)
			inl$(4)='4. Void previous check    '
		else if ckoption=2 then
			mat inl$(3)
		end if
		inl$(1)='1. Reprint the same check    '
		if ckoption=1 or ckoption=3 then
			inl$(2)='2. Continue with next check    '
		else
			inl$(2)='2. Print next check and Stop   '
		end if
		if ckoption=1 or ckoption=3 then
			inl$(3)='3. Completed with checks'
		else
			inl$(3)='3. Print All remaining checks  '
		end if
	end if  ! ~allign
SCR_CKPRT7: !
	fnTos
	respc=0
	fnLbl(1,1,'',40,0)
	fnLbl(1,1,'Print Options:',38,0)
	fnOpt(2,3,inl$(1),0)
	resp$(respc+=1)='False'
	fnOpt(3,3,inl$(2),0)
	resp$(respc+=1)='True' !  if ckoption=1 or ckoption=3 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(4,3,inl$(3),0)
	if ckoption=2 then
		resp$(respc+=1)='True'
	else
		resp$(respc+=1)='False'
		if trim$(inl$(4))<>"" then
			fnOpt(5,3,inl$(4),0)
			resp$(respc+=1)='False'
		end if
	end if
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if (ckey=5 or ckey=99) and ckoption=1 then fn_write_ck_hist_1 : goto MENU1
	if (ckey=5 or cmdkey=99) then fn_write_history : goto Finis
	for j=1 to 4
		if resp$(j)(1:1)="T" then allign=j : goto L2300
	next j
	L2300: !
	if ckoption=1 and allign=2 then fn_write_ck_hist_1 ! write regular check history if not a reprint
	! L2310: !
	if ckoption=1 and allign=3 then fn_write_ck_hist_1 !  write regular check history
	on allign goto ALIGN_REPR_SAME,ALIGN_PRINT_NEXT,ALIGN_COMPLETED,ALIGN_PRINT_NEXT none SCR_CKPRT7

	ALIGN_REPR_SAME: !
	if prenum=1 then
		write #trmstr1,using 'form pos 1,N 2,N 1,G 8,g 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",1,dp,1
		ckn=ckn+1
		tr$(1)=str$(ckn)
	end if
	goto SubPrintCheck
	ALIGN_COMPLETED: !
	if ckoption=3 and allign=3 then fn_write_history : goto Finis
	ALIGN_PRINT_NEXT: !
	if allign=4 and prenum=1 then
		write #trmstr1,using 'form pos 1,N 2,N 1,G 8,g 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",0,dp,1
	end if
	ckn=ckn+1
	goto L2420
	L2420: !
	mat iv$=('') : mat de2$=('')
	mat amt=(0) : mat de2$=('')
	amt=arec=x=y=0
	x=y=1
	st1=0
	holdvn$=vn$
	tr$(3)=tr$(4)=tr$(5)=''
	hiv$=''
	tac=0
	if (ckoption=1 or ckoption=3) and allign=3 then goto MENU1
return ! /r

ScrFinal: ! r: (reprint or transfer to history)
	fnTos
	respc=0
	
	fnLbl(1,1,'Reprint Options:',16,1)
	fnOpt(1,18,'Reprint Checks',0)
	resp$(respc+=1)='False'
	fnOpt(2,18,'Transfer to Check History',0)
	resp$(respc+=1)='False'

	! fnLbl(1,1,'Reprint Options:',38)
	! dim item5$(2)*35
	! item5$(1)='Reprint Checks'
	! item5$(2)='Transfer to Check History'
	! fnComboA('ckprt-cmb1',1,40,mat item5$,tt$)
	! resp$(respc+=1)=item5$(2)
	fnCmdSet(41)
	ckey=fnAcs(mat resp$)
	if resp$(1)='True' then ti2=1 else ti2=2
	allign=0
	if ti2=1 then 
		goto ScrReprint
	else if ti2=2 then
		fn_write_history
		goto Finis
	else 
		goto ScrFinal
	end if
! /r
ScrReprint: ! r: (Reprint Options)
	fnTos
	respc=0
	fnLbl(1,1,'Reprint Options:',17)
	dim item4$(2)*35
	item4$(1)='Reprint all checks'
	item4$(2)='Begin with specific Payee'
	fnComboA('ckprt-cmb2',1,19,mat item4$,tt$)
	resp$(respc+=1)=item4$(1)
	fnLbl(3,1,'Starting Payee:',17)
	fnComboF('Paymstr',3,19,0,'[Q]\CLmstr\PayMstr.h[cno]',1,8,9,30,'[Q]\CLmstr\Payidx1.h[cno]',0,pas, 'Enter the beginning payee number if you wish to only reprint part of the checks')
	resp$(respc+=1)=holdpayee$
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)=item4$(1) then ti2=1 else ti2=2
	begvn$=resp$(2)(1:8) ! beginning payee to start reprint
	if ti2=1 then ! REPRINT ALL
		restore #hPayTrans,key>='                    ':
	else if ti2=2 then ! reprint beginning with specific payee
		restore #hPayTrans,key>=lpad$(rtrm$(begvn$),8)&'            ': nokey ScrReprint
	end if
	hiv$='' : de2$=''
goto MAIN_QUESTIONS
! /r
Finis: ! r: COMPLETE
	fn_close(trmstr1)
	fn_close(trmstr2)
	fn_close(tralloc)
	fn_close(hPayTrans)
	fn_close(ivpaid)
	fn_close(bankmstr)
	fn_close(h_paymstr1)
	fn_close(company)
	if idx then fn_index
	goto Xit ! /r
def fn_close(&h_closeme)
	close #h_closeme: ioerr ignore
	h_closeme=0
fnend
Xit: fnXit

CKOPTION1_CHECK_ENTRY: ! r:
	mat resp$=('')
CKOPTION1_CHECK_ENTRY_2: !
	ckey=fn_scr_check_entry
	if ckey=5 then
		goto Xit
	else if ckey=20 or ckey=21 then
		goto ASSIGN_SCREENS
	else
		goto STORE_GL_BREAKDOWNS
	end if
! /r
STORE_GL_BREAKDOWNS: ! r: store general ledger breakdowns
	x=0 : tac=0
	dim holdresp$(94)*50
	for j=1 to 146 step 5
		x=x+3
		if x=33 then x=35 ! skip check# and date (resp$(33)&34
		in3$(j)=holdresp$(x)(1:3) ! gl$(1)
		in3$(j+1)=holdresp$(x)(4:9) ! gl$(2)
		in3$(j+2)=holdresp$(x)(10:12) ! gl$(3)
		in3$(j+3)=holdresp$(x+1) ! amount
		in3$(j+4)=holdresp$(x+2)(1:30) ! description  kj 081507
		tac+=val(in3$(j+3))
	next j
	if ckey=20 or ckey=21 then
		goto ASSIGN_SCREENS
	else
		screen=0
		if ckey<>2 then goto PrintRegularChecks ! skip automatic allocation
		fn_read_standard_breakdowns
		goto CKOPTION1_CHECK_ENTRY_2
	end if
! /r
ASSIGN_SCREENS: ! r: assign screen # based on more and back options
	if screen=0 then screen=1
	if ckey=20 and screen=1 then screen=2 : goto L5070
	if ckey=20 and screen=2 then screen=3 : goto L5070
	if ckey=20 and screen=3 then screen=3 : goto L5070 ! shouldn't happen
	if ckey=21 and screen=2 then screen=1 : goto L5070
	if ckey=21 and screen=1 then screen=1 : goto L5070 ! shouldn't happen
	if ckey=21 and screen=3 then screen=2 : goto L5070
	L5070: !
	for j=1 to 30
		if screen=1 then x=j+2
		if screen=2 then x=j+34
		if screen=3 then x=j+64
		if int(j+2/3)=(j+2/3) then resp$(j+2)=holdresp$(x) else resp$(j+2)=holdresp$(x)
	next j
	goto CKOPTION1_CHECK_ENTRY_2 ! /r

PrintRegularChecks: ! r:
	fn_cknum
	amt=arec=x=y=0
	mat amt=(0) : mat de2$=('')
	if tac<>val(tr$(3)) then fn_msg_allocations_off : goto CKOPTION1_CHECK_ENTRY_2 ! ALLOCATIONS NOT EQUAL
	for j=1 to 30 ! kj was 10
		! if in3$(j*5)=hiv$ and rtrm$(hiv$)<>'' then goto L5460
		if in3$(j*5)<>hiv$ or rtrm$(hiv$)='' then
			y+=1: if y>2 then y=1
			if y=1 then x+=1
			de2$(x,y)=in3$(j*5)(1:13)
		end if
		! L5460: !
		amt(x,y)=amt(x,y)+val(in3$(j*5-1))
		hiv$=in3$(j*5)(1:15)
	next j
	amt=val(tr$(3)) : vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
	gosub SubPrintCheck
	mat holdresp$=('')
	goto CKOPTION1_CHECK_ENTRY
! /r
ReprintChecks: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'First Check Number to Reprint:',38,1)
	fnTxt(1,40,8,0,1,'30',0,'')
	resp$(respc+=1)=str$(firstckn)
	fnLbl(2,1,'Last Check Number to Reprint:',38,1)
	fnTxt(2,40,8,0,1,'30',0,'')
	resp$(respc+=1)=str$(lastckn)
	if reprintckn>0 then fnLbl(4,1,'Last Check Number Reprinted '&str$(reprintckn)&':',38,1)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	firstckn=ckn1=reprintckn=val(resp$(1))
	lastckn=val(resp$(2)) : if lastckn=0 then lastckn=firstckn
	if lastckn>0 and lastckn<firstckn then goto ReprintChecks ! smaller lastckn
	ReprintChecksLoopTop: !
		check_ref$=cnvrt$('pic(ZZ)',bankcode)&str$(1)&cnvrt$('n 8',reprintckn)
		read #trmstr1,using 'form pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey L7780
		!	pr 'key=';check_ref$ : pause
		prdmmddyy=val(tr$(2)) ! use old check date
		vn$=lpad$(trim$(tr$(4)),8)
		amt=tr3 ! set amount for check
		mat amt=(0) : mat de2$=('') : mat iv$=('') : x=1: y=0
		st1=0 : holdvn$='        ': amt=0
		vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
	goto L7790
	L7780: !
		if firstckn=reprintckn then goto L7785
		if reprintckn>=lastckn then goto L7970 ! complete
		mat ml$(2)
		ml$(1)='Could not locate check number '&str$(reprintckn)&' for bank number '&str$(bankcode)&'.'
		ml$(2)='This check will be skipped.'
		reprintckn+=1
		fnMsgBox(mat ml$,resp$)
	goto ReprintChecksLoopTop
	L7785: !
		mat ml$(2)
		ml$(1)='Cannot locate the first check (number '&str$(reprintckn)&' for bank number '&str$(bankcode)&'.)'
		ml$(2)='You must choose another check number.'
		fnMsgBox(mat ml$,resp$)
	goto ReprintChecks
	L7790: !
	key$=cnvrt$('pic(ZZ)',bankcode)&str$(1)&cnvrt$('n 8',reprintckn)
	restore #tralloc,key>=key$: nokey L7780
	do
		READ_DETAILS: !
		dim gl$*12
		read #tralloc,using 'form pos 1,N 2,N 1,n 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': transbankcode,tcode,transckn,gl$,alloc,allocde$,allocdate conv L7815,eof L7910
		goto L7820
		L7815: !
		reread #tralloc,using 'form pos 1,c 2': x$ eof L7910 : goto READ_DETAILS
		L7820: !
		if transbankcode><bankcode or tcode<>1 or transckn<>reprintckn then goto L7910
		y=y+1: if y>2 then y=1
		if y=1 then x+=1
		if x>15 then gosub SubPrintCheck
		iv$(x,y)=cnvrt$('pic(zzzzzz)',allocdate) ! IV$(1:12)
		de2$(x,y)=allocde$(1:13) ! this one for printing from unpaid file
		amt(x,y)=amt(x,y)+alloc
		ivdate(x,y)=0
		disamt(x,y)=0 ! already out of net check
	loop
	L7910: !
	fnOpenPrn
	ckn1=reprintckn: amt=tr3
	if scc$='CSS' then fn_portion_check   : fn_portion_stub(1) : fn_portion_stub(2)
	! if scc$='CSS' then fn_portion_check   : fn_portion_stub(1) : fn_portion_stub(2) ! reprint checks does it double for no reason!?
	if scc$='SCS' then fn_portion_stub(1) : fn_portion_check   : fn_portion_stub(2)
	if scc$='SSC' then fn_portion_stub(1) : fn_portion_stub(2) : fn_portion_check
	if scc$='SCC' then fn_portion_stub(1) : fn_portion_check    : fn_portion_check
	if lastckn>0 and reprintckn<lastckn then reprintckn+=1 : pr #255: newpage : goto ReprintChecksLoopTop
	L7970: !
		fnClosePrn
		if firstckn<>lastckn then goto Xit
	goto ReprintChecks ! /r

def fn_scr_check_entry
	fnTos
	respc=0
	fnFra(1,1,6,87,'Check',' ')
	fnLbl(1,1,env$('cnam'),40,2,0,1)
	fnLbl(2,1,bn$,40,2,0,1)
	fnLbl(3,55,'Amount:',10,1,0,1)
	fnTxt(3,67,12,0,1,'10',0,'',1)
	resp$(respc+=1)=tr$(3)
	fnLbl(5,1,'Payee:',8,1,0,1)
	fnComboF('Paymstr',5,10,30,'[Q]\CLmstr\PayMstr.h[cno]',1,8,9,30,'[Q]\CLmstr\Payidx1.h[cno]',0,pas, 'Enter the payee number or simply enter the payee name if no vendor record exits',1)
	resp$(respc+=1)=holdpayee$
	fnFra(9,1,12,96,'Breakdown Information',' ')
	fnLbl(1,1,'General Ledger',30,0,0,2)
	fnLbl(1,41,'Amount             Description',12,0,0,2)
	fnLbl(1,56,'Description',30,0,0,2)
	for j=1 to 10
		fnQgl(j+1,1,2,2)
		resp$(respc+=1)=fnrgl$(resp$(respc))
		!
		fnTxt(j+1,41,12,0,1,'currency',0,'',2)
		resp$(respc+=1)=resp$(respc)
		!
		fnTxt(j+1,56,30,0,0,'',0,'',2)
		resp$(respc+=1)=resp$(respc)
		!
	next j
	if screen=2 or screen=3 then
		fnButton(12,74,'Back',21,'Previous breakdown screen',1,4,2)
	end if
	if screen=0 or screen=1 or screen=2 then
		fnButton(12,82,'More',20,'Allows another screen of breakdowns',1,4,2)
	end if
	pas=1 ! don't redo combo boxes
	fnLbl(1,45,'Check Number:',15,1,0,1)
	fnTxt(1,62,8,0,1,'30',0,'',1)
	resp$(respc+=1)=str$(ckn)
	fnLbl(3,30,'Check Date:',12,1,0,1)
	fnTxt(3,44,10,0,1,'3',0,'',1)
	resp$(respc+=1)=str$(prd)
	fnButton(5,52,'Add Payee',50,'Click to add a new payee record',0,0,1)
	fnCmdKey('Print',1,1,0,'Prnt this check and advance to next check')
	fnCmdKey('&Allocate',2,0,0,'Automatically allocates the general ledger breakdown if payee record contains the breakdown information')
	fnCmdKey('&Complete',5,0,1,'Return to menu.')
	! need a fnCmdKey to change screens for the breakdowns  (screen 1,2 or 3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then screen=0 : goto SCE_XIT
	!
	for j=3 to 30, step 3
		resp$(j)=fnagl$(resp$(j))
	next j
	!
	if ckey=50 then fn_payee_add : goto CKOPTION1_CHECK_ENTRY_2
	tr$(3)=resp$(1) ! amount
	vn$=tr$(4)=lpad$(rtrm$(resp$(2)(1:8)),8) ! payee number
	read #h_paymstr1,using 'form pos 1,c 8',key=vn$,release: vn$ nokey SCE_L4640
	tr$(5)=resp$(2)(9:30) ! payee name
	goto SCE_L4650
	SCE_L4640: !
	tr$(5)=resp$(2)(1:30)
	vn$=tr$(4)='': b$(1)=tr$(5) ! payee name without vendor record
	SCE_L4650: !
	ckn=val(resp$(33)) ! ckey number
	holdpayee$=resp$(2)
	prd=val(resp$(34)): ! date
	prdmmddyy=val(resp$(34)(5:6))*10000+val(resp$(34)(7:8))*100+val(resp$(34)(3:4)) ! convert date back to mmddyy format
	tr$(2)=cnvrt$('pic(######)',prdmmddyy)
	! STORE_RESPONSES: ! hold all 94 possible responses in holdresp$
	x=0
	if screen=0 then screen=1
	for j=1 to 34
		if j=1 then holdresp$(j)=resp$(j): goto SCE_L4820 ! amount
		if j=2 then holdresp$(j)=resp$(j): goto SCE_L4820 ! vendor
		if j=33 then holdresp$(j)=resp$(j): goto SCE_L4820 ! checknumber
		if j=34 then holdresp$(j)=resp$(j): goto SCE_L4820 ! date
		if screen=1 then x=j ! (3-32)
		if screen=2 then x=j+32 ! (35-64)
		if screen=3 then x=j+62 ! (65-94)
		if int(j+2/3)=(j+2/3) then
			holdresp$(x)=resp$(j)
		else
			holdresp$(x)=resp$(j) ! hold all general ledger breakdowns
		end if
	SCE_L4820: !
	next j
	SCE_XIT: !
	fn_scr_check_entry=ckey
fnend
def fn_read_standard_breakdowns ! pull standard gl breakdowns from payee file
	dim fax$*12
	read #h_paymstr1,using 'form pos 1,C 8,4*C 30,PD 5.2,N 2,C 11,X 6,C 12,C 30,C 50,C 12,C 20',key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey RSB_XIT
	mat holdresp$=('')
	restore #payeegl,key>=vn$: nokey RSB_EO_READSTGL
	totalalloc=0
	for j=3 to 92 step 3
		if j=33 or j=34 then goto RSB_L5310 ! skip ckey num and date  (resp$(33)&34)
	RSB_L5240: !
		read #payeegl,using 'form pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,gldesc$ eof RSB_EO_READSTGL
		if vn$<>payeekey$ then goto RSB_EO_READSTGL
		if trim$(payeegl$)='' or payeegl$='  0     0   0' then goto RSB_L5310
		read #hGlAccount,using 'form pos 13,C 30',key=payeegl$,release: de$ nokey RSB_L5240
		resp$(j)=payeegl$
		resp$(j+1)=str$(round(val(tr$(3))*percent*.01,2))
		totalalloc+=val(resp$(j+1))
		resp$(j+2)=gldesc$ ! description
	RSB_L5310: !
	next j
	RSB_EO_READSTGL: !
	if val(tr$(3))<>totalalloc then
		resp$(4)=str$(val(resp$(4))+val(tr$(3))-totalalloc)
	end if
	RSB_XIT: !
fnend
def fn_write_history
	holdvn$=''
	hck=0
	fn_close(hPayTrans:=4)
	open #hPayTrans=fnH: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr',i,outIn,k
	WH_LOOP_TOP: !
	read #hPayTrans,using 'form pos 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ckpay,dp,gde eof WH_XIT
	if gde=1 then gde=0 ! dont allow posting code of 1 from unpaid file
	if upa=0 then goto WH_L3910
	if dp=0 and ckpay=0 then goto WH_LOOP_TOP
	if bc=0 then bc=bankcode ! if coded for payment without bank code, pay from the current bank account
	if bc<>bankcode then goto WH_LOOP_TOP
	iv$=rpad$(ltrm$(iv$),12)
	read #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8',key=vn$&iv$,release: vn$ nokey WH_L3650
	goto WH_L3660
	WH_L3650: !
	write #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8': vn$,iv$,dp,ckpay
	WH_L3660: !
	if vn$=holdvn$ and hck=ckpay then goto WH_L3770
	dim tr(2)
	mat tr=(0)
	totalupa=0
	vn$=lpad$(rtrm$(vn$),8)
	read #h_paymstr1,using 'form pos 9,c 30',key=vn$,release: b$(1) nokey WH_L3740 ! PAYEE FILE
	if ltrm$(vn$)(1:2)="T-" then
		delete #h_paymstr1,key=vn$: nokey ignore
	end if  ! ltrm$(vn$)(1:2)="T-"
	WH_L3740: !
	if holdvn$<>vn$ or (hck<>ckey and hck>0) then
		write #trmstr1,using 'form pos 1,N 2,N 1,G 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bc,1,ckpay,dp,upa,vn$,b$(1),0,0,1
	end if  ! holdvn$<>vn$ or (hck<>ckey and hck>0)
	holdvn$=vn$
	hck=ckey
	WH_L3770: !
	read #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WH_L3800
	bal=bal-upa
	rewrite #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckpay nokey ignore ! WH_L3800
	WH_L3800: ! form pos 1,n 2,n 1,g 8,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
	restore #hUnPdAlloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_EO_UNPDALOC
	do
		! WH_L3820: !
		read #hUnPdAlloc,using 'form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$ eof WH_EO_UNPDALOC
		if trim$(allockey$(1:8))<>trim$(vn$) or trim$(allockey$(9:20))<>trim$(iv$) then goto WH_EO_UNPDALOC ! if ALLOCKEY$<>VN$&IV$ Then Goto 3690
		! if sum(agl)=0 and aamt=0 then goto WH_L3820 ! don't allow zero allocations to write
		if sum(mat agl) or aamt then ! don't allow zero allocations to write
			write #tralloc,using 'form pos 1,N 2,N 1,G 8,G 3,G 6,G 3,PD 5.2,C 30,G 6,x 3,C 12,N 1': bc,1,ckpay,mat agl,aamt,ltrm$(rtrm$(iv$))&' '&ade$(1:17),up$(1),up$(3),gde
			! hOLDVN$=VN$
			hck=ckpay
			totalupa+=aamt
		end if
	loop ! 	goto WH_L3820
	WH_EO_UNPDALOC: !
	rewrite #trmstr1,using 'form pos 18,pd 10.2',key=lpad$(rtrm$(str$(bc)),2)&'1'&lpad$(rtrm$(str$(ckpay)),8): totalupa
	WH_L3910: !
	delete #hPayTrans:
	do
		delete #hUnPdAlloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_L3940
	loop
	WH_L3940: !
	goto WH_LOOP_TOP
	WH_XIT: !
fnend
def fn_englishdollar(dolamt) ! returns eng$
	if ~setup_englishdollar then
		setup_englishdollar=1
		data One,Two,Three,Four,Five,Six,Seven,Eight,Nine,Ten,Eleven,Twelve
		data Thirteen,Fourteen,Fifteen,Sixteen,Seventeen,Eighteen,Nineteen
		data Twenty,Thirty,Forty,Fifty,Sixty,Seventy,Eighty,Ninety
		read mat wording$
	end if  ! ~setup_englishdollar
	dol=dolamt ! ENGLISH DOLLAR ROUTINE
	n=64
	if dol<1000000 and dol>=0 then goto L2760
	eng$='Value too big for editing or was less than zero'
	goto ENGLISHDOLLAR_XIT

	L2760: !
	eng$='***'
	amount(1)=int(dol*100+.500000001)
	for a0=2 to 10
		amount(a0)=int(amount(a0-1)/10+.000000001)
	next a0
	for a0=1 to 10
		amount(a0)=amount(a0)-amount(a0+1)*10
	next a0
	if amount(11)+amount(10)+amount(9)=0 then goto L2880
	a0=9
	gosub ENGLISHDOLLAR_L3190
	eng$=rtrm$(eng$)&' Million'
	L2880: if amount(8)+amount(7)+amount(6)=0 then goto L2920
	a0=6
	gosub ENGLISHDOLLAR_L3190
	eng$=rtrm$(eng$)&' Thousand'
	L2920: !
	if amount(5)+amount(4)+amount(3)=0 then goto L2950
	a0=3
	gosub ENGLISHDOLLAR_L3190
	L2950: !
	if dol>=1 then goto L2970
	eng$=rtrm$(eng$)&' Zero'
	L2970: !
	eng$=rtrm$(eng$)&' Dollar'
	if dol<2 and dol>=1 then goto L3010
	eng$=rtrm$(eng$)&'s'
	if len(rtrm$(eng$))>64 then goto L3010
	L3010: !
	eng$=rtrm$(eng$)&' and'
	if amount(2)+amount(1)=0 then goto L3080
	amount(3)=0
	a0=1
	gosub ENGLISHDOLLAR_L3190
	goto L3090

	L3080: !
	eng$=rtrm$(eng$)&' Zero'
	L3090: !
	eng$=rtrm$(eng$)&' Cent'
	if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3120
	eng$=rtrm$(eng$)&'s'
	L3120: !
	if len(rtrm$(eng$))<64 then goto L3170
	for j=1 to 9
		n=65-j
		if eng$(n:n)=' ' then goto L3170
	next j
	L3170: !
	goto ENGLISHDOLLAR_XIT

	ENGLISHDOLLAR_L3190: !
		if amount(a0+2)=0 then goto L3210
		eng$=rtrm$(eng$)&' '&wording$(amount(a0+2))
		eng$=rtrm$(eng$)&' Hundred'
		L3210: if amount(a0+1)=0 and amount(a0)=0 then goto ED_L3190_XIT
		if amount(a0+1)<2 then goto L3260
		eng$=rtrm$(eng$)&' '&wording$(amount(a0+1)+18)
		if amount(a0)=0 then goto ED_L3190_XIT
		amount(a0+1)=0
		L3260: eng$=rtrm$(eng$)&' '&wording$(amount(a0+1)*10+amount(a0))
		ED_L3190_XIT: !
	return  ! ENGLISHDOLLAR_L3190
	ENGLISHDOLLAR_XIT: !
fnend
def fn_msg_allocations_off
	mat ml$(3)
	ml$(1)='The net check ('&tr$(3)&') must agree with the total'
	ml$(2)='allocations ('&str$(tac)&').  Correct the allocation'
	ml$(3)='amounts or the net check to proceed.'
	fnMsgBox(mat ml$,resp$,'',16)
fnend
def fn_checkDiscount ! check for any discounts
	if disamt and ddate=>prd then
		upa-=disamt
		rewrite #hPayTrans,using 'form pos 63,n 10.2,pos 97,n 10.2',rec=lr4: upa,0 ! subtract discount, rewrite new unpaid invoice amount and zero discount amt
		restore #hUnPdAlloc,key>=vn$&iv$: nokey DiscountFinis ! get the fund # from 1st g/l in the allocation file.
		read #hUnPdAlloc,using 'form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$
		if trim$(allockey$(1:8))=trim$(vn$) and trim$(allockey$(9:20))=trim$(iv$) then fundkey$=cnvrt$('pic(ZZZ)',agl(1)) else fundkey$='   '
		apgl$=discountgl$=''
		read #hGlControl, using 'form pos 52,c 12,pos 64,c 12',key=fundkey$: apgl$,discountgl$ nokey DiscountGlControlNoKey
		DiscountWrite: !
		write #hUnPdAlloc,using 'form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,discountgl$,-disamt,'Discount=$'&str$(disamt)
		! create an entry on the unpaid allocation file to record the discount
		if method$='A' and pcd>0 then 
			! create an entry in the unpaid allocation file to record the reduction in accounts payable if accrued and posted
			write #hUnPdAlloc,using 'form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,apgl$,disamt,'Discount=$'&str$(disamt)
		end if
	end if
	goto DiscountFinis
	DiscountGlControlNoKey: !
		mat ml$(5)
		ml$(1)='Invoice '&trim$(iv$)&' on payee '&trim$(vn$)&' quallifies for a discount of '&trim$(cnvrt$('pic($$$$$$.##)',disamt))&','
		ml$(2)='but you have not entered the discount G/L # in the G/L control file.'
		ml$(3)='The discount will be taken, but the entry in check history will not'
		ml$(4)='contain a G/L number.  Fix the GL # in the transaction file and place the '
		ml$(5)='discount G/L #s in the G/L control file.'
		fnMsgBox(mat ml$,resp$,'',16)
	goto DiscountWrite
	DiscountFinis: !
fnend
def fn_write_ck_hist_1 ! WRITE TRANSACTION FOR SINGLE CHECK ENTRY
	mat tr=(0)
	tr$(1)=lpad$(str$(ckn),8)
	tr$(4)=lpad$(rtrm$(tr$(4)),8)
	! k$=lpad$(str$(bankcode),2)&'1'&tr$(1)
	tr$(1)=lpad$(str$(ckn),8)
	tr3=val(tr$(3))
	write #trmstr1,using 'form pos 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,tr$(1),prdmmddyy,tr3,tr$(4),tr$(5),0,clr,1
	read #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WCH1_AFTER_WRITE
	bn$=rtrm$(bn$)
	bal=bal-val(tr$(3)) conv ignore
	rewrite #bankmstr,using 'form pos 3,C 30,pos 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckn nokey WCH1_AFTER_WRITE
	WCH1_AFTER_WRITE: ! k$=LPAD$(RTRM$(STR$(BANKCODE)),2)&LPAD$(STR$(1),1)&LPAD$(TR$(1),8)
	for j=1 to 30
		if val(in3$(j*5-1))<>0 then
			gl$=''
			gl$=cnvrt$('N 3',val(in3$(j*5-4)))&cnvrt$('N 6',val(in3$(j*5-3)))&cnvrt$('N 3',val(in3$(j*5-2)))
			alloc=val(in3$(j*5-1))
			de$=in3$(j*5) ! de$=rtrm$(tr$(5)(1:17))&'-'&in3$(j*5)(1:12)
			tr$(1)=lpad$(str$(ckn),8)
			write #tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': bankcode,1,tr$(1),gl$,alloc,de$,0,'',0
		end if
	next j
	tr$(2)
	mat tr$=('')
	mat in3$=('')
	if ltrm$(vn$)(1:2)='T-' then delete #h_paymstr1,key=vn$: nokey ignore
fnend
def fn_payee_add
	vn$=tr$(4)
	mat pr$=('')
	mat desc$=('')
	mat gl$=('')
	contact$=email$=fax$=myact$=ss$=ph$=''
	fnaddpayee
	pas=0
fnend
def fn_cknum ! CHECK FOR DUPLICATE CHECK NUMBERS
	CKNUM_TOP: ! CHECK FOR DUPLICATE CHECK NUMBERS
	dk$=lpad$(str$(bankcode),2)&'1'&lpad$(str$(ckn),8)
	read #trmstr1,using 'form pos 4,C 8,G 6,pd 10.2,C 8,C 35',key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey CKNUM_XIT
	dtr$(3)=str$(dtr3)
	SCR_CKPRT6: !
	fnTos
	respc=0
	fnLbl(1,1,'Check number '&str$(ckn)&' has been previously used.',45,1)
	fnLbl(2,10,' Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',val(dtr$(2))),45,0)
	fnLbl(3,10,' Amount: '&dtr$(3),45,0)
	fnLbl(4,10,' To: '&dtr$(5),45,0)
	fnChk(6,48,'Delete the previous entry:',1)
	resp$(respc+=1)='False'
	fnLbl(8,1,'New check number (if applicable):',45,1)
	fnTxt(8,48,8,0,1,'30',0,'You will never enter the new check number if you are deleting the old check.')
	resp$(respc+=1)=''

	fnCmdSet(2): ckey=fnAcs(mat resp$)
	ckn2=val(resp$(2))
	if resp$(1)(1:1)='T' then goto CKNUM_DEL_PRV ! delete previous check
	if ckn2<=0 then
		mat ml$(2)
		ml$(1)='You must supply the new check number any time'
		ml$(2)='you choose not to delete the old check.'
		fnMsgBox(mat ml$,resp$,'',16)
		goto SCR_CKPRT6
	end if
	ckn=ckn2
	tr$(1)=lpad$(str$(ckn2),8)
	goto CKNUM_TOP ! ***********************
	CKNUM_DEL_PRV: !
	bal=bal+val(dtr$(3))
	delete #trmstr1,key=dk$:
	rewrite #bankmstr,using 'form pos 45,PD 6.2',key=lpad$(str$(bankcode),2): bal nokey ignore
	restore #tralloc,key>=dk$: nokey CKNUM_XIT
	do
		read #tralloc,using 'form pos 1,c 11': trkey$ eof CKNUM_XIT
		if trkey$<>dk$ then goto CKNUM_XIT
		delete #tralloc,key=dk$:
	loop
	CKNUM_XIT: !
fnend
def fn_index
	L4050: !
	open #31: 'Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],NoShr',i,outIn,k ioerr L4070
	close #31: !
	goto L4080
	L4070: !
	mat ml$(2)
	ml$(1)='You must get everyone out of the Unpaid Invoice File'
	ml$(2)='before you can continue!  Press OK when ready.'
	fnMsgBox(mat ml$,resp$,'',16)
	goto L4050
	L4080: !
	fn_close(hUnPdAlloc)
	fn_close(ivpaid)
	fnRemoveDeletedRecords('[Q]\CLmstr\PayTrans.h[cno]')
	fnRemoveDeletedRecords('[Q]\CLmstr\UnPdAloc.h[cno]')
	fnIndex('[Q]\CLmstr\PayTrans.h[cno]','[Q]\CLmstr\UNPdIdx1.h[cno]','1,20')
	fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx2.h[cno]','1,20')
	fnIndex('[Q]\CLmstr\unpdaloc.h[cno]','[Q]\CLmstr\Uaidx1.h[cno]','9,12')
	fnIndex('[Q]\CLmstr\IvPaid.h[cno]','[Q]\CLmstr\IVIndex.h[cno]','1,20')
fnend

def fn_portion_stub(stubOnCheck)
	! stubOnCheck - 1 or 2 to say if it is the first or second stub on a check.  Some formats care
	if env$('client')='Billings' or env$('client')='Diamond' then
		fn_portion_stub_billings(stubOnCheck)
	! else if env$('client')='Divernon' then
	! 	fn_portion_stub_divernon
	else
		fn_portion_stub_generic
		if (env$('client')='Edison' or env$('client')='Crockett County') and stubOnCheck=1 then
			pr #255: ''
			pr #255: ''
			pr #255: ''
			if env$('client')='Edison' then pr #255: ''
		end if
	end if
fnend
def fn_portion_stub_generic
	if trim$(holdvn$)<>'' then read #h_vf1,using 'form pos 9,4*C 30',key=holdvn$,release: mat b$ nokey L10970
	L10970: !
	pr #255: ''
	if env$('client')='Crockett County' then
		pr #255,using 'form pos 19,C 30,pos 50,C 12,PIC(ZZ/ZZ/ZZ),pos 74,N 6': b$(1),holdvn$,prdmmddyy
	else
		pr #255,using 'form pos 19,C 30,pos 50,C 12,PIC(ZZ/ZZ/ZZ),pos 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
	end if
	pr #255: ''
	mat b$=(' ') : b$(1)=tr$(5)(1:30)
	if h_vf1=23 then vp1=173 else vp1=147
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	pr #255: 'Invoice Number   Amount   Description   Invoice Number   Amount   Description'
	pr #255: '_______________________________________ _______________________________________'
	for j=1 to 15
		pr #255,using 'form pos 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,pos 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de2$(j,1),iv$(j,2)(1:12),amt(j,2),de2$(j,2)
	next j
fnend
def fn_portion_stub_billings(stubOnCheck)
	pr #255: ''
	if trim$(holdvn$)<>'' then read #h_vf1,using 'form pos 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
	pr #255: ''
	pr #255,using 'form pos 19,C 30,pos 50,C 12,PIC(ZZ/ZZ/ZZ),pos 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
	mat b$=(' ') : b$(1)=tr$(5)(1:30)
	if h_vf1=23 then vp1=173 else vp1=147
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	pr #255: 'Invoice Number   Amount   Description   Invoice Number   Amount   Description'
	pr #255: '_______________________________________ _______________________________________'
	for j=1 to 15
		pr #255,using 'form pos 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,pos 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de2$(j,1),iv$(j,2)(1:12),amt(j,2),de2$(j,2)
	next j
	if stubOnCheck=1 then ! it is the first stub on the check
		pr #255: ''
		pr #255: ''
		pr #255: ''
	end if
fnend
! def fn_portion_stub_divernon
! 	pr #255: ''
! 	if trim$(holdvn$)<>'' then read #h_vf1,using 'form pos 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
! 	pr #255: ''
! 	pr #255,using 'form pos 19,C 30,pos 50,C 12,PIC(ZZ/ZZ/ZZ),pos 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
! 	pr #255: ''
! 	mat b$=(' ') : b$(1)=tr$(5)(1:30)
! 	if h_vf1=23 then vp1=173 else vp1=147
! 	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
! 	pr #255: 'Invoice Number   Amount   Description   Invoice Number   Amount   Description'
! 	pr #255: '_______________________________________ _______________________________________'
! 	for j=1 to 15
! 		pr #255,using 'form pos 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,pos 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de2$(j,1),iv$(j,2)(1:12),amt(j,2),de2$(j,2)
! 	next j
! fnend
def fn_portion_check
	if env$('client')='ACS' and bankcode=2 then
		fn_portion_check_acs(amt)
	else if env$('client')='Ash Grove' then
		fn_portion_check_ashgrove(amt)
	else if env$('client')='Billings' or env$('client')='Diamond' then
		fn_portion_check_billings(amt)
	else if env$('client')='Campbell' then
		fn_portion_check_generic(amt, 28,57)
	else if env$('client')='Cerro Gordo V' then
		fn_portion_check_cerroGordoV(amt)
	else if env$('client')='Cerro Gordo T' then
		fn_portion_check_generic(amt, 28,55)
	else if env$('client')='Crockett County' then
		fn_portion_check_Crocket(amt)
	! else if env$('client')='Divernon' then
	! 	fn_portion_check_divernon(amt)
	else if env$('client')='Edison' then
		fn_portion_check_edison(amt)
	else if env$('client')='Kimberling' then
		fn_portion_check_kimber(amt)
	! else if env$('client')='Lovington' then
	! 	fn_portion_check_generic(amt, 29)
	else
		fn_portion_check_generic(amt)
	end if
fnend
def fn_portion_check_generic(dolamt; length,posDate)
	! r: gather information, etc
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	! /r
	if env$('client')='ACS' or env$('client')='Thomasboro' or env$('client')='Hope Welty' then goto L1730 ! don't skip  ! or env$('client')='Divernon'
	pr #255: '' ! line 1
	L1730: !
	skipline=9
	if prenum=2 then
		pr #255,using 'form skip 2,pos 74,n 8,skip 1': ckn1 ! line 2,3, 4
		skipline-=3
	end if
	pr #255,using 'form skip SKIPLINE,pos 9,C 80,skip 1,pos 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: '' ! line 13
	if posDate then
		a=posDate
	else
		a=65
		if env$('client')='Bethany' then a=54
		if env$('client')='Thomasboro' or env$('client')='Unity' or env$('client')='Hope Welty' or env$('client')='Edinburg' then a=55
	end if
	if env$('client')='Campbell' then pr #255: ''
	pr #255,using 'form pos A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$ ! line 14
	if env$('client')<>'Campbell' then pr #255: ''
	pr #255: '' ! line 16
	if env$('client')='Cerro Gordo T'  then pr #255: ''
	if env$('client')='Thomasboro' then pr #255: '' : pr #255: '' ! bump address down two spaces
	for j=1 to 4
		pr #255,using 'form pos 8,C 30': b$(j) ! lines 17-20
	next j
	if length=0 then ! do it the old way
		skipline=6
		if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
		! if env$('client')='Washington Parrish' then skipline=skipline-1
		if env$('client')='ACS' or env$('client')='Hope Welty' then skipline=skipline+2
		! if env$('client')='PiattCO' then skipline=skipline+4
		for j=1 to skipline
			pr #255: ''
		next j
	else
		for lineItem=21 to length  ! default length is 27, i think
			pr #255: ''
		nex lineItem
	end if
fnend
def fn_portion_check_ashgrove(dolamt)
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	fn_englishdollar(dolamt)
	x=3 ! 1  add three lines after top stub
	for j=1 to x
		pr #255: ''
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	skipline=9
	if prenum=2 then 
		pr #255,using 'form skip 2,pos 74,n 8,skip 1': ckn1
		skipline=max(skipline-3,1)
	end if
	pr #255,using 'form skip SKIPLINE,pos 9,C 80,skip 1,pos 9,C 70': eng$(1:n), eng$(n+1:128)
	a=65
	pr #255,using 'form pos A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ''
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	for j=1 to 4
		pr #255,using 'form pos 8,C 30': b$(j)
	next j
	skipline=6
	if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline+3
		pr #255: ''
	next j
fnend
def fn_portion_check_kimber(dolamt)
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6480
	goto L6490
	L6480: mat b$=('')
	L6490: fn_englishdollar(dolamt)
	x=1
	for j=1 to x
		pr #255: ''
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	skipline=9
	if prenum=2 then 
		pr #255,using 'form skip 2,pos 74,n 8,skip 1': ckn1
		skipline=max(skipline-3,1)
	end if
	pr #255,using 'form skip SKIPLINE,pos 9,C 80,skip 1,pos 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: ''
	pr #255,using 'form pos 60,PIC(ZZ/ZZ/ZZ),X 7,C 18': prdmmddyy,ca$
	pr #255: ''
	pr #255: ''
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	for j=1 to 4
		pr #255,using 'form pos 8,C 30': b$(j)
	next j
	skipline=8
	if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ''
	next j
fnend
def fn_portion_check_Crocket(dolamt)
	! r: this is the same as generic, but with spacing for crocket
	! if env$('acsDeveloper')<>'' then pause
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	pr #255,using 'form skip 4,c 1': ' ' ! line 1
		! for j=1 to 4 ! just print name here
	pr #255,using 'form pos 8,C 30': b$(1) ! lines 17-20
	! next j
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=lpad$('*'&trim$(cnvrt$('PIC(ZZZ,ZZZ,ZZ#.##)',dolamt)),14,' ') ! changed for preprinted symbol
	skipline=0
	if prenum=2 then
		pr #255,using 'form skip 2,pos 74,n 8,skip 1': ckn1 ! line 2,3, 4
		skipline-=3
	end if
		a=60
	pr #255,using 'form pos A,PIC(ZZ/ZZ/ZZ),X 5,C 18': prdmmddyy,ca$ ! line 14
	pr #255: ''
	! pr #255: ''
	pr #255,using 'form skip SKIPLINE,pos 9,C 80,skip 1,pos 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: '' ! spacing
	pr #255,using 'form pos 12, c 30': trim$(b$(1))
	pr #255,using 'form pos 12, c 30': trim$(b$(2))
	pr #255,using 'form pos 12,C 30': b$(3)
	pr #255,using 'form pos 12,C 30': b$(4) ! last address line or blank for spacing
	if length=0 then ! do it the old way
		skipline=11
		if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
		for j=1 to skipline
			pr #255: ''
		next j
	else
		for lineItem=21 to length  ! default length is 27, i think
			pr #255: ''
		nex lineItem
	end if
	! /r
fnend
! def fn_portion_check_divernon(dolamt)
! 	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6690
! 	goto L6700
! 	L6690: mat b$=('')
! 	L6700: fn_englishdollar(dolamt)
! 	x=1
! 	for j=1 to x
! 		pr #255: ''
! 	next j
! 	if dolamt=0 then eng$='        *** V O I D ***'
! 	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
! 	for j=1 to 9
! 		pr #255: ' '
! 	next j
! 	a=62
! 	pr #255,using 'form pos A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
! 	skipline=2
! 	pr #255,using 'form skip SKIPLINE,pos 4,C 80,skip 1,pos 9,C 70': eng$(1:n), eng$(n+1:128)
! 	if trim$(b$(2))='' then
! 		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
! 	else if trim$(b$(3))='' then
! 		b$(3)=b$(4) : b$(4)=''
! 	end if
! 	for j=1 to 4
! 		pr #255,using 'form pos 8,C 30': b$(j)
! 	next j
! 	skipline=6
! 	for j=1 to skipline
! 		pr #255: ''
! 	next j
! fnend
def fn_portion_check_acs(dolamt)
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))='' then
		b$(2)=b$(3) : b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	pr #255: ''
	skipline=9
	if prenum=2 then 
		pr #255,using 'form skip 2,pos 74,n 8,skip 1': ckn1
		skipline=max(skipline-3,1)
	end if
	pr #255,using 'form skip 1, pos 80,pic(zz/zz/zz)': prdmmddyy
	pr #255: ''
	pr #255,using 'form skip 1,pos 15,C 30,pos 73,c 18': b$(1),ca$
	pr #255: ''
	pr #255,using 'form skip 1,pos 9,C 80,skip 1,pos 9,C 70': eng$(1:n),eng$(n+1:128)
	pr #255: ''
	for j=1 to 4
		pr #255,using 'form pos 8,C 30': b$(j)
	next j
	skipline=10
	if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ''
	next j
fnend
def fn_portion_check_cerroGordoV(dolamt)
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	fn_englishdollar(dolamt)
	x=2
	for j=1 to x
		pr #255: ''
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	skipline=8
	if prenum=2 then 
		pr #255,using 'form skip 3,pos 74,n 8,skip 1': ckn1
		skipline=max(skipline-3,1)
	end if
	skipline=skipline-2
	pr #255,using 'form skip SKIPLINE,pos 9,C 80,skip 1,pos 9,C 70,skip 1': eng$(1:n), eng$(n+1:128)
	pr #255: ''
	a=55
	pr #255,using 'form pos 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	for j=1 to 4
		pr #255,using 'form pos 8,C 30': b$(j)
	next j
	skipline=6
	if scc$='SCC' then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ''
	next j
fnend
def fn_portion_check_billings(dolamt)
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
!
	pr #255: ''
	pr #255,using 'form pos 42,c 38': 'Void After 60 Days'
	pr #255: ''
	pr #255: ''
	pr #255: ''
	if trim$(eng$(n+1:128))='' then
		pr #255: ''
		pr #255,using 'form pos 9,C 80': eng$(1:n)
	else
		pr #255,using 'form pos 9,C 80': eng$(1:n)
		pr #255,using 'form pos 9,C 70': eng$(n+1:128)
	end if
	pr #255,using 'form pos 59,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using 'form pos 8,C 30': b$(1)
	pr #255,using 'form pos 8,C 30': b$(2)
	pr #255,using 'form pos 8,C 30': b$(3)
	pr #255,using 'form pos 8,C 30': b$(4)
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
fnend
def fn_portion_check_edison(dolamt)
	mat b$=('')
	read #h_vf1,using 'form pos 9,4*C 30,pos VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))='' then
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=''
	else if trim$(b$(3))='' then
		b$(3)=b$(4) : b$(4)=''
	end if
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$='***VOID***' else ca$=rtrm$(cnvrt$('PIC($$$,$$$,$$$.##)',dolamt))
	!
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using 'form pos 74,PIC(ZZ/ZZ/ZZ),pos 82,N 8': prdmmddyy,ckn1
	pr #255: ''
	pr #255: ''
	if trim$(eng$(n+1:128))='' then
		pr #255: ''
		pr #255,using 'form pos 9,C 80': eng$(1:n)
	else
		pr #255,using 'form pos 9,C 80': eng$(1:n)
		pr #255,using 'form pos 9,C 70': eng$(n+1:128)
	end if
	pr #255,using 'form pos 79,C 18': ca$   ! line 11
	pr #255: ''
	pr #255,using 'form pos 8,C 30': b$(1)
	pr #255,using 'form pos 8,C 30': b$(2)
	pr #255,using 'form pos 8,C 30': b$(3)
	pr #255,using 'form pos 8,C 30': b$(4)
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
fnend
def fn_scr_main_questions(;___,ckey)
	if ~smq_setup then
		smq_setup=1
		dim layoutOption$(4)*18,scc$(4)*3
		layoutOption$(1)='Stub, Check, Stub'  : scc$(1)='SCS'
		layoutOption$(2)='Check, Stub, Stub'  : scc$(2)='CSS'
		layoutOption$(3)='Stub, Stub, Check'  : scc$(3)='SSC'
		layoutOption$(4)='Stub, Check, Check' : scc$(4)='SCC'
		fncreg_read('Check Layout Option',layoutOptionSelected$, layoutOption$(1))
	end if
	ckey=0
	fnTos
	respc=0
	fnLbl(1,1,'Method of Printing checks:',38,1)
	fnOpt(1,40,'Enter and pr Checks',0)
	if ckoption<=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(2,40,'Print Checks for Selected Invoices',0)
	if ckoption=2 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnOpt(3,40,'Reprint from Check History',0)
	if ckoption=3 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnLbl(5,1,'Date of Checks:',38,1)
	fnTxt(5,40,10,0,1,'3',0,'')
	resp$(respc+=1)=date$('ccYYMMDD')
	fnLbl(6,1,'Beginning check number:',38,1)
	fnTxt(6,40,8,0,1,'30',0,'Next available check #. If reprinting checks from history, this check # is not applicable.')
	resp$(respc+=1)=str$(ckn)
	fnLbl(7,1,'Bank Account:',38,1)
	fnComboF('Bankmstr',7,40,20,'[Q]\CLmstr\bankmstr.h[cno]',1,2,3,15,'[Q]\CLmstr\Bankidx1.h[cno]',1,0, 'Select bank account for printing')
	resp$(respc+=1)=str$(bankcode)
	fnLbl(8,1,'Check Format:',38,1)
	fnComboA('ckprt-2',8,40,mat layoutOption$)
	resp$(respc+=1)=layoutOptionSelected$
	!   if env$('client')='Washington Parrish' then resp$(respc)=layoutOption$(4)
	if env$('client')='Billings' or (env$('client')='ACS'and bankcode=2) then resp$(respc)=layoutOption$(2)
	! need button to show totals
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		for j=1 to 3
			if resp$(j)='True' then ckoption=j
		next j
		prd=val(resp$(4)) ! date of checks
		prdmmddyy=val(resp$(4)(5:6))*10000+val(resp$(4)(7:8))*100+val(resp$(4)(3:4)) ! convert date back to mmddyy format
		ckn=val(resp$(5)) ! beginning ckey number
		bankcode=val(resp$(6)(1:3))
		layoutOptionSelected$=resp$(7)
		for j=1 to 4
			if trim$(layoutOptionSelected$)=trim$(layoutOption$(j)) then scc$=scc$(j)
		next j
		fncreg_write('Check Layout Option',layoutOptionSelected$)
	end if  ! ckey<>5
	fn_scr_main_questions=ckey
fnend
include: ertn
