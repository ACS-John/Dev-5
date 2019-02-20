
	library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnTos,fnLbl,fnTxt,fncomboa,fnChk,fnCmdSet,fnAcs,fncombof,fnFra,fnmsgbox,fnButton,fnOpt,fnCmdKey,fnaddpayee,fnqgl,fnagl$,fnrgl$,fncreg_read,fncreg_write
	library 'S:\Core\Library': fnIndex_it,fnRemoveDeletedRecords
	on error goto ERTN

	dim vn$*8,holdvn$*8,up$(4),amt(15,3),iv$(15,3),de$(15,3)*13,ivdate(15,3)
	dim cnam$*40,tr(2),misc$(10)*20,miscgl$(10)*12
	dim de$*50,lcn$*8,whgl$(5)*12,gl$*12,allocde$*30
	dim whgl(5,3),in3$(150)*30,bn$*30,b$(4)*30
	dim ade$*30,dedcode(10),agl(3),de$*50
	dim t1(5),arec(100),d(2)
	dim pr$(4)*30,myact$*20,disamt(15,3)
	dim resp$(50)*50,item4$(2)*35,item5$(2)*35
	dim holdpayee$*50,holdresp$(94)*50,contact$*30,email$*50,fax$*12
	dim inl$(4)*50,ml$(5)*90
	dim allockey$*20
	dim eng$*128,wording$(27)*9,amount(11)
	dim tr$(5)*35,sn$*30,dtr$(5)*35,payeegl$*12,gldesc$*30

	fntop(program$)
	prd=val(date$(4:5)&date$(7:8)&date$(1:2))
	open #bankmstr:=12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed 
	open #h_paymstr1:=13: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed 
	open #paymstr2:=14: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed 
	open #trmstr1:=1: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed 
	open #trmstr2:=2: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
	open #tralloc:=3: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\tralloc-idx.h[cno],Shr",internal,outIn,keyed 
	open #h_paytrans:=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed 
	open #h_unpdaloc:=7: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\uaidx2.h[cno],Shr",internal,outIn,keyed 
	open #glmstr18:=18: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
	open #glcontrol:=19: "Name=[Q]\CLmstr\Fundmstr.h[cno],KFName=[Q]\CLmstr\Fundidx1.h[cno],Shr",internal,outIn,keyed 
	open #ivpaid:=6: "Name=[Q]\CLmstr\IvPaid.h[cno],KFName=[Q]\CLmstr\IVIndex.h[cno],Shr",internal,outIn,keyed 
	open #payeegl:=17: "Name=[Q]\CLmstr\payeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Shr",internal,outIn,keyed 
	fn_get_coinfo
MENU1: ! 
	read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
	t1(1)=bal
	upi=t1(5)
	t1(3)=t1(1)-t1(2)
	ckn=val(lcn$)+1 conv ignore
	bn$=rtrm$(bn$)
MAIN_QUESTIONS: ! 
	if fn_scr_main_questions=5 then goto XIT
! 
	tac=0
	if ti1=3 and pri=1 then h_vf1=23 else h_vf1=h_paymstr1
	if ti1=1 then h_vf1=13
	allign=0
	if ti1=3 then goto REPRINT_CHECKS
	open #company=15: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative 
	rewrite #company,using 'Form POS 152,N 2',rec=1: bankcode
	close #company: 
	read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey MAIN_QUESTIONS
	mat in3$=("")
	bn$=rtrm$(bn$)
	if ti1=1 or ti1=3 then goto CKOPTION1_CHECK_ENTRY
	restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU1
	amt=arec=x=y=0
	mat amt=(0) : mat de$=("")
READ_4: ! 
	read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,n 1,n 6,n 10.2,n 8',release: vn$,iv$,mat up$,upa,pcde,bc,ck$,dp,gde,pdate,disamt,ddate eof EOF_ROUTINE
	vn$=lpad$(rtrm$(vn$),8)
	if rtrm$(vn$)="" then goto READ_4
	if pcde=0 then goto READ_4
	if bc=0 then bc=bankcode ! if coded for payment with no bank code, pay from the  current bank account
	if bc<>bankcode then goto READ_4
	lr4=rec(h_paytrans) ! what to do
	if holdvn$=vn$ or rtrm$(holdvn$)="" then goto L1130 else gosub SUB_PRINT_CHECK
L1130: if arec>30 then gosub SUB_PRINT_CHECK
	fn_checkdiscount
	if iv$<>hiv$ then 
		y=y+1
		if y>2 then y=1
		if y=1 then x+=1
		if x>15 then gosub SUB_PRINT_CHECK
		iv$(x,y)=iv$(1:12)
		de$(x,y)=up$(4)(1:13) ! this one for printing from unpaid file
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

SUB_PRINT_CHECK: ! r:
	fn_cknum
! if env$('client')="Washington Parrish" then let fnprocess(1) ! skip Atlantis screen
	fnopenprn(cp,42,220,process)
	ckn1=ckn
!   on ckoption goto L1360,L1360 none L1360 ! L1390
! L1360: ! 
	if amt<=0 then goto L2420
	if scc$="CSS" then let fn_portion_check : fn_portion_stub(1) : fn_portion_stub(2)
! L1390: ! 
	if scc$="SCS" then let fn_portion_stub(1) : fn_portion_check : fn_portion_stub(2)
	if scc$="SSC" then let fn_portion_stub(1) : fn_portion_stub(2) : fn_portion_check
	if scc$="SCC" then let fn_portion_stub(1) : fn_portion_check : fn_portion_check
	gosub UPDATEINVOICE
return ! /r

UPDATEINVOICE: ! r:
	for j=1 to arec
		rewrite #h_paytrans,using 'Form POS 76,N 8,N 6',rec=arec(j): ckn,prdmmddyy
	next j
	idx=1
	! allign=3  !  remove allign routine alltogether   <-- that did not work.
	if allign=3 then pr #255: newpage : goto ALIGN_COMPLETED
	pr #255: newpage
	fncloseprn
! if env$('client')="Washington Parrish" then let fnprocess(0)
	holdpayee$=""
	if ti1=1 then ckoption=1 : allign=2 : goto L2300 ! skip the continue routine when entering and printing checks
	if ~allign then 
		if ckoption=1 or ckoption=3 then 
			mat inl$(4)
			inl$(4)="4. Void previous check    "
		else if ckoption=2 then 
			mat inl$(3)
		end if 
		inl$(1)="1. Reprint the same check    "
		if ckoption=1 or ckoption=3 then 
			inl$(2)="2. Continue with next check    "
		else 
			inl$(2)="2. Print next check and Stop   "
		end if 
		if ckoption=1 or ckoption=3 then 
			inl$(3)="3. Completed with checks"
		else 
			inl$(3)="3. Print All remaining checks  "
		end if 
	end if  ! ~allign
SCR_CKPRT7: ! 
	fnTos(sn$="ckprt-7")
	respc=0
	fnLbl(1,1,"",40,0)
	fnLbl(1,1,"Print Options:",38,0)
	fnOpt(2,3,inl$(1),0)
	resp$(respc+=1)="False"
	fnOpt(3,3,inl$(2),0)
	resp$(respc+=1)="True" !  if ckoption=1 or ckoption=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(4,3,inl$(3),0)
	if ckoption=2 then 
		resp$(respc+=1)="True"
	else 
		resp$(respc+=1)="False"
		if trim$(inl$(4))<>"" then 
			fnOpt(5,3,inl$(4),0)
			resp$(respc+=1)="False"
		end if 
	end if 
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if (ck=5 or ck=99) and ckoption=1 then let fn_write_ck_hist_1 : goto MENU1
	if (ck=5 or cmdkey=99) then goto TRANS_TO_CK_HIST
	for j=1 to 4
		if resp$(j)(1:1)="T" then allign=j : goto L2300
	next j
L2300: ! 
	if ckoption=1 and allign=2 then let fn_write_ck_hist_1 ! write regular check history if not a reprint
! L2310: ! 
	if ckoption=1 and allign=3 then let fn_write_ck_hist_1 !  write regular check history
	on allign goto ALIGN_REPR_SAME,ALIGN_PRINT_NEXT,ALIGN_COMPLETED,ALIGN_PRINT_NEXT none SCR_CKPRT7

ALIGN_REPR_SAME: ! 
	if prenum=1 then 
		write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",1,dp,1
		ckn=ckn+1
		tr$(1)=str$(ckn)
	end if 
	goto SUB_PRINT_CHECK
ALIGN_COMPLETED: ! 
	if ckoption=3 and allign=3 then goto TRANS_TO_CK_HIST
ALIGN_PRINT_NEXT: ! 
	if allign=4 and prenum=1 then 
		write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,g 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,ckn,prdmmddyy,0,"","VOID",0,dp,1
	end if 
	ckn=ckn+1
L2420: ! 
	mat iv$=("") : mat de$=("")
	mat amt=(0) : mat de$=("")
	amt=arec=x=y=0
	x=y=1
	st1=0
	holdvn$=vn$
	tr$(3)=tr$(4)=tr$(5)=""
	hiv$=""
	tac=0
	if (ckoption=1 or ckoption=3) and allign=3 then goto MENU1
	return ! /r
EOF_ROUTINE: ! r:
	if st1=1 then gosub SUB_PRINT_CHECK
	fncloseprn
! if env$('client')="Washington Parrish" then let fnprocess(0)
	mat amt=(0) : mat de$=("") : mat iv$=("") : mat de$=("") : x=y=1
	st1=0 : holdvn$=""
	goto MENU3
! /r
MENU3: ! r: (reprint or transfer to history)
	fnTos(sn$="ckprt-4")
	respc=0
	fnLbl(1,1,"Reprint Options:",38)
	item5$(1)="Reprint Checks"
	item5$(2)="Transfer to Check History"
	fncomboa("ckprt-cmb1",1,40,mat item5$,tt$)
	resp$(respc+=1)=item5$(2)
	fnCmdSet(41): fnAcs(sn$,0,mat resp$,ck)
	if resp$(1)=item5$(1) then ti2=1 else ti2=2
	allign=0
	on ti2 goto MENU4,TRANS_TO_CK_HIST none MENU3
! /r
MENU4: ! r: (Reprint Options)
	fnTos(sn$="ckprt-reprint")
	respc=0
	fnLbl(1,1,"Reprint Options:",38)
	item4$(1)="Reprint all checks"
	item4$(2)="Begin with specific Payee"
	fncomboa("ckprt-cmb2",1,40,mat item4$,tt$)
	resp$(respc+=1)=item4$(1)
	fnLbl(3,1,"Beginning payee number:",38)
	fncombof("Paymstr",3,10,30,"[Q]\CLmstr\paymstr.h[cno]",1,8,9,30,"[Q]\CLmstr\Payidx1.h[cno]",0,pas, "Enter the beginning payee number if you wish to only reprint part of the checks")
	resp$(respc+=1)=holdpayee$
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT
	if resp$(1)=item4$(1) then ti2=1 else ti2=2
	begvn$=resp$(2)(1:8) ! beginning payee to start reprint
	on ti2 goto L3430,L3470
L3430: ! REPRINT ALL
	restore #h_paytrans,key>="                    ": 
	hiv$="" : de$="" ! ???
	goto MAIN_QUESTIONS
L3470: ! reprint beginning with specific payee
	restore #h_paytrans,key>=lpad$(rtrm$(begvn$),8)&"            ": nokey MENU4
	hiv$="" : de$=""
	goto MAIN_QUESTIONS
! /r
TRANS_TO_CK_HIST: ! r: TRANSFER TO CHECK HISTORY
	fn_write_history
	goto FINIS ! /r
FINIS: ! r: COMPLETE
	fn_close(1)
	fn_close(trmstr2)
	fn_close(tralloc)
	fn_close(h_paytrans)
	fn_close(ivpaid)
	fn_close(bankmstr)
	fn_close(h_paymstr1)
	fn_close(company)
	if idx then let fn_index
	goto XIT ! /r
	def fn_close(h_closeme)
		close #h_closeme: ioerr CLOSE_IGNORE
CLOSE_IGNORE: ! 
	fnend  ! fn_close
XIT: fnxit
IGNORE: continue 
CKOPTION1_CHECK_ENTRY: ! r:
	mat resp$=("")
CKOPTION1_CHECK_ENTRY_2: ! 
	ck=fn_scr_check_entry
	if ck=5 then 
		goto XIT
	else if ck=20 or ck=21 then 
		goto ASSIGN_SCREENS
	else 
		goto STORE_GL_BREAKDOWNS
	end if 
! /r
STORE_GL_BREAKDOWNS: ! r: store general ledger breakdowns
	x=0 : tac=0
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
	if ck=20 or ck=21 then 
		goto ASSIGN_SCREENS
	else 
		goto COMPLETED_WITH_SCREEN
	end if 
! /r
ASSIGN_SCREENS: ! r: assign screen # based on more and back options
	if screen=0 then screen=1
	if ck=20 and screen=1 then screen=2 : goto L5070
	if ck=20 and screen=2 then screen=3 : goto L5070
	if ck=20 and screen=3 then screen=3 : goto L5070 ! shouldn't happen
	if ck=21 and screen=2 then screen=1 : goto L5070
	if ck=21 and screen=1 then screen=1 : goto L5070 ! shouldn't happen
	if ck=21 and screen=3 then screen=2 : goto L5070
L5070: for j=1 to 30
		if screen=1 then x=j+2
		if screen=2 then x=j+34
		if screen=3 then x=j+64
		if int(j+2/3)=(j+2/3) then resp$(j+2)=holdresp$(x) else resp$(j+2)=holdresp$(x)
	next j
	goto CKOPTION1_CHECK_ENTRY_2 ! /r
COMPLETED_WITH_SCREEN: ! r:
	screen=0
	if ck<>2 then goto PRINT_REGULAR_CHECKS ! skip automatic allocation
	fn_read_standard_breakdowns
	goto CKOPTION1_CHECK_ENTRY_2
! /r
PRINT_REGULAR_CHECKS: ! r:
	fn_cknum
	amt=arec=x=y=0
	mat amt=(0) : mat de$=("")
	if tac<>val(tr$(3)) then let fn_msg_allocations_off : goto CKOPTION1_CHECK_ENTRY_2 ! ALLOCATIONS NOT EQUAL
	for j=1 to 30 ! kj was 10
		if in3$(j*5)=hiv$ and rtrm$(hiv$)<>"" then goto L5460
		y=y+1: if y>2 then y=1
		if y=1 then x=x+1
		de$(x,y)=in3$(j*5)(1:13)
L5460: ! 
		amt(x,y)=amt(x,y)+val(in3$(j*5-1))
		hiv$=in3$(j*5)(1:15)
	next j
	amt=val(tr$(3)) : vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
	gosub SUB_PRINT_CHECK
	mat holdresp$=("")
	goto CKOPTION1_CHECK_ENTRY
! /r
include: ertn
REPRINT_CHECKS: ! r:
	fnTos(sn$="reprintnumber")
	respc=0
	fnLbl(1,1,"First Check Number to Reprint:",38,1)
	fnTxt(1,40,8,0,1,"30",0,"")
	resp$(respc+=1)=str$(firstckn)
	fnLbl(2,1,"Last Check Number to Reprint:",38,1)
	fnTxt(2,40,8,0,1,"30",0,"")
	resp$(respc+=1)=str$(lastckn)
	if reprintckn>0 then let fnLbl(4,1,"Last Check Number Reprinted "&str$(reprintckn)&":",38,1)
	fnCmdSet(2): fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT
	firstckn=ckn1=reprintckn=val(resp$(1))
	lastckn=val(resp$(2)) : if lastckn=0 then lastckn=firstckn
	if lastckn>0 and lastckn<firstckn then goto REPRINT_CHECKS ! smaller lastckn
REPRINT_CHECK_LOOP_TOP: ! 
	check_ref$=cnvrt$("pic(ZZ)",bankcode)&str$(1)&cnvrt$("n 8",reprintckn)
	read #trmstr1,using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey L7780
! pr 'key=';check_ref$ : pause
	prdmmddyy=val(tr$(2)) ! use old check date
	vn$=lpad$(trim$(tr$(4)),8)
	amt=tr3 ! set amount for check
	mat amt=(0) : mat de$=("") : mat iv$=("") : x=1: y=0
	st1=0 : holdvn$="        ": amt=0
	vn$=holdvn$=lpad$(rtrm$(tr$(4)),8)
	goto L7790
L7780: ! 
	if firstckn=reprintckn then goto L7785
	if reprintckn>=lastckn then goto L7970 ! complete
	mat ml$(2)
	ml$(1)="Could not locate check number "&str$(reprintckn)&" for bank number "&str$(bankcode)&"."
	ml$(2)="This check will be skipped."
	reprintckn+=1
	fnmsgbox(mat ml$,resp$)
	goto REPRINT_CHECK_LOOP_TOP
L7785: ! 
	mat ml$(2)
	ml$(1)="Cannot locate the first check (number "&str$(reprintckn)&" for bank number "&str$(bankcode)&".)"
	ml$(2)="You must choose another check number."
	fnmsgbox(mat ml$,resp$)
	goto REPRINT_CHECKS
L7790: ! 
	key$=cnvrt$('pic(ZZ)',bankcode)&str$(1)&cnvrt$("n 8",reprintckn)
	restore #tralloc,key>=key$: nokey L7780
	do 
READ_DETAILS: ! 
! 
		read #tralloc,using 'Form POS 1,N 2,N 1,n 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': transbankcode,tcode,transckn,gl$,alloc,allocde$,allocdate conv L7815,eof L7910
		goto L7820
L7815: ! 
		reread #tralloc,using 'Form POS 1,c 2': x$ eof L7910 : goto READ_DETAILS
L7820: ! 
		if transbankcode><bankcode or tcode<>1 or transckn<>reprintckn then goto L7910
		y=y+1: if y>2 then y=1
		if y=1 then x+=1
		if x>15 then gosub SUB_PRINT_CHECK
		iv$(x,y)=cnvrt$("pic(zzzzzz)",allocdate) ! IV$(1:12)
		de$(x,y)=allocde$(1:13) ! this one for printing from unpaid file
		amt(x,y)=amt(x,y)+alloc
		ivdate(x,y)=0
		disamt(x,y)=0 ! already out of net check
	loop 
L7910: ! 
	fnopenprn
	ckn1=reprintckn: amt=tr3
	if scc$="CSS" then let fn_portion_check   : fn_portion_stub(1) : fn_portion_stub(2)
	if scc$="CSS" then let fn_portion_check   : fn_portion_stub(1) : fn_portion_stub(2)
	if scc$="SCS" then let fn_portion_stub(1) : fn_portion_check   : fn_portion_stub(2)
	if scc$="SSC" then let fn_portion_stub(1) : fn_portion_stub(2) : fn_portion_check
	if scc$="SCC" then let fn_portion_stub(1) : fn_portion_check    : fn_portion_check
	if lastckn>0 and reprintckn<lastckn then reprintckn+=1 : pr #255: newpage : goto REPRINT_CHECK_LOOP_TOP
L7970: ! 
	fncloseprn
	if firstckn<>lastckn then goto XIT
	goto REPRINT_CHECKS ! /r
def fn_get_coinfo
	open #company=15: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative 
	read #company,using 'Form POS 1,C 40,POS 150,2*N 1,N 2,POS 418,10*C 20,POS 668,10*C 12,POS 298,15*PD 4,POS 618,10*N 1,POS 406,N 1,POS 788,N 1',rec=1,release: cnam$,mat d,bankcode ,mat misc$,mat miscgl$,mat whgl,mat dedcode,prenum,port
	method$="C" ! temporary kJ  ! Read #COMPANY,Using 'Form POS 789,c 1',Rec=1,Release: method$
	close #company: 
	for j=1 to 5
		whgl$(j)=lpad$(str$(whgl(j,1)),3)&lpad$(str$(whgl(j,2)),6)&lpad$(str$(whgl(j,3)),3)
	next j
	w1$=whgl$(1)
	whgl$(1)=whgl$(2)
	whgl$(2)=w1$
	! 
	do 
		read #h_paytrans,using 'Form POS 63,N 10.2,N 1',release: upa,pcde eof EO_PAYTRANS_1
		if pcde=1 then t1(2)+=upa else t1(4)+=upa
		t1(5)+=upa
	loop 
	EO_PAYTRANS_1: ! 
fnend 
def fn_scr_check_entry
	fnTos(sn$="ckprt-3")
	respc=0
	fnFra(1,1,6,87,"Check"," ")
	fnLbl(1,1,env$('cnam'),40,2,0,1)
	fnLbl(2,1,bn$,40,2,0,1)
	fnLbl(3,55,"Amount:",10,1,0,1)
	fnTxt(3,67,12,0,1,"10",0,"",1)
	resp$(respc+=1)=tr$(3)
	fnLbl(5,1,"Payee:",8,1,0,1)
	fncombof("Paymstr",5,10,30,"[Q]\CLmstr\paymstr.h[cno]",1,8,9,30,"[Q]\CLmstr\Payidx1.h[cno]",0,pas, "Enter the payee number or simply enter the payee name if no vendor record exits",1)
	resp$(respc+=1)=holdpayee$
	fnFra(9,1,12,96,"Breakdown Information"," ")
	fnLbl(1,1,"General Ledger",30,0,0,2)
	fnLbl(1,41,"Amount             Description",12,0,0,2)
	fnLbl(1,56,"Description",30,0,0,2)
	for j=1 to 10
		fnqgl(j+1,1,2,2)
		resp$(respc+=1)=fnrgl$(resp$(respc))
		! 
		fnTxt(j+1,41,12,0,1,"currency",0,"",2)
		resp$(respc+=1)=resp$(respc)
		! 
		fnTxt(j+1,56,30,0,0,"",0,"",2)
		resp$(respc+=1)=resp$(respc)
		! 
	next j
	if screen=2 or screen=3 then 
		fnButton(12,74,"Back",21,"Previous breakdown screen",1,4,2)
	end if 
	if screen=0 or screen=1 or screen=2 then 
		fnButton(12,82,"More",20,"Allows another screen of breakdowns",1,4,2)
	end if 
	pas=1 ! don't redo combo boxes
	fnLbl(1,45,"Check Number:",15,1,0,1)
	fnTxt(1,62,8,0,1,"30",0,"",1)
	resp$(respc+=1)=str$(ckn)
	fnLbl(3,30,"Check Date:",12,1,0,1)
	fnTxt(3,44,10,0,1,"3",0,"",1)
	resp$(respc+=1)=str$(prd)
	fnButton(5,52,"Add Payee",50,"Click to add a new payee record",0,0,1)
	fnCmdKey("Print",1,1,0,"Prnt this check and advance to next check")
	fnCmdKey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
	fnCmdKey("&Complete",5,0,1,"Return to menu.")
	! need a fnCmdKey to change screens for the breakdowns  (screen 1,2 or 3)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then screen=0 : goto SCE_XIT
	! 
	for j=3 to 30, step 3
		resp$(j)=fnagl$(resp$(j))
	next j
	! 
	if ck=50 then let fn_payee_add : goto CKOPTION1_CHECK_ENTRY_2
	tr$(3)=resp$(1) ! amount
	vn$=tr$(4)=lpad$(rtrm$(resp$(2)(1:8)),8) ! payee number
	read #h_paymstr1,using "Form pos 1,c 8",key=vn$,release: vn$ nokey SCE_L4640
	tr$(5)=resp$(2)(9:30) ! payee name
	goto SCE_L4650
	SCE_L4640: ! 
	tr$(5)=resp$(2)(1:30)
	vn$=tr$(4)="": b$(1)=tr$(5) ! payee name without vendor record
	SCE_L4650: ! 
	ckn=val(resp$(33)) ! ck number
	holdpayee$=resp$(2)
	prd=val(resp$(34)): ! date
	prdmmddyy=val(resp$(34)(5:6))*10000+val(resp$(34)(7:8))*100+val(resp$(34)(3:4)) ! convert date back to mmddyy format
	tr$(2)=cnvrt$("pic(######)",prdmmddyy)
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
	fn_scr_check_entry=ck
fnend 
def fn_read_standard_breakdowns ! pull standard gl breakdowns from payee file
	read #h_paymstr1,using "Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,X 6,C 12,C 30,C 50,C 12,C 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey RSB_XIT
	mat holdresp$=("")
	restore #payeegl,key>=vn$: nokey RSB_EO_READSTGL
	totalalloc=0
	for j=3 to 92 step 3
		if j=33 or j=34 then goto RSB_L5310 ! skip ck num and date  (resp$(33)&34)
	RSB_L5240: ! 
		read #payeegl,using "Form Pos 1,C 8,c 12,n 6.2,c 30",release: payeekey$,payeegl$,percent,gldesc$ eof RSB_EO_READSTGL
		if vn$<>payeekey$ then goto RSB_EO_READSTGL
		if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto RSB_L5310
		read #glmstr18,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey RSB_L5240
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
	holdvn$=""
	hck=0
	fn_close(h_paytrans:=4)
	open #h_paytrans: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed 
	WH_LOOP_TOP: ! 
	read #h_paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,N 1,N 2,G 8,G 6,N 1': vn$,iv$,mat up$,upa,pcde,bc,ckpay,dp,gde eof WH_XIT
	if gde=1 then gde=0 ! dont allow posting code of 1 from unpaid file
	if upa=0 then goto WH_L3910
	if dp=0 and ckpay=0 then goto WH_LOOP_TOP
	if bc=0 then bc=bankcode ! IF CODED FOR PAYMENT WITH NO BANK CODE, PAY FROM THE CURRENT BANK ACCOUNT
	if bc<>bankcode then goto WH_LOOP_TOP
	iv$=rpad$(ltrm$(iv$),12)
	read #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8',key=vn$&iv$,release: vn$ nokey WH_L3650
	goto WH_L3660
	WH_L3650: ! 
	write #ivpaid,using 'form pos 1,c 8,c 12,n 6,n 8': vn$,iv$,dp,ckpay
	WH_L3660: ! 
	if vn$=holdvn$ and hck=ckpay then goto WH_L3770
	mat tr=(0)
	totalupa=0
	vn$=lpad$(rtrm$(vn$),8)
	read #h_paymstr1,using 'form pos 9,c 30',key=vn$,release: b$(1) nokey WH_L3740 ! PAYEE FILE
	if ltrm$(vn$)(1:2)="T-" then 
		delete #h_paymstr1,key=vn$: nokey ignore
	end if  ! ltrm$(vn$)(1:2)="T-"
	WH_L3740: ! 
	if holdvn$<>vn$ or (hck<>ck and hck>0) then 
		write #trmstr1,using 'Form POS 1,N 2,N 1,G 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bc,1,ckpay,dp,upa,vn$,b$(1),0,0,1
	end if  ! holdvn$<>vn$ or (hck<>ck and hck>0)
	holdvn$=vn$
	hck=ck
	WH_L3770: ! 
	read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WH_L3800
	bal=bal-upa
	rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckpay nokey ignore ! WH_L3800
	WH_L3800: ! form pos 1,n 2,n 1,g 8,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
	restore #h_unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_EO_UNPDALOC
	WH_L3820: ! 
	read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$ eof WH_EO_UNPDALOC
	if trim$(allockey$(1:8))<>trim$(vn$) or trim$(allockey$(9:20))<>trim$(iv$) then goto WH_EO_UNPDALOC ! if ALLOCKEY$<>VN$&IV$ Then Goto 3690
	if sum(agl)=0 and aamt=0 then goto WH_L3820 ! don't allow zero allocations to write
	write #tralloc,using 'Form POS 1,N 2,N 1,G 8,G 3,G 6,G 3,PD 5.2,C 30,G 6,x 3,C 12,N 1': bc,1,ckpay,mat agl,aamt,ltrm$(rtrm$(iv$))&" "&ade$(1:17),up$(1),up$(3),gde
	! hOLDVN$=VN$
	hck=ckpay
	totalupa+=aamt
	goto WH_L3820
	WH_EO_UNPDALOC: ! 
	rewrite #trmstr1,using 'Form POS 18,pd 10.2',key=lpad$(rtrm$(str$(bc)),2)&"1"&lpad$(rtrm$(str$(ckpay)),8): totalupa
	WH_L3910: ! 
	delete #h_paytrans: 
	do 
		delete #h_unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey WH_L3940
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
	eng$="Value too big for editing or was less than zero"
	goto ENGLISHDOLLAR_XIT
	
	L2760: ! 
	eng$="***"
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
	eng$=rtrm$(eng$)&" Million"
	L2880: if amount(8)+amount(7)+amount(6)=0 then goto L2920
	a0=6
	gosub ENGLISHDOLLAR_L3190
	eng$=rtrm$(eng$)&" Thousand"
	L2920: ! 
	if amount(5)+amount(4)+amount(3)=0 then goto L2950
	a0=3
	gosub ENGLISHDOLLAR_L3190
	L2950: ! 
	if dol>=1 then goto L2970
	eng$=rtrm$(eng$)&" Zero"
	L2970: ! 
	eng$=rtrm$(eng$)&" Dollar"
	if dol<2 and dol>=1 then goto L3010
	eng$=rtrm$(eng$)&"s"
	if len(rtrm$(eng$))>64 then goto L3010
	L3010: ! 
	eng$=rtrm$(eng$)&" and"
	if amount(2)+amount(1)=0 then goto L3080
	amount(3)=0
	a0=1
	gosub ENGLISHDOLLAR_L3190
	goto L3090
	
	L3080: ! 
	eng$=rtrm$(eng$)&" Zero"
	L3090: ! 
	eng$=rtrm$(eng$)&" Cent"
	if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3120
	eng$=rtrm$(eng$)&"s"
	L3120: ! 
	if len(rtrm$(eng$))<64 then goto L3170
	for j=1 to 9
		n=65-j
		if eng$(n:n)=" " then goto L3170
	next j
	L3170: ! 
	goto ENGLISHDOLLAR_XIT
	
	ENGLISHDOLLAR_L3190: ! 
		if amount(a0+2)=0 then goto L3210
		eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
		eng$=rtrm$(eng$)&" Hundred"
		L3210: if amount(a0+1)=0 and amount(a0)=0 then goto ED_L3190_XIT
		if amount(a0+1)<2 then goto L3260
		eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
		if amount(a0)=0 then goto ED_L3190_XIT
		amount(a0+1)=0
		L3260: eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
		ED_L3190_XIT: ! 
	return  ! ENGLISHDOLLAR_L3190
	ENGLISHDOLLAR_XIT: ! 
fnend 
def fn_msg_allocations_off
	mat ml$(3)
	ml$(1)="The net check ("&tr$(3)&") must agree with the total"
	ml$(2)="allocations ("&str$(tac)&").  Correct the allocation"
	ml$(3)="amounts or the net check to proceed."
	fnmsgbox(mat ml$,resp$,'',16)
fnend 
def fn_checkdiscount ! check for any discounts
	if disamt=0 then goto DISCOUNTRETURN
	if ddate<prd then goto DISCOUNTRETURN ! already passed discount date
	upa=upa-disamt
	rewrite #h_paytrans,using "form pos 63,n 10.2,pos 97,n 10.2",rec=lr4: upa,0 ! subtract discount, rewrite new unpaid invoice amount and zero discount amt
	restore #h_unpdaloc,key>=vn$&iv$: nokey DISCOUNTRETURN ! get the fund # from 1st g/l in the allocation file.
	read #h_unpdaloc,using 'Form pos 1,c 20,N 3,N 6,N 3,PD 5.2,C 30': allockey$,mat agl,aamt,ade$
	if trim$(allockey$(1:8))=trim$(vn$) and trim$(allockey$(9:20))=trim$(iv$) then fundkey$=cnvrt$("pic(ZZZ)",agl(1)) else fundkey$="   "
	apgl$=discountgl$=""
	read #glcontrol, using "Form pos 52,c 12,pos 64,c 12",key=fundkey$: apgl$,discountgl$ nokey MSGBOX6
	L6410: ! 
	write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,discountgl$,-disamt,"Discount=$"&str$(disamt)
	! create an entry on the unpaid allocation file to record the discount
	if method$="A" and pcd>0 then write #h_unpdaloc,using 'Form pos 1,c 20,c 12,PD 5.2,C 30': vn$&iv$,apgl$,disamt,"Discount=$"&str$(disamt) ! create an entry in the unpaid allocation file to record the reduction in accounts payable if accrued and posted
	goto DISCOUNTRETURN
	MSGBOX6: ! 
	mat ml$(5)
	ml$(1)="Invoice # "&trim$(iv$)&" on payee # "&trim$(vn$)&" quallifies for a discount of "&trim$(cnvrt$("pic($$$$$$.##)",disamt))&","
	ml$(2)="but you have not entered the discount G/L # in the G/L control file."
	ml$(3)="The discount will be taken, but the entry in check history will not"
	ml$(4)="contain a G/L number.  Fix the GL # in the transaction file and place the "
	ml$(5)="discount G/L #s in the G/L control file."
	fnmsgbox(mat ml$,resp$,'',16)
	goto L6410
	DISCOUNTRETURN: ! 
fnend 
def fn_write_ck_hist_1 ! WRITE TRANSACTION FOR SINGLE CHECK ENTRY
	mat tr=(0)
	tr$(1)=lpad$(str$(ckn),8)
	tr$(4)=lpad$(rtrm$(tr$(4)),8)
	! k$=lpad$(str$(bankcode),2)&"1"&tr$(1)
	tr$(1)=lpad$(str$(ckn),8)
	tr3=val(tr$(3))
	write #trmstr1,using 'Form POS 1,N 2,N 1,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': bankcode,1,tr$(1),prdmmddyy,tr3,tr$(4),tr$(5),0,clr,1
	read #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey WCH1_AFTER_WRITE
	bn$=rtrm$(bn$)
	bal=bal-val(tr$(3)) conv ignore
	rewrite #bankmstr,using 'Form POS 3,C 30,POS 45,PD 6.2,PD 6.2,G 8',key=lpad$(str$(bankcode),2): bn$,bal,upi,ckn nokey WCH1_AFTER_WRITE
	WCH1_AFTER_WRITE: ! k$=LPAD$(RTRM$(STR$(BANKCODE)),2)&LPAD$(STR$(1),1)&LPAD$(TR$(1),8)
	for j=1 to 30
		if val(in3$(j*5-1))<>0 then 
			gl$=""
			gl$=cnvrt$("N 3",val(in3$(j*5-4)))&cnvrt$("N 6",val(in3$(j*5-3)))&cnvrt$("N 3",val(in3$(j*5-2)))
			alloc=val(in3$(j*5-1))
			de$=in3$(j*5) ! de$=rtrm$(tr$(5)(1:17))&"-"&in3$(j*5)(1:12)
			tr$(1)=lpad$(str$(ckn),8)
			write #tralloc,using 'Form POS 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,N 6,x 3,C 12,N 1': bankcode,1,tr$(1),gl$,alloc,de$,0,"",0
		end if 
	next j
	tr$(2)
	mat tr$=("")
	mat in3$=("")
	if ltrm$(vn$)(1:2)="T-" then delete #h_paymstr1,key=vn$: nokey ignore 
fnend 
def fn_payee_add
	vn$=tr$(4)
	mat pr$=("")
	mat desc$=("")
	mat gl$=("")
	contact$=email$=fax$=myact$=ss$=ph$=""
	fnaddpayee
	pas=0
fnend 
def fn_cknum ! CHECK FOR DUPLICATE CHECK NUMBERS
	CKNUM_TOP: ! CHECK FOR DUPLICATE CHECK NUMBERS
	dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(ckn),8)
	read #trmstr1,using 'Form POS 4,C 8,G 6,pd 10.2,C 8,C 35',key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey CKNUM_XIT
	dtr$(3)=str$(dtr3)
	SCR_CKPRT6: ! 
	fnTos(sn$="ckprt-6")
	respc=0
	fnLbl(1,1,"Check number "&str$(ckn)&" has been previously used.",45,1)
	fnLbl(2,10," Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),45,0)
	fnLbl(3,10," Amount: "&dtr$(3),45,0)
	fnLbl(4,10," To: "&dtr$(5),45,0)
	fnChk(6,48,"Delete the previous entry:",1)
	resp$(respc+=1)="False"
	fnLbl(8,1,"New check number (if applicable):",45,1)
	fnTxt(8,48,8,0,1,"30",0,"You will never enter the new check number if you are deleting the old check.")
	resp$(respc+=1)=""
	
	fnCmdSet(2): fnAcs(sn$,0,mat resp$,ck)
	ckn2=val(resp$(2))
	if resp$(1)(1:1)="T" then goto CKNUM_DEL_PRV ! delete previous check
	if ckn2<=0 then 
		mat ml$(2)
		ml$(1)="You must supply the new check number any time"
		ml$(2)="you choose not to delete the old check."
		fnmsgbox(mat ml$,resp$,'',16)
		goto SCR_CKPRT6
	end if 
	ckn=ckn2
	tr$(1)=lpad$(str$(ckn2),8)
	goto CKNUM_TOP ! ***********************
	CKNUM_DEL_PRV: ! 
	bal=bal+val(dtr$(3))
	delete #trmstr1,key=dk$: 
	rewrite #bankmstr,using 'Form POS 45,PD 6.2',key=lpad$(str$(bankcode),2): bal nokey ignore
	restore #tralloc,key>=dk$: nokey CKNUM_XIT
	do 
		read #tralloc,using 'Form POS 1,c 11': trkey$ eof CKNUM_XIT
		if trkey$<>dk$ then goto CKNUM_XIT
		delete #tralloc,key=dk$: 
	loop 
	CKNUM_XIT: ! 
fnend 
def fn_index
	L4050: ! 
	open #31: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],NoShr",internal,outIn,keyed ioerr L4070
	close #31: ! 
	goto L4080
	L4070: ! 
	mat ml$(2)
	ml$(1)="You must get everyone out of the Unpaid Invoice File"
	ml$(2)="before you can continue!  Press OK when ready."
	fnmsgbox(mat ml$,resp$,'',16)
	goto L4050
	L4080: ! 
	fn_close(h_unpdaloc)
	fn_close(ivpaid)
	fnRemoveDeletedRecords("[Q]\CLmstr\PayTrans.h[cno]")
	fnRemoveDeletedRecords("[Q]\CLmstr\UnPdAloc.h[cno]") 
	fnIndex_it("[Q]\CLmstr\PayTrans.h[cno]","[Q]\CLmstr\UNPdIdx1.h[cno]","1,20")
	fnIndex_it("[Q]\CLmstr\unpdaloc.H[cno]","[Q]\CLmstr\Uaidx2.H[cno]","1,20")
	fnIndex_it("[Q]\CLmstr\unpdaloc.H[cno]","[Q]\CLmstr\Uaidx1.H[cno]","9,12")
	fnIndex_it("[Q]\CLmstr\IvPaid.h[cno]","[Q]\CLmstr\IVIndex.h[cno]","1,20")
fnend 
! 
def fn_portion_stub(stubOnCheck)
	! stubOnCheck - 1 or 2 to say if it is the first or second stub on a check.  Some formats care
	if env$('client')="Billings" then 
		fn_portion_stub_billings(stubOnCheck)
	else if env$('client')="Divernon" then 
		fn_portion_stub_divernon
	else 
		fn_portion_stub_generic
		if (env$('client')='Edison' or env$('client')='Crocket County') and stubOnCheck=1 then
			pr #255: ''
			pr #255: ''
			pr #255: ''
			pr #255: ''
		end if
	end if 
fnend 
def fn_portion_stub_generic
	if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey L10970
	L10970: ! 
	pr #255: ""
	pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
	pr #255: ""
	mat b$=(" ") : b$(1)=tr$(5)(1:30)
	if h_vf1=23 then vp1=173 else vp1=147
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
	pr #255: "_______________________________________ _______________________________________"
	for j=1 to 15
		pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
	next j
fnend 
def fn_portion_stub_billings(stubOnCheck)
	pr #255: ""
	if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
	pr #255: ""
	pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
	mat b$=(" ") : b$(1)=tr$(5)(1:30)
	if h_vf1=23 then vp1=173 else vp1=147
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
	pr #255: "_______________________________________ _______________________________________"
	for j=1 to 15
		pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
	next j
	if stubOnCheck=1 then ! it is the first stub on the check
		pr #255: ""
		pr #255: ""
		pr #255: ""
	end if 
fnend 
def fn_portion_stub_divernon
	pr #255: ""
	if trim$(holdvn$)<>"" then read #h_vf1,using 'Form POS 9,4*C 30',key=holdvn$,release: mat b$ nokey ignore
	pr #255: ""
	pr #255,using 'Form POS 19,C 30,POS 50,C 12,PIC(ZZ/ZZ/ZZ),POS 74,N 6': b$(1),holdvn$,prdmmddyy,ckn1
	pr #255: ""
	mat b$=(" ") : b$(1)=tr$(5)(1:30)
	if h_vf1=23 then vp1=173 else vp1=147
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	pr #255: "Invoice Number   Amount   Description   Invoice Number   Amount   Description"
	pr #255: "_______________________________________ _______________________________________"
	for j=1 to 15
		pr #255,using 'Form POS 1,C 12,PIC(ZZZZ,ZZZ.ZZCR),X 1,C 13,POS 41,C 12,PIC(ZZZZ,ZZZ.ZZCR),C 13': iv$(j,1)(1:12),amt(j,1),de$(j,1),iv$(j,2)(1:12),amt(j,2),de$(j,2)
	next j
fnend 
def fn_portion_check
	if env$('client')="ACS" and bankcode=2 then 
		fn_portion_check_acs(amt)
	else if env$('client')="Ash Grove" then 
		fn_portion_check_ashgrove(amt)
	else if env$('client')="Billings" then 
		fn_portion_check_billings(amt)
	else if env$('client')="Campbell" then 
		fn_portion_check_generic(amt, 28,57)
	else if env$('client')="Cerro Gordo" then 
		fn_portion_check_cerrogordo(amt)
	else if env$('client')="Cerro Gordo T" then 
		fn_portion_check_generic(amt, 28,55)
	else if env$('client')="Crocket County" then 
		fn_portion_check_Crocket(amt)
	else if env$('client')="Divernon" then 
		fn_portion_check_divernon(amt)
	else if env$('client')="Edison" then 
		fn_portion_check_edison(amt)
	else if env$('client')="Kimberling" then 
		fn_portion_check_kimber(amt)
	else if env$('client')="Lovington" then 
		fn_portion_check_generic(amt, 29)
	else 
		fn_portion_check_generic(amt)
	end if 
fnend 
def fn_portion_check_generic(dolamt; length,posDate)
	! r: gather information, etc
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	! /r
	if env$('client')="ACS" or env$('client')="Thomasboro" or env$('client')="Hope Welty" or env$('client')="Philo" or env$('client')="Divernon" then goto L1730 ! don't skip
	pr #255: "" ! line 1
	L1730: !
	skipline=9
	if prenum=2 then 
		pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1 ! line 2,3, 4
		skipline-=3
	end if
	pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: "" ! line 13
	if posDate then
		a=posDate
	else
		a=65
		if env$('client')="Bethany" then a=54
		if env$('client')="Thomasboro" or env$('client')="Unity" then a=55
		if env$('client')="Hope Welty" or env$('client')="Philo" then a=55
		if env$('client')="Monticello" or env$('client')="Edinburg" then a=55
	end if
	if env$('client')='Campbell' then pr #255: ''
	pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$ ! line 14
	if env$('client')<>'Campbell' then pr #255: ''
	pr #255: "" ! line 16
	if env$('client')="Cerro Gordo T"  then pr #255: ""
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j) ! lines 17-20
	next j
	if length=0 then ! do it the old way
		skipline=6
		if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
		! if env$('client')="Washington Parrish" then skipline=skipline-1
		if env$('client')="ACS" or env$('client')="Hope Welty" then skipline=skipline+2
		if env$('client')="Philo" or env$('client')="Thomasboro" then skipline=skipline+2
		! if env$('client')="PiattCO" then skipline=skipline+4
		for j=1 to skipline
			pr #255: ""
		next j
	else
		for lineItem=21 to length  ! default length is 27, i think
			pr #255: ''
		nex lineItem
	end if
fnend 
def fn_portion_check_ashgrove(dolamt)
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	fn_englishdollar(dolamt)
	x=3 ! 1  add three lines after top stub
	for j=1 to x
		pr #255: ""
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	skipline=9
	if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
	if prenum=2 then skipline=max(skipline-3,1)
	pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
	a=65
	pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ""
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j)
	next j
	skipline=6
	if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline+3
		pr #255: ""
	next j
fnend 
def fn_portion_check_kimber(dolamt)
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6480
	goto L6490
	L6480: mat b$=("")
	L6490: fn_englishdollar(dolamt)
	x=1
	for j=1 to x
		pr #255: ""
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	skipline=9
	if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
	if prenum=2 then skipline=max(skipline-3,1)
	pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: ""
	pr #255,using 'Form POS 60,PIC(ZZ/ZZ/ZZ),X 7,C 18': prdmmddyy,ca$
	pr #255: ""
	pr #255: ""
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j)
	next j
	skipline=8
	if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ""
	next j
fnend 
def fn_portion_check_Crocket(dolamt)
	! r: this is the same as generic, but with spacing for crocket
	! if env$('acsDeveloper')<>"" then pause
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	pr #255,using 'form skip 3,c 1': " " ! line 1
		! for j=1 to 4 ! just print name here
	pr #255,using "Form Pos 8,C 30": b$(1) ! lines 17-20
	! next j
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	skipline=0
	if prenum=2 then 
		pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1 ! line 2,3, 4
		skipline-=3
	end if
		a=60
	pr #255,using 'Form pos 8, c 30,POS A,PIC(ZZ/ZZ/ZZ),X 5,C 18': trim$(b$(2)),prdmmddyy,ca$ ! line 14
	pr #255,using "Form Pos 8,C 30": b$(3) ! line 16
	pr #255,using "Form Pos 8,C 30": b$(4) ! last address line or blank for spacing 
	pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
	pr #255: "" ! spacing 
	if length=0 then ! do it the old way
		skipline=12
		if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
		for j=1 to skipline
			pr #255: ""
		next j
	else
		for lineItem=21 to length  ! default length is 27, i think
			pr #255: ''
		nex lineItem
	end if
	! /r
fnend
def fn_portion_check_divernon(dolamt)
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey L6690
	goto L6700
	L6690: mat b$=("")
	L6700: fn_englishdollar(dolamt)
	x=1
	for j=1 to x
		pr #255: ""
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	for j=1 to 9
		pr #255: " "
	next j
	a=62
	pr #255,using 'Form POS A,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	skipline=2
	pr #255,using 'Form SKIP SKIPLINE,POS 4,C 80,SKIP 1,POS 9,C 70': eng$(1:n), eng$(n+1:128)
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j)
	next j
	skipline=6
	for j=1 to skipline
		pr #255: ""
	next j
fnend 
def fn_portion_check_acs(dolamt)
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))="" then 
		b$(2)=b$(3) : b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	pr #255: ""
	skipline=9
	if prenum=2 then pr #255,using "form skip 2,pos 74,n 8,skip 1": ckn1
	if prenum=2 then skipline=max(skipline-3,1)
	pr #255,using "form skip 1, pos 80,pic(zz/zz/zz)": prdmmddyy
	pr #255: ""
	pr #255,using 'Form skip 1,POS 15,C 30,pos 73,c 18': b$(1),ca$
	pr #255: ""
	pr #255,using 'Form SKIP 1,POS 9,C 80,SKIP 1,POS 9,C 70': eng$(1:n),eng$(n+1:128)
	pr #255: ""
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j)
	next j
	skipline=10
	if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ""
	next j
fnend 
def fn_portion_check_cerrogordo(dolamt)
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	fn_englishdollar(dolamt)
	x=2
	for j=1 to x
		pr #255: ""
	next j
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	skipline=8
	if prenum=2 then pr #255,using "form skip 3,pos 74,n 8,skip 1": ckn1
	if prenum=2 then skipline=max(skipline-3,1)
	skipline=skipline-2
	pr #255,using 'Form SKIP SKIPLINE,POS 9,C 80,SKIP 1,POS 9,C 70,SKIP 1': eng$(1:n), eng$(n+1:128)
	pr #255: ""
	a=55
	pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	for j=1 to 4
		pr #255,using "Form Pos 8,C 30": b$(j)
	next j
	skipline=6
	if scc$="SCC" then skipline=skipline-1 ! don't space as far if stub,check,check
	for j=1 to skipline
		pr #255: ""
	next j
fnend 
def fn_portion_check_billings(dolamt)
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
! 
	pr #255: ""
	pr #255,using 'form pos 42,c 38': "Void After 60 Days"
	pr #255: ""
	pr #255: ""
	pr #255: ""
	if trim$(eng$(n+1:128))='' then 
		pr #255: ""
		pr #255,using 'Form POS 9,C 80': eng$(1:n)
	else 
		pr #255,using 'Form POS 9,C 80': eng$(1:n)
		pr #255,using 'Form POS 9,C 70': eng$(n+1:128)
	end if 
	pr #255,using 'Form POS 59,PIC(ZZ/ZZ/ZZ),X 4,C 18': prdmmddyy,ca$
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255,using "Form Pos 8,C 30": b$(1)
	pr #255,using "Form Pos 8,C 30": b$(2)
	pr #255,using "Form Pos 8,C 30": b$(3)
	pr #255,using "Form Pos 8,C 30": b$(4)
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
fnend 
def fn_portion_check_edison(dolamt)
	mat b$=("")
	read #h_vf1,using 'Form POS 9,4*C 30,POS VP1,2*PD 3',key=holdvn$,release: mat b$ nokey ignore
	if trim$(b$(2))="" then 
		b$(2)=b$(3): b$(3)=b$(4) : b$(4)=""
	else if trim$(b$(3))="" then 
		b$(3)=b$(4) : b$(4)=""
	end if 
	fn_englishdollar(dolamt)
	if dolamt=0 then eng$='        *** V O I D ***'
	if dolamt<=0 then ca$="***VOID***" else ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",dolamt))
	! 
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255: ''
	pr #255,using 'Form POS 74,PIC(ZZ/ZZ/ZZ),pos 82,N 8': prdmmddyy,ckn1
	pr #255: ''
	pr #255: ''
	if trim$(eng$(n+1:128))='' then 
		pr #255: ""
		pr #255,using 'Form POS 9,C 80': eng$(1:n)
	else 
		pr #255,using 'Form POS 9,C 80': eng$(1:n)
		pr #255,using 'Form POS 9,C 70': eng$(n+1:128)
	end if 
	pr #255,using 'Form POS 79,C 18': ca$   ! line 11
	pr #255: ""
	pr #255,using "Form Pos 8,C 30": b$(1)
	pr #255,using "Form Pos 8,C 30": b$(2)
	pr #255,using "Form Pos 8,C 30": b$(3)
	pr #255,using "Form Pos 8,C 30": b$(4)
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
def fn_scr_main_questions(;___,ck)
	if ~smq_setup then
		smq_setup=1
		dim layoutOption$(4)*18,scc$(4)*3
		layoutOption$(1)="Stub, Check, Stub"  : scc$(1)="SCS"
		layoutOption$(2)="Check, Stub, Stub"  : scc$(2)="CSS"
		layoutOption$(3)="Stub, Stub, Check"  : scc$(3)="SSC"
		layoutOption$(4)="Stub, Check, Check" : scc$(4)="SCC"
		fncreg_read('Check Layout Option',layoutOptionSelected$, layoutOption$(1))
	end if
	ck=0
	fnTos(sn$="ckprt1a")
	respc=0
	fnLbl(1,1,"Method of Printing checks:",38,1)
	fnOpt(1,40,"Enter and pr Checks",0)
	if ckoption<=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(2,40,"Print Checks for Selected Invoices",0)
	if ckoption=2 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnOpt(3,40,"Reprint from Check History",0)
	if ckoption=3 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
	fnLbl(5,1,"Date of Checks:",38,1)
	fnTxt(5,40,10,0,1,"3",0,"")
	resp$(respc+=1)=date$("ccYYMMDD")
	fnLbl(6,1,"Beginning check number:",38,1)
	fnTxt(6,40,8,0,1,"30",0,"Next available check #. If reprinting checks from history, this check # is not applicable.")
	resp$(respc+=1)=str$(ckn)
	fnLbl(7,1,"Bank Account:",38,1)
	fncombof("Bankmstr",7,40,20,"[Q]\CLmstr\bankmstr.h[cno]",1,2,3,15,"[Q]\CLmstr\Bankidx1.h[cno]",1,0, "Select bank account for printing")
	resp$(respc+=1)=str$(bankcode)
	fnLbl(8,1,"Check Format:",38,1)
	fncomboa("ckprt-2",8,40,mat layoutOption$)
	resp$(respc+=1)=layoutOptionSelected$
	!   if env$('client')="Washington Parrish" then resp$(respc)=layoutOption$(4)
	if env$('client')="Billings" or (env$('client')="ACS"and bankcode=2) then resp$(respc)=layoutOption$(2)
	! need button to show totals
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck<>5 then 
		for j=1 to 3
			if resp$(j)='True' then ti1=j : ckoption=j
		next j
		prd=val(resp$(4)) ! date of checks
		prdmmddyy=val(resp$(4)(5:6))*10000+val(resp$(4)(7:8))*100+val(resp$(4)(3:4)) ! convert date back to mmddyy format
		ckn=val(resp$(5)) ! beginning ck number
		bankcode=val(resp$(6)(1:3))
		layoutOptionSelected$=resp$(7)
		for j=1 to 4
			if trim$(layoutOptionSelected$)=trim$(layoutOption$(j)) then scc$=scc$(j)
		next j
		fncreg_write('Check Layout Option',layoutOptionSelected$)
	end if  ! ck<>5
	fn_scr_main_questions=ck
fnend 