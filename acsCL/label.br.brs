! Replace S:\acsCL\Label
! pr labels for payees

	library 'S:\Core\Library': fntop,fnxit,fnerror,fnaddlabel,fnlabel,fnTos,fnLbl,fncomboa,fnChk,fnCmdSet,fnAcs,fnTxt,fncombof
	on error goto ERTN

	dim io2$(3),wrd2$(3)
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,prtbegin$*5
	dim labeltext$(5)*120,pt$(5),message$*40,item1$(3),resp$(10)*30

	fntop(program$,"Payee Labels")
	cancel=99 : right=1 : limit_to_list=1 : on=1 
	off=0 : left=0 : center=2

MAIN: ! 
	fnTos(sn$="cllabel-1")
	respc=0 : mylen=25 : mypos=mylen+2
	fnLbl(1,1,"Print Labels For:",mylen,right)
	fi$="cllabels" 
	item1$(print_all=1)="[All]" 
	item1$(check_range=2)="Range of Checks" 
	item1$(specific_payees=3)="Specific payees" 
	fncomboa(fi$,1,mypos,mat item1$,"The labels can be printed in Customer Number order,Customer Name order, or in Bar Code sequence") 
	resp$(respc+=1)=item1$(1)
	fnChk(2,mypos+2,'Print Payee Number on Label',right) 
	resp$(respc+=1)='False'
	fnLbl(4,1,"Bank:",mylen,right)
	fncombof('Bank',4,mypos,33,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list) 
	resp$(respc+=1)=""
	fnLbl(5,1,"Starting Check Number:",25,1)
	fncombof('Check',5,mypos,33,"[Q]\CLmstr\TrMstr.h[cno]",4,8,36,35) 
	resp$(respc+=1)=""
	fnLbl(6,1,"Ending Check Number:",25,1)
	fncombof('Check',6,mypos,33,"[Q]\CLmstr\TrMstr.h[cno]",4,8,36,35) 
	resp$(respc+=1)=""
	fnLbl(8,1,"Starting Payee Number:",25,1)
! fnTxt(8,27,8,0,1,"",0,'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"')! 
	! rESP$(RESPC+=1)=""
	fncombof("Payee",8,27,20,"[Q]\CLmstr\Paymstr.h[cno]",1,8,9,20,"[Q]\CLmstr\Payidx1.h[cno]",1,0, "Select starting payee record for printing") 
	resp$(respc+=1)=""
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck) 
	if ck=5 then 
		goto XIT 
	else if resp$(1)=item1$(1) then 
		prtall=print_all 
	else if resp$(1)=item1$(2) then
		prtall=check_range 
	else if resp$(1)=item1$(3) then 
		prtall=specific_payees
	end if
	printpayeenum$=resp$(2) 
	wbc=val(resp$(3)(1:2)) ! working bank code 
	c1=val(resp$(4)(1:8)) ! starting check number 
	c2=val(resp$(5)(1:8)) ! ending check number 
	vn$=lpad$(rtrm$(resp$(6)(1:8)),8) ! starting vendor number
	open #paymstr=1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,input,keyed 
	open #trmstr=2: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno],Shr",internal,input,keyed 
	if prtall=check_range then 
		goto ASK_BANK_ETC 
	else if prtall=print_all then 
		goto ASK_FIRST_PAYEE
	end if
ASK_VN: ! 
	fnTos(sn$="cllabel-2") 
	respc=0 : mylen=20
	fnLbl(1,1,"Payee to Print:",mylen,right)
	fncombof("Payee",1,22,20,"[Q]\CLmstr\Paymstr.h[cno]",1,8,9,20,"[Q]\CLmstr\Payidx1.h[cno]",1,0, 'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"') 
	resp$(respc+=1)=""
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ck) 
	if ck=5 then 
		goto END1 
	else 
		vn$=lpad$(rtrm$(resp$(1)(1:8)),8)
	end if
	read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey ASK_VN
	goto L530

ASK_FIRST_PAYEE: ! 
	if trim$(vn$)="" or trim$(vn$)="0" then goto READ_PAYMSTR
	vn$=lpad$(rtrm$(vn$),8) 
	read #paymstr,using 'Form Pos 1,C 8,4*C 30',key>=vn$: vn$,nam$,ad1$,ad2$,csz$ eof END1
	goto L520

READ_PAYMSTR: ! 
	read #paymstr,using 'Form Pos 1,C 8,4*C 30': vn$,nam$,ad1$,ad2$,csz$ eof END1
L520: !
	if prtall=check_range and curbal=0 then 
		goto READ_PAYMSTR
	end if
L530: gosub PRT
	if prtall=specific_payees then 
		goto ASK_VN 
	else 
		goto READ_PAYMSTR
	end if

END1: close #paymstr: 
	fnlabel(mat pt$)
XIT: fnxit

OPT2: ! 
	if wbc=0 then goto MAIN ! if failed to set bank account information
	tr4$=cnvrt$("N 2",wbc)&str$(1)&cnvrt$("N 8",c1) 
	restore #trmstr,key>=tr4$: nokey EO_OPT2
READ_TRMSTR: ! 
	read #trmstr,using 'Form POS 4,C 8,POS 28,C 8': ck$,vn$ eof EO_OPT2
	if ck$>lpad$(str$(c2),8) then goto EO_OPT2
	read #paymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey READ_TRMSTR
PRT: ! 
	mat labeltext$=("")
	if printpayeenum$="True" then labeltext$(1)=vn$
	labeltext$(2)= nam$ 
	labeltext$(3)=ad1$ 
	labeltext$(4)=ad2$ 
	labeltext$(5)=csz$
	if trim$(labeltext$(3))="" then 
		labeltext$(3)=labeltext$(4) 
	else
		labeltext$(4)=""
	end if
	if trim$(labeltext$(4))="" then
		labeltext$(4)=labeltext$(5)
		labeltext$(5)=""
	end if
	fnaddlabel(mat labeltext$)
	if prtall=check_range then goto READ_TRMSTR
EO_OPT2: return 

ASK_BANK_ETC: ! 
	gosub OPT2
goto END1

include: ertn