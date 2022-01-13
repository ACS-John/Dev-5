! Replace S:\acsCL\Label
! pr labels for payees
! r: setup stuff
	autoLibrary
	on error goto Ertn

	dim resp$(64)*256

	fnTop(program$)

	dim optSelection$(3)
	optSelection$(print_all=1       )="[All]"
	optSelection$(check_range=2     )="Range of Checks"
	optSelection$(specific_payees=3 )="Specific payees"
goto MAIN ! /r

omitAddress=1

MAIN: ! r:
	fnTos
	respc=0 : mylen=25 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,"Print Labels For:",mylen,1)
	! fnLbl(10,70," ")
	fncomboa("cllabels",lc,mypos,mat optSelection$,"The labels can be printed in Customer Number order,Customer Name order, or in Bar Code sequence")
	resp$(respc+=1)=optSelection$(1)
	lc+=1
	fnChk(lc+=1,mypos+2,'Print Payee Number',1)
	resp$(resp_enablePayeeNumber:=respc+=1)='False'
	fnChk(lc+=1,mypos+2,'Print Payee Address',1)
	resp$(resp_enablePayeeAddress:=respc+=1)='True'
	lc+=1
	fnLbl(lc+=1,1,"Bank:",mylen,1)
	fncombof('Bank',lc,mypos,33,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",1)
	resp$(resp_bank:=respc+=1)=''
	fnLbl(lc+=1,1,"Starting Check Number:",25,1)
	fncombof('Check',lc,mypos,33,"[Q]\CLmstr\TrMstr.h[cno]",4,8,36,35)
	resp$(resp_checkStart:=respc+=1)=''
	fnLbl(lc+=1,1,"Ending Check Number:",25,1)
	fncombof('Check',lc,mypos,33,"[Q]\CLmstr\TrMstr.h[cno]",4,8,36,35)
	resp$(resp_checkEnd:=respc+=1)=''
	fnLbl(lc+=1,1,"Starting Payee Number:",25,1)
	! fnTxt(lc,27,8,0,1,'',0,'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"')!
	! rESP$(RESPC+=1)=''
	fncombof("Payee",lc,27,20,"[Q]\CLmstr\PayMstr.h[cno]",1,8,9,20,"[Q]\CLmstr\Payidx1.h[cno]",1,0, "Select starting payee record for printing")
	resp$(resp_payee:=respc+=1)=''
	fnLbl(lc+=1,1,'.',70,1) ! just so the right side of the comboboxes for checks can be seen
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else if resp$(1)=optSelection$(1) then
		prtall=print_all
	else if resp$(1)=optSelection$(2) then
		prtall=check_range
	else if resp$(1)=optSelection$(3) then
		prtall=specific_payees
	end if
	printpayeenum$=resp$(resp_enablePayeeNumber)
	printPayeeAddress$=resp$(resp_enablePayeeAddress)
	
	wbc=val(resp$(resp_bank)(1:2)) ! working bank code
	c1 =val(resp$(resp_checkStart)(1:8)) ! starting check number
	c2 =val(resp$(resp_checkEnd)(1:8)) ! ending check number
	dim vn$*8 ! starting payee number
	vn$=lpad$(rtrm$(resp$(resp_payee)(1:8)),8)
	open #hPaymstr=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",i,i,k
	dim nam$*30,ad1$*30,ad2$*30,csz$*30
	open #hTrmstr=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",i,i,k
	if prtall=check_range then
		if wbc=0 or c1=0 then
			dim message$(0)*256
			mat message$(0)
			fnAddOneC(mat message$,'To utilize Check Range you must specify')
			fnAddOneC(mat message$,'a bank account and a ')
			fnAddOneC(mat message$,'a starting check number')
			fnmsgbox(mat message$)
			goto MAIN ! if failed to set bank account information
		end if
		gosub ProcessCheckRange
		goto Finis
	else if prtall=print_all then
		goto ReadForFirstPayee
	end if
! /r
ASK_VN: ! r:
	fnTos
	respc=0 : mylen=20
	fnLbl(1,1,"Payee to Print:",mylen,1)
	fncombof("Payee",1,22,20,"[Q]\CLmstr\PayMstr.h[cno]",1,8,9,20,"[Q]\CLmstr\Payidx1.h[cno]",1,0, 'If you wish to start with a specific payee, enter their number.  Only appllicable to printing "All Payees"')
	resp$(respc+=1)=''
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Finis
	else
		vn$=lpad$(rtrm$(resp$(1)(1:8)),8)
	end if
goto GetStarted ! /r

GetStarted: ! r:
	read #hPaymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey ASK_VN
	goto L530
	ReadForFirstPayee: !
	if trim$(vn$)='' or trim$(vn$)="0" then goto READ_PAYMSTR
	vn$=lpad$(rtrm$(vn$),8)
	read #hPaymstr,using 'Form Pos 1,C 8,4*C 30',key>=vn$: vn$,nam$,ad1$,ad2$,csz$ eof Finis
	goto L520
	READ_PAYMSTR: !
	read #hPaymstr,using 'Form Pos 1,C 8,4*C 30': vn$,nam$,ad1$,ad2$,csz$ eof Finis
	L520: !
	if prtall=check_range and curbal=0 then
		goto READ_PAYMSTR
	end if
	L530: !
	fn_payeeAddLabel(vn$,nam$,ad1$,ad2$,csz$)
	if prtall=specific_payees then
		goto ASK_VN
	end if
goto READ_PAYMSTR ! /r

Finis: ! r:
	close #hPaymstr:
	dim pt$(5)
	fnlabel(mat pt$)
goto Xit ! /r
Xit: fnXit

ProcessCheckRange: ! r: uses: ebc,c1,hTrmstr,hPaymstr
	tr4$=cnvrt$("N 2",wbc)&str$(1)&cnvrt$("N 8",c1)
	restore #hTrmstr,key>=tr4$: nokey EO_OPT2
	do
		READ_TRMSTR: !
		read #hTrmstr,using 'Form POS 4,C 8,POS 28,C 8': checkNumber$,vn$ eof EO_OPT2
		if c2>0 and checkNumber$>lpad$(str$(c2),8) then goto EO_OPT2
		read #hPaymstr,using 'Form Pos 1,C 8,4*C 30',key=vn$: vn$,nam$,ad1$,ad2$,csz$ nokey READ_TRMSTR
		if printPayeeAddress$<>'True' then
			ad1$=ad2$=csz$=''
		end if
		fn_payeeAddLabel(vn$,nam$,ad1$,ad2$,csz$)
	loop
	EO_OPT2: !
return ! /r
def fn_payeeAddLabel(vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30) ! uses local printpayeenum$
	dim labeltext$(5)*120
	mat labeltext$=('')
	if printpayeenum$='True' then 
		labeltext$(1)=vn$
	end if
	labeltext$(2)= nam$
	if printPayeeAddress$='True' then
		labeltext$(3)=ad1$
		labeltext$(4)=ad2$
		labeltext$(5)=csz$
		if trim$(labeltext$(3))='' then
			labeltext$(3)=labeltext$(4)
		else
			labeltext$(4)=''
		end if
		if trim$(labeltext$(4))='' then
			labeltext$(4)=labeltext$(5)
			labeltext$(5)=''
		end if
	end if
	! labelAddCount+=1
	fnaddlabel(mat labeltext$)
fnend

include: ertn
