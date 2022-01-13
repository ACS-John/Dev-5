! formerly S:\acsGL\ACGLMrge
! GL Merge program, chained to from GL>Enter Transactions and Other ACS systems too,
! i.e. Checkbook>Post to GL

! pr 'welcome to GL Merge' : pause

autoLibrary
fnTop(program$)
on error goto Ertn
dim resp$(40)*256

fnAutomatedSavePoint('before Merge')

! r: setup constants and open files
	dim prg$*256
	fnprg(prg$)
	if fnstyp<>99 then
		if fnstyp=9 then prg$="S:\Client Billing\Legacy\tmMenu" else prg$="S:\acsGL\acGLAuto"
		fnprg(prg$,2)
		open #hCompany=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
		read #hCompany,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
		close #hCompany:
	end if
	open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	F_Glmstr1: form pos 87,pd 6.2,pos 333,2*pd 3
	F_Glmstr2: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
	open #hGlTrans=fnH: 'Name=[Q]\GLmstr\GLTrans.h[cno],kfname=[Q]\GLmstr\glTrans-IdxAcct.h[cno],Shr',internal,outIn,keyed
	FglTrans: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
	open #hMerge=fnH: "Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],NoShr",internal,outIn
	F_merge1: Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8,POS 93,C 12
	F_merge2: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30
	F_merge3: form pos 27,n 2
	open #hPaymstr=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	if ~exists("[Q]\GLmstr\bankrec.h[cno]") then
		open #hBankRec=fnH: "Name=[Q]\GLmstr\bankrec.h[cno],KFName=[Q]\GLmstr\bankrec-idx.h[cno],Version=1,RecL=91,use,kps=79/3/4,kln=12/1/8,Shr",internal,outIn,keyed
		close #hBankRec:
		fnIndex("[Q]\GLmstr\bankrec.h[cno]","[Q]\GLmstr\bankrec-idx.h[cno]","79/3/4 12/1/8")
	end if
	open #hBankRec=fnH: "Name=[Q]\GLmstr\BankRec.h[cno],KFName=[Q]\GLmstr\BankRec-idx.h[cno],Shr",internal,outIn,keyed
	open #hTr1099=fnH: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed
! /r

do ! r:  main loop - cycle through Merge file
	dim glAcct$*12
	dim l$*12
	dim p$*30
	dim ven$*8
	dim xn(2)
	read #hMerge,using F_merge1: glAcct$,xs,tranAmt,mat xn,l$,p$,ven$,glBank$ eof Finis
	! pr 'read from Merge work file';glAcct$;xs;tranAmt : pause
	prtrans=0
	if xn(1)=4 then xn(1)=1 : prtrans=1 ! convert payroll transaction types to a regular disbursment
	if xn(2)=9 then goto NextMergeRecord ! CHECK PREVIOUS POST
	if tranAmt=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto NextMergeRecord
	! pr 'A. glAcct$="'&glAcct$&'"'



	! r: added because weird Std Adjustments were coming through
	if ~use_dept and ~use_sub then
		if glAcct$(len(glAcct$)-2:len(glAcct$))='  0' then glAcct$(len(glAcct$)-2:len(glAcct$))=''
		glAcct$='  0'&lpad$(str$(val(glAcct$)),6)&'  0'
		goto ReadAccount
	end if
	! /r


	if val(glAcct$(1:3))=0 and val(glAcct$(4:9))=0 and val(glAcct$(10:12))=0 and tranAmt=0 then goto NextMergeRecord
	! pr 'B. glAcct$="'&glAcct$&'"'
	if glAcct$(1:3)="   " then glAcct$(3:3)="0"
	! pr 'C. glAcct$="'&glAcct$&'"'
	if glAcct$(10:12)="   " then glAcct$(12:12)="0"
	! pr 'D. glAcct$="'&glAcct$&'"'

	ReadAccount: !
	dim ta(2)
	! pr 'Z. glAcct$="'&glAcct$&'"' : pause
	read #hAccount,using F_Glmstr1,key=glAcct$: cb,mat ta nokey ScrMissingGl
	WriteTrans: !
	! pr 'writting trans';glAcct$;xs;tranAmt : pause
	write #hGlTrans,using FglTrans: glAcct$,xs,tranAmt,mat xn,l$,p$,0
	lr2=lrec(hGlTrans)
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then rewrite #hGlTrans,using 'form pos 71,pd 3',rec=ta(2): lr2
	ta(2)=lr2
	cb+=tranAmt
	rewrite #hAccount,using F_Glmstr1,key=glAcct$: cb,mat ta
	rewrite #hMerge,using F_merge3: 9
	

 ! r: ! BankRec file
	if l$="999999999999" then goto EoBankRec ! don't update bkrec for contra entries
	if xn(1)>2 then goto EoBankRec ! only allow receipts or disbursments to bank rec
	l$=trim$(l$)(1:8)
	l$=lpad$(rtrm$(l$),8)
	dim bank$*25
	bank$=glBank$&str$(xn(1))&l$
	read #hBankRec,using L650,key=bank$: amt nokey WRITE_NEW_BANKREC ioerr EoBankRec
	L650: form pos 18,pd 10.2
	amt+=tranAmt
	rewrite #hBankRec,using L650: amt
	goto EoBankRec !
	
	WRITE_NEW_BANKREC: !
	bankgl$=glBank$
	tcde=xn(1) ! transaction type
	dim tr$(5)*35
	tr$(1)  =lpad$(rtrm$(l$),8)  	! ref #
	tr$(2)  =str$(xs)             	!  check date
	tx3     =tranAmt              	! amount
	tr$(4)  =ven$                 	! payee
	tr$(5)  =p$                   	! name or desc
	pcde    =0                    	! posting code
	clr     =0                    	! cleared date
	scd     =0                    	! source code
	if tcde=2 then tx3=-tx3 ! turn sign around on bank rec file for receipts
	write #hBankRec,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd
	form pos 1,c 12,c 12,c 30,c 2,n 6,pd 5.2,n 1
	
	EoBankRec: ! /r

	VENDOR_FILE: ! r:
		if rtrm$(ven$)="" or ltrm$(rtrm$(ven$))="0" then goto EoVendorFile
		if xn(1)<>1 or prtrans=1 then goto EoVendorFile ! only disbursments and not payroll trans
		ven$=lpad$(rtrm$(ven$),8)
		! L790: !
		! lr5=lrec(5)+1
		! write #hTr1099,using L810,rec=lr5,reserve: ven$,xs,tranAmt,l$,p$,0 duprec L790
		write #hTr1099,using L810,reserve: ven$,xs,tranAmt,l$,p$,0
		L810: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
	EoVendorFile: ! /r

	NextMergeRecord: !
loop ! /r

ScrMissingGl: ! r:
	do
		fnTos : rc=0
		mylen=40: mypos=mylen+3
		fnLbl(1,10,"  Account Number: "&glAcct$,mylen,left)
		fnLbl(2,10,"            Date: "&str$(xs),mylen,left)
		fnLbl(3,10, "          Amount: "&str$(tranAmt),mylen,left)
		fnLbl(4,10, "Reference Number: "&l$ ,mylen,left)
		fnLbl(5,10, "     Description: "&p$ ,mylen,left)
		fnLbl(7,5, "This general ledger account does not exist!" ,60,0)
		fnOpt(8,10,"Add this Account",0,0)
		resp$(respc_accountAdd=rc+=1)='True'
		fnOpt(9,10,"Change Account Number",0,0)
		resp$(respc_AccountChg=rc+=1)='True'
		resp$(1)='False'
		fnCmdKey("&Next",1,1,0,"Allows you to either add the account or change the account number.")
		ckey=fnAcs(mat resp$)
		if resp$(respc_accountAdd)='True' then
			goto ADD
		else if resp$(respc_AccountChg)='True' then
			! r: Change Account
			fnTos
			mylen=23: mypos=mylen+3
			fnLbl(1,1,"General Ledger Number:",mylen,1)
			fnQglBig(1,mypos,0,2,1)
			resp$(1)=fnRglBig$(gl$)
			fnCmdKey("&Next",1,1,0,"Will change to the selected account.")
			ckey=fnAcs(mat resp$)
			if ckey<>5 then
				glAcct$=gl$=fnAgl$(resp$(1))
			end if
			! /r
			goto ReadAccount
		end if
	loop 
! /r
	ADD: ! r:
		dno=val(glAcct$(1:3)) conv ignore
		ano=val(glAcct$(4:9)) conv ignore
		sno=val(glAcct$(10:12)) conv ignore
		fnTos
		mylen=23 : mypos=mylen+3 : rc=0
		if use_dept then
			fnLbl(1,26,"Fund #",6,2)
			fnTxt(2,26,3,0,1,"30",0,"Fund portion of the general ledger number",0 )
			resp$(rc+=1)=str$(dno)
		end if
		fnLbl(2,1,"General Ledger Number:",mylen,1)
		fnTxt(2,31,6,0,1,"30",0,"Main part of the general ledger number",0 )
		resp$(rc+=1)=str$(ano)
		if use_sub then
			fnLbl(1,40,"Sub #",6,2)
			fnTxt(2,40,3,0,1,"30",0,"Sub portion of the general ledger number",0 )
			resp$(rc+=1)=str$(sno)
		end if
		fnLbl(3,1,"Description:",mylen,1)
		fnTxt(3,mypos,50,0,left,"",0,"Account description",0 )
		resp$(rc+=1)=""

		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey=5 then 
			goto ScrMissingGl  		! todo:  XXX new cancel logic   should probably be tested
		else 
			pas=0
			dno=ano=sno=0
			if use_dept then dno=val(resp$(1)) : ano=val(resp$(2))
			if ~use_dept then ano=val(resp$(1))
			if use_dept and use_sub then sno=val(resp$(3))
			if ~use_dept and use_sub then sno=val(resp$(2))
	
			dim d$*50
			if use_dept and use_sub then d$=resp$(4)
			if ~use_dept and use_sub then d$=resp$(3)
			if ~use_dept and ~use_sub then d$=resp$(2)
			if use_dept and ~use_sub then d$=resp$(3)
			glBank$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
			read #hAccount,using 'Form POS 1,N 3',key=glBank$: dno nokey ignore
			mat ta=(0)
			cb=0
			dim zo(50)
			write #hAccount,using F_Glmstr2: glAcct$,d$,mat zo
			goto WriteTrans 
		end if
	! /r


Finis: ! r:
	fncreg_write('Enter Transaction - Process Ending Date','')
	! fncreg_write('Enter Transaction - Bank Account','')
	close #hAccount: ioerr ignore
	close #hGlTrans: ioerr ignore
	close #hMerge: ioerr ignore
	close #hPaymstr: ioerr ignore
	close #hTr1099: ioerr ignore
	close #hBankRec: ioerr ignore

	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")

	open #30: "Name=[Q]\GLmstr\Process.h[cno],Shr",i,outi,r ioerr L1760
	read #30,using "form pos 1,n 1",rec=1: process noRec L1760 ! read post payroll code
	rewrite #30,using "form pos 1,n 1",rec=1: 0 noRec L1760 ! clear post payroll code
	L1760: !
	close #30: ioerr ignore

	if process=1 or process=4 then fnchain("S:\acsGL\prMerge")

goto Xit ! /r

Xit: fnXit


include: ertn
