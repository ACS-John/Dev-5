! Replace S:\acsGL\ACGLMrge
! GL Merge program, chained to from other systems, 
	! like Checkbook-post to GL; also used to merge entries entered directly
!
	library 'S:\Core\Library': fntop,fnxit, fnprocess,fnchain,fnprg
	library 'S:\Core\Library': fnstyp,fnmsgbox,fnaddglpayee
	library 'S:\Core\Library': fnTos,fnLbl,fnCmdKey,fnAcs,fnagl$,fnTxt,fnCmdSet,fnOpt
	library 'S:\Core\Library': fnqglbig,fnrglbig$,fnindex_it,fnFree
	fntop(program$,"General Ledger Merge")
	on error goto Ertn
!
	dim adr(2),ta(2),prg$*256,k(10,8),gl$(5)*12,gl1(5),tr$(5)*35
	dim t$*12,n(2),l$*12,p$*30,ven$*8
	dim zo(50),d$*50
	dim ml$(3)*80
	dim bank$*25
	dim nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11
	dim fl1$(6)
	dim resp$(40)*256
!
	dim prg$*256
	fnprg(prg$)
	if fnstyp<>99 then 
		if fnstyp=9 then prg$="S:\acsTM\tmMenu" else prg$="S:\acsGL\acGLAuto"
		fnprg(prg$,2)
		open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input 
		read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
		close #company: 
	end if
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative 
	open #3: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],NoShr",internal,outIn 
	open #paymstr=4: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed 
	if ~exists("[Q]\GLmstr\bankrec.H[cno]") then 
		open #6: "Name=[Q]\GLmstr\bankrec.H[cno],KFName=[Q]\GLmstr\bankrec-idx.H[cno],Version=1,RecL=91,use,kps=79/3/4,kln=12/1/8,Shr",internal,outIn,keyed 
		close #6: 
		fnindex_it("[Q]\GLmstr\bankrec.H[cno]","[Q]\GLmstr\bankrec-idx.h[cno]","79/3/4 12/1/8")
	end if
	open #6: "Name=[Q]\GLmstr\BankRec.h[cno],KFName=[Q]\GLmstr\BankRec-idx.h[cno],Shr",internal,outIn,keyed 
	if exists("[Q]\GLmstr\gltr1099.h[cno]")=0 then 
		open #trans=5: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed 
	else 
		open #trans=5: "Name=[Q]\GLmstr\GLTR1099.h[cno],KFName=[Q]\GLmstr\gltrIdx1.h[cno],Shr",internal,outIn,keyed 
	end if
	L360: !
	read #3,using "Form POS 1,C 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8,POS 93,C 12": t$,s,k,mat n,l$,p$,ven$,key$ eof L1450
	prtrans=0
	if n(1)=4 then n(1)=1 : prtrans=1 ! convert payroll transaction types to a regular disbursment
	form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,pos 93,c 12
	if n(2)=9 then goto L360 ! CHECK PREVIOUS POST
	if k=0 and uprc$(ltrm$(rtrm$(p$)))<>"VOID" then goto L360
	if val(t$(1:3))=0 and val(t$(4:9))=0 and val(t$(10:12))=0 and k=0 then goto L360
	if t$(1:3)="   " then t$(3:3)="0"
	if t$(10:12)="   " then t$(12:12)="0"
	L440: !
	read #1,using L450,key=t$: cb,mat ta nokey L840
	L450: form pos 87,pd 6.2,pos 333,2*pd 3
	L460: ! READ #2,USING 460,REC=1: LR2
	lr2=lrec(2)+1
	write #2,using L540,rec=lr2: t$,s,k,mat n,l$,p$,0 duprec L460
	if ta(1)=0 then ta(1)=lr2
	if ta(2)>0 then rewrite #2,using L550,rec=ta(2): lr2
	ta(2)=lr2
	cb=cb+k
	rewrite #1,using L450,key=t$: cb,mat ta
	L540: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
	L550: form pos 71,pd 3
	rewrite #3,using L570: 9
	L570: form pos 27,n 2
BANK_REC_FILE: ! 
	if l$="999999999999" then goto VENDOR_FILE ! don't update bkrec for contra entries
	if n(1)>2 then goto VENDOR_FILE ! only allow receipts or disbursments to bank rec
	l$=trim$(l$): l$=l$(1:8)
	l$=lpad$(rtrm$(l$),8)
	bank$=key$&str$(n(1))&l$
	read #6,using L650,key=bank$: amt nokey WRITE_NEW_BANKREC ioerr VENDOR_FILE
	L650: form pos 18,pd 10.2
	amt=amt+k
	rewrite #6,using L650: amt
	goto VENDOR_FILE
WRITE_NEW_BANKREC: ! 
	bankgl$=key$ 
	tcde=n(1) ! transaction code 
	tr$(1)=lpad$(rtrm$(l$),8) ! ref # 
	tr$(2)=str$(s) !  check date 
	tx3= k ! amount
	tr$(4)=ven$ ! payee
	tr$(5)=p$ ! name or desc 
	pcde=0 ! posting code 
	clr=0 ! cleared date 
	scd=0 ! source code
	if tcde=2 then tx3=-tx3 ! turn sign around on bank rec file for receipts
	write #6,using 'Form POS 79,c 12,pos 3,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bankgl$,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),pcde,clr,scd
	form pos 1,c 12,c 12,c 30,c 2,n 6,pd 5.2,n 1
VENDOR_FILE: ! 
	if rtrm$(ven$)="" or ltrm$(rtrm$(ven$))="0" then goto L360
	if n(1)<>1 or prtrans=1 then goto L360 ! only disbursments and not payroll trans
	ven$=lpad$(rtrm$(ven$),8)
	L790: !
	lr5=lrec(5)+1
	write #5,using L810,rec=lr5,reserve: ven$,s,k,l$,p$,0 duprec L790
	L810: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
goto L360
!
L840: ! 
	fnTos(sn$="GLmerge") 
	mylen=40: mypos=mylen+3 : right=1
	fnLbl(1,10,"  Account Number: "&t$,mylen,left)
	fnLbl(2,10,"            Date: "&str$(s),mylen,left)
	fnLbl(3,10, "          Amount: "&str$(k),mylen,left)
	fnLbl(4,10, "Reference Number: "&l$ ,mylen,left)
	fnLbl(5,10, "     Description: "&p$ ,mylen,left)
	fnLbl(7,5, "This general ledger account does not exist!" ,60,0)
	fnOpt(8,10,"Add this Account",0,0) 
	resp$(1)="True"
	fnOpt(9,10,"Change Account Number",0,0) 
	resp$(1)="False"
	fnCmdKey("&Next",1,1,0,"Allows you to either add the account or change the account #.")
	fnAcs(sn$,0,mat resp$,ckey)
	if resp$(1)="True" then goto ADD
	if resp$(2)="True" then goto CHANGE_ACCOUNTS
	goto L840
!
ADD: ! 
	dno=val(t$(1:3)) conv ignore
	ano=val(t$(4:9)) conv ignore
	sno=val(t$(10:12)) conv ignore
	fnTos(sn$="GLmerge3") 
	mylen=23: mypos=mylen+3 : right=1: rc=0
	if use_dept =1 then let fnLbl(1,26,"Fund #",6,2)
	if use_sub  =1 then let fnLbl(1,40,"Sub #",6,2)
	fnLbl(2,1,"General Ledger Number:",mylen,right)
	if use_dept=1 then 
		let fnTxt(2,26,3,0,right,"30",0,"Enter the fund portion of the general ledger number.",0 ) 
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,31,6,0,right,"30",0,"Enter the main part of the general ledger number.",0 ) 
	resp$(rc+=1)=str$(ano)
	if use_sub=1 then 
		fnTxt(2,40,3,0,right,"30",0,"Enter the sub portion of the general ledger number.",0 ) 
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,right)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=""
! 
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	pas=0
	dno=ano=sno=0
	if use_dept=1 then dno=val(resp$(1)) : ano=val(resp$(2))
	if use_dept=0 then ano=val(resp$(1))
	if use_dept=1 and use_sub=1 then sno=val(resp$(3))
	if use_dept=0 and use_sub=1 then sno=val(resp$(2))
! 
	if use_dept=1 and use_sub=1 then d$=resp$(4)
	if use_dept=0 and use_sub=1 then d$=resp$(3)
	if use_dept=0 and use_sub=0 then d$=resp$(2)
	if use_dept=1 and use_sub=0 then d$=resp$(3)
	key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	read #1,using 'Form POS 1,N 3',key=key$: dno nokey L1300
! msgbox
L1300: mat ta=(0)
	cb=0
	write #1,using L1330: t$,d$,mat zo
L1330: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
	goto L460
!
CHANGE_ACCOUNTS: ! 
	fnTos(sn$="GLmerge4") 
	mylen=23: mypos=mylen+3 : right=1
	fnLbl(1,1,"General Ledger Number:",mylen,right)
	fnqglbig(1,mypos,0,2) 
	resp$(1)=fnrglbig$(gl$)
	fnCmdKey("&Next",1,1,0,"Will change to the selected account.")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto L440
	gl$=fnagl$(resp$(1)) : t$=gl$ : goto L440
!
L1450: if fnstyp><92 then goto L1620
	open #20: "Name=CNo.H"&wsid$,internal,outIn,relative  
	read #20,using "Form POS 239,5*C 12,5*N 10.2",rec=1: mat gl$,mat gl1 conv L1620 
	close #20: 
	ckgl=0
	for j=1 to 5
		if val(gl$(j)(4:9))=0 then goto L1570 else gl2=0
		read #1,using 'form pos 87,pd 6.2',key=gl$(j): gl2 nokey ignore
		if gl1(j)=gl2 then goto L1570
		if ckgl=0 then pr newpage; bell
		pr using 'form pos 1,c 11,c 14,c 15,n 12.2,x 4,c 12,n 12.2,skip 1': "Account #:",gl$(j),"Client Balance:",gl1(j),"GL Balance:",gl2
		ckgl=1
L1570: next j
	if ckgl=0 then goto L1620
	pr f "24,35,Cc 10,B,1": "Next  (F1)"
L1600: input fields "24,2,C 1,AE,N": pause$
	if cmdkey><1 then goto L1600
L1620: close #1: 
	close #2: 
	close #3: 
	fnFree("[Q]\GLmstr\GLPT"&wsid$&".H[cno]")
L1660: ! 
	close #4: ioerr ignore
	if new1=1 or new2=1 then 
		fnindex_it("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
	end if
	if new1=1 then 
		fnindex_it("[Q]\GLmstr\GLmstr.h[cno]","[Q]\GLmstr\GLIndex.h[cno]","1 12")
	end if
	if new2=1 then 
		fnindex_it("[Q]\GLmstr\GL1099.h[cno]","[Q]\GLmstr\GL109IDX.h[cno]","1 8")
	end if
	open #30: "Name=[Q]\GLmstr\Process.h[cno],Shr",internal,outIn,relative ioerr L1760
	read #30,using "form pos 1,n 1",rec=1: process noRec L1760 ! read post payroll code
	rewrite #30,using "form pos 1,n 1",rec=1: 0 noRec L1760 ! clear post payroll code
	close #30: 
	if process=1 or process=4 then let fnchain("S:\acsGL\prMerge")
L1760: goto XIT
!
XIT: fnxit
include: ertn