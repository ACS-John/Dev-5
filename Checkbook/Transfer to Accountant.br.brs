! Transfer to Accountant
 
autoLibrary
fnTop(program$)
on error goto Ertn
 
	dim de$*35
	dim td$*30
	dim prd(23)
	dim srgln$(17)*12
	dim sra(17)
	dim prgln$(15)*12
	dim pra(15)
	dim glwk$*256
	dim ml$(0)*128
 
	dim dat$*20
	fndat(dat$,1)
	dim ty$(3)*20
	ty$(1)=' Check Number:'
	ty$(2)=' Deposit Number:'
	ty$(3)=' Adjustment Number: '
 
MAIN: ! r:
	fnTos
	mylen=40 : mypos=mylen+2 : lc=0
	fnLbl(lc+=1,1,'Starting Date:',mylen,1)
	fnTxt(lc,mypos,10,0,1,'1003',0,'Earliest transation date to be transferred')
	! rESP$(1)=''
	fnLbl(lc+=1,1,'Ending Date:',mylen,1)
	fnTxt(lc,mypos,10,0,1,'1003',0,'Last transation date to be transferred')
	! rESP$(2)=''
	lc+=1
	fnChk(lc+=1,mypos,'Transfer previously posted transactions:',1)
	if resp$(3)='' then resp$(3)='False'
	lc+=1
	fnLbl(lc+=1,1,'Destination Path:',mylen,1)
	fnTxt(lc,mypos,66)
	if resp$(4)='' then resp$(4)='A:\'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1)(5:6))*10000+val(resp$(1)(7:8))*100+val(resp$(1)(3:4)) ! beginning date
	d2=val(resp$(2)(5:6))*10000+val(resp$(2)(7:8))*100+val(resp$(2)(3:4)) ! ending date  ! convert dates back to mmddyy
	if resp$(3)(1:1)='T' then pvt$='Y': else pvt$='N'
		! post previously entries
	dv$=resp$(4)
	gosub GETPRC
 
	fnwait
	open #20: 'Name=[Q]\CLmstr\Company.h[cno],Shr',i,outi,r
	read #20,using 'form pos 618,10*N 1': mat dedcode
	close #20:
	d2$=cnvrt$('PIC(######)',d2)
	open #20: 'Name=[Q]\GLmstr\glBucket.h[cno],Shr',i,i,r ioerr L400
	read #20,using 'form pos 1,N 1',rec=1: glb noRec ignore
	close #20:
	L400: !
	if glb=2 then 
		glwk$='[Q]\GLmstr\GL'&d2$&'.h[cno]'
	else
		glwk$='[Q]\GLmstr\GL_Work_[acsUserId].h[cno]'
	end if
	open #trmstr=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr',i,outIn,k
	open #tralloc=2: 'Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr',i,i,k
	open #glwk101=3: 'Name=[Q]\CLmstr\GLWK101.h[cno],Size=0,RecL=104,Replace',internal,output
	open #glwk201=4: 'Name=[Q]\CLmstr\GLWK201.h[cno],Size=0,RecL=110,Replace',internal,output
	open #bankmstr=5: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',i,i,k
	open #paymstr=6: 'Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr',i,i,k
	fnopenprn
	gosub HDR
goto READ_TRMSTR ! /r
READ_TRMSTR: ! r: main loop
	read #trmstr,using 'form pos 1,G 2,N 1,C 8,N 6,pd 10.2,C 8,C 35,N 1,X 6,N 1': bk$,cde,tr$,tr4,amt,ven$,de$,pcde,scd eof Finis
	if bk$><hbk$ then gosub CONTRA
	hbk$=bk$
	! IF SCD=2 THEN GOTO 'ASDF2'
	if amt=0 then goto READ_TRMSTR
	tr$=lpad$(rtrm$(tr$),12)
	pr1=0
	if scd=4 then pr1=val(ven$) conv L590
	L590: !
	if pvt$='Y' then goto L610
	if pcde=1 then goto READ_TRMSTR
	L610: !
	if fndate_mmddyy_to_ccyymmdd(tr4)<fndate_mmddyy_to_ccyymmdd(d1) then goto READ_TRMSTR
	if fndate_mmddyy_to_ccyymmdd(tr4)>fndate_mmddyy_to_ccyymmdd(d2) then goto READ_TRMSTR
	gosub CKALLOC
	if tal<>amt then
		pr #255: 'Bank Code: '&bk$&ty$(cde)&ltrm$(tr$)&' Total: '&ltrm$(cnvrt$('N 10.2',amt))&' Allocations: '&ltrm$(cnvrt$('N 10.2',tal))&' Entry Skipped'
	end if
	amt=typ=0
	read #paymstr,using 'form pos 134,N 2',key=ven$: typ nokey ignore
	if typ=0 then ven$=''
	bgl$=''
	read #bankmstr,using 'form pos 33,C 12',key=bk$: bgl$ nokey ignore
	key$=bk$&str$(cde)&tr$(5:12)
	restore #tralloc,key=key$: nokey EO_TRALLOC
	READ_TRALLOC: !
	read #tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,tr5,td$,ivd eof EO_TRALLOC
	if key$<>newkey$ then goto EO_TRALLOC
	td$=de$(1:30) ! SEND NAME TO GL INSTEAD OF DESCRIPTION
	if tr5=0 then goto READ_TRALLOC
	tr6=cde
	if tr6>1 then tr5=-tr5
	if prc$='Y' and scd=4 then goto COMBINEPR
	
	WRITE_GLWK101: !
	write #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$,tr4,tr5,tr6,0,tr$,td$,ven$,'','','',bgl$
	AFTER_WRITE_GLWK101: !
	amt+=tr5
	if tr6=3 then
		write #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,tr4,-tr5,tr6,0,tr$,td$,ven$,'','','',bgl$
	end if
	if pr1 then
		if ivd=1 then prd(4)+=tr5
		if ivd>1 and ivd<5 then prd(ivd+3)=-tr5
		if ivd=15 then prd(8)=-tr5
		if ivd>4 and ivd<15 and dedcode(ivd-4)=2 then tr5=-tr5
		if ivd>4 and ivd<15 then prd(ivd+4)=-tr5
		if ivd=16 then prd(19)=-tr5
		if fp(ivd*.01)=.19 then prd(20)=int(ivd*.01)
	end if
	goto READ_TRALLOC
 
	EO_TRALLOC: !
	rewrite #trmstr,using 'form pos 71,N 1': 1
	if pr1 then
		prd(1)=pr1 : prd(2)=tr4
		prd(3)=val(tr$) conv ignore
		prd(22)=amt
		write #glwk201,using 'form pos 1,N 4,2*PD 4,19*PD 5.2,PD 3': mat prd
	end if
	mat prd=(0)
	if cde=2 then p1=68 else p1=56
	if cde=3 and amt>0 then t1+=amt: t2=t2-amt: goto L1080
	if cde=3 and amt<0 then t2+=amt: t1=t1-amt: goto L1110
	if cde=2 then
		c2+=amt : t2+=amt
	else
		c1+=amt : t1+=amt
	end if
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE
	goto THERE
	L1080: !
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE: p1=68
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(tr$),tr4,de$,-amt pageoflow NEWPGE
	goto THERE
	L1110: !
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(tr$),tr4,de$,-amt pageoflow NEWPGE: p1=68
	pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(tr$),tr4,de$,amt pageoflow NEWPGE
	THERE: !
goto READ_TRMSTR ! /r
 
NEWPGE: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
 
HDR: ! r:
	pr #255,using 'form pos 1,C 8,CC 76': date$,env$('program_caption')
	pr #255,using 'form pos 1,C 8,pos 31,C 40': time$,'Transferred to General Ledger'
	pr #255,using 'form pos 1,C 4,N 4,CC 76': 'Page',pg+=1,dat$
	pr #255: ''
	pr #255: 'Ref-Numb    Date    Payee/Description                      Checks     Deposits'
	pr #255: '________  ________  ___________________________________  __________  __________'
return ! /r
 
Finis: ! r:
	if pri1=1 then gosub PRWRITE
	gosub CONTRA
	pr #255: tab(56);'  ____________  ____________'
	pr #255,using 'form pos 56,2*N 14.2': t1,t2
	fncloseprn
	close #1:
	close #tralloc:
	close #glwk101:
	lr4=lrec(glwk201)
	close #glwk201:
	close #bankmstr:
	if trim$(dv$)='' then goto TRY_TO_SEND_TO_GL
	execute 'Copy [Q]\CLmstr\GLWk101.h[cno] '&dv$&' -n' ioerr MSGBOX1
	execute 'Copy [Q]\CLmstr\GLWk201.h[cno] '&dv$&' -n'
	execute 'Copy [Q]\CLmstr\PayMstr.h[cno] '&dv$&' -n'
	execute 'Copy [Q]\CLmstr\PayIdx1.h[cno] '&dv$&' -n'
goto Xit ! /r
Xit: fnXit
 
TRY_TO_SEND_TO_GL: ! r:
	if glb=2 then goto BUCKET
	fnCopy('[Q]\CLmstr\GLWK101.h[cno]','[Q]\GLmstr\GL_Work_[acsUserId].h[cno]')
	fnCopy('[Q]\CLmstr\GLWK201.h[cno]','[Q]\GLmstr\GLWK2[acsUserId].h[cno]')
	if lr4=0 then goto L1550
	open #1: 'Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr',i,outIn,k ioerr L1550
	fnprg('S:\acsGL\PRMerge',2)
	fnstyp(99)
	L1550: !
fnchain('S:\General Ledger\Merge') ! /r
 
CONTRA: ! r:
	t9$='999999999999'
	if trim$(dv$)='' then
		if c1 then
			td$='CHECKS   '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1)&' - '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2)
			write #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,d2,-c1,1,0,t9$,td$,'','','','',bgl$
			p1=68
			t2=t2-c1
			pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(t9$)(1:10),d2,td$(1:35),-c1 pageoflow NEWPGE
		end if
		if c2 then
			td$='DEPOSITS '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1)&' - '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2)
			write #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': bgl$,d2,-c2,2,0,t9$,td$,'','','','',bgl$
			p1=56
			t1=t1-c2
			pr #255,using 'form pos 1,C 10,PIC(ZZ/ZZ/ZZ),X 2,C 35,pos P1,N 13.2': ltrm$(t9$)(1:10),d2,td$(1:35),-c2 pageoflow NEWPGE
		end if
	end if
	c1=c2=0
return ! /r
 
GETPRC: ! r:
	pri1=0
	open #7: 'Name=[Q]\PRmstr\Company.h[cno],Shr',internal,input ioerr GetPrcFinis
	read #7,using 'form pos 437,15*C 12': mat prgln$
	close #7:
	pri1=1
	! pr newpage
	! close #103: ioerr ignore
	! open #103: 'SROW=9,SCOL=09,EROW=14,ECOL=70,BORDER=SR,Caption=<'&env$('program_caption'),display,outIn
	mat ml$(4) : mat ml$=('')
	ml$(1)='This program can combine all like General Ledger Numbers for'
	ml$(2)='Payroll Withholding Accounts as they are transferred to GL.'
	ml$(4)='Do you wish to combine these accounts?'
	fnmsgbox(mat ml$,resp$,'',3)
	if resp$='Cancel' then goto Xit else prc$=resp$(1:1)
	GetPrcFinis: !
return ! /r
 
COMBINEPR: ! r:
	for j=1 to 15
		if prgln$(j)=gl$ then goto L1900
	next j
	goto WRITE_GLWK101
	L1900: !
	pra(j)=pra(j)+tr5
goto AFTER_WRITE_GLWK101 ! /r
 
PRWRITE: ! r:
	td$='PR-WH: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1)&' - '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d2)
	for j=1 to 15
		if pra(j) then
			write #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': prgln$(j),d2,pra(j),1,0,str$(d2),td$,'','','','',bgl$
		end if
	next j
return ! /r
 
CKALLOC: ! r:
	tal=0
	key$=bk$&str$(cde)&tr$(5:12)
	restore #tralloc,key=key$: nokey EO_CKALLOC
	CKALLOC_READ_TRALLOC: !
	read #tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6': newkey$,gl$,tr5,td$,ivd eof EO_CKALLOC
	if key$<>newkey$ then
		goto EO_CKALLOC
	else
		tal+=tr5
		goto CKALLOC_READ_TRALLOC
	end if
	EO_CKALLOC: !
return ! /r
 
BUCKET: ! r: MOVE TO GLBUCKET
	open #glwk101=3: 'Name=[Q]\CLmstr\GLWK101.h[cno]',internal,input
	open #9: 'Name='&glwk$&',RecL=104,USE',internal,output
	do
		read #glwk101,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$, tr4,tr5,tr6,tr7, tr$,td$,ven$,j$,j$,j$,bgl$ eof L2160
		write #9,using 'form pos 1,C 12,N 6,PD 6.2,2*N 2,C 12,C 30,C 8,C 6,C 5,C 3,C 12': gl$,tr4,tr5,tr6,tr7,tr$,td$,ven$,j$,j$,j$,bgl$
	loop
	L2160: !
	open #1: 'Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr',i,outIn,k ioerr Xit
fnchain('S:\acsGL\PRMerge') ! /r
 
MSGBOX1: ! r:
	mat ml$(2)
	ml$(1)='Make sure the diskette is properly inserted '
	ml$(2)='and the proper device has been selected.'
	fnmsgbox(mat ml$,resp$,'',16)
goto MAIN ! /r
 
include: ertn
