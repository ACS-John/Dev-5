! formerly S:\acsGL\AcGLAcTB
! pr Accumulated Trial Balance
! r: setup library, on error, dims, and constants
autoLibrary
	on error goto Ertn

	dim a$(9)*3
	a$(1)='C/D'
	a$(2)='C/R'
	a$(3)='ADJ'
	a$(4)='A/P'
	a$(5)='PR'
	a$(6)='A/R'
	a$(7)='S/J'
	a$(8)='P/J'
	a$(9)=' '
! /r
	fnTop(program$)
	dim cogl$(3)*12
	open #20: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
	read #20,using 'form pos 152,3*C 12',rec=1: mat cogl$
	read #20,using 'form pos 296,N 2',rec=1: lmu
	read #20,using 'form pos 384,n 2',rec=1: nap
	close #20:
	fncreg_read('Last "Capital" Account',lastCapitalAccount$,cogl$(3))
	fncreg_Read('Print Ending Balance on First Line',petro_opt$,'False')
	fncreg_Read(env$('program_caption')&': DayStart',tmp$) : startday=val(tmp$)
	fncreg_Read(env$('program_caption')&': DayEnd'  ,tmp$) : endday=val(tmp$)
	m2GlmCbAmtPos=87
	if nap=13 then m1GlmBbAmtPos=171-6 else m1GlmBbAmtPos=171-12 ! 171 was 249
	! last=val(lastCapitalAccount$(4:9))
	open #h_glmstr:=1: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr',i,i,k
	open #h_actrans=fnH: 'Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr',i,i,k
	if fnProcess=1 then s1=1 : goto mainLoopInit
goto SCREEN1
SCREEN1: ! r:
	fnTos
	dim resp$(20)*128
	mylen=53: mypos=mylen+3 : rc=0 : right=1
	fnLbl(1,1,'General ledger number for the last "Capital" account:',mylen,right)
	fnQgl(1,mypos,0,2)
	resp$(respc_lastCapitalAccount:=rc+=1)=lastCapitalAccount$
	fnChk(2,mypos,'Print Ending Balance on First Line:',1,0)
	resp$(respc_prBalFirst:=rc+=1)=petro_opt$
	fnLbl(3,1,'Period Code to pr (blank for all):',mylen,right)
	fnTxt(3,mypos,2,0,1,'30',0,'You can pr any month or the entire year.')
	resp$(respc_periodCode:=rc+=1)='' ! STR$(LMU)
	fnFra(5,1,5,90,'Selection Type',' ',0) : frameno=1
	fnOpt(1,3,'Print All GL Accounts',0,frameno)
	resp$(respc_printAll:=rc+=1)='True'
	fnOpt(2,3,'Print Selected GL Accounts',0,frameno)
	resp$(respc_printSelected:=rc+=1)='False'
	fnOpt(3,3,'Print a Range of Accounts',0,frameno)
	resp$(respc_printRange:=rc+=1)='False'
	mylen=6 : mypos=mylen+2
	fnLbl(4,1+10,'First:',mylen,right,0,frameno)
	fnQgl(4,mypos+10,frameno,2)
	resp$(respc_rangeStart:=rc+=1)=''
	fnLbl(5,1+10,'Last:',mylen,right,0,frameno)
	fnQgl(5,mypos+10,frameno,2)
	resp$(respc_rangeEnd:=rc+=1)=''
	fnFra(12,1,4,90,'Filters',' ',0) : frameno=2
	mylen=14 : mypos=mylen+2
	fnLbl(1,1,'Starting Date:',mylen,right,0,frameno,0,'Enter a date to filter results or blank for all')
	fnTxt(1,mypos,10,0,1,'3',0,'Enter a date to filter results or blank for all',frameno)
	resp$(resp_dateStart:=rc+=1)=date$(startday,'ccyymmdd')
	fnLbl(2,1,'Ending Date:',mylen,right,0,frameno,0,'Enter a date to filter results or blank for all')
	fnTxt(2,mypos,10,0,1,'3',0,'Enter a date to filter results or blank for all',frameno)
	resp$(resp_dateEnd:=rc+=1)=date$(endday,'ccyymmdd')
	fnLbl(4,1,'Fund Number:',mylen,right,0,frameno,0,'Select a Cost Center to filter results or blank for all') ! costCenterFilter
	fnTxt(4,mypos,2,0,1,'30',0,'Select a Cost Center to filter results or blank for all',frameno)
	resp$(resp_costCenter:=rc+=1)=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	lastCapitalAccount$=cogl$(3)=fnagl$(resp$(respc_lastCapitalAccount))
	petro_opt$=resp$(respc_prBalFirst)
	periodToPrint=val(resp$(respc_periodCode)) ! period code to print
	if resp$(respc_printAll)='True' then s1=1 ! method of selecting
	if resp$(respc_printSelected)='True' then s1=2
	if resp$(respc_printRange)='True' then s1=3
	n1$=fnagl$(resp$(respc_rangeStart))
	n2$=fnagl$(resp$(respc_rangeEnd))
	startday=days(resp$(resp_dateStart),'ccyymmdd')
	endday=days(resp$(resp_dateEnd),'ccyymmdd')
	costCenterFilter=val(resp$(resp_costCenter))
	fncreg_write('Last "Capital" Account',lastCapitalAccount$)
	fncreg_write('Print Ending Balance on First Line',petro_opt$)
	fncreg_write(env$('program_caption')&': DayStart',str$(startday))
	fncreg_write(env$('program_caption')&': DayEnd'  ,str$(endday))
	fncreg_write('Print Ending Balance on First Line',petro_opt$)
	if periodToPrint>1 then m1GlmBbAmtPos=periodToPrint*6+81
	m2GlmCbAmtPos=periodToPrint*6+87
	if periodToPrint><1 then goto L640
	if nap=13 then m1GlmBbAmtPos=171-6 else m1GlmBbAmtPos=171-12 ! 171 was 249
L640: if s1=3 and n2$<n1$ then goto SCREEN1
! Read #h_GLmstr,Using 880,Key=N1$: N$,D$,BB,CB Nokey 670
	if f1=1 then goto AfterReadGlmstr
	f1=1
	! if fnUseDeptNo=0 or fnProcess=1 then goto READ_GLMSTR ! L840
	dim n$*12
	n$=cnvrt$('N 3',costCenterFilter)&'         '
	restore #h_glmstr,key>=n$: nokey SCREEN1
	on pageoflow goto PgOf
	on fkey 5 goto TOTALS
goto mainLoopInit ! /r (costCenterFilter)
mainLoopInit: ! r: main loop setup (costCenterFilter)
	fnOpenPrn
	gosub HDR
	goto READ_GLMSTR ! /r main loop setup
READ_GLMSTR: ! r: main loop
	if s1=2 then
		gosub SELECT_ACCOUNT
	else
		do
			dim d$*50
			dim bp(13)
			read #h_glmstr,using F_GLMSTR: n$,d$,bb,cb,mat bp eof TOTALS
			! m2GlmCbAmtPos=87=current balance
			F_GLMSTR: form pos 1,c 12,c 50,pos m1GlmBbAmtPos,pd 6.2,pos m2GlmCbAmtPos,pd 6.2,pos 171,13*pd 6.2
			bb=bp(nap)
!     pause !
		loop while s1=3 and n$<n1$
		if s1=3 and n$>n2$ then goto TOTALS
	end if
	if costCenterFilter><0 and val(n$(1:3))><costCenterFilter then goto TOTALS
AfterReadGlmstr: !
	dno=val(n$(1:3))
	ano=val(n$(4:9))
	sno=val(n$(10:12))
	if (periodToPrint=0 or periodToPrint=1) and (dno>val(lastCapitalAccount$(1:3)) or ano>val(lastCapitalAccount$(4:9))) then  ! added the dno logic on 2/4/2017
		bb=0
! else
!   bb=cb-activity  ! added the activity logic on 2/7/2017, before it was just bb
!   activity=0
!   pr str$(dno)&'-'&str$(ano)&'-'&str$(sno) : pause
	end if
	if petro_opt$='True' then
		pr #255,using L1380: dno,ano,sno,d$,bb,cb
		L1380: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr),pos 110,pic(zz,zzz,zzz.## cr)
	else
		pr #255,using L1390: dno,ano,sno,d$,bb
		L1390: form pos 1,pic(zzz),x 1,pic(------),x 1,pic(---),x 2,c 50,pos 80,pic(--,---,--z.## cr)
	end if
	restore #h_actrans,key>=n$&cnvrt$('N 2',periodToPrint)&'      ': nokey END_OF_TRANS
	t9=0
	do
		gosub READ_TR
		if t9=9 then goto END_OF_TRANS
		gosub PrintAtrans
	loop
!
END_OF_TRANS: !
	gosub PRINT_CB_OR_SUMTR
goto READ_GLMSTR ! /r
TOTALS: ! r: EOF ON MASTER FILE
	pr #255: ''
	pr #255,using L1100: 'Trial Balance Proof Totals',begbal,trtotal,curbal
	L1100: form pos 1,cr 78,pos 80,pic(zz,zzz,zzz.## cr),pic(z,zzz,zzz.## cr),pic(z,zzz,zzz.## cr)
	close #h_glmstr:
	close #h_actrans:
	fnClosePrn
	goto Xit ! /r
Xit: fnXit
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
HDR: ! r:
	pr #255,using fHeader1: env$('cnam'),date$('mm/dd/yy')
	pr #255,using fHeader1: 'Accumulated Trial Balance', time$
	pr #255,using fHeader1: fnpedat$,'Page '&str$(p1+=1)
	fHeader1: form pos 21,cc 80,cr 21
	pr #255: ''
	pr #255: '      Account';
	pr #255: tab(70);'Reference';tab(84);'Beginning';tab(99);'Current';
	pr #255: tab(115);'Ending'
	pr #255,using fHeader2: 'Number','Account Name/Transaction Description','Date  Source','Number','Balance','Activity','Balance'
	fHeader2: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7,pos 99,c 8,pos 115,c 7
	pr #255,using fHeader3: '__________','____________________________________','____','______','___________','_________','__________','_________'
	fHeader3: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 9,pos 98,c 10,pos 114,c 10
return ! /r
READ_TR: ! r:
	dim tr(7),tr$*12,td$*30
	dim t$*12
	read #h_actrans,using F_ACTRANS: t$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pcde eof ReadTrFinisT9
	F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,n 2
	if startday>0 and days(tr(4),'mmddyy')<startday then goto READ_TR
	if endday>0 and days(tr(4),'mmddyy')>endday then goto READ_TR
	if t$><n$ then goto ReadTrFinisT9
	if periodToPrint<>0 and periodToPrint><pcde then
		goto ReadTrFinisT9
	end if
	if tr(5)=0 then goto READ_TR
	if tr(6)><0 then goto ReadTrXit else tr(6)=9
	goto ReadTrXit
	ReadTrFinisT9: ! r:
		t9=9
	goto ReadTrXit ! /r
	ReadTrXit: !
return ! /r
PrintAtrans: ! r:
	dim x$*3
	x$=a$(tr(6))
	if val(cogl$(1)(4:9))=0 or val(cogl$(2)(4:9))=0 then goto PatPrTrans
	if t$>=cogl$(1) and t$<=cogl$(2) then
			if tr(5)>0 then goto PatPrTrans
			u0=u0+tr(5)
			trtotal=trtotal+tr(5)
			dim u$*12
			u$=t$
			goto PatFinis
	end if
	if tr$='999999999999' then tr$=' '
	PatPrTrans: !

	pr #255,using L1610: td$,tr(4),x$,lpad$(rtrm$(tr$),12),tr(5)
	L1610: form pos 21,c 30,pos 52,pic(zz/zz/zz),pos 62,c 3,pos 67,c 12,pos 95,pic(zz,zzz,zzz.## cr)
	trtotal=trtotal+tr(5)
	u$=t$

	PatFinis: !
return ! /r
PRINT_CB_OR_SUMTR: ! r:
	if u0 and u$=>cogl$(1) and u$<=cogl$(2) then
		pr #255,using L1690: 'Summary Transaction',u0
		L1690: form pos 21,c 30,pos 95,pic(zz,zzz,zz#.## cr)
		u0=0
	end if
	if petro_opt$='False' then
		pr #255,using 'form pos 110,pic(zz,zzz,zzz.## cr)': cb
	end if
	curbal=curbal+cb
	begbal=begbal+bb
return ! /r
SELECT_ACCOUNT: ! r:
	fnTos
	mylen=38: mypos=mylen+3 : right=1
	fnLbl(1,1,'General ledger # to print:',mylen,right,0,0)
	fnQgl(1,mypos,0,2)
	resp$(1)=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto TOTALS
	n$=fnagl$(resp$(1))
	read #h_glmstr,using F_GLMSTR,key=n$: n$,d$,bb,cb,mat bp nokey SELECT_ACCOUNT
	bb=bp(nap)
return ! /r
include: ertn
