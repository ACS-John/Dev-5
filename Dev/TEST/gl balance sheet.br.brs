! formerly S:\acsGL\BalanceSheet
! Balance Sheet - Standard 8.5x11
! r: setup
	autoLibrary
	on error goto Ertn
 
	dim b$*3
	dim a$(8)*30
	dim oldtrans$*16
	dim g(8)
	dim d(2)
	dim foot$*132
	dim underlin$*14
! /r
	fnTop(program$)
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode(actpd)
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit           ! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fnfscode
	fnpriorcd
 
	if fnProcess=1 or fnUseDeptNo=0 then goto GetStarted else goto Screen1
! /r
Screen1: ! r:
	fnTos(sn$="GLInput")
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
goto GetStarted ! /r
GetStarted: ! r:
	fnFsIndexBalSht
	dim fsN(0),fs$(0)*128
	hFsD=fn_openFio('GL FSDesign',mat fs$,mat fsN,1)
	open #hGl=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",i,i,k
	fnOpenPrn
	dim reportHeading1$*50
	reportHeading1$=env$('program_caption')
	do
		READ_TOP: !
 
		read #hFsD,using form$(hFsD): mat fs$,mat fsN eof Finis
 
		if ltrm$(fs$(fsd_number))<>"" and ltrm$(fs$(fsd_number))<>"0" then
			if costcntr=0 or costcntr=fsN(fsd_costCenterCode) then
				if fs$(fsd_entryType)<>"S" and fs$(fsd_entryType)<>"F" and fs$(fsd_entryType)<>"R" and heading=0 then
					fn_header(reportHeading1$,reportHeading2$)
				end if
				if fs$(fsd_entryType)='R' then      ! R = Report Heading (Places name of report in heading)
					fn_teRS(mat fs$,mat fsN,reportHeading1$,foot$,tabnote)
				else if fs$(fsd_entryType)='S' then ! S = Sub Heading (Places sub heading at top of F/S)
					dim reportHeading2$*50
					fn_teRS(mat fs$,mat fsN,reportHeading2$,foot$,tabnote)
				else if fs$(fsd_entryType)='F' then ! F = Footnote (Used to place footnotes at bottom of F/S)
					fn_teFootnote(mat fs$,mat fsN,foot$,tabnote)
				else if fs$(fsd_entryType)='H' then ! H = Header (Places headings within the F/S)
					fn_teHeading(mat fs$,mat fsN,foot$,tabnote,mat accum)
				else if fs$(fsd_entryType)='D' then ! D = Detail (Pulls amounts from G/L accounts)
					fn_teDE(mat fs$,mat fsN,mp1,notrans,actpd,mat accum,foot$,tabnote,total,cb)
				else if fs$(fsd_entryType)='E' then ! E = Equity (Used to combine P&L with equity
					fn_teDE(mat fs$,mat fsN,mp1,notrans,actpd,mat accum,foot$,tabnote,total,cb)
				else if fs$(fsd_entryType)='P' then ! P = Profit or Loss (Used to place P & L amount on B/S
					fn_teTP(mat fs$,mat fsN,mat accum,foot$,tabnote)
				else if fs$(fsd_entryType)='T' then ! T = Total  (Used to pr totals or subtotals)
					fn_teTP(mat fs$,mat fsN,mat accum,foot$,tabnote)
				else
					! B = Bank Accounts (Beginning and Ending on Fund Stmt)
					! C = Cash Flow Pause Indicator (Pauses & asks amounts)
					pr bell;'unexpectedTe="'&fs$(fsd_entryType)&'"'
					pause
					goto Finis
					! on pos ("RFHDTSPE",fs$(fsd_entryType),1) goto UnexpectedTe,UnexpectedTe,UnexpectedTe,UnexpectedTe,UnexpectedTe,UnexpectedTe,UnexpectedTe,UnexpectedTe none UnexpectedTe
					!    pos ("RFHDTSPE",fs$(fsd_entryType),1) goto L970,L1010,L460,L520,L840,L970,L840,L520 none READ_TOP
					!    pos ("RFHDTSPE",fs$(fsd_entryType),1) goto TeRS,TeF  ,TeH ,TeDE,TeTP,TeRS,TeTP,TeDE none READ_TOP
				end if
			end if
		end if
	loop
! /r EoF (above) goes to Finis
 
def fn_teRS(mat fs$,mat fsN,&reportHeadingX$,foot$*132,tabnote)
	reportHeadingX$=fs$(fsd_description)
	fn_footer(foot$,tabnote,mat fsN)
fnend
def fn_teFootnote(mat fs$,mat fsN,&foot$,&tabnote)
	if ~foot1 then
		foot1=1
		tabnote=fsN(fsd_startPos)
		foot$=fs$(fsd_description)
	else
		foot$=rtrm$(foot$)&fs$(fsd_description)
	end if
fnend
 
def fn_teHeading(mat fs$,mat fsN,foot$*132,tabnote,mat accum; ___,tmpStartPos)
	pr #255,using L470: fs$(fsd_description)
	tmpStartPos=fsN(fsd_startPos)
	L470: form pos tmpStartPos,c 50,skip 1
	fn_footer(foot$,tabnote,mat fsN)
	fn_setAccum(mat fsN,mat accum)
fnend
def fn_teDE(mat fs$,mat fsN,mp1,&notrans,actpd,mat accum,foot$*132,tabnote,&total,&cb; ___,dollar$*1,dollar,sp2,tmpStartPos)
 
	! local to TeDE only, but retained values: br
	if notrans=1 then goto L660
	if br>=val(fs$(fsd_number)) and val(fs$(fsd_number))><0 then goto L610
	ReadGlMasterAmounts: ! read general ledger master file for amounts
	dim by(13)
	dim bp(13)
 
 
	read #hGl,using 'form pos MP1,PD 3,pos 87,27*PD 6.2': br,cb,mat by,mat bp eof EoGlMasterAmounts
	if br=0 then goto ReadGlMasterAmounts
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto L610
	if fnfscode<1 or fnfscode>12 then let fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	L610: !
	if br=val(fs$(fsd_number)) then
		total+=cb
		goto ReadGlMasterAmounts
	else if br<val(fs$(fsd_number)) then
		goto ReadGlMasterAmounts
	else if br>val(fs$(fsd_number)) then
		goto L660
	end if
 
	EoGlMasterAmounts: !
	notrans=1
 
	L660: !
	if fs$(fsd_entryType)="E" then total=-accum(fsN(fsd_accumulatorToPrint))
 
	if fsN(fsd_clearAccumulator1)<>9 then accum(1)+=total
	if fsN(fsd_clearAccumulator2)<>9 then accum(2)+=total
	if fsN(fsd_clearAccumulator3)<>9 then accum(3)+=total
	if fsN(fsd_clearAccumulator4)<>9 then accum(4)+=total
	if fsN(fsd_clearAccumulator5)<>9 then accum(5)+=total
	if fsN(fsd_clearAccumulator6)<>9 then accum(6)+=total
	if fsN(fsd_clearAccumulator7)<>9 then accum(7)+=total
	if fsN(fsd_clearAccumulator8)<>9 then accum(8)+=total
	if fsN(fsd_clearAccumulator9)<>9 then accum(9)+=total
 
	if fsN(fsd_reverseSign)=1 then total=-total
	if fsN(fsd_dollarSign)=1 then dollar$="$" else dollar$=" "
	dollar=24+14*fsN(fsd_column) ! If CP=1 Then dOLLAR=50+14*fsN(fsd_column) Else dOLLAR=24+14*fsN(fsd_column)
	if total=0 and fsn(fsd_lineSkip)+fsN(fsd_underline)+fsN(fsd_dollarSign)+fsN(fsd_percentageBase)<=0 then
		goto READ_TOP
	end if
	sp2=dollar-fsN(fsd_startPos)-1
	if fsN(fsd_underline)=1 then
		tmpStartPos=fsN(fsd_startPos)
		pr #255,using F_withoutUl: fs$(fsd_description)(1:sp2),dollar$,"{\ul ",total,"}" pageoflow PgOf  ! atlantis underline
		F_withoutUl: form pos tmpStartPos,c sp2,pos dollar,c 1,c 5,pic(---,---,---.##),c 2,skip 0  ! ! atlantis underline
	else
		tmpStartPos=fsN(fsd_startPos)
		pr #255,using F_withUl:    fs$(fsd_description)(1:sp2),dollar$,total pageoflow PgOf
		F_withUl:    form pos tmpStartPos,c sp2,pos dollar,c 1,pic(---,---,---.##),skip 0
	end if
	total=0
	fn_setAccum(mat fsN,mat accum)
	if fsN(fsd_underline)<>1 then ! atlantis underline
		fn_underline(mat fsN)
	end if
	fn_footer(foot$,tabnote,mat fsN)
fnend
def fn_teTP(mat fs$,mat fsN,mat accum,foot$*132,tabnote; ___,dollar$,sp2,j,accum1,dollar)
	if fsN(fsd_accumulatorToPrint)=0 then fsN(fsd_accumulatorToPrint)=1
	if fsN(fsd_reverseSign)=1 then accum1=-accum(fsN(fsd_accumulatorToPrint)) else accum1=accum(fsN(fsd_accumulatorToPrint))
	if fsN(fsd_dollarSign)=1 then dollar$="$" else dollar$=" "
	dollar=24+14*fsN(fsd_column) ! if  CP=1 Then dOLLAR=50+14*fsN(fsd_column) Else dOLLAR=24+14*fsN(fsd_column)
	sp2=dollar-fsN(fsd_startPos)-1
	if fsN(fsd_underline)=1 then
		pr #255,using F_withoutUl: fs$(fsd_description)(1:sp2),dollar$,"{\ul ",accum1,"}" pageoflow PgOf
	else
		pr #255,using F_withUl:    fs$(fsd_description)(1:sp2),dollar$,accum1 pageoflow PgOf
	end if
	if env$('acsDeveloper')<>'' and trim$(fs$(fsd_description))='PROFIT OR LOSS' then
		pr trim$(fs$(fsd_description)),dollar$,accum1
		pause
	end if
	fn_setAccum(mat fsN,mat accum)
	if fsN(fsd_underline)<>1 then ! atlantis underline
		fn_underline(mat fsN)
	end if
	fn_footer(foot$,tabnote,mat fsN)
	if fs$(fsd_entryType)="P" then
		for j=1 to 9
			accum(j)=accum(j)-accum(fsN(fsd_accumulatorToPrint))
		next j
	end if
fnend
def fn_setAccum(mat fsN,mat accum; ___,j) ! SET_ACCUM:
	for j=1 to 9
		! if ac(j)=0 or ac(j)=9 then
		! 	goto L1130
		! else
		! 	accum(j)=0
		! end if
		! L1130: !
		! if ac(j)<>0 and ac(j)<>9 then
		! 	accum(j)=0
		! end if
		if fsN(fsd_clearAccumulator1)<>0 and fsN(fsd_clearAccumulator1)<>9 then accum(1)=0
		if fsN(fsd_clearAccumulator2)<>0 and fsN(fsd_clearAccumulator2)<>9 then accum(2)=0
		if fsN(fsd_clearAccumulator3)<>0 and fsN(fsd_clearAccumulator3)<>9 then accum(3)=0
		if fsN(fsd_clearAccumulator4)<>0 and fsN(fsd_clearAccumulator4)<>9 then accum(4)=0
		if fsN(fsd_clearAccumulator5)<>0 and fsN(fsd_clearAccumulator5)<>9 then accum(5)=0
		if fsN(fsd_clearAccumulator6)<>0 and fsN(fsd_clearAccumulator6)<>9 then accum(6)=0
		if fsN(fsd_clearAccumulator7)<>0 and fsN(fsd_clearAccumulator7)<>9 then accum(7)=0
		if fsN(fsd_clearAccumulator8)<>0 and fsN(fsd_clearAccumulator8)<>9 then accum(8)=0
		if fsN(fsd_clearAccumulator9)<>0 and fsN(fsd_clearAccumulator9)<>9 then accum(9)=0
	next j
fnend
def fn_footer(foot$*132,tabnote,mat fsN; ___,tmpLineSkip)
	if fsn(fsd_lineSkip)=99 then
		fn_footerPrint(foot$,tabnote)
	else if fsn(fsd_lineSkip)<>0 then
		pr #255,using F_skipLs: " "
		tmpLineSkip=fsn(fsd_lineSkip)
		F_skipLs: form pos 1,c 1,skip tmpLineSkip
	end if
fnend
def fn_footerPrint(foot$*132,tabnote; eofcode,___,sk,fl) ! FooterPrint
	if ~pglen then
		fnPgLen(pglen)
		if pglen=0 and env$('acsDeveloper')<>'' then pr 'pglen=0' : pause
		! If PGLEN<>42 Then pGLEN=58
	end if
	sk=pglen-krec(255)
	fl=len(rtrm$(foot$))
	! If PGLEN=42 Then sK+=1
	pr #255,using F_footer: rtrm$(foot$)
	F_footer: form skip sk,pos tabnote,c fl,skip 1
	if eofcode<>1 then
		pr #255: newpage
		fn_header(reportHeading1$,reportHeading2$)
	end if
fnend
PgOf: ! r: foot$,tabnote
	fn_footerPrint(foot$,tabnote)
continue ! /r
def fn_underline(mat fsN)
	if fsN(fsd_underline) then
		underlin=25+14*fsN(fsd_column) ! if CP=1 Then uNDERLIN=51+14*fsN(fsd_column) Else uNDERLIN=25+14*fsN(fsd_column)
		if fsN(fsd_underline)<>1 then
			underlin$="=============="
			pr #255,using F_underline: underlin$
			F_underline: form pos underlin,c 14,skip 1
		else
			underlin$="______________"
			pr #255,using F_underline: underlin$
		end if
	else
		pr #255: ''
	end if
	! pr #255,using 'form skip 1,c 1,skip 0': " "
fnend
def fn_header(reportHeading1$*50,reportHeading2$*50)
	heading=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(reportHeading1$)&"}"
	if trim$(reportHeading2$)<>"" then
		pr #255: "\qc  {\f181 \fs18 \b "&trim$(reportHeading2$)&"}"
	end if
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(fnpedat$)&"}"
	pr #255: "\ql "
fnend
Finis: ! r:
	eofcode=1
	fn_footerPrint(foot$,tabnote,1)
	fnfscode(actpd)
	fnpriorcd(1)
	fnClosePrn
	close #hGl: ioerr ignore
	close #hFsD: ioerr ignore
goto Xit ! /r
Xit: fnXit
include: ertn
include: fn_open
