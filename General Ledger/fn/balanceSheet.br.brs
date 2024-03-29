def library fnBalanceSheet(; defaultFormat$)
	autoLibrary
	on error goto Ertn
	fnBalanceSheet=fn_balanceSheet( defaultFormat$)
	Xit: !
	! pr 'the end' : pause
fnend
def fn_balanceSheet(; defaultFormat$)
	dim foot$*132
	
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode(actpd)
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Finis           ! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fnfscode
	fnpriorcd

	if fnProcess=1 or fnUseDeptNo=0 then goto GetStarted else goto Screen1

	Screen1: ! r: gets costCntr
		fnTos
		mylen=33
		mypos=mylen+3 : right=1
		fnLbl(1,1,'Cost Center or Department Number:',mylen,right)
		fnTxt(1,mypos,3,0,right,'30',0,'Enter the cost center or department number if you wish to pr only one department, else leave blank for all.',0 )
		resp$(1)=''
		fnLbl(2,1,'(Blank for all Departments)',mylen,right)
		fnCmdKey('&Next',1,1,0,'Prints the financial statement.')
		fnCmdKey('&Cancel',5,0,1,'Returns to menu without posting.')
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto Finis2
		costcntr=val(resp$(1)) conv Screen1
	goto GetStarted ! /r
	
	GetStarted: ! r:
		fnFsIndexBalSht
		if fnps=2 then mp1=66 else mp1=63 ! hFsD=fnOpenFsDesignInput('balance sheet',mp1)
		dim fsN(0),fs$(0)*128
		hFsD=fn_openFio('GL FSDesign',mat fs$,mat fsN,1) ! requires [FinancialStatementCode]
		open #hGl=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr',i,i,k
		fnOpenPrn
		dim reportHeading1$*50
		if env$('client')='Billings' then reportHeading1$='Income and Expense Statement'
		do
			read #hFsD,using form$(hFsD): mat fs$,mat fsN eof Finis
			! if pos(trim$(fs$(2)),'CURRENT ASSETS') then pause
			if ltrm$(fs$(fsd_number))<>'' and ltrm$(fs$(fsd_number))<>'0' then
				if costcntr=0 or costcntr=fsN(fsd_costCenterCode) then
					if fs$(fsd_entryType)<>'S' and fs$(fsd_entryType)<>'F' and fs$(fsd_entryType)<>'R' and heading=0 then
						fn_tePrnHeader(reportHeading1$,reportHeading2$)
					end if
					if fs$(fsd_entryType)='R' then      ! R = Report Heading (Places name of report in heading)
						pr 'r1='&fs$(fsd_description)
						fn_teSetHeaderOrSubHead(mat fs$,mat fsN,reportHeading1$,foot$,tabnote)
					else if fs$(fsd_entryType)='S' then ! S = Sub Heading (Places sub heading at top of F/S)
						dim reportHeading2$*50
						pr 'r2='&fs$(fsd_description)
						fn_teSetHeaderOrSubHead(mat fs$,mat fsN,reportHeading2$,foot$,tabnote)
					else if fs$(fsd_entryType)='F' then ! F = Footnote (Used to place footnotes at bottom of F/S)
						fn_teSetFootnote(mat fs$,mat fsN,foot$,tabnote)
					else if fs$(fsd_entryType)='H' then ! H = Header (Places headings within the F/S)
						fn_tePrnSectionHeading(mat fs$,mat fsN,foot$,tabnote,mat accum)
					else if fs$(fsd_entryType)='D' then ! D = Detail (Pulls amounts from G/L accounts)
						fn_tePrnDetailAndEquity(mat fs$,mat fsN,mp1,notrans,actpd,mat accum,foot$,tabnote,total,cb)
					else if fs$(fsd_entryType)='E' then ! E = Equity (Used to combine P&L with equity
						fn_tePrnDetailAndEquity(mat fs$,mat fsN,mp1,notrans,actpd,mat accum,foot$,tabnote,total,cb)
					else if fs$(fsd_entryType)='P' then ! P = Profit or Loss (Used to place P & L amount on B/S
						fn_tePrnProfitLossAndTotal(mat fs$,mat fsN,mat accum,foot$,tabnote)
					else if fs$(fsd_entryType)='T' then ! T = Total  (Used to pr totals or subtotals)
						fn_tePrnProfitLossAndTotal(mat fs$,mat fsN,mat accum,foot$,tabnote)
					else
						! B = Bank Accounts (Beginning and Ending on Fund Stmt)
						! C = Cash Flow Pause Indicator (Pauses & asks amounts)
						pr bell;'unexpectedTe="'&fs$(fsd_entryType)&'"'
						pause
						goto Finis
						!    pos ('RFHDTSPE',fs$(fsd_entryType),1) goto TeRS,TeF  ,TeH ,TeDE,TeTP,TeRS,TeTP,TeDE none READ_TOP
					end if
				end if
			end if
		loop
	! /r EoF (above) goes to Finis
	Finis: ! r:
		eofcode=1
		fn_footerPrint(foot$,tabnote,1)
		fnfscode(actpd)
		fnpriorcd(1)
		fnClosePrn
	goto Finis2 ! /r
	Finis2: ! r:
		close #hGl: ioerr ignore
		hGl=0
		close #hFsD: ioerr ignore
		hFsD=0 
	! /r
fnend 

def fn_teSetHeaderOrSubHead(mat fs$,mat fsN,&reportHeadingX$,foot$*132,tabnote)
	reportHeadingX$=rtrm$(fs$(fsd_description))
	fn_footer(foot$,tabnote,mat fsN)
fnend
def fn_teSetFootnote(mat fs$,mat fsN,&foot$,&tabnote)
	if ~foot1 then
		foot1=1
		tabnote=fsN(fsd_startPos)
		foot$=fs$(fsd_description)
	else
		foot$=rtrm$(foot$)&fs$(fsd_description)
	end if
fnend
def fn_tePrnSectionHeading(mat fs$,mat fsN,foot$*132,tabnote,mat accum; ___,tmpStartPos)
	tmpStartPos=fsN(fsd_startPos)
	pr #255,using L470: rtrm$(fs$(fsd_description))
	! if env$('acsDeveloper')='Laura' then print tmpStartPos,dollar : pause
	L470: form pos tmpStartPos,c 50,skip 1
	fn_footer(foot$,tabnote,mat fsN)
	fn_setAccum(mat fsN,mat accum)
fnend
def fn_tePrnDetailAndEquity(mat fs$,mat fsN,mp1,&notrans,actpd,mat accum,foot$*132,tabnote,&total,&cb; ___,dollar$*1,dollar,sp2,tmpStartPos)
	! De_
	! local to TeDE only, but retained values: br
	if notrans=1 then goto De_L660
	if br>=val(fs$(fsd_number)) and val(fs$(fsd_number))><0 then goto De_L610
	De_ReadGlMasterAmounts: ! read general ledger master file for amounts
	dim by(13)
	dim bp(13)
	read #hGl,using 'form pos mp1,PD 3,pos 87,27*PD 6.2': br,cb,mat by,mat bp eof De_EoGlMasterAmounts
	if br=0 then goto De_ReadGlMasterAmounts
	if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then goto De_L610
	if fnfscode<1 or fnfscode>12 then fnfscode(1)
	if fnpriorcd=1 then cb=by(fnfscode) else cb=bp(fnfscode)
	De_L610: !
	if br=val(fs$(fsd_number)) then
		total+=cb
		goto De_ReadGlMasterAmounts
	else if br<val(fs$(fsd_number)) then
		goto De_ReadGlMasterAmounts
	else if br>val(fs$(fsd_number)) then
		goto De_L660
	end if

	De_EoGlMasterAmounts: !
	notrans=1

	De_L660: !
	if fs$(fsd_entryType)='E' then total=-accum(fsN(fsd_accumulatorToPrint))

	if fsN(fsd_clearAccumulator1)<>9 then accum(1)+=total
	if fsN(fsd_clearAccumulator2)<>9 then accum(2)+=total
	if fsN(fsd_clearAccumulator3)<>9 then accum(3)+=total
	if fsN(fsd_clearAccumulator4)<>9 then accum(4)+=total
	if fsN(fsd_clearAccumulator5)<>9 then 
		if debug and total<>0 then
			pr #255: '                                **accum5('&str$(accum(5))&')+total('&str$(total)&')='&str$(accum(5)+total)
		end if
		accum(5)+=total
	end if
	if fsN(fsd_clearAccumulator6)<>9 then accum(6)+=total
	if fsN(fsd_clearAccumulator7)<>9 then accum(7)+=total
	if fsN(fsd_clearAccumulator8)<>9 then accum(8)+=total
	if fsN(fsd_clearAccumulator9)<>9 then accum(9)+=total

	if fsN(fsd_reverseSign)=1 then total=-total
	if fsN(fsd_dollarSign)=1 then dollar$='$' else dollar$=' '
	dollar=24+14*fsN(fsd_column) ! If CP=1 Then dOLLAR=50+14*fsN(fsd_column) Else dOLLAR=24+14*fsN(fsd_column)
	if total=0 and fsn(fsd_lineSkip)+fsN(fsd_underline)+fsN(fsd_dollarSign)+fsN(fsd_percentageBase)<=0 then
		goto De_Finis
	end if
	sp2=dollar-fsN(fsd_startPos)-1
	tmpStartPos=fsN(fsd_startPos)
	if fsN(fsd_underline)=1 then
		pr #255,using F_withoutUl: fs$(fsd_description)(1:sp2),dollar$,'{\ul ',total,'}' pageoflow PgOf  ! atlantis underline
		! F_withoutUl: form pos tmpStartPos,c sp2,pos dollar,c 1,c 5,pic(---,---,---.##),c 2,skip 0  ! ! atlantis underline
		F_withoutUl: form pos tmpStartPos,c sp2,pos dollar,c 1,c 5,pic(---,---,---.##),c 2,skip 1
	else
		pr #255,using F_withUl:    fs$(fsd_description)(1:sp2),dollar$,total pageoflow PgOf
		! F_withUl:    form pos tmpStartPos,c sp2,pos dollar,c 1,pic(---,---,---.##),skip 0
		F_withUl:    form pos tmpStartPos,c sp2,pos dollar,c 1,pic(---,---,---.##),skip 1
	end if
	total=0
	fn_setAccum(mat fsN,mat accum)
	if fsN(fsd_underline)<>1 then ! atlantis underline
		fn_underline(mat fsN)
	end if
	fn_footer(foot$,tabnote,mat fsN)
	De_Finis: !
fnend
def fn_tePrnProfitLossAndTotal(mat fs$,mat fsN,mat accum,foot$*132,tabnote; ___,dollar$,sp2,j,accum1,dollar,tmpStartPos)
	if fsN(fsd_accumulatorToPrint)=0 then fsN(fsd_accumulatorToPrint)=1
	if fsN(fsd_reverseSign)=1 then accum1=-accum(fsN(fsd_accumulatorToPrint)) else accum1=accum(fsN(fsd_accumulatorToPrint))
	if fsN(fsd_dollarSign)=1 then dollar$='$' else dollar$=' '
	dollar=24+14*fsN(fsd_column) ! if  CP=1 Then dOLLAR=50+14*fsN(fsd_column) Else dOLLAR=24+14*fsN(fsd_column)
	sp2=dollar-fsN(fsd_startPos)-1
	tmpStartPos=fsN(fsd_startPos) !  if env$('acsDeveloper')='Laura' then print tmpStartPos,dollar : pause
	if fsN(fsd_underline)=1 then
		pr #255,using F_withoutUl: fs$(fsd_description)(1:sp2),dollar$,'{\ul ',accum1,'}' pageoflow PgOf
	else
		pr #255,using F_withUl:    fs$(fsd_description)(1:sp2),dollar$,accum1 pageoflow PgOf
	end if
	! if debug and trim$(fs$(fsd_description))='PROFIT OR LOSS' then
	! 	pr trim$(fs$(fsd_description)),dollar$,accum1
	! 	pause
	! end if
	fn_setAccum(mat fsN,mat accum)
	if fsN(fsd_underline)<>1 then ! atlantis underline
		fn_underline(mat fsN)
	end if
	fn_footer(foot$,tabnote,mat fsN)
	if fs$(fsd_entryType)='P' then
		for j=1 to 9
			! if debug and j=5 and accum(fsN(fsd_accumulatorToPrint))<>0 then
			! 	pr #255: '                                **accum5('&str$(accum(5))&')-accum(fsN(fsd_accumulatorToPrint))('&str$(accum(fsN(fsd_accumulatorToPrint)))&')='&str$(accum(j)-accum(fsN(fsd_accumulatorToPrint)))
			! end if
			accum(j)=accum(j)-accum(fsN(fsd_accumulatorToPrint))
		next j
	end if
fnend
def fn_setAccum(mat fsN,mat accum; ___,j) ! SET_ACCUM:
	if fsN(fsd_clearAccumulator1)<>0 and fsN(fsd_clearAccumulator1)<>9 then accum(1)=0
	if fsN(fsd_clearAccumulator2)<>0 and fsN(fsd_clearAccumulator2)<>9 then accum(2)=0
	if fsN(fsd_clearAccumulator3)<>0 and fsN(fsd_clearAccumulator3)<>9 then accum(3)=0
	if fsN(fsd_clearAccumulator4)<>0 and fsN(fsd_clearAccumulator4)<>9 then accum(4)=0
	if fsN(fsd_clearAccumulator5)<>0 and fsN(fsd_clearAccumulator5)<>9 then 
		! if debug and fsN(fsd_clearAccumulator5)<>9 then
		! 	pr #255: '                                **accum5('&str$(accum(5))&')=0      (CLEARED)'
		! end if
		accum(5)=0
	end if
	if fsN(fsd_clearAccumulator6)<>0 and fsN(fsd_clearAccumulator6)<>9 then accum(6)=0
	if fsN(fsd_clearAccumulator7)<>0 and fsN(fsd_clearAccumulator7)<>9 then accum(7)=0
	if fsN(fsd_clearAccumulator8)<>0 and fsN(fsd_clearAccumulator8)<>9 then accum(8)=0
	if fsN(fsd_clearAccumulator9)<>0 and fsN(fsd_clearAccumulator9)<>9 then accum(9)=0
fnend
def fn_footer(foot$*132,tabnote,mat fsN; ___,tmpLineSkip)
	if fsn(fsd_lineSkip)=99 then
		fn_footerPrint(foot$,tabnote)
	else if fsn(fsd_lineSkip)<>0 then
		tmpLineSkip=fsn(fsd_lineSkip)
		pr #255,using F_skipLs: ' ' ! pr #255: 'got here '&str$(tmplineskip) ! pause
		F_skipLs: form pos 1,c 1,skip tmpLineSkip
	end if
fnend
def fn_footerPrint(foot$*132,tabnote; eofcode,___,sk,fl) ! FooterPrint
	if ~pglen then
		fnPgLen(pglen)
		if pglen=0 and debug then pr 'pglen=0' : pause
		! If PGLEN<>42 Then pGLEN=58
	end if
	sk=pglen-krec(255)
	foot$=rtrm$(foot$)
	fl=len(foot$)
	! If PGLEN=42 Then sK+=1
	pr #255,using F_footer: foot$
	F_footer: form skip sk,pos tabnote,c fl,skip 1
	if eofcode<>1 then
		pr #255: newpage
		fn_tePrnHeader(reportHeading1$,reportHeading2$)
	end if
fnend
PgOf: ! r: foot$,tabnote
	fn_footerPrint(foot$,tabnote)
continue ! /r
def fn_underline(mat fsN; ___,underlin$*14)
	if fsN(fsd_underline) then
		underlin=25+14*fsN(fsd_column) ! if CP=1 Then uNDERLIN=51+14*fsN(fsd_column) Else uNDERLIN=25+14*fsN(fsd_column)
		if fsN(fsd_underline)<>1 then
			underlin$='=============='
			pr #255,using F_underline: underlin$
			F_underline: form pos underlin,c 14,skip 1
		else
			underlin$='______________'
			pr #255,using F_underline: underlin$
		end if
	else
	end if
fnend
def fn_tePrnHeader(reportHeading1$*50,reportHeading2$*50)
	heading=1
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cnam')&'}'
	! if reportHeading1$<>env$('program_caption') then
		pr #255: '\qc  {\f181 \fs24 \b '&env$('program_caption')&'}'
	! end if
	pr #255: '\qc  {\f181 \fs24 \b '&reportHeading1$&'}'
	if trim$(reportHeading2$)<>'' then
		pr #255: '\qc  {\f181 \fs18 \b '&trim$(reportHeading2$)&'}'
	end if
	pr #255: '\qc  {\f181 \fs16 \b '&trim$(fnpedat$)&'}'
	pr #255: '\ql '
fnend

include: ertn
include: fn_open
