! Comparative Balance Sheet - formerly S:\acsGL\AcGLBalC
! r: setup library, on error, dims, fnTop
	autoLibrary
	on error goto Ertn
	fnTop(program$)

	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Pprior,period to print)
	actpd$=fnactpd$
	actpd=fnactpd

	! /r
! r: ask cost center
	costCntr=0
	if fnProcess=1 or ~fnUseDeptNo then goto L320
	fnTos
	mylen=26 : mypos=mylen+3
	fnLbl(1,1,'Cost Center or Department:',mylen,1)
	fnTxt(1,mypos,3,0,1,'30',0,'Enter the cost center or department number if you wish to pr only one department, else leave blank for all.',0 )
	resp$(1)=''
	fnLbl(2,1,'(Blank for all Departments)',mylen,1)
	fnCmdKey('&Next',1,1,0,'Prints the financial statement.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costCntr=val(resp$(1))
	L320: !
! /r
! r: open glmstr
		dim fl1$*256
		if fnPs=2 then mp1=66 else mp1=63
		fnFsIndexBalSht
		open #hFsDesign=fnH: fl1$,i,i,k ! formerly #1
		fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[Temp]\fsindex.h[cno]',str$(mp1)&' 3')
		open #hglm2=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr',i,i,k ! formerly #3
! /r
fnOpenPrn
dim report$*50
report$=env$('program_caption')
MainLoopTop: ! r: main loop (on financial statement design)
	dim r$*5
	dim d$*50
	dim te$*1
	dim ac(9)
	read #hFsDesign,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof Finis
	rN=val(r$)
	L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if ltrm$(r$)='' or ltrm$(r$)='0' then goto MainLoopTop
	if costCntr and costCntr><fc then goto MainLoopTop
	if heading=0 and te$<>'R' and te$<>'S' and te$<>'F' then gosub PrHeading

on pos ('RFHDTSPE',te$,1) goto TypeRandS,TypeF,TypeH,TypeDandE,TypeTandP,TypeRandS,TypeTandP,TypeDandE none MainLoopTop
! /r
TypeH: ! r:
	pr #255,using L520: d$
	L520: form pos sp,c 50,skip 1
	gosub PrSkipLines
	gosub ProcessClearAccum
goto MainLoopTop ! /r
TypeDandE: ! r:
		
	if ~notrans then ! r: read general ledger master file for amounts (; ___,br,cb	
		if br>rN and val(r$)><0 then goto L650
		restore #hglm2:
		do
			TdeReadGlm: ! 
			dim bp(13)
			dim by(13)
			read #hglm2,using 'form pos MP1,PD 3,pos 87,27*PD 6.2': br,cb,mat by,mat bp eof EoGlm2
			if br=0 then goto TdeReadGlm
			if fnfscode=0 or (fnfscode=actpd and fnpriorcd=1) then 
				goto L650
			else
				if fnfscode<1 or fnfscode>13 then fnfscode(1)
				if fnpriorcd=1 then cb=by(fnfscode)
			end if
			
			L650: !
			if br=rN then
				total+=cb
				total2+=bp(fnfscode)
			end if
		loop while br<=rN
		if br>rN then goto L710
		EoGlm2: !
		notrans=1
	end if
	L710: ! /r
	
	dim accum(9,2)
	if te$='E' then total=-accum(ap,1) : total2=-accum(ap,2)
	for j=1 to 9
		if ac(j)<>9 then
			accum(j,1)=accum(j,1)+total : accum(j,2)=accum(j,2)+total2
		end if
	next j
	if rs=1 then total=-total : total2=-total2
	if ds=1 then dollar$='$' else dollar$=' '
	dollar=24+14*bc
	if ~total and ~total2 and ls+ul+ds+ic<=0 then 
		goto MainLoopTop
	end if
	
	sp2=dollar-sp-1
	if ul=1 then 
		pr #255,using L816: d$(1:sp2),dollar$,'{\ul ',total,'}',dollar$,'{\ul ',total2,'}' pageoflow PgOf
		L816: form pos sp,c sp2,pos dollar,c 1,c 5,pic(--,---,---.##),c 1,x 28,c 1,c 5,pic(--,---,---.##),c 1,skip 1
	else
		pr #255,using L820: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow PgOf
		L820: form pos sp,c sp2,pos dollar,c 1,pic(--,---,---.##),x 28,c 1,pic(--,---,---.##),skip 1
	end if
	total=total2=0
	gosub ProcessClearAccum
	if ul=1 then goto L870
	gosub PrUnderline
	L870: !
	gosub PrSkipLines
goto MainLoopTop ! /r
TypeTandP: ! r:
	if ap=0 then ap=1
	if rs=1 then 
		accum1=-accum(ap,1)
		accum2=-accum(ap,2)
	else
		accum1=accum(ap,1)
		accum2=accum(ap,2)
	end if
	if ds=1 then dollar$='$' else dollar$=' '
	dollar=24+14*bc
	sp2=dollar-sp-1
	if ul=1 then 
		pr #255,using L816: d$(1:sp2),dollar$,'{\ul ',accum1,'}',dollar$,'{\ul ',accum2,'}' pageoflow PgOf 
	else
		pr #255,using L820: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PgOf
	end if
	gosub ProcessClearAccum
	if ul<>1 then 
		gosub PrUnderline
	end if
	gosub PrSkipLines
	if te$><'P' then goto L1010
	for j=1 to 9
		accum(j,1)=accum(j,1)-accum(ap,1)
		accum(j,2)=accum(j,2)-accum(ap,2)
	next j
	L1010: !
goto MainLoopTop ! /r
TypeRandS: ! r:
	if te$='R' then
		report$=d$
	else if te$='S' then
		dim secondr$*50
		secondr$=d$
	end if
	gosub PrSkipLines
goto MainLoopTop ! /r
TypeF: ! r:
	if foot1=1 then
		dim foot$*132
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp : foot1=1 : foot$=d$
	end if
goto MainLoopTop ! /r

ProcessClearAccum: ! r:
	for j=1 to 9
		if ac(j) and ac(j)<>9 then
			accum(j,1)=accum(j,2)=0
		end if
	next j
return ! /r
PrSkipLines: ! r: maybe skip some lines if ls>0 - if ls=99 then do the newpage thing
	if ls=99 then
		gosub PrNewPageThing
	else if ls then
		pr #255,using fSkipLs: ' '
		fSkipLs: form pos 1,c 1,skip ls
	end if
return ! /r
PrNewPageThing: ! ! r: newpage thing.  pr footer on page and if eofcode<>1 then pr newpage and heading
	fnPgLen(pglen)
	sk=pglen-krec(255)
	fl=len(rtrm$(foot$))
	if trim$(foot$)<>'' then 
		pr #255,using L1280: rtrm$(foot$)
		L1280: form skip sk,pos tabnote,c fl,skip 1
	end if 
	if eofcode<>1 then
		pr #255: newpage
		gosub PrHeading
	end if
	L1320: !
return ! /r
PgOf: ! r:
	gosub PrNewPageThing
continue ! /r
PrUnderline: ! r:
	if ul then 
		dim underlin$*56
		underlin=24+14*bc
		if ul=1 then 
			underlin$='______________                            ______________'
			pr #255,using Funderlin: underlin$
			Funderlin: form pos underlin,c 56,skip 1
		else
			underlin$='==============                            =============='
			pr #255,using L1420: underlin$
			L1420: form pos underlin,c 56,skip 1
		end if
	end if
return ! /r
PrHeading: ! r: heading
	heading=1
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&trim$(report$)&'}'
	if trim$(secondr$)<>'' then pr #255: '\qc  {\f181 \fs18 \b '&trim$(secondr$)&'}'
	pr #255: '\qc  {\f181 \fs16 \b '&trim$(fnpedat$)&'}'
	pr #255: '' ! '\ql '  moved down to the beginning of another line
	pr #255: ''
	pr #255: ! '\ql {\f181                                                                                                                             Current Year                                                                    Prior Year }'
	pr #255: '                                        Current Year                             Prior Year'
	pr #255,using L1630: '__________________________________________',' _________________________________________'
	L1630: form pos 38,cc 42,x 1,cc 42,skip 1
return ! /r
Finis: ! r:
	eofcode=1
	gosub PrNewPageThing
	fnfscode(actpd)
	fnpriorcd(1)
	fnClosePrn
	fnStatusClose
	close #hFsDesign: ioerr ignore
	close #hGlm2: ioerr ignore
goto Xit ! /r
Xit: fnXit
include: ertn
