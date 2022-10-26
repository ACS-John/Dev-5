! Replace S:\acsGL\fnGlAskFormatPriorCdPeriod
! ask questions for General Ledger Financial Statements
def library fnGlAskFormatPriorCdPeriod(; defaultFormatOption$,___,returnN,x,lc,rc,frame,mylen,mypos)

	if glfsSetup<>val(env$('cno')) then ! r:
		glfsSetup=val(env$('cno'))
		autoLibrary
		on error goto Ertn

		open #company=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,i,r
		! nap = Number of Accounting Periods
		read #company,using 'form pos 384,N 2',rec=1: nap
		close #company:

		dim formatOption$(2)
		formatOption$(1)='Primary'
		formatOption$(2)='Secondary'

		dim priorOrCurrentOption$(2)
		priorOrCurrentOption$(1)='Current'
		priorOrCurrentOption$(2)='Prior'

		dim periodOption$(13)
		mat periodOption$(nap)
		for j=1 to nap
			periodOption$(j)=str$(j)
		next j
	end if ! /r
	actpd$=fnactpd$
	actpd=fnactpd
	format=srch(mat formatOption$,defaultFormatOption$)
	if format<=0 or format>(udim(mat formatOption$)) then format=1
	if fnprocess=1 then
		fnps(1)
		fnpriorcd(1)
	else

		! ! r: origional screen
		! fnTos
		! lc=rc=0 : mylen=7 : mypos=mylen+3
		! lc+=1
		! fnLbl(lc+=1,1,'Format:',mylen,1)
		! fnComboA('ps',lc,mypos,mat formatOption$)
		! resp$(resp_format:=rc+=1)=formatOption$(format)
		! lc+=1
		! lc+=1
		! lc+=1
		! fnLbl(lc+=1,1,'Year:',mylen,1)
		! fnComboA('PriorCD',lc,mypos,mat priorOrCurrentOption$)
		! resp$(resp_priorOrCurrent:=rc+=1)=priorOrCurrentOption$(1)
		! lc+=1
		! lc+=1
		! lc+=1
		! fnLbl(lc+=1,1,'Period:',mylen,1)
		! fnComboA('actPd',lc,mypos,mat periodOption$)
		! resp$(resp_actpd:=rc+=1)=str$(actpd) ! periodOption$(1)
		! lc+=1
		! lc+=1
		! lc+=1
		! fnLbl(lc+=1,1,' ',mylen,1)
		! fnCmdSet(3)
		! ckey=fnAcs(mat resp$)
		! if ckey=5 then
		! 	returnN=5
		! else
		! 	format=srch(mat formatOption$,resp$(resp_format))
		! 	priorOrCurrent=srch(mat priorOrCurrentOption$,resp$(resp_priorOrCurrent))
		! 	actpd=srch(mat periodOption$,resp$(resp_actpd))
		! 	actpd$=resp$(resp_actpd)
		! 	fnps(format)
		! 	fnpriorcd(priorOrCurrent)
		! 	fnfscode(actpd)
		! 	! fnactpd$(actpd$)
		! 	! fnactpd	(actpd )
		! end if
		! !  /r

		! r: new screen
		dim resp$(128)*128
		fnTos
		fn_addFrameWithOptions(1, 1,'Format',mat formatOption$         	,frame,mat resp_format         , format)
		fn_addFrameWithOptions(5, 1,'Year'  ,mat priorOrCurrentOption$	,frame,mat resp_priorOrCurrent, 1      ,2)
		fn_addFrameWithOptions(1,18,'Period',mat periodOption$         	,frame,mat resp_actpd          , actpd)
		fnCmdSet(3)
		ckey=fnAcs(mat resp$)

		if ckey=5 then
			returnN=5
		else
			format         	=fn_respFrameWithOption(mat resp$,mat resp_format)
			priorOrCurrent	=fn_respFrameWithOption(mat resp$,mat resp_priorOrCurrent)
			actpd          	=fn_respFrameWithOption(mat resp$,mat resp_actpd)
			actpd$=str$(actpd)

			! format=srch(mat formatOption$,resp$(resp_format))
			! priorOrCurrent=srch(mat priorOrCurrentOption$,resp$(resp_priorOrCurrent))
			! actpd=srch(mat periodOption$,resp$(resp_actpd))
			! actpd$=resp$(resp_actpd)
			fnps(format)
			fnpriorcd(priorOrCurrent)
			fnfscode(actpd)
		!  /r
		end if
	end if
	Xit: !
	fnGlAskFormatPriorCdPeriod=returnN
fnend
def fn_addFrameWithOptions(li,posi,title$,mat option$,&frame,mat respEnum; optPreSelected,extraWidth,___,x,lc,lenMax)
	for x=1 to udim(mat option$)
		lenMax=max(lenMax,len(option$(x)))
	nex x
	fnFra(li,posi,udim(mat option$),6+lenMax+extraWidth,title$,' ',0)
	frame+=1
	mat respEnum(udim(mat option$)) : mat respEnum=(0)
	for x=1 to udim(mat option$)
		fnOpt(lc+=1,3,option$(x),0,frame) : resp$(respEnum(x)=rc+=1)='False' : if x=optPreSelected then resp$(respEnum(x))='True'
	nex x
fnend
def fn_respFrameWithOption(mat resp$,mat enum)
	fn_respFrameWithOption=srch(mat resp$(enum(1):enum(udim(mat enum))),'True')
fnend
include: ertn
