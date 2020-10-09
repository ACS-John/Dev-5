! Replace S:\acsGL\fnGlAskFormatPriorCdPeriod
! ask questions for General Ledger Financial Statements
def library fnGlAskFormatPriorCdPeriod(; defaultFormatOption$,___,returnN)

	if glfsSetup<>val(env$('cno')) then ! r:
		glfsSetup=val(env$('cno'))
		autoLibrary
		on error goto Ertn
		
		open #company=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative 
		read #company,using 'Form Pos 296,n 2,Pos 384,N 2',rec=1: lmu,nap
		! lmu = Last Accounting Period Closed
		! nap = Number of Accounting Periods
		close #company: 
		
		dim formatOption$(2)
		formatOption$(1)="Primary" 
		formatOption$(2)="Secondary" 
		
		dim priorOrCurrentOption$(2)
		priorOrCurrentOption$(1)="Current" 
		priorOrCurrentOption$(2)="Prior" 
		
		dim periodOption$(13)
		mat periodOption$(nap)
		for j=1 to nap 
			periodOption$(j)=str$(j)
		next j 
	end if ! /r
	actpd$=fnactpd$ 
	actpd=fnactpd
	format=srch(mat formatOption$,defaultFormatOption$)
	if format<=0 or format >(udim(mat formatOption$)) then format=1
	if fnprocess=1 then 
		fnps(1)
		fnpriorcd(1)
	else
		fnTos
		lc=rc=0 : mylen=23 : mypos=mylen+3
		
		fnLbl(lc+=1,1,"Statement Format:",mylen,1)
		fncomboa("ps",lc,mypos,mat formatOption$) 
		resp$(resp_format:=rc+=1)=formatOption$(format)
		
		fnLbl(lc+=1,1,"Year:",mylen,1)
		fncomboa("PriorCD",lc,mypos,mat priorOrCurrentOption$) 
		resp$(resp_priorOrCurrent:=rc+=1)=priorOrCurrentOption$(1)
		
		fnLbl(lc+=1,1,"Period to Print:",mylen,1)
		fncomboa("actPd",lc,mypos,mat periodOption$) 
		resp$(resp_actpd:=rc+=1)=str$(actpd) ! periodOption$(1)
		
		fnCmdSet(3)
		fnAcs(mat resp$,ckey)
		if ckey=5 then 
			returnN=5 
		else
			format=srch(mat formatOption$,resp$(resp_format))
			priorOrCurrent=srch(mat priorOrCurrentOption$,resp$(resp_priorOrCurrent))
			actpd=srch(mat periodOption$,resp$(resp_actpd))
			actpd$=resp$(resp_actpd)
			fnps(format)
			fnpriorcd(priorOrCurrent)
			fnfscode(actpd)
			! fnactpd$(actpd$) 
			! fnactpd	(actpd )
		end if
	end if
	Xit: ! 
	fnGlAskFormatPriorCdPeriod=returnN
fnend 
include: ertn
