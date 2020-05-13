! formerly S:\acsGL\PeriodEndingDate

	autoLibrary
	fntop(program$)
	on error goto Ertn

	dim periodOption$(13)*2,resp$(10)*200
	for x=1 to 13 : periodOption$(x)=str$(x) : next x

	dim pedat$*20
	pedat$=fnpedat$ 
	dim dat$*20
	fndat(dat$,1) 
	actpd=fnactpd 
	dim cch$*20
	cch$=fncch$

	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative  
	read #20,using 'Form Pos 296,n 2',rec=1: lmu
	close #20: 

PERIOD_ENDING_DATES: ! 
	fnTos
	respc=0
	fnLbl(1,1,"Period Ending Date:",40,1)
	fnTxt(1,42,20,0,0,"",0,"This will be the period ended date shown at the top of all financial statements and reports.  Use character format.") 
	resp$(respc+=1)=pedat$
	fnLbl(2,1,"Today's Date:",40,1)
	fnTxt(2,42,20,0,0,"",0,"The current date should most ofter be todays date and will be printed on cover letters, etc.  Use character format. ") 
	resp$(respc+=1)=dat$
	fnLbl(3,1,"Current Accounting Period:",40,1)
	fncomboa("ped1",3,42,mat periodOption$,"If you are reporting on a calander year, use the month code.  For fiscal years, this will be the number of months into the year.") 
	resp$(respc+=1)=str$(actpd)
	fnLbl(4,1,"Income Statement Current Column Heading:",40,1)
	fnTxt(4,42,20,0,2,"",0,"This will be the column heading for the current period column. Use something simple as 'Current', or change it each month to be the name of the month") 
	resp$(respc+=1)=cch$
	fnLbl(6,1,"Last Accounting Period closed was "&str$(lmu),60,2)
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto XIT
	pedat$=resp$(1)
	dat$=resp$(2)
	actpd=val(resp$(3))
	cch$=resp$(4)
	
	! r: save answers 
		fnpedat$(pedat$) 
		fndat(dat$,2) 
		fnactpd(actpd) 
		fnactpd$(str$(actpd)) 
		fncch$(cch$)
	! /r
	
goto XIT ! /r
XIT: fnxit
include: Ertn
