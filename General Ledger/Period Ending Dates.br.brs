autoLibrary
fnTop(program$)
on error goto Ertn

dim periodOption$(13)*2,resp$(10)*200
for x=1 to 13 : periodOption$(x)=str$(x) : next x

fnTos
respc=0
fnLbl(1,1,"Period Ending Date:",40,1)
fnTxt(1,42,20,0,0,"",0,"This will be the period ended date shown at the top of all financial statements and reports.  Use character format.") 
resp$(respc+=1)=fnpedat$
fnLbl(2,1,"Today's Date:",40,1)
fnTxt(2,42,20,0,0,"",0,"The current date should most ofter be todays date and will be printed on cover letters, etc.  Use character format. ") 
dim dat$*20
fndat(dat$,1) 
resp$(respc+=1)=dat$
fnLbl(3,1,"Current Accounting Period:",40,1)
fnComboA("ped1",3,42,mat periodOption$,"If you are reporting on a calander year, use the month code.  For fiscal years, this will be the number of months into the year.") 
resp$(respc+=1)=str$(fnactpd)
fnLbl(4,1,"Income Statement Current Column Heading:",40,1)
fnTxt(4,42,20,0,2,"",0,"This will be the column heading for the current period column. Use something simple as 'Current', or change it each month to be the name of the month") 
resp$(respc+=1)=fncch$
fnLbl(6,1,"Last Accounting Period closed was "&str$(fnLastAccountingPeriodClosed),60,2)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit

fnpedat$(resp$(1)) 
fndat(resp$(2),2) 
dat$=resp$(2)
fnactpd(val(resp$(3))) 
fnactpd$(str$(val(resp$(3)))) 
fncch$(resp$(4))
	
goto Xit
Xit: fnXit
include: ertn
