00010 ! formerly S:\acsGL\PeriodEndingDate
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndat,fnpedat$,fncch$,fnactpd,fnactpd$,fnactpd,fncch$,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs
00050   let fntop(program$,cap$="Period Ending Dates")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,pedat$*20,cch$*20,iom$(2),scm$(2),cap$*128
00090   dim periodOption$(13)*2,resp$(10)*200
00092   for x=1 to 13 : periodOption$(x)=str$(x) : next x
00100 ! ______________________________________________________________________
20000   let fntop(program$,cap$="Period Ending Dates")
20020   let pedat$=fnpedat$ 
20040   let fndat(dat$,1) 
20060   actpd=fnactpd 
20080   cch$=fncch$
20100 ! 
20120   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative  
20140   read #20,using 'Form Pos 296,n 2',rec=1: lmu
20160   close #20: 
20180 ! ______________________________________________________________________
20200 PERIOD_ENDING_DATES: ! 
20220   cancel=99
20240   let fntos("ped") 
20260   let respc=0
20280   let fnlbl(1,1,"Period Ending Date:",40,1)
20300   let fntxt(1,42,20,0,0,"",0,"This will be the period ended date shown at the top of all financial statements and reports.  Use character format.") 
20320   let resp$(respc+=1)=pedat$
20340   let fnlbl(2,1,"Today's Date:",40,1)
20360   let fntxt(2,42,20,0,0,"",0,"The current date should most ofter be todays date and will be printed on cover letters, etc.  Use character format. ") 
20380   let resp$(respc+=1)=dat$
20400   let fnlbl(3,1,"Current Accounting Period:",40,1)
20520   let fncomboa("ped1",3,42,mat periodOption$,"If you are reporting on a calander year, use the month code.  For fiscal years, this will be the number of months into the year.") 
20540   let resp$(respc+=1)=str$(actpd)
20560   let fnlbl(4,1,"Income Statement Current Column Heading:",40,1)
20580   let fntxt(4,42,20,0,2,"",0,"This will be the column heading for the current period column. Use something simple as 'Current', or change it each month to be the name of the month") 
20600   let resp$(respc+=1)=cch$
20620   let fnlbl(6,1,"Last Accounting Period closed was "&str$(lmu),60,2)
20640   let fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
20660   if ckey=5 then goto XIT
20680   let pedat$=resp$(1)
20700   let dat$=resp$(2)
20720   actpd=val(resp$(3))
20740   cch$=resp$(4)
20760 ! r: save answers 
20780   let fnpedat$(pedat$) 
20800   let fndat(dat$,2) 
20820   let fnactpd(actpd) 
20840   let fnactpd$(str$(actpd)) 
20860   let fncch$(cch$)
20880   goto XIT ! /r
20920 XIT: let fnxit
50000 ! <Updateable Region: ERTN>
50020 ERTN: let fnerror(program$,err,line,act$,"xit")
50040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50100 ERTN_EXEC_ACT: execute act$ : goto ERTN
50120 ! /region
50140 ! ______________________________________________________________________
