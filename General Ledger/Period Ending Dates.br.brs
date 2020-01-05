00010 ! formerly S:\acsGL\PeriodEndingDate
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndat,fnpedat$,fncch$,fnactpd,fnactpd$,fnactpd,fncch$,fnTos,fnLbl,fnTxt,fncomboa,fnCmdSet,fnAcs
00050   fntop(program$,cap$="Period Ending Dates")
00060   on error goto Ertn
00070 ! ______________________________________________________________________
00080   dim cnam$*40,dat$*20,pedat$*20,cch$*20,iom$(2),scm$(2),cap$*128
00090   dim periodOption$(13)*2,resp$(10)*200
00092   for x=1 to 13 : periodOption$(x)=str$(x) : next x
00100 ! ______________________________________________________________________
20000   fntop(program$,cap$="Period Ending Dates")
20020   pedat$=fnpedat$ 
20040   fndat(dat$,1) 
20060   actpd=fnactpd 
20080   cch$=fncch$
20100 ! 
20120   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative  
20140   read #20,using 'Form Pos 296,n 2',rec=1: lmu
20160   close #20: 
20180 ! ______________________________________________________________________
20200 PERIOD_ENDING_DATES: ! 
20220   cancel=99
20240   fnTos("ped") 
20260   respc=0
20280   fnLbl(1,1,"Period Ending Date:",40,1)
20300   fnTxt(1,42,20,0,0,"",0,"This will be the period ended date shown at the top of all financial statements and reports.  Use character format.") 
20320   resp$(respc+=1)=pedat$
20340   fnLbl(2,1,"Today's Date:",40,1)
20360   fnTxt(2,42,20,0,0,"",0,"The current date should most ofter be todays date and will be printed on cover letters, etc.  Use character format. ") 
20380   resp$(respc+=1)=dat$
20400   fnLbl(3,1,"Current Accounting Period:",40,1)
20520   fncomboa("ped1",3,42,mat periodOption$,"If you are reporting on a calander year, use the month code.  For fiscal years, this will be the number of months into the year.") 
20540   resp$(respc+=1)=str$(actpd)
20560   fnLbl(4,1,"Income Statement Current Column Heading:",40,1)
20580   fnTxt(4,42,20,0,2,"",0,"This will be the column heading for the current period column. Use something simple as 'Current', or change it each month to be the name of the month") 
20600   resp$(respc+=1)=cch$
20620   fnLbl(6,1,"Last Accounting Period closed was "&str$(lmu),60,2)
20640   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
20660   if ckey=5 then goto XIT
20680   pedat$=resp$(1)
20700   dat$=resp$(2)
20720   actpd=val(resp$(3))
20740   cch$=resp$(4)
20760 ! r: save answers 
20780   fnpedat$(pedat$) 
20800   fndat(dat$,2) 
20820   fnactpd(actpd) 
20840   fnactpd$(str$(actpd)) 
20860   fncch$(cch$)
20880   goto XIT ! /r
20920 XIT: fnxit
50000 ! <Updateable Region: ERTN>
50020 ERTN: fnerror(program$,err,line,act$,"xit")
50040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50100 ERTN_EXEC_ACT: execute act$ : goto ERTN
50120 ! /region
50140 ! ______________________________________________________________________
