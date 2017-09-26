12000 ! r: setup stuff
12020   on error goto ERTN
12040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn, fncloseprn,fnerror,fntos,fnfra,fnopt,fnlbl,fntxt,fncmbact,fncmdkey,fnacs,fnchk,fnDedNames
12060   let fntop(program$,cap$="Pension Report")
12100 ! 
12120   dim em$*30,tcp(32),tdc(10),cp(32),ttdc(10)
12140   dim dedcode(20),calcode(20),dedfed(20),fullname$(20)*20,resp$(50)*60
12160   dim abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
12180   dim sel_ded(20),sel_pen(20)
12200   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
12260   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",Shr",internal,input,relative 
12280   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,input,keyed 
12300   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&",Shr",internal,input,relative 
12320 ! /r
18000   gosub SCREEN_PENSION1
18020   let fnopenprn ! 
18040   gosub HDR
18060 READ_EMPLOYEE: ! 
18080   read #1,using 'form pos 1,n 8,c 30,pos 99,c 11': eno,em$,ss$ eof FINIS
18100   let a=pos (rtrm$(em$)," ",1)
18120   let b=pos (rtrm$(em$)," ",a+1)
18140   let em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
18160   let reg_earnings=ded_pension=pension_amount=0
18180   let checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
20040   mat tcp=(0) : mat ttdc=(0)
20060   restore #4,key>=checkkey$: nokey READ_EMPLOYEE
20080 READ_TRANS: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof PRINT_ONE
22000   if heno=eno then 
22020     if prd<beg_date or prd>end_date then goto READ_TRANS
22040     mat tcp=tcp+cp : mat ttdc=ttdc+tdc
22060     for j=1 to 20
22080       if sel_ded(j)=1 and dedcode(j)=1 then let ded_pension+=cp(j+4) ! PENSION
22100       if sel_ded(j)=1 and dedcode(j)>1 then let ded_pension-=cp(j+4) ! PENSION
22120       if sel_pen(j)=1 then let pension_amount+=cp(j+4) ! PENSION
22140     next j
22160     let reg_earnings+=cp(31) ! REGULAR EARNINGS
22180     goto READ_TRANS ! 
22200   end if 
26000 PRINT_ONE: ! r:
26010   if pension_amount<>0 then ! skip if no pension wh
26020     print #255,using F_LINE_OUT: em$(1:24),ss$,reg_earnings,ded_pension,pension_amount,reg_earnings+ded_pension pageoflow PGOF
26040 F_LINE_OUT: form pos 1,c 24,c 12,4*n 12.2
26060     let total_salary+=reg_earnings
26080     let total_ded+=ded_pension
26100     let total_pension+=pension_amount
26120   end if 
26900   goto READ_EMPLOYEE ! /r
28000 FINIS: ! r:
28010   close #1: ioerr ignore
28020   close #2: ioerr ignore
28040   print #255: "                                      ----------  ----------  ----------  ---------- "
28060   print #255,using F_LINE_OUT: " "," ",total_salary,total_ded,total_pension,total_salary+total_ded
28080   print #255: "                                      =========-  ==========  ==========  ========== "
28120   let fncloseprn
28140   close #25: ioerr ignore
28160   goto XIT ! /r
29000 XIT: let fnxit
29020 IGNORE: continue 
29040 ! <Updateable Region: ERTN>
29060 ERTN: let fnerror(program$,err,line,act$,"xit")
29080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
29100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
29120   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
29140 ERTN_EXEC_ACT: execute act$ : goto ERTN
29160 ! /region
30000 HDR: ! r:
30020   print #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
30040   print #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
30060   print #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
30080   print #255: "\ql   "
30100   print #255: "Name                    SS Number     Total Wage     Ded/Add     Pension  Pension Wage"
30120   return  ! /r
32000 PGOF: ! r:
32020   print #255: newpage
32040   gosub HDR
32200   continue  ! /r
60000 SCREEN_PENSION1: ! r:
60010   let fntos(sn$="Pension-1")
60020   let rc=cf=0
60040   let fnfra(1,1,21,23,"Deductions Effecting Pension Wage","Mark any deduction that either needs to be added to gross wages or deducted from gross wages before calculating the Pension Wage",0)
60060   let cf+=1 : let fratype=cf
60080   for j=1 to 20
60100     let fnchk(j,3,fullname$(j),0,fratype)
60120     let resp$(rc+=1)="False"
60140   next j
60160   let fnfra(1,30,20,23,"Pension Deduction","Mark the pension deduction that you want printed on the report",0)
60180   let cf+=1 : let fratype=cf
60200   for j=1 to 20
60220     let fnopt(j,3,fullname$(j),0,fratype)
60240     let resp$(rc+=1)="False"
60260   next j
60280   let fnfra(1,60,3,42,"Date Range","Enter the beginning and ending date range covered by this report.")
60300   let cf+=1 : let fradate=cf : let mylen=26 : let mypos=mylen+2
60320   let fnlbl(1,1,"Starting Date:",mylen,1,0,fradate)
60340   let fntxt(1,mypos,10,0,1,"3",0,empty$,fradate)
60360   let resp$(rc+=1)=str$(beg_date)
60380   let fnlbl(2,1,"Ending Date:",mylen,1,0,fradate)
60400   let fntxt(2,mypos,10,0,1,"3",0,empty$,fradate)
60420   let resp$(rc+=1)=str$(end_date)
60440   let fncmdkey("Next",1,1,0,"Prints the report")
60460   let fncmdkey("Cancel",5,0,1,"Returns to menu")
60480   let fnacs(sn$,0,mat resp$,ckey)
60500   if ckey=5 then goto XIT
60520   for j=1 to 20
60540     if resp$(j)="True" then let sel_ded(j)=1
60560   next j
60580   for j=1 to 20
60600     if resp$(j+20)="True" then let sel_pen(j)=1
60620   next j
60640   let beg_date=val(resp$(41))
60660   let end_date=val(resp$(42))
60680   return  ! /r
