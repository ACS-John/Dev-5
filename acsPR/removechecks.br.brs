00010 ! Replace S:\acsPR\RemoveChecks
00020 ! Remove Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnacs,fntos,fntxt,fndate_mmddyy_to_ccyymmdd,fncmdset,fnlbl
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim de$*30,cap$*128,tr$(5)*35,cp(32),tdc(10)
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100   fntop(program$,"Remove Old Payroll Checks")
00110   cancel=99 : right=1 : center=2 : on=1 : off=0 !:
        left=0
00120   open #1: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",NoShr",internal,outin,keyed 
00130   open #work1:=2: "Name="&env$('Q')&"\PRmstr\Work1."&wsid$&",Size=0,RecL=224,replace",internal,outin,relative 
00140   fntos(sn$='RemoveChecks') !:
        mylen=22 : mypos=mylen+3 : lc=0
00150   fnlbl(lc+=1,1,"Oldest Date to Retain:",mylen,1)
00160   fntxt(lc,mypos,10,0,0,'1003') !:
        resp$(1)=str$(date('ccyymmdd')-50000)
00170   lc+=1
00180   fnlbl(lc+=1,1,"All transactions with a",mylen*2,center)
00190   fnlbl(lc+=1,1,"date prior to this date will be removed.",mylen*2,center)
00200   fncmdset(2)
00210   fnacs(sn$,0,mat resp$,ckey)
00220   if ckey=5 or ckey=cancel then goto XIT else !:
          rd1=val(resp$(1))
00230 READ_CHECKS: ! 
00240   read #1,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp eof END1
00250   if prd>=rd1 then goto KEEP
00260   goto READ_CHECKS
00270 ! ______________________________________________________________________
00280 KEEP: ! 
00290   write #work1,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp
00300   goto READ_CHECKS
00310 ! ______________________________________________________________________
00320 END1: ! 
00330   close #work1: 
00340   close #1,free: 
00350   execute "Rename "&env$('Q')&"\PRmstr\Work1."&wsid$&' '&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&" -n"
00360   execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&' '&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&" 1 17 Replace DupKeys -n"
00370   goto XIT
00380 ! ______________________________________________________________________
00390 XIT: fnxit
00400 ! ______________________________________________________________________
00410 ! <Updateable Region: ERTN>
00420 ERTN: fnerror(program$,err,line,act$,"xit")
00430   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
