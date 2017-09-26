00010 ! Replace acsPR\newprRpt_S1.brs,Source
00011 ! ______________________________________________________________________
00012   library "S:\Core\Library": fnerror,fngethandle,fnGetPayrollDates,fnwin3b,fnopenprn,fncloseprn,fnlbl,fntxt,fncmdkey,fnacs,fnprocess,fntos,fnconsole,fnstatus,fnstatus_pause
00014   on error goto ERTN
00020 ! ______________________________________________________________________
00030   dim em$(3)*30,ss$*11,rs(2),em(16),ta(2),tdt(4),tcd(3),tdet(20),tdy(6)
00031   dim tdc(6),ty(21),tqm(17),tcp(22),message$*40,cap$*40,resp$(10)*50
00032   dim rn$*2,rt$*78,ch$(2)*132,psc(100),ti(20),inp(20),pp(20),dt(125)
00033   dim gt(125),dh$*20,a$*40,d1$*20,cp(32),tdc(10),tcp(32),dc(10)
00039 ! ______________________________________________________________________
00050   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
00080 ! ______________________________________________________________________
00081   let rn$=" 2"
00082   if fnprocess=1 then goto L141
00089 ! ______________________________________________________________________
00090   gosub ASK_DATES
00124 ! ______________________________________________________________________
00141 L141: ! On Fnkey 5 Goto EOF1
00142   let fnopenprn
00143 ! ______________________________________________________________________
00190   open #1: "Name="&env$('Q')&"\PRmstr\PRREPORT.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\PRRPTIDX.h"&env$('cno')&",shr",internal,input,keyed 
00200   read #1,using L210,key=rn$: rt$,mat ch$,ips,tdep,cp,mat psc,mat inp,mat pp,mat ti
00210 L210: form pos 3,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
00220   close #1: 
00221 ! ______________________________________________________________________
00300   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00301   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",Use,RecL=224,KPs=1,KLn=17",internal,outin,keyed 
00302   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\deptidx.h"&env$('cno'),internal,outin,keyed 
00320   gosub HDR
00330   goto PRTRPT
00331 ! ______________________________________________________________________
00340 PGOF: ! r:
00342   print #255: newpage
00350   gosub HDR
00360   continue  ! /r
00370 HDR: ! r:
00371   print #255: 
00372   print #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00373   print #255: "\qc "&" {\f221 \fs22 \b "&env$('cnam')&"}"
00374   print #255: "\qc "&" {\f201 \fs20 \b "&trim$(rt$)&"}"
00375   print #255: "\qc "&" {\f181 \fs16 "&"From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
00376   print #255: "\qc "&" {\f181 \fs16 "&trim$(dh$)&"}"
00377   print #255: "\ql   "
00400   print #255: ch$(1)
00410   print #255: ch$(2)
00420   return ! /r
00430 EOF1: ! r:
00450   close #1: ioerr ignore
00460   close #2: ioerr ignore
00465   let fncloseprn ! /r
00469 XIT: chain "S:\acsPR\newprRpt3"
00470 ignore: continue
19800 PRTRPT: ! r:
19801 read #1,using "Form POS 1,N 8,3*C 30,C 11,2*N 1,7*N 2,2*PD 3.3,6*PD 4.2,2*N 6,PD 5.2,2*PD 3,C 12,N 6": eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd eof EOF1 ! fnstatus(str$(eno))
19802   let ipsw=0
19803   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey PRTRPT
19804   mat tcp=(0): mat tdc=(0)
19805 L19804: read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L20000
19806   if teno<>eno then goto L20000
19807   gosub ADD_EARNINGS
19808   if ips=0 then goto L19899
19899 L19899: if tdep=1 then goto F_PR_OUT
19900   print #255, using F_PR_OUT: ss$,em$(1),tcp(9),tcp(10),tcp(11) pageoflow PGOF
19910 F_PR_OUT: form pos 1,c 11,pos 15,c 30,pos 30,pic(--,---,---.##),pos 45,pic(--,---,---.##),pos 57,pic(--,---,---.##)
19989 ! If TDEP=0 Then Mat TCP=(0): Mat TDC=(0)
19990   goto L19804
20000 L20000: ! 
20099   goto PRTRPT ! /r
20100 L20100: ! r: Check for Totals to print ______________
21200   goto EOF1 ! /r
50000 ERTN: ! r:
50001   let fnerror(program$,err,line,act$,"xit") 
50010   if uprc$(act$)<>"PAUSE" then goto L50040
50020   execute "list "&str$(line) : pause : goto L50040
50030   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause 
50040 L50040: execute act$
50050   goto ERTN
50060 ! /r
60000 ADD_EARNINGS: ! r:
60001   mat tcp=(0): mat tdc=(0)
60004   let ipsw=9
60005   let checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
60010   restore #4,key>=checkkey$: nokey L60080
60020 L60020: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn2,prd,ckno,mat dc,mat cp eof L60080
60030   if heno<>eno then goto L60080
60040   if prd<beg_date or prd>end_date then goto L60020
60045   if tdn<>tdn2 then goto L60020 ! was    TDEP=1 AND
60050   let holdckno=ckno
60060   mat tcp=tcp+cp : mat tdc=tdc+dc
60070   goto L60020
60080 L60080: return ! /r
61000 ASK_DATES: ! r:
61005   let fntos(sn$="UserRpt-1")
61006   let mylen=30: let mypos=mylen+3
61020   let fnlbl(1,1,"Starting Date (ccyymmdd):",mylen,1,0,fradate)
61030   let fntxt(1,mypos,10,0,1,"3",0,"The report can be run for any date range.  Enter the first date to be used.",0) 
61032   let resp$(1)=str$(beg_date)
61040   let fnlbl(2,1,"Ending Date (ccyymmdd):",mylen,1,0,0)
61050   let fntxt(2,mypos,10,0,1,"3",0,"Enter the last date to be used.  For a single payroll, use the same beginning and ending dates.",0) 
61051   let resp$(2)=str$(end_date)
61052   let fnlbl(3,1,"Report Heading Date:",mylen,1,0,0)
61054   let fntxt(3,mypos,20,0,0,"",0,"Enter the date you want shown on the reports.",0)
61056   let resp$(3)=d1$
61060   let fncmdkey("Next",1,1,0,"Prints the report")
61070   let fncmdkey("Cancel",5,0,1,"Returns to menu")
61080   let fnacs(sn$,0,mat resp$,ckey) 
61082   if ckey=5 then goto XIT
61090   let beg_date=val(resp$(1))
61100   let end_date=val(resp$(2))
61110   let dh$=resp$(3)
61120   return ! /r
