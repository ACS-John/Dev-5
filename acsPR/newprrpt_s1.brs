00010 ! Replace acsPR\newprRpt_S1.brs,Source
00011 !
00012   library "S:\Core\Library": fnerror,fngethandle,fnGetPayrollDates,fnwin3b,fnopenprn,fncloseprn,fnLbl,fnTxt,fnCmdKey,fnAcs2,fnprocess,fnTos,fnconsole,fnStatus,fnStatusPause
00013   library "S:\Core\Library": fnPayPeriodEndingDate
00014   on error goto Ertn
00020 !
00030   dim em$(3)*30,ss$*11,rs(2),em(16),w4step2,w4Year$,W4Step3,tdt(4),tcd(3),tdet(20),tdy(6)
00031   dim tdc(6),ty(21),tqm(17),tcp(22),message$*40,cap$*40,resp$(10)*50
00032   dim rn$*2,rt$*78,ch$(2)*132,psc(100),ti(20),inp(20),pp(20),dt(125)
00033   dim gt(125),dh$*20,a$*40,cp(32),tdc(10),tcp(32),dc(10)
00039 !
00050   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
00051   d1=fnPayPeriodEndingDate
00080 !
00081   rn$=" 2"
00082   if fnprocess=1 then goto L141
00089 !
00090   gosub ASK_DATES
00124 !
00141 L141: ! On Fnkey 5 Goto EOF1
00142 fnopenprn
00143 !
00190 open #1: "Name=[Q]\PRmstr\PRREPORT.h[cno],KFName=[Q]\PRmstr\PRRPTIDX.h[cno],shr",internal,input,keyed 
00200 read #1,using L210,key=rn$: rt$,mat ch$,ips,tdep,cp,mat psc,mat inp,mat pp,mat ti
00210 L210: form pos 3,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
00220 close #1: 
00221 !
00300 open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
00301 open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Use,RecL=224,KPs=1,KLn=17",internal,outIn,keyed 
00302 open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\deptidx.h[cno]",internal,outIn,keyed 
00320 gosub HDR
00330 goto PRTRPT
00331 !
00340 PGOF: ! r:
00342   pr #255: newpage
00350   gosub HDR
00360 continue  ! /r
00370 HDR: ! r:
00371   pr #255: 
00372   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
00373   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
00374   pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
00375   pr #255: "\qc  {\f181 \fs16 From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
00376   pr #255: "\qc  {\f181 \fs16 "&trim$(dh$)&"}"
00377   pr #255: "\ql   "
00400   pr #255: ch$(1)
00410   pr #255: ch$(2)
00420 return ! /r
00430 EOF1: ! r:
00450   close #1: ioerr ignore
00460   close #2: ioerr ignore
00465   fncloseprn ! /r
00469 Xit: chain "S:\acsPR\newprRpt3"
00470 ignore: continue
19800 PRTRPT: ! r:
19801 read #1,using "Form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,n 1,C 4,x 1,c 12,n 6,n 12.2": eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4step2,w4Year$,ph$,bd,W4Step3 eof EOF1 ! fnStatus(str$(eno))
19802   ipsw=0
19803   restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey PRTRPT
19804   mat tcp=(0): mat tdc=(0)
19805 L19804: read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L20000
19806   if teno<>eno then goto L20000
19807   gosub ADD_EARNINGS
19808   if ips=0 then goto L19899
19899   L19899: if tdep=1 then goto F_PR_OUT
19900   pr #255, using F_PR_OUT: ss$,em$(1),tcp(9),tcp(10),tcp(11) pageoflow PGOF
19910   F_PR_OUT: form pos 1,c 11,pos 15,c 30,pos 30,pic(--,---,---.##),pos 45,pic(--,---,---.##),pos 57,pic(--,---,---.##)
19989   ! If TDEP=0 Then Mat TCP=(0): Mat TDC=(0)
19990 goto L19804
20000 L20000: ! 
20099   goto PRTRPT ! /r
20100 L20100: ! r: Check for Totals to pr ______________
21200 goto EOF1 ! /r
50000 ERTN: ! r:
50001   fnerror(program$,err,line,act$,"Xit") 
50010   if uprc$(act$)<>"PAUSE" then goto L50040
50020   execute "list "&str$(line) : pause : goto L50040
50030   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
50040   L50040: execute act$
50050 goto ERTN
50060 ! /r
60000 ADD_EARNINGS: ! r:
60001   mat tcp=(0): mat tdc=(0)
60004   ipsw=9
60005   checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
60010   restore #4,key>=checkkey$: nokey L60080
60020 L60020: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn2,prd,ckno,mat dc,mat cp eof L60080
60030   if heno<>eno then goto L60080
60040   if prd<beg_date or prd>end_date then goto L60020
60045   if tdn<>tdn2 then goto L60020 ! was    TDEP=1 AND
60050   holdckno=ckno
60060   mat tcp=tcp+cp : mat tdc=tdc+dc
60070   goto L60020
60080 L60080: return ! /r
61000 ASK_DATES: ! r:
61005   fnTos
61006   mylen=30: mypos=mylen+3
61020   fnLbl(1,1,"Starting Date (ccyymmdd):",mylen,1,0,fradate)
61030   fnTxt(1,mypos,10,0,1,"3",0,"The report can be run for any date range.  Enter the first date to be used.",0) 
61032   resp$(1)=str$(beg_date)
61040   fnLbl(2,1,"Ending Date (ccyymmdd):",mylen,1,0,0)
61050   fnTxt(2,mypos,10,0,1,"3",0,"Enter the last date to be used.  For a single payroll, use the same beginning and ending dates.",0) 
61051   resp$(2)=str$(end_date)
61052   fnLbl(3,1,"Report Heading Date:",mylen,1,0,0)
61054   fnTxt(3,mypos,20,0,0,"",0,"Enter the date you want shown on the reports.",0)
61056   resp$(3)=date$("Month DD, CCYY")
61060   fnCmdKey("Next",1,1,0,"Prints the report")
61070   fnCmdKey("Cancel",5,0,1,"Returns to menu")
61080   fnAcs2(mat resp$,ckey) 
61082   if ckey=5 then goto Xit
61090   beg_date=val(resp$(1))
61100   end_date=val(resp$(2))
61110   dh$=resp$(3)
61120 return ! /r
