00010 ! Replace S:\acsPR\newprOPDReg
00020 ! Other Pay and Deductions Register
00030 ! r: setup library, on error, dims, open files, etc
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnprocess,fnDedNames,fnGetPayrollDates
00050   on error goto ERTN
00060 ! 
00070   dim em$*30,tdc(10),tcp(32),rptemp(25),rptot(25),cap$*128,message$*40
00080   dim dat$*20,name$(27)*8,cp(32),printline1(20)
00090   dim printline1(20),printline2(20),dedname$(20)*8
00100   dim fullname$(20)*20,abbrevname$(20)*8, newdedcode(20)
00110 ! ______________________________________________________________________
00130   let fntop(program$,cap$="Other Pay and Deductions Register")
00140   fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode)
00170   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
00200   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00201 ! r: setup mat name$, mat dedname$, numberded1, numberded2
00202   let name$(1)="O/T"
00203   let name$(2)="Other"
00204   let name$(3)="Meals"
00205   let name$(4)="Tips"
00206   let name$(5)="Total"
00207   let name$(26)="EIC"
00208   let name$(27)="Total"
00209   let numberded=0
00210   for j=6 to 25
00211     if trim$(abbrevname$(j-5))<>"" then 
00212       let dedname$(numberded+=1)=abbrevname$(j-5)(1:5)
00213     end if 
00214   next j
00215   mat dedname$(numberded)
00216   let numberded1=max(round(numberded/2,0),1) ! # of deductions listed on line 1
00217   let numberded2=max(int(numberded/2),1) ! # of deductions listed on line 2
00218 ! /r
00219   if fnprocess=1 then goto START_REPORT
00220 ! /r
00230 ASK_PAYROLL_DATE: ! r:
00240   let fntos(sn$="OtherPayded") !:
        let respc=0
00250   let fnlbl(1,1,"",34,1) ! bigger screen
00260   let fnlbl(2,1,"Payroll Date:",20,1)
00270   let fntxt(2,23,10,0,1,"3",0,"You can pr or reprint for any pay period.  Normally you would use the last payroll date.")
00280   let resp$(respc+=1)=str$(d1)
00290   let fncmdkey("&Next",1,1,0,"Proceed with importing time." ) !:
        let fncmdkey("E&xit",5,0,1,"Returns to menu")
00300   let fnacs(sn$,0,mat resp$,ckey) ! ask employee #
00310   if ckey=5 then goto XIT
00320   let ppd=val(resp$(1))
00330 ! /r
00340 START_REPORT: !  r: main report loop
00480 ! 
00490   on fkey 5 goto DONE
00500   let fnopenprn
00510   gosub HDR
00520   open #1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00530 L530: read #1,using "Form POS 1,N 8,C 30,pos 162,n 6": eno,em$,lastpaydate eof FINALTOTALS
00535   if fndate_mmddyy_to_ccyymmdd(lastpaydate)<>ppd then goto L530
00540   mat rptemp=(0): mat tcp=(0)
00550   a=pos (rtrm$(em$)," ",1)
00560   b=pos (rtrm$(em$)," ",a+1)
00570   ! if env$('client')="West Rest Haven" then goto L590
00580   let em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a)
00590 ! L590: form pos 1,n 8,c 30,pos 162,n 6,pos 173,2*pd 3
00600   checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
00610   restore #4,key>=checkkey$: nokey L530
00620 L620: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L680
00630   if heno<>eno then goto L680
00640   if prd><ppd then goto L620
00650   let holdckno=ckno
00660   mat tcp=tcp+cp : mat ttdc=ttdc+tdc
00670   goto L620
00680 L680: if sum(tcp)=0 and sum(ttdc)=0 then goto L530 ! no pay on this person for this payroll date
00690   for j=1 to 25
00700     if j>4 then goto L730
00710     let rptemp(j)=rptemp(j)+tcp(j+26)
00720     goto L740
00730 L730: let rptemp(j)=rptemp(j)+tcp(j)
00740 L740: next j
00750   let eic+=tcp(25)
00760   let totaleic+=tcp(25)
00770   gosub L790
00780   goto L530
00790 L790: mat rptot=rptot+rptemp
00800   for j=1 to 15
00810     if rptemp(j)><0 then goto L830
00820   next j
00830 L830: let tottemp=0
00840   for j=1 to 20
00850     if newdedcode(j)=3 then goto L900
00860     if newdedcode(j)=1 then goto L890
00870     let tottemp=tottemp-rptemp(j+4)
00880     goto L900
00890 L890: let tottemp=tottemp+rptemp(j+4)
00900 L900: next j
00910   let tottemp=tottemp-rptemp(25)
00920   let totded=totded+tottemp
00930   let rpxxxx=rptemp(1)+rptemp(2)+rptemp(3)+rptemp(4)
00940   let w=x=z=0: let y=4: for j=1 to 20
00950     if trim$(abbrevname$(j))="" then goto L1000
00960     let w+=1: if int(w/2)=w/2 then goto L990 else goto L970
00970 L970: let printline1(x+=1)=rptemp(j+4) ! set variables to pr line
00980     goto L1000
00990 L990: let printline2(z+=1)=rptemp(j+4) ! set line two
01000 L1000: next j
01010   mat printline1(numberded1): mat printline2(numberded2)
01020   if int(numberded1/2)=numberded1/2 then pr #255,using L1040: eno,em$(1:15),rptemp(1),rptemp(2),rptemp(3),rptemp(4),rpxxxx,mat printline1,tottemp pageoflow PGOF else pr #255,using L1050: eno,em$(1:15),rptemp(1),rptemp(2),rptemp(3),rptemp(4),rpxxxx,mat printline1,tottemp pageoflow PGOF
01030   pr #255,using L1060: mat printline2 pageoflow PGOF
01040 L1040: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,n 12.2,skip 1
01050 L1050: form pos 1,pic(zzzzzzzz),pos 10,c 15,n 7.2,n 8.2,pos 40,n 6.2,n 6.2,pos 52,n 8.2,pos 60,numberded1*n 12.2,x 6,n 12.2,skip 1
01060 L1060: form pos 66,numberded2*n 12.2,skip 1
01070   return  ! /r
01090 PGOF: ! r:
01100   pr #255: newpage
01110   gosub HDR
01120   continue 
01130   return  ! /r
01150 FINALTOTALS: ! r:
01160   pr #255,using 'form skip 2,pos 10,c 12,skip 2': "Final Totals"
01180   mat rptemp=rptot
01190   let tottemp=totded
01200   let rpxxxx=rptemp(1)+rptemp(2)+rptemp(3)+rptemp(4)
01210   for j=1 to 27
01220     if j=5 then goto L1320
01230     if j=26 then goto L1340
01240     if j=27 then goto L1360
01250     if j>5 and j<26 then goto L1290
01260     pr #255,using L1270: name$(j),rptemp(j)
01270 L1270: form pos 5,c 8,pos 12,pic(---,---,---.##),skip 1
01280     goto L1370
01290 L1290: if trim$(abbrevname$(j-5))="" then goto L1310 ! skip ded if not used
01300     pr #255,using L1270: rtrm$(abbrevname$(j-5)),rptemp(j-1)
01310 L1310: goto L1370
01320 L1320: pr #255,using L1270: name$(j),rpxxxx
01330     goto L1370
01340 L1340: pr #255,using L1270: name$(j),totaleic
01350     goto L1370
01360 L1360: pr #255,using L1270: name$(j),tottemp
01370 L1370: next j
01380   goto DONE ! /r
01400 DONE: ! r:
01410   close #1: ioerr ignore
01420   close #2: ioerr ignore
01430   let fncloseprn
01440 XIT: let fnxit ! /r
01450 IGNORE: continue 
01460 HDR: ! r:
01470 ! pr #255,Using 1380: DATE$,TIME$,CAP$
01480   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
01490   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
01500   pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
01510   pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
01520   pr #255: "\ql   "
01530 ! 
01540   form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 49,c 33,skip 1
01550   pr #255,using L1560: dat$
01560 L1560: form pos dattab,c 20,skip 2
01570   pr #255,using L1580: "<------------Other Pay------------>","     Other Deductions-->"
01580 L1580: form pos 25,c 35,pos 60,c 34,pos 94,c 39,skip 1
01590   if int(numberded1/2)=numberded1/2 then let total$=" Total" else let total$="       Total"
01600   pr #255,using L1610: "Number","Name",name$(1),name$(2),name$(3),name$(4),name$(5),mat dedname$,total$
01610 L1610: form pos 2,c 6,pos 14,c 4,pos 29,c 6,c 6,c 7,c 7,c 7,pos 66,numberded*c 6,c 12,skip 1
01620   return  ! /r
01640 ! <Updateable Region: ERTN>
01650 ERTN: let fnerror(program$,err,line,act$,"xit")
01660   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01670   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01680   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01690 ERTN_EXEC_ACT: execute act$ : goto ERTN
01700 ! /region
