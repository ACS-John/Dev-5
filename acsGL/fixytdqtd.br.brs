00010 ! Replace S:\acsGL\fixytdqtd
00020 ! -- PAYROLL REGISTER
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$,fntos,fnfra,fnlbl,fntxt,fncmdkey,fnacs,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,miscname$(10)*20,dedcode(10),cap$*128,empd(22)
00080   dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
00090   dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Fix YTD - QTD Earnings")
00120   fncno(cno,cnam$) !:
        fndat(dat$)
00130 ! ______________________________________________________________________
00140   fntos(sn$="FixYTDQTD") !:
        let rc=cf=0: let mylen=28: let mypos=mylen+3: let frameno=1
00150   fnfra(1,1,3,45,"Date Range to Fix Quarter To Date Earnings","Enter the date range for the payrolls to be included in this quarter. Leave blank to skip quarter.")
00160   fnlbl(1,1,"Beginning Date of Quarter:",mylen,1,0,frameno)
00170   fntxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno) !:
        let resp$(rc+=1)=str$(beg_date)
00180   fnlbl(2,1,"Ending Date of Quarter:",mylen,1,0,frameno)
00190   fntxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this quarter. Blank if not fixing quarter.",frameno) !:
        let resp$(rc+=1)=str$(end_date)
00200   let frameno=2
00210   fnfra(6,1,3,45,"Date Range to Fix YTD Earnings.","Enter the date range for the payrolls to be included in year to date earnings. Leave blank to skip fixing the year to date earnings.")
00220   fnlbl(1,1,"Beginning Date of the Year:",mylen,1,0,frameno)
00230   fntxt(1,mypos,12,0,1,"3",0,"Enter the first day of the year. Leave blank in only fixing the quarter.",frameno) !:
        let resp$(rc+=1)=str$(begytd_date)
00240   fnlbl(2,1,"Ending Date of Year:",mylen,1,0,frameno)
00250   fntxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in then annual figures. Blank if not fixing year to date.",frameno) !:
        let resp$(rc+=1)=str$(endytd_date)
00260   fncmdkey("Next",1,1,0,"Fix earnings records.")
00270   fncmdkey("Cancel",5,0,1,"Returns to menu without printing.")
00280   fnacs(sn$,0,mat resp$,ckey)
00290   if ckey=5 then goto XIT
00300 ! 
00310   beg_date=val(resp$(1)) !:
        end_date=val(resp$(2))
00320   begytd_date=val(resp$(3)) !:
        endytd_date=val(resp$(4))
00330   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative: read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,POS 407,PD 5.3,PD 5.2,POS 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode !:
        close #1: 
00340   let ficarate=ficarate/100 : let feducrat=feducrat/100 : let mcr=mcr/100
00350   let nametab=66-int(len(rtrm$(cnam$))/2)
00360   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00370   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",Shr",internal,outin,relative 
00380 L380: read #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m,mat adr eof XIT
00390   let fixqtr=fixytd=0
00400   if beg_date>0 and end_date>0 then let fixqtr=1: goto L410 else goto L440
00410 L410: for j=2 to 36 step 2 ! set qtd to zero
00420     let m(j)=0
00430   next j
00440 L440: if begytd_date>0 and endytd_date>0 then let fixytd=1: goto L450 else goto L480
00450 L450: for j=1 to 35 step 2 ! set year to date to zero
00460     let m(j)=0
00470   next j
00480 L480: if adr(1)=0 then goto REWRITE_MASTER
00490   ca=adr(1)
00500 L500: read #2,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca norec REWRITE_MASTER
00510   if fixqtr=0 or (fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date) then goto L560
00520   let x=3
00530   for j=2 to 36 step 2
00540     let m(j)+=d(x+=1) ! add quarterly info
00550   next j
00560 L560: if fixytd=0 or (fndate_mmddyy_to_ccyymmdd(d(2))<begytd_date or fndate_mmddyy_to_ccyymmdd(d(2))>endytd_date) then goto L610
00570   let x=3
00580   for j=1 to 35 step 2
00590     let m(j)+=d(x+=1)
00600   next j
00610 L610: if nca=0 then goto REWRITE_MASTER
00620   ca=nca
00630   goto L500
00640 REWRITE_MASTER: ! 
00650   rewrite #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m
00660   goto L380
00670 XIT: let fnxit
00680 ! ______________________________________________________________________
00690 ! <updateable region: ertn>
00700 ERTN: let fnerror(program$,err,line,act$,"xit")
00710   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00720   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00740 ERTN_EXEC_ACT: execute act$ : goto ERTN
00750 ! /region
