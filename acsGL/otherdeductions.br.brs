00010 ! Replace S:\acsGL\OtherDeductioins
00020 ! -- Other deductions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$,fntos,fnfra,fntxt,fncmdkey,fnacs,fndate_mmddyy_to_ccyymmdd,fnlbl
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,miscname$(10)*20,dedcode(10),cap$*128,totalded(10)
00080   dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20,empd(22)
00090   dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Other Deductions Registers")
00120   fncno(cno,cnam$) !:
        fndat(dat$)
00130 ! ______________________________________________________________________
00140   fntos(sn$="OtherDed") !:
        let rc=cf=0: let mylen=22: let mypos=mylen+3: let frameno=1
00150   fnfra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included in this report.")
00160   fnlbl(1,1,"Beginning Date:",mylen,1,0,frameno)
00170   fntxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno) !:
        let resp$(rc+=1)=str$(beg_date)
00180   fnlbl(2,1,"Ending Date:",mylen,1,0,frameno)
00190   fntxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno) !:
        let resp$(rc+=1)=str$(end_date)
00200   fncmdkey("Next",1,1,0,"Print report.")
00210   fncmdkey("Cancel",5,0,1,"Returns to menu without printing.")
00220   fnacs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT
00240   beg_date=val(resp$(1)) !:
        end_date=val(resp$(2))
00250   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative: read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,POS 407,PD 5.3,PD 5.2,POS 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode !:
        close #1: 
00260   for j=1 to 10
00270     let miscname$(j)=lpad$(rtrm$(miscname$(j)(1:9)),9)
00280   next j
00290   let nametab=66-int(len(rtrm$(cnam$))/2)
00300   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00310   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",Shr",internal,outin,relative 
00320   let report$="Other Deductions Register"
00330   fnopenprn
00340   gosub L620
00350 L350: if d(1)>0 then goto L360 else goto L400
00360 L360: if sum(empd)=0 then goto L400
00370   pr #255,using L700: eno,"Total",empd(9),empd(10),empd(11),empd(12),empd(13),empd(14),empd(15),empd(16),empd(17),empd(18) pageoflow L950
00380   pr #255: 
00390   mat empd=(0)
00400 L400: read #1,using 'Form POS 1,N 4,3*C 25,POS 271,2*N 5': eno,mat k$,mat adr eof L720
00410   mat empd=(0)
00420   if adr(1)=0 then goto L400
00430   ca=adr(1)
00440 L440: read #2,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L400
00450   if fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date then goto L480
00460   gosub L670
00470   gosub L790
00480 L480: if nca=0 then goto L350
00490   ca=nca
00500   goto L440
00510 ! ______________________________________________________________________
00520 HEADER: ! 
00530   pr #255,using L540: date$('mm/dd/yy'),time$,cnam$
00540 L540: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
00550   let p1=66-int(len(rtrm$(report$))/2)
00560   pr #255,using L570: rtrm$(report$)
00570 L570: form pos p1,c 50
00580   let p1=66-int(len(rtrm$(dat$))/2)
00590   pr #255,using L570: rtrm$(fnpedat$)
00600   return 
00610 ! ______________________________________________________________________
00620 L620: gosub HEADER
00630   pr #255: 
00640   pr #255,using L650: "Emp #","Name",miscname$(1)(1:9) ,miscname$(2)(1:9),miscname$(3)(1:9),miscname$(4)(1:9),miscname$(5)(1:9),miscname$(6)(1:9),miscname$(7)(1:9),miscname$(8)(1:9),miscname$(9)(1:9),miscname$(10)(1:9)
00650 L650: form pos 1,c 6,c 21,10*c 10
00660   return 
00670 L670: ! pr details
00680   pr #255,using L700: eno,k$(1)(1:20),d(9),d(10),d(11),d(12),d(13),d(14),d(15),d(16),d(17),d(18)
00690   mat empd=empd+d
00700 L700: form pos 1,n 5,x 1,c 20,10*n 10.2
00710   return 
00720 L720: ! pr TOTALS
00730   pr #255,using L740: "---------","---------","---------","---------","---------","---------","---------","---------","---------","---------"
00740 L740: form pos 28,10 *c 10
00750   pr #255,using L770: "","Totals",mat totalded
00760   form pos 1,c 6,c 20,10*n 10.2
00770 L770: form pos 1,c 6,c 20,10*n 10.2
00780   fncloseprn : goto XIT
00790 L790: ! ACCUMULATE TOTALS
00800   for j=1 to 10
00810     let totalded(j)+=d(j+8)
00820   next j
00830   return 
00840   pr #255: newpage
00850   gosub L620
00860   continue 
00870 XIT: let fnxit
00880 ! ______________________________________________________________________
00890 ! <updateable region: ertn>
00900 ERTN: let fnerror(program$,err,line,act$,"xit")
00910   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00920   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00930   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00940 ERTN_EXEC_ACT: execute act$ : goto ERTN
00950 L950: ! /region
