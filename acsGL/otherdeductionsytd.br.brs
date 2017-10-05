00010 ! Replace S:\acsGL\OtherDeductionsYTD
00020 ! -- Other deductions Ytd/Qtd
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,miscname$(10)*20,dedcode(10),cap$*128,totalytd(10)
00080   dim totalqtd(10)
00090   dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
00100   dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Other Deductions Registers-YTD QTD")
00130   fncno(cno,cnam$) !:
        fndat(dat$)
00140   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative: read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,POS 407,PD 5.3,PD 5.2,POS 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode !:
        close #1: 
00150   for j=1 to 10
00160     miscname$(j)=lpad$(rtrm$(miscname$(j)(1:9)),9)
00170   next j
00180   nametab=66-int(len(rtrm$(cnam$))/2)
00190   open #1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00200   open #2: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&str$(cno)&",Shr",internal,outin,relative 
00210   report$="Other Deductions Register-YTD QTD"
00220   fnopenprn(cp,58,220,0)
00230   gosub L390
00240 L240: read #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m eof L490
00250   gosub L440
00260   gosub L570
00270   goto L240
00280 ! ______________________________________________________________________
00290 HEADER: ! 
00300   pr #255,using L310: date$('mm/dd/yy'),time$,cnam$
00310 L310: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
00320   p1=66-int(len(rtrm$(report$))/2)
00330   pr #255,using L340: rtrm$(report$)
00340 L340: form pos p1,c 50
00350   p1=66-int(len(rtrm$(dat$))/2)
00360   pr #255,using L340: rtrm$(fnpedat$)
00370   return 
00380 ! ______________________________________________________________________
00390 L390: gosub HEADER
00400   pr #255: 
00410   pr #255,using L420: "Emp #","Name",miscname$(1)(1:9) ,miscname$(2)(1:9),miscname$(3)(1:9),miscname$(4)(1:9),miscname$(5)(1:9),miscname$(6)(1:9),miscname$(7)(1:9),miscname$(8)(1:9),miscname$(9)(1:9),miscname$(10)(1:9)
00420 L420: form pos 1,c 6,c 21,10*c 10
00430   return 
00440 L440: ! pr details
00450   pr #255,using L470: eno,k$(1)(1:16)&"YTD",m(11),m(13),m(15),m(17),m(19),m(21),m(23),m(25),m(27),m(29)
00460   pr #255,using L470: eno,"                QTD ",m(12),m(14),m(16),m(18),m(20),m(22),m(24),m(26),m(28),m(30)
00470 L470: form pos 1,n 5,x 1,c 20,10*n 10.2
00480   return 
00490 L490: ! pr TOTALS
00500   pr #255,using L510: "---------","---------","---------","---------","---------","---------","---------","---------","---------","---------"
00510 L510: form pos 28,10 *c 10
00520   pr #255,using L550: "","Totals-YTD",mat totalytd
00530   pr #255,using L550: "","Totals-QTD",mat totalqtd
00540   form pos 1,c 6,c 20,10*n 10.2
00550 L550: form pos 1,c 6,c 20,10*n 10.2
00560   fncloseprn : goto XIT
00570 L570: ! ACCUMULATE TOTALS
00580   for j=1 to 10
00590     totalytd(j)+=m(j*2+9)
00600     totalqtd(j)+=m(j*2+10)
00610   next j
00620   return 
00630   pr #255: newpage
00640   gosub L390
00650   continue 
00660 XIT: fnxit
00670 ! ______________________________________________________________________
00680 ! <updateable region: ertn>
00690 ERTN: fnerror(program$,err,line,act$,"xit")
00700   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00710   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00720   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00730 ERTN_EXEC_ACT: execute act$ : goto ERTN
00740 ! /region
