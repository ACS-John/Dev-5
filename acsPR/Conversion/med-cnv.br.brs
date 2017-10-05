00010 !  Replace S:\acsPR\Conversion\Med-Cnv
00020 ! Split out Medicare if converted to new payroll in 2000
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnputcno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,d(14),ty(21),s(13),t(13),z$*8,desc$(6)*15,amt(6)
00080   dim w(13),a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12
00090   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),dedcode(10)
00100   dim fm4$*255,in4$(30)
00110 ! ______________________________________________________________________
00120   fncno(cno)
00130   pr newpage
00140   pr f "10,20,c 21,n": "Company Number:"
00150 L150: rinput fields "10,41,Nz 5,UE,N": cno conv L150
00155   fnputcno(cno)
00160   let fm4$="Form  Pos 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
00170   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&",Shr",internal,input ioerr L150 !:
        read #1,using 'Form POS 1,3*C 40,C 12,POS 150,10*C 8,N 2,POS 317,10*C 12,POS 618,10*N 1,POS 638,10*N 1,POS 133,PD 6.3,PD 6.2,POS 236,PD 3.3,PD 4.2': mat a$,b$,mat d$,loccode,mat e$,mat dedcode,mat dedfed,mcrate,mcmax,ssrate,ssmax !:
        close #1: ! company was prcoinfo before conversion
00180   mcmax=9999999
00190   for j=1 to 3: a$(j)=a$(j)(1:30): next j
00200   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00210   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,outin,relative 
00220 L220: read #1,using L230: eno,mat em$,ss$,em6,ta eof XIT
00230 L230: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
00240 L240: read #2,using L250,rec=ta: teno,tcd,mat ty,ta
00250 L250: form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
00260   if tcd<1 or tcd>10 then tcd=1
00270   dedfica=0
00280   dedret=0
00290   for j=1 to 10
00300     if dedfed(j)>=1 and dedcode(j)=1 then dedret+=ty(j+3)
00310 ! IF DEDFED(J)=1 AND DEDCODE(J)=1 THEN tDEDRET+=TY(J+3)
00320     if dedfed(j)=2 and dedcode(j)=1 then dedfica+=ty(j+3)
00330   next j
00340   let w(2)=w(2)+ty(21)-dedret !:
        ! TOTAL TAXABLE WAGES
00350   let w3=w3+ty(2)+ty(15) !:
        ! FICA W/H YTD
00360   let w(11)=w(11)+ty(21)-dedfica !:
        ! TOTAL MC WAGES & TIPS
00370   if em6=1 then let w(11)=0 ! NO MC
00380   let w(3)=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) !:
        ! SS WH !:
        ! change to seperate medicare
00390   let w(12)=w3-w(3) !:
        ! MEDICARE WITHHELD  !:
        ! change to seperate medicare
00400   if em6=1 then let w(12)=0 : let w(3)=w3 !:
          ! NO MC ALL SS !:
          ! change to seperate medicare
00410   if em6=2 then let w(3)=0 : let w(12)=w3 !:
          ! NO SS ALL MC !:
          ! change to seperate medicare
00420   if em6=9 then let w(3)=w(5)=w(11)=w(12)=0 !:
          ! NO SS OR MC
00430   ty(2)=w(3)
00440   ty(15)=w3-w(3)
00450   rewrite #2,using 'Form POS 173,PD 5.2,POS 238,PD 5.2': ty(2),ty(15)
00460   mat w=(0)
00470   nqp=dcb=w3=0
00480   if ta>0 then goto L240 else goto L220
00490 ! ______________________________________________________________________
00500 XIT: stop 
00510 ! ______________________________________________________________________
00520 ! <updateable region: ertn>
00530 ERTN: fnerror(program$,err,line,act$,"xit")
00540   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00550   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00560   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00570 ERTN_EXEC_ACT: execute act$ : goto ERTN
00580 ! /region
00590 ! ______________________________________________________________________
