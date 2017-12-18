00010 ! Replace S:\acsUB\Bill-Rpt
00020 ! pr utility billing reports based on bills
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fnchk,fncmbrt2,fntos,fnopenprn,fncloseprn,fnerror,fncno,fnxit,fndat,fnLastBillingDate,fncmdset,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,z$*10,e$(4)*30,temp$(3)*26,resp$(4)*40,cnam$*40,dat$*20
00080 ! ______________________________________________________________________
00090   fntop("S:\acsUB\Bill-Rpt", cap$="Final Billing")
00100   fncno(cno,cnam$) !:
        ! 
00110   fnLastBillingDate(d1)
00120   fndat(dat$,1)
00130 ! ______________________________________________________________________
00140 SCR1: ! 
00150   fntos(sn$="Bill-Rpt")
00160   fnlbl(1,1,"Billing Date:",15,1)
00170   fntxt(1,17,8,8,1,"1",0,"Only enter the billing date if you wish to limit the report to those billed and finaled this month. (mmddyy)") !:
        resp$(1)=str$(d1)
00180   fnlbl(2,1,"Route Number:",15,1)
00190   fncmbrt2(2,17,0) !:
        resp$(2)= "[All]"
00200   fnchk(4,2,"Outstanding Balances Only") !:
        resp$(3)="False"
00210   fncmdset(3)
00220   fnacs(sn$,0,mat resp$,ck)
00230   if ck=5 then goto XIT
00240   d1= val(resp$(1)) conv SCR1 !:
        if uprc$(resp$(2))=uprc$("[All]") then route=0 else !:
          route=val(resp$(2))
00250   if resp$(3)="False" then oob$="N" else oob$="Y"
00260   goto STARTREPORT
00270 ! ______________________________________________________________________
00280 DONE: ! 
00290   fncloseprn
00300 XIT: fnxit
00310 ! ______________________________________________________________________
00320 STARTREPORT: ! 
00330   fnwait("Printing: please wait...",1)
00340   on fkey 5 goto DONE
00350   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00360   fnopenprn
00370   gosub HEADER
00380   goto REPORT
00390 ! ______________________________________________________________________
00400 HEADER: ! 
00410   if d1<>0 then temp$(1)="Billing Date: "&cnvrt$("pic(zz/zz/zz)",d1)
00420   if oob$="Y" then temp$(2)="Only Outstanding Balances"
00430   pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
00440   pr #255: "\qc  {\f181 \fs22 \b "&env$('program_caption')&"}"
00450   pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
00460   if d1<>0 or oob$="Y" then !:
          pr #255: "\qc "&trim$(temp$(1))&"   "&temp$(2)
00470   pr #255,using L480: "\ql  ","Page "&str$(pg+=1)
00480 L480: form pos 1,c 82,c 10,skip 1
00490   pr #255: "{\ul Act.Number} {\ul Customer Name                 } {\ul    Balance} {\ul Billing Date} {\ul  Deposit}"
00500   return 
00510 ! ______________________________________________________________________
00520 PGOF: ! !:
        pr #255: newpage !:
        gosub HEADER !:
        continue 
00530 ! ______________________________________________________________________
00540 REPORT: ! 
00550 L550: read #1,using 'Form POS 1,C 10,4*C 30,POS 1821,N 1,POS 292,PD 4.2,PD 4,POS 227,PD 5,POS 1741,N 2,pos 185,4*pd 4.2': z$,mat e$,finalbil,bal,lastbilldate,usage,extra(1),watdep,sewdep,elecdep,gasdep eof DONE
00551   deposit=watdep+sewdep+elecdep+gasdep
00560   if finalbil>0 then goto L570 else goto L550
00570 L570: if d1<>0 and d1<>lastbilldate then goto REPORT
00580   if route>0 and extra(1)<>route then goto REPORT
00590   if oob$="Y" and bal<=0 then goto REPORT
00600   pr #255,using 'Form POS 1,C 10,X 1,C 30,X 1,N 10.2,X 3,PIC(ZZ/ZZ/ZZ),X 2,N 9.2': z$,e$(2),bal,lastbilldate,deposit pageoflow PGOF
00610   goto REPORT
00620 ! ______________________________________________________________________
00630 ! <Updateable Region: ERTN>
00640 ERTN: fnerror(program$,err,line,act$,"xit")
00650   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00660   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00670   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00680 ERTN_EXEC_ACT: execute act$ : goto ERTN
00690 ! /region
00700 ! ______________________________________________________________________
