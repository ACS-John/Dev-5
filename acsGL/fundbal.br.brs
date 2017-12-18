00010 ! Replace S:\acsGL\FundBal
00020 ! Fund Balance Report
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror
00050   fntop(program$,"CHANGE_ME")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cnam$*40,pedat$*20,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
00090   dim a$(9)*3,cogl$(2)*12,u$*12,address$(2)*40,b$(2)*12,c$*5,d(2),ta(2)
00100   dim desc$(10)*20,beg(10),inc(10),disb(10),end(10),bank$(90)*25,begb(90),endb(90),bankdr(90),bankcr(90),tr$*12,td$*30,tr(7)
00110 ! ______________________________________________________________________
00120   open #20: "Name=CNO.H"&wsid$,internal,input,relative  !:
        read #20,using 'Form POS 145,2*N 1,POS 159,2*C 12,POS 195,C 20',rec=1: mat d,mat cogl$,pedat$ !:
        close #20: 
00130   on fkey 5 goto XIT
00140   data "GENERAL"
00150   data "WATER"
00160   data "WASTEWATER"
00170   data "SINKING FUND"
00180   data "PARK"
00190   data "STORMWATER"
00200   data "CAPITAL IMPROVEMENTS"
00210   data "HUD"
00220   data " "
00230   data " "
00240   read mat desc$
00250   fnopenprn(cp,58,220,process)
00260   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00270   open #2: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&env$('cno')&",Shr",internal,input,relative 
00280   pr newpage
00290   pr f "10,20,C 30,H,N": " FUND BALANCE IN PROCESS"
00300   pr f "12,2,C 18,B,5": " Press F5 to stop"
00310 L310: read #1,using L520: n$,d$,bb,cb,mat ta eof L550
00320   fund=val(n$(1:3)): acct=val(n$(4:9))
00330   if acct>90 then goto L460
00340   if acct<1 then goto L310
00350   bank$(acct)=d$(1:25)
00360   begb(acct)=bb
00370   endb(acct)=cb
00380   nta=ta(1)
00390 L390: if nta=0 then goto L310
00400   read #2,using L410,rec=nta: mat tr,tr$,td$,nta norec L310
00410 L410: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 30,pd 3
00420   if tr(5)>0 then bankdr(acct)=bankdr(acct)+tr(5)
00430   if tr(5)<0 then bankcr(acct)=bankcr(acct)+tr(5)
00440   goto L390
00450 ! ______________________________________________________________________
00460 L460: if fund=0 then goto L310
00470   if fund<>1 then goto L510
00480   if acct<140 then inc(fund)=inc(fund)+cb-bb else !:
          disb(fund)=disb(fund)+cb-bb
00490   goto L530
00500 ! ______________________________________________________________________
00510 L510: if fp(acct*.01)*100<20 then inc(fund)=inc(fund)+cb-bb else !:
          disb(fund)=disb(fund)+cb-bb
00520 L520: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 333,2*pd 3
00530 L530: goto L310
00540 ! ______________________________________________________________________
00550 L550: ! EOF pr INFO
00560   pr newpage
00570   gosub L1020
00580   gosub L750
00590 XIT: fnxit
00600 ! ______________________________________________________________________
00610   pr #255: newpage
00620 L620: pr #255,using L630: date$('mm/dd/yy'),cnam$
00630 L630: form pos 1,c 8,cc 74
00640   pr #255,using L650: time$,"Fund Balance Report"
00650 L650: form pos 1,c 8,pos 31,c 28,skip 1
00660   p1=p1+1
00670   pr #255,using L680: rtrm$(pedat$),"PAGE ",p1
00680 L680: form pos 20,cc 40,pos 70,c 5,n 4,skip 2
00690   pr #255,using L700: "  BEGINNING"," "," ","     ENDING"
00700 L700: form pos 29,4*c 13,skip 1
00710   pr #255,using L720: "   BALANCE","       INCOME","      EXPENSE","    BALANCE"
00720 L720: form pos 29,4*c 13,skip 0
00730   pr #255,using L700: " ___________"," ____________"," ____________"," ___________"
00740   return 
00750 L750: gosub L620
00760   for j=1 to 10
00770     pr #255,using L780: desc$(j),beg(j),-inc(j),disb(j),beg(j)-inc(j)-disb(j)
00780 L780: form pos 1,c 25,pos 28,4*pic(--,---,---.--),skip 1
00790   next j
00800   pr #255,using L700: "------------","------------","------------","------------"
00810   pr #255,using L780: "TOTAL",sum(beg),-sum(inc),sum(disb),sum(beg)-sum(inc)-sum(disb)
00820   pr #255,using L700: "============","============","============","============"
00830   pr #255,using L870: "ACCOUNT BALANCES"
00840   pr #255,using L700: "  BEGINNING"," "," ","     ENDING"
00850   pr #255,using L720: "   BALANCE","       INCOME","      EXPENSE","    BALANCE"
00860   pr #255,using L700: " ___________"," ____________"," ____________"," ___________"
00870 L870: form skip 4,pos 34,c 20,skip 2
00880   for j=1 to 90
00890     if rtrm$(bank$(j))="" then goto L910
00900     pr #255,using L780: bank$(j),begb(j),bankdr(j),-bankcr(j),endb(j)
00910 L910: next j
00920   pr #255,using L700: "------------","------------","------------","------------"
00930   pr #255,using L780: "TOTAL",sum(begb),sum(bankdr),-sum(bankcr),sum(endb)
00940   pr #255,using L700: "============","============","============","============"
00950   fncloseprn
00960   return 
00970 ! ______________________________________________________________________
00980   pr #255: newpage
00990   gosub L620
01000   return 
01010 ! ______________________________________________________________________
01020 L1020: pr newpage
01030   close #101: ioerr L1040
01040 L1040: open #101: "SROW=4,SCOL=18,EROW=17,ECOL=64,BORDER=DR,CAPTION=ENTER FUND BALANCES",display,outin 
01050   pr f "18,25,C 32,R,N": "Press F1 to continue; F5 to stop"
01060   pr f "5,20,c 45,n": "ENTER THE FUND BALANCE AT BEGINNING OF MONTH"
01070   for j=1 to 10
01080     pr f str$(j+6)&",25,C 20,N": desc$(j) !:
          io1$(j)=str$(j+6)&",45,N 12.2,UT,N"
01090   next j
01100 L1100: input fields mat io1$,attr "R": mat beg conv L1100
01110   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01120   if cmdkey>0 then goto L1190 else ce=curfld
01130 L1130: ce=ce+1: if ce>udim(io1$) then ce=1
01140 L1140: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L1130
01150   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1100
01160 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
01170   ce=cnt+1
01180 ERR1: pr f "24,78,C 1": bell : goto L1140
01190 L1190: ! 
01200   pr newpage
01210   return 
01220 ! ______________________________________________________________________
01230 ! <Updateable Region: ERTN>
01240 ERTN: fnerror(program$,err,line,act$,"xit")
01250   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01270   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01280 ERTN_EXEC_ACT: execute act$ : goto ERTN
01290 ! /region
