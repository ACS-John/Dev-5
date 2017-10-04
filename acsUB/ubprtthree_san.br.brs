00010 ! Replace S:\acsUB\ubprtthree_san
00020 ! pr bills (three per page modified for Sangamon
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnopenprn,fncloseprn
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*50,txt$*40,mg$(3)*30,rw(22,13),cap$*128,datafile$*256
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00090   dim newservice$(10)*3,indexfile$*256
00100   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,cd$(13),ba(12)
00110   dim servicename$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1
00120   dim cap$*128
00130 ! ______________________________________________________________________
00140   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00150   read mat cd$
00160 ! ______________________________________________________________________
00170   fncno(cno,cnam$) !:
        fnd1(d1)
00190   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00200   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form POS 1,10*C 20,10*C 2,10*C 1,10*C 1,10*N 2',rec=1: mat servicename$,mat service$,mat tax_code$,mat penalty$,mat subjectto !:
        close #20: 
00210   let newservice$(1)="WTR" !:
        let newservice$(2)="SWR" !:
        let newservice$(3)="LM " !:
        let newservice$(4)="WFC" !:
        let newservice$(5)="REP" !:
        let newservice$(6)="NTE" !:
        let newservice$(7)="IFC" !:
        let newservice$(8)="OTH" !:
        let newservice$(9)="SFC"
00220   at$(1)=cnam$ !:
        let z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00230   let z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00240   linelength=62
00250   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00260   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00270   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00280   def fnc(x)=int(100*(x+sgn(x)*.0001))
00290 ! ______________________________________________________________________
00300 SCREEN1: ! 
00310   a$="" : let prtbkno=0
00320   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00330   a$="" : let prtbkno=0
00340   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00350 ! Let FNLBL(1,1,"Service From:",LL,1)
00360 ! Let FNTXT(1,PF,8,8,1,"1",0,TT$) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
00370 ! Let FNLBL(2,1,"Service To:",LL,1)
00380 ! Let FNTXT(2,PF,8,8,1,"1") !:
        ! Let RESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
00390   fnlbl(3,1,"Penalty Due Date:",ll,1)
00400   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00410   fnlbl(4,1,"Message on Bill:",ll,1)
00420   fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00430   fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00440   fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00450   fnlbl(7,1,"Date of Billing:",ll,1)
00460   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00470   fnlbl(8,1,"Starting Account:",ll,1)
00480   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00490   fnlbl(9,1,"Route Number:",ll,1)
00500   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00510   fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00520   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00530   if ck=5 then goto ENDSCR
00540   let d1 = val(resp$(5)) !:
        let d4 = val(resp$(1)) !:
        let mg$(1) = resp$(2) !:
        let mg$(2) = resp$(3) !:
        let mg$(3) = resp$(4)
00550   if resp$(6)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(6)(1:10)),10)
00560   if resp$(7)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(7))
00570   if resp$(8)="True" then sl1=1 else sl1=0
00580   if trim$(a$)<>"" then !:
          read #1,using 'Form POS 1,C 10,POS 1741,N 2,N 7',key=a$: z$,route,sequence nokey SCREEN1 !:
          st1=1
00590   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": !:
          ! if no beginning account or starting route #, start at beginning of file
00600   if trim$(a$)<>"" then !:
          restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00610   if trim$(a$)="" and prtbkno>0 then !:
          restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": !:
          ! selected a route and no beginning Account
00620 ! ______________________________________________________________________
00630   open #3: "Name="&env$('Q')&"\UBmstr\ubAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\AdrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00640   fnopenprn
00650 ! ______________________________________________________________________
00660   on fkey 5 goto F5_CANCEL
00670   gosub BULKSORT
00680 L680: if sl1=1 then goto SCREEN3 ! select accounts
00690 L690: read #7,using 'Form POS 1,PD 3': r6 eof F5_CANCEL
00700   read #1,using L730,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est,df$ nokey L690
00710   goto L730
00720   read #2,using L730: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est,df$ eof F5_CANCEL
00730 L730: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6,pos 1831,n 9,pos 1712,c 1
00740   if d2=0 and d2x>0 then let d2=d2x ! set date to screen if no date in record
00750   if d3=0 and d3x>0 then let d3=d3x ! set date to screen if no date in record
00760   mat ba=(0): budget=0
00770   if trim$(z$)="" then goto L810
00780   read #81,using L790,key=z$: x$,mat ba nokey L810 ioerr L810
00790 L790: form pos 1,c 10,pd 4,12*pd 5.2
00800   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00810 L810: if prtbkno=0 then goto L830
00820   if prtbkno><route then goto F5_CANCEL
00830 L830: if f><d1 then goto L680
00840   if st1=0 then goto HERE
00850   if st1$=z$ then st1=0 else goto L680
00860 HERE: ! 
00870 ! read alternate billing address
00880   read #3,using L890,key=z$: mat ba$ nokey L960 eof L940
00890 L890: form pos 11,4*c 30
00900   e1=0 : mat pe$=("")
00910   for j=1 to 4
00920     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=ba$(j)
00930   next j
00940 L940: goto L1080
00950 ! ______________________________________________________________________
00960 L960: e1=0 : mat pe$=("")
00970   for j=2 to 4
00980     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=e$(j)
00990   next j
01000   goto L1080
01010 ! ______________________________________________________________________
01020 F5_CANCEL: ! 
01030   close #1: ioerr L1040
01040 L1040: close #3: ioerr L1050
01050 L1050: let fncloseprn
01060   goto ENDSCR
01070 ! ______________________________________________________________________
01080 L1080: ! 
01090   let pb=bal-g(11)
01100 ! ______________print bill routine______________________________________
01110   gosub PRINTBILL
01120 ! _____________end of pr routine______________________________________
01130   bct(2)=bct(2)+1
01140   goto L680
01150 ! ______________________________________________________________________
01160 SCREEN3: ! 
01170   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01180   let txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01190   if trim$(a$)="" then goto L1200 else goto L1210
01200 L1200: if z$<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          fnlbl(3,1,txt$,44,1)
01210 L1210: let fncmbact(1,17) ! !:
        let resp$(1)=a$
01220   fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01230   if ck=5 then goto F5_CANCEL
01240   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto ENDSCR
01250   read #1,using L730,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est,df$ nokey SCREEN3
01260   goto HERE
01270 ! ______________________________________________________________________
01280 ! ______________________________________________________________________
01290 BULKSORT: ! sort in bulk sort code sequence
01300   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01310 L1310: form pos 1,c 128
01320   write #9,using L1310: "FILE customer.H"&str$(cno)&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01330   if prtbkno>0 then write #9,using L1310: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01340   write #9,using L1310: "MASK 1942,12,C,A,1,10,C,A"
01350   close #9: 
01360   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1370
01370 L1370: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01380   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01390   return 
01400 ! ______________________________________________________________________
01410 ENDSCR: ! pr totals screen
01420   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01430   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01440   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01450   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01460   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01470 XIT: let fnxit
01480 ! ______________________________________________________________________
01490 ! <Updateable Region: ERTN>
01500 ERTN: let fnerror(program$,err,line,act$,"xit")
01510   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01520   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01530   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01540 ERTN_EXEC_ACT: execute act$ : goto ERTN
01550 ! /region
01560 ! ______________________________________________________________________
01570 PRINTBILL: ! 
01580   if final=2 then let g(8)=g(8)-b(8): let g(11)=g(12)+g(8): bal=bal +g(8)
01590   let penalty=0
01600   for j=1 to 10
01610     if penalty$(j)="Y" then let penalty=penalty+g(j) !:
            let g(j)=0 ! accumulate all penalties and set charge to zero
01620   next j
01630   let pb=bal-g(11)
01640   pr #255,using L1650: "FROM",int(d3*.01),"TO",int(d2*.01),d1
01650 L1650: form skip 2,pos 1,c 5,pic(##/##),x 1,c 3,pic(##/##),pos 24,pic(##/##/##),skip 4
01660   if pb<>0 then let pb$="   PRIOR BALANCE" else let pb$=""
01670   pr #255,using L1680: pb$,pb
01680 L1680: form pos 3,c 17,nz 10.2,skip 1
01690   if g(1)=0 then let t$="" else let t$=newservice$(1)
01700   pr #255,using L1780: t$,0,d(1),d(3),g(1)
01710 L1710: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2
01720   form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz)
01730   if g(2)=0 then let t$="" else let t$=newservice$(2)
01740   if bal<=0 then !:
          pr #255,using L1780: t$,0,0,0,g(2),"Balance",bal : goto L1790 ! sangamon
01750   if cno=1 and penalty>0 then let penalty=bal*.20 ! company #1 20 %
01760   if cno=2 and penalty>0 then let penalty=bal*.10 ! company #2 @ 10 %
01770   pr #255,using L1780: t$,0,0,0,g(2),"Pay by: "&cnvrt$("pic(zz/zz/zz",d4),bal ! sangamon
01780 L1780: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 8,c 19,nz 10.2
01790 L1790: if g(3)=0 then let t$="" else let t$=newservice$(3)
01800   if bal<=0 then let payafter=bal else let payafter=bal+penalty
01810   pr #255,using L1780: t$,0,0,0,g(3),"Pay after: "& cnvrt$("pic(zz/zz/zz)",d4),payafter ! sangamon
01820   if g(9)=0 then let t$="" else let t$=newservice$(9)
01830   pr #255,using L1710: t$,0,0,0,g(9)
01840   if g(4)=0 then let t$="" else let t$=newservice$(4)
01850   pr #255,using L1710: t$,0,0,0,g(4)
01860   if g(6)=0 then let t$="" else let t$=newservice$(6)
01870   pr #255,using L1880: t$,0,0,0,g(6),z$
01880 L1880: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,pos 38,c 10
01890   form pos 1,c 3,2*nz 6,nz 5,nz 10.2,x 1,c 10
01900   if g(8)=0 and g(7)=0 then let t$="" else let t$=newservice$(8)
01910   pr #255,using L1920: t$,0,0,0,g(8)+g(7),e$(1)(1:18)
01920 L1920: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 8,c 18
01930   if est=1 then est$="BILL ESTIMATED" else est$=""
01940   if final>0 then let final$="FINAL BILL" else let final$=""
01950   if df$="Y" and trim$(final$)="" then let final$="DRAFTED" !:
          ! if drafted and finaled, use final bill as wording
01960   if bal<=0 then let penalty=0
01970   if bal<=0 then pr #255,using L1980: est$,pe$(1)(1:25),"Balance:",bal,pe$(2)(1:25),pe$(3)(1:25),z$,final$,pe$(4)(1:25) : goto L2010 ! sangamon
01980 L1980: form skip 1,pos 7,c 20,pos 38,c 25,skip 1,pos 1,c 19,nz 10.2,pos 38,c 25,skip 1,pos 38,c 25,skip 1,pos 2,c 10,x 5,c 10,pos 38,c 25,skip 1
01990   pr #255,using L2000: est$,pe$(1)(1:25),"Pay by: "&cnvrt$("pic(zz/zz/zz",d4),bal,pe$(2)(1:25),"Pay after: "&cnvrt$("pic(zz/zz/zz",d4),bal+penalty,pe$(3)(1:25),z$,final$,pe$(4)(1:25): goto L2010 ! sangamon
02000 L2000: form skip 1,pos 7,c 20,pos 38,c 25,skip 1,pos 1,c 19,nz 10.2,pos 38,c 25,skip 1,pos 1,c 19,nz 10.2,pos 38,c 25,skip 1,pos 2,c 10,x 5,c 10,pos 38,c 25,skip 1
02010 L2010: bills=bills+1
02020   pr #255,using L2030: mg$(1),mg$(2),mg$(3)
02030 L2030: form pos 2,c 30,skip 1
02040   if int(bills/3)<>bills/3 then pr #255,using L2030: " "," " !:
          ! space extra if 1st or 2nd bill
02050   if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
02060   return 
02070 ! ______________________________________________________________________
