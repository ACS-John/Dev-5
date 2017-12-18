00010 ! Replace S:\acsUB\ubprtbl1_cerro
00020 ! pr bills (new format)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnopenprn,fncloseprn,fncmdkey
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128,datafile$*256,indexfile$*256
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,cd$(13),ba(12)
00100   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00110   read mat cd$
00120 ! ______________________________________________________________________
00130   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00150 ! 
00160   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00170   at$(1)=cnam$ !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00180   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00190   linelength=62
00200   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
00210   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence #
00220   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00230   def fnc(x)=int(100*(x+sgn(x)*.0001))
00240 ! ______________________________________________________________________
00250 SCREEN1: ! 
00260   a$="" : prtbkno=0
00270   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00280   a$="" : prtbkno=0
00290   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00300   fnlbl(1,1,"Service From:",ll,1)
00310   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00320   fnlbl(2,1,"Service To:",ll,1)
00330   fntxt(2,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00340   fnlbl(3,1,"Penalty Due Date:",ll,1)
00350   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00360   fnlbl(4,1,"Message on Bill:",ll,1)
00370   fntxt(4,pf,30,30) !:
        resp$(respc+=1)=mg$(1)
00380   fntxt(5,pf,30,30) !:
        resp$(respc+=1)=mg$(2)
00390   fntxt(6,pf,30,30) !:
        resp$(respc+=1)=mg$(3)
00400   fnlbl(7,1,"Date of Billing:",ll,1)
00410   fntxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00420   fnlbl(8,1,"Starting Account:",ll,1)
00430   fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&env$('cno') !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&env$('cno') !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00440   fnlbl(9,1,"Route Number:",ll,1)
00450   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00460   fnchk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00470   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00480   if ck=5 then goto ENDSCR
00490   d1 = val(resp$(7)) !:
        d2x= val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6)
00500   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00510   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00520   if resp$(10)="True" then sl1=1 else sl1=0
00530   if trim$(a$)<>"" then read #2,using L540,key=a$: holdz$,route,sequence nokey SCREEN1 ! !:
          st1=1
00540 L540: form pos 1,c 10,pos 1741,n 2,n 7
00550   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00560   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00570   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00580 ! ______________________________________________________________________
00590   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00600   fnopenprn
00610 ! ______________________________________________________________________
00620   on fkey 5 goto F5_CANCEL
00630   gosub BULKSORT
00640 L640: if sl1=1 then goto SCREEN3 ! select accounts
00650 L650: read #7,using L660: r6 eof F5_CANCEL
00660 L660: form pos 1,pd 3
00670   read #1,using L710,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey L650
00680   if trim$(a$)<>"" and holdz$<>z$ then goto L650 else a$=""
00690   goto L710
00700   read #2,using L710: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 eof F5_CANCEL
00710 L710: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6
00720   mat ba=(0): budget=0
00730   if d2=0 and d2x>0 then d2=d2x
00740   if d3=0 and d3x>0 then d3=d3x
00750   read #81,using L760,key=z$: x$,mat ba nokey L780
00760 L760: form pos 1,c 10,pd 4,12*pd 5.2
00770   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00780 L780: if prtbkno=0 then goto L800
00790   if prtbkno><route then goto F5_CANCEL
00800 L800: if f><d1 then goto L640
00810 ! If ST1=0 Then Goto HERE
00820 ! If ST1$=Z$ Then sT1=0 Else Goto 650
00830 HERE: ! 
00840 ! read alternate billing address
00850   read #3,using L870,key=z$: mat ba$ nokey L940
00860   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L940
00870 L870: form pos 11,4*c 30
00880   e1=0 : mat pe$=("")
00890   for j=1 to 4
00900     if trim$(ba$(j))<>"" then !:
            pe$(e1+=1)=ba$(j)
00910   next j
00920   goto L1060
00930 ! ______________________________________________________________________
00940 L940: e1=0 : mat pe$=("")
00950   for j=2 to 4
00960     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00970   next j
00980   goto L1060
00990 ! ______________________________________________________________________
01000 F5_CANCEL: ! 
01010   close #1: ioerr L1020
01020 L1020: close #3: ioerr L1030
01030 L1030: fncloseprn
01040   goto ENDSCR
01050 ! ______________________________________________________________________
01060 L1060: ! 
01070   pb=bal-g(11)
01080 ! ______________print bill routine______________________________________
01090   gosub PRINTBILL
01100 ! _____________end of pr routine______________________________________
01110   bct(2)=bct(2)+1 !:
        ! accumulate totals
01120   goto L640
01130 ! ______________________________________________________________________
01140 SCREEN3: ! 
01150   fntos(sn$="ubprtbl1-2")
01160   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01170   if z$<>"" then !:
          fnlbl(3,1,"Last Account entered was "&z$,44,1)
01180   fncmbact(1,17) ! !:
        resp$(1)=a$
01190   fncmdkey("Add",1,1,0) !:
        fncmdkey("Print",5,0,1)
01200   fnacs(sn$,0,mat resp$,ck)
01210   if ck=5 then goto F5_CANCEL
01220   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto ENDSCR
01230   read #1,using L710,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey SCREEN3
01240   goto HERE
01250 ! ______________________________________________________________________
01260 BULKSORT: ! sort in bulk sort code sequence
01270   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01280 L1280: form pos 1,c 128
01290   write #9,using L1280: "FILE customer.H"&env$('cno')&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01300   if prtbkno>0 then write #9,using L1280: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01310   write #9,using L1280: "MASK 1942,12,C,A,1,10,C,A"
01320   close #9: 
01330   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1340
01340 L1340: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01350   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01360   return 
01370 ! ______________________________________________________________________
01380 ENDSCR: ! pr totals screen
01390   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01400   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01410   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01420   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01430   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01440 XIT: fnxit
01450 ! ______________________________________________________________________
01460 ERTN: fnerror(program$,err,line,act$,"xit")
01470   if uprc$(act$)<>"PAUSE" then goto L1500
01480   execute "list -"&str$(line) !:
        pause  !:
        goto L1500
01490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01500 L1500: execute act$
01510   goto ERTN
01520 ! ______________________________________________________________________
01530 PRINTBILL: ! 
01540   if final=2 then g(8)=g(8)-b(8): g(11)=g(12)+g(8): bal=bal +g(8)
01550   pb=bal-g(11)
01560   pr #255,using L1570: int(d2*.01),int(d3*.01)
01570 L1570: form pos 15,pic(##/##),"-",pic(##/##),skip 3
01580   if g(1)=0 then t$="" else t$="WA"
01590   pr #255,using L1600: t$,d(2),d(1),d(3),g(1),cnvrt$("PIC(##/##/##)",d1)
01600 L1600: form pos 2,c 3,3*nz 9,x 1,nz 10.2,x 3,c 30,skip 1
01610   if g(2)=0 then t$="" else t$="SW"
01620   pr #255,using L1600: t$,0,0,0,g(2),pe$(1)
01630   t$=""
01640   pr #255,using L1600: t$,0,0,0,0,pe$(2)
01650   if g(9)=0 then tr$="" else t$="TX"
01660   pr #255,using L1600: t$,0,0,0,g(9),pe$(3)
01670   if g(8)=0 then t$="" else t$="OC"
01680   pr #255,using L1600: t$,0,0,0,g(8),pe$(4)
01690   pr #255: 
01700   if final>0 then final$="FINAL BILL" else final$=""
01710   pr #255,using L1720: final$
01720 L1720: form pos 11,c 20,skip 1
01730   pn=o
01740   if pb=0 then pr #255: else pr #255,using L1750: "Prior Bill",pb
01750 L1750: form pos 11,c 20,pos 33,n 10.2,skip 1
01760 L1760: form pos 2,pic(zz/zz/zz),x 1,c 20,n 10.2,skip 1
01770   if pn=0 then pr #255: else pr #255,using L1760: 0,"PENALTY",pn
01780   for j=1 to 4 !:
          pr #255: !:
        next j
01790   pr #255: rtrm$(mg$(1))&" "&rtrm$(mg$(2))&" "&rtrm$(mg$(3))
01800   pn=g(5)+g(10)
01810   pr #255,using L1820: d4,pn+bal,bal,pn+bal,bal
01820 L1820: form skip 2,pos 4,pic(##/##/##),n 12.2,x 7,n 12.2,x 1,n 12.2,x 3,n 12.2,skip 3
01830   pr #255,using L1840: z$,z$
01840 L1840: form pos 1,c 10,pos 47,c 10,skip 1
01850   return  ! read next record
01860 ! ______________________________________________________________________
