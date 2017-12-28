00010 ! Replace S:\acsUB\ubPrtthree_ash
00020 ! pr bills (new format)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fnformnumb$,fnpause,fnopenprn,fncloseprn,fnget_services
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*60,txt$*40,mg$(3)*30,rw(22,13),indexfile$*256
00080   dim cap$*128,datafile$*256
00090   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00100   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cd$(13),ba(12)
00110   dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1
00120 ! ______________________________________________________________________
00130   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00140   read mat cd$
00150 ! ______________________________________________________________________
00160 ! fnTop - set by another calling program
00170   fnLastBillingDate(d1)
00180 ! 
00190   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00200   fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
00210   at$(1)=env$('cnam') !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00220   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00230   linelength=62
00240   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
00250   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence #
00260   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed 
00270   def fnc(x)=int(100*(x+sgn(x)*.0001))
00280 ! ______________________________________________________________________
00290 SCREEN1: ! 
00300   a$="" : prtbkno=0
00310   fnTos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00320   a$="" : prtbkno=0
00330   fnTos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00340   fnLbl(1,1,"Service From:",ll,1)
00350   fnTxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00360   fnLbl(2,1,"Service To:",ll,1)
00370   fnTxt(2,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00380   fnLbl(3,1,"Penalty Due Date:",ll,1)
00390   fnTxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00400   fnLbl(4,1,"Message on Bill:",ll,1)
00410   fnTxt(4,pf,30,30) !:
        resp$(respc+=1)=mg$(1)
00420   fnTxt(5,pf,30,30) !:
        resp$(respc+=1)=mg$(2)
00430   fnTxt(6,pf,30,30) !:
        resp$(respc+=1)=mg$(3)
00440   fnLbl(7,1,"Date of Billing:",ll,1)
00450   fnTxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00460   fnLbl(8,1,"Starting Account:",ll,1)
00470   fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&env$('cno') !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&env$('cno') !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00480   fnLbl(9,1,"Route Number:",ll,1)
00490   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00500   fnChk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00510   fnCmdSet(3) !:
        fnAcs(sn$,0,mat resp$,ck)
00520   if ck=5 then goto ENDSCR
00530   d1 = val(resp$(7)) !:
        d2x= val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6)
00540   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:10)),10)
00550   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00560   if resp$(10)="True" then sl1=1 else sl1=0
00570   if trim$(a$)<>"" then read #1,using L580,key=a$: z$,route,sequence nokey SCREEN1 !:
          st1=1
00580 L580: form pos 1,c 10,pos 1741,n 2,n 7
00590   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00600   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00610   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00620 ! ______________________________________________________________________
00630   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00640   fnopenprn
00650 ! ______________________________________________________________________
00660   on fkey 5 goto F5_CANCEL
00670   gosub BULKSORT
00680 L680: if sl1=1 then goto SCREEN3 ! select accounts
00690 L690: if prtbkno<>0 then goto L740
00700   read #7,using L710: r6 eof F5_CANCEL
00710 L710: form pos 1,pd 3
00720   read #1,using L750,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey L690
00730   goto L750
00740 L740: read #2,using L750: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est eof F5_CANCEL
00750 L750: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6,pos 1831,n 9
00760   if d2=0 and d2x>0 then d2=d2x ! set date to screen if no date in record
00770   if d3=0 and d3x>0 then d3=d3x ! set date to screen if no date in record
00780   mat ba=(0): budget=0
00790   read #81,using L800,key=z$: x$,mat ba nokey L820
00800 L800: form pos 1,c 10,pd 4,12*pd 5.2
00810   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00820 L820: if prtbkno=0 then goto L840
00830   if prtbkno><route then goto F5_CANCEL
00840 L840: if f><d1 then goto L680
00850   if st1=0 then goto HERE
00860   if st1$=z$ then st1=0 else goto L680
00870 HERE: ! 
00880 ! read alternate billing address
00890   read #3,using L900,key=z$: mat ba$ nokey L970
00900 L900: form pos 11,4*c 30
00910   e1=0 : mat pe$=("")
00920   for j=1 to 4
00930     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00940   next j
00950   goto L1090
00960 ! ______________________________________________________________________
00970 L970: e1=0 : mat pe$=("")
00980   for j=2 to 4
00990     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
01000   next j
01010   goto L1090
01020 ! ______________________________________________________________________
01030 F5_CANCEL: ! 
01040   close #1: ioerr L1050
01050 L1050: close #3: ioerr L1060
01060 L1060: fncloseprn
01070   goto ENDSCR
01080 ! ______________________________________________________________________
01090 L1090: ! 
01100   pb=bal-g(11)
01110 ! ______________print bill routine______________________________________
01120   gosub PRINTBILL
01130 ! _____________end of pr routine______________________________________
01140   bct(2)=bct(2)+1
01150   goto L680
01160 ! ______________________________________________________________________
01170 SCREEN3: ! 
01180   sn$ = "UBPrtBl1-2" !:
        fnTos(sn$)
01190   txt$="Account (blank to stop)" !:
        fnLbl(1,1,txt$,31,1)
01200   if trim$(a$)="" then goto L1210 else goto L1220
01210 L1210: if z$<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnLbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnLbl(3,1,txt$,44,1)
01220 L1220: fncmbact(1,17) ! !:
        resp$(1)=a$
01230   fnCmdSet(3): fnAcs(sn$,0,mat resp$,ck)
01240   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" or ck=5 then goto F5_CANCEL
01250   read #1,using L750,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey SCREEN3
01260   goto HERE
01270 ! ______________________________________________________________________
01280 BULKSORT: ! sort in bulk sort code sequence
01290   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01300 L1300: form pos 1,c 128
01310   write #9,using L1300: "FILE customer.H"&env$('cno')&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01320   if prtbkno>0 then write #9,using L1300: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01330   write #9,using L1300: "MASK 1942,12,C,A,1,10,C,A"
01340   close #9: 
01350   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1360
01360 L1360: execute "Sort "&env$('Temp')&"\Control."&session$
01370   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01380   return 
01390 ! ______________________________________________________________________
01400 ENDSCR: ! pr totals screen
01410   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01420   fnTos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01430   fnLbl(1,1,"Total Bills Printed:",mylen,1)
01440   fnTxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01450   fnCmdSet(52) !:
        fnAcs(sn$,0,mat resp$,ck)
01460 XIT: fnxit
01470 ! ______________________________________________________________________
01480 PRINTBILL: ! 
01490   if final=2 then !:
          g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
01500   penalty=0
01510   for j=1 to 10
01520     if penalty$(j)="Y" then penalty+=g(j) !:
            g(j)=0 ! accumulate all penalties and set charge to zero
01530   next j
01540   pb=bal-g(11)
01550   pr #255: "" !:
        pr #255,using L1560: "FROM",int(d2x*.01),"TO",int(d3x*.01),d1
01560 L1560: form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##),skip 4
01570   if pb<>0 then pb$="   PRIOR BALANCE" else pb$=""
01580   pr #255: ""
01590 L1590: form pos 3,c 17,nz 10.2,pos 38,c 10,skip 1
01600   if g(1)=0 then t$="" else t$=service$(1)
01610   pr #255,using L1620: t$,0,d(1),d(3),g(1)
01620 L1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2,skip 1
01630 L1630: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz),skip 1
01640   if g(2)=0 then t$="" else t$=service$(2)
01650   if bal<=0 then pr #255,using L1620: t$,0,0,0,g(2),0,bal : goto L1670
01660   pr #255,using L1620: t$,0,0,0,g(2),bal+penalty,bal
01670 L1670: if g(4)=0 then t$="" else t$=service$(4)
01680   pr #255,using L1630: t$,0,0,0,g(4),d4
01690   if g(5)=0 then t$="" else t$=service$(5)
01700   pr #255,using L1620: t$,0,0,0,g(5)
01710   if g(6)=0 then t$="" else t$=service$(6)
01720   pr #255,using L1620: t$,0,0,0,g(6)
01730   if g(8)=0 then t$="" else t$=service$(8)
01740   pr #255,using L1620: t$,0,0,0,g(8)
01750   if g(9)=0 then t$="" else t$=service$(9)
01760   pr #255,using L1620: t$,0,0,0,g(9)
01770   pr #255,using L1590: pb$,pb,z$
01780   form pos 1,c 3,2*nz 6,nz 5,nz 10.2,x 1,c 10,skip 1
01790   if est=1 then est$="BILL ESTIMATED" else est$=""
01800   if c4>0 then final$="FINAL BILL" else final$=""
01810   if df$="Y" then final$="DRAFTED"
01820   if bal<=0 then penalty=0
01840   pr #255: ""
01850   pr #255,using 'Form POS 7,C 20,POS 38,C 25': est$,pe$(1)(1:25)
01860   pr #255,using 'Form POS 1,CR 7,X 1,PIC(ZZ/ZZ/ZZ),NZ 13.2,POS 38,C 25': 'DUE BY:',d4,bal,pe$(2)(1:25)
01870   pr #255,using 'Form POS 13,C 18,POS 38,C 25': e$(1)(1:18),pe$(3)(1:25)
01880   pr #255,using 'Form POS 2,C 10,X 5,C 10,POS 38,C 25': z$,final$,pe$(4)(1:25)
01890   bills+=1
01900   pr #255,using L1930: mg$(1)
01910   pr #255,using L1930: mg$(2)
01920   pr #255,using L1930: mg$(3)
01930 L1930: form pos 2,c 30,skip 1
01940   if int(bills/3)<>bills/3 then pr #255,using L1930: " "," " !:
          ! space extra if 1st or 2nd bill
01950   if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
01960   return 
01970 ! ______________________________________________________________________
01980 ! <Updateable Region: ERTN>
01990 ERTN: fnerror(program$,err,line,act$,"xit")
02000   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02010   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02020   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02030 ERTN_EXEC_ACT: execute act$ : goto ERTN
02040 ! /region
02050 ! ______________________________________________________________________
