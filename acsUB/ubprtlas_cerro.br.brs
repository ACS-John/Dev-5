00010 ! Replace S:\acsUB\ubPrtLas_Cerro
00020 ! pr bills (new format)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fnformnumb$,fnpause,fnopenprn,fncloseprn,fnget_services
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*80,txt$*40,mg$(3)*30,rw(22,13),indexfile$*256
00080   dim cap$*128,datafile$*256
00090   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00100   dim gb(10),pe$(4)*30,ba$(4)*30,cnam$*40,ba(12)
00110   dim servicename$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1
00120 ! ______________________________________________________________________
00160 ! fnTop - set by another calling program
00170   fncno(cno,cnam$) !:
        fnd1(d1)
00180 ! 
00200   fnget_services(mat servicename$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
00230   linelength=62
00240   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00250   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00260   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00280 ! ______________________________________________________________________
00290 SCREEN1: ! 
00300   a$="" : let prtbkno=0
00310   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00320   a$="" : let prtbkno=0
00330   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00340   fnlbl(1,1,"Service From:",ll,1)
00350   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00360   fnlbl(2,1,"Service To:",ll,1)
00370   fntxt(2,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00380   fnlbl(3,1,"Penalty Due Date:",ll,1)
00390   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00400   fnlbl(4,1,"Message on Bill:",ll,1)
00410   fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00420   fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00430   fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00440   fnlbl(7,1,"Date of Billing:",ll,1)
00450   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00460   fnlbl(8,1,"Starting Account:",ll,1)
00470   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00480   fnlbl(9,1,"Route Number:",ll,1)
00490   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00500   fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00510   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00520   if ck=5 then goto ENDSCR
00530   let d1 = val(resp$(7)) !:
        let d2x= val(resp$(1)) !:
        let d3x= val(resp$(2)) !:
        let d4 = val(resp$(3)) !:
        let mg$(1) = resp$(4) !:
        let mg$(2) = resp$(5) !:
        let mg$(3) = resp$(6)
00540   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:10)),10)
00550   if resp$(9)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(9))
00560   if resp$(10)="True" then sl1=1 else sl1=0
00570   if trim$(a$)<>"" then read #1,using L580,key=a$: z$,route,sequence nokey SCREEN1 !:
          st1=1
00580 L580: form pos 1,c 10,pos 1741,n 2,n 7
00590   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00600   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00610   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00620 ! ______________________________________________________________________
00630   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00640   fnopenprn
00650 ! ______________________________________________________________________
00660   on fkey 5 goto RELEASE_PRINT
00670   gosub BULKSORT
00680 L680: if sl1=1 then goto SCREEN3 ! select accounts
00690 L690: read #7,using L700: r6 eof RELEASE_PRINT
00700 L700: form pos 1,pd 3
00710   read #1,using L740,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey L690
00720   goto L740
00730   read #2,using L740: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est eof RELEASE_PRINT
00740 L740: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6,pos 1831,n 9
00750   if d2=0 and d2x>0 then let d2=d2x ! set date to screen if no date in record
00760   if d3=0 and d3x>0 then let d3=d3x ! set date to screen if no date in record
00770   mat ba=(0): budget=0
00780   read #81,using L790,key=z$: x$,mat ba nokey L810
00790 L790: form pos 1,c 10,pd 4,12*pd 5.2
00800   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00810 L810: if prtbkno=0 then goto L830
00820   if prtbkno><route then goto RELEASE_PRINT
00830 L830: if f><d1 then goto L680
00840   if st1=0 then goto HERE
00850   if st1$=z$ then st1=0 else goto L680
00860 HERE: ! 
00870 ! read alternate billing address
00880   read #3,using L890,key=z$: mat ba$ nokey L960
00890 L890: form pos 11,4*c 30
00900   e1=0 : mat pe$=("")
00910   for j=1 to 4
00920     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=ba$(j)
00930   next j
00940   goto L1080
00950 ! ______________________________________________________________________
00960 L960: e1=0 : mat pe$=("")
00970   for j=2 to 4
00980     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=e$(j)
00990   next j
01000   goto L1080
01010 ! ______________________________________________________________________
01020 RELEASE_PRINT: ! 
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
01230   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" or ck=5 then goto RELEASE_PRINT
01240   read #1,using L740,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey SCREEN3
01250   goto HERE
01260 ! ______________________________________________________________________
01270 BULKSORT: ! sort in bulk sort code sequence
01280   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01290 L1290: form pos 1,c 128
01300   write #9,using L1290: "FILE customer.H"&str$(cno)&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01310   if prtbkno>0 then write #9,using L1290: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01320   write #9,using L1290: "MASK 1942,12,C,A,1,10,C,A"
01330   close #9: 
01340   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1350
01350 L1350: execute "Sort "&env$('Temp')&"\Control."&session$
01360   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01370   return 
01380 ! ______________________________________________________________________
01390 ENDSCR: ! pr totals screen
01400   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01410   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01420   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01430   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01440   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01450 XIT: !
01452 let fnxit
01470 PRINTBILL: ! r:
01480   if final=2 then !:
          let g(8)-=b(8): let g(11)=g(12)+g(8): bal+=g(8)
01490   let penalty=0
01500   for j=1 to 10
01510     if penalty$(j)="Y" then let penalty+=g(j) !:
            let g(j)=0 ! accumulate all penalties and set charge to zero
01520   next j
01530   let pb=bal-g(11)
01540   pr #255: "" !:
        pr #255: "" !:
        pr #255,using L1550: "FROM",int(d2x*.01),"TO",int(d3x*.01),d1
01550 L1550: form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##),skip 4
01560   if pb<>0 then let pb$="   PRIOR BALANCE" else let pb$=""
01570   pr #255: ""
01580 L1580: form pos 3,c 17,nz 10.2,pos 38,c 10,skip 1
01590   if g(1)=0 then let t$="" else let t$=service$(1)
01600   pr #255,using L1610: t$,0,d(1),d(3),g(1)
01610 L1610: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2,skip 1
01620 L1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz),skip 1
01630   if g(2)=0 then let t$="" else let t$=service$(2)
01640   if bal<=0 then pr #255,using L1610: t$,0,0,0,g(2),0,bal : goto L1660
01650   pr #255,using L1610: t$,0,0,0,g(2),bal+penalty,bal
01660 L1660: if g(3)=0 then let t$="" else let t$=service$(3)
01670   pr #255,using L1620: t$,0,0,0,g(3),d4
01680   if g(4)=0 then let t$="" else let t$=service$(4)
01690   pr #255,using L1610: t$,0,0,0,g(4)
01700   if g(5)=0 then let t$="" else let t$=service$(5)
01710   pr #255,using L1610: t$,0,0,0,g(5)
01720   if g(6)=0 then let t$="" else let t$=service$(6)
01730   pr #255,using L1580: pb$,pb,z$
01740   form pos 1,c 3,2*nz 6,nz 5,nz 10.2,x 1,c 10,skip 1
01750   if g(8)=0 then let t$="" else let t$=service$(8)
01760   pr #255,using L1610: t$,0,0,0,g(8)
01770   if est=1 then est$="BILL ESTIMATED" else est$=""
01780   if c4>0 then let final$="FINAL BILL" else let final$=""
01790   if df$="Y" then let final$="DRAFTED"
01800   if bal<=0 then let penalty=0
01810   if env$('client')="Cerro Gordo" and bal<0 then let g(5)=0
01820   pr #255: ""
01830   pr #255,using 'Form POS 7,C 20,POS 38,C 25': est$,pe$(1)(1:25)
01840   pr #255,using 'Form POS 1,CR 7,X 1,PIC(ZZ/ZZ/ZZ),NZ 13.2,POS 38,C 25': 'DUE BY:',d4,bal,pe$(2)(1:25)
01850   pr #255,using 'Form POS 13,C 18,POS 38,C 25': e$(1)(1:18),pe$(3)(1:25)
01860   pr #255,using 'Form POS 2,C 10,X 5,C 10,POS 38,C 25': z$,final$,pe$(4)(1:25)
01870   bills+=1
01880   pr #255,using L1910: mg$(1)
01890   pr #255,using L1910: mg$(2)
01900   pr #255,using L1910: mg$(3)
01910 L1910: form pos 2,c 30,skip 1
01920   if int(bills/3)<>bills/3 then pr #255,using L1910: " "," " !:
          ! space extra if 1st or 2nd bill
01930   if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
01940   return ! /r
01960 ! <Updateable Region: ERTN>
01970 ERTN: let fnerror(program$,err,line,act$,"xit")
01980   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01990   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02000   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02010 ERTN_EXEC_ACT: execute act$ : goto ERTN
02020 ! /region
