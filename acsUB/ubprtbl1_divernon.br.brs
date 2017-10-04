00010 ! Replace S:\acsUB\ubprtbl1_divernon
00020 ! pr bills (new format)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnopenprn,fncloseprn
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*44,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,cd$(13),ba(12),extra1$*30
00100   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00110   read mat cd$
00120 ! ______________________________________________________________________
00130   fncno(cno,cnam$) !:
        fnd1(d1)
00150 ! 
00160   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00170   at$(1)=cnam$ !:
        let z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00180   let z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00190   linelength=62
00200   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00210   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00220   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00230   def fnc(x)=int(100*(x+sgn(x)*.0001))
00240 ! ______________________________________________________________________
00250 SCREEN1: ! 
00260   a$="" : let prtbkno=0
00270   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00280   fnlbl(1,1,"Service From:",ll,1)
00290   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00300   fnlbl(2,1,"Meter Reading Date:",ll,1)
00310   fntxt(2,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00320   fnlbl(3,1,"Penalty Due Date:",ll,1)
00330   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00340   fnlbl(4,1,"Message on Bill:",ll,1)
00350   fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00360   fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00370   fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00380   fnlbl(7,1,"Date of Billing:",ll,1)
00390   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00400   fnlbl(8,1,"Starting Account:",ll,1)
00410   let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof("ubm-act-nam",8,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),kp,kl,dp,dl,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2) !:
        let resp$(respc+=1)="[All]"
00420   fnlbl(9,1,"Route Number:",ll,1)
00430   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00440   fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00450   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00460   if ck=5 then goto ENDSCR
00470   let d1 = val(resp$(7)) !:
        let d2 = val(resp$(1)) !:
        let d3 = val(resp$(2)) !:
        let d4 = val(resp$(3)) !:
        let mg$(1) = resp$(4) !:
        let mg$(2) = resp$(5) !:
        let mg$(3) = resp$(6)
00480   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:10)),10)
00490   if resp$(9)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(9))
00500   if resp$(10)="True" then sl1=1 else sl1=0
00510   if trim$(a$)<>"" then read #1,using L520,key=a$: z$,route,sequence nokey SCREEN1 !:
          st1=1
00520 L520: form pos 1,c 10,pos 1741,n 2,n 7
00530   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00540   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00550   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00560 ! ______________________________________________________________________
00570   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00580   fnopenprn
00590 ! ______________________________________________________________________
00600   on fkey 5 goto F5_CANCEL
00610   gosub BULKSORT
00620 L620: if sl1=1 then goto SCREEN3 ! select accounts
00630 L630: read #7,using L640: r6 eof F5_CANCEL
00640 L640: form pos 1,pd 3
00650   read #1,using L680,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,bulk$,extra1$ nokey L630
00660   goto L680
00670   read #2,using L680: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,bulk$,extra1$ eof F5_CANCEL
00680 L680: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1942,c 12,pos 1864,c 30
00690   mat ba=(0): budget=0
00700   read #81,using L710,key=z$: x$,mat ba nokey L730
00710 L710: form pos 1,c 10,pd 4,12*pd 5.2
00720   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00730 L730: if prtbkno=0 then goto L750
00740   if prtbkno><route then goto F5_CANCEL
00750 L750: if f><d1 then goto L620
00760   if st1=0 then goto HERE
00770   if st1$=z$ then st1=0 else goto L620
00780 HERE: ! 
00790 ! read alternate billing address
00800   read #3,using L820,key=z$: mat ba$ nokey L890
00810   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" then goto L890 ! IF NO INFO IN ALTERNATE FILE SKIP
00820 L820: form pos 11,4*c 30
00830   e1=0 : mat pe$=("")
00840   for j=1 to 4
00850     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=ba$(j)
00860   next j
00870   goto L1020
00880 ! ______________________________________________________________________
00890 L890: e1=0 : mat pe$=("")
00900   for j=2 to 4
00910     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=e$(j)
00920   next j
00930   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$
00940   goto L1020
00950 ! ______________________________________________________________________
00960 F5_CANCEL: ! 
00970   close #1: ioerr L980
00980 L980: close #3: ioerr L990
00990 L990: let fncloseprn
01000   goto ENDSCR
01010 ! ______________________________________________________________________
01020 L1020: ! 
01030   let pb=bal-g(11)
01040 ! ______________print bill routine______________________________________
01050   gosub PRINTBILL
01060 ! _____________end of pr routine______________________________________
01070   bct(2)=bct(2)+1 !:
        ! accumulate totals
01080   goto L620
01090 ! ______________________________________________________________________
01100 SCREEN3: ! 
01110   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01120   let txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01130 ! If TRIM$(A$)="" Then Goto 1080 Else Goto 1120
01140   if z$<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          fnlbl(3,1,txt$,44,1)
01150   fncmbact(1,17) ! !:
        let resp$(1)=a$
01160   fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01170   if ck=5 then goto F5_CANCEL
01180   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto ENDSCR
01190   read #1,using L680,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,bulk$,extra1$ nokey SCREEN3
01200   goto HERE
01210 ! ______________________________________________________________________
01220 ! ______________________________________________________________________
01230 BULKSORT: ! sort in bulk sort code sequence
01240   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01250 L1250: form pos 1,c 128
01260   write #9,using L1250: "FILE customer.H"&str$(cno)&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01270   if prtbkno>0 then write #9,using L1250: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01280   write #9,using L1250: "MASK 1942,12,C,A,1,10,C,A"
01290   close #9: 
01300   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1310
01310 L1310: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01320   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01330   return 
01340 ! ______________________________________________________________________
01350 ENDSCR: ! pr totals screen
01360   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01370   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01380   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01390   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01400   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01410 XIT: let fnxit
01420 ! ______________________________________________________________________
01430 ! <Updateable Region: ERTN>
01440 ERTN: let fnerror(program$,err,line,act$,"xit")
01450   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01480 ERTN_EXEC_ACT: execute act$ : goto ERTN
01490 ! /region
01500 ! ______________________________________________________________________
01510 PRINTBILL: ! 
01520   if a(1)<1 or a(1)>3 then a1=1 else a1=a(1)
01530   a2=4 ! SEWER
01540   if a(4)<1 or a(4)>2 then a4=5 else a4=a(4)+4 ! GAS
01550   a3=7 ! ADMINISTRATION FEE
01560   a6=8 ! TAX
01570   let pb=bal-g(11)
01580   let pn=g(5)+g(6)+g(7)
01590 ! pr #255,Using 1750: HEX$("2B0205000A1018")
01600   form c 9,skip 0
01610   pr #255,using L1620: d1
01620 L1620: form pos 22,pic(zz/zz/zz),skip 4
01630   if g(1)>0 then pr #255,using L1640: cd$(a1),d(1),d(3),g(1) else pr #255: 
01640 L1640: form pos 1,c 3,2*g 8,nz 12.2,pos 35,c 30,skip 1
01650   if g(2)>0 then pr #255,using L1640: cd$(a2),"","",g(2) else pr #255: 
01660   if g(4)>0 then pr #255,using L1670: cd$(a4),d(9),d(11),g(4),pb,bal else pr #255,using L1680: pb,bal
01670 L1670: form pos 1,c 3,2*g 8,n 12.2,pos 33,n 12.2,n 13.2,skip 1
01680 L1680: form pos 31,n 10.2,n 13.2,skip 1
01690   if g(9)>0 then code$="TX" else code$=""
01700   pr #255,using L1640: code$,"","",g(9)
01710   if g(3)>0 then code$="AF" else code$=""
01720   pr #255,using L1640: code$,"","",g(3)
01730   code$="": if g(8)>0 then code$="OC" else goto L1760
01740   pr #255,using L1750: code$,"","",g(8)+g(10),z$,bulk$: goto L1780 ! IF OTHER CHARGES AND IEPA BOTH JUST ADD TOGETHER AND CALL OTHER
01750 L1750: form pos 1,c 3,2*g 8,nz 12.2,pos 35,c 10,x 2,c 12,skip 1
01760 L1760: if g(10)>0 then code$="IEP" else code$=""
01770   pr #255,using L1750: code$,"","",g(10),z$,bulk$
01780 L1780: if pn > 0 then code$="P  " else code$=""
01790   pr #255,using L1640: code$,"","",pn
01800   if pb > 0 then code$="ARR" else code$=""
01810   pr #255,using L1830: code$,"","",pb,pe$(1)
01820   if budget>0 then pr #255,using L1830: "TT","","",budget,pe$(2) else pr #255,using L1830: "TT","","",bal,pe$(2)
01830 L1830: form pos 1,c 3,2*g 8,nz 12.2,pos 35,c 30,skip 1
01840   if escrow>0 then pr #255,using L1640: "CR","","",escrow,pe$(3) else pr #255,using L1900: pe$(3)
01850   pr #255,using L1900: pe$(4)
01860   pr #255: 
01870   pr #255,using L1880: d4,bal
01880 L1880: form pos 5,pic(zz/zz/zz),x 7,n 12.2,skip 2
01890   pr #255,using L1900: mg$(1)
01900 L1900: form pos 35,c 30
01910   pr #255,using L1900: mg$(2)
01920   pr #255,using L1930: z$,mg$(3)
01930 L1930: form pos 3,c 10,pos 35,c 30
01940   pr #255,using L1950: "METER READING DATE",d3
01950 L1950: form pos 1,c 19,pic(zz/zz/zz)
01960   pr #255: newpage
01970   return  ! read next record
01980 ! ______________________________________________________________________
