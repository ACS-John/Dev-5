00010 ! Replace acsUB\ubprtbl1_Granby
00020 ! print bills (new format) 3 1/2" x 7.5" - for Granby
00030 ! ______________________________________________________________________
00040   library 'Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnpause,fnopenprn,fncloseprn,fncmdkey
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,mg$(3)*50,cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,cd$(13),ba(12)
00100 ! ______________________________________________________________________
00110   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00120   read mat cd$
00130 ! ______________________________________________________________________
00140   let fntop("acsUB\Bill35X75",cap$="Bills-Dot Matrix 3.5x7.5") !:
        ! don't use  PROGRAM$(4:POS(PROGRAM$,'.',-1)-1)   !:
        ! - it needs to think it is the program that chained to it
00150   let fncno(cno,cnam$) !:
        let fnd1(d1)
00160 ! 
00170   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00180   let at$(1)=cnam$ !:
        let z=21 !:
        let at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        let at$(1)=rpt$(" ",int(y/2))&at$(1)
00190   let z=26 !:
        for j=2 to udim(at$) !:
          let at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          let at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00200   let linelength=62
00210   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.H"&str$(cno)&",Shr",internal,input,keyed  ! open in account order
00220   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.H"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence
00230   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.H"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.H"&str$(cno)&",Shr",internal,outin,keyed 
00240 ! 
00250 SCREEN1: ! 
00260   let a$="" : let prtbkno=0
00270   let fntos(sn$="PrtBl35x75") !:
        let pf=26 : let ll=24 !:
        let respc=0
00280   let fnlbl(1,1,"Service From:",ll,1)
00290   let fntxt(1,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00300   let fnlbl(2,1,"Service To:",ll,1)
00310   let fntxt(2,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00320   let fnlbl(3,1,"Penalty Due Date:",ll,1)
00330   let fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00340   let fnlbl(4,1,"Message on Bill:",ll,1)
00350   let fntxt(4,pf,50) !:
        let resp$(respc+=1)=mg$(1)
00360   let fntxt(5,pf,50) !:
        let resp$(respc+=1)=mg$(2)
00370   let fntxt(6,pf,50) !:
        let resp$(respc+=1)=mg$(3)
00380   let fnlbl(7,1,"Date of Billing:",ll,1)
00390   let fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00400   let fnlbl(8,1,"Starting Route/Sequence:",ll,1)
00410   let fncombof("ubm-act-nam",8,pf,40,env$('Q')&"\UBmstr\Customer.h"&str$(cno),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&str$(cno),2) !:
        let resp$(respc+=1)="[All]"
00420   let fnlbl(9,1,"Route Number:",ll,1)
00430   let fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00440   let fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00450   let fncmdset(3)
00460   let fnacs(sn$,0,mat resp$,ck)
00470   if ck=5 then goto XIT
00480   let d1 = val(resp$(7)) !:
        let d2x=val(resp$(1)) !:
        let d3x= val(resp$(2)) !:
        let d4 = val(resp$(3)) !:
        let mg$(1) = resp$(4) !:
        let mg$(2) = resp$(5) !:
        let mg$(3) = resp$(6)
00490   if resp$(8)="[All]" then !:
          let a$="" else !:
          let a$ = lpad$(trim$(resp$(8)(1:9)),9)
00500   if resp$(9)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(9))
00510   if resp$(10)="True" then let sl1=1 else let sl1=0
00520   if trim$(a$)<>"" then !:
          read #2,using L530,key=a$: holdz$,route,sequence nokey SCREEN1 !:
          let st1=1
00530 L530: form pos 1,c 10,pos 1741,n 2,n 7
00540   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": !:
          ! if no beginning account or starting route #, start at beginning of file
00550   if trim$(a$)<>"" then !:
          restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00560   if trim$(a$)="" and prtbkno>0 then !:
          restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": !:
          ! selected a route and no beginning account
00570 ! 
00580   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.H"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.H"&str$(cno)&",Shr",internal,input,keyed 
00590   let fnopenprn
00600 ! 
00610   gosub BULKSORT
00620 L620: if sl1=1 then goto SCREEN3 ! select accounts
00630 L630: read #7,using L640: r6 eof F5_CANCEL
00640 L640: form pos 1,pd 3
00650   read #1,using L690,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey L630
00660   if trim$(a$)<>"" and holdz$<>z$ then goto L630 else let a$=""
00670   goto L690
00680   read #2,using L690: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 eof F5_CANCEL
00690 L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6
00700   mat ba=(0): let budget=0
00710   if d2=0 and d2x>0 then let d2=d2x
00720   if d3=0 and d3x>0 then let d3=d3x
00730   read #81,using 'Form POS 1,C 10,PD 4,12*PD 5.2',key=z$: x$,mat ba nokey L750
00740   for j=2 to 12: let budget=budget+ba(j): next j ! get total budget amount
00750 L750: if prtbkno=0 then goto L770
00760   if prtbkno><route then goto F5_CANCEL
00770 L770: if f><d1 then goto L620
00780 ! If ST1=0 Then Goto HERE
00790 ! If ST1$=Z$ Then Let ST1=0 Else Goto 650
00800 HERE: ! 
00810 ! read alternate billing address
00820   read #3,using L840,key=z$: mat ba$ nokey L910
00830   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L910
00840 L840: form pos 11,4*c 30
00850   let e1=0 : mat pe$=("")
00860   for j=1 to 4
00870     if trim$(ba$(j))<>"" then !:
            let pe$(e1+=1)=ba$(j)
00880   next j
00890   goto L1030
00900 ! ______________________________________________________________________
00910 L910: let e1=0 : mat pe$=("")
00920   for j=2 to 4
00930     if rtrm$(e$(j))<>"" then !:
            let e1=e1+1 : let pe$(e1)=e$(j)
00940   next j
00950   goto L1030
00960 ! ______________________________________________________________________
00970 F5_CANCEL: ! 
00980   close #1: ioerr L990
00990 L990: close #3: ioerr L1000
01000 L1000: let fncloseprn
01010   goto ENDSCR
01020 ! ______________________________________________________________________
01030 L1030: ! 
01040   let pb=bal-g(11)
01050 ! ______________print bill routine______________________________________
01060   gosub PRINTBILL
01070 ! _____________end of print routine______________________________________
01080   let bct(2)=bct(2)+1 !:
        ! accumulate totals
01090   goto L620
01100 ! ______________________________________________________________________
01110 SCREEN3: ! 
01120   let fntos(sn$="Pennington-Bill")
01130   let fnlbl(1,1,"Account (blank to stop)",31,1)
01140   if z$<>"" then !:
          let fnlbl(3,1,"Last Account entered was "&z$,44,1)
01150   let fncmbact(1,17) ! !:
        let resp$(1)=a$
01160   let fncmdkey("Add",1,1,0) !:
        let fncmdkey("Print",5,0,1)
01170   let fnacs(sn$,0,mat resp$,ck)
01180   if ck=5 then goto F5_CANCEL
01190   let a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto ENDSCR
01200   read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey SCREEN3
01210   goto HERE
01220 ! ______________________________________________________________________
01230 BULKSORT: ! sort in bulk sort code sequence
01240   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01250 L1250: form pos 1,c 128
01260   write #9,using L1250: "FILE customer.H"&str$(cno)&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01270   if prtbkno>0 then !:
          write #9,using L1250: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01280   write #9,using L1250: "MASK 1942,12,C,A,1,10,C,A"
01290   close #9: 
01300   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1310
01310 L1310: execute "Sort "&env$('Temp')&"\Control."&session$
01320   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01330   return 
01340 ! ______________________________________________________________________
01350 ENDSCR: ! print totals screen
01360   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01370   let fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01380   let fnlbl(1,1,"Total Bills Printed:",mylen,1)
01390   let fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01400   let fncmdset(52) !:
        let fnacs(sn$,0,mat resp$,ck)
01410   goto XIT
01420 ! ______________________________________________________________________
01430 XIT: let fnxit
01440 ! ______________________________________________________________________
01450 ! <Updateable Region: ERTN>
01460 ERTN: let fnerror(program$,err,line,act$,"xit")
01470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01490   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01500 ERTN_EXEC_ACT: execute act$ : goto ERTN
01510 ! /region
01520 ! ______________________________________________________________________
01530 PRINTBILL: ! 
01540   let pb=bal-g(11)
01550   print #255: ""
01560   print #255,using 'Form POS 43,C 30': e$(1) !:
        print #255: "" !:
        print #255: "" !:
        print #255: "" !:
        print #255: ""
01570   print #255,using 'Form POS 3,c 10,pos 18,n 6,pos 25,n 6,pos 38,n 9,n 9,n 9, n 9.2': 'Water',d3,d2,d(2),d(1),d(3),g(1)
01580   if g(2)=0 then !:
          print #255,using 'Form POS 3,C 10': "" else !:
          print #255,using 'Form POS 3,C 10,pos 65,n 9.2': "SEWER",g(2)
01590   if g(4)=0 then !:
          print #255,using 'Form POS 3,C 10': "" else !:
          print #255,using 'Form POS 3,c 10,pos 18,n 6,pos 25,n 6,pos 38,n 9,n 9,n 9, n 9.2': 'Gas',d3,d2,d(10),d(9),d(11),g(4)
01600   if g(5)=0 then !:
          print #255,using 'Form POS 3,C 10': "" else !:
          print #255,using 'Form POS 3,C 10,POS 65,N 9.2': "SANIT",g(5)
01610   if g(8)=0 then !:
          print #255,using 'Form POS 3,C 10': "" else !:
          print #255,using 'Form POS 3,C 10,POS 65,N 9.2': "OTHER",g(8),pe$(2)
01620   if g(9)=0 then !:
          print #255,using 'Form POS 3,C 10': "" else !:
          print #255,using 'Form POS 3,C 10,POS 65,N 9.2': "Sales Tax",g(9)
01630   if pb<0 then let balance$="Credit" else let balance$="Overdue"
01640   if pb=0 then !:
          print #255,using 'Form POS 3,c 10': "" else !:
          print #255,using 'Form POS 3,C 10,POS 65,N 9.2': balance$,pb
01650   print #255,using "form pos 12,c 30": pe$(1)
01660   print #255,using "form pos 12,c 30,pos 51,c 10,pos 64,pic(zz/zz/zz)": pe$(2),z$,d4
01670   print #255,using "form pos 12,c 30": pe$(3)
01680   print #255,using "form pos 12,c 30,pos 51,n 10.2,pos 62,n 10.2": pe$(4),bal+g(10),bal
01690   if trim$(mg$(1))="" then let mg$(1)="Emergency vehicles need house numbers-please help"
01700   if trim$(mg$(2))="" then let mg$(2)="If you smell gas call 472-6556 or 472-3535"
01710   print #255,using "Form pos 1,c 60,skip 1,pos 1,c 60,skip 1,pos 1,c 60": mg$(1),mg$(2)
01720   print #255: newpage
01730   return  ! read next record
01740 ! ______________________________________________________________________
