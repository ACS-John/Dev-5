00010 ! Replace acsUB\ubprtbl1_Granby
00020 ! pr bills (new format) 3 1/2" x 7.5" - for Granby
00030 ! ______________________________________________________________________
00040   library 'Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnpause,fnopenprn,fncloseprn,fnCmdKey
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,mg$(3)*50,cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,cd$(13),ba(12)
00100 ! ______________________________________________________________________
00110   data RW,CW,BW,SW,RG,CG,AF,TX,ST,P,ARR,OC,TT
00120   read mat cd$
00130 ! ______________________________________________________________________
00140   fntop("acsUB\Bill35X75",cap$="Bills-Dot Matrix 3.5x7.5") !:
        ! don't use  PROGRAM$(4:POS(PROGRAM$,'.',-1)-1)   !:
        ! - it needs to think it is the program that chained to it
00150   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00160 ! 
00170   open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00180   at$(1)=cnam$ !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00190   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00200   linelength=62
00210   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.H[cno],Shr",internal,input,keyed  ! open in account order
00220   open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.H[cno],Shr",internal,input,keyed  ! open in route-sequence
00230   open #81: "Name=[Q]\UBmstr\BudMstr.H[cno],KFName=[Q]\UBmstr\BudIdx1.H[cno],Shr",internal,outIn,keyed 
00240 ! 
00250 SCREEN1: ! 
00260   a$="" : prtbkno=0
00270   fnTos(sn$="PrtBl35x75") !:
        pf=26 : ll=24 !:
        respc=0
00280   fnLbl(1,1,"Service From:",ll,1)
00290   fnTxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00300   fnLbl(2,1,"Service To:",ll,1)
00310   fnTxt(2,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00320   fnLbl(3,1,"Penalty Due Date:",ll,1)
00330   fnTxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00340   fnLbl(4,1,"Message on Bill:",ll,1)
00350   fnTxt(4,pf,50) !:
        resp$(respc+=1)=mg$(1)
00360   fnTxt(5,pf,50) !:
        resp$(respc+=1)=mg$(2)
00370   fnTxt(6,pf,50) !:
        resp$(respc+=1)=mg$(3)
00380   fnLbl(7,1,"Date of Billing:",ll,1)
00390   fnTxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00400   fnLbl(8,1,"Starting Route/Sequence:",ll,1)
00410   fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2) !:
        resp$(respc+=1)="[All]"
00420   fnLbl(9,1,"Route Number:",ll,1)
00430   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00440   fnChk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00450   fnCmdSet(3)
00460   fnAcs(sn$,0,mat resp$,ck)
00470   if ck=5 then goto XIT
00480   d1 = val(resp$(7)) !:
        d2x=val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6)
00490   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00500   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00510   if resp$(10)="True" then sl1=1 else sl1=0
00520   if trim$(a$)<>"" then !:
          read #2,using L530,key=a$: holdz$,route,sequence nokey SCREEN1 !:
          st1=1
00530 L530: form pos 1,c 10,pos 1741,n 2,n 7
00540   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": !:
          ! if no beginning account or starting route #, start at beginning of file
00550   if trim$(a$)<>"" then !:
          restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00560   if trim$(a$)="" and prtbkno>0 then !:
          restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": !:
          ! selected a route and no beginning account
00570 ! 
00580   open #3: "Name=[Q]\UBmstr\UBAdrBil.H[cno],KFName=[Q]\UBmstr\adrIndex.H[cno],Shr",internal,input,keyed 
00590   fnopenprn
00600 ! 
00610   gosub BULKSORT
00620 L620: if sl1=1 then goto SCREEN3 ! select accounts
00630 L630: read #7,using L640: r6 eof F5_CANCEL
00640 L640: form pos 1,pd 3
00650   read #1,using L690,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey L630
00660   if trim$(a$)<>"" and holdz$<>z$ then goto L630 else a$=""
00670   goto L690
00680   read #2,using L690: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 eof F5_CANCEL
00690 L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6
00700   mat ba=(0): budget=0
00710   if d2=0 and d2x>0 then d2=d2x
00720   if d3=0 and d3x>0 then d3=d3x
00730   read #81,using 'Form POS 1,C 10,PD 4,12*PD 5.2',key=z$: x$,mat ba nokey L750
00740   for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
00750 L750: if prtbkno=0 then goto L770
00760   if prtbkno><route then goto F5_CANCEL
00770 L770: if f><d1 then goto L620
00780 ! If ST1=0 Then Goto HERE
00790 ! If ST1$=Z$ Then sT1=0 Else Goto 650
00800 HERE: ! 
00810 ! read alternate billing address
00820   read #3,using L840,key=z$: mat ba$ nokey L910
00830   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L910
00840 L840: form pos 11,4*c 30
00850   e1=0 : mat pe$=("")
00860   for j=1 to 4
00870     if trim$(ba$(j))<>"" then !:
            pe$(e1+=1)=ba$(j)
00880   next j
00890   goto L1030
00900 ! ______________________________________________________________________
00910 L910: e1=0 : mat pe$=("")
00920   for j=2 to 4
00930     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00940   next j
00950   goto L1030
00960 ! ______________________________________________________________________
00970 F5_CANCEL: ! 
00980   close #1: ioerr L990
00990 L990: close #3: ioerr L1000
01000 L1000: fncloseprn
01010   goto ENDSCR
01020 ! ______________________________________________________________________
01030 L1030: ! 
01040   pb=bal-g(11)
01050 ! ______________print bill routine______________________________________
01060   gosub PRINTBILL
01070 ! _____________end of pr routine______________________________________
01080   bct(2)=bct(2)+1 !:
        ! accumulate totals
01090   goto L620
01100 ! ______________________________________________________________________
01110 SCREEN3: ! 
01120   fnTos(sn$="Pennington-Bill")
01130   fnLbl(1,1,"Account (blank to stop)",31,1)
01140   if z$<>"" then !:
          fnLbl(3,1,"Last Account entered was "&z$,44,1)
01150   fncmbact(1,17) ! !:
        resp$(1)=a$
01160   fnCmdKey("Add",1,1,0) !:
        fnCmdKey("Print",5,0,1)
01170   fnAcs(sn$,0,mat resp$,ck)
01180   if ck=5 then goto F5_CANCEL
01190   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto ENDSCR
01200   read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3 nokey SCREEN3
01210   goto HERE
01220 ! ______________________________________________________________________
01230 BULKSORT: ! sort in bulk sort code sequence
01240   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01250 L1250: form pos 1,c 128
01260   write #9,using L1250: "FILE customer.H[cno],[Q]\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01270   if prtbkno>0 then !:
          write #9,using L1250: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
01280   write #9,using L1250: "MASK 1942,12,C,A,1,10,C,A"
01290   close #9: 
01300   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1310
01310 L1310: execute "Sort "&env$('Temp')&"\Control."&session$
01320   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01330   return 
01340 ! ______________________________________________________________________
01350 ENDSCR: ! pr totals screen
01360   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01370   fnTos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01380   fnLbl(1,1,"Total Bills Printed:",mylen,1)
01390   fnTxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01400   fnCmdSet(52) !:
        fnAcs(sn$,0,mat resp$,ck)
01410   goto XIT
01420 ! ______________________________________________________________________
01430 XIT: fnxit
01440 ! ______________________________________________________________________
01450 ! <Updateable Region: ERTN>
01460 ERTN: fnerror(program$,err,line,act$,"xit")
01470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01500 ERTN_EXEC_ACT: execute act$ : goto ERTN
01510 ! /region
01520 ! ______________________________________________________________________
01530 PRINTBILL: ! 
01540   pb=bal-g(11)
01550   pr #255: ""
01560   pr #255,using 'Form POS 43,C 30': e$(1) !:
        pr #255: "" !:
        pr #255: "" !:
        pr #255: "" !:
        pr #255: ""
01570   pr #255,using 'Form POS 3,c 10,pos 18,n 6,pos 25,n 6,pos 38,n 9,n 9,n 9, n 9.2': 'Water',d3,d2,d(2),d(1),d(3),g(1)
01580   if g(2)=0 then !:
          pr #255,using 'Form POS 3,C 10': "" else !:
          pr #255,using 'Form POS 3,C 10,pos 65,n 9.2': "SEWER",g(2)
01590   if g(4)=0 then !:
          pr #255,using 'Form POS 3,C 10': "" else !:
          pr #255,using 'Form POS 3,c 10,pos 18,n 6,pos 25,n 6,pos 38,n 9,n 9,n 9, n 9.2': 'Gas',d3,d2,d(10),d(9),d(11),g(4)
01600   if g(5)=0 then !:
          pr #255,using 'Form POS 3,C 10': "" else !:
          pr #255,using 'Form POS 3,C 10,POS 65,N 9.2': "SANIT",g(5)
01610   if g(8)=0 then !:
          pr #255,using 'Form POS 3,C 10': "" else !:
          pr #255,using 'Form POS 3,C 10,POS 65,N 9.2': "OTHER",g(8),pe$(2)
01620   if g(9)=0 then !:
          pr #255,using 'Form POS 3,C 10': "" else !:
          pr #255,using 'Form POS 3,C 10,POS 65,N 9.2': "Sales Tax",g(9)
01630   if pb<0 then balance$="Credit" else balance$="Overdue"
01640   if pb=0 then !:
          pr #255,using 'Form POS 3,c 10': "" else !:
          pr #255,using 'Form POS 3,C 10,POS 65,N 9.2': balance$,pb
01650   pr #255,using "form pos 12,c 30": pe$(1)
01660   pr #255,using "form pos 12,c 30,pos 51,c 10,pos 64,pic(zz/zz/zz)": pe$(2),z$,d4
01670   pr #255,using "form pos 12,c 30": pe$(3)
01680   pr #255,using "form pos 12,c 30,pos 51,n 10.2,pos 62,n 10.2": pe$(4),bal+g(10),bal
01690   if trim$(mg$(1))="" then mg$(1)="Emergency vehicles need house numbers-please help"
01700   if trim$(mg$(2))="" then mg$(2)="If you smell gas call 472-6556 or 472-3535"
01710   pr #255,using "Form pos 1,c 60,skip 1,pos 1,c 60,skip 1,pos 1,c 60": mg$(1),mg$(2)
01720   pr #255: newpage
01730   return  ! read next record
01740 ! ______________________________________________________________________
