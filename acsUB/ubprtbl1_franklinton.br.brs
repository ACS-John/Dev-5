00010 ! Replace S:\acsUB\ubprtbl1_Franklinton
00020 ! pr bills for Town of Franklinton
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fncmdkey,fnbarcode,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$) !:
        fnd1(d1)
00120   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00130   at$(1)=cnam$ !:
        let z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00140   let z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00150   linelength=62
00160 ! 
00170 ! fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00180 ! Gosub BULKSORT ! want printed in alphabetic order
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00210   def fnc(x)=int(100*(x+sgn(x)*.0001))
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   a$="" : prtbkno=0
00250   fntos(sn$="UBPrtBl1-1") !:
        pf=33 : ll=30 !:
        respc=0
00260   fnlbl(3,1,"Penalty Due Date:",ll,1)
00270   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00280   fnlbl(4,1,"Message on Bill:",ll,1)
00290   fntxt(4,pf,30,30) !:
        resp$(respc+=1)=mg$(1)
00300   fntxt(5,pf,30,30) !:
        resp$(respc+=1)=mg$(2)
00310   fntxt(6,pf,30,30) !:
        resp$(respc+=1)=mg$(3)
00320   fnlbl(7,1,"Date of Billing:",ll,1)
00330   fntxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00340   fnlbl(8,1,"Starting Account:",ll,1)
00350   let fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00360   fnlbl(9,1,"Route Number:",ll,1)
00370   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00380   fnchk(10,pf,"Select Accounts to Print:",1) !:
        resp$(respc+=1)="False"
00390   fnlbl(11,1,"Date Meter Read:",ll,1) !:
        fntxt(11,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",newd3)
00400   fnlbl(12,1,"Previous Reading Date:",ll,1) !:
        fntxt(12,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",newd2)
00410   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00420   if ck=5 then goto ENDSCR
00430   d1 = val(resp$(5)) !:
        d4 = val(resp$(1)) !:
        mg$(1) = resp$(2) !:
        mg$(2) = resp$(3) !:
        mg$(3) = resp$(4)
00440   if resp$(6)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(6)(1:9)),9)
00450   if resp$(7)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(7))
00460   if resp$(8)="True" then sl1=1: let z$="" else sl1=0
00470   if trim$(a$)<>"" then read #2,using L500,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00480   newd3=val(resp$(9))
00490   newd2=val(resp$(10))
00500 L500: form pos 1,c 10,pos 1741,n 2,n 7
00510   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00520   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00530   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00540 ! ______________________________________________________________________
00550   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00560   gosub BUD1
00570   gosub VBOPENPRINT
00580 ! ______________________________________________________________________
00590   on fkey 5 goto RELEASE_PRINT
00600 L600: if sl1=1 then goto SCREEN3
00610 L610: ! Read #6,Using 640: Z$ Eof 910
00620   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L610 ! start with
00630   begin=0 ! cancel starting account
00640   form pos 22,c 10
00650 ! Read #1,Using 680,Key=Z$: Z$,MAT E$,F$,A3,MAT B,FINAL,MAT D,BAL,F,MAT G,BRA,MAT GB,ROUTE,D3,D2,BULK$,EXTRA1$,ESTIMATEDATE Nokey 610
00660   read #1,using L690: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate eof RELEASE_PRINT
00670   if d3=0 then d3=newd3
00680   if d2=0 then d2=newd2
00690 L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
00700   if prtbkno=0 then goto L720
00710   if prtbkno><route then goto L610
00720 L720: if f><d1 then goto L600
00730   if st1=0 then goto READALTADR
00740 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00750 READALTADR: ! 
00760 ! read alternate billing address
00770   read #3,using L780,key=z$: mat ba$ nokey L850
00780 L780: form pos 11,4*c 30
00790   e1=0 : mat pe$=("")
00800   for j=1 to 4
00810     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00820   next j
00830   goto L1010
00840 ! ______________________________________________________________________
00850 L850: e1=0 : mat pe$=("")
00860   for j=2 to 4
00870     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00880   next j
00890   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00900   goto L1010
00910 ! ______________________________________________________________________
00920 RELEASE_PRINT: ! 
00930   close #1: ioerr L940
00940 L940: close #3: ioerr L950
00950 L950: fnpa_finis
00990   goto ENDSCR
01000 ! ______________________________________________________________________
01010 L1010: ! 
01020   if bud1=1 then gosub BUD2
01030   pb=bal-g(11)
01040 ! If BAL<=0 Then Let G(10)=0 ! don't show penalty if balance 0 or less
01050 ! ______________print bill routine______________________________________
01060   gosub VBPRINT
01070 ! _____________end of pr routine______________________________________
01080   bct(2)=bct(2)+1 !:
        ! accumulate totals
01090   goto L600
01100 ! ______________________________________________________________________
01110 SCREEN3: ! 
01120   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01130   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01140 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01150   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01160   fncmbact(1,17) ! !:
        resp$(1)=a$
01170   fncmdkey("&Next",1,1,0,"Accept this record for printing") !:
        fncmdkey("&Complete",5,0,1,"Print all selected records")
01180   fnacs(sn$,0,mat resp$,ck)
01190   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01200   if ck=5 then goto RELEASE_PRINT
01210   read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01220   if d3=0 then d3=newd3
01230   if d2=0 then d2=newd2
01240   goto READALTADR
01250 ! ______________________________________________________________________
01260 SORT1: ! SELECT & SORT
01270   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1520
01280   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01290   s5=1
01300   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01310   restore #2,search>=routekey$: 
01320 L1320: read #2,using L1330: z$,f,route eof END5
01330 L1330: form pos 1,c 10,pos 296,pd 4,pos 1741
01340   if prtbkno=0 then goto L1360
01350   if prtbkno><route then goto END5
01360 L1360: if f><d1 then goto L1320
01370   let zip5$=cr$=""
01380   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1390
01390 L1390: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01400   goto L1320
01410 ! ______________________________________________________________________
01420 END5: close #6: 
01430   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01440 L1440: form pos 1,c 128
01450   write #9,using L1440: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01460   write #9,using L1440: "Mask 1,19,C,A"
01470   close #9: 
01480   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1490
01490 L1490: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01500   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01510   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01520 L1520: return 
01530 ! ______________________________________________________________________
01540 ENDSCR: ! pr totals screen
01550   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01560   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01570   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01580   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01590 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01600 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01610 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01620 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01630 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01640 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01650   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01660 XIT: fnxit
01670 ! ______________________________________________________________________
01680 ERTN: fnerror(program$,err,line,act$,"xit")
01690   if uprc$(act$)<>"PAUSE" then goto L1720
01700   execute "list -"&str$(line) !:
        pause  !:
        goto L1720
01710   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01720 L1720: execute act$
01730   goto ERTN
01740 ! ______________________________________________________________________
01750 VBOPENPRINT: ! 
01770   fnPa_open("Landscape")
01800   lyne=3
01830   return 
01840 ! ______________________________________________________________________
01850 VBPRINT: ! 
01860 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01870   checkcounter+=1
01880   if checkcounter=1 then let xmargin=20 : let ymargin=0
01890   if checkcounter=2 then let xmargin=174 : let ymargin=0
01900   if checkcounter=3 then let xmargin=20: let ymargin=108
01910   if checkcounter=4 then let xmargin=174 : let ymargin=108 !:
          checkcounter=0
01920 ! ______________________________________________________________________
01930   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01940   pr #20: "Call Print.MyFontBold(True)"
01950   pr #20: 'Call Print.MyFontSize(12)'
01960   pr #20: 'Call Print.MyFont("Courier New")'
01970   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01980   pr #20: 'Call Print.MyFont("Lucida Console")'
01990   pr #20: 'Call Print.MyFontSize(10)'
02000   pr #20: 'Call Print.MyFontBold(False)'
02010   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
02020   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
02030   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
02040   pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
02050   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",newd2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",newd3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
02060   pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
02070   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
02080   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
02090   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
02100   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
02110   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
02120 ! ______________________________________________________________________
02130 PRINTGRID: meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02140   if g(1)=0 then goto L2150 else !:
          pr #20: 'Call Print.AddText("Water",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02150 L2150: if g(2)=0 then goto L2160 else !:
          pr #20: 'Call Print.AddText("Sewer",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02160 L2160: ! If A4=1 Then Let GCODE$="RSGS" Else !:
        ! If A4=2 Then Let GCODE$="CMGS" Else !:
        ! If A4=3 Then Let GCODE$="INGS" Else !:
        let gcode$="GAS"
02170   if g(4)=0 then goto L2180 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02180 L2180: if g(3)=0 then goto L2200 else !:
          pr #20: 'Call Print.AddText("Safe Water Fee",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02190   pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02200 L2200: if g(5)=0 then goto L2210 else !:
          pr #20: 'Call Print.AddText("Sanitation",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02210 L2210: if g(6)=0 then goto L2220 else !:
          pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02220 L2220: if g(7)=0 then goto L2230 else !:
          pr #20: 'Call Print.AddText("Tax-Water",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02230 L2230: if g(8)=0 then goto L2240 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02240 L2240: if g(9)=0 then goto L2250 else !:
          pr #20: 'Call Print.AddText("Tax-Gas",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02250 L2250: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
02260   if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
02270 ! If BUDGET>0 Then pB=PBUD ! owe old budget payment
02280   if pb=0 then goto L2290 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
02290 L2290: pr #20: 'Call Print.MyFontSize(10)'
02300 ! ______________________________________________________________________
02310   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin+10)&',63,0)'
02330 ! pr #20: 'Call Print.AddText("Budget Payment",'&STR$(XMARGIN+99)&','&STR$(LYNE*12+YMARGIN)&')'
02340 ! If BUDGET>0 Then bAL=BUDGET+PBUD ! IF BUDGET MAKE NET DUE = BUDGET PLUS ANY OLD BUDGET PAYMENTS NOT MADE
02350   if budget>0 then goto L2360 else goto L2410
02360 L2360: pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02370   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
02380   pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02390   pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
02400   goto L2460
02410 L2410: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02420   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
02430   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02440   if bal<=0 then payafter=bal else payafter=bal+g(10) ! (BAL*.10,2)
02450   pr #20: 'Call Print.AddText("'&fnformnumb$(payafter,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
02460 L2460: pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
02470   pr #20: 'Call Print.AddText("Phone: 985-839-3569",'&str$(xmargin+1)&','&str$(lyne*31+ymargin)&')'
02480 ! pr #20: 'Call Print.AddText("Re-connect fee $??.00",'&STR$(XMARGIN+1)&','&STR$(LYNE*28+YMARGIN)&')'
02490 ! ______________________________________________________________________
02500   special=28
02510 ! ______________________________________________________________________
02520   pr #20: 'Call Print.MyFontSize(7)'
02530   pr #20: 'Call Print.AddLine('&str$(xmargin+112)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02540   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+0)&',7,0)'
02550   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+2.8)&',7,0)'
02560   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+5.6)&',7,0)'
02570   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+8.4)&',7,0)'
02580   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+11.2)&',7,0)'
02590   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+14)&',7,0)'
02600   pr #20: 'Call Print.AddLine('&str$(xmargin+105)&','&str$(ymargin+17)&',7,0)'
02610 ! pr #20: 'Call Print.AddText("   Pre-Sorted",'&STR$(XMARGIN+115)&','&STR$(LYNE*1-1+YMARGIN)&')'
02620   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+115)&','&str$(lyne*2-1+ymargin)&')'
02630   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+115)&','&str$(lyne*3-1+ymargin)&')'
02640   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+115)&','&str$(lyne*4-1+ymargin)&')'
02650   pr #20: 'Call Print.AddText("  Permit No 040",'&str$(xmargin+115)&','&str$(lyne*5-1+ymargin)&')'
02660   pr #20: 'Call Print.MyFontSize(9)'
02670 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+99)&','&STR$(LYNE*7+YMARGIN-6)&')'
02680   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+99)&','&str$(lyne*7+ymargin)&')'
02690   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+99)&','&str$(lyne*8+ymargin)&')'
02700   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+99)&','&str$(lyne*9+ymargin)&')'
02710   pr #20: 'Call Print.MyFontSize(10)'
02720   if budget>0 then goto L2730 else goto L2760
02730 L2730: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ)",d4*.01)&':",'&str$(xmargin+99)&','&str$(lyne*11+ymargin)&')'
02740   pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02750   goto L2810
02760 L2760: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ)",d4*.01)&':",'&str$(xmargin+99)&','&str$(lyne*11+ymargin)&')'
02770   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+126)&','&str$(lyne*11+ymargin)&')'
02780   if bal<=0 then payafter=bal else payafter=bal+g(10) ! AL*.10,2)
02790   pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ)",d4*.01)&':",'&str$(xmargin+99)&','&str$(lyne*12+ymargin)&')'
02800   pr #20: 'Call Print.AddText("'&fnformnumb$(payafter,2,9)&'",'&str$(xmargin+126)&','&str$(lyne*12+ymargin)&')'
02810 L2810: pr #20: 'Call Print.MyFontSize(9)'
02820   addy=14
02830   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+99)&','&str$((addy+=1)*lyne+ymargin)&')'
02840   pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+99)&','&str$((addy+=1)*lyne+ymargin)&')'
02850   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+99)&','&str$((addy+=1)*lyne+ymargin)&')'
02860   addy+=1
02870   pr #20: 'Call Print.MyFontSize(10)'
02880   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02890   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02900   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+99)&','&str$(lyne*(addy+=1)+ymargin)&')'
02910   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+99)&','&str$(lyne*(addy+=1)+ymargin)&')'
02920   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+99)&','&str$(lyne*(addy+=1)+ymargin)&')'
02930   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+99)&','&str$(lyne*(addy+=1)+ymargin)&')'
02940   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+99)&','&str$(lyne*(addy+=1)+ymargin)&')'
02950   if checkcounter=1 then checkx=1.375 : checky=3.6875 !:
          let xbar=119: let ybar=75
02960   if checkcounter=2 then checkx=6.75 : checky=3.6875 !:
          let xbar=273: let ybar=75
02970   if checkcounter=3 then checkx=1.375 : checky=7.9375 !:
          let xbar=119: let ybar=185
02980   if checkcounter=0 then checkx=6.75 : checky=7.9375 !:
          let xbar=273: let ybar=185
02995   fnbarcode(z$,xbar,ybar)
03010   if checkcounter=0 then !:
          fnpa_newpage
03020   return 
03030 ! ______________________________________________________________________
03040 BULKSORT: ! bulk sort order
03050   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
03060   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03070 L3070: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3100
03080   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03090   goto L3070
03100 L3100: close #1: ioerr L3110
03110 L3110: close #6: ioerr L3120
03120 L3120: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3140
03130   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
03140 L3140: return 
03150 BUD1: bud1=0
03160   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
03170   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L3230
03180   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&",Shr",internal,outin,relative 
03190   bud1=1
03200   for j=1 to 5
03210     bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
03220   next j
03230 L3230: return 
03240 BUD2: ! 
03250   budget=pbud=bd1=0
03260   mat bd1(5)
03270   mat bd1=(0)
03280   mat bd2=(0)
03290   if bud1=0 then goto L3390
03300   read #81,using L3310,key=z$: z$,mat ba,mat badr nokey L3390
03310 L3310: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
03320   ta1=badr(1)
03330 L3330: if ta1=0 then goto L3390
03340   read #82,using L3350,rec=ta1: z$,mat bt1,nba norec L3390
03350 L3350: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
03360   if bt1(1,1)=d1 then budget=budget+bt1(12,1): goto L3380 ! budget for current month
03370   if bt1(14,1)=0 then pbud=pbud+bt1(12,1): goto L3380 ! budget for any previous months not paid
03380 L3380: ta1=nba : goto L3330
03390 L3390: return 
