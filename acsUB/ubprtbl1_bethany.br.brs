00010 ! Replace S:\acsUB\ubprtbl1_Bethany
00020 ! pr bills for Village of Bethany
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fncmdkey,fnpa_finis,fnpa_txt,fnpa_newpage,fnpa_open
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
00170   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00180 ! Gosub BULKSORT ! want printed in alphabetic order
00190 ! Open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&STR$(CNO)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",Internal,Input,Keyed  ! open in Account order
00192   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBIndx2.h"&str$(cno)&",Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
00193   open #8: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
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
00655   read #1,using L680: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate eof RELEASE_PRINT
00660   if newd3<>0 then d3=newd3
00670   if newd2<>0 then d2=newd2
00680 L680: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
00690   if prtbkno=0 then goto L710
00700   if prtbkno><route then goto RELEASE_PRINT
00710 L710: if f><d1 then goto L600
00720   if st1=0 then goto READALTADR
00730 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00740 READALTADR: ! 
00750 ! read alternate billing address
00760   read #3,using L770,key=z$: mat ba$ nokey L840
00770 L770: form pos 11,4*c 30
00780   e1=0 : mat pe$=("")
00790   for j=1 to 4
00800     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00810   next j
00820   goto L1000
00830 ! ______________________________________________________________________
00840 L840: e1=0 : mat pe$=("")
00850   for j=2 to 4
00860     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00870   next j
00880   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00890   goto L1000
00900 ! ______________________________________________________________________
00910 RELEASE_PRINT: ! 
00920   close #1: ioerr L930
00930 L930: close #3: ioerr L940
00940 L940: fnpa_finis
00980   goto ENDSCR
00990 ! ______________________________________________________________________
01000 L1000: ! 
01010   if bud1=1 then gosub BUD2
01020   pb=bal-g(11)
01030 ! If BAL<=0 Then Let G(10)=0 ! don't show penalty if balance 0 or less
01040 ! ______________print bill routine______________________________________
01050   gosub VBPRINT
01060 ! _____________end of pr routine______________________________________
01070   bct(2)=bct(2)+1 !:
        ! accumulate totals
01080   goto L600
01090 ! ______________________________________________________________________
01100 SCREEN3: ! 
01110   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01120   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01130 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01140   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01150   fncmbact(1,17) ! !:
        resp$(1)=a$
01160   fncmdkey("&Next",1,1,0,"Accept this record for printing") !:
        fncmdkey("&Complete",5,0,1,"Print all selected records")
01170   fnacs(sn$,0,mat resp$,ck)
01180   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01190   if ck=5 then goto RELEASE_PRINT
01200   read #8,using L680,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01210   if d3=0 then d3=newd3
01220   if d2=0 then d2=newd2
01230   goto READALTADR
01240 ! ______________________________________________________________________
01250 SORT1: ! SELECT & SORT
01260   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1510
01270   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01280   s5=1
01290   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01300   restore #2,search>=routekey$: 
01310 L1310: read #2,using L1320: z$,f,route eof END5
01320 L1320: form pos 1,c 10,pos 296,pd 4,pos 1741
01330   if prtbkno=0 then goto L1350
01340   if prtbkno><route then goto END5
01350 L1350: if f><d1 then goto L1310
01360   let zip5$=cr$=""
01370   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1380
01380 L1380: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01390   goto L1310
01400 ! ______________________________________________________________________
01410 END5: close #6: 
01420   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01430 L1430: form pos 1,c 128
01440   write #9,using L1430: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01450   write #9,using L1430: "Mask 1,19,C,A"
01460   close #9: 
01470   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1480
01480 L1480: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01490   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01500   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01510 L1510: return 
01520 ! ______________________________________________________________________
01530 ENDSCR: ! pr totals screen
01540   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01550   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01560   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01570   fntxt(1,mypos,8,0,1,"",0) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01640   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01650 XIT: fnxit
01660 ! ______________________________________________________________________
01670 ERTN: fnerror(program$,err,line,act$,"xit")
01680   if uprc$(act$)<>"PAUSE" then goto L1710
01690   execute "list -"&str$(line) !:
        pause  !:
        goto L1710
01700   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01710 L1710: execute act$
01720   goto ERTN
01730 ! ______________________________________________________________________
01740 VBOPENPRINT: ! 
01750   fnPa_open("Landscape")
01790   lyne=3
01820 return 
01830 ! ______________________________________________________________________
01840 VBPRINT: ! 
01850 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01860   checkcounter+=1
01870   if checkcounter=1 then let xmargin=0 : let ymargin=0
01880   if checkcounter=2 then let xmargin=139 : let ymargin=0
01890   if checkcounter=3 then let xmargin=0 : let ymargin=108
01900   if checkcounter=4 then let xmargin=139 : let ymargin=108 !:
          checkcounter=0
01910 ! ______________________________________________________________________
01920   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01930   pr #20: "Call Print.MyFontBold(True)"
01940   pr #20: 'Call Print.MyFontSize(12)'
01950   pr #20: 'Call Print.MyFont("Courier New")'
01960   fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01970   pr #20: 'Call Print.MyFont("Lucida Console")'
01980   pr #20: 'Call Print.MyFontSize(10)'
01990   pr #20: 'Call Print.MyFontBold(False)'
02000   fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2) ! pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
02010   fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
02020   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
02030   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin) ! pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
02040   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
02050   pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
02060   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
02070   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
02080   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
02090   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
02100   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
02110 ! ______________________________________________________________________
02120 PRINTGRID: meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02130   if g(1)=0 then goto L2140 else !:
          pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02140 L2140: if g(2)=0 then goto L2150 else !:
          pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02150 L2150: if g(3)=0 and d(7)=0 then goto L2170 else !:
          pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02160   pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02170 L2170: ! If A4=1 Then Let GCODE$="RSGS" Else !:
        ! If A4=2 Then Let GCODE$="CMGS" Else !:
        ! If A4=3 Then Let GCODE$="INGS" Else !:
        let gcode$="GAS"
02180   if g(4)=0 then goto L2190 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02190 L2190: if g(5)=0 then goto L2200 else !:
          pr #20: 'Call Print.AddText("SL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02200 L2200: if g(6)=0 then goto L2210 else !:
          pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02210 L2210: if g(7)=0 then goto L2220 else !:
          pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02220 L2220: if g(8)=0 then goto L2230 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02230 L2230: if g(9)=0 then goto L2240 else !:
          pr #20: 'Call Print.AddText("GAS TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02240 L2240: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
02250   if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
02260 ! If BUDGET>0 Then pB=PBUD ! owe old budget payment
02270   if pb=0 then goto L2280 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
02280 L2280: pr #20: 'Call Print.MyFontSize(10)'
02290 ! ______________________________________________________________________
02300   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02310   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin+10)&',63,0)'
02320 ! pr #20: 'Call Print.AddText("Budget Payment",'&STR$(XMARGIN+68)&','&STR$(LYNE*12+YMARGIN)&')'
02330 ! If BUDGET>0 Then bAL=BUDGET+PBUD ! IF BUDGET MAKE NET DUE = BUDGET PLUS ANY OLD BUDGET PAYMENTS NOT MADE
02340   if budget>0 then goto L2350 else goto L2400
02350 L2350: pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02360   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
02370   pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02380   pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
02390   goto L2440
02400 L2400: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02410   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
02420   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02430   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
02440 L2440: pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
02450 ! pr #20: 'Call Print.AddText("Phone: 217-665-3351",'&STR$(XMARGIN+1)&','&STR$(LYNE*27+YMARGIN)&')'
02460 ! pr #20: 'Call Print.AddText("Re-connect fee $??.00",'&STR$(XMARGIN+1)&','&STR$(LYNE*28+YMARGIN)&')'
02470 ! ______________________________________________________________________
02480   special=28
02490 ! ______________________________________________________________________
02500   pr #20: 'Call Print.MyFontSize(7)'
02510   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02520   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02530   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02540   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02550   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02560   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02570   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02580   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02590 ! pr #20: 'Call Print.AddText("   Pre-Sorted",'&STR$(XMARGIN+100)&','&STR$(LYNE*1-1+YMARGIN)&')'
02600   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02610   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02620   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02630   pr #20: 'Call Print.AddText("  Permit No 1",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02640   pr #20: 'Call Print.MyFontSize(9)'
02650 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02660   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
02670   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
02680   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
02690   pr #20: 'Call Print.MyFontSize(10)'
02700   if budget>0 then goto L2710 else goto L2740
02710 L2710: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02720   pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02730   goto L2780
02740 L2740: pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02750   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02760   pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
02770   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
02780 L2780: pr #20: 'Call Print.MyFontSize(9)'
02790   addy=14
02800   fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
02810   fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
02820   fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
02830   addy+=1
02840   pr #20: 'Call Print.MyFontSize(10)'
02850   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02860   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
02870   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02880   if pe$(1)<>"" then !:
          fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
02890   if pe$(2)<>"" then !:
          fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
02900   if pe$(3)<>"" then !:
          fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
02910   if pe$(4)<>"" then !:
          fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
02920   if checkcounter=1 then checkx=1.375 : checky=3.6875
02930   if checkcounter=2 then checkx=6.75 : checky=3.6875
02940   if checkcounter=3 then checkx=1.375 : checky=7.9375
02950   if checkcounter=0 then checkx=6.75 : checky=7.9375
02980   if checkcounter=0 then !:
          fnpa_newpage
02990   return 
03000 ! ______________________________________________________________________
03010 BULKSORT: ! bulk sort order
03020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
03030   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03040 L3040: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3070
03050   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03060   goto L3040
03070 L3070: close #1: ioerr L3080
03080 L3080: close #6: ioerr L3090
03090 L3090: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3110
03100   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
03110 L3110: return 
03120 BUD1: bud1=0
03130   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
03140   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L3200
03150   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&",Shr",internal,outin,relative 
03160   bud1=1
03170   for j=1 to 5
03180     bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
03190   next j
03200 L3200: return 
03210 BUD2: ! 
03220   budget=pbud=bd1=0
03230   mat bd1(5)
03240   mat bd1=(0)
03250   mat bd2=(0)
03260   if bud1=0 then goto L3360
03270   read #81,using L3280,key=z$: z$,mat ba,mat badr nokey L3360
03280 L3280: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
03290   ta1=badr(1)
03300 L3300: if ta1=0 then goto L3360
03310   read #82,using L3320,rec=ta1: z$,mat bt1,nba norec L3360
03320 L3320: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
03330   if bt1(1,1)=d1 then budget=budget+bt1(12,1): goto L3350 ! budget for current month
03340   if bt1(14,1)=0 then pbud=pbud+bt1(12,1): goto L3350 ! budget for any previous months not paid
03350 L3350: ta1=nba : goto L3300
03360 L3360: return 
