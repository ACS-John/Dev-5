00010 ! Replace S:\acsUB\ubprtbl1_montic
00020 ! pr bills for Village of Monticello (4 part plain paper)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00120   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00130   at$(1)=cnam$ !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00140   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00150   linelength=62
00160 ! 
00170   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00180   gosub BULKSORT
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence #
00210   def fnc(x)=int(100*(x+sgn(x)*.0001))
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   a$="" : prtbkno=0
00250   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
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
00350   fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&env$('cno') !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&env$('cno') !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00360   fnlbl(9,1,"Route Number:",ll,1)
00370   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00380   fnchk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00390   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00400   if ck=5 then goto ENDSCR
00410   d1 = val(resp$(5)) !:
        d4 = val(resp$(1)) !:
        mg$(1) = resp$(2) !:
        mg$(2) = resp$(3) !:
        mg$(3) = resp$(4)
00420   if resp$(6)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(6)(1:9)),9)
00430   if resp$(7)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(7))
00440   if resp$(8)="True" then sl1=1: z$="" else sl1=0
00450   if trim$(a$)<>"" then read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00460 L460: form pos 1,c 10,pos 1741,n 2,n 7
00470   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00480   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00490   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00500 ! ______________________________________________________________________
00510   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00520   gosub VBOPENPRINT
00530 ! ______________________________________________________________________
00540   on fkey 5 goto RELEASE_PRINT
00550 L550: if sl1=1 then goto SCREEN3
00560 L560: read #6,using L590: z$ eof RELEASE_PRINT
00570   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
00580   begin=0 ! cancel starting account
00590 L590: form pos 22,c 10
00600   read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L560
00610 L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
00620   if prtbkno=0 then goto L640
00630   if prtbkno><route then goto RELEASE_PRINT
00640 L640: if f><d1 then goto L550
00650   if st1=0 then goto READALTADR
00660 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00670 READALTADR: ! 
00680 ! read alternate billing address
00690   read #3,using L700,key=z$: mat ba$ nokey L790
00700 L700: form pos 11,4*c 30
00710   e1=0 : mat pe$=("")
00720   for j=1 to 4
00730     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00740   next j
00750   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00760   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00770   goto L940
00780 ! ______________________________________________________________________
00790 L790: e1=0 : mat pe$=("")
00800   for j=2 to 4
00810     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00820   next j
00830   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00840   goto L940
00850 ! ______________________________________________________________________
00860 RELEASE_PRINT: ! 
00870   close #1: ioerr L880
00880 L880: close #3: ioerr L890
00890 L890: fnpa_finis
00920   goto ENDSCR
00930 ! ______________________________________________________________________
00940 L940: ! 
00950   pb=bal-g(11)
00960   if bal<=0 then g(9)=g(10)=0 ! don't show penalty if balance 0 or less
00970 ! ______________print bill routine______________________________________
00980   gosub VBPRINT
00990 ! _____________end of pr routine______________________________________
01000   bct(2)=bct(2)+1 !:
        ! accumulate totals
01010   goto L550
01020 ! ______________________________________________________________________
01030 SCREEN3: ! 
01040   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01050   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01060 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01070   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01080   fncmbact(1,17) ! !:
        resp$(1)=a$
01090   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
01100   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01110   if ck=5 then goto RELEASE_PRINT
01120   read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01130   goto READALTADR
01140 ! ______________________________________________________________________
01150 SORT1: ! SELECT & SORT
01160   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr L1410
01170   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01180   s5=1
01190   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01200   restore #2,search>=routekey$: 
01210 L1210: read #2,using L1220: z$,f,route eof END5
01220 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
01230   if prtbkno=0 then goto L1250
01240   if prtbkno><route then goto END5
01250 L1250: if f><d1 then goto L1210
01260   zip5$=cr$=""
01270   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
01280 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01290   goto L1210
01300 ! ______________________________________________________________________
01310 END5: close #6: 
01320   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01330 L1330: form pos 1,c 128
01340   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01350   write #9,using L1330: "Mask 1,19,C,A"
01360   close #9: 
01370   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
01380 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
01390   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01400   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01410 L1410: return 
01420 ! ______________________________________________________________________
01430 ENDSCR: ! pr totals screen
01440   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01450   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01460   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01470   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01480 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01490 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01500 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01510 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01520 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01530 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01540   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01550 XIT: fnxit
01560 ! ______________________________________________________________________
01570 ERTN: fnerror(program$,err,line,act$,"xit")
01580   if uprc$(act$)<>"PAUSE" then goto L1610
01590   execute "list -"&str$(line) !:
        pause  !:
        goto L1610
01600   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01610 L1610: execute act$
01620   goto ERTN
01630 ! ______________________________________________________________________
01640 VBOPENPRINT: ! 
01660     fnPa_open("Landscape")
01690     lyne=3
01720   return 
01730 ! ______________________________________________________________________
01740 VBPRINT: ! 
01750 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01760   checkcounter+=1
01770   if checkcounter=1 then xmargin=0 : ymargin=0
01780   if checkcounter=2 then xmargin=139 : ymargin=0
01790   if checkcounter=3 then xmargin=0 : ymargin=108
01800   if checkcounter=4 then xmargin=139 : ymargin=108 !:
          checkcounter=0
01810 ! ______________________________________________________________________
01820   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01830   pr #20: "Call Print.MyFontBold(True)"
01840   pr #20: 'Call Print.MyFontSize(12)'
01850   pr #20: 'Call Print.MyFont("Courier New")'
01860   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01870   pr #20: 'Call Print.MyFont("Lucida Console")'
01880   pr #20: 'Call Print.MyFontSize(10)'
01890   pr #20: 'Call Print.MyFontBold(False)'
01900   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
01910   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
01920   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
01930   pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
01940   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+1+ymargin)&')'
01950   pr #20: 'Call Print.AddText("                       ",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
01960   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01970   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
01980   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
01990   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
02000   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
02010 ! ______________________________________________________________________
02020 PRINTGRID: meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02030   if g(1)=0 then goto L2040 else !:
          pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02040 L2040: if g(2)=0 then goto L2050 else !:
          pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02050 L2050: if g(3)=0 and d(7)=0 then goto L2070 else !:
          pr #20: 'Call Print.AddText("LM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02060 !  pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02070 L2070: if a4=1 then gcode$="RSGS" else !:
          if a4=2 then gcode$="CMGS" else !:
            if a4=3 then gcode$="INGS" else !:
              gcode$="GAS"
02080   if g(4)=0 then goto L2090 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02090 L2090: if g(5)=0 then goto L2100 else !:
          pr #20: 'Call Print.AddText("WSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02100 L2100: if g(6)=0 then goto L2110 else !:
          pr #20: 'Call Print.AddText("SSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02110 L2110: if g(7)=0 then goto L2120 else !:
          pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02120 L2120: if g(8)=0 then goto L2130 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02130 L2130: goto L2140 ! If G(9)=0 Then Goto 2074 Else !:
        pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
        pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02140 L2140: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
02150   if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02160   if pb=0 then goto L2170 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02170 L2170: pr #20: 'Call Print.MyFontSize(10)'
02180 ! ______________________________________________________________________
02190   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02200   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
02210   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
02220   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
02230   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
02240   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(9)+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')'
02250   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
02260   pr #20: 'Call Print.AddText("Phone: 217-762-2583",'&str$(xmargin+1)&','&str$(lyne*27+ymargin)&')'
02270 ! ______________________________________________________________________
02280   special=28
02290 ! ______________________________________________________________________
02300   pr #20: 'Call Print.MyFontSize(7)'
02310   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02330   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02340   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02350   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02360   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02370   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02380   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02390   pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
02400   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02410   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02420   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02430   pr #20: 'Call Print.AddText("  Permit No 67",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02440   pr #20: 'Call Print.MyFontSize(9)'
02450   pr #20: 'Call Print.AddText("Return Service Requested",'&str$(xmargin+77)&','&str$(lyne*7+ymargin-3)&')'
02460 ! pr #20: 'Call Print.AddText("Please return this",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN)&')'
02470   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
02480   pr #20: 'Call Print.AddText("side with payment.",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
02490   pr #20: 'Call Print.MyFontSize(10)'
02500   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02510   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02520   pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
02530   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(9)+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
02540   pr #20: 'Call Print.MyFontSize(9)'
02550   addy=14
02560   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02570   pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02580   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02590   addy+=1
02600   pr #20: 'Call Print.MyFontSize(10)'
02610   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&")"
02620   if final>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
02630   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=3)+ymargin)&')'
02640   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=2)+ymargin)&')'
02650   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02660   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02670   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02680   if checkcounter=1 then checkx=1.375 : checky=3.6875
02690   if checkcounter=2 then checkx=6.75 : checky=3.6875
02700   if checkcounter=3 then checkx=1.375 : checky=7.9375
02710   if checkcounter=0 then checkx=6.75 : checky=7.9375
02720   bc$=""
02730   if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02740   if checkcounter=0 then !:
          fnpa_newpage
02750   return 
02760 ! ______________________________________________________________________
02770 BULKSORT: ! bulk sort order
02780   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
02790   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02800 L2800: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2830
02810   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02820   goto L2800
02830 L2830: close #1: ioerr L2840
02840 L2840: close #6: ioerr L2850
02850 L2850: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2870
02860   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02870 L2870: return 
