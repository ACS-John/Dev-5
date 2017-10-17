00010 ! Replace S:\acsUB\ubprtbl1_kin
00020 ! pr bills for Village of Kincaid  (4 part plain paper)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_newpage,fnpa_open
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00120   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
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
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00210   def fnc(x)=int(100*(x+sgn(x)*.0001))
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   a$="" : prtbkno=0
00242   mg$(1)='Temp Return Service Requested'
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
        datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
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
00490 ! If TRIM$(A$)="" AND PRTBKNO>0 Then Restore #2,Key>=CNVRT$("pic(zz)",PRTBKNO)&"       ": ! selected a route and no beginning Account
00500 ! ______________________________________________________________________
00510   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00520   gosub VBOPENPRINT
00530 ! ______________________________________________________________________
00540   on fkey 5 goto RELEASE_PRINT
00550 L550: if sl1=1 then goto SCREEN3
00560 L560: read #6,using L590: z$ eof RELEASE_PRINT
00570   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
00580   begin=0 ! cancel starting account
00590 L590: form pos 22,c 10
00600   read #1,using L620,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L560
00610   if prtbkno>0 and route<>prtbkno then goto L560
00620 L620: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
00630   if prtbkno=0 then goto L650
00640   if prtbkno><route then goto L560
00650 L650: if f><d1 then goto L550
00660   if st1=0 then goto READALTADR
00670 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00680 READALTADR: ! 
00690 ! read alternate billing address
00700   read #3,using L710,key=z$: mat ba$ nokey L800
00710 L710: form pos 11,4*c 30
00720   e1=0 : mat pe$=("")
00730   for j=1 to 4
00740     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00750   next j
00760   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00770   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00780   goto L950
00790 ! ______________________________________________________________________
00800 L800: e1=0 : mat pe$=("")
00810   for j=2 to 4
00820     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00830   next j
00840   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00850   goto L950
00860 ! ______________________________________________________________________
00870 RELEASE_PRINT: ! 
00880   close #1: ioerr L890
00890 L890: close #3: ioerr L900
00900 L900: fnpa_finis
00930   goto ENDSCR
00940 ! ______________________________________________________________________
00950 L950: ! 
00960   pb=bal-g(11)
00970   if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
00980 ! ______________print bill routine______________________________________
00990   gosub VBPRINT
01000 ! _____________end of pr routine______________________________________
01010   bct(2)=bct(2)+1 !:
        ! accumulate totals
01020   goto L550
01030 ! ______________________________________________________________________
01040 SCREEN3: ! 
01050   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01060   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01070 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01080   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01090   fncmbact(1,17) ! !:
        resp$(1)=a$
01100   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
01110   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01120   if ck=5 then goto RELEASE_PRINT
01130   read #1,using L620,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01140   goto READALTADR
01150 ! ______________________________________________________________________
01160 SORT1: ! SELECT & SORT
01170   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1420
01180   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01190   s5=1
01200   routekey$="" ! If PRTBKNO=0 Then rOUTEKEY$="" Else !:
        ! rOUTEKEY$=CNVRT$("N 2",PRTBKNO)&"       " !:
        ! key off first record in route (route # no longer part of customer #)
01210   restore #2,search>=routekey$: 
01220 L1220: read #2,using L1230: z$,f,route eof END5
01230 L1230: form pos 1,c 10,pos 296,pd 4,pos 1741
01240   if prtbkno=0 then goto L1260
01250   if prtbkno><route then goto L1220
01260 L1260: if f><d1 then goto L1220
01270   zip5$=cr$=""
01280   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1290
01290 L1290: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01300   goto L1220
01310 ! ______________________________________________________________________
01320 END5: close #6: 
01330   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01340 L1340: form pos 1,c 128
01350   write #9,using L1340: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01360   write #9,using L1340: "Mask 1,19,C,A"
01370   close #9: 
01380   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1390
01390 L1390: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01400   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01410   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01420 L1420: return 
01430 ! ______________________________________________________________________
01440 ENDSCR: ! pr totals screen
01450   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01460   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01470   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01480   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01490 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01500 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01510 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01520 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01530 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01540 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01550   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01560 XIT: fnxit
01570 ! ______________________________________________________________________
01580 ERTN: fnerror(program$,err,line,act$,"xit")
01590   if uprc$(act$)<>"PAUSE" then goto L1620
01600   execute "list -"&str$(line) !:
        pause  !:
        goto L1620
01610   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01620 L1620: execute act$
01630   goto ERTN
01640 ! ______________________________________________________________________
01650 VBOPENPRINT: ! 
01670     fnPa_open("Landscape")
01700     lyne=3
01730   return 
01740 ! ______________________________________________________________________
01750 VBPRINT: ! 
01760 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01770   checkcounter+=1
01780   if checkcounter=1 then xmargin=0 : ymargin=0
01790   if checkcounter=2 then xmargin=139 : ymargin=0
01800   if checkcounter=3 then xmargin=0 : ymargin=108
01810   if checkcounter=4 then xmargin=139 : ymargin=108 !:
          checkcounter=0
01820 ! ______________________________________________________________________
01830   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01840   pr #20: "Call Print.MyFontBold(True)"
01850   pr #20: 'Call Print.MyFontSize(12)'
01860   pr #20: 'Call Print.MyFont("Courier New")'
01870   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01880   pr #20: 'Call Print.MyFont("Lucida Console")'
01890   pr #20: 'Call Print.MyFontSize(10)'
01900   pr #20: 'Call Print.MyFontBold(False)'
01910   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
01920   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
01930   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
01940   pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
01950   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+1+ymargin)&')'
01960   pr #20: 'Call Print.AddText("                       ",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
01970   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01980   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
01990   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
02000   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
02010   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
02020 ! ______________________________________________________________________
02030 PRINTGRID: meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02040   if g(1)=0 then goto L2050 else !:
          pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02050 L2050: if g(2)=0 then goto L2060 else !:
          pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02060 L2060: if g(3)=0 and d(7)=0 then goto L2080 else !:
          pr #20: 'Call Print.AddText("LM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02070 !  pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02080 L2080: if a4=1 then gcode$="RSGS" else !:
          if a4=2 then gcode$="CMGS" else !:
            if a4=3 then gcode$="INGS" else !:
              gcode$="GAS"
02090   if g(4)=0 then goto L2100 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02100 L2100: if g(5)=0 then goto L2110 else !:
          pr #20: 'Call Print.AddText("WSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02110 L2110: if g(6)=0 then goto L2120 else !:
          pr #20: 'Call Print.AddText("SSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02120 L2120: if g(7)=0 then goto L2130 else !:
          pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02130 L2130: if g(8)=0 then goto L2140 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02140 L2140: if g(9)=0 then goto L2150 else !:
          pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02150 L2150: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
02160   if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02170   if pb=0 then goto L2180 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02180 L2180: pr #20: 'Call Print.MyFontSize(10)'
02190 ! ______________________________________________________________________
02200   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*20+ymargin)&')'
02210   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*22+1+ymargin)&',63,0)'
02220   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*23+ymargin)&')'
02230   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*23+ymargin)&')'
02240   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
02250   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
02260   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*25+1+ymargin)&',63,0)'
02270   pr #20: 'Call Print.AddText("Phone: 217-237-4713",'&str$(xmargin+1)&','&str$(lyne*26+ymargin)&')'
02271   pr #20: "Call Print.MyFontBold(True)"
02272   pr #20: 'Call Print.AddText("Shut off notices will",'&str$(xmargin+8)&','&str$(lyne*27+ymargin)&')'
02273   pr #20: 'Call Print.AddText("no longer be mailed.",'&str$(xmargin+10)&','&str$(lyne*28+ymargin)&')'
02274   pr #20: 'Call Print.AddText("Last Tuesday of the month",'&str$(xmargin+8)&','&str$(lyne*29+ymargin)&')'
02275   pr #20: 'Call Print.AddText("at 9 AM water will be",'&str$(xmargin+8)&','&str$(lyne*30+ymargin)&')'
02276   pr #20: 'Call Print.AddText("disconnected if unpaid.",'&str$(xmargin+8)&','&str$(lyne*31+ymargin)&')'
02277   pr #20: 'Call Print.MyFontBold(False)'
02280 ! ______________________________________________________________________
02290   special=28
02300 ! ______________________________________________________________________
02310   pr #20: 'Call Print.MyFontSize(7)'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02330   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02340   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02350   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02360   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02370   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02380   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02390   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02400   pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
02410   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02420   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02430   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02440   pr #20: 'Call Print.AddText("  Permit No 13",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02450   pr #20: 'Call Print.MyFontSize(9)'
02460 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02470   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
02480   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
02490   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
02500   pr #20: 'Call Print.MyFontSize(10)'
02510   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02520   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02530   pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
02540   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
02550   pr #20: 'Call Print.MyFontSize(9)'
02560   addy=14
02570   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02580   pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02590   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02600   addy+=1
02610   pr #20: 'Call Print.MyFontSize(10)'
02620   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02630   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02640   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02650   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=2)+ymargin)&')'
02660   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02670   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02680   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02690   if checkcounter=1 then checkx=1.375 : checky=3.6875
02700   if checkcounter=2 then checkx=6.75 : checky=3.6875
02710   if checkcounter=3 then checkx=1.375 : checky=7.9375
02720   if checkcounter=0 then checkx=6.75 : checky=7.9375
02730 ! bc$=""
02740 ! if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02750   if checkcounter=0 then !:
          fnpa_newpage
02760   return 
02770 ! ______________________________________________________________________
02780 BULKSORT: ! bulk sort order
02790   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02800   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02810 L2810: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2840
02820   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02830   goto L2810
02840 L2840: close #1: ioerr L2850
02850 L2850: close #6: ioerr L2860
02860 L2860: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2880
02870   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02880 L2880: return 
