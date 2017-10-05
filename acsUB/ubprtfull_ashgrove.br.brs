00010 ! Replace S:\acsUB\ubprtfull_ashgrove
00020 ! pr bills for Ash Grove (full page)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnmsgbox,fncmdkey,fnpa_text,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(20)*160,txt$*80,mg$(13)*160,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100   dim dueby$*30,prebal$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$) !:
        fnd1(d1)
00130   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00140   at$(1)=cnam$ !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00150   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00160   linelength=62
00170 ! 
00180   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00190   gosub BULKSORT
00200   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00210   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00220   open #ubtransvb=15: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00223   if exists(env$('Q')&"\UBmstr\message.h"&str$(cno))=0 then goto L3250
00225 L225: open #16: "Name="&env$('Q')&"\UBmstr\message.h"&str$(cno),internal,outin,relative 
00226   for j=1 to 13
00227     read #16,using "form pos 1,c 60",rec=j: mg$(j) norec L228
00228 L228: next j
00230   def fnc(x)=int(100*(x+sgn(x)*.0001))
00240 ! ______________________________________________________________________
00250   prebal$="10:00 AM, xxxxxxx  xx"
00260 SCREEN1: ! 
00270   a$="" : prtbkno=0
00280   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00290   fnlbl(3,1,"Penalty Due Date:",ll,1)
00300   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00310   fnlbl(4,1,"Message on Bill:",ll,1)
00320   fntxt(4,pf,60,60) !:
        resp$(respc+=1)=mg$(1)
00330   fntxt(5,pf,60,60) !:
        resp$(respc+=1)=mg$(2)
00340   fntxt(6,pf,60,60) !:
        resp$(respc+=1)=mg$(3)
00350   fntxt(7,pf,60,60) !:
        resp$(respc+=1)=mg$(4)
00360   fntxt(8,pf,60,60) !:
        resp$(respc+=1)=mg$(5)
00370   fntxt(9,pf,60,60) !:
        resp$(respc+=1)=mg$(6)
00380   fntxt(10,pf,60,60) !:
        resp$(respc+=1)=mg$(7)
00390   fntxt(11,pf,60,60) !:
        resp$(respc+=1)=mg$(8)
00400   fntxt(12,pf,60,60) !:
        resp$(respc+=1)=mg$(9)
00410   fntxt(13,pf,60,60) !:
        resp$(respc+=1)=mg$(10)
00411   fntxt(14,pf,60,60) !:
        resp$(respc+=1)=mg$(11)
00412   fntxt(15,pf,60,60) !:
        resp$(respc+=1)=mg$(12)
00413   fntxt(16,pf,60,60) !:
        resp$(respc+=1)=mg$(13)
00420   fnlbl(17,1,"Date of Billing:",ll,1)
00430   fntxt(17,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00440   fnlbl(18,1,"Starting Account:",ll,1)
00450   fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,18,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00460   fnlbl(19,1,"Route Number:",ll,1)
00470   fncmbrt2(19,pf) !:
        resp$(respc+=1)="[All]"
00480   fnchk(20,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00490 ! fnLBL(18,1,"Previous Balance Due By:",LL,1)
00500 ! fnTXT(18,PF,35,35,0,"",0,"Example: 10:00AM, August 30") !:
        ! rESP$(RESPC+=1)=PREBAL$
00510 ! fnLBL(19,1,"If not paid by::",LL,1)
00520 !  fnTXT(19,PF,25,25,0,"",0,"Example: September 4, 2007") !:
        !  rESP$(RESPC+=1)=DUEBY$
00530   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00540   if ck=5 then goto ENDSCR
00550   d1 = val(resp$(15)) !:
        d4 = val(resp$(1)) !:
        mg$(1) = resp$(2) !:
        mg$(2) = resp$(3) !:
        mg$(3) = resp$(4) !:
        mg$(4) = resp$(5) !:
        mg$(5) = resp$(6) !:
        mg$(6) = resp$(7) !:
        mg$(7) = resp$(8) !:
        mg$(8) = resp$(9)
00560   mg$(9) = resp$(10) !:
        mg$(10) = resp$(11) !:
        mg$(11) = resp$(12) !:
        mg$(12) = resp$(13) !:
        mg$(13) = resp$(14)
00565   for j=1 to 13
00566     rewrite #16,using "form pos 1,c 60",rec=j: mg$(j) norec L568 : lastj=j
00567   next j
00568 L568: if lastj<13 then 
00569     for j=lastj+1 to 13
00570       write #16,using "form pos 1,c 60": mg$(j)
00571     next j
00572   end if 
00574   close #16: 
00575   if resp$(16)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(16)(1:9)),9)
00580   if resp$(17)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(17))
00590   if resp$(18)="True" then sl1=1: z$="" else sl1=0
00600   prebal$=resp$(19)
00610   dueby$=resp$(20)
00620   goto L640 ! If TRIM$(PREBAL$)="" OR TRIM$(DUEBY$)="" Then Goto 550 Else Goto 560
00630   mat ml$(2) !:
        ml$(1)="You must answer the last two questions!" !:
        ml$(2)="Click OK to enter this informatio." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto SCREEN1
00640 L640: if trim$(a$)<>"" then read #2,using L650,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00650 L650: form pos 1,c 10,pos 1741,n 2,n 7
00660   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00670   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00680   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00690 ! ______________________________________________________________________
00700   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00710   gosub VBOPENPRINT
00720 ! ______________________________________________________________________
00730   on fkey 5 goto RELEASE_PRINT
00740 L740: if sl1=1 then goto SCREEN3
00750 ! Read #6,Using 780: Z$ Eof 1040
00760 ! If TRIM$(A$)<>"" AND BEGIN=1 AND Z$<>HOLDZ$ Then Goto 750 ! start with
00770 ! bEGIN=0 ! cancel starting account
00780   form pos 22,c 10
00790 L790: read #1,using L800: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg eof RELEASE_PRINT
00800 L800: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,pos 1822,n 9
00810   if prtbkno=0 then goto L830
00820   if prtbkno><route then goto L790
00830 L830: if f><d1 then goto L740
00840   if st1=0 then goto READALTADR
00850 READALTADR: ! 
00860 ! read alternate billing address
00870   read #3,using L880,key=z$: mat ba$ nokey L970
00880 L880: form pos 11,4*c 30
00890   e1=0 : mat pe$=("")
00900   for j=1 to 4
00910     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00920   next j
00930   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00940   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00950   goto L1120
00960 ! ______________________________________________________________________
00970 L970: e1=0 : mat pe$=("")
00980   for j=2 to 4
00990     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
01000   next j
01010   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
01020   goto L1120
01030 ! ______________________________________________________________________
01040 RELEASE_PRINT: ! 
01050   close #1: ioerr L1060
01060 L1060: close #3: ioerr L1070
01070 L1070: fnpa_finis
01100   goto ENDSCR
01110 ! ______________________________________________________________________
01120 L1120: ! 
01130   pb=bal-g(11)
01140   if bal<=0 then g(9)=g(10)=0 ! don't show penalty if balance 0 or less
01150 ! ______________print bill routine______________________________________
01160   gosub VBPRINT
01170 ! _____________end of pr routine______________________________________
01180   bct(2)=bct(2)+1 !:
        ! accumulate totals
01190   goto L740
01200 ! ______________________________________________________________________
01210 SCREEN3: ! 
01220   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01230   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01240 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01250   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01260   fncmbact(1,17) ! !:
        resp$(1)=""
01270   fncmdkey("&Print",1,1) !:
        fncmdkey("&Finish",5,0,1)
01280   fnacs(sn$,0,mat resp$,ck)
01290   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01300   if ck=5 then goto RELEASE_PRINT
01310   read #1,using L800,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg nokey SCREEN3
01320   goto READALTADR
01330 ! ______________________________________________________________________
01340 SORT1: ! SELECT & SORT
01350   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1600
01360   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01370   s5=1
01380   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01390   restore #2,search>=routekey$: 
01400 L1400: read #2,using L1410: z$,f,route eof END5
01410 L1410: form pos 1,c 10,pos 296,pd 4,pos 1741
01420   if prtbkno=0 then goto L1440
01430   if prtbkno><route then goto END5
01440 L1440: if f><d1 then goto L1400
01450   zip5$=cr$=""
01460   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1470
01470 L1470: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01480   goto L1400
01490 ! ______________________________________________________________________
01500 END5: close #6: 
01510   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01520 L1520: form pos 1,c 128
01530   write #9,using L1520: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01540   write #9,using L1520: "Mask 1,19,C,A"
01550   close #9: 
01560   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1570
01570 L1570: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01580   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01590   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01600 L1600: return 
01610 ! ______________________________________________________________________
01620 ENDSCR: ! pr totals screen
01630   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01640   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01650   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01660   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01670 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01680 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01690 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01700 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01710 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01720 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01730   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01740 XIT: fnxit
01750 ! ______________________________________________________________________
01760 ERTN: fnerror(program$,err,line,act$,"xit")
01770   if uprc$(act$)<>"PAUSE" then goto L1800
01780   execute "list -"&str$(line) !:
        pause  !:
        goto L1800
01790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01800 L1800: execute act$
01810   goto ERTN
01820 ! ______________________________________________________________________
01830 VBOPENPRINT: ! 
01850     fnpa_open
01880     lyne=3
01910   return 
01920 ! ______________________________________________________________________
01930 VBPRINT: ! 
01940 ! -- Printer Program for Laser 1-Per Page Utility Bills
01950 ! ______________________________________________________________________
01960 ! pr #20: 'Call Print.AddPicture("Ash Grove.jpg",20,1)'
01970   addy=20
01980   pr #20: 'Call Print.MyFontSize(12)'
01990   if pe$(1)<>"" then !:
          txt$=trim$(pe$(1)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(52)&')'
02000   if pe$(2)<>"" then !:
          txt$=trim$(pe$(2)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(57)&')'
02010 ! 
02020   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
02030   if pe$(3)<>"" then !:
          txt$=trim$(pe$(3)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(62)&')'
02040   if pe$(4)<>"" then !:
          txt$=trim$(pe$(4)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(67)&')'
02050   pr #20: 'Call Print.MyFontSize(24)'
02060   pr #20: 'Call Print.AddText("'&"City of Ash Grove"&'",'&str$(65)&','&str$(10)&')'
02070   pr #20: 'Call Print.MyFontSize(12)'
02080   pr #20: 'Call Print.AddText("'&"P O Box 235"&'",'&str$(93)&','&str$(19)&')'
02090   pr #20: 'Call Print.AddText("'&"Ash Grove, Mo 65604"&'",'&str$(83)&','&str$(23)&')'
02100 ! 
02110   pr #20: 'Call Print.MyFontSize(10)'
02120   pr #20: 'Call Print.AddText("'&"417-751-2333"&'",'&str$(93)&','&str$(27)&')'
02130   pr #20: 'Call Print.MyFontSize(9)'
02140   pr #20: 'Call Print.AddLine('&str$(135)&','&str$(40)&',52,22,1)'
02150   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(140)&','&str$(42)&')'
02160   txt$="       Account:"&trim$(z$) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(140)&','&str$(46)&')'
02170   pr #20: 'Call Print.AddText("Service From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'",'&str$(140)&','&str$(50)&')'
02180   pr #20: 'Call Print.AddText("Service To: '&cnvrt$("PIC(zzZZ/ZZ/ZZ)",d3)&'",'&str$(140)&','&str$(54)&')'
02190   pr #20: 'Call Print.AddText("Due Date: '&cnvrt$("PIC(zzzzZZ/ZZ/ZZ)",d4)&'",'&str$(140)&','&str$(58)&')'
02200   if final>0 then pr #20: 'Call Print.AddText("Final Bill'&cnvrt$("PIC(ZZzZZzZZ)",0)&'",'&str$(82)&','&str$(56)&')'
02210   pr #20: 'Call Print.MyFontSize(16)'
02220   pr #20: 'Call Print.AddLine('&str$(19)&','&str$(70)&',166,0)'
02230 ! 
02240   pr #20: 'Call Print.MyFontSize(10)'
02250   pr #20: 'Call Print.MyFontItalic(1)'
02260   if pb<0 then a$=cnvrt$("pic(--------.##)",pb) !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(110)&','&str$(72)&')': goto L2280
02270   if pb=0 then goto L2300 else !:
          a$=cnvrt$("pic(--------.##)",pb) !:
          pr #20: 'Call Print.AddText("Past Due Balance - Due Immediately",'&str$(80)&','&str$(72)&')'
02280 L2280: pr #20: 'Call Print.MyFontItalic(0)'
02290   pr #20: 'Call Print.AddText("'&a$&'",'&str$(160)&','&str$(72)&')'
02300 L2300: ! pr #20: 'Call Print.AddLine('&STR$(109)&','&STR$(115)&',65,0)'
02310   pr #20: 'Call Print.MyFontItalic(1)'
02320   pr #20: 'Call Print.MyFontBold(1)'
02330   txt$=trim$(e$(1)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(135)&','&str$(65)&')'
02340   pr #20: 'Call Print.AddText("Current Charges",'&str$(110)&','&str$(78)&")"
02350   pr #20: 'Call Print.MyFontBold(0)'
02360 ! 
02370   pr #20: 'Call Print.MyFontItalic(0)'
02380   pr #20: 'Call Print.AddText("Current Reading",'&str$(56)&','&str$(84)&")"
02390   pr #20: 'Call Print.AddText("Prior Reading",'&str$(93)&','&str$(84)&")"
02400   pr #20: 'Call Print.AddText("Usage",'&str$(128)&','&str$(84)&')'
02410   pr #20: 'Call Print.AddText("Charge",'&str$(170)&','&str$(84)&')'
02420   adder=5: lyne=85
02430   if g(1)=0 then goto L2450 else !:
          a$=cnvrt$("pic(zzzzzzzz#)",d(1)) !:
          b$=cnvrt$("pic(zzzzzzzz#)",d(3)) !:
          c$=cnvrt$("pic(--------.##)",g(1)) !:
          d$=cnvrt$("pic(zzzzzzzz#)",d(2))
02440   pr #20: 'Call Print.AddText("Water",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&a$&'",'&str$(70)&','&str$(lyne)&')' !:
        pr #20: 'Call Print.AddText("'&d$&'",'&str$(102)&','&str$(lyne)&')' !:
        pr #20: 'Call Print.AddText("'&b$&'",'&str$(121)&','&str$(lyne)&')' !:
        !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02450 L2450: if g(2)=0 then goto L2470 else !:
          c$=cnvrt$("pic(--------.##)",g(2))
02460   pr #20: 'Call Print.AddText("Sewer",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02470 L2470: if g(4)=0 then goto L2500 else !:
          c$=cnvrt$("pic(--------.##)",g(4))
02480 ! 
02490   pr #20: 'Call Print.AddText("Sewer Fee",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02500 L2500: if g(5)=0 then goto L2520 else !:
          c$=cnvrt$("pic(--------.##)",g(5))
02510   pr #20: 'Call Print.AddText("Primacy Fee",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02520 L2520: if g(6)=0 then goto L2540 else !:
          c$=cnvrt$("pic(--------.##)",g(6))
02530   pr #20: 'Call Print.AddText("Trash Charge",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02540 L2540: if g(8)=0 then goto L2560 else !:
          c$=cnvrt$("pic(--------.##)",g(8))
02550   pr #20: 'Call Print.AddText("Other Charge",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02560 L2560: if g(9)=0 then goto L2580 else !:
          c$=cnvrt$("pic(--------.##)",g(9))
02570   pr #20: 'Call Print.AddText("Sales Tax",'&str$(26)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
02580 L2580: pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
02590   a$=cnvrt$("pic(--------.##)",g(11)) !:
        pr #20: 'Call Print.AddText("Total Current Charges",'&str$(110)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&a$&'",'&str$(160)&','&str$(lyne)&')'
02600   pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
02610   pr #20: 'Call Print.MyFontSize(14)'
02620   pr #20: 'Call Print.MyFontBold(1)'
02630   c$=cnvrt$("pic(--------.##)",bal) !:
        pr #20: 'Call Print.AddText("Total Due",'&str$(120)&','&str$(lyne+=adder)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(150)&','&str$(lyne)&')'
02640   pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
02650   pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=1)&',22,0)'
02660   pr #20: 'Call Print.MyFontBold(1)'
02670   pr #20: 'Call Print.AddLine('&str$(26)&','&str$(125)&',160,55,1)'
02680   pr #20: 'Call Print.MyFontBold(0)'
02690   pr #20: 'Call Print.MyFontBold(0)'
02700   pr #20: 'Call Print.MyFontSize(10)'
02705   pr #20: 'Call Print.AddText("'&"Notes From City"&'",'&str$(90)&','&str$(123.5)&')'
02710   pr #20: 'Call Print.MyFontItalic(1)'
02720   lyne=124
02740   for j=1 to 13
02750     fnpa_text(20,mg$(j),40,lyne+=4) ! pr #20: 'Call Print.AddText("'&mg$(j)&'",'&str$(40)&','&str$(lyne+=4)&')'
02760   next j
02770   pr #20: 'Call Print.MyFontItalic(0)'
02780   x=0
02790 ! For J=1 To 38
02800 ! pr #20: 'Call Print.AddLine('&STR$(X+=5)&','&STR$(208)&',3,0)'
02810 ! Next J
02820   pr #20: 'Call Print.MyFontSize(7)'
02830   pr #20: 'Call Print.AddText("Please detach here and return with payment.",'&str$(70)&','&str$(191)&')'
02840   pr #20: 'Call Print.AddText("Make checks payable to City of Ash Grove.",'&str$(72)&','&str$(195)&')'
02850   pr #20: 'Call Print.AddText("A 10% penalty if not paid by the due date.",'&str$(72)&','&str$(199)&')'
02860   pr #20: 'Call Print.MyFontSize(10)'
02870   txt$="        Account:"&trim$(z$) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(223)&')'
02880   pr #20: 'Call Print.AddText("Due Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(130)&','&str$(227)&')'
02890   c$=cnvrt$("pic(--------.##)",bal) !:
        pr #20: 'Call Print.AddText("Total Due:",'&str$(130)&','&str$(231)&')' !:
        pr #20: 'Call Print.AddText("'&c$&'",'&str$(171)&','&str$(231)&')'
02900   txt$="After "&cnvrt$("pic(##/##/##)",d4)&" Pay: "&cnvrt$("pic(-------.##)",bal+g(10)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(235)&')'
02910   if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(.82)&','&str$(2.75)&',"'&bc$&'")'
02920   pr #20: 'Call Print.AddText("'&"City of Ash Grove"&'",'&str$(30)&','&str$(246)&')'
02930   pr #20: 'Call Print.AddText("'&"P O Box 235"&'",'&str$(30)&','&str$(250)&')'
02940   pr #20: 'Call Print.AddText("'&"Ash Grove, Mo 65604"&'",'&str$(30)&','&str$(254)&')'
02950   if pe$(1)<>"" then !:
          txt$=trim$(pe$(1)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(248)&')'
02960   if pe$(2)<>"" then !:
          txt$=trim$(pe$(2)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(252)&')'
02970   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
02980   if pe$(3)<>"" then !:
          txt$=trim$(pe$(3)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(256)&')'
02990   if pe$(4)<>"" then !:
          txt$=trim$(pe$(4)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(260)&')'
03000   fnpa_newpage
03010   return 
03020 BULKSORT: ! bulk sort order
03030   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
03040   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03050 L3050: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3080
03060   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03070   goto L3050
03080 L3080: close #1: ioerr L3090
03090 L3090: close #6: ioerr L3100
03100 L3100: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3120
03110   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
03120 L3120: return 
03130 PRIOR_USAGES: ! 
03140   mat usage=(0): mat billdate=(0)
03150   restore #15,key>=z$&"         ": nokey L3240 ! no average but active customer (use 0 usage)
03160 L3160: read #ubtransvb,using L3170: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L3240
03170 L3170: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
03180   if p$<>z$ then goto L3240
03190   if tcode<>1 then goto L3160 ! only charge transactions
03200   usage(3)=usage(2): billdate(3)=billdate(2)
03210   usage(2)=usage(1): billdate(2)=billdate(1)
03220   usage(1)=wu: billdate(1)=tdate
03230   goto L3160
03240 L3240: return 
03250 L3250: open #16: "Name="&env$('Q')&"\UBmstr\message.h"&str$(cno)&",RecL=132,replace",internal,outin,relative 
03255   for j=1 to 10
03256     write #16,using "form pos 1,c 60": "" ! write 10 blank messages
03257   next j
03258   close #16: 
03260   goto L225
