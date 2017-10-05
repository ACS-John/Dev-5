00010 ! Replace S:\acsUB\ubprtthree_Carrizo
00020 ! pr three per page prace fill in blanks for Carrizo Springs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnmsgbox,fnpa_finis,fnpa_newpage,fnpa_open
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(12)*60,txt$*100,mg$(3)*60,rw(22,13),cap$*128,fb$(3)*60
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100   dim servicename$(10)*20,service$(10)*2, rt(10,3)
00110   dim dueby$*30,prebal$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
00120 ! ______________________________________________________________________
00130   fncno(cno,cnam$) !:
        fnd1(d1)
00140   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00150   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00160   at$(1)=cnam$ !:
        let z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00170   let z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00180   linelength=62
00190 ! 
00200   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00210   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00220   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00230   open #ubtransvb=15: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00240   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form POS 1,10*C 20,10*C 2',rec=1: mat servicename$,mat service$ !:
        close #20: 
00250   def fnc(x)=int(100*(x+sgn(x)*.0001))
00260 ! ______________________________________________________________________
00270   prebal$="10:00 AM, xxxxxxx  xx"
00280 SCREEN1: ! 
00290   a$="" : prtbkno=0
00300   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00310   fnlbl(1,1,"Current Reading Date:",ll,1)
00320   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00330   fnlbl(2,1,"Previous Reading Date:",ll,1)
00340   fntxt(2,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00350   fnlbl(3,1,"Penalty Due Date:",ll,1)
00360   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00370   fnlbl(4,1,"Message on Bill:",ll,1)
00380   fntxt(4,pf,60,60) !:
        resp$(respc+=1)=mg$(1)
00390   fntxt(5,pf,60,60) !:
        resp$(respc+=1)=mg$(2)
00400   fntxt(6,pf,60,60) !:
        resp$(respc+=1)=mg$(3)
00410   fnlbl(7,1,"Date of Billing:",ll,1)
00420   fntxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00430   fnlbl(8,1,"Starting Account:",ll,1)
00440   let fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00450   fnlbl(9,1,"Route Number:",ll,1)
00460   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00470   fnchk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00480   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00490   if ck=5 then goto ENDSCR
00500   d2x= val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6) !:
        d1 = val(resp$(7))
00510   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00520   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00530   if resp$(10)="True" then sl1=1: let z$="" else sl1=0
00540   if trim$(a$)<>"" then read #2,using L550,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00550 L550: form pos 1,c 10,pos 1741,n 2,n 7
00560   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00570   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00580   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00590 ! Gosub BULKSORT
00600 ! ______________________________________________________________________
00610   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00620   fnpa_open
00630 ! ______________________________________________________________________
00640   on fkey 5 goto RELEASE_PRINT
00650 L650: if sl1=1 then goto SCREEN3
00660 ! Read #6,Using 690: BC$,Z$ Eof 990
00670 ! If TRIM$(A$)<>"" AND BEGIN=1 AND Z$<>HOLDZ$ Then Goto 660 ! start with
00680   begin=0 ! cancel starting account
00690   form pos 1,c 12,c 10
00700   read #1,using L720: z$,mat e$,f$,a1,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ eof RELEASE_PRINT
00710   if estimatedate>0 then est=1 else est=0
00720 L720: form pos 1,c 10,4*c 30,c 12,pos 143,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,c 9,n 2,c 17
00730   if prtbkno=0 then goto L750
00740   if prtbkno><route then goto RELEASE_PRINT
00750 L750: if f><d1 then goto L650
00760 L760: e1=0 : mat pe$=("")
00770   for j=2 to 4
00780     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00790   next j
00800   if st1=0 then goto READALTADR
00810 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00820 READALTADR: ! 
00830 ! read alternate billing address
00840   read #3,using L860,key=z$: mat ba$ nokey L950
00850   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L950
00860 L860: form pos 11,4*c 30
00870   e1=0 : mat pe$=("")
00880   for j=1 to 4
00890     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00900   next j
00910   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00920   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00930   goto L1070
00940 ! ______________________________________________________________________
00950 L950: ! 
00960   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00970   goto L1070
00980 ! ______________________________________________________________________
00990 RELEASE_PRINT: ! 
01000   close #1: ioerr L1010
01010 L1010: close #3: ioerr L1020
01020 L1020: fnpa_finis
01050   goto ENDSCR
01060 ! ______________________________________________________________________
01070 L1070: ! 
01080   pb=bal-g(11)
01090   if bal<=0 then let g(9)=g(10)=0 ! don't show penalty if balance 0 or less
01100   let fb$(1)=mg$(1)
01110   let fb$(2)=mg$(2)
01120   let fb$(3)=mg$(3)
01130 ! If C4>0 Then Let FB$(1)="          Final Bill" : Let FB$(2)="": Let FB$(3)=""
01140 ! ______________print bill routine______________________________________
01150   gosub VBPRINT
01160 ! _____________end of pr routine______________________________________
01170   bct(2)=bct(2)+1 !:
        ! accumulate totals
01180   goto L650
01190 ! ______________________________________________________________________
01200 SCREEN3: ! 
01210   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01220   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01230 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01240   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01250   fncmbact(1,17) ! !:
        resp$(1)=a$
01260   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
01270   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01280   if ck=5 then goto RELEASE_PRINT
01290   read #1,using L720,key=a$: z$,mat e$,f$,a1,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ nokey SCREEN3
01300   goto L760
01310 ! ______________________________________________________________________
01320 SORT1: ! SELECT & SORT
01330   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1580
01340   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=22",internal,output 
01350   s5=1
01360   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01370   restore #2,search>=routekey$: 
01380 L1380: read #2,using L1390: z$,f,route eof END5
01390 L1390: form pos 1,c 10,pos 296,pd 4,pos 1741,n 2
01400   if prtbkno=0 then goto L1420
01410   if prtbkno><route then goto END5
01420 L1420: if f><d1 then goto L1380
01430   cr$=""
01440   read #5,using "Form POS 96,C 12",key=z$: cr$ nokey L1450
01450 L1450: write #6,using "Form POS 1,C 12,C 10": cr$,z$
01460   goto L1380
01470 ! ______________________________________________________________________
01480 END5: close #6: 
01490   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01500 L1500: form pos 1,c 128
01510   write #9,using L1500: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01520   write #9,using L1500: "Mask 1,19,C,A"
01530   close #9: 
01540   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1550
01550 L1550: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01560   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01570   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01580 L1580: return 
01590 ! ______________________________________________________________________
01600 ENDSCR: ! pr totals screen
01610   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01620   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01630   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01640   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01650 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01660 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01670 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01680 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01690 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01700 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01710   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01720 XIT: fnxit
01730 ! ______________________________________________________________________
01740 ERTN: fnerror(program$,err,line,act$,"xit")
01750   if uprc$(act$)<>"PAUSE" then goto L1780
01760   execute "list -"&str$(line) !:
        pause  !:
        goto L1780
01770   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01780 L1780: execute act$
01790   goto ERTN
01800 ! ______________________________________________________________________
01900 VBPRINT: ! 
01910 ! -- Printer Program for three per page  Utility Bills
01920   d2=d3x ! If D2=0 Then d2=D3X
01930   d3=d2x ! If D3=0 Then d3=D2X
01940 ! Gosub PRIOR_USAGES
01950   column1=41 !:
        column2=79 !:
        column3=117 !:
        column4=152
01960   column5=88.5 ! charge column
01970   pr #20: 'Call Print.MyFontSize(10)'
01980   txt$=z$
01990   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(33)&','&str$(lyne+=4.5)&')'
02000 ! tXT$=CNVRT$("pic(zz/zz/zz",D4)
02010 !    pr #20: 'Call Print.AddText("'&TXT$&'",'&STR$(90)&','&STR$(LYNE)&')'
02020   lyne+=7 !:
        txt$=cnvrt$("pic(zz/zz/zz",d2) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(52)&','&str$(lyne)&')'
02030   txt$=cnvrt$("pic(zz/zz/zz",d3) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
02040   txt$=cnvrt$("pic(---,---.##)",g(11)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
02050   txt$=cnvrt$("pic(---,---.##)",bal) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
02055   lyne+=9
02060   if g(1)>0 then de$=service$(1) else de$="  " : goto L2090
02070   txt$=de$& cnvrt$("pic(zzzzzzz)",d(2))&cnvrt$("pic(zzzzzzz)",d(1))&cnvrt$("pic(zzzzz#)",d(3)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02080   txt$=cnvrt$("pic(zzzzz.zz)",g(1)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02090 L2090: txt$=z$&"       "&serviceid$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
02100   if g(2)>0 then de$=service$(2) else de$="  " : goto L2140
02110   if bal>0 then penbal=bal+round(bal*.10,2) else penbal=0
02120   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02130   txt$=cnvrt$("pic(-----.--)",g(2)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02140 L2140: if g(3)>0 then de$=service$(3) else de$="  " : goto L2170
02150   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne)&')'
02160   txt$=cnvrt$("pic(-----.--)",g(3)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02170 L2170: if g(4)>0 then de$=service$(4) else de$=" " : goto L2200
02180   lyne+=4 !:
        txt$=de$& cnvrt$("pic(zzzzzzz)",d(10))&cnvrt$("pic(zzzzzzz)",d(9))&cnvrt$("pic(zzzzz#)",d(11)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02190   txt$=cnvrt$("pic(-----.--)",g(4)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02200 L2200: if g(5)>0 then de$=service$(5) else de$=" " : goto L2230
02210   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02220   txt$=cnvrt$("pic(-----.--)",g(5)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02230 L2230: if g(6)>0 then de$=service$(6) else de$=" " : goto L2260
02240   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02250   txt$=cnvrt$("pic(-----.--)",g(6)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02260 L2260: if g(7)>0 then de$=service$(7) else de$=" " : goto L2290
02270   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02280   txt$=cnvrt$("pic(-----.--)",g(7)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02290 L2290: if g(8)>0 then de$=service$(8) else de$=" " : goto L2320
02300   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02310   txt$=cnvrt$("pic(-----.--)",g(8)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02320 L2320: if g(9)>0 then de$=service$(9) else de$=" " : goto L2350
02330   txt$=de$ !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02340   txt$=cnvrt$("pic(-----.--)",g(9)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column5)&','&str$(lyne)&')'
02350 L2350: ! 
02360   if lyne<90 then lyne=65.5: goto L2390 ! allow for three forms
02370   if lyne<=180 then lyne=158: goto L2390
02380   if lyne>180 then lyne=251
02390 L2390: txt$=cnvrt$("pic(-----.--)",pb) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02400   txt$=cnvrt$("pic(-----.--)",g(11)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+25)&','&str$(lyne)&')'
02410   txt$=cnvrt$("pic(-----.--)",bal) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+50)&','&str$(lyne)&')'
02420   txt$=cnvrt$("pic(zz/zz/zz)",d4) !:
        lyne+=8 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
02430   txt$=e$(1) !:
        lyne+=4 !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(60)&','&str$(lyne)&')'
02440   if est=1 then de2$="ESTIMATED" : goto L2470 else de2$="              "
02450   if final>0 then de2$="Final Bill": goto L2470 else de2$="               "
02460   if df$="Y" then de2$="DRAFTED": goto L2470 else de2$="               "
02470 L2470: txt$=de2$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1+50)&','&str$(lyne-4)&')'
02480   pr #20: 'Call Print.MyFontSize(12)'
02490   txt$=pe$(1)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-30)&')'
02500   txt$=pe$(2)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-25)&')'
02510   txt$=pe$(3)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-20)&')'
02520   txt$=pe$(4)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-15)&')'
02530   pr #20: 'Call Print.MyFontSize(10)'
02540   txt$=mg$(1) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-47)&')'
02550   txt$=mg$(2) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-43.5)&')'
02560   txt$=mg$(3) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-40)&')'
02570   if a1=5 then gosub WATER: goto L2580 else goto L2590 ! senior citized discount
02580 L2580: txt$="Senior Discount "&cnvrt$("pic($$$,$$$.##",(stdwater-g(1))) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne-36.5)&')'
02590 L2590: if lyne<90 then let updown=3
02600   if lyne>90 and lyne<180 then let updown=6.5
02610   if lyne>180 and lyne<270 then let updown=10
02620   if trim$(cr$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(3)&','&str$(updown)&',"'&cr$&'")'
02630   bills+=1
02640   if int(bills/3)=bills/3 then let fnpa_newpage : lyne=0: goto L2670
02650   if lyne<90 then lyne=93 : goto L2670
02660   if lyne>90 then lyne=186: goto L2670
02670 L2670: return 
02680 BULKSORT: ! bulk sort order
02690   close #1: ioerr L2700
02700 L2700: open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02710   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02720 L2720: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2750
02730   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02740   goto L2720
02750 L2750: ! Close #1: Ioerr 2490
02760   close #6: ioerr L2770
02770 L2770: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2790
02780   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02790 L2790: return 
02800 PRIOR_USAGES: ! 
02810   mat usage=(0): mat billdate=(0)
02820   restore #15,key>=z$&"         ": nokey L2910 ! no average but active customer (use 0 usage)
02830 L2830: read #ubtransvb,using L2840: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2910
02840 L2840: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
02850   if p$<>z$ then goto L2910
02860   if tcode<>1 then goto L2830 ! only charge transactions
02870   let usage(3)=usage(2): billdate(3)=billdate(2)
02880   let usage(2)=usage(1): billdate(2)=billdate(1)
02890   let usage(1)=wu: billdate(1)=tdate
02900   goto L2830
02910 L2910: return 
02920 WATER: ! water charge if residential instead of senior citizen (returns stdwater)
02930   stdwater=0
02940   let waterrate=1 ! set rate code water residential
02950   let x1=d(3) ! water usage
02960   read #ratemst,using L2970, key="WA"&lpad$(str$(waterrate),2): mc1,mu1,mat rt nokey WATER_COMPLETED
02970 L2970: form pos 55,32*g 10
02980   stdwater=mc1*max(1,d(13))
02990   if x1<=mu1*max(1,d(13)) then goto WATER_COMPLETED else mu2=mu1*max(1,d(13))
03000   for j=1 to 10
03010     if rt(j,1)>x1 then goto WATER_COMPLETED
03020     if x1<rt(j,2) then let w1=x1-mu2 else let w1=rt(j,2)-mu2
03030     stdwater=stdwater+round(w1*rt(j,3),2)
03040     if rt(j,2)>x1 then goto WATER_COMPLETED
03050     mu2=rt(j,2)
03060   next j
03070   goto WATER_COMPLETED
03080 WATER_COMPLETED: ! 
03090   return 
