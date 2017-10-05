00010 ! Replace S:\acsUB\ubprtthree_barcode
00020 ! barcode sample for three per page
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnmsgbox,fnbarcode,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(12)*60,txt$*100,mg$(3)*60,rw(22,13),cap$*128,fb$(3)*60
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100   dim dueby$*30,prebal$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$) !:
        fnd1(d1)
00130   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00140   open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00150   at$(1)=cnam$ !:
        let z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00160   let z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00170   linelength=62
00180 ! 
00190   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00200   gosub BULKSORT
00210   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00220   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00230   open #ubtransvb=15: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00240   def fnc(x)=int(100*(x+sgn(x)*.0001))
00250 ! ______________________________________________________________________
00260   prebal$="10:00 AM, xxxxxxx  xx"
00270 SCREEN1: ! 
00280   a$="" : prtbkno=0
00290   fntos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00300   fnlbl(1,1,"Current Reading Date:",ll,1)
00310   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00320   fnlbl(2,1,"Previous Reading Date:",ll,1)
00330   fntxt(2,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00340   fnlbl(3,1,"Penalty Due Date:",ll,1)
00350   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00360   fnlbl(4,1,"Message on Bill:",ll,1)
00370   fntxt(4,pf,60,60) !:
        resp$(respc+=1)=mg$(1)
00380   fntxt(5,pf,60,60) !:
        resp$(respc+=1)=mg$(2)
00390   fntxt(6,pf,60,60) !:
        resp$(respc+=1)=mg$(3)
00400   fnlbl(7,1,"Date of Billing:",ll,1)
00410   fntxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00420   fnlbl(8,1,"Starting Account:",ll,1)
00430   let fe$="ubm-act-nam" !:
        datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00440   fnlbl(9,1,"Route Number:",ll,1)
00450   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00460   fnchk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00470   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00480   if ck=5 then goto ENDSCR
00490   d2x= val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6) !:
        d1 = val(resp$(7))
00500   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00510   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00520   if resp$(10)="True" then sl1=1: let z$="" else sl1=0
00530   if trim$(a$)<>"" then read #2,using L540,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00540 L540: form pos 1,c 10,pos 1741,n 2,n 7
00550   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00560   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00570   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00580 ! ______________________________________________________________________
00590   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00600   fnpa_open
00610 ! ______________________________________________________________________
00620   on fkey 5 goto RELEASE_PRINT
00630 L630: if sl1=1 then goto SCREEN3
00640 L640: read #6,using L670: z$ eof RELEASE_PRINT
00650   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L640 ! start with
00660   begin=0 ! cancel starting account
00670 L670: form pos 22,c 10
00680   read #1,using L690,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L640
00690 L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1
00700   if prtbkno=0 then goto L720
00710   if prtbkno><route then goto RELEASE_PRINT
00720 L720: if f><d1 then goto L630
00730 L730: e1=0 : mat pe$=("")
00740   for j=2 to 4
00750     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00760   next j
00770   if st1=0 then goto READALTADR
00780 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00790 READALTADR: ! 
00800 ! read alternate billing address
00810   read #3,using L820,key=z$: mat ba$ nokey L910
00815   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L910
00820 L820: form pos 11,4*c 30
00830   e1=0 : mat pe$=("")
00840   for j=1 to 4
00850     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00860   next j
00870   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00880   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00890   goto L1030
00900 ! ______________________________________________________________________
00910 L910: ! 
00920   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00930   goto L1030
00940 ! ______________________________________________________________________
00950 RELEASE_PRINT: ! 
00960   close #1: ioerr L970
00970 L970: close #3: ioerr L980
00980 L980: ! 
00990   fnpa_finis
01010   goto ENDSCR
01020 ! ______________________________________________________________________
01030 L1030: ! 
01040   pb=bal-g(11)
01050   if bal<=0 then let g(9)=g(10)=0 ! don't show penalty if balance 0 or less
01060   let fb$(1)=mg$(1)
01070   let fb$(2)=mg$(2)
01080   let fb$(3)=mg$(3)
01090   if c4>0 then let fb$(1)="          Final Bill" : let fb$(2)="": let fb$(3)=""
01100 ! ______________print bill routine______________________________________
01110   gosub VBPRINT
01120 ! _____________end of pr routine______________________________________
01130   bct(2)=bct(2)+1 !:
        ! accumulate totals
01140   goto L630
01150 ! ______________________________________________________________________
01160 SCREEN3: ! 
01170   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01180   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01190 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01200   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01210   fncmbact(1,17) ! !:
        resp$(1)=a$
01220   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
01230   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01240   if ck=5 then goto RELEASE_PRINT
01250   read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final nokey SCREEN3
01260   goto L730
01270 ! ______________________________________________________________________
01280 SORT1: ! SELECT & SORT
01290   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1540
01300   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01310   s5=1
01320   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01330   restore #2,search>=routekey$: 
01340 L1340: read #2,using L1350: z$,f,route eof END5
01350 L1350: form pos 1,c 10,pos 296,pd 4,pos 1741
01360   if prtbkno=0 then goto L1380
01370   if prtbkno><route then goto END5
01380 L1380: if f><d1 then goto L1340
01390   let zip5$=cr$=""
01400   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1410
01410 L1410: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01420   goto L1340
01430 ! ______________________________________________________________________
01440 END5: close #6: 
01450   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01460 L1460: form pos 1,c 128
01470   write #9,using L1460: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01480   write #9,using L1460: "Mask 1,19,C,A"
01490   close #9: 
01500   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1510
01510 L1510: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01520   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01530   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01540 L1540: return 
01550 ! ______________________________________________________________________
01560 ENDSCR: ! pr totals screen
01570   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01580   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01590   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01600   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01610 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01620 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01630 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01640 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01650 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01660 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01670   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01680 XIT: fnxit
01690 ! ______________________________________________________________________
01700 ERTN: fnerror(program$,err,line,act$,"xit")
01710   if uprc$(act$)<>"PAUSE" then goto L1740
01720   execute "list -"&str$(line) !:
        pause  !:
        goto L1740
01730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01740 L1740: execute act$
01750   goto ERTN
01760 ! ______________________________________________________________________
01860 VBPRINT: ! 
01870 ! -- Printer Program for three per page  Utility Bills
01880 ! Gosub PRIOR_USAGES
01890   pr #20: 'Call Print.MyFontSize(10)'
01900   txt$=trim$(z$) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(62)&','&str$(lyne+10)&')'
01903   txt$=cnvrt$("pic(-------.##)",g(12)+pb)&"   "&cnvrt$("pic(zzbzzbzz)",d4)&cnvrt$("pic(-----.##)",g(11)+pb)
01904   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(85)&','&str$(lyne+38)&')'
01905   fnbarcode(z$,90,lyne+45)
01906 ! fnBARCODE(CNVRT$("pic(------.##",BAL),86,LYNE+18)
01910   txt$=trim$(fb$(1)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(95)&','&str$(lyne+7)&')'
01920   if pb<>0 then de$="Prv  " else de$="     "
01930   txt$=de$&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzz)",0)&" "&cnvrt$("pic(-----.--)",pb) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+25)&')'
01940   if g(1)>0 then de$="WatER" else de$="     "
01950   txt$=de$&cnvrt$("pic(zzzzzzzz)",d(1))&cnvrt$("pic(zzzzzzzz)",d(2))&cnvrt$("pic(zzzzzz)",d(3))&" "&cnvrt$("pic(-----.--)",g(1)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+30)&')'
01960   if g(2)>0 then de$="Sewer" else de$="     "
01970   txt$=de$&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzz)",0)&" "&cnvrt$("pic(-----.##)",g(2))&"     " !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+35)&')'
01980   if g(3)>0 then de$="Fee  " else de$="     "
01990   txt$=de$&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzz)",0)&" "&cnvrt$("pic(-----.--)",g(3))& "     " !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+40)&')'
02000   if g(5)>0 then de$="P/T" else de$="   "
02010   if g(5)>0 then txt$=cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzzzz)",0)&cnvrt$("pic(zzzzzz)",0)&" "&de$&cnvrt$("pic(-----.##)",g(5)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+45)&')'
02020   cd1$="Other Charge"
02030   read #8,using L2040,key="OC"&lpad$(str$(a4),2): cd1$ nokey L2050
02040 L2040: form pos 5,c 12
02050 L2050: if g(8)>0 then cd1$="Other Charge" else cd1$=""
02060   if g(8)>0 then txt$=cd1$&cnvrt$("pic(-------.##)",g(8)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+50)&')'
02070   if g(9)>0 then de$="Tax" else de$="   "
02080   if d2=0 then d2=d3x
02090   if d3=0 then d3=d2x
02100   txt$=cnvrt$("pic(zzbzzbbzz)",d3x)&" "&cnvrt$("pic(zzbzzbbzz)",d2x)&cnvrt$("pic(------.##)",g(12)+pb) &cnvrt$("pic(------.##)",g(11)+pb)&"    " !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(1)&','&str$(lyne+76)&')'
02101   pr #20: 'Call Print.AddText("'&pe$(1)&'",'&str$(90)&','&str$(lyne+60)&')'
02102   pr #20: 'Call Print.AddText("'&pe$(2)&'",'&str$(90)&','&str$(lyne+64)&')'
02103   pr #20: 'Call Print.AddText("'&pe$(3)&'",'&str$(90)&','&str$(lyne+68)&')'
02104   pr #20: 'Call Print.AddText("'&pe$(4)&'",'&str$(90)&','&str$(lyne+72)&')'
02110   bills+=1
02120   if int(bills/3)=bills/3 then let fnpa_newpage: lyne=0: goto L2150
02130   lyne=lyne+90
02150 L2150: return 
02160 BULKSORT: ! bulk sort order
02170   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02180   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02190 L2190: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2220
02200   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02210   goto L2190
02220 L2220: close #1: ioerr L2230
02230 L2230: close #6: ioerr L2240
02240 L2240: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2260
02250   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02260 L2260: return 
02270 PRIOR_USAGES: ! 
02280   mat usage=(0): mat billdate=(0)
02290   restore #15,key>=z$&"         ": nokey L2380 ! no average but active customer (use 0 usage)
02300 L2300: read #ubtransvb,using L2310: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2380
02310 L2310: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
02320   if p$<>z$ then goto L2380
02330   if tcode<>1 then goto L2300 ! only charge transactions
02340   let usage(3)=usage(2): billdate(3)=billdate(2)
02350   let usage(2)=usage(1): billdate(2)=billdate(1)
02360   let usage(1)=wu: billdate(1)=tdate
02370   goto L2300
02380 L2380: return 
