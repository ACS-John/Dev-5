00010 ! Replace S:\acsUB\ubpdreminder
00020 ! pr bills for Village of Thomasboro
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
00520   gosub VBOPENPRINT ! Open #20: "Name="&env$('Q')&"\UBmstr\Bill"&WSID$&".txt,Replace,RecL=5000",Display,Output  !:
        ! fnOPENPRN
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
00690   read #3,using L700,key=z$: mat ba$ nokey L770
00700 L700: form pos 11,4*c 30
00710   e1=0 : mat pe$=("")
00720   for j=1 to 4
00730     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00740   next j
00750   goto L920
00760 ! ______________________________________________________________________
00770 L770: e1=0 : mat pe$=("")
00780   for j=2 to 4
00790     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00800   next j
00810   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00820   goto L920
00830 IGNORE: continue 
00840 RELEASE_PRINT: ! 
00850   close #1: ioerr ignore
00860   close #3: ioerr ignore
00880   fnpa_finis
00900   goto ENDSCR
00910 ! ______________________________________________________________________
00920 L920: ! 
00930   pb=bal-g(11)
00940   if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
00950 ! ______________print bill routine______________________________________
00960   gosub VBPRINT
00970 ! _____________end of pr routine______________________________________
00980   bct(2)=bct(2)+1 !:
        ! accumulate totals
00990   goto L550
01000 ! ______________________________________________________________________
01010 SCREEN3: ! 
01020   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01030   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01040 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01050   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01060   fncmbact(1,17) ! !:
        resp$(1)=a$
01070   fncmdset(3): fnacs(sn$,0,mat resp$,ck)
01080   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01090   if ck=5 then goto RELEASE_PRINT
01100   read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01110   goto READALTADR
01120 ! ______________________________________________________________________
01130 SORT1: ! SELECT & SORT
01140   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr L1390
01150   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01160   s5=1
01170   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01180   restore #2,search>=routekey$: 
01190 L1190: read #2,using L1200: z$,f,route eof END5
01200 L1200: form pos 1,c 10,pos 296,pd 4,pos 1741
01210   if prtbkno=0 then goto L1230
01220   if prtbkno><route then goto END5
01230 L1230: if f><d1 then goto L1190
01240   zip5$=cr$=""
01250   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1260
01260 L1260: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01270   goto L1190
01280 ! ______________________________________________________________________
01290 END5: close #6: 
01300   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01310 L1310: form pos 1,c 128
01320   write #9,using L1310: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01330   write #9,using L1310: "Mask 1,19,C,A"
01340   close #9: 
01350   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1360
01360 L1360: execute "Sort "&env$('Temp')&"\Control."&session$
01370   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01380   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01390 L1390: return 
01400 ! ______________________________________________________________________
01410 ENDSCR: ! pr totals screen
01420   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01430   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01440   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01450   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01460 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01470 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01480 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01490 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01500 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01510 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01520   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01530 XIT: fnxit
01540 ! ______________________________________________________________________
01550 ERTN: fnerror(program$,err,line,act$,"xit")
01560   if uprc$(act$)<>"PAUSE" then goto L1590
01570   execute "list -"&str$(line) !:
        pause  !:
        goto L1590
01580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01590 L1590: execute act$
01600   goto ERTN
01610 ! ______________________________________________________________________
01620 VBOPENPRINT: ! 
01630   fnpa_open
01660   lyne=3
01670   character=1.5
01680   spacer=0
01700   return 
01710 ! ______________________________________________________________________
01720 VBPRINT: ! 
01730   pr #20: "Call Print.MyFontBold(True)"
01740   pr #20: 'Call Print.MyFontSize(16)'
01750   pr #20: 'Call Print.MyFont("Courier New")'
01760   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(10)&','&str$(lyne*4+spacer)&')'
01770   pr #20: 'Call Print.MyFont("Lucida Console")'
01780   pr #20: 'Call Print.MyFontSize(12)'
01790   pr #20: 'Call Print.MyFontBold(False)'
01800   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(10)&','&str$(lyne*6.5+spacer)&')'
01810   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(10)&','&str$(lyne*8+spacer)&')'
01820   pr #20: "Call Print.MyFontBold(True)"
01830   pr #20: 'Call Print.MyFontSize(12)'
01840   pr #20: 'Call Print.AddLine('&str$(115)&','&str$(lyne*12+spacer)&',75,'&str$(30)&',True)'
01850   pr #20: 'Call Print.AddText("A Friendly Reminder....",'&str$(100)&','&str$(lyne+spacer)&')'
01860   pr #20: 'Call Print.MyFontSize(10)'
01870   pr #20: 'Call Print.MyFontBold(False)'
01880   pr #20: 'Call Print.AddText("If your check has already been mailed,please ",'&str$(100)&','&str$(lyne*3+spacer)&')'
01890   pr #20: 'Call Print.AddText("disregard this notice.  If not, your remittance by mail ",'&str$(100)&','&str$(lyne*4+spacer)&')'
01900   pr #20: 'Call Print.AddText("will be greatly appreciated.",'&str$(100)&','&str$(lyne*5+spacer)&')'
01910   pr #20: 'Call Print.AddText("Thank You!",'&str$(150)&','&str$(lyne*7+spacer)&')'
01920   pr #20: 'Call Print.AddText("Customer #:  '&z$&'",'&str$(125)&','&str$(lyne*14+spacer)&')'
01930   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(zZZ/ZZ/ZZ)",d1)&'",'&str$(125)&','&str$(lyne*16+spacer)&')'
01940   pr #20: 'Call Print.AddText("Balance Due: '&cnvrt$("pic(---,---.##)",bal)&'",'&str$(125)&","&str$(lyne*18+spacer)&')'
01950   pr #20: 'Call Print.MyFontSize(13)'
01960   pr #20: 'Call Print.AddText("'&pe$(1)&'",'&str$(20)&','&str$(lyne*16+spacer)&')'
01970   pr #20: 'Call Print.AddText("'&pe$(2)&'",'&str$(20)&','&str$(lyne*17.5+spacer)&')'
01980   pr #20: 'Call Print.AddText("'&pe$(3)&'",'&str$(20)&','&str$(lyne*19+spacer)&')'
01990   pr #20: 'Call Print.AddText("'&pe$(4)&'",'&str$(20)&','&str$(lyne*20.5+spacer)&')'
02000   checkcounter+=1 !:
        spacer+=90
02010   if checkcounter=3 then !:
          fnpa_newpage !:
          checkcounter=0 !:
          spacer=0
02020   return 
02030 ! ______________________________________________________________________
02040 BULKSORT: ! bulk sort order
02050   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
02060   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02070 L2070: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2100
02080   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02090   goto L2070
02100 L2100: close #1: ioerr L2110
02110 L2110: close #6: ioerr L2120
02120 L2120: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2140
02130   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02140 L2140: return 
