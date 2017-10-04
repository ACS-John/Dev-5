00010 ! Replace S:\acsUB\ubprtthree_Gilb
00020 ! pr bills for Gilbertown (prace three per page)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnmsgbox,fnbarcode,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(12)*60,txt$*100,mg$(3)*60,rw(22,13),cap$*128,fb$(3)*60
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100   dim servicename$(10)*20,service$(10)*2
00110   dim dueby$*30,prebal$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
00120 ! ______________________________________________________________________
00130   fncno(cno,cnam$) !:
        fnd1(d1)
00140   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00150   ! open #ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
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
00180   let linelength=62
00190 ! 
00200   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00210   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00220   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00230   open #ubtransvb=15: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00240   open #20: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&str$(cno)&",Shr",internal,input,relative  !:
        read #20,using 'Form POS 1,10*C 20,10*C 2',rec=1: mat servicename$,mat service$ !:
        close #20: 
00260   def fnc(x)=int(100*(x+sgn(x)*.0001))
00270 ! ______________________________________________________________________
00280   let prebal$="10:00 AM, xxxxxxx  xx"
00290 SCREEN1: ! 
00300   a$="" : let prtbkno=0
00310   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : let ll=24 !:
        let respc=0
00320   fnlbl(1,1,"Current Reading Date:",ll,1)
00330   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00340   fnlbl(2,1,"Previous Reading Date:",ll,1)
00350   fntxt(2,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00360   fnlbl(3,1,"Penalty Due Date:",ll,1)
00370   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00380   fnlbl(4,1,"Message on Bill:",ll,1)
00390   fntxt(4,pf,60,60) !:
        let resp$(respc+=1)=mg$(1)
00400   fntxt(5,pf,60,60) !:
        let resp$(respc+=1)=mg$(2)
00410   fntxt(6,pf,60,60) !:
        let resp$(respc+=1)=mg$(3)
00420   fnlbl(7,1,"Date of Billing:",ll,1)
00430   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00440   fnlbl(8,1,"Starting Account:",ll,1)
00450   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00460   fnlbl(9,1,"Route Number:",ll,1)
00470   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00480   fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00490   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00500   if ck=5 then goto ENDSCR
00510   let d2x= val(resp$(1)) !:
        let d3x= val(resp$(2)) !:
        let d4 = val(resp$(3)) !:
        let mg$(1) = resp$(4) !:
        let mg$(2) = resp$(5) !:
        let mg$(3) = resp$(6) !:
        let d1 = val(resp$(7))
00520   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00530   if resp$(9)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(9))
00540   if resp$(10)="True" then let sl1=1: let z$="" else let sl1=0
00550   if trim$(a$)<>"" then read #2,using L560,key=a$: z$,route,sequence nokey SCREEN1 !:
          let holdz$=z$: begin=1 !:
          let st1=1
00560 L560: form pos 1,c 10,pos 1741,n 2,n 7
00570   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00580   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00590   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00595   gosub SORT1
00600 ! ______________________________________________________________________
00610   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00620   fnpa_open 
00630 ! ______________________________________________________________________
00640   on fkey 5 goto RELEASE_PRINT
00650 L650: if sl1=1 then goto SCREEN3
00660 L660: read #6,using L690: cr$,z$ eof RELEASE_PRINT
00670   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L660 ! start with
00680   begin=0 ! cancel starting account
00690 L690: form pos 1,c 12,c 10
00700   read #1,using L720,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ nokey L660
00710   if estimatedate>0 then let est=1 else let est=0
00720 L720: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,c 9,n 2,c 17
00730   if prtbkno=0 then goto L750
00740   if prtbkno><route then goto RELEASE_PRINT
00750 L750: if f><d1 then goto L650
00760 L760: let e1=0 : mat pe$=("")
00770   for j=2 to 4
00780     if rtrm$(e$(j))<>"" then !:
            let e1=e1+1 : let pe$(e1)=e$(j)
00790   next j
00800   if st1=0 then goto READALTADR
00810 ! If ST1$=Z$ Then Let ST1=0 Else Goto 560
00820 READALTADR: ! 
00830 ! read alternate billing address
00840   read #3,using L860,key=z$: mat ba$ nokey L950
00850   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L950
00860 L860: form pos 11,4*c 30
00870   let e1=0 : mat pe$=("")
00880   for j=1 to 4
00890     if rtrm$(ba$(j))<>"" then !:
            let e1=e1+1 : let pe$(e1)=ba$(j)
00900   next j
00910   if trim$(pe$(2))="" then let pe$(2)=pe$(3): let pe$(3)=""
00920   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
00930   goto L1070
00940 ! ______________________________________________________________________
00950 L950: ! 
00960   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00970   goto L1070
00980 ! ______________________________________________________________________
00990 RELEASE_PRINT: ! 
01000   close #1: ioerr L1010
01010 L1010: close #3: ioerr L1020
01020 L1020: let fnpa_finis
01050   goto ENDSCR
01060 ! ______________________________________________________________________
01070 L1070: ! 
01080   let pb=bal-g(11)
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
01210   let sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01220   let txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01230 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01240   if trim$(z$)<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          fnlbl(3,1,txt$,44,1)
01250   fncmbact(1,17) ! !:
        let resp$(1)=a$
01260   fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01270   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01280   if ck=5 then goto RELEASE_PRINT
01290   read #1,using L720,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,dr$,bc,da$ nokey SCREEN3
01300   goto L760
01310 ! ______________________________________________________________________
01320 SORT1: ! SELECT & SORT
01330   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1580
01340   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=22",internal,output 
01350   let s5=1
01360   if prtbkno=0 then let routekey$="" else !:
          let routekey$=cnvrt$("N 2",prtbkno)&"       " !:
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
01610   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01620   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01630   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01640   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01650 ! Let FNLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01660 ! Let FNTXT(2,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01670 ! Let FNLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01680 ! Let FNTXT(3,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01690 ! Let FNLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01700 ! Let FNTXT(4,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01710   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01720 XIT: let fnxit
01730 ! ______________________________________________________________________
01740 ERTN: let fnerror(program$,err,line,act$,"xit")
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
01920   if d2=0 then let d2=d3x
01930   if d3=0 then let d3=d2x
01940 ! Gosub PRIOR_USAGES
01950   pr #20: 'Call Print.MyFontSize(10)'
01960   let txt$="FROM "&cnvrt$("pic(zz/zz)",int(d2x*.01))&"  TO "&cnvrt$("pic(zz/zz)",int(d3x*.01))&" "&cnvrt$("pic(##/##/##)",d1) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=13)&')'
01970   let lyne+=14
01980   if g(1)>0 then let de$=service$(1) else let de$="  "
01990   let txt$=de$& cnvrt$("pic(zzzzzzzzzz)",d(1))&cnvrt$("pic(zzzzzzzz)",d(3))&cnvrt$("pic(------.--)",g(1)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02000   if g(2)>0 then let de$=service$(2) else let de$="  "
02010   if bal>0 then let penbal=bal+round(bal*.10,2) else let penbal=0
02020   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(2))&"     "&cnvrt$("pic(-----.--)",penbal)&cnvrt$("pic(-----,---.--)",bal) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02030   if g(3)>0 then let de$=service$(3) else let de$="  "
02040   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(3))&"   "&cnvrt$("pic(zz/zz/zz)",d4) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02050   if g(4)>0 then let de$=service$(4) else let de$=" " : goto L2070
02060   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(4)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02070 L2070: if g(5)>0 then let de$=service$(5) else let de$=" " : goto L2090
02080   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(5)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02090 L2090: if g(6)>0 then let de$=service$(6) else let de$=" " : goto L2110
02100   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(6)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02110 L2110: if g(7)>0 then let de$=service$(7) else let de$=" " : goto L2130
02120   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(7)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02130 L2130: if g(8)>0 then let de$=service$(8) else let de$=" " : goto L2150
02140   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(8)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02150 L2150: if g(9)>0 then let de$=service$(9) else let de$=" " : goto L2170
02160   let txt$=de$&"                   "&cnvrt$("pic(-----.--)",g(9)) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02165   fnbarcode(z$,86,lyne)
02170 L2170: ! 
02180   if lyne<90 then let lyne=50: goto L2210 ! allow for three forms
02190   if lyne<=180 then let lyne=140: goto L2210
02200   if lyne>180 then let lyne=230
02210 L2210: if pb>0 then let de2$="Prior Balance" else let de2$="             "
02220   let txt$=de2$&"        "&cnvrt$("pic(-----.--)",pb)&"      "&z$ !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne)&')'
02230   if est=1 then let de2$="BILL ESTIMATED" : goto L2260 else let de2$="              "
02240   if final>0 then let de2$="   Final Bill  " : goto L2260 else let de2$="               "
02250   if df$="Y" then let de2$="   DRAFTED     ": goto L2260 else let de2$="               "
02260 L2260: let txt$=de2$&"                   "&pe$(1)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=7.0)&')'
02270   let txt$="Due by: "&cnvrt$("pic(zz/zz/zz)",d4)&cnvrt$("pic(--,---,---.--)",bal)&"     "&pe$(2)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02280   let txt$=e$(1)(1:18)&"                "&pe$(3)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02290   let txt$=z$&"                                            "&pe$(4)(1:25) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02300   let txt$=mg$(1) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02310   let txt$=mg$(2) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02320   let txt$=mg$(3) !:
        pr #20: 'Call Print.AddText("'&txt$&'",'&str$(6)&','&str$(lyne+=3.5)&')'
02330   if lyne<90 then let updown=3
02340   if lyne>90 and lyne<180 then let updown=6.5
02350   if lyne>180 and lyne<270 then let updown=10
02360   if trim$(cr$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(3)&','&str$(updown)&',"'&cr$&'")'
02370   bills+=1
02380   if int(bills/3)=bills/3 then let fnpa_newpage: let lyne=0: goto L2410
02390   if lyne<90 then let lyne=90 : goto L2410
02400   if lyne>90 then let lyne=180 : goto L2410
02410 L2410: return 
02420 BULKSORT: ! bulk sort order
02430   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02440   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02450 L2450: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2480
02460   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02470   goto L2450
02480 L2480: close #1: ioerr L2490
02490 L2490: close #6: ioerr L2500
02500 L2500: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2520
02510   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02520 L2520: return 
02530 PRIOR_USAGES: ! 
02540   mat usage=(0): mat billdate=(0)
02550   restore #15,key>=z$&"         ": nokey L2640 ! no average but active customer (use 0 usage)
02560 L2560: read #ubtransvb,using L2570: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2640
02570 L2570: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
02580   if p$<>z$ then goto L2640
02590   if tcode<>1 then goto L2560 ! only charge transactions
02600   let usage(3)=usage(2): billdate(3)=billdate(2)
02610   let usage(2)=usage(1): billdate(2)=billdate(1)
02620   let usage(1)=wu: billdate(1)=tdate
02630   goto L2560
02640 L2640: return 
