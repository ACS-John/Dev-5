00010 ! Replace S:\acsUB\ubprtbl1_Gilbertown
00020 ! pr bills for Village of Illiopolis
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnbarcode,fncmdkey,fnpa_finis,fnpa_open,fnpa_newpage
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
00180 ! Gosub BULKSORT
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00210   def fnc(x)=int(100*(x+sgn(x)*.0001))
00220   gosub SORT1
00230 ! ______________________________________________________________________
00240 SCREEN1: ! 
00250   a$="" : let prtbkno=0
00260   fntos(sn$="UBPrtBl1-1") !:
        let pf=33 : ll=30 !:
        let respc=0
00270   fnlbl(3,1,"Penalty Due Date:",ll,1)
00280   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00290   fnlbl(4,1,"Message on Bill:",ll,1)
00300   fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00310   fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00320   fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00330   fnlbl(7,1,"Date of Billing:",ll,1)
00340   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00350   fnlbl(8,1,"Starting Account:",ll,1)
00360   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00370   fnlbl(9,1,"Route Number:",ll,1)
00380   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00390   fnchk(10,pf,"Select Accounts to Print:",1) !:
        let resp$(respc+=1)="False"
00400   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00410   if ck=5 then goto ENDSCR
00420   let d1 = val(resp$(5)) !:
        let d4 = val(resp$(1)) !:
        let mg$(1) = resp$(2) !:
        let mg$(2) = resp$(3) !:
        let mg$(3) = resp$(4)
00430   if resp$(6)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(6)(1:9)),9)
00440   if resp$(7)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(7))
00450   if resp$(8)="True" then sl1=1: let z$="" else sl1=0
00460   if trim$(a$)<>"" then read #2,using L470,key=a$: z$,route,sequence nokey SCREEN1 !:
          let holdz$=z$: begin=1 !:
          st1=1
00470 L470: form pos 1,c 10,pos 1741,n 2,n 7
00480   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00490   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00500   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00510 ! ______________________________________________________________________
00520   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00530   gosub VBOPENPRINT
00540 ! ______________________________________________________________________
00550   on fkey 5 goto RELEASE_PRINT
00560 L560: if sl1=1 then goto SCREEN3
00570 L570: read #6,using L600: z$ eof RELEASE_PRINT
00580   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L570 ! start with
00590   begin=0 ! cancel starting account
00600 L600: form pos 22,c 10
00610   read #1,using L620,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L570
00620 L620: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
00630   if prtbkno=0 then goto L650
00640   if route <prtbkno then goto L570
00641   if route > prtbkno then goto RELEASE_PRINT
00650 L650: if f<>d1 and bal<>0 then mat g=(0): goto L660
00652   if f><d1 then goto L560
00660 L660: if st1=0 then goto READALTADR
00670 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00680 READALTADR: ! 
00690 ! read alternate billing address
00700   read #3,using L710,key=z$: mat ba$ nokey L780
00710 L710: form pos 11,4*c 30
00720   e1=0 : mat pe$=("")
00730   for j=1 to 4
00740     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=ba$(j)
00750   next j
00760   goto L930
00770 ! ______________________________________________________________________
00780 L780: e1=0 : mat pe$=("")
00790   for j=2 to 4
00800     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=e$(j)
00810   next j
00820   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00830   goto L930
00840 ! ______________________________________________________________________
00850 RELEASE_PRINT: ! 
00860   close #1: ioerr L870
00870 L870: close #3: ioerr L880
00880 L880: let fnpa_finis
00910   goto ENDSCR
00920 ! ______________________________________________________________________
00930 L930: ! 
00940   let pb=bal-g(11)
00950 ! If BAL<=0 Then Let G(10)=0 ! don't show penalty if balance 0 or less
00960 ! ______________print bill routine______________________________________
00970   gosub VBPRINT
00980 ! _____________end of pr routine______________________________________
00990   bct(2)=bct(2)+1 !:
        ! accumulate totals
01000   goto L560
01010 ! ______________________________________________________________________
01020 SCREEN3: ! 
01030   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01040   let txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01050 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01060   if trim$(z$)<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          fnlbl(3,1,txt$,44,1)
01070   fncmbact(1,17) ! !:
        let resp$(1)=a$
01080   fncmdkey("&Print",1,1,0) !:
        fncmdkey("&Complete",5,0,1)
01090   fnacs(sn$,0,mat resp$,ck)
01100   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01110   if ck=5 then goto RELEASE_PRINT
01120   read #1,using L620,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
01130   goto READALTADR
01140 ! ______________________________________________________________________
01150 SORT1: ! SELECT & SORT
01160   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1410
01170   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
01180   s5=1
01190   if prtbkno=0 then let routekey$="" else !:
          let routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01200   restore #2,search>=routekey$: 
01210 L1210: read #2,using L1220: z$,f,route,bal eof END5
01220 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741,n 2,pos 292,pd 4.2
01230   if prtbkno=0 then goto L1250
01240   if prtbkno><route then goto END5
01250 L1250: if bal<>0 then goto L1260
01251   if f><d1 then goto L1210
01260 L1260: let zip5$=cr$=""
01270   read #5,using "Form POS 96,C 12,C 4",key=z$: bc$,cr$ nokey L1280
01280 L1280: write #6,using "Form POS 1,C 12,C 4,x 5,C 10": bc$,cr$,z$
01290   goto L1210
01300 ! ______________________________________________________________________
01310 END5: close #6: 
01320   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01330 L1330: form pos 1,c 128
01340   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01350   write #9,using L1330: "Mask 1,19,C,A"
01360   close #9: 
01370   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1380
01380 L1380: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01390   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01400   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01410 L1410: return 
01420 ! ______________________________________________________________________
01430 ENDSCR: ! pr totals screen
01440   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01450   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01460   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01470   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01480 ! Let FNLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01490 ! Let FNTXT(2,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01500 ! Let FNLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01510 ! Let FNTXT(3,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01520 ! Let FNLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01530 ! Let FNTXT(4,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01540   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01550 XIT: let fnxit
01560 ! ______________________________________________________________________
01570 ERTN: let fnerror(program$,err,line,act$,"xit")
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
01700   return 
01710 ! ______________________________________________________________________
01720 VBPRINT: ! 
01730 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01740   checkcounter+=1
01750   if checkcounter=1 then let xmargin=17 : let ymargin=0
01760   if checkcounter=2 then let xmargin=169 : let ymargin=0
01770   if checkcounter=3 then let xmargin=17 : let ymargin=108
01780   if checkcounter=4 then let xmargin=169 : let ymargin=108 !:
          checkcounter=0
01790   adder=0
01800 ! ______________________________________________________________________
01810   pr #20: 'Call Print.MyFontSize(10)'
01820   pr #20: 'Call Print.MyFont("Courier New")'
01830 ! ______________________________________________________________________
01840 PRINTGRID: ! 
01850   pr #20: 'Call Print.AddText("'&z$&'",'&str$(xmargin+107)&','&str$(19+ymargin)&')'
01860   pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(xmargin+134)&','&str$(19+ymargin)&')' ! due date
01870   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+107)&','&str$(24+ymargin)&')'
01880   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+129)&','&str$(24+ymargin)&')'
01890   if g(1)=0 then goto L1920 else !:
          let txt$=cnvrt$("N 7",d(1)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(xmargin+4)&','&str$(25+ymargin)&')' !:
          let txt$=cnvrt$("N 7",d(2)) !:
          pr #20: 'Call Print.AddText("'&txt$&'",'&str$(xmargin+20)&','&str$(25+ymargin)&')'
01900   pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+39)&','&str$(25+ymargin)&')'
01910   pr #20: 'Call Print.AddText("Water",'&str$(xmargin+68)&','&str$(25+ymargin)&')' !:
        pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+81)&','&str$(25+ymargin)&')'
01920 L1920: adder=25
01930   if g(5)=0 then goto L1940 else !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+81)&','&str$((adder+=5)+ymargin)&')' !:
          !:
          pr #20: 'Call Print.AddText("Garbage",'&str$(xmargin+68)&','&str$(adder+ymargin)&')'
01940 L1940: if g(8)=0 then goto L1950 else !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+81)&','&str$((adder+=5)+ymargin)&')' ! !:
          pr #20: 'Call Print.AddText("Other",'&str$(xmargin+68)&','&str$(adder+ymargin)&')'
01950 L1950: if g(9)=0 then goto L1960 else !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+81)&','&str$((adder+=5)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("Tax",'&str$(xmargin+68)&','&str$(adder+ymargin)&')' ! tax
01960 L1960: if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+20)&','&str$(45+ymargin)&')'
01970   adder=55
01980   pr #20: 'Call Print.AddText("'&z$&'",'&str$(xmargin+5)&','&str$((adder+=9)+ymargin)&')'
01990   pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+30)&','&str$(adder+ymargin)&')' ! date read
02000   pr #20: 'Call Print.AddText("'&fnformnumb$(g(12),2,9)&'",'&str$(xmargin+45)&','&str$(adder+ymargin)&')' ! net
02010   pr #20: 'Call Print.AddText("'&fnformnumb$(g(11),2,9)&'",'&str$(xmargin+81)&','&str$(adder+ymargin)&')' ! net
02020   pr #20: 'Call Print.AddText("'&fnformnumb$(pb+round(pb*.10,2),2,9)&'",'&str$(xmargin+45)&','&str$((adder+=4)+ymargin)&')' ! previous balance
02030   pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+81)&','&str$((adder)+ymargin)&')' ! previous balance
02040   pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(xmargin+30)&','&str$((adder+=4.5)+ymargin)&')' ! due date
02050   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+45)&','&str$(adder+ymargin)&')' ! total
02060   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+81)&','&str$(adder+ymargin)&')' ! total
02070 ! ______________________________________________________________________
02080   addy=14
02090   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+40)&','&str$(80+ymargin)&')'
02100   pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+40)&','&str$(85+ymargin)&')'
02110   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+40)&','&str$(90+ymargin)&')'
02120   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(105+ymargin)
02130   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(105+ymargin)&')'
02144   pr #20: 'Call Print.MyFontSize(9)'
02145   pr #20: 'Call Print.MyFontBold(1)'
02150   pr #20: 'Call Print.AddText("Return Service Requested",'&str$(xmargin+107)&','&str$(41+ymargin)&')'
02155   pr #20: 'Call Print.MyFontBold(0)'
02156   pr #20: 'Call Print.MyFontSize(10)'
02160   pr #20: 'Call Print.MyFontBold(1)'
02170   fnbarcode(z$,xmargin+110,ymargin+50)
02180   fnbarcode(z$,xmargin+110,ymargin+50)
02190   pr #20: 'Call Print.MyFontBold(0)'
02200   addy=55
02210   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+108)&','&str$((addy+=5)+ymargin)&')'
02220   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+108)&','&str$((addy+=5)+ymargin)&')'
02230   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+108)&','&str$((addy+=5)+ymargin)&')'
02240   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+108)&','&str$((addy+=5)+ymargin)&')'
02250   if checkcounter=1 then checkx=1.675 : checky=4.0000
02260   if checkcounter=2 then checkx=7.80 : checky=4.0000
02270   if checkcounter=3 then checkx=1.675 : checky=8.0000
02280   if checkcounter=0 then checkx=7.80 : checky=8.0000
02290   bc$=""
02300   read #5,using "Form POS 96,C 12",key=z$: bc$ nokey L2310
02310 L2310: if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02320   if checkcounter=0 then !:
          fnpa_newpage
02330   return 
02340 ! ______________________________________________________________________
02350 BULKSORT: ! bulk sort order
02360   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02370   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02380 L2380: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2410
02390   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02400   goto L2380
02410 L2410: close #1: ioerr L2420
02420 L2420: close #6: ioerr L2430
02430 L2430: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2450
02440   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02450 L2450: return 
