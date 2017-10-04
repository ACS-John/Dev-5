00010 ! Replace S:\acsUB\ubprtbl1_tho
00020 ! pr bills for Village of Thomasboro
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt
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
00180   gosub BULKSORT
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno)&",Shr",internal,input,keyed  ! open in route-sequence #
00210   def fnc(x)=int(100*(x+sgn(x)*.0001))
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   a$="" : let prtbkno=0
00250   fntos(sn$="UBPrtBl1-1") !:
        let pf=26 : ll=24 !:
        let respc=0
00260   fnlbl(3,1,"Penalty Due Date:",ll,1)
00270   fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00280   fnlbl(4,1,"Message on Bill:",ll,1)
00290   fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00300   fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00310   fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00320   fnlbl(7,1,"Date of Billing:",ll,1)
00330   fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00340   fnlbl(8,1,"Starting Account:",ll,1)
00350   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&str$(cno) !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&str$(cno) !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00360   fnlbl(9,1,"Route Number:",ll,1)
00370   fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00380   fnchk(10,pf,"Select Accounts to Print",1) !:
        let resp$(respc+=1)="False"
00390   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00400   if ck=5 then goto ENDSCR
00410   let d1 = val(resp$(5)) !:
        let d4 = val(resp$(1)) !:
        let mg$(1) = resp$(2) !:
        let mg$(2) = resp$(3) !:
        let mg$(3) = resp$(4)
00420   if resp$(6)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(6)(1:9)),9)
00430   if resp$(7)="[All]" then !:
          let prtbkno=0 else !:
          let prtbkno = val(resp$(7))
00440   if resp$(8)="True" then sl1=1: let z$="" else sl1=0
00450   if trim$(a$)<>"" then read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1 !:
          let holdz$=z$: begin=1 !:
          st1=1
00460 L460: form pos 1,c 10,pos 1741,n 2,n 7
00470   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00480   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00490   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00500 ! ______________________________________________________________________
00510   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00520   gosub VBOPENPRINT ! Open #20: "Name="&env$('Q')&"\UBmstr\Bill"&WSID$&".txt,Replace,RecL=5000",Display,Output  !:
        ! Let FNOPENPRN
00530 ! ______________________________________________________________________
00540   on fkey 5 goto RELEASE_PRINT
00550 L550: if sl1=1 then goto SCREEN3
00560 L560: read #6,using L590: z$ eof RELEASE_PRINT
00570   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
00580   begin=0 ! cancel starting account
00590 L590: form pos 22,c 10
00600   read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,df$,extra22 nokey L560
00610 L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1712,c 1,pos 1854,pd 5.2
00620   if prtbkno=0 then goto L640
00630   if prtbkno><route then goto RELEASE_PRINT
00640 L640: if f><d1 then goto L550
00650   if st1=0 then goto READALTADR
00660 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00670 READALTADR: ! 
00671   if extra22=2 then goto L770 ! coded to skip
00680 ! read alternate billing address
00690   read #3,using L700,key=z$: mat ba$ nokey L770
00700 L700: form pos 11,4*c 30
00710   e1=0 : mat pe$=("")
00720   for j=1 to 4
00730     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=ba$(j)
00740   next j
00750   goto L920
00760 ! ______________________________________________________________________
00770 L770: e1=0 : mat pe$=("")
00780   for j=2 to 4
00790     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : let pe$(e1)=e$(j)
00800   next j
00810   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00820   goto L920
00830 ! ______________________________________________________________________
00840 RELEASE_PRINT: ! 
00850   close #1: ioerr L860
00860 L860: close #3: ioerr L870
00870 L870: let fnpa_finis
00900   goto ENDSCR
00910 ! ______________________________________________________________________
00920 L920: ! 
00930   let pb=bal-g(11)
00940   if bal<=0 then let g(10)=0 ! don't show penalty if balance 0 or less
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
01030   let txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01040 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01050   if trim$(z$)<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          fnlbl(3,1,txt$,44,1)
01060   fncmbact(1,17) ! !:
        let resp$(1)=a$
01070   fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01080   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01090   if ck=5 then goto RELEASE_PRINT
01100   read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,df$,extra22 nokey SCREEN3
01110   goto READALTADR
01120 ! ______________________________________________________________________
01130 SORT1: ! SELECT & SORT
01140   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1390
01150   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01160   s5=1
01170   if prtbkno=0 then let routekey$="" else !:
          let routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01180   restore #2,search>=routekey$: 
01190 L1190: read #2,using L1200: z$,f,route eof END5
01200 L1200: form pos 1,c 10,pos 296,pd 4,pos 1741
01210   if prtbkno=0 then goto L1230
01220   if prtbkno><route then goto END5
01230 L1230: if f><d1 then goto L1190
01240   let zip5$=cr$=""
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
01420   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01430   fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01440   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01450   fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01460 ! Let FNLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01470 ! Let FNTXT(2,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01480 ! Let FNLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01490 ! Let FNTXT(3,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01500 ! Let FNLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01510 ! Let FNTXT(4,MYPOS,8,0,1,"",1) !:
        ! Let RESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01520   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01530 XIT: let fnxit
01540 ! ______________________________________________________________________
01550 ERTN: let fnerror(program$,err,line,act$,"xit")
01560   if uprc$(act$)<>"PAUSE" then goto L1590
01570   execute "list -"&str$(line) !:
        pause  !:
        goto L1590
01580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01590 L1590: execute act$
01600   goto ERTN
01610 ! ______________________________________________________________________
01620 VBOPENPRINT: ! 
01640   fnPa_open("Landscape")
01670   lyne=3
01700   return 
01710 ! ______________________________________________________________________
01720 VBPRINT: ! 
01730 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01740   checkcounter+=1
01750   if checkcounter=1 then let xmargin=0 : let ymargin=0
01760   if checkcounter=2 then let xmargin=139 : let ymargin=0
01770   if checkcounter=3 then let xmargin=0 : let ymargin=108
01780   if checkcounter=4 then let xmargin=139 : let ymargin=108 !:
          checkcounter=0
01790 ! ______________________________________________________________________
01800   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01810   pr #20: "Call Print.MyFontBold(True)"
01820   pr #20: 'Call Print.MyFontSize(12)'
01830   pr #20: 'Call Print.MyFont("Courier New")'
01840   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01850   pr #20: 'Call Print.MyFont("Lucida Console")'
01860   pr #20: 'Call Print.MyFontSize(10)'
01870   pr #20: 'Call Print.MyFontBold(False)'
01880   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
01890   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
01900   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
01910   pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
01920   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
01930   pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
01940   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01950   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
01960   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
01970   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
01980   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
01990 ! ______________________________________________________________________
02000 PRINTGRID: let meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02010   if g(1)=0 then goto L2020 else !:
          pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02020 L2020: if g(2)=0 then goto L2030 else !:
          pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02030 L2030: if g(3)=0 and d(7)=0 then goto L2050 else !:
          pr #20: 'Call Print.AddText("LM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02040 !  pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02050 L2050: if a4=1 then let gcode$="RSGS" else !:
          if a4=2 then let gcode$="CMGS" else !:
            if a4=3 then let gcode$="INGS" else !:
              let gcode$="GAS"
02060   if g(4)=0 then goto L2070 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02070 L2070: ! If G(5)=0 Then Goto 2040 Else !:
        ! pr #20: 'Call Print.AddText("MET",'&STR$(XMARGIN+1)&','&STR$(LYNE*(METER+=1)+YMARGIN)&')' !:
        ! pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(5),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02080 ! If G(6)=0 Then Goto 2050 Else !:
        ! pr #20: 'Call Print.AddText("FUR",'&STR$(XMARGIN+1)&','&STR$(LYNE*(METER+=1)+YMARGIN)&')' !:
        ! pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(6),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02090   if g(7)=0 then goto L2100 else !:
          pr #20: 'Call Print.AddText("TRASH   ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02100 L2100: if g(8)=0 then goto L2110 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02110 L2110: if g(9)=0 then goto L2120 else !:
          pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02120 L2120: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
02130   if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02140   if pb=0 then goto L2150 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02150 L2150: pr #20: 'Call Print.MyFontSize(10)'
02160 ! ______________________________________________________________________
02170   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02180   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
02190   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
02200   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
02210   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
02220   fnpa_txt(fnformnumb$(bal+round((bal-g(7))*.10,2),2,9),xmargin+42,lyne*25+ymargin)
02230   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
02240   pr #20: 'Call Print.AddText("Phone: 217-643-2675",'&str$(xmargin+1)&','&str$(lyne*27+ymargin)&')'
02250 ! ______________________________________________________________________
02260   special=28
02270 ! ______________________________________________________________________
02280   pr #20: 'Call Print.MyFontSize(7)'
02290   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02300   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02310   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02330   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02340   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02350   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02360   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02370   pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
02380   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02390   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02400   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02410   pr #20: 'Call Print.AddText("  Permit No 1",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02420   pr #20: 'Call Print.MyFontSize(9)'
02430 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02440   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
02450   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
02460   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
02470   pr #20: 'Call Print.MyFontSize(10)'
02480   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02490   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02500   pr #20: 'Call Print.AddText("After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
02510   fnpa_txt(fnformnumb$(bal+round((bal-g(7))*.10,2),2,9),xmargin+106,lyne*12+ymargin)
02520   pr #20: 'Call Print.MyFontSize(9)'
02530   addy=14
02540   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02550   pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02560   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02570   addy+=1
02580   pr #20: 'Call Print.MyFontSize(10)'
02600   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02610   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02620   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02630   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02635   if uprc$(df$)='Y' then !:
          pr #20: 'Call Print.AddText("Direct Debit",'&str$(xmargin+36)&','&str$(lyne*(addy)+ymargin)&')'
02640   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02650   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02660   if checkcounter=1 then checkx=1.375 : checky=3.6875
02670   if checkcounter=2 then checkx=6.75 : checky=3.6875
02680   if checkcounter=3 then checkx=1.375 : checky=7.9375
02690   if checkcounter=0 then checkx=6.75 : checky=7.9375
02700   bc$=""
02710   if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02720   if checkcounter=0 then !:
          fnpa_newpage
02730   return 
02740 ! ______________________________________________________________________
02750 BULKSORT: ! bulk sort order
02760   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,input,keyed  ! open in Account order
02770   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
02772   do
02780   read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2810
02790   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02800   loop
02810   L2810: close #1: ioerr ignore
02820   close #6: ioerr ignore
02830   execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2850
02840   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
02850 L2850: return 
