00010 ! Replace S:\acsUB\ubprtbl1_fin
00020 ! print bills for Village of Findlay
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnd1,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fnpa_text,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_fontsize,fnpa_txt
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128,a(7)
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   let fnd1(d1)
00120   open #21: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00122   let penalty_rate=.1 ! if env$('client')='Findlay' then let penalty_rate=.1 else let penalty_rate=.05
00130   let at$(1)=env$('cnam') !:
        let z=21 !:
        let at$(1)=trim$(at$(1))(1:z) !:
        let x=len(at$(1)) : let y=z-x !:
        let at$(1)=rpt$(" ",int(y/2))&at$(1)
00140   let z=26 !:
        for j=2 to udim(at$) !:
          let at$(j)=trim$(at$(j))(1:z) !:
          let x=len(at$(j)) : let y=z-x !:
          let at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00150   let linelength=62
00160 ! 
00180   gosub BULKSORT
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence #
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   let a$="" : let prtbkno=0
00250   let fntos(sn$="UBPrtBl1-1") !:
        let pf=27 : let ll=25 !:
        let respc=0
00260   let fnlbl(3,1,"Penalty Due Date:",ll,1)
00270   let fntxt(3,pf,8,8,1,"1",0,tt$) !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00280   let fnlbl(4,1,"Message on Bill:",ll,1)
00290   let fntxt(4,pf,30,30) !:
        let resp$(respc+=1)=mg$(1)
00300   let fntxt(5,pf,30,30) !:
        let resp$(respc+=1)=mg$(2)
00310   let fntxt(6,pf,30,30) !:
        let resp$(respc+=1)=mg$(3)
00320   let fnlbl(7,1,"Date of Billing:",ll,1)
00330   let fntxt(7,pf,8,8,1,"1") !:
        let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00340   let fnlbl(8,1,"Starting Route/Sequence:",ll,1)
00350   let fe$="ubm-act-nam" !:
        let datafile$=env$('Q')&"\UBmstr\Customer.h"&env$('cno') !:
        let indexfile$=env$('Q')&"\UBmstr\ubindx5.h"&env$('cno') !:
        let kp=1741: let kl=9 : let dp=41 : let dl=30 !:
        let fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        let resp$(respc+=1)="[All]"
00360   let fnlbl(9,1,"Route Number:",ll,1)
00370   let fncmbrt2(9,pf) !:
        let resp$(respc+=1)="[All]"
00380   let fnchk(10,pf,"Select Accounts to Print",1)
00382   let resp$(respc+=1)="False"
00384   let fnlbl(12,1,"Service From Date:",ll,1)
00386   let fntxt(12,pf,8,8,1,"1")
00388   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2_override)
00390   let fnlbl(13,1,"Service To Date:",ll,1)
00392   let fntxt(13,pf,8,8,1,"1")
00394   let resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3_override)
00396   let fncmdset(3)
00398   let fnacs(sn$,0,mat resp$,ck)
00400   if ck=5 then goto ENDSCR
00402   let d1 = val(resp$(5))
00404   let d4 = val(resp$(1))
00406   let mg$(1) = resp$(2)
00408   let mg$(2) = resp$(3)
00410   let mg$(3) = resp$(4)
00412   let d2_override=val(resp$(9))
00414   let d3_override=val(resp$(10))
00420   if resp$(6)="[All]" then let a$="" else let a$ = lpad$(trim$(resp$(6)(1:9)),9)
00430   if resp$(7)="[All]" then let prtbkno=0 else let prtbkno = val(resp$(7))
00440   if resp$(8)="True" then let sl1=1: let z$="" else let sl1=0
00450   if trim$(a$)<>"" then read #2,using 'form pos 1,c 10,pos 1741,n 2,n 7',key=a$: z$,route,sequence nokey SCREEN1
00452   let holdz$=z$: let begin=1
00454   let st1=1
00470   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00480   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00490   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00500 ! ______________________________________________________________________
00510   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00520   gosub BUD1
00530   gosub VBOPENPRINT
00540 ! ______________________________________________________________________
00550   on fkey 5 goto RELEASE_PRINT
00560 L560: if sl1=1 then goto SCREEN3
00570 L570: read #6,using L600: z$ eof RELEASE_PRINT
00580   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L570 ! start with
00590   let begin=0 ! cancel starting account
00600 L600: form pos 22,c 10
00610   read #1,using L620,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,energy$,mat a,extra11,extra12 nokey L570
00620 L620: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1954,c 12,pos 143,7*pd 2,pos 1806,2*n 2
00630   if prtbkno=0 then goto L650
00640   if prtbkno><route then goto RELEASE_PRINT
00650 L650: if f><d1 then goto L560
00660 L660: gosub BUD2 ! determine if budget customer
00670   let gas=0
00680   let energy=0: let energy=val(energy$) conv L690
00690 L690: if st1=0 then goto READALTADR
00700 ! If ST1$=Z$ Then Let ST1=0 Else Goto 560
00710 READALTADR: ! 
00720 ! read alternate billing address
00730   read #3,using L740,key=z$: mat ba$ nokey L810
00740 L740: form pos 11,4*c 30
00750   let e1=0 : mat pe$=("")
00760   for j=1 to 4
00770     if rtrm$(ba$(j))<>"" then !:
            let e1=e1+1 : let pe$(e1)=ba$(j)
00780   next j
00790   goto L960
00800 ! ______________________________________________________________________
00810 L810: let e1=0 : mat pe$=("")
00820   for j=2 to 4
00830     if rtrm$(e$(j))<>"" then !:
            let e1=e1+1 : let pe$(e1)=e$(j)
00840   next j
00850   if trim$(extra1$)<>"" then let pe$(4)=pe$(3): let pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00860   goto L960
00870 ! ______________________________________________________________________
00880 RELEASE_PRINT: ! 
00890   close #1: ioerr L900
00900 L900: close #3: ioerr L910
00910 L910: ! 
00920   let fnpa_finis
00940   goto ENDSCR
00950 ! ______________________________________________________________________
00960 L960: ! 
00970   let pb=bal-g(11)
00980   if bal<=0 then let g(5)=g(6)=g(7)=0 ! don't show penalty if balance 0 or less
00982   if d2_override<>0 then let d2=d2_override
00984   if d3_override<>0 then let d3=d3_override
00990 ! ______________print bill routine______________________________________
01000   gosub VBPRINT
01010 ! _____________end of print routine______________________________________
01020   let bct(2)=bct(2)+1 !:
        ! accumulate totals
01030   goto L560
01040 ! ______________________________________________________________________
01050 SCREEN3: ! 
01060   let sn$ = "UBPrtBl1-2" !:
        let fntos(sn$)
01070   let txt$="Account (blank to stop)" !:
        let fnlbl(1,1,txt$,31,1)
01080 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01090   if trim$(z$)<>"" then !:
          let txt$="Last Account entered was "&z$ !:
          let fnlbl(3,1,txt$,44,1) else !:
          let txt$="" !:
          let fnlbl(3,1,txt$,44,1)
01100   let fncmbact(1,17) ! !:
        let resp$(1)=a$
01110   let fncmdset(3): let fnacs(sn$,0,mat resp$,ck)
01120   let a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01130   if ck=5 then goto RELEASE_PRINT
01140   read #1,using L620,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,energy$,mat a,extra11,extra12 nokey SCREEN3
01150   goto L660
01160 ! ______________________________________________________________________
01170 SORT1: ! SELECT & SORT
01180   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr L1430
01190   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01200   let s5=1
01210   if prtbkno=0 then let routekey$="" else !:
          let routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01220   restore #2,search>=routekey$: 
01230 L1230: read #2,using L1240: z$,f,route eof END5
01240 L1240: form pos 1,c 10,pos 296,pd 4,pos 1741
01250   if prtbkno=0 then goto L1270
01260   if prtbkno><route then goto END5
01270 L1270: if f><d1 then goto L1230
01280   let zip5$=cr$=""
01290   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1300
01300 L1300: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01310   goto L1230
01320 ! ______________________________________________________________________
01330 END5: close #6: 
01340   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01350 L1350: form pos 1,c 128
01360   write #9,using L1350: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01370   write #9,using L1350: "Mask 1,19,C,A"
01380   close #9: 
01390   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1400
01400 L1400: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01410   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01420   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01430 L1430: return 
01440 ! ______________________________________________________________________
01450 ENDSCR: ! print totals screen
01460   if sum(bct)=0 then let pct=0 else let pct=bct(2)/sum(bct)*100
01470   let fntos(sn$="Bills-Total") !:
        let mylen=23 : let mypos=mylen+2 !:
        let respc=0
01480   let fnlbl(1,1,"Total Bills Printed:",mylen,1)
01490   let fntxt(1,mypos,8,0,1,"",1) !:
        let resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01560   let fncmdset(52) !:
        let fnacs(sn$,0,mat resp$,ck)
01570 XIT: let fnxit
01580 ! ______________________________________________________________________
01590 ERTN: let fnerror(program$,err,line,act$,"xit")
01600   if uprc$(act$)<>"PAUSE" then goto L1630
01610   execute "list -"&str$(line) !:
        pause  !:
        goto L1630
01620   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause 
01630 L1630: execute act$
01640   goto ERTN
01650 ! ______________________________________________________________________
01660 VBOPENPRINT: ! 
01680   let fnPa_open("Landscape")
01710   let lyne=3
01740   return 
01750 ! ______________________________________________________________________
01760 VBPRINT: ! 
01770 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01780   let checkcounter+=1
01790   if checkcounter=1 then let xmargin=0 : let ymargin=0
01800   if checkcounter=2 then let xmargin=139 : let ymargin=0
01810   if checkcounter=3 then let xmargin=0 : let ymargin=108
01820   if checkcounter=4 then let xmargin=139 : let ymargin=108 !:
          let checkcounter=0
01830 ! ______________________________________________________________________
01840   print #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01850   print #20: "Call Print.MyFontBold(True)"
01860   print #20: 'Call Print.MyFontSize(12)'
01870   print #20: 'Call Print.MyFont("Courier New")'
01880   let fnpa_text(20,at$(1),xmargin+8,lyne*1-1+ymargin) ! print #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01890   print #20: 'Call Print.MyFont("Lucida Console")'
01900   print #20: 'Call Print.MyFontSize(10)'
01910   print #20: 'Call Print.MyFontBold(False)'
01920   let fnpa_text(20,at$(2),xmargin+6,lyne*2+1+ymargin-.2) !  print #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
01930   let fnpa_text(20,at$(3),xmargin+6,lyne*3+1+ymargin) ! print #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
01940   print #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
01950   print #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
01960   print #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
01970 ! Print #20: 'Call Print.AddText("Is due now and payable.",'&STR$(XMARGIN+2)&','&STR$(LYNE*8+YMARGIN)&')'
01980   print #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01990   print #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
02000   print #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
02010   print #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
02020   print #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
02030 ! ______________________________________________________________________
02040 PRINTGRID: let meter=14 !:
        print #20: 'Call Print.MyFontSize(8)'
02050   if havebudget=1 then let payby=bal-gb(4)+budgetpb
02060   if havebudget=1 then let gas=ba(5) else let gas=g(4)
02070   if havebudget=1 then let currentcharges=g(1)+g(2)+g(3)+gas+g(8)+g(9)
02080   if havebudget=0 then let currentcharges=g(1)+g(2)+g(3)+g(4)+g(8)+g(9)
02090   if havebudget=1 then let pb=payby-currentcharges
02100   if pb=0 then goto L2110 else !:
          print #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02110 L2110: if g(1)=0 then goto L2120 else !:
          print #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02120 L2120: if g(2)=0 then goto L2130 else !:
          print #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02130 L2130: if g(3)=0 and d(7)=0 then goto L2150 else !:
          print #20: 'Call Print.AddText("Pool R",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          ! Print #20: 'Call Print.AddText("'&FNFORMNUMB$(D(5),0,9)&'",'&STR$(XMARGIN+6)&','&STR$(LYNE*METER+YMARGIN)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02140 !  Print #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
02150 L2150: if a4=1 then let gcode$="RSGS" else !:
          if a4=2 then let gcode$="CMGS" else !:
            if a4=3 then let gcode$="INGS" else !:
              let gcode$="GAS"
02160   if g(4)=0 then goto L2230 else !:
          print #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02170   if havebudget=0 then !:
          print #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')' else !:
          print #20: 'Call Print.AddText("'&fnformnumb$(ba(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02180   if havebudget=1 then print #20: 'Call Print.AddText("Actual Gas Charge: '&trim$(cnvrt$("pic($$$,$$$.##",g(4)))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02190   if havebudget=1 and gb(4)>=0 then print #20: 'Call Print.AddText("Level billing behind '&trim$(cnvrt$("pic($$$,$$$.##",abs(gb(4)-g(4))))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02200   if havebudget=1 and gb(4)<0 then print #20: 'Call Print.AddText("Level billing ahead '&trim$(cnvrt$("pic($$$,$$$.##",abs(gb(4)-g(4))))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02205   if havebudget=1 and gb(4)<>0 then print #20: 'Call Print.AddText("before paying this bill. ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02210   if havebudget=1 and energy<>0 then print #20: 'Call Print.AddText("Less CEFS Applied:             '&trim$(cnvrt$("pic(zzz,zzz.##",energy))&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02220 ! IF HAVEBUDGET=1 THEN LET BAL=G(1)+G(2)+G(3)+GAS+G(8)+G(9)+BUDGETPB ! change balance on bill if they have a budget (show as current months charges plus andy prior budget amounts not paid
02230 L2230: if g(8)=0 then goto L2240 else !:
          print #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02240 L2240: if g(9)=0 then goto L2250 else !:
          print #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          print #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02250 L2250: ! If PB><0 Then Print #20: 'Call Print.AddLine('&STR$(XMARGIN+46)&','&STR$(LYNE*(METER+=1)+YMARGIN)&',15,0)'
02280   print #20: 'Call Print.MyFontSize(10)'
02290 ! ______________________________________________________________________
02300   if estimatedate=d1 then print #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02310   print #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*24+1+ymargin)&',63,0)'

02320   print #20: 'Call Print.AddText("   Pay By  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
02330   if havebudget=1 then print #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')': goto L2350
02340   print #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')'
02350 L2350: print #20: 'Call Print.AddText("Pay After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*26+ymargin)&')'
02360   if havebudget=1 and payby<=0 then print #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420 ! owe current gas budget plus other services Plus any old budgets not paid
02370   if havebudget=1 and payby>0 then print #20: 'Call Print.AddText("'&fnformnumb$((payby)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420 ! owe current gas budget plus other services Plus any old budgets not paid
02380   if bal<=0 then print #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420
02390   if a(5)=0 and extra11=0 and extra12=0 then print #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')' : goto L2420
02400   if havebudget=0 then print #20: 'Call Print.AddText("'&fnformnumb$(min(g(5)+g(6)+g(7)+bal,bal+max(0,round(bal*penalty_rate,2))),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
02410   if havebudget=1 and bal>0 and g(7)>0 then print #20: 'Call Print.AddText("'&fnformnumb$((currentcharges+budgetpb)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
02412   if havebudget=1 and bal>0 and g(7)=0 then print #20: 'Call Print.AddText("'&fnformnumb$((currentcharges+budgetpb),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*26+ymargin)&')'
02420 L2420: print #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*27+1+ymargin)&',63,0)'
02430   print #20: 'Call Print.AddText("Office 756-8997 Fire 756-3110",'&str$(xmargin+1)&','&str$(lyne*28+ymargin)&')'
02440   print #20: 'Call Print.AddText("      Police 756-3311",'&str$(xmargin+1)&','&str$(lyne*29.5+ymargin)&')'
02450 ! ______________________________________________________________________
02460   let special=28
02470 ! ______________________________________________________________________
02480   fnpa_fontsize(7)
02490   print #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02500   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02510   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02520   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02530   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02540   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02550   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02560   print #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02570   print #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
02580   print #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02590   print #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02600   print #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02610   print #20: 'Call Print.AddText("  Permit No 1",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02620   fnpa_fontsize(9)
02630 ! Print #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02640   print #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
02650   print #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
02660   fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin) ! print #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
02670   fnpa_fontsize ! print #20: 'Call Print.MyFontSize(10)'
02680   fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin) ! print #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
02690   if havebudget=1 then 
02692     print #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02694   else
02700     print #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
02702   end if
02710   L2710: !
02712   print #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
02720   if havebudget=1 and payby=<0 then print #20: 'Call Print.AddText("'&fnformnumb$(payby,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')': goto L2750
02730   if havebudget=1 and payby>0 and g(7)>0 then print #20: 'Call Print.AddText("'&fnformnumb$((payby)+round((currentcharges-gas+g(4))*penalty_rate,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')' : goto L2750
02740   if g(5)+g(6)+g(7)>0 then print #20: 'Call Print.AddText("'&fnformnumb$(bal+round(currentcharges*penalty_rate,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
02750 L2750: fnpa_fontsize(9)
02760   let addy=14
02770   let fnpa_text(20,mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
02780   let fnpa_text(20,mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
02790   let fnpa_text(20,mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
02800   let addy+=1
02810   print #20: 'Call Print.MyFontSize(10)'
02820   if df$="Y" then !:
          print #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02830   if c4>0 then !:
          print #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02840   print #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02850   if pe$(1)<>"" then !:
          print #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02860   if pe$(2)<>"" then !:
          print #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02870   if pe$(3)<>"" then !:
          print #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02880   if pe$(4)<>"" then !:
          print #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
02890   if checkcounter=1 then let checkx=1.375 : let checky=3.6875
02900   if checkcounter=2 then let checkx=6.75 : let checky=3.6875
02910   if checkcounter=3 then let checkx=1.375 : let checky=7.9375
02920   if checkcounter=0 then let checkx=6.75 : let checky=7.9375
02950   if checkcounter=0 then !:
          let fnpa_newpage
02960   return 
02970 ! ______________________________________________________________________
02980 BULKSORT: ! bulk sort order
02990   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
03000   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03010 L3010: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3040
03020   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03030   goto L3010
03040 L3040: close #1: ioerr ignore
03050   close #6: ioerr ignore
03060   execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\Tempidx."&wsid$&" 1,19,Replace,DupKeys -n" ioerr L3080
03070   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\Tempidx."&wsid$,internal,input,keyed 
03080 L3080: return 
03082 IGNORE: continue 
03090 BUD1: let bud1=0
03100   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5)
03110   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr EO_BUD1
03120   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
03130   let bud1=1
03140 EO_BUD1: return 
03150 ! ______________________________________________________________________
03160 BUD2: ! 
03170   let totba=bd1=bd2=budgetpb=havebudget=00
03180   mat bd1(5) : mat bd1=(0) : mat bd2=(0)
03190   if bud1=0 then goto EO_BUD2
03200   read #81,using L3230,key=z$: z$,mat ba,mat badr nokey EO_BUD2
03210   let havebudget=1
03220   for j=2 to 12: let totba=totba+ba(j): next j
03230   L3230: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
03240   if totba=0 then let havebudget=0: goto EO_BUD2
03250   let ta1=badr(1)
03260   L3260: if ta1=0 then goto EO_BUD2
03270   read #82,using L3280,rec=ta1: z$,mat bt1,nba norec EO_BUD2
03280   L3280: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
03290   if bt1(14,1)>0 then goto L3340
03300   ! IF BT1(1,2)=F THEN GOTO 3350 ! ignore current budget billing record
03310   let budgetpb=budgetpb+bt1(5,1) ! add up prior balance for budget billing customers (any unpaid not counting current bill
03320   let bd1=bd1+1
03330   if bd1>5 then goto EO_BUD2
03340   L3340: let ta1=nba : goto L3260
03350   EO_BUD2: ! 
03360 return 
