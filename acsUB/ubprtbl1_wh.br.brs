00010 ! Replace S:\acsUB\ubprtbl1_wh
00020 ! pr bills for Village of white hall
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fncmbrt2,fncombof,fnchk,fnerror,fnopt,fntos,fncmbact,fncno,fnLastBillingDate,fnxit,fncmdset,fntop,fnformnumb$,fnpause,fncmdkey,fnpa_finis,fnpa_open,fnpa_newpage
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*80,txt$*45,mg$(3)*30,rw(22,13),cap$*128,udf$*256
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
00100 ! ______________________________________________________________________
00110   fncno(cno,cnam$) !:
        fnLastBillingDate(d1) !:
        udf$=env$('temp')&'\'
00112   addr_indent=8 : addr_down=3
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
00170   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
00180   gosub BULKSORT ! want printed in alphabetic order
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.H"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
00192 !  open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx2.H"&env$('cno')&",Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
00193   open #8: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndEX.H"&env$('cno')&",Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
00200   open #2: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.H"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence #
00220 ! ______________________________________________________________________
00222   mg$(1)=''
00230 SCREEN1: ! 
00240   a$="" : prtbkno=0
00250   fntos(sn$="UBPrtBl1-1") !:
        pf=33 : ll=30 !:
        respc=0
00260   fnlbl(1,1,"Penalty Due Date:",ll,1)
00270   fntxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00280   fnlbl(2,1,"Message on Bill:",ll,1)
00290   fntxt(2,pf,30,30) !:
        resp$(respc+=1)=mg$(1)
00300 ! fntxt(5,pf,30,30) !:
        ! resp$(respc+=1)=mg$(2)
00310 !   fntxt(6,pf,30,30) !:
        !   resp$(respc+=1)=mg$(3)
00320   fnlbl(3,1,"Date of Billing:",ll,1)
00330   fntxt(3,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00340   fnlbl(4,1,"Starting Route/Sequence:",ll,1)
00350   fncombof("ubm-act-nam",4,pf,40,env$('Q')&"\UBmstr\Customer.h"&env$('cno'),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&env$('cno'),2)
00352   resp$(respc+=1)=""
00360   fnlbl(5,1,"Route Number:",ll,1)
00370   fncmbrt2(5,pf) !:
        resp$(respc+=1)="[All]"
00380   fnchk(6,pf,"Select Accounts to Print:",1) !:
        resp$(respc+=1)="False"
00382   fnlbl(8,1,"Service From Date:",ll,1)
00384   fntxt(8,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",0)
00386   fnlbl(9,1,"Service To Date:",ll,1)
00388   fntxt(9,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",0)
00410   fncmdset(3) !:
        fnacs(sn$,0,mat resp$,ck)
00420   if ck=5 then goto ENDSCR
00430   d4 = val(resp$(1))
00432   mg$(1) = resp$(2)
00434 ! mg$(2) = resp$(3)
00436 ! mg$(3) = resp$(4)
00438   d1 = val(resp$(3))
00440   if resp$(4)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(4)(1:9)),9)
00450   if resp$(5)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(5))
00460   if resp$(6)="True" then sl1=1: z$="" else sl1=0
00462   pr service_from=val(resp$(7)) : service_to=val(resp$(8))
00466   if service_from<>0 and service_to<>0 then use_entered_dates=1 else use_entered_dates=0
00470   if trim$(a$)<>"" then read #2,using L500,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00500 L500: form pos 1,c 10,pos 1741,n 2,n 7
00510   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00520   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00530   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00540 ! ______________________________________________________________________
00550   open #h_adrbil:=3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.H"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00560   gosub BUD1
00570   gosub VBOPENPRINT
00580 ! ______________________________________________________________________
00600 L600: if sl1=1 then goto SCREEN3
00610 L610: read #6,using 'form pos 22,c 10': z$ eof RELEASE_PRINT
00612   if prtbkno>val(trim$(z$(1:2))) then goto L610 ! fixes select route for pr problem
00620   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L610 ! start with
00630   begin=0 ! cancel starting account
00650   read #1,using F_CUSTOMER,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4,extra_22 nokey L610
00655 ! read #1,using F_CUSTOMER: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4 eof RELEASE_PRINT
00660   if d3=0 then d3=extra_3
00670   if d2=0 then d2=extra_4
00680 F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1750,2*n 6,pos 1854,pd 5.2
00690   if prtbkno=0 then goto L710
00700   if prtbkno><route then goto L610
00710 L710: if f><d1 then goto L600
00720   if st1=0 then goto READALTADR
00730 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00740 READALTADR: ! 
00750 ! read alternate billing address
00752   if extra_22<>1 then goto L840
00760   read #h_adrbil,using L770,key=z$: mat ba$ nokey L840
00770 L770: form pos 11,4*c 30
00780   e1=0 : mat pe$=("")
00790   for j=1 to 4
00800     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00810   next j
00820   goto L1000
00830 ! ______________________________________________________________________
00840 L840: e1=0 : mat pe$=("")
00850   for j=2 to 4
00860     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00870   next j
00880   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00890   goto L1000
00900 ! ______________________________________________________________________
00905 RELEASE_PRINT: ! r:
00910   close #6: ioerr ignore
00915   close #1: ioerr ignore
00920   close #h_adrbil: ioerr ignore
00925 ! 
00930   fnpa_finis
00955   goto ENDSCR ! /r
00990 ! ______________________________________________________________________
01000 L1000: ! 
01010   if bud1=1 then gosub BUD2
01020   pb=bal-g(11)
01030 ! If BAL<=0 Then g(10)=0 ! don't show penalty if balance 0 or less
01040 ! ______________print bill routine______________________________________
01050   gosub VBPRINT
01060 ! _____________end of pr routine______________________________________
01070   bct(2)=bct(2)+1 !:
        ! accumulate totals
01080   goto L600
01090 ! ______________________________________________________________________
01100 SCREEN3: ! 
01110   sn$ = "UBPrtBl1-2" !:
        fntos(sn$)
01120   txt$="Account (blank to stop)" !:
        fnlbl(1,1,txt$,31,1)
01130 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01140   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnlbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnlbl(3,1,txt$,44,1)
01150   fncmbact(1,17) ! !:
        resp$(1)=a$
01160   fncmdkey("&Next",1,1,0,"Accept this record for printing") !:
        fncmdkey("&Complete",5,0,1,"Print all selected records")
01170   fnacs(sn$,0,mat resp$,ck)
01180   a$=lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01190   if ck=5 then goto RELEASE_PRINT
01200   read #8,using F_CUSTOMER,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4,extra_22 nokey SCREEN3
01210   if d3=0 then d3=extra_3
01220   if d2=0 then d2=extra_4
01230   goto READALTADR
01240 ! ______________________________________________________________________
01250 SORT1: ! r: SELECT & SORT
01260   open #5: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr L1510
01270   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
01280   s5=1
01290   if prtbkno=0 then routekey$="" else !:
          routekey$=cnvrt$("N 2",prtbkno)&"       " !:
          ! key off first record in route (route # no longer part of customer #)
01300   restore #2,search>=routekey$: 
01310 L1310: read #2,using L1320: z$,f,route eof END5
01320 L1320: form pos 1,c 10,pos 296,pd 4,pos 1741
01330   if prtbkno=0 then goto L1350
01340   if prtbkno><route then goto END5
01350 L1350: if f><d1 then goto L1310
01360   zip5$=cr$=""
01370   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1380
01380 L1380: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
01390   goto L1310
01400 ! 
01410 END5: close #6: 
01420   open #9: "Name="&env$('temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
01430 L1430: form pos 1,c 128
01440   write #9,using L1430: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01450   write #9,using L1430: "Mask 1,19,C,A"
01460   close #9: 
01470   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
01480   execute "Sort "&env$('temp')&"\Control."&session$&" -n"
01490   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
01500   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01510 L1510: return  ! /r
01520 ! ______________________________________________________________________
01530 ENDSCR: ! pr totals screen
01540   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01550   fntos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01560   fnlbl(1,1,"Total Bills Printed:",mylen,1)
01570   fntxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01580 ! fnLBL(2,1,"Total  Bills  Coded:",MYLEN,1)
01590 ! fnTXT(2,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
01600 ! fnLBL(3,1,"Total Bills Not Coded:",MYLEN,1)
01610 ! fnTXT(3,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
01620 ! fnLBL(4,1,"Percent of Bills Coded:",MYLEN,1)
01630 ! fnTXT(4,MYPOS,8,0,1,"",1) !:
        ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
01640   fncmdset(52) !:
        fnacs(sn$,0,mat resp$,ck)
01650 XIT: fnxit
01655 IGNORE: continue 
01660 ! ______________________________________________________________________
01670 ERTN: fnerror(program$,err,line,act$,"xit")
01680   if uprc$(act$)<>"PAUSE" then goto L1710
01690   execute "list -"&str$(line) !:
        pause  !:
        goto L1710
01700   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01710 L1710: execute act$
01720   goto ERTN
01730 ! ______________________________________________________________________
01740 VBOPENPRINT: ! r:
01760     fnPa_open("Landscape")
01790     lyne=3
01820   return  ! /r
01830 ! ______________________________________________________________________
01840 VBPRINT: ! r:
01850 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01860   checkcounter+=1
01870   if checkcounter=1 then xmargin=0 : ymargin=0
01880   if checkcounter=2 then xmargin=137 : ymargin=0
01890   if checkcounter=3 then xmargin=0 : ymargin=108
01900   if checkcounter=4 then xmargin=137 : ymargin=108 : checkcounter=0
01910 ! ______________________________________________________________________
01920   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
01930   pr #20: "Call Print.MyFontBold(True)"
01940   pr #20: 'Call Print.MyFontSize(12)'
01950   pr #20: 'Call Print.MyFont("Courier New")'
01960   pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
01970   pr #20: 'Call Print.MyFont("Lucida Console")'
01980   pr #20: 'Call Print.MyFontSize(10)'
01990   pr #20: 'Call Print.MyFontBold(False)'
02000   pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
02010   pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
02020   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
02030   pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
02032   if ~use_entered_dates then 
02034     service_from=extra_4
02036     service_to=extra_3
02038   end if 
02040   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_from)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_to)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
02050   pr #20: 'Call Print.AddText("Due upon receipt",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
02052   pr #20: 'Call Print.AddText("'&e$(2)&'",'&str$(xmargin+2)&','&str$(lyne*9+ymargin)&')'
02060   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
02070   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
02071   pr #20: 'Call Print.MyFontSize(7)'
02080   pr #20: 'Call Print.AddText("Current",'&str$(xmargin+12+5)&','&str$(lyne*13+ymargin)&')'
02090   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+35+5)&','&str$(lyne*13+ymargin)&')'
02100   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+52+5)&','&str$(lyne*13+ymargin)&')'
02110 ! ______________________________________________________________________
02112 PRINTGRID: ! 
02114   meter=14 ! 02114   meter=20 ! lyne=2 ! 3 ! 2.15 !  started at 20 and 2.1
02116   pr #20: 'Call Print.MyFontSize(10)' ! 02116   pr #20: 'Call Print.MyFontSize(10)' ! line_top(1)
02118   if g(1)<>0 then 
02120     pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02122     pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
02124     pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
02126     pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02128     pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02130     pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02132   end if  ! g(1)<>0
02134   if g(2)<>0 then 
02136     pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02138     pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02140     pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02142     pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02144   end if  ! g(2)<>0
02146   if g(3)<>0 or d(7)<>0 then 
02148     pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02150     pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
02152     pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02154     pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02156     pr #20: 'Call Print.AddText("EL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02158     pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02160   end if  ! g(3)<>0 or d(7)<>0
02162 ! If A4=1 Then gCODE$="RSGS" Else If A4=2 Then gCODE$="CMGS" Else If A4=3 Then gCODE$="INGS" Else 
02164   gcode$="GAS"
02166   if g(4)<>0 then 
02168     pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02170     pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
02172     pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
02174     pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02176     pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02178     pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02180   end if  ! g(4)<>0
02182   if g(5)<>0 then 
02184     pr #20: 'Call Print.AddText("SL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02186     pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02188     pr #20: 'Call Print.AddText("SL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02190     pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02192   end if  ! g(5)<>0
02194   if g(6)<>0 then 
02196     pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02198     pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02200     pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02202     pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02204   end if  ! g(6)<>0
02206   if g(7)<>0 then 
02208     pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02210     pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02212     pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
02214     pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02216   end if  ! g(7)=0
02218   if g(8)<>0 then 
02220     pr #20: 'Call Print.AddText("Other",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02222     pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02224     pr #20: 'Call Print.AddText("Other",'&str$(xmargin+01)&','&str$(lyne*(meter)+ymargin)&')'
02226     pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02228   end if  ! g(8)<>0
02230   if g(9)<>0 then 
02232     pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
02234     pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
02236     pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin)&')'
02238     pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
02240   end if  ! g(9)<>0
02242 ! if pb><0 then
02244   pr #20: 'Call Print.AddLine('&str$(xmargin+49)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
02245   pr #20: 'Call Print.AddLine('&str$(xmargin+91+14)&','&str$(lyne*(meter)+ymargin+2)&',15,0)'
02246   pr #20: 'Call Print.AddText("   Net Bill",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
02250   pr #20: 'Call Print.AddText("Net",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin+2)&')'
02252 ! end if  ! pb><0
02254   pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
02256   pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
02258 ! If BUDGET>0 Then pB=PBUD ! owe old budget payment
02260   if pb then 
02262     pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
02264     pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
02266     pr #20: 'Call Print.AddText("Prior",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin+2)&')'
02268     pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
02270   end if  ! pb
02272   pr #20: 'Call Print.MyFontSize(10)' : lyne=3
02274 ! 
02276   if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
02278   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin+10)&',63,0)'
02280 ! pr #20: 'Call Print.AddText("Budget Payment",'&STR$(XMARGIN+68)&','&STR$(LYNE*12+YMARGIN)&')'
02282 ! If BUDGET>0 Then bAL=BUDGET+PBUD ! IF BUDGET MAKE NET DUE = BUDGET PLUS ANY OLD BUDGET PAYMENTS NOT MADE
02284   if budget>0 then 
02286     pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02288     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
02290     pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02292     pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
02293 !  if trim$(z$)='100100.00' then let fnpause
02294   else 
02296     pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
02298     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
02300     pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
02302     pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
02304   end if 
02306   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
02308 ! pr #20: 'Call Print.AddText("Phone: 217-665-3351",'&STR$(XMARGIN+1)&','&STR$(LYNE*27+YMARGIN)&')'
02310 ! pr #20: 'Call Print.AddText("Re-connect fee $??.00",'&STR$(XMARGIN+1)&','&STR$(LYNE*28+YMARGIN)&')'
02312 ! ______________________________________________________________________
02314   special=28
02316 ! ______________________________________________________________________
02318   pr #20: 'Call Print.MyFontSize(7)'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02322   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
02324   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
02326   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
02328   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
02330   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
02332   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
02334   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
02336 ! pr #20: 'Call Print.AddText("   Pre-Sorted",'&STR$(XMARGIN+100)&','&STR$(LYNE*1-1+YMARGIN)&')'
02338   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
02340   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
02342   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
02344   pr #20: 'Call Print.AddText("  Permit No 4",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
02346   pr #20: 'Call Print.MyFontSize(9)'
02348 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02350   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68+addr_indent)&','&str$(lyne*7+ymargin)&')'
02352   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68+addr_indent)&','&str$(lyne*8+ymargin)&')'
02354   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*9+ymargin)&')'
02356   pr #20: 'Call Print.MyFontSize(10)'
02358   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*11+ymargin)&')'
02360   if budget>0 then 
02364     pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
02368   else 
02370     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
02372     pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*12+ymargin)&')'
02374     pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*12+ymargin)&')'
02376   end if 
02377   pr #20: 'Call Print.MyFontSize(9)'
02378   addy=12
02380   pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02382 ! pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02384   pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
02386   addy+=1
02840   pr #20: 'Call Print.MyFontSize(10)'
02850   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
02860   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
02870   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+addr_indent+30)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
02880   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
02890   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
02900   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
02910   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
02912   pr #20: 'Call Print.AddText("Return Service Requested.",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=2)+ymargin+20+addr_down)&')'
02920   if checkcounter=1 then checkx=1.375 : checky=3.6875
02930   if checkcounter=2 then checkx=6.75 : checky=3.6875
02940   if checkcounter=3 then checkx=1.375 : checky=7.9375
02950   if checkcounter=0 then checkx=6.75 : checky=7.9375
02960   bc$=""
02970   if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02980   if checkcounter=0 then !:
          fnpa_newpage
02990   return  ! /r
03000 ! ______________________________________________________________________
03010 BULKSORT: ! r: bulk sort order
03020   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.H"&env$('cno')&",Shr",internal,input,keyed  ! open in Account order
03030   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
03040 L3040: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3070
03050   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
03060   goto L3040
03070 L3070: close #1: ioerr ignore
03080   close #6: ioerr ignore
03090   execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,21,Replace,DupKeys -n" ioerr L3110 ! 1,19 was changed to 22,10 to switch sorting to account
03100   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed 
03110 L3110: return  ! /r
03120 BUD1: ! r:
03122   bud1=0
03130   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
03140   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L3200
03150   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
03160   bud1=1
03170   for j=1 to 5
03180     bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
03190   next j
03200 L3200: return  ! /r
03210 BUD2: ! r:
03220   budget=pbud=bd1=0
03230   mat bd1(5)
03240   mat bd1=(0)
03250   mat bd2=(0)
03260   if bud1=0 then goto L3360
03270   read #81,using L3280,key=z$: z$,mat ba,mat badr nokey L3360
03280 L3280: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
03290   ta1=badr(1)
03300 L3300: if ta1=0 then goto L3360
03310   read #82,using L3320,rec=ta1: z$,mat bt1,nba norec L3360
03320 L3320: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
03330   if bt1(1,1)=d1 then budget=budget+bt1(12,1): goto L3350 ! budget for current month
03340 ! if bt1(14,1)=0 then pbud=pbud+bt1(12,1): goto L3350 ! budget for any previous months not paid
03350 L3350: ta1=nba : goto L3300
03360 L3360: return  ! /r
20000   def fn_pay_after_amt
20020     if bal<0 then let fn_pay_after_amt=round(bal,2) else let fn_pay_after_amt=round(bal*1.05,2)
20040   fnend  ! fn_pay_after_amt
