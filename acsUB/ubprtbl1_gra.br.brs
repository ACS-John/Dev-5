00010 ! Replace S:\acsUB\ubprtbl1_gra
00020 ! pr bills for Village of Grandview
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_txt,fnpa_newpage,fnpa_open
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim resp$(10)*50,txt$*45,mg$(3)*30,rw(22,13),cap$*128
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
00100   dim datafile$*256,indexfile$*256
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00130   open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  !:
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
00170   fntop("S:\acsUB\ubprtbl1",cap$="Bills-Laser (4 per page)")
00180   gosub BULKSORT
00190   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.H[cno],Shr",internal,input,keyed  ! open in Account order
00200   open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.H[cno],Shr",internal,input,keyed  ! open in route-sequence #
00220 ! ______________________________________________________________________
00230 SCREEN1: ! 
00240   a$="" : prtbkno=0
00250   fnTos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00260   fnLbl(3,1,"Penalty Due Date:",ll,1)
00270   fnTxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00280   fnLbl(4,1,"Message on Bill:",ll,1)
00290   fnTxt(4,pf,30,30) !:
        resp$(respc+=1)=mg$(1)
00300   fnTxt(5,pf,30,30) !:
        resp$(respc+=1)=mg$(2)
00310   fnTxt(6,pf,30,30) !:
        resp$(respc+=1)=mg$(3)
00320   fnLbl(7,1,"Date of Billing:",ll,1)
00330   fnTxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00340   fnLbl(8,1,"Starting Account:",ll,1)
00350   fe$="ubm-act-nam" !:
        datafile$="[Q]\UBmstr\Customer.h[cno]" !:
        indexfile$="[Q]\UBmstr\ubindx5.h[cno]" !:
        kp=1741: kl=9 : dp=41 : dl=30 !:
        fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) !:
        resp$(respc+=1)="[All]"
00360   fnLbl(9,1,"Route Number:",ll,1)
00370   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00380   fnChk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00390   fnCmdSet(3) !:
        fnAcs(sn$,0,mat resp$,ck)
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
00510   open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.H[cno],Shr",internal,input,keyed 
00520   gosub VBOPENPRINT
00530 ! ______________________________________________________________________
00540   on fkey 5 goto RELEASE_PRINT
00550 L550: if sl1=1 then goto SCREEN3
00560 L560: read #6,using L590: z$ eof RELEASE_PRINT
00570   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
00580   begin=0 ! cancel starting account
00590 L590: form pos 22,c 10
00600   read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$ nokey L560
00610 L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30
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
00830 ! ______________________________________________________________________
00840 RELEASE_PRINT: ! 
00850   close #1: ioerr L860
00860 L860: close #3: ioerr L870
00870 L870: fnpa_finis
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
        fnTos(sn$)
01030   txt$="Account (blank to stop)" !:
        fnLbl(1,1,txt$,31,1)
01040 ! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
01050   if trim$(z$)<>"" then !:
          txt$="Last Account entered was "&z$ !:
          fnLbl(3,1,txt$,44,1) else !:
          txt$="" !:
          fnLbl(3,1,txt$,44,1)
01060   fncmbact(1,17) ! !:
        resp$(1)=a$
01070   fnCmdSet(3): fnAcs(sn$,0,mat resp$,ck)
01080   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01090   if ck=5 then goto RELEASE_PRINT
01100   read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$ nokey SCREEN3
01110   goto READALTADR
01120 ! ______________________________________________________________________
01130 SORT1: ! SELECT & SORT
01140   open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\CASS1IDX.H[cno],Shr",internal,input,keyed ioerr L1390
01150   open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=19",internal,output 
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
01320   write #9,using L1310: "File "&env$('Temp')&"\Temp."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
01330   write #9,using L1310: "Mask 1,19,C,A"
01340   close #9: 
01350   execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1360
01360 L1360: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
01370   open #6: "Name="&env$('Temp')&"\Temp."&session$,internal,input,relative 
01380   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
01390 L1390: return 
01400 ! ______________________________________________________________________
01410 ENDSCR: ! pr totals screen
01420   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01430   fnTos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01440   fnLbl(1,1,"Total Bills Printed:",mylen,1)
01450   fnTxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01520   fnCmdSet(52) !:
        fnAcs(sn$,0,mat resp$,ck)
01530 XIT: fnxit
01540 IGNORE: continue 
01550 ERTN: fnerror(program$,err,line,act$,"xit")
01560   if uprc$(act$)<>"PAUSE" then goto L1590
01570   execute "list -"&str$(line) !:
        pause  !:
        goto L1590
01580   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01590 L1590: execute act$
01600   goto ERTN
01610 ! ______________________________________________________________________
01620 VBOPENPRINT: ! r:
01640     fnPa_open("Landscape")
01670     lyne=3
01700   return  ! /r
01710 ! ______________________________________________________________________
01720 VBPRINT: ! 
01722   xmod_for_right=5
01730 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
01740   checkcounter+=1
01750   if checkcounter=1 then xmargin=0 : ymargin=0
01760   if checkcounter=2 then xmargin=145 : ymargin=0 ! xmargin=140
01770   if checkcounter=3 then xmargin=0 : ymargin=113 ! ymargin=108
01780   if checkcounter=4 then xmargin=145 : ymargin=113 ! xmargin=140 : ymargin=108
01782   if checkcounter=4 then checkcounter=0
01790 ! ______________________________________________________________________
01800   pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',55,'&str$(lyne*3+3)&',True)'
01810   pr #20: "Call Print.MyFontBold(True)"
01820   pr #20: 'Call Print.MyFontSize(12)'
01830   pr #20: 'Call Print.MyFont("Courier New")'
01840   fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
01850   pr #20: 'Call Print.MyFont("Lucida Console")'
01860   pr #20: 'Call Print.MyFontSize(10)'
01870   pr #20: 'Call Print.MyFontBold(False)'
01880   fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
01890   fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
01900   pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
01910   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
01920   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin)&','&str$(lyne*7+ymargin)&')'
01930   pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
01940   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
01950   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
01960   pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
01970   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
01980   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+48)&','&str$(lyne*13+ymargin)&')'
01990 ! ______________________________________________________________________
02000 PRINTGRID: meter=14 !:
        pr #20: 'Call Print.MyFontSize(8)'
02010   if g(1)=0 then goto L2020 else !:
          pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02020 L2020: if g(2)=0 then goto L2030 else !:
          pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02030 L2030: if g(3)=0 then goto L2040 else !:
          pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+23)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02040 L2040: if a4=1 then gcode$="RSGS" else !:
          if a4=2 then gcode$="CMGS" else !:
            if a4=3 then gcode$="INGS" else !:
              gcode$="GAS"
02050   if g(4)=0 then goto L2060 else !:
          pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+23)&','&str$(lyne*meter+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02060 L2060: if g(5)=0 then goto L2070 else !:
          pr #20: 'Call Print.AddText("MET",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02070 L2070: if g(6)=0 then goto L2080 else !:
          pr #20: 'Call Print.AddText("SUR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02080 L2080: if g(7)=0 then goto L2090 else !:
          pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02090 L2090: if g(8)=0 then goto L2100 else !:
          pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02100 L2100: if g(9)=0 then goto L2110 else !:
          pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02110 L2110: if pb=0 then goto L2120 else !:
          pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' !:
          pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
02120 L2120: pr #20: 'Call Print.MyFontSize(10)'
02130 ! ______________________________________________________________________
02140   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
02150   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
02160   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+40)&','&str$(lyne*24+ymargin)&')'
02170   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
02180   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+40)&','&str$(lyne*25+ymargin)&')'
02190   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
02200   pr #20: 'Call Print.AddText("Phone: 217-528-7624",'&str$(xmargin+1)&','&str$(lyne*27+ymargin)&')'
02210 ! ______________________________________________________________________
02220   special=28
02230 ! ______________________________________________________________________
02240   pr #20: 'Call Print.MyFontSize(7)'
02250   pr #20: 'Call Print.AddCircle('&str$(xmargin+78+xmod_for_right)&','&str$(lyne*2+3+ymargin)&','&"9,0"&')'
02260   pr #20: 'Call Print.AddText("Springfield",'&str$(xmargin+70+xmod_for_right)&','&str$(lyne*2-1+ymargin)&')'
02270   pr #20: 'Call Print.AddText("     IL    ",'&str$(xmargin+70+xmod_for_right)&','&str$(lyne*3-1+ymargin)&')'
02280   pr #20: 'Call Print.AddText("   62702   ",'&str$(xmargin+70+xmod_for_right)&','&str$(lyne*4-1+ymargin)&')'
02290   pr #20: 'Call Print.AddLine('&str$(xmargin+97+xmod_for_right)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
02300   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+0)&',7,0)'
02310   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+2.8)&',7,0)'
02320   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+5.6)&',7,0)'
02330   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+8.4)&',7,0)'
02340   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+11.2)&',7,0)'
02350   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+14)&',7,0)'
02360   pr #20: 'Call Print.AddLine('&str$(xmargin+90+xmod_for_right)&','&str$(ymargin+17)&',7,0)'
02370   pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100+xmod_for_right)&','&str$(lyne*1-1+ymargin)&')'
02380   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100+xmod_for_right)&','&str$(lyne*2-1+ymargin)&')'
02390   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100+xmod_for_right)&','&str$(lyne*3-1+ymargin)&')'
02400   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100+xmod_for_right)&','&str$(lyne*4-1+ymargin)&')'
02410   pr #20: 'Call Print.AddText("  Permit No 916",'&str$(xmargin+100+xmod_for_right)&','&str$(lyne*5-1+ymargin)&')'
02420   pr #20: 'Call Print.MyFontSize(9)'
02430 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
02440   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*7+ymargin)&')'
02450   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*8+ymargin)&')'
02460   pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*9+ymargin)&')'
02470   pr #20: 'Call Print.MyFontSize(10)'
02480   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*11+ymargin)&')'
02490   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+104+xmod_for_right)&','&str$(lyne*11+ymargin)&')'
02500   pr #20: 'Call Print.AddText("After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*12+ymargin)&')'
02510   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+104+xmod_for_right)&','&str$(lyne*12+ymargin)&')'
02520   pr #20: 'Call Print.MyFontSize(9)'
02530   addy=14
02540   fnpa_txt(mg$(1),xmargin+68+xmod_for_right,(addy+=1)*lyne+ymargin)
02550   fnpa_txt(mg$(2),xmargin+68+xmod_for_right,(addy+=1)*lyne+ymargin)
02560   fnpa_txt(mg$(3),xmargin+68+xmod_for_right,(addy+=1)*lyne+ymargin)
02570   addy+=1
02580   pr #20: 'Call Print.MyFontSize(10)'
02590   if df$="Y" then !:
          pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)
02600   if c4>0 then !:
          pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)
02610   if d(10)=1 then !:
          pr #20: 'Call Print.AddText("Bill Estimated",'&str$(xmargin+1+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)
02620   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)&')'
02630   if pe$(1)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)&')'
02640   if pe$(2)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)&')'
02650   if pe$(3)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)&')'
02660   if pe$(4)<>"" then !:
          pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68+xmod_for_right)&','&str$(lyne*(addy+=1)+ymargin)&')'
02670   if checkcounter=1 then checkx=1.375 : checky=3.6875
02680   if checkcounter=2 then checkx=6.75 : checky=3.6875
02690   if checkcounter=3 then checkx=1.375 : checky=7.9375
02700   if checkcounter=0 then checkx=6.75 : checky=7.9375
02710 ! bc$=""
02720 ! if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
02730   if checkcounter=0 then !:
          fnpa_newpage
02740   return 
02750 ! ______________________________________________________________________
02760 BULKSORT: ! bulk sort order
02770   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.H[cno],Shr",internal,input,keyed  ! open in Account order
02780   open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output 
02790 L2790: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2820
02800   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
02810   goto L2790
02820 L2820: close #1: ioerr ignore
02830   close #6: ioerr ignore
02840   execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('Temp')&"\Tempidx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2860
02850   open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('Temp')&"\Tempidx."&session$,internal,input,keyed 
02860 L2860: return 
