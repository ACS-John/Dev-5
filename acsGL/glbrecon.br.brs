00010 ! Replace S:\acsGL\glBRecon
00020 ! Bank Reconciliation File  (maintains the bank rec file)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fndat,fnerror, fnopenprn,fncloseprn,fndate_mmddyy_to_ccyymmdd,fnchain,fnindex_it
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl2$(7),sc2$(7)*38
00080   dim flo2$(3),sa$(2),sb$(3),sc$(9),sd$(7),se$(7)*20,sf$(4)
00090   dim cnam$*40,dat$*20,cap$*128
00100   dim gl$*12,c$*12,p$*30,s$*2,a(3),dcode$*24,glc$*24,holdgc$*24,currgl$*12
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Bank Reconciliation")
00130   fntop("S:\acsGL\glBRecon",cap$="Bank Reconciliation")
00140   fncno(cno,cnam$)
00150   fndat(dat$)
00160   for j=1 to 7: fl2$(j)=str$(j+3)&",2,C 38": next j
00170   sc2$(1)="1. Initial File Preparation"
00180   sc2$(2)="2. Add"
00190   sc2$(3)="3. Edit"
00200   sc2$(4)="4. Enter Cleared Checks"
00210   sc2$(5)="5. pr Bank Reconciliation"
00220   sc2$(6)="6. Listing of Checks"
00230   sc2$(7)="7. Change Bank Account Number"
00240   fl2$(3)="6,2,C 38,C,N"
00250   flo2$(1)="5,31,C 3,UT,N"
00260   flo2$(2)="5,35,C 6,UT,N"
00270   flo2$(3)="5,42,C 3,UT,N"
00280   sc$(1)="5,21,G 3,UT,N"
00290   sc$(2)="5,25,G 6,UT,N"
00300   sc$(3)="5,32,G 3,UT,N"
00310   sc$(4)="6,23,c 12,UT,N"
00320   sc$(5)="7,16,C 30,UT,N"
00330   sc$(6)="8,22,C 2,UT,N"
00340   sc$(7)="9,22,N 6,UT,N"
00350   sc$(8)="10,22,N 10.2,UT,N"
00360   sc$(9)="11,31,N 1,UT,N"
00370   for j=1 to 7: sd$(j)=str$(j+4)&",9,C 20" : next j
00380   se$(1)="G/L Number:"
00390   se$(2)="Check Number:"
00400   se$(3)="Payee:"
00410   se$(4)="Source:"
00420   se$(5)="Date:"
00430   se$(6)="Amount:"
00440   se$(7)="Cleared Bank (1=YES)"
00450 OPEN_FILES: ! 
00460   open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLRecIdx.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2660
00470   goto L2390
00480 L480: pr newpage
00490   close #101: ioerr L500
00500 L500: open #101: "SROW=3,SCOL=20,EROW=13,ECOL=59,BORDER=DR,CAPTION=<Bank Reconciliation File",display,outin 
00510   pr #101,fields "1,1,Cc 40,R,N": cnam$
00520   pr #101,fields "2,1,Cc 40,R,N": "Company Number "&str$(cno)
00530   pr f "14,35,C 09,B,5": "Exit (F5)"
00540   rinput #101,select mat fl2$,attr "H": mat sc2$
00550   scode=curfld
00560   if cmdkey=5 then goto L2610
00570 L570: on scode goto L590,L770,L810,L1460,L1760,L1770,L2390
00580 ! ______________________________________________________________________
00590 L590: pr newpage
00600   close #101: ioerr L610
00610 L610: open #101: "SROW=5,SCOL=20,EROW=15,ECOL=59,BORDeR=SR,CAPTION=Initial File Preparation",display,outin 
00620   pr #101,fields "1,1,Cc 40,R,N": cnam$
00630   pr #101,fields "2,1,Cc 40,R,N": "Company Number "&str$(cno)
00640   pr #101,fields "4,1,Cc 40,R,N": "* * *   WARNING   * * *"
00650   pr #101,fields "5,1,Cc 40,N": "This selection will destroy all"
00660   pr #101,fields "6,1,Cc 40,N": "existing Bank Reconciliation records"
00670   pr f "16,34,C 11,B,5": "Cancel (F5)"
00680   pr #101,fields "8,2,C 24,N": "Enter ERASE to continue:"
00690 L690: input #101,fields "8,27,Cu 5,UT,N": pas$
00700   if cmdkey=5 then goto L480
00710   if pas$><"ERASE" then goto L690
00720   let new1=1
00730   close #1: ioerr L740
00740 L740: open #1: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&str$(cno)&",Size=0,RecL=68,Replace",internal,outin,relative 
00750   goto L2620
00760 L760: form pos 1,n 3,n 6,n 3,c 12,c 30,c 2,n 6,pd 5.2,n 1
00770 L770: let new1=1
00780   goto L1000
00790 ! ______________________________________________________________________
00800 L800: form pos 1,c 12,c 12
00810 L810: sf$(1)="11,10,N 3,UT,N"
00820   sf$(2)="11,14,N 6,UT,N"
00830   sf$(3)="11,21,N 3,UT,N"
00840   sf$(4)="11,30,C 12,CUE,N"
00850   pr newpage
00860   close #101: ioerr L870
00870 L870: open #101: "SROW=9,SCOL=8,EROW=12,ECOL=43,BORDER=SR,CAPTION=<Maintain Bank Records",display,outin 
00880   pr #101: newpage
00890   pr f mat sf$: val(currgl$(1:3)),val(currgl$(4:9)),val(currgl$(10:12)),""
00900   pr f "10,10,C 14,R,N": "Bank GL Number"
00910   pr f "10,30,C 12,R,N": "Check Number"
00920   pr f "13,35,C 09,B,5": "Stop (F5)"
00930 L930: input fields mat sf$: gl1,gl2,gl3,c$ conv L930
00940   if cmdkey=5 then goto L480
00950   c$=lpad$(rtrm$(c$),12)
00960   let dcode$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$
00970   read #1,using L980,key=dcode$: gl$,c$,p$,s$,mat a nokey L930
00980 L980: form pos 1,c 12,c 12,c 30,c 2,n 6,pd 5.2,n 1
00990   let holdgc$=gl$&c$
01000 L1000: pr newpage
01010   close #101: ioerr L1020
01020 L1020: open #101: "SROW=2,SCOL=8,EROW=12,ECOL=49,Border=Sr,Caption=<Maintain Bank Reconciliation Records",display,outin 
01030   pr #101: newpage
01040   pr f "3,10,Cc 38,R,N": "Enter Check Number as 0 to Delete"
01050   pr f mat sd$: mat se$
01060   pr f "13,30,C 09,B,1": "Next (F1)"
01070   pr f "13,41,C 09,B,5": "Stop (F5)"
01080   if scode=2 then goto L1110
01090   pr f mat sc$: gl$(1:3),gl$(4:9),gl$(10:12),c$,p$,s$,mat a
01100   ce=5: goto L1170
01110 L1110: pr f mat sc$: currgl$(1:3),currgl$(4:9),currgl$(10:12)
01120   ce=4: goto L1170
01130 L1130: input fields mat sc$: gl1,gl2,gl3,c$,p$,s$,mat a conv L1130
01140   if ce>0 then sc$(ce)(ce1:ce2)="U": ce=0
01150   if cmdkey>0 then goto L1220 else ce=curfld+1
01160   if ce>udim(sc$) then ce=1
01170 L1170: sc$(ce)=rtrm$(uprc$(sc$(ce))) : ce1=pos(sc$(ce),"U",1)
01180   ce2=ce1+1 : sc$(ce)(ce1:ce1)="UC" : goto L1130
01190 CONVC: if ce>0 then sc$(ce)(ce1:ce2)="U"
01200   ce=cnt+1
01210 ERRC: pr f "24,78,C 1": bell : goto L1170
01220 L1220: if scode=2 and (ltrm$(c$)="" or ltrm$(c$)="0") then goto L480
01230   c$=lpad$(rtrm$(c$),12)
01240   if a(1)<10100 or a(1)>123199 then ce=7: goto ERRC
01250   if a(3)<0 or a(3)>1 then ce=9: goto ERRC
01260   if scode=2 then goto L1380
01270   if ltrm$(c$)="" or ltrm$(c$)="0" then goto L1280 else goto L1350
01280 L1280: pr f "18,10,C 40,R,N": hex$("07")&" ENTER 1 TO DELETE; ENTER 2 TO RE-ENTER"
01290 L1290: input fields "18,51,N 1,UE,N": dcode conv L1290
01300   on dcode goto L1330,L1310 none L1290
01310 L1310: c$=holdgc$(13:24)
01320   goto L1000
01330 L1330: delete #1,key=holdgc$: 
01340   goto L1440
01350 L1350: if holdgc$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$ then goto L1360 else goto L1380
01360 L1360: rewrite #1,using L760,key=gl$&c$: gl1,gl2,gl3,c$,p$,s$,mat a
01370   goto L570
01380 L1380: let dcode$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)&c$
01390   read #1,using L800,key=dcode$: gl$,c$ nokey L1410
01400   ce=3: goto ERRC
01410 L1410: if scode=2 then goto L1430
01420   delete #1,key=holdgc$: 
01430 L1430: write #1,using L760: gl1,gl2,gl3,c$,p$,s$,mat a
01440 L1440: let new1=1
01450   goto L570
01460 L1460: sa$(1)="10,43,C 12,UT,N"
01470   sa$(2)="12,43,N 12.2,UT,N"
01480 L1480: pr newpage
01490   close #101: ioerr L1500
01500 L1500: open #101: "SROW=8,SCOL=24,EROW=13,ECOL=55,BORDER=DR,CAPTION=<Enter Cleared Checks",display,outin 
01510   pr f "8,24,C 32,R,N": "      Bank #: "&currgl$
01520   pr f "10,25,C 25": "  Check Number:"
01530   pr f "12,25,C 25": "Amount to Verify:"
01540   pr f "14,35,C 09,B,5": "Stop (F5)"
01550 L1550: input fields mat sa$: c$,amt conv L1550
01560   if cmdkey=5 then goto L480
01570   c$=lpad$(rtrm$(c$),12)
01580   pr f "15,5,C 70,N": ""
01590   pr f "16,5,C 70,N": ""
01600   if ltrm$(c$)="" or ltrm$(c$)="0" then goto L480
01610   read #1,using L1620,key=currgl$&c$: a2,a3 nokey L1740
01620 L1620: form pos 63,pd 5.2,n 1
01630   if a3=1 then pr f "16,10,C 54,R,N": hex$("07")&" Check Number "&c$&" has been coded as cleared" else goto L1650
01640   goto L1550
01650 L1650: if amt=0 then goto L1710
01660   if amt=a2 then goto L1710
01670   pr f "16,5,C 75,R,N": hex$("07")&" Amount entered does not equal amount on file!"
01680   pr f "17,5,C 70,R,N": " Enter 1 to code as cleared or 2 to bypass"
01690 L1690: input fields "17,75,N 1,UE,N": goon conv L1690
01700   on goon goto L1710,L1480 none L1690
01710 L1710: rewrite #1,using L1720,key=currgl$&c$: 1
01720 L1720: form pos 68,n 1
01730   goto L1480
01740 L1740: pr f "16,10,C 69,R,N": hex$("07")&" Check Number "&rtrm$(c$)&" in Bank Number "&ltrm$(currgl$)&" is not on file"
01750   goto L1550
01760 L1760: let fnchain("S:\acsGL\glCkRec")
01770 L1770: pr newpage
01780   sb$(1)="2,23,C 20,UT,N"
01790   sb$(2)="3,37,Nz 6,UT,N"
01800   sb$(3)="4,37,Nz 6,UT,N"
01810   close #101: ioerr L1820
01820 L1820: open #101: "SROW=9,SCOL=9,EROW=15,ECOL=71,BORDER=DR,CAPTION=<Print Listing of Checks written for Bank # "&ltrm$(currgl$),display,outin 
01830   pr #101: newpage
01840   pr #101,fields "2,2,C 20,N": "Report Heading Date:"
01850   pr #101,fields "3,2,Cr 34,N": "Starting Date to appear on report:"
01860   pr #101,fields "4,2,Cr 34,N": "Ending Date to appear on report:"
01870   pr f "16,30,C 09,B,1": "Next (F1)"
01880   pr f "16,41,C 09,B,5": "Stop (F5)"
01890 L1890: rinput #101,fields mat sb$: dat$,begdat,enddat conv L1890
01900   if ce>0 then sb$(ce)(ce1:ce2)="U": ce=0
01910   if cmdkey>0 then goto L1980 else ce=curfld+1
01920   if ce>udim(sb$) then ce=1
01930 L1930: sb$(ce)=rtrm$(uprc$(sb$(ce))) : ce1=pos(sb$(ce),"U",1)
01940   ce2=ce1+1 : sb$(ce)(ce1:ce1)="UC" : goto L1890
01950 CONVB: if ce>0 then sb$(ce)(ce1:ce2)="U"
01960   ce=cnt+1
01970 ERRB: pr f "24,78,C 1": bell : goto L1930
01980 L1980: if cmdkey=5 then goto L480
01990   if fndate_mmddyy_to_ccyymmdd(begdat)>fndate_mmddyy_to_ccyymmdd(enddat) then goto L1890
02000   let namtab=66-int(len(rtrm$(cnam$))/2)
02010   let dattab=66-int(len(rtrm$(dat$))/2)
02020   pr newpage
02030   close #101: ioerr L2040
02040 L2040: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDeR=SR,CAPTION=<Print Listing of Checks",display,outin 
02050   pr #101: newpage
02060   pr #101,fields "1,1,Cc 41,R,N": cnam$
02070   pr #101,fields "2,1,Cc 41,R,N": "Company Number "&str$(cno)
02080   pr #101,fields "4,1,Cc 41,N": "Printing..."
02090   pr f "13,34,C 11,B,5": "Cancel (F5)"
02100   on fkey 5 goto L2250
02110   restore #1,key>=currgl$&"            ": nokey L2370
02120   fnopenprn(cp,58,220,process)
02130   gosub L2290
02140 L2140: read #1,using L980: gl$,c$,p$,s$,mat a eof L2250
02150   if currgl$=gl$ then goto L2160 else goto L2250
02160 L2160: if fndate_mmddyy_to_ccyymmdd(a(1))<fndate_mmddyy_to_ccyymmdd(begdat) or fndate_mmddyy_to_ccyymmdd(a(1))>fndate_mmddyy_to_ccyymmdd(enddat) then goto L2140
02170   if a(3)=1 then pr #255,using L2180: c$,p$,a(2),a(1),s$,"Yes" pageoflow L2210 else pr #255,using L2180: c$,p$,a(2),a(1),s$,"No" pageoflow L2210
02180 L2180: form pos 1,c 12,pos 15,c 30,pos 48,n 11.2,pos 65,pic(zz/zz/zz),pos 75,c 2,pos 83,c 3,skip 1
02190 L2190: let tot=tot+a(2)
02200   goto L2140
02210 L2210: pr #255: newpage
02220   gosub L2290
02230   goto L2190
02240 ! ______________________________________________________________________
02250 L2250: pr #255,using L2260: "------------",tot,"============"
02260 L2260: form pos 47,c 12,skip 1,pos 47,n 12.2,skip 1,pos 47,c 12,skip 1
02270   goto L2350
02280 ! ______________________________________________________________________
02290 L2290: pr #255,using L2300: cnam$,"Report of Checks Written",currgl$(1:3),currgl$(4:9),currgl$(10:12),dat$
02300 L2300: form pos namtab,c 40,skip 1,pos 54,c 24,skip 1,pos 59,c 3,x 1,c 6,x 1,c 3,skip 1,pos dattab,c 20,skip 2
02310   pr #255,using L2320: "Check Number","Payee","Amount","Date","Source","Cleared"
02320 L2320: form pos 1,c 12,pos 15,c 5,pos 53,c 6,pos 67,c 4,pos 73,c 6,pos 81,c 7,skip 2
02330   return 
02340 ! ______________________________________________________________________
02350 L2350: let fncloseprn
02360   on fkey 5 ignore 
02370 L2370: let tot=0
02380   goto L480
02390 L2390: pr newpage
02400   sb$(1)="2,31,N 3,UT,N"
02410   sb$(2)="2,35,N 6,UT,N"
02420   sb$(3)="2,42,N 3,UT,N"
02430   close #101: ioerr L2440
02440 L2440: open #101: "SROW=9,SCOL=18,EROW=11,ECOL=62,BORDER=DR,CAPTION=<Working Bank Account Number",display,outin 
02450   pr #101: newpage
02460   pr f "12,30,C 09,B,1": "Next (F1)"
02470   pr f "12,41,C 09,B,5": "Exit (F5)"
02480   pr #101,fields "2,2,C 28,N": "Working Bank Account Number:"
02490 L2490: input #101,fields mat sb$: gl1,gl2,gl3 conv L2490
02500   if ce>0 then sb$(ce)(ce1:ce2)="U": ce=0
02510   if cmdkey>0 then goto L2580 else ce=curfld+1
02520   if ce>udim(sb$) then ce=1
02530 L2530: sb$(ce)=rtrm$(uprc$(sb$(ce))) : ce1=pos(sb$(ce),"U",1)
02540   ce2=ce1+1 : sb$(ce)(ce1:ce1)="UC" : goto L2490
02550 CONV1: if ce>0 then sb$(ce)(ce1:ce2)="U"
02560   ce=cnt+1
02570 ERR1: pr f "24,78,C 1": bell : goto L2530
02580 L2580: if cmdkey=5 then goto XIT
02590   currgl$=lpad$(str$(gl1),3)&lpad$(str$(gl2),6)&lpad$(str$(gl3),3)
02600   goto L480
02610 L2610: if new1=1 or z1=1 then goto L2620 else goto XIT
02620 L2620: close #1: 
02630   fnindex_it(env$('Q')&"\GLmstr\GLBREC.h"&env$('cno'),env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno'),"1 24")
02640   if z1=1 then goto OPEN_FILES else goto XIT
02650 ! ______________________________________________________________________
02660 L2660: if err=4152 then goto L740 else goto ERTN
02670 ! ______________________________________________________________________
02680 XIT: let fnxit
02690 ! ______________________________________________________________________
02700 ! <Updateable Region: ERTN>
02710 ERTN: let fnerror(program$,err,line,act$,"xit")
02720   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02730   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02740   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02750 ERTN_EXEC_ACT: execute act$ : goto ERTN
02760 ! /region
