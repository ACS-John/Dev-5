00010 ! Replace S:\acsGL\VendorReas
00020 ! Vendor File - Transaction List
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnopenprn,fncloseprn,fncno,fndat,fnprocess,fnchain
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
00080   dim cnam$*40,dat$*20,adr(2),id1$*25,cap$*128
00090   dim rn$*12,de$*30,adr(2),tvn$*8
00100   dim flit$(4)*16,scrt$(4)*20,scid$*79,desc$(6)*14
00110   dim sc$(8),sd$(8),se$(8)*30,pl$(8,2)*35,fl2$(7),sc2$(7)*38
00120 ! ______________________________________________________________________
00130   fntop("S:\acsGL\VendorReas",cap$="Reassign Vendor Transaction Addresses")
00140   fncno(cno,cnam$) !:
        fndat(dat$)
00150 ! 
00160   for j=1 to 7: fl2$(j)=str$(j+3)&",2,C 38": next j
00170   sc2$(1)="1. Initial File Preparation"
00180   sc2$(2)="2. Add"
00190   sc2$(3)="3. Edit"
00200   sc2$(4)="4. pr Proof List"
00210   fl2$(3)="6,2,C 38,C,N"
00220   sc2$(5)="5. pr Transaction Listing"
00230   sc2$(6)="6. Reassign Transaction Addresses"
00240   sc2$(7)="7. pr 1099 Forms"
00250   cnam$=rtrm$(cnam$)
00260   sc$(1)="5,30,C 8,UT,N"
00270   sc$(2)="6,30,C 35,CUT,N"
00280   sc$(3)="7,30,C 20,UT,N"
00290   sc$(4)="8,30,C 20,UT,N"
00300   sc$(5)="9,30,C 20,UT,N"
00310   sc$(6)="10,30,N 10.2,UT,N"
00320   sc$(7)="11,30,N 2,UT,N"
00330   sc$(8)="12,30,C 11,UT,N"
00340   for j=1 to udim(se$)
00350     sd$(j)=str$(j+4)&",9,Cr 20"
00360   next j
00370   se$(1)="Vendor Number:"
00380   se$(2)="Name:"
00390   se$(3)="Address:"
00400   se$(4)="Address:"
00410   se$(5)="City,State Zip Code:"
00420   se$(6)="YTD Purchases:"
00430   se$(7)="1099 Type:"
00440   se$(8)="Federal ID or SS #:"
00450   gosub L3200
00460   open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L4750
00470   open #11: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\VNINDX2.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L4090
00480 L480: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",Shr",internal,outin,relative ioerr L500
00490   goto L530
00500 L500: close #2: ioerr L510
00510 L510: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",RecL=64,Replace",internal,outin,relative 
00520   write #2,using L850,rec=1: "",0,0,"","",1
00530 L530: pr newpage
00540   if fnprocess=1 then ti=4 else goto MENU1
00550   goto L600
00560 ! ______________________________________________________________________
00570 MENU1: ! 
00580   ti=6
00590   if ti>3 then restore #1,key>="        ",release: nokey L600 eof L600
00600 L600: goto L3330
00610   pr newpage
00620   close #102: ioerr L630
00630 L630: open #102: "SROW=4,SCOL=20,EROW=12,ECOL=59,Border=Sr,Caption=<"&cap$,display,outin 
00640   pr #102: newpage
00650   pr #102,fields "1,1,Cc 41,R,N": cnam$
00660   pr #102,fields "2,1,Cc 40,R,N": "Company Number "&str$(cno)
00670   pr #102,fields "4,1,Cc 40,R,N": "* * *   Warning   * * *"
00680   pr #102,fields "5,1,Cc 40,N": "This selection will destroy all"
00690   pr #102,fields "6,1,Cc 40,N": "existing records in the Vendor File."
00700   pr f "13,34,C 11,B,5": "Cancel (F5)"
00710   pr #102,fields "8,2,C 24,N": "Enter ERASE to continue:"
00720 L720: input #102,fields "8,27,Cu 5,UT,N": pas$
00730   if cmdkey=5 then goto L530
00740   if pas$><"ERASE" then goto L720
00750   close #1: ioerr L760
00760 L760: close #11: ioerr L770
00770 L770: open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno),internal,outin,keyed ioerr L790
00780   close #1,free: ioerr L790
00790 L790: open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",RecL=127,Replace",internal,outin,relative ioerr L820
00800   close #2: ioerr L810
00810 L810: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno),internal,outin,relative ioerr L830
00820 L820: close #2,free: ioerr L830
00830 L830: open #2: "Name="&env$('Q')&"\GLmstr\GLTR1099.H"&str$(cno)&",RecL=64,Replace",internal,outin,relative ioerr L840
00840 L840: write #2,using L850,rec=1: "",0,0,"","",1
00850 L850: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
00860   new1=1
00870   goto L2600
00880 L880: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
00890   new1=1
00900 L900: pr newpage
00910   close #101: ioerr L920
00920 L920: open #101: "SROW=3,SCOL=8,EROW=15,ECOL=66,BORDER=SR,Caption=<"&cap$,display,outin 
00930   pr f "3,8,Cc 59,R,N": "Enter Vendor Number as blank when completed"
00940   pr f mat sd$: mat se$
00950   pr f "16,25,C 09,B,1": "Next (F1)"
00960   pr f "16,35,C 09,B,5": "Stop (F5)"
00970 ! pr f "16,45,C 09,B,5": "Help (F6)" ! didn't work anyway
00980   if ti=3 or ce>0 then goto L1070
00990 L990: input fields "5,30,C 8,UT,N": vcode$ conv L990
01000   if ltrm$(rtrm$(vcode$))="0" or rtrm$(vcode$)="" then goto L530
01010   vcode$=lpad$(rtrm$(vcode$),8)
01020   read #1,using L880,key=vcode$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L1080
01030   oldti=2
01040   ti=3
01050   holdvn$=vn$
01060   holdytdp=ytdp
01070 L1070: if ti=3 or ce>0 then pr f mat sc$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$
01080 L1080: pr f "16,25,C 09,B,1": "Next (F1)"
01090   pr f "16,35,C 09,B,5": "Stop (F5)"
01100 L1100: input fields mat sc$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ conv CONVC
01110   if ce>0 then sc$(ce)(ce1:ce2)="U": ce=0
01120   if cmdkey>0 then goto L1190 else ce=curfld+1
01130   if ce>udim(sc$) then ce=1
01140 L1140: sc$(ce)=rtrm$(uprc$(sc$(ce))) : ce1=pos(sc$(ce),"U",1)
01150   ce2=ce1+1 : sc$(ce)(ce1:ce1)="UC" : goto L1100
01160 CONVC: if ce>0 then sc$(ce)(ce1:ce2)="U"
01170   ce=cnt+1
01180 ERRC: pr f "24,78,C 1": bell : goto L1140
01190 L1190: if cmdkey=5 then goto MENU1
01200   vn$=lpad$(rtrm$(vn$),8)
01210   if ti=3 then goto L1460
01220   if rtrm$(vn$)="" or ltrm$(rtrm$(vn$))="0" then goto L530
01230   read #1,using L1240,key=vn$: vn$ nokey L1270
01240 L1240: form pos 1,c 8
01250   pr f "5,35,c 30,H,N": "Duplicate Vendor Number"
01260   goto ERRC
01270 L1270: mat adr=(0)
01280   if ytdp=0 then goto L1320
01290   rec2=lrec(2)+1
01300   write #2,using L850,rec=rec2: vn$,dat,ytdp,"","Beginning Balance",0
01310   mat adr=(rec2)
01320 L1320: write #1,using L880: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
01330   goto L600
01340 L1340: pr newpage
01350   pr f "10,15,C 37,N": "Vendor Number (blank when completed):"
01360 L1360: input fields "10,60,C 8,UT,N": vcode$ conv L1360
01370   if ltrm$(rtrm$(vcode$))="0" or rtrm$(vcode$)="" then goto L530
01380   vcode$=lpad$(rtrm$(vcode$),8)
01390   read #1,using L880,key=vcode$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L1360
01400 L1400: holdvn$=vn$
01410   holdytdp=ytdp
01420 L1420: pr newpage
01430   pr f "2,13,C 45,N": "*** Review Vendor Records ***"
01440   pr f "3,10,c 60,n": "Enter Vendor Number as blank to Delete"
01450   goto L900
01460 L1460: if ltrm$(vn$)="" or rtrm$(ltrm$(vn$))="0" then goto L1470 else goto L1650
01470 L1470: pr newpage
01480   pr f "10,10,c 60,n": "Vendor Number "&holdvn$&" will be Deleted"
01490   pr f "12,10,C 31,N": "Do you wish to continue? (Y/N):"
01500 L1500: input fields "12,43,Cu 1,UT,N": yn$ conv L1500
01510   if yn$="Y" then goto L1550
01520   if yn$><"N" then goto L1500
01530   vn$=holdvn$
01540   goto L1420
01550 L1550: delete #1,key=holdvn$: 
01560   new1=1
01570   rec4=adr(1)
01580   if adr(1)=0 then goto L1340
01590 L1590: read #2,using L1610,rec=rec4: tvn$, am,nta
01600   rewrite #2,using L1610,rec=rec4: "", 0,nta
01610 L1610: form pos 1,c 8,pos 15,pd 5.2,pos 62,pd 3
01620   if nta=0 then goto L1340
01630   rec4=nta
01640   goto L1590
01650 L1650: if holdvn$=vn$ then goto L1780 else goto L1660
01660 L1660: read #1,using L1240,key=vn$: vn$ nokey L1680
01670   goto L1340
01680 L1680: delete #1,key=holdvn$: 
01690   new1=1
01700   rec5=adr(1)
01710   if adr(1)=0 then goto L1780
01720 L1720: read #2,using L1730,rec=rec5: holdvn$,nta
01730 L1730: form pos 1,c 8,pos 62,pd 3
01740   rewrite #2,using L1730,rec=rec5: vn$,nta
01750   if nta=0 then goto L1780
01760   rec5=nta
01770   goto L1720
01780 L1780: if holdytdp=ytdp then goto L1800
01790   goto L2710
01800 L1800: pr newpage
01810   if adr(1)=0 then goto L2710
01820   pr f "10,10,C 33,N": "Review Detail Transactions (Y/N):"
01830   let yn$="N"
01840 L1840: rinput fields "10,45,Cu 1,UT,N": yn$ conv L1840
01850   if yn$="Y" then goto L2710
01860   if yn$<>"N" then goto L1840
01870 L1870: if holdvn$=vn$ then goto L1890
01880   goto L1320
01890 L1890: rewrite #1,using L880,key=vn$: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
01900   if oldti=2 then ti=2
01910   oldti=0
01920   goto L600
01930 ! ______________________________________________________________________
01940   pr newpage
01950   namtab=66-int(len(rtrm$(cnam$))/2)
01960   dattab=66-int(len(rtrm$(dat$))/2)
01970   if fnprocess<>1 then gosub ASKDAT
01980   goto L2130
01990 ! _________________________
02000 ASKDAT: ! 
02010   pr newpage
02020   close #102: ioerr L2030
02030 L2030: open #102: "SRow=11,SCol=18,ERow=13,ECol=61,Border=Sr,Caption=<"&cap$,display,outin 
02040   pr #102: newpage
02050   pr #102,fields "2,2,C 20,N": "Report Heading Date:"
02060   pr f "14,34,C 11,B,5": "Cancel (F5)"
02070 L2070: rinput #102,fields "2,23,C 20,UT,N": dat$ conv L2070
02080   if cmdkey=5 then goto MENU1
02090   dattab=66-int(len(rtrm$(dat$))/2)
02100   close #102: ioerr L2120
02110   return 
02120 L2120: ! ______________________________________________________________________
02130 L2130: pr newpage
02140   close #101: ioerr L2150
02150 L2150: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,Border=SR,Caption=<"&cap$,display,outin 
02160   pr #101: newpage
02170   pr f "08,18,Cc 41,R,N": cnam$
02180   pr f "09,18,Cc 41,R,N": "Company Number "&str$(cno)
02190   pr f "11,18,Cc 41,N": "Printing..."
02200   pr f "13,34,C 11,B,5": "Cancel (F5)"
02210   on fkey 5 goto L2560
02220   fnopenprn
02230 L2230: j=0
02240   eofc=0
02250 L2250: read #1,using L880,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr eof L2530
02260   j=j+1
02270   pl$(1,j)=vn$
02280   pl$(2,j)=nam$
02290   pl$(3,j)=ad1$
02300   pl$(4,j)=ad2$
02310   pl$(5,j)=csz$
02320   pl$(6,j)=str$(ytdp)
02330   pl$(7,j)=str$(typ)
02340   pl$(8,j)=ss$
02350   if j=2 then goto L2370
02360   goto L2250
02370 L2370: if pcnt><0 then goto L2400
02380   pr #255,using L2390: date$('mm/dd/yy'),cnam$,time$,"Vendor Proof List",dat$
02390 L2390: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 57,c 20,skip 1,pos dattab,c 20,skip 2
02400 L2400: for i=1 to 8
02410     pr #255,using L2420: se$(i),pl$(i,1),pl$(i,2)
02420 L2420: form pos 1,c 30,pos 35,c 35,pos 75,c 35,skip 1
02430   next i
02440   pr #255: 
02450   mat pl$=(" ")
02460   if eofc=1 then goto L2560
02470   pcnt=pcnt+1
02480   if pcnt=6 then goto L2500
02490   goto L2230
02500 L2500: pr #255: newpage
02510   pcnt=0
02520   goto L2230
02530 L2530: if j=0 then goto L2560
02540   eofc=1
02550   goto L2370
02560 L2560: fncloseprn
02570   on fkey 5 ignore 
02580   if fnprocess=1 then goto XIT
02590   goto L530
02600 L2600: close #1: 
02610   rewrite #2,using L3510,rec=1: lrec(2)
02620   close #2: 
02630   if new1=1 then goto L2650
02640   if ti=0 and i2=0 then goto XIT
02650 L2650: close #11: ioerr L2660
02660 L2660: execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&" 1 8 Replace DupKeys -n"
02670   execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\VNINDX2.h"&str$(cno)&" 9 25 Replace DupKeys -n"
02680   if i2=1 then goto L2690 else goto XIT
02690 L2690: fnchain("S:\acsGL\GLBld109")
02700   goto L2710
02710 L2710: tt=0
02720   rec3=adr(1)
02730   if adr(1)=0 then goto L3010
02740 L2740: read #2,using L2760,rec=rec3: tvn$,dt,am,rn$,de$,nta
02750   tt=tt+am
02760 L2760: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
02770   scid$="Vendor #: "&ltrm$(vn$)&"   Balance: "&str$(ytdp)&"   Transactions: "&str$(tt)
02780   pr newpage
02790   close #101: ioerr L2800
02800 L2800: open #101: "SROW=5,SCOL=8,EROW=13,ECOL=62,Border=SR,Caption=<"&cap$,display,outin 
02810   pr f mat desc$: mat scrt$
02820   pr f "14,30,C 09,B,1": "Next (F1)"
02830   pr f "14,41,C 09,B,5": "Stop (F5)"
02840 L2840: rinput fields mat flit$: dt,am,rn$,de$ conv CONVT
02850   if ce>0 then flit$(ce)(ce1:ce2)="U": ce=0
02860   if cmdkey>0 then goto L2930 else ce=curfld+1
02870   if ce>udim(flit$) then ce=1
02880 L2880: flit$(ce)=rtrm$(uprc$(flit$(ce))) : ce1=pos(flit$(ce),"U",1)
02890   ce2=ce1+1 : flit$(ce)(ce1:ce1)="UC" : goto L2840
02900 CONVT: if ce>0 then flit$(ce)(ce1:ce2)="U"
02910   ce=cnt+1
02920 ERRT: pr f "24,78,C 1": bell : goto L2880
02930 L2930: if cmdkey=5 then goto L530
02940   if dt=0 then goto L2960
02950   if dt<10100 or dt>123199 then goto L2840
02960 L2960: ! 
02970   rewrite #2,using L2760,rec=rec3: tvn$,dt,am,rn$,de$,nta
02980   if nta=0 then goto L3010
02990   rec3=nta
03000   goto L2740
03010 L3010: if tt><ytdp then goto L3030
03020   goto L1870
03030 L3030: pr newpage
03040 ! Dim IOX$(2)
03050   pr f "8,10,Cc 60,H,N": "YTD Purchases do not agree with Total Transactions."
03060   pr f "10,21,C 78": "YTD Purchases:  "
03070   pr f "10,40,N 12.2,N": ytdp
03080   pr f "11,18,C 60": "Total Transactions:  "
03090   pr f "11,40,N 12.2,N": tt
03100   iox$(1)="13,28,C 24,N"
03110   iox$(2)="14,28,C 24,N"
03120 L3120: rinput select mat iox$,attr "H": "1. Correct YTD Purchases","2. Correct Transactions"
03130   j1=curfld
03140   on j1 goto L3150,L2710 none L3120
03150 L3150: pr newpage
03160   pr f "10,2,c 25,n": "New YTD Purchases:"
03170   let ytdp=tt
03180 L3180: rinput fields "10,35,Nz 11.2,UT,N": ytdp conv L3180
03190   goto L3010
03200 L3200: flit$(1)="6,30,Nz 6,UT,N"
03210   flit$(2)="8,30,n 10.2,UT,N"
03220   flit$(3)="10,30,C 12,UT,N"
03230   flit$(4)="12,30,c 30,UT,N"
03240   desc$(1)="6,9,Cr 20"
03250   desc$(2)="8,9,Cr 20"
03260   desc$(3)="10,9,Cr 20"
03270   desc$(4)="12,9,Cr 20"
03280   scrt$(1)="Date:"
03290   scrt$(2)="Amount:"
03300   scrt$(3)="Reference Number:"
03310   scrt$(4)="Description:"
03320   return 
03330 L3330: pr newpage
03340   pr f "10,15,Cc 60,N": "Reassigning Transaction Addresses..."
03350   restore #1,key>="        ": nokey L3360
03360 L3360: read #1,using L3370: mat adr eof L3400
03370 L3370: form pos 122,2*pd 3
03380   rewrite #1,using L3370: 0,0
03390   goto L3360
03400 L3400: lr2=lrec(2)
03410   rewrite #2,using L3510,rec=1: lr2
03420   for j=1 to lr2
03430     read #2,using L3440,rec=j: vn$,nta norec L3520
03440 L3440: form pos 1,c 8,pos 62,pd 3
03450     read #1,using L3370,key=vn$: mat adr nokey L3520
03460     if adr(1)=0 then adr(1)=j
03470     if adr(2)>0 then rewrite #2,using L3510,rec=adr(2): j
03480     adr(2)=j
03490     rewrite #1,using L3370,key=vn$: mat adr
03500     rewrite #2,using L3510,rec=j: 0
03510 L3510: form pos 62,pd 3
03520 L3520: next j
03530   goto XIT
03540   gosub ASKDAT
03550   pr newpage
03560   close #101: ioerr L3570
03570 L3570: open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=SR,CAPTION=<"&cap$,display,outin 
03580   pr #101: newpage
03590   pr #101,fields "1,1,Cc 41,R,N": cnam$
03600   pr #101,fields "2,1,Cc 41,R,N": "Company Number "&str$(cno)
03610   pr #101,fields "4,1,Cc 41,N": "Printing..."
03620   pr f "13,34,C 11,B,5": "Cancel (F5)"
03630   on fkey 5 goto L2560
03640   fnopenprn
03650   gosub L3870
03660 L3660: read #1,using L3670: vn$,nam$,ytdp,mat adr eof L2560
03670 L3670: form pos 1,c 8,c 35,pos 104,pd 5.2,pos 122,2*pd 3
03680   let fst=0
03690   ec$=""
03700   tot=0
03710   if adr(1)=0 and ytdp=0 then goto L3660
03720   nta=adr(1)
03730 L3730: read #2,using L3740,rec=nta,release: dt,am,rn$,de$,nta norec L3660
03740 L3740: form pos 9,n 6,pd 5.2,c 12,c 30,pd 3
03750   tot=tot+am
03760   if nta=0 then goto L3770 else goto L3780
03770 L3770: if ytdp=tot then goto L3780 else ec$="ERROR"
03780 L3780: if fst=0 then pr #255,using L3790: vn$,nam$,dt,rn$,de$,am,ytdp,ec$ pageoflow L3820 else pr #255,using L3800: dt,rn$,de$,am,ec$ pageoflow L3820
03790 L3790: form pos 1,c 8,pos 10,c 35,pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 115,n 10.2,pos 127,c 5,skip 1
03800 L3800: form pos 46,pic(zz/zz/zz),pos 56,c 12,pos 69,c 30,pos 100,n 10.2,pos 127,c 5,skip 1
03810   goto L3830
03820 L3820: gosub L3860
03830 L3830: let fst=1
03840   if nta=0 then pr #255: else goto L3730
03850   goto L3660
03860 L3860: pr #255: newpage
03870 L3870: p2=p2+1
03880   pr #255,using L3890: date$('mm/dd/yy'),cnam$,time$,"Vendor Transaction Listing"
03890 L3890: form skip 1,pos 1,c 8,pos 41,cc 40,skip 1,pos 1,c 8,pos 53,c 40,skip 1
03900   p1=66-int((len(rtrm$(dat$))+6)/2)
03910   pr #255,using L3920: rtrm$("As of "&dat$),"Page",p2
03920 L3920: form pos p1,c 30,pos 110,c 4,n 5,skip 2
03930   pr #255,using L3940: "Vendor #","Vendor Name","Date","Reference #","Description","Amount","YTD Purchases"
03940 L3940: form pos 1,c 8,pos 10,c 11,pos 48,c 4,pos 56,c 11,pos 70,c 11,pos 104,c 6,pos 112,c 13,skip 2
03950   return 
03960 ! _________________________________________
03970   ce=curfld
03980   ce1=pos(uprc$(sc$(ce)),"U",1)
03990   ce2=ce1+1
04000   sc$(ce)(ce1:ce2)="UC"
04010   read #10,using L4020,rec=ce: mat hlp$ norec L4080
04020 L4020: form pos 1,20*c 78
04030   pr f mat flh$: mat hlp$,se$(ce),"Enter 0 to Continue or 1 to Update Help Screen:"
04040 L4040: input fields "24,69,N 1,EUT,N": j2 conv L4040
04050   if j2<>1 then goto L4080
04060   input fields mat flh$: mat hlp$
04070   rewrite #10,using L4020,rec=ce: mat hlp$
04080 L4080: if ti=3 then goto L1420 else goto L900
04090 L4090: close #1: ioerr L4100
04100 L4100: close #11: ioerr L4110
04110 L4110: execute "Index "&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&' '&env$('Q')&"\GLmstr\VNINDX2.h"&str$(cno)&" 9 25 Replace DupKeys -n"
04120   open #1: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GL109IDX.h"&str$(cno)&",Shr",internal,outin,keyed 
04130   open #11: "Name="&env$('Q')&"\GLmstr\GL1099.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\VNINDX2.h"&str$(cno)&",Shr",internal,outin,keyed 
04140   goto L480
04150 ! SEARCH  VENDOR FILE
04160   pr newpage
04170   close #101: ioerr L4180
04180 L4180: open #101: "SROW=10,SCOL=27,EROW=13,ECOL=52,BORDER=SR,CAPTION=<"&cap$,display,outin 
04190   pr #101: newpage
04200   pr #101,fields "2,2,C,N": "Search by: "
04210   pr f "14,35,C 09,B,5": "Stop (F5)"
04220 L4220: rinput #101,select "2,13,C 13,N;3,13,C 13,N",attr "H": "Vendor Name","Vendor Number"
04230   ti2=curfld
04240   if cmdkey=5 then close #101: : goto MENU1
04250 L4250: on ti2 goto L4260,L4370 none L4220
04260 L4260: close #101: ioerr L4270
04270 L4270: open #101: "SROW=10,SCOL=10,EROW=12,ECOL=69,BORDER=SR,CAPTION=<"&cap$,display,outin 
04280   pr #101,fields "2,2,C 32,N": "Search Criteria (blank for all):"
04290   pr f "13,35,C 09,B,5": "Stop (F5)"
04300 L4300: input #101,fields "2,35,C 25,UE,N": id1$
04310   if cmdkey=5 then close #101: : goto MENU1
04320   id1$=rtrm$(id1$)
04330   l1=len(id1$)
04340   s1=11
04350   restore #s1,search>=id1$,release: nokey L4300
04360   goto L4480
04370 L4370: close #101: ioerr L4380
04380 L4380: open #101: "SROW=10,SCOL=15,EROW=12,ECOL=65,BORDER=SR,CAPTION=<"&cap$,display,outin 
04390   pr #101: newpage
04400   pr #101,fields "2,2,C 40": "Beginning Vendor Number (blank for all):"
04410   pr f "13,35,C 09,B,5": "Stop (F5)"
04420 L4420: input #101,fields "2,43,Nz 8,UT,N": vn1 conv L4420
04430   if cmdkey=5 then close #101: : goto MENU1
04440   vn$=lpad$(str$(vn1),8)
04450   restore #1,search>=vn$,release: nokey L4420
04460   s1=1
04470   l1=0
04480 L4480: close #101: ioerr L4490
04490 L4490: open #101: "SROW=2,SCOL=2,EROW=23,ECOL=79,BORDER=SR,CAPTION=<"&cap$,display,outin 
04500   ln=0
04510   pr #101: newpage
04520   pr f "2,3,C 8,R,N": "Vendor #"
04530   pr f "2,13,C 35,R,N": "Vendor Name"
04540   pr f "2,50,C 12,R,N": " Phone #"
04550   pr f "2,64,C 10,R,N": " Sales YTD"
04560 L4560: read #s1,using L880,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr eof L4650
04570   if l1=0 then goto L4590
04580   if v$(1)(1:l1)><id1$ then goto L4650
04590 L4590: ln=ln+1
04600   pr f str$(ln+2)&",3,C 8,N": vn$
04610   pr f str$(ln+2)&",13,C 35,N": nam$
04620   pr f str$(ln+2)&",50,C 12,N": ss$
04630   pr f str$(ln+2)&",64,N 10.2,N": ytdp
04640   if ln<20 then goto L4560
04650 L4650: pr f "23,13,C 33,N": "or enter Vendor Number to modify:"
04660   pr f "23,3,C 09,B,5": "Stop (F5)"
04670 L4670: input fields "23,47,Cu 8,UT,N": vcode$ conv L4670
04680   if cmdkey=5 then goto L4250
04690   if rtrm$(vcode$)="" then goto L4730
04700   vcode$=lpad$(rtrm$(vcode$),8)
04710   read #1,using L880,key=vcode$,release: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr nokey L4670
04720   ti=3: goto L1400
04730 L4730: if ln<20 then goto L4250
04740   goto L4480
04750 L4750: if err=4152 then goto L790
04760 XIT: fnxit
04770 ! ______________________________________________________________________
04780 ! <Updateable Region: ERTN>
04790 ERTN: fnerror(program$,err,line,act$,"xit")
04800   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
04810   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
04820   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
04830 ERTN_EXEC_ACT: execute act$ : goto ERTN
04840 ! /region
04850 ! ______________________________________________________________________
