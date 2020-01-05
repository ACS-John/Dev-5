00010 ! Replace S:\acsGL\acglCas5
00020 ! CASH FLOW STATEMENT  WITH BUDGET
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fntop, fnopenprn,fncloseprn,fnerror,fnprocess,fncno,fnpedat$,fnps,fnpriorcd,fnfscode,fnactpd$,fncch$,fnGlAskFormatPriorCdPeriod,fnactpd
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20 ,in3$(4),cap$*128,udf$*256
00080   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00090   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00100   dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13)
00110 ! ______________________________________________________________________
00120   fntop(program$,cap$="Cash Flow with Budget")
00130   fncno(cno,cnam$)
00140   udf$=env$('temp')&'\'
00150   if fnGlAskFormatPriorCdPeriod=5 then goto XIT !:
          ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
00160   actpd$=fnactpd$
00170   pedat$=fnpedat$
00180   actpd$=fnactpd$
00190   actpd=fnactpd
00200   fscode=fnfscode
00210   priorcd=fnpriorcd
00220   open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative  !:
        read #20,using 'Form Pos 384,n 2',rec=1: nap : close #20: 
00230   if nap<12 or nap> 13 then nap=12
00240   in3$(1)="8,5,N 12.2,UT,N" : in3$(2)="8,25,N 12.2,UT,N" !:
        in3$(3)="8,45,N 12.2,UT,N" : in3$(4)="8,65,N 12.2,UT,N"
00250   mp1=75
00260   if ps=2 then mp1=mp1+3
00270   fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\FNSFIndx.h[cno],Shr"
00280   if ps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\FNSGIndx.h[cno],Shr"
00290   pr newpage
00300   pr f "10,20,C 30,h,n": "CASH FLOW STATEMENT IN PROCESS"
00310   on fkey 5 goto L2130
00320   fnopenprn(cp,58,220,process)
00330   open #1: fl1$,internal,input,keyed 
00340   if process=1 or d(1)=0 then goto L390
00350   pr newpage
00360   pr f "10,5,C 75,": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY pr A STATEMENT"
00370 L370: pr f "11,5,C 65": "ON ONE DEPARTMENT; ELSE ENTER 0 TO pr ALL DEPARTMENTS"
00380   input fields "11,70,N 3,ue,N": costcntr conv L370
00390 L390: pr newpage
00400   pr f "10,1,Cc 80,N": "Printing: please wait..."
00410   pr f "12,2,c 30,B,5": "Press F5 to stop"
00420   if fnps=2 then goto L450 ! secondary
00430   execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 75 3 Replace DupKeys -N"
00440   goto L460
00450 L450: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.H[cno] 78 3 Replace DupKeys -N"
00460 L460: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed 
00470 L470: read #1,using L510: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2130
00480   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L470
00490   if costcntr=0 then goto L510
00500   if costcntr><fc then goto L470
00510 L510: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00520   if te$="S" or te$="F" then goto L540
00530   if heading=0 and te$><"R" then gosub L1980
00540 L540: on pos ("RFHDTSBC",te$,1) goto L1520,L1570,L550,L610,L1350,L1520,L610,L2180 none L470
00550 L550: pr #255,using L560: d$(1:40)
00560 L560: form pos sp,c 40
00570   gosub L1720
00580   gosub L1660
00590   goto L470
00600 ! ______________________________________________________________________
00610 L610: if te$="B" and ap>0 then goto L1350 ! ENDING BANK BALANCE
00620   if notrans=1 then goto L960
00630   if ir>=val(r$) and val(r$)><0 then goto L750
00640 L640: ! read amounts from gl master file
00650 L650: read #3,using L740: ir,bb,cb,mat by,mat bp,mat bm eof L950
00660   if ir=0 then goto L650
00670   if fscode=0 then goto L740
00680   if fscode<1 or fscode>13 then fscode=1
00690   if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00700   if priorcd=2 then goto L730
00710   if fscode>1 then bb=by(fscode-1) else bb=0
00720   goto L740
00730 L730: if fscode>1 then bb=bp(fscode-1) else bb=0
00740 L740: form pos mp1,pd 3,pos 81,41*pd 6.2
00750 L750: if ir=val(r$) then total=total+(cb-bb) else goto L930
00760   if te$="B" then total=total-(cb-bb): total=total - bb: total2=total2-bp(nap) : goto L780
00770   total2=total2+cb
00780 L780: for z=1 to 13
00790     annualb=annualb+bm(z)
00800   next z
00810   if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode)
00820   if fscode=0 then goto L830 else goto L880
00830 L830: for j=1 to actpd
00840     ytdb=ytdb+bm(j)
00850   next j
00860   goto L640
00870 ! ______________________________________________________________________
00880 L880: for j=1 to fscode
00890     ytdb=ytdb+bm(j)
00900   next j
00910   goto L640
00920 ! ______________________________________________________________________
00930 L930: if ir<val(r$) then goto L640
00940   if ir>val(r$) then goto L960
00950 L950: notrans=1
00960 L960: overundr=ytdb-total2
00970   unexpend=annualb-total2
00980   for j=1 to 9
00990     if ac(j)=9 then goto L1070
01000     accum(j,1)=accum(j,1)+total
01010     accum(j,2)=accum(j,2)+total2
01020     accum(j,3)=accum(j,3)+annualb
01030     accum(j,4)=accum(j,4)+monthb
01040     accum(j,5)=accum(j,5)+ytdb
01050     accum(j,6)=accum(j,6)+overundr
01060     accum(j,7)=accum(j,7)+unexpend
01070 L1070: next j
01080   if rs=1 then total=-total else goto L1150
01090   total2=-total2
01100   annualb=-annualb
01110   monthb=-monthb
01120   ytdb=-ytdb
01130   overundr=overundr
01140   unexpend=unexpend
01150 L1150: if ds=1 then dollar$="$" else dollar$=" "
01160   if annualb><0 or total2><0 then goto L1190
01170   if total<>0 then goto L1190
01180   if ls+ds+ul+ic>0 then goto L1190 else goto L470
01190 L1190: sp2=24-sp-1
01200   if te$="B" then total=-total: total2=-total2 ! REVERSE SIGN ON BEGINNING BANK BALANCE
01210   pr #255,using L1220: d$(1:sp2),dollar$,monthb,dollar$,total,dollar$,total2,dollar$,ytdb,dollar$,annualb
01220 L1220: form pos sp,c sp2,pos 24,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip 0
01230   total=0
01240   total2=0
01250   annualb=0
01260   monthb=0
01270   ytdb=0
01280   overundr=0
01290   unexpend=0
01300   gosub L1660
01310   gosub L1850
01320   gosub L1720
01330   goto L470
01340 ! ______________________________________________________________________
01350 L1350: if ap=0 then ap=1
01360   if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
01370   if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
01380   if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
01390   if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
01400   if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
01410   if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
01420   if rs=1 then accum7=accum(ap,7) else accum7=accum(ap,7)
01430   if ds=1 then dollar$="$" else dollar$=" "
01440   sp2=24-sp-1
01450   if te$="B" then accum3=accum4=0
01460   pr #255,using L1220: d$(1:sp2),dollar$,accum4,dollar$,accum1,dollar$,accum2,dollar$,accum5,dollar$,accum3
01470   gosub L1660
01480   gosub L1850
01490   gosub L1720
01500   goto L470
01510 ! ______________________________________________________________________
01520 L1520: if te$="R" then report$=d$
01530   if te$="S" then secondr$=d$
01540   gosub L1720
01550   goto L470
01560 ! ______________________________________________________________________
01570 L1570: if foot1=1 then goto L1630
01580   tabnote=sp
01590   foot1=1
01600   foot$=d$
01610   goto L470
01620 ! ______________________________________________________________________
01630 L1630: foot$=rtrm$(foot$)&d$
01640   goto L470
01650 ! ______________________________________________________________________
01660 L1660: for j=1 to 9
01670     if ac(j)=0 or ac(j)=9 then goto L1690
01680     for j2=1 to 7 : accum(j,j2)=0 : next j2
01690 L1690: next j
01700   return 
01710 ! ______________________________________________________________________
01720 L1720: if ls=0 then goto L1830
01730   if ls=99 then goto L1770
01740   pr #255,using L1750: " "
01750 L1750: form pos 1,c 1,skip ls
01760   goto L1830
01770 L1770: sk=62-krec(255) !:
        fl=len(rtrm$(foot$))
01780   pr #255,using L1790: rtrm$(foot$)
01790 L1790: form skip sk,pos tabnote,c fl
01800   if eofcode=1 then goto L1830
01810   pr #255: newpage
01820   gosub L1980
01830 L1830: return 
01840 ! ______________________________________________________________________
01850 L1850: if ul=0 then goto L1940
01860   if ul=1 then goto L1910
01870   underlin$="=============="
01880   pr #255: 
01890   goto L1920
01900   goto L1940
01910 L1910: underlin$="______________"
01920 L1920: pr #255,using L1930: underlin$,underlin$,underlin$,underlin$,underlin$
01930 L1930: form skip 0,pos 24,5*c 15,skip 0
01940 L1940: pr #255,using L1950: " "
01950 L1950: form skip 1,c 1,skip 0
01960   return 
01970 ! ______________________________________________________________________
01980 L1980: heading=1
01990   pr #255: ""
02000   pr #255,using L2020: cnam$
02010   pr #255,using L2020: rtrm$(report$)
02020 L2020: form pos 1,cc 80
02030   if rtrm$(secondr$)="" then goto L2050
02040   pr #255,using L2020: rtrm$(secondr$)
02050 L2050: pr #255,using L2020: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(pedat$)
02060   pr #255: ""
02070   pr #255: ""
02080   pr #255: tab(31);"Monthly";tab(38);cch$;tab(61);"Year To";tab(77);"Budget";tab(92);"Annual"
02090   pr #255: tab(32);"Budget";tab(44);"       ";tab(62);"Date";tab(77);"To Date";tab(92);"Budget"
02100   pr #255: ""
02110   return 
02120 ! ______________________________________________________________________
02130 L2130: eofcode=1
02140   gosub L1770
02150   fncloseprn
02160   goto XIT
02170 ! ______________________________________________________________________
02180 L2180: pr newpage
02190   pr f "2,5,C 75,N": "Enter the Following Information for "& rtrm$(d$)
02200   pr f "6,5,C 70,N": "Monthly             Current             Year To             Annual"
02210   pr f "7,5,C 70,N": "Budget               Month                Date              Budget"
02220 L2220: input fields mat in3$: monthb,total,total2,annualb conv L2220
02230   pr newpage
02240   goto L960
02250 ! ______________________________________________________________________
02260 XIT: fnxit
02270 ! ______________________________________________________________________
02280 ! <Updateable Region: ERTN>
02290 ERTN: fnerror(program$,err,line,act$,"xit")
02300   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02330 ERTN_EXEC_ACT: execute act$ : goto ERTN
02340 ! /region
