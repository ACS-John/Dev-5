00010 ! Replace R:\acsGL\AcGlInc5
00020 ! -- PRINT INCOME STATEMENT
00030 ! -- INCOME STATEMENT WITH BUDGET
00040   library 'R:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnerror,fncno,fnchain
00050   let fntop(program$,cap$="Income Statement")
00060 ! ______________________________________________________________________
00070   on error goto ERTN
00080   on fkey 5 goto L1930
00090 ! ______________________________________________________________________
00100   dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
00110   dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
00120   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
00130   dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13),cap$*128
00140 ! ______________________________________________________________________
00150   let fncno(cno,cnam$)
00160   open #20: "Name=CNO.H"&wsid$,internal,input,relative 
00170   read #20,using "Form POS 141,6*N 1,3*N 2,C 6,3*C 12,2*C 20",rec=1: process,ps,filno,priorcd,mat d,fscode,lmu,actpd,actpd$,mat cogl$,pedat$,cch$
00180   close #20: 
00190 ! ______________________________________________________________________
00200   let fnopenprn(cp,58,220,process)
00210   let mp1=69
00220   if ps=2 then let mp1=mp1+3
00230   let fl1$="Name=Q:\GLmstr\ACGLFNSI.h"&str$(cno)&",KFName=Q:\GLmstr\FNSIINDX.h"&str$(cno)&",Shr"
00240   if ps=2 then let fl1$="Name=Q:\GLmstr\ACGLFNSJ.h"&str$(cno)&",KFName=Q:\GLmstr\FNSJINDX.h"&str$(cno)&",Shr"
00250   print newpage
00260   print fields "10,20,C 30,h,n": "INCOME STATEMENT IN PROCESS"
00270   form c 7,skip 0
00280   let nametab=int(44-len(rtrm$(cnam$))/2)
00290   open #1: fl1$,internal,input,keyed 
00300   open #3: "Name=Q:\GLmstr\GLmstr.h"&str$(cno)&",Shr",internal,input,relative 
00310   open #2: "Name=Q:\GLmstr\GLAddr."&session$&",NoShr",internal,input 
00320   if process=1 or d(1)=0 then goto L410
00330   print newpage
00340   close #101: ioerr L350
00350 L350: open #101: "SROW=9,SCOL=4,EROW=12,ECOL=75,BORDER=DR,CAPTION=PRINT INCOME STATEMENT",display,outin 
00360   print fields "13,29,C 9,B,1": "Next (F1)"
00370   print fields "13,40,C 11,B,5": "Cancel (F5)"
00380   print fields "10,5,c 70,n": "ENTER THE COST CENTER OR DEPT # IF YOU WISH TO ONLY PRINT A STATEMENT"
00390   print fields "11,5,c 65,n": "ON ONE DEPARTMENT; ELSE ENTER 0 TO PRINT ALL DEPARTMENTS"
00400 L400: input fields "11,70,N 3,eu,N": costcntr conv L400
00410 L410: print newpage
00420   print fields "10,20,C 32,h,n": " INCOME STATEMENT IN PROCESS"
00430   print fields "23,2,c 18,R,n": " Press F5 to stop"
00440   let report$="STATEMENT OF INCOME AND EXPENSES"
00450 L450: read #1,using L500: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1930
00460   if ltrm$(r$)="" or ltrm$(r$)="0" then goto L450
00470   if costcntr=0 then goto L500
00480   if fc=0 and te$="F" then goto L510 ! 5/08/1989
00490   if costcntr><fc then goto L450
00500 L500: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00510 L510: if te$="S" or te$="F" then goto L530
00520   if heading=0 and te$><"R" then gosub L1760
00530 L530: on pos ("RFHDTS",te$,1) goto L1310,L1350,L540,L590,L1180,L1310 none L450
00540 L540: print #255,using L550: d$(1:40)
00550 L550: form pos sp,c 40,skip 1
00560   gosub L1500
00570   gosub L1420
00580   goto L450
00590 L590: if notrans=1 then goto L900
00600   if ir>=val(r$) and val(r$)><0 then goto L720
00610 L610: read #2,using L620: record eof L890
00620 L620: form pd 3
00630   read #3,using L710,rec=record: ir,bb,cb,mat by,mat bp,mat bm
00640   if fscode=0 then goto L710
00650   if fscode<1 or fscode>13 then let fscode=1
00660   if priorcd=1 then let cb=by(fscode) else let cb=bp(fscode)
00670   if priorcd=2 then goto L700
00680   if fscode>1 then let bb=by(fscode-1) else let bb=0
00690   goto L710
00700 L700: if fscode>1 then let bb=bp(fscode-1) else let bb=0
00710 L710: form pos mp1,pd 3,pos 81,41*pd 6.2
00720 L720: if ir=val(r$) then let total=total+(cb-bb) else goto L870
00730   let total2=total2+cb
00740   for z=1 to 13
00750     let annualb=annualb+bm(z)
00760   next z
00770   if fscode=0 then let monthb=monthb+bm(actpd) else let monthb=monthb+bm(fscode) ! 11/24/86
00780   if fscode=0 then goto L790 else goto L830 ! 11/24/86
00790 L790: for j=1 to actpd
00800     let ytdb=ytdb+bm(j)
00810   next j
00820   goto L610
00830 L830: for j=1 to fscode ! 11/24/86
00840     let ytdb=ytdb+bm(j) ! 11/24/86
00850   next j ! 11/24/86
00860   goto L610 ! 11/24/86
00870 L870: if ir<val(r$) then goto L610
00880   if ir>val(r$) then goto L900
00890 L890: let notrans=1
00900 L900: let unexpend=annualb-total2
00910   for j=1 to 9
00920     if ac(j)=9 then goto L970 ! 10/14/87
00930     let accum(j,1)=accum(j,1)+annualb
00940     let accum(j,2)=accum(j,2)+total
00950     let accum(j,3)=accum(j,3)+total2
00960     let accum(j,4)=accum(j,4)+unexpend
00970 L970: next j
00980   if rs=1 then let total=-total else goto L1020
00990   let total2=-total2
01000   let annualb=-annualb
01010   let unexpend=unexpend
01020 L1020: if ds=1 then let dollar$="$" else let dollar$=" "
01030   if annualb><0 or total2><0 then goto L1060
01040   if total<>0 then goto L1060
01050   if ls+ds+ul+ic>0 then goto L1060 else goto L450
01060 L1060: let sp2=26-sp-1
01070   if annualb<>0 then let abp=round((total2/annualb*100),2) else let abp=0
01080   print #255,using L1090: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend,abp,"%" pageoflow L1630
01090 L1090: form pos sp,c sp2,pos 26,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,pic(--,---,---.##),c 1,skip 0
01100   let total=0
01110   let total2=0
01120   let annualb=0
01130   let unexpend=0
01140   gosub L1420
01150   gosub L1640
01160   gosub L1500
01170   goto L450
01180 L1180: if ap=0 then let ap=1
01190   if rs=1 then let accum1=-accum(ap,1) else let accum1=accum(ap,1)
01200   if rs=1 then let accum2=-accum(ap,2) else let accum2=accum(ap,2)
01210   if rs=1 then let accum3=-accum(ap,3) else let accum3=accum(ap,3)
01220   if rs=1 then let accum4=accum(ap,4) else let accum4=accum(ap,4)
01230   if ds=1 then let dollar$="$" else let dollar$=" "
01240   let sp2=26-sp-1
01250   if annualb<>0 then let abp=round((total2/annualb*100),2) else let abp=0
01260   print #255,using L1090: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,abp,"%" pageoflow L1630
01270   gosub L1420
01280   gosub L1640
01290   gosub L1500
01300   goto L450
01310 L1310: if te$="R" then let report$=d$
01320   if te$="S" then let secondr$=d$
01330   gosub L1500
01340   goto L450
01350 L1350: if foot1=1 then goto L1400
01360   let tabnote=sp
01370   let foot1=1
01380   let foot$=d$
01390   goto L450
01400 L1400: let foot$=rtrm$(foot$)&d$
01410   goto L450
01420 L1420: for j=1 to 9
01430     if ac(j)=0 or ac(j)=9 then goto L1480 ! 10/14/87
01440     let accum(j,1)=0
01450     let accum(j,2)=0
01460     let accum(j,3)=0
01470     let accum(j,4)=0
01480 L1480: next j
01490   return 
01500 L1500: if ls=0 then goto L1620
01510   if ls=99 then goto L1550
01520   print #255,using L1530: " "
01530 L1530: form pos 1,c 1,skip ls
01540   goto L1620
01550 L1550: let sk=58-krec(255): let fl=len(rtrm$(foot$))
01560   if pglen=42 then let sk=sk+1
01570   print #255,using L1580: rtrm$(foot$)
01580 L1580: form skip sk,pos tabnote,c fl,skip 1
01590   if eofcode=1 then goto L1620
01600   print #255: newpage
01610   gosub L1760
01620 L1620: return 
01630 L1630: gosub L1550: continue 
01640 L1640: if ul=0 then goto L1730
01650   if ul=1 then goto L1700
01660   let underlin$="=============="
01670   print #255: 
01680   goto L1710
01690   goto L1730
01700 L1700: let underlin$="______________"
01710 L1710: print #255,using L1720: underlin$,underlin$,underlin$,underlin$,underlin$
01720 L1720: form skip 0,pos 26,5*c 15,skip 0
01730 L1730: print #255,using L1740: " "
01740 L1740: form skip 1,c 1,skip 0
01750   return 
01760 L1760: let heading=1
01770   print #255,using L1780: cnam$
01780 L1780: form skip 2,pos nametab,c 40,skip 1
01790   let p1=44-len(rtrm$(report$))/2
01800   print #255,using L1810: rtrm$(report$)
01810 L1810: form pos p1,c 70
01820   if rtrm$(secondr$)="" then goto L1850
01830   let p1=44-len(rtrm$(secondr$))/2
01840   print #255,using L1810: rtrm$(secondr$)
01850 L1850: let p1=30-len(rtrm$(actpd$))/2-len(rtrm$(pedat$))/2
01860   print #255,using L1810: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(pedat$)
01870   print #255: 
01880   print #255: 
01890   print #255: tab(35);cch$;tab(56);"YEAR TO DATE";tab(71);"UNEXPENDED";tab(86);"ACTUAL TO BUDGET"
01900   print #255: tab(26);"ANNUAL BUDGET";tab(41);"MONTH";tab(56);"BALANCE";tab(71);"BALANCE ";tab(86);"PERCENTAGE"
01910   print #255: 
01920   return 
01930 L1930: let eofcode=1
01940   gosub L1550
01950   let fncloseprn(nw)
01960   if process=1 then goto L1980
01970 XIT: let fnxit
01980 L1980: let fnchain("R:\acsGL\acglAuto")
01990 ! ______________________________________________________________________
02000 ! <updateable region: ertn>
02010 ERTN: let fnerror(cap$,err,line,act$,"xit")
02020   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02030   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02040   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
02050 ERTN_EXEC_ACT: execute act$ : goto ERTN
02060 ! /region
