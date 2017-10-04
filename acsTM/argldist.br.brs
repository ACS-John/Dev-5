00020   on fkey 5 goto L690 ! 9/5/86
00030   on error goto L730
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fndat
00050   fntop("S:\acsTM\arpostgl",cap$="Post General Ledger")
00060   fncno(cno,cnam$)
00070   fndat(dat$)
00080   dim p$*5,iv$*12,gl(3),gh(3),td$*30,tr$*12,a$*40,cap$*128,dat$*20
00090   let td$="AR SUMMARY"
00100   let tr6=5
00110   open #8: "Name="&env$('Q')&"\TMmstr\pedate.h"&str$(cno)&",RecL=20,use,Shr",internal,outin,relative 
00120   if lrec(8)=0 then write #8,using "form pos 1,n 6": dat else read #8,using "form pos 1,n 6",rec=1,release: dat
00130   pr newpage
00140   pr f "10,15,c 60": "POSITION PAPER FOR G/L DISTRIBUTION REPORT"
00150   pr f "12,12,c 60": "ENTER THE AS OF DATE: FORMAT = MMDDYY"
00160   pr f "12,53,n 6,N": dat
00170 L170: input fields "12,53,n 6,ue": tr4 conv L170
00180   if tr4<10100 or tr4>123199 then goto L170
00190   rewrite #8,using "form pos 1,n 6",rec=1: d1
00200   let pa=43-int(len(rtrm$(a$))/2)
00210   open #1: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr L730
00220   open #2: "Name="&env$('temp')&"\Work."&session$,internal,input,relative ioerr L730
00230   pr newpage
00240   pr f "10,15,c 50,h,n": "PRINT A/R GENERAL LEDGER DISTRIBUTION IN PROCESS"
00250   pr f "23,2,C 30,N": "Press F5 to stop"
00260   gosub L590
00270 L270: read #1,using L280: r1 eof L670 ioerr L730
00280 L280: form pos 1,pd 3
00290   read #2,using L300,rec=r1: p$,iv$,tr1,tr3,mat gl ioerr L730
00300 L300: form pos 1,c 5,c 12,n 6,pd 5.2,n 3,n 6,n 3
00310   gosub L330
00320   goto L430
00330 L330: if gh(2)=gl(2) or gh(2)=0 then goto L420
00340 L340: pr #255,using L350: mat gh,"TOTAL ",tdb,tcr
00350 L350: form skip 1,pic(zzzz),pic(zzzzzzz),pic(zzzz),x 5,c 27,2*n 15.2,skip 2
00360   let tt=tdb-tcr
00370   if tt=0 then goto L400
00380   let gtdb=gtdb+tdb
00390   let gtcr=gtcr+tcr
00400 L400: let tdb=0
00410   let tcr=0
00420 L420: return 
00430 L430: adb=0
00440   acr=0
00450   if tr3>0 then adb=tr3 else acr=-tr3
00460   if adb=0 then goto L500
00470   pr #255,using L480: mat gl,p$,iv$,tr1,adb pageoflow L560
00480 L480: form pos 1,pic(zzzz),n 7,pic(zzzz),x 2,c 8,c 14,pic(zz/zz/zz),n 15.2
00490   goto L520
00500 L500: pr #255,using L510: mat gl,p$,iv$,tr1,acr pageoflow L560
00510 L510: form pos 1,pic(zzzz),n 7,pic(zzzz),x 2,c 8,c 14,pic(zz/zz/zz),x 15,n 15.2
00520 L520: let tdb=tdb+adb
00530   let tcr=tcr+acr
00540   mat gh=gl
00550   goto L270
00560 L560: pr #255: newpage
00570   gosub L590
00580   continue 
00590 L590: pr #255,using L600: date$,a$
00600 L600: form skip 2,pos 1,c 8,pos pa,c 40,skip 1
00610   pr #255,using L620: time$,"G/L DISTRIBUTION FOR ACCOUNTS RECEIVABLE"
00620 L620: form pos 1,c 8,pos 20,c 45,skip 1
00630   pr #255,using L640: "AS OF ",tr4
00640 L640: form pos 33,c 6,pic(zz/zz/zz),skip 2
00650   pr #255: "  G/L ACCOUNT # CLIENT #    INVOICE #    DATE           DEBITS        CREDITS"
00660   return 
00670 L670: gosub L340
00680   pr #255,using L350: 0,0,0,"FINAL TOTAL ",gtdb,gtcr
00690 L690: close #1: ioerr L700
00700 L700: close #2: ioerr L710
00710 L710: if nw=1 then close #255: else pr #255: newpage
00720 XIT: let fnxit
00730 L730: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L750
00740   goto L790
00750 L750: pr newpage
00760   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L780
00770   goto L790
00780 L780: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00790 L790: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00800   input fields "24,60,C 1,N": quitcode$
00810   if rtrm$(uprc$(quitcode$))="Q" then goto L850
00820   pr f "23,3,C 78,N": ""
00830   pr f "24,3,C 78,N": ""
00840   retry 
00850 L850: goto XIT
