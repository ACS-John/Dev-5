00010 ! Replace S:\acsGL\acglSchP
00020 ! pr Schedules  (once used to pr schedules; now called glschprt)
00030 ! ________________Needs fncmo and others to run _____________________
00040   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fnopenprn,fncloseprn
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$*256,dollar$*1,k$*2,by(13),bp(13)
00080   dim gl2$*12,d2$*50,by2(13),bp2(13)
00090   dim sn$*78,ft$*78,gl$(80)*12,prtsch(99),d$*50
00100   dim cnam$*40,d(2),actpd$*6,pedat$*20,cch$*20,d(2),cap$*128
00110 ! ______________________________________________________________________
00120   let fntop(program$,cap$="Print Schedules")
00130   let fncno(cno,cnam$)
00140   open #20: "Name=CNO.H"&wsid$,internal,input,relative  !:
        read #20,using 'Form POS 141,6*N 1,3*N 2,C 6,POS 195,2*C 20',rec=1: process,ps,filno,priorcd,mat d,fscode,lmu,actpd,actpd$,pedat$,cch$ !:
        close #20: 
00150   if process=1 then let prtall=1 : goto L320
00160 L160: pr newpage
00170   pr fields "5,25,c 50,h,n": "SELECTION OF SCHEDULES TO PRINT"
00180   pr fields "10,5,C 60,N": "ENTER 1 TO pr ALL SCHEDULES  "
00190   pr fields "11,5,c 60,n": "ENTER 0 TO SELECT SPECIFIC SCHEDULES TO PRINT"
00200   let fa$="11,65,N 1,uE,N"
00210 L210: input fields fa$: prtall conv L210
00220   if prtall=1 then goto L320
00230   if prtall><0 then goto L160
00240   let fb$="10,65,N 2,uE,N"
00250   for j=1 to 99
00260     pr newpage
00270     if j=1 then pr fields "10,5,C 60,N": "ENTER SCHEDULE NUMBER TO PRINT" else pr fields "10,5,c 75,n": "ENTER NEXT SCHEDULE TO PRINT, ELSE ENTER 0 WHEN COMPLETE"
00280 L280: input fields fb$: prtsch(j) conv L280
00290     if prtsch(j)=0 then goto L310
00300   next j
00310 L310: let j=0
00320 L320: open #1: "Name="&env$('Q')&"\GLmstr\ACGLSCHS.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\schindex.h"&str$(cno)&",Shr",internal,input,keyed ioerr DONE
00330   open #3: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00340   pr newpage
00350   pr fields "10,20,C 60,N": "PRINT SCHEDULES IN PROCESS"
00360   pr fields "12,2,C 30,B,5": "Press F5 to stop"
00370   let fnopenprn(cp,58,220,process)
00380 L380: if prtall=1 then goto L440
00390 L390: let g=g+1
00400   if prtsch(g)=0 then goto DONE
00410   let k$=lpad$(str$(prtsch(g)),2)
00420   read #1,using L450,key=k$: sn,sn$,ft$,dp,rs,cm,mat gl$ nokey L390
00430   goto L460
00440 L440: read #1,using L450: sn,sn$,ft$,dp,rs,cm,mat gl$ eof DONE
00450 L450: form pos 1,n 2,2*c 78,3*n 1,80*c 12
00460 L460: if dp=1 then let dollar$="$" else let dollar$=" "
00470   gosub L1130
00480   for j=1 to 80
00490     if gl$(j)="  0     0  0" then goto L830
00500     if j1><51 then goto L530
00510     gosub L1060
00520     gosub L1130
00530 L530: read #3,using L600,key=gl$(j): d$,bb,cb,mat by,mat bp nokey L830
00540     if cno<>99 then goto L600
00550 L550: read #3,using L560: gl2$,d2$,bb2,cb2,mat by2,mat bp2 eof L600
00560 L560: form pos 1,c 12,pos 13,c 50,pos 81,41*pd 6.2
00570     if gl2$=gl$(j) then goto L580 else goto L600
00580 L580: bb=bb+bb2: cb=cb+cb2: mat by=by+by2: mat bp=bp+bp2
00590     goto L550
00600 L600: form pos 13,c 50,pos 81,41*pd 6.2
00610     if fscode=0 then goto L690 ! CURRENT OR PRIOR
00620     if fscode<0 or fscode>12 then let fscode=1
00630     if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
00640     if priorcd=2 then goto L680
00650     if fscode>1 then bb=by(fscode-1) else bb=0
00660     goto L690
00670 ! ______________________________________________________________________
00680 L680: if fscode>1 then bb=bp(fscode-1) else bb=0
00690 L690: curmo=cb-bb
00700     if rs=1 then cb=-cb
00710     if rs=1 then curmo=-curmo
00720     if cm=1 then goto L780
00730     pr #255,using L750: d$,dollar$,cb
00740     let dollar$=" "
00750 L750: form pos 1,c 50,pos 67,c 1,pic(--,---,---.##),skip 1
00760     goto L820
00770 ! ______________________________________________________________________
00780 L780: pr #255,using L810: d$,dollar$,curmo,dollar$,cb
00790     let j1=j1+1
00800     let dollar$=" "
00810 L810: form pos 1,c 50,pos 51,c 1,pic(--,---,---.##),pos 67,c 1,pic(--,---,---.##),skip 1
00820 L820: gosub L1330
00830 L830: next j
00840   let j1=0
00850   gosub L890
00860   gosub L1060
00870   goto L380
00880 ! ______________________________________________________________________
00890 L890: ! TOTAL PRINT
00900   if dp=1 then let dollar$="$" else let dollar$=" "
00910   if cm=1 then goto L980
00920   pr #255,using L930: "______________"
00930 L930: form pos 67,c 14,skip 1
00940   pr #255,using L750: "    TOTAL",dollar$,ytdtot
00950   pr #255,using L930: "=============="
00960   goto L1020
00970 ! ______________________________________________________________________
00980 L980: pr #255,using L990: "______________","______________"
00990 L990: form pos 51,c 14,pos 67,c 14,skip 1
01000   pr #255,using L810: "    TOTAL",dollar$,cmtot,dollar$,ytdtot
01010   pr #255,using L990: "==============","=============="
01020 L1020: cmtot=0
01030   let ytdtot=0
01040   return 
01050 ! ______________________________________________________________________
01060 L1060: let fttab=int(43-len(rtrm$(ft$))/2)
01070   let sk=58-krec(255): let fl=len(rtrm$(ft$))
01080   pr #255,using L1090: rtrm$(ft$)
01090 L1090: form skip sk,pos fttab,c fl,skip 1
01100   pr #255: newpage
01110   return 
01120 ! ______________________________________________________________________
01130 L1130: ! PAGE HEADING
01140   pr #255,using L1150: cnam$,"SCHEDULE ",sn
01150 L1150: form pos 11,cc 58,c 9,pic(zz)
01160   let sntab=int(43-len(rtrm$(sn$))/2)
01170   pr #255,using L1180: sn$
01180 L1180: form pos sntab,c 80,skip 1
01190   let dattab=int(43-len(rtrm$(pedat$))/2)
01200   pr #255,using L1210: pedat$
01210 L1210: form pos dattab,c 80,skip 2
01220   if cm><1 then goto L1250
01230   pr #255,using L1240: cch$,"YEAR TO DATE"
01240 L1240: form pos 48,c 20,pos 69,c 12,skip 2
01250 L1250: return 
01260 ! ______________________________________________________________________
01270 DONE: ! 
01280   let fncloseprn
01290   goto XIT
01300 ! ______________________________________________________________________
01310 XIT: let fnxit
01320 ! ______________________________________________________________________
01330 L1330: let ytdtot=ytdtot+cb
01340   if cm><1 then goto L1360
01350   cmtot=cmtot+curmo
01360 L1360: return 
01370 ! ______________________________________________________________________
01380 ! <Updateable Region: ERTN>
01390 ERTN: let fnerror(program$,err,line,act$,"xit")
01400   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01410   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01420   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01430 ERTN_EXEC_ACT: execute act$ : goto ERTN
01440 ! /region
01450 ! ______________________________________________________________________
