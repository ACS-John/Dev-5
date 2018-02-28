00020 ! (C) COPYRIGHT - 1986 - ADVANCED COMPUTER SERVICES, INC.
00030   on error goto L2010
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$,fnconsole
00050   library 'S:\Core\Library': fnopenprn,fncloseprn
00060   fntop(program$,cap$="Service Code")
00070   fncno(cno,cnam$)
00080   dim sc$*4,ds$*30,holdsc$*4,scode$*4
00090   dim cnam$*40,dat$*20
00100   dim sb$(4),sd$(4),se$(4)*25,pl$(4,3)*30
00110   sb$(1)="5,30,c 4,u  ,n"
00120   sb$(2)="6,30,c 30,CU  ,n"
00130   sb$(3)="7,30,N 7.2,u  ,n"
00140   sb$(4)="8,30,n 9.2,u  ,n"
00150   sd$(1)="5,2,c 25,n"
00160   sd$(2)="6,2,c 25,n"
00170   sd$(3)="7,2,c 25,n"
00180   sd$(4)="8,2,c 25,n"
00190   se$(1)="SERVICE CODE"
00200   se$(2)="DESCRIPTION"
00210   se$(3)="TOTAL HOURS - YTD"
00220   se$(4)="STANDARD FEES - YTD"
00230   gosub L1780
00240   open #1: "Name=[Q]\TMmstr\SCMSTR.H[cno],KFName=[Q]\TMmstr\SCIndex.H[cno],Shr",internal,outIn,keyed ioerr L2000
00250   goto L260
00260 L260: pr newpage
00270   pr f "3,9,c 55,N": "SERVICE CODE MASTER FILE"
00280   pr f "4,6,C 72,N": "COMPANY NUMBER [cno]  "&ltrm$(cnam$)
00290   pr f "6,9,c 55,n": "1 = INITIAL FILE PREPARATION"
00300   pr f "7,9,c 55,n": "2 = ADD NEW RECORDS"
00310   pr f "8,9,c 55,n": "3 = FILE MAINTENANCE / INQUIRY"
00320   pr f "9,9,c 55,n": "4 = pr PROOF LIST"
00330   pr f "10,9,c 55,n": "5 = pr SERVICE CODE LISTING"
00340   pr f "12,9,c 55,n": "0 = COMPLETED (RETURN TO T/M MENU)"
00350   pr f "14,9,c 55,n": "ENTER SELECTION #:"
00360 L360: input fields "14,28,N 1,EU,N": ti conv L360
00370   if ti=0 then goto L1700
00380   restore #1,key>="    ": nokey L390 eof L390
00390 L390: on ti goto L410,L580,L930,L1260,L400 none L360
00400 L400: chain "S:\acsTM\TMSCLIST"
00410 L410: pr newpage
00420   center=42-int(len(rtrm$(cnam$))/2)
00430   pr f "5,"&str$(center)&",C 50,N": cnam$
00440   pr f "8,25,c 50,h,n": "***********   WARNING   ***********"
00450   pr f "10,20,c 52,n": "THIS SELECTION WILL DESTROY ALL EXISTING SERVICE"
00460   pr f "11,20,c 60,n": "CODE RECORDS THAT ARE CURRENTLY SET UP ON THIS COMPANY."
00470   pr f "12,20,c 50,n": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO"
00480   pr f "13,20,c 23,n": "RETURN TO THE SUB-MENU."
00490 L490: input fields "13,50,C 5,IE,n": a$ conv L490
00500   if uprc$(a$)="THINK" then goto L510 else goto L260
00510 L510: i2=1
00520   close #1: ioerr L530
00530 L530: open #1: "Name=[Q]\TMmstr\SCMSTR.H[cno],KFName=[Q]\TMmstr\SCIndex.H[cno]",internal,outIn,keyed ioerr L550
00540   close #1,free: ioerr L550
00550 L550: open #1: "Name=[Q]\TMmstr\SCMSTR.H[cno],SIZE=0,RecL=43,NoShr",internal,outIn,relative ioerr L2010
00560   goto L1700
00570 L570: form pos 1,c 4,c 30,pd 4.2,pd 5.2
00580 L580: new1=1
00590 L590: pr newpage
00600   pr f "2,15,c 40,n": "*** ADD SERVICE CODE RECORDS ***"
00610   pr f "3,10,c 60,n": "ENTER SERVICE CODE NUMBER AS BLANK WHEN COMPLETED"
00620 L620: pr f mat sd$: mat se$
00630   if ti=3 or convc>0 then goto L710
00640 L640: input fields "5,30,C 4,eu,n": scode$ conv L640
00650   if ltrm$(rtrm$(scode$))="0" or rtrm$(scode$)="" then goto L260
00660   scode$=lpad$(rtrm$(scode$),4)
00670   read #1,using L570,key=scode$: sc$,ds$,th,sf nokey L730 ioerr L2010
00680   oldti=2
00690   ti=3
00700   holdsc$=sc$
00710 L710: if ti=3 or convc>0 then pr f mat sb$: sc$,ds$,th,sf
00720   pr f "23,5,C 60,H,N": "F6=HELP"
00730 L730: input fields mat sb$: sc$,ds$,th,sf conv L740
00740 L740: if convc=0 then goto L770
00750   sb$(convc)(cp1:cp2)="U  "
00760   convc=0
00770 L770: if cmdkey=6 then goto L1860
00780   if cnt=4 then goto L840
00790   convc=cnt+1
00800   cp1=len(rtrm$(sb$(convc)))-4
00810   cp2=cp1+2
00820   sb$(convc)(cp1:cp2)="RC "
00830   goto L730
00840 L840: sc$=lpad$(rtrm$(sc$),4)
00850   if ti=3 then goto L1040
00860   if rtrm$(sc$)="" or ltrm$(rtrm$(sc$))="0" then goto L260
00870   read #1,using L880,key=sc$: sc$ nokey L910 ioerr L2010
00880 L880: form pos 1,c 4
00890   pr f "5,35,c 30,h,n": "DUPLICATE SERVICE CODE NUMBER"
00900   goto L730
00910 L910: write #1,using L570: sc$,ds$,th,sf
00920   goto L390
00930 L930: pr newpage ! *****  FILE MAINT   *****
00940   pr f "10,15,c 50,n": "ENTER SERVICE CODE NUMBER, ENTER 0 WHEN COMPLETED"
00950 L950: input fields "10,70,c 4,eu,n": scode$ conv L950
00960   if ltrm$(rtrm$(scode$))="0" or rtrm$(scode$)="" then goto L260
00970   scode$=lpad$(rtrm$(scode$),4)
00980   read #1,using L570,key=scode$: sc$,ds$,th,sf nokey L950 ioerr L2010
00990   holdsc$=sc$
01000 L1000: pr newpage
01010   pr f "2,13,c 45,n": "*** REVIEW SERVICE CODE RECORDS ***"
01020   pr f "3,10,c 60,n": "ENTER SERVICE CODE NUMBER AS BLANK TO DELETE"
01030   goto L620
01040 L1040: if ltrm$(sc$)="" or rtrm$(ltrm$(sc$))="0" then goto L1050 else goto L1160
01050 L1050: pr newpage
01060   pr f "10,10,c 60,n": "SERVICE CODE NUMBER "&holdsc$&" WILL BE DELETED"
01070   pr f "12,10,c 40,n": "ENTER 1 TO DELETE; ENTER 2 TO RE-ENTER"
01080 L1080: input fields "12,55,n 1,eu,n": dcode conv L1080
01090   if dcode=1 then goto L1130
01100   if dcode><2 then goto L1080
01110   sc$=holdsc$
01120   goto L1000
01130 L1130: delete #1,key=holdsc$: 
01140   new1=1
01150   goto L930
01160 L1160: if holdsc$=sc$ then goto L1220
01170   read #1,using L880,key=sc$: sc$ nokey L1190 ioerr L2010
01180   goto L930
01190 L1190: delete #1,key=holdsc$: 
01200   new1=1
01210   goto L910
01220 L1220: rewrite #1,using L570,key=sc$: sc$,ds$,th,sf
01230   if oldti=2 then ti=2
01240   oldti=0
01250   goto L390
01260 L1260: pr newpage
01270   if process=1 then goto L1340
01280   pr f "8,10,c 50,n": "POSITION PAPER FOR SERVICE CODE PROOF LIST"
01290   pr f "12,10,c 25,n": "ENTER DATE FOR PROOF LIST"
01300 L1300: rinput fields "12,40,c 20,uE,n": dat$ conv L1300
01310   namtab=66-int(len(rtrm$(cnam$))/2)
01320   dattab=66-int(len(rtrm$(dat$))/2)
01330   pr newpage
01340 L1340: pr f "10,10,c 50,n": "PRINT SERVICE CODE PROOF LIST IN PROCESS"
01350   pr f "23,2,C 30,N": "Press F5 to stop"
01360   on fkey 5 goto L1670
01370   fnopenprn(cp,58,220,process)
01380 L1380: j=0
01390   eofc=0
01400 L1400: read #1,using L570: sc$,ds$,th,sf eof L1640 ioerr L2010
01410   j=j+1
01420   pl$(1,j)=sc$
01430   pl$(2,j)=ds$
01440   pl$(3,j)=str$(th)
01450   pl$(4,j)=str$(sf)
01460   if j=3 then goto L1480
01470   goto L1400
01480 L1480: if pcnt><0 then goto L1510
01490   pr #255,using L1500: date$,cnam$,time$,"SERVICE CODE PROOF LIST",dat$
01500 L1500: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 55,c 24,skip 1,pos dattab,c 20,skip 2
01510 L1510: for i=1 to 4
01520     pr #255,using L1530: se$(i),pl$(i,1),pl$(i,2),pl$(i,3)
01530 L1530: form pos 1,c 25,pos 30,c 30,pos 65,c 30,pos 100,c 30,skip 1
01540   next i
01550   pr #255: 
01560   mat pl$=(" ")
01570   if eofc=1 then goto L1670
01580   pcnt=pcnt+1
01590   if pcnt=11 then goto L1610
01600   goto L1380
01610 L1610: pr #255: newpage
01620   pcnt=0
01630   goto L1380
01640 L1640: if j=0 then goto L1670
01650   eofc=1
01660   goto L1480
01670 L1670: fncloseprn
01680   on fkey 5 ignore 
01690   goto L260
01700 L1700: close #1: 
01710   if new1=1 then goto L1730
01720   if ti=0 and i2=0 then goto XIT
01730 L1730: execute "Index [Q]\TMmstr\SCMSTR.H[cno]"&' '&"[Q]\TMmstr\SCIndex.H[cno] 1 4 REPLACE DupKeys"
01740   if i2=1 then chain "S:\acsTM\SVMAINT"
01750   if t1=0 then goto L1770
01760   chain "S:\acsTM\SVMAINT"
01770 L1770: goto XIT
01780 L1780: dim hlp$(20)*78,flh$(22)*18,a$*5
01790   open #10: "Name=S:\acsTM\SC.HLP,Shr",internal,outIn,relative ioerr L2010
01800   for j=1 to 20
01810     flh$(j)=str$(j+2)&",2,C 78,U,N"
01820   next j
01830   flh$(21)="1,25,C 40,H,N"
01840   flh$(22)="24,5,C 65,H,N"
01850   return 
01860 L1860: pr newpage
01870   convc=currow-4
01880   if convc<1 or convc>8 then convc=0: goto L1950
01890   cp1=pos(uprc$(sb$(convc)),"U",1)
01900   cp2=cp1+2
01910   sb$(convc)(cp1:cp2)="UC "
01920   read #10,using L1930,rec=convc: mat hlp$ noRec L1990 ioerr L2010
01930 L1930: form pos 1,20*c 78
01940   pr f mat flh$: mat hlp$,se$(convc),"ENTER 0 TO CONTINUE OR 1 TO UPDATE HELP SCREEN:"
01950 L1950: input fields "24,69,N 1,EU,N": j2 conv L1950
01960   if j2<>1 then goto L1990
01970   input fields mat flh$: mat hlp$
01980   rewrite #10,using L1930,rec=convc: mat hlp$
01990 L1990: if ti=3 then goto L1000 else goto L590
02000 L2000: if err=4152 then goto L510
02010 L2010: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2030
02020   goto L2070
02030 L2030: pr newpage
02040   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2060
02050   goto L2070
02060 L2060: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02070 L2070: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02080   input fields "24,60,C 1,N": quitcode$
02090   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
02100   pr f "23,3,C 78,N": ""
02110   pr f "24,3,C 78,N": ""
02120   retry 
02130 XIT: fnxit
