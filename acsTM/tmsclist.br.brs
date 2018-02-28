00020   on fkey 5 goto L510
00030   on error goto L550
00040 ! (C) COPYRIGHT - 1986 - ADVANCED COMPUTER SERVICES, INC.
00050   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$
00060   fntop(program$,cap$="Service Code")
00070   fncno(cno,cnam$)
00090   dim sc$*4,ds$*30
00100   dim cnam$*40,prg$*20
00110   open #1: "Name=[Q]\TMmstr\SCMSTR.H[cno],KFName=[Q]\TMmstr\SCIndex.H[cno]",internal,input,keyed ioerr L550
00120   namtab=43-int(len(rtrm$(cnam$))/2)
00130 L130: pr newpage
00140   pr f "7,10,c 60,n": "POSITION PAPER IN PRINTER FOR SERVICE CODE LISTING"
00150   pr f "10,10,c 52,n": "ENTER DATE FOR SERVICE CODE LISTING IN MMDDYY FORMAT"
00160   pr f "10,66,n 6,n": dat
00170 L170: input fields "10,66,n 6,eu,n": dat conv L170
00180   if dat<10100 or dat>123199 then goto L130
00200   pr newpage
00210   pr f "10,25,c 48,n": "SERVICE CODE LISTING IN PROCESS"
00220   pr f "23,2,c 20,n": "Press F5 to stop"
00230   fnopenprn
00250   gosub L410
00260 L260: read #1,using L270: sc$,ds$ eof L520 ioerr L550
00270 L270: form pos 1,c 4,c 30
00280   sc=int(val(sc$)/100)
00290   if fst=1 then goto L320
00300   holdsc=sc
00310   fst=1
00320 L320: if holdsc=sc then goto L350
00330   pr #255: 
00340   holdsc=sc
00350 L350: pr #255,using L360: sc$,ds$ pageoflow L380
00360 L360: form pos 1,c 4,pos 9,c 30,skip 1
00370   goto L260
00380 L380: pr #255: newpage
00390   gosub L410
00400   goto L260
00410 L410: p1=p1+1
00420   pr #255,using L430: cnam$,"PAGE",p1
00430 L430: form skip 3,pos namtab,c 40,pos 76,c 5,n 4,skip 1
00440   pr #255,using L450: "SERVICE CODE LISTING"
00450 L450: form pos 33,c 22,skip 1
00460   pr #255,using L470: dat
00470 L470: form pos 38,pic(zz/zz/zz),skip 3
00480   pr #255,using L490: "CODE","DESCRIPTION"
00490 L490: form pos 2,c 4,pos 14,c 11,skip 2
00500   return 
00510 L510: close #1: ioerr L520
00520 L520: fncloseprn
00530   if uprc$(rtrm$(prg$))="S:\acsTM\SVMAINT" then chain prg$
00540   goto XIT
00550 L550: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L570
00560   goto L610
00570 L570: pr newpage
00580   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L600
00590   goto L610
00600 L600: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00610 L610: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00620   input fields "24,60,C 1,N": quitcode$
00630   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00640   pr f "23,3,C 78,N": ""
00650   pr f "24,3,C 78,N": ""
00660   retry 
00670 XIT: fnxit
