00020   on fkey 5 goto L400
00030   on error goto L440
00040 ! 
00050   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$
00060   fntop("S:\acsTM\emaint",cap$="Employee")
00070   fncno(cno,cnam$)
00080   fnopenprn
00090   dim eno$*9,e$*25,r(11),cnam$*40,prg$*20
00100   namtab=66-int(len(rtrm$(cnam$))/2)
00110   open #1: "Name=S:\Core\Data\acsllc\EMmstr.H[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,input,keyed ioerr L440
00120 L120: pr newpage
00130   pr f "10,10,c 48,n": "ENTER DATE FOR STAFF DIRECTORY IN MMDDYY FORMAT"
00140   pr f "10,60,n 6,n": dat
00150 L150: input fields "10,60,n 6,eu,n": dat conv L150
00160   if dat<10100 or dat>123199 then goto L120
00170   pr newpage
00180   pr f "10,25,c 30,n": "STAFF DIRECTORY IN PROCESS"
00190   pr f "23,2,c 30,n": "Press F5 to stop"
00200   gosub L290
00210 L210: read #1,using L220: eno$,e$,dept,mat r eof L400 ioerr L440
00220 L220: form pos 1,c 9,c 25,pd 2,pos 578,11*pd 3.2
00230   pr #255,using L240: eno$,e$,dept,mat r pageoflow L260
00240 L240: form pos 1,c 9,pos 12,c 25,n 5,11*n 8.2,skip 1
00250   goto L210
00260 L260: pr #255: newpage
00270   gosub L290
00280   goto L210
00290 L290: pr #255,using L300: cnam$
00300 L300: form skip 3,pos namtab,c 40,skip 1
00310   pr #255,using L320: "STAFF DIRECTORY"
00320 L320: form pos 58,c 15,skip 1
00330   pr #255,using L340: dat
00340 L340: form pos 61,pic(zz/zz/zz),skip 3
00350   pr #255,using L360: "EMPLOYEE","EMPLOYEE","DEPT"
00360 L360: form pos 2,c 8,pos 20,c 8,pos 38,c 4,skip 1
00370   pr #255,using L380: "NUMBER","NAME","CODE","RATE-1  RATE-2  RATE-3  RATE-4  RATE-5  RATE-6  RATE-7  RATE-8  RATE-9 RATE-10 RATE-11"
00380 L380: form pos 4,c 6,pos 24,c 4,pos 38,c 4,pos 44,c 88,skip 2
00390   return 
00400 L400: close #1: ioerr L410
00410 L410: fncloseprn
00420   if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then chain prg$
00430   goto XIT
00440 L440: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L460
00450   goto L500
00460 L460: pr newpage
00470   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L490
00480   goto L500
00490 L490: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00500 L500: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00510   input fields "24,60,C 1,N": quitcode$
00520   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00530   pr f "23,3,C 78,N": ""
00540   pr f "24,3,C 78,N": ""
00550   retry 
00560 XIT: fnxit
