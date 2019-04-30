00020   on fkey 5 goto L490
00022   on error goto L530
00030 ! 
00040   library 'S:\Core\Library': fntop,fncno,fnxit,fnopenprn,fncloseprn
00044   fntop(program$,cap$="Client Listing")
00046   fncno(cno,cnam$)
00060   form pos 1,n 2,c 40,c 20,pos 83,n 6,pos 89,2*n 1
00070   fnopenprn
00100   dim z$*5,a$*30,cnam$*40,prg$*20,app(20),ma(20),a3$*30
00110   namtab=42-int(len(rtrm$(cnam$))/2)
00120   pr newpage
00130   pr f "10,10,c 52,n": "ENTER 1 FOR NUMERIC LISTING, ENTER 2 FOR ALPHABETIC"
00140 L140: input fields "10,65,n 1,eu,n": numalp conv L230
00141   pr newpage
00143   pr f "10,5,C 60": "ENTER APPLICATION NUMBER TO pr OR BLANK FOR ALL:"
00144 L144: input fields "10,58,N 1,UE,N": sapp conv L144
00145   if sapp<0 or sapp>20 then goto L144
00150   if numalp<1 or numalp>2 then goto L140
00160   if numalp=1 then goto L170 else goto L190
00170 L170: open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed ioerr L530
00180   goto L200
00190 L190: open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,input,keyed ioerr L530
00200 L200: pr newpage
00210   pr f "10,10,c 46,n": "ENTER DATE FOR CLIENT LISTING IN MMDDYY FORMAT"
00220   pr f "10,60,n 6,n": dat
00230 L230: input fields "10,60,n 6,eu,n": dat conv L230
00240   if dat<10100 or dat>123199 then goto L200
00250   pr newpage
00260   pr f "10,25,c 48,n": "CLIENT LISTING IN PROCESS"
00270   pr f "23,2,c 30,n": "Press F5 to stop"
00280   gosub L370
00290 L290: read #1,using L300: z$,a$,a3$,mat app,mat ma eof L490 ioerr L530
00300 L300: form pos 1,c 5,c 30,x 30,c 30,pos 375,20*n 1,20*pd 3.2
00302   if sapp=0 then goto L304
00303   if app(sapp)=0 then goto L290
00304 L304: for j=1 to 20
00305     if app(j)=0 then goto L308
00306     if ma(j)>0 then app(j)=2
00308 L308: next j
00310   pr #255,using L320: z$,a$ pageoflow L340 ! MAT APP
00320 L320: form pos 1,c 5,pos 10,c 30,20*n 3,skip 1
00322 ! pr #255: TAB(10);A3$ PAGEOFLOW 340
00330   goto L290
00340 L340: pr #255: newpage
00350   gosub L370
00360   continue 
00370 L370: p1=p1+1
00380   pr #255,using L390: date$,cnam$,"PAGE",p1
00390 L390: form skip 3,pos 1,c 8,pos namtab,c 40,pos 76,c 5,n 4,skip 1
00400   pr #255,using L410: time$,"CLIENT LISTING"
00410 L410: form pos 1,c 8,pos 35,c 16,skip 1
00420   pr #255,using L430: dat
00430 L430: form pos 39,pic(zz/zz/zz),skip 3
00440   pr #255,using L450: "CLIENT"
00450 L450: form pos 2,c 6,skip 1
00460   pr #255,using L470: "NUMBER","CLIENT NAME"
00470 L470: form pos 2,c 6,pos 14,c 26,c 60,skip 2
00480   return 
00490 L490: close #1: ioerr L500
00500 L500: fncloseprn
00510   if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$
00520   goto XIT
00530 L530: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L550
00540   goto L590
00550 L550: pr newpage
00560   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L580
00570   goto L590
00580 L580: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00590 L590: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00600   input fields "24,60,C 1,N": quitcode$
00610   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00620   pr f "23,3,C 78,N": ""
00630   pr f "24,3,C 78,N": ""
00640   retry 
00650 XIT: fnxit
