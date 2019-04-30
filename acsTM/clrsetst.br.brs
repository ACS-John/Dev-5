00020 ! 
00030   on error goto L770
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$,fnconsole
00050   fncno(cno,cnam$)
00060   form pos 1,n 2,c 40,c 20,pos 83,n 6
00070   dim z$*5,a$*30,sc(10),cnam$*40,cat$(30)*30,prg$*20
00080   namtab=43-int(len(rtrm$(cnam$))/2)
00090   open #1: "Name=S:\Core\Data\acsllc\TMCat.h[cno],Shr",internal,input,relative ioerr L770
00100   read #1,using L110,rec=1: mat cat$ ioerr L770
00110 L110: form pos 1,30*c 30
00120   close #1: 
00130   open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed ioerr L770
00140 L140: pr newpage
00150   pr f "10,10,c 53,n": "ENTER 1 FOR A LISTING, ENTER 2 TO RESET STATUS CODES"
00160   input fields "10,65,n 1,eu,n": lstres conv L220
00170   on lstres goto L180,L580 none L140
00180 L180: pr newpage
00190   pr f "7,10,c 60,n": "POSITION PAPER IN PRINTER FOR LISTING OF COMPLETED JOBS"
00200   pr f "10,2,c 68,n": "ENTER DATE OF LISTING IN MMDDYY FORMAT"
00210   pr f "10,72,n 6,n": dat
00220 L220: input fields "10,72,n 6,eu,n": dat conv L220
00230   if dat<10100 or dat>123199 then goto L180
00240   pr newpage
00250   pr f "10,15,c 60,n": "COMPLETED JOB LISTING IN PROCESS"
00260   gosub L450
00270 L270: read #1,using L280: z$,a$,mat sc eof L570 ioerr L770
00280 L280: form pos 1,c 5,c 30,pos 220,10*n 1
00290   namcd=0
00300   for j=1 to 10
00310     if sc(j)=2 then goto L320 else goto L430
00320 L320: if namcd=1 then goto L380
00330     namcd=1
00340     pr #255: 
00350     pr #255,using L360: z$,a$,j,cat$(j)
00360 L360: form pos 1,c 5,pos 10,c 30,pos 50,n 2,pos 55,c 30,skip 1
00370     goto L430
00380 L380: pr #255,using L390: j,cat$(j) pageoflow L410
00390 L390: form pos 50,n 2,pos 55,c 30,skip 1
00400     goto L430
00410 L410: pr #255: newpage
00420     gosub L450
00430 L430: next j
00440   goto L270
00450 L450: p1=p1+1
00460   pr #255,using L470: cnam$,"PAGE ",p1
00470 L470: form skip 3,pos namtab,c 40,pos 76,c 5,n 4,skip 1
00480   pr #255,using L490: "LISTING OF COMPLETED JOBS"
00490 L490: form pos 29,c 28,skip 1
00500   pr #255,using L510: dat
00510 L510: form pos 39,pic(zz/zz/zz),skip 3
00520   pr #255,using L530: "CLIENT","CATEGORY","CATEGORY"
00530 L530: form pos 2,c 6,pos 47,c 8,pos 59,c 8,skip 1
00540   pr #255,using L550: "NUMBER","NAME","NUMBER","NAME"
00550 L550: form pos 2,c 6,pos 22,c 4,pos 49,c 6,pos 61,c 4,skip 2
00560   return 
00570 L570: pr #255: newpage
00580 L580: pr newpage
00590   pr f "10,10,c 53,n": "ENTER CLIENT NUMBER TO CHANGE, ENTER 0 WHEN COMPLETE"
00600 L600: input fields "10,72,n 5,eu,n": clinum conv L600
00610   if clinum=0 then goto L740
00620   read #1,using L280,key=lpad$(rtrm$(str$(clinum)),5): z$,a$,mat sc nokey L580 ioerr L770
00630 L630: pr newpage
00640   pr f "6,10,c 69,n": "CLIENT NUMBER "&ltrm$(z$)&"   CLIENT NAME "&rtrm$(a$)
00650   pr f "10,10,c 53,n": "ENTER CATEGORY NUMBER TO RESET, ENTER 0 WHEN COMPLETE"
00660 L660: input fields "10,72,n 2,eu,n": cat conv L660
00670   if cat=0 then goto L720
00680   if cat<1 or cat>10 then goto L630
00690   if sc(cat)=1 then goto L630
00700   sc(cat)=0
00710   goto L630
00720 L720: rewrite #1,using L280,key=lpad$(rtrm$(str$(clinum)),5): z$,a$,mat sc
00730   goto L580
00740 L740: close #1: 
00750   if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$
00760   goto XIT
00770 L770: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L790
00780   goto L830
00790 L790: pr newpage
00800   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L820
00810   goto L830
00820 L820: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00830 L830: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00840   input fields "24,60,C 1,N": quitcode$
00850   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00860   pr f "23,3,C 78,N": ""
00870   pr f "24,3,C 78,N": ""
00880   retry 
00890 XIT: fnxit
