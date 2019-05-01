	library 'S:\Core\Library': fnopenprn,fncloseprn
	on fkey 5 goto L830
	on error goto L860
! 
	library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$
	fntop(program$,cap$="Client Directory")
	fncno(cno,cnam$)
	open #1: "Name=S:\Core\Data\acsllc\TMCat.h[cno],Shr",internal,input,relative ioerr L860
	read #1,using L110: mat cat$ ioerr L860
L110: form pos 1,10*c 30
	close #1: 
	for j=1 to 10
		flo$(j)=str$(j+10)&",30,C 30,N"
		fli$(j)=str$(j+10)&",62,N 1,U,N"
	next j
	dim z$*5,a$(5)*30,ph$*12,cnam$*40,prg$*20,cm$*70
	dim cat$(10)*30,flo$(10),fli$(10),catcode(10),dd(10),ph2$*12
	namtab=44-len(rtrm$(cnam$))/2
	open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed ioerr L860
L210: pr newpage
	pr f "10,10,c 48,n": "ENTER DATE FOR CLIENT DIRECTORY IN MMDDYY FORMAT"
	pr f "10,60,n 6,n": dat
L250: input fields "10,60,n 6,eu,n": dat conv L250
	if dat<10100 or dat>123199 then goto L210
	form pos 83,n 6
	pr newpage
	pr f "3,15,C 65,N": "You have the option to get the client directory sorted by "
	pr f "4,10,c 70,n": "category.  Place a 1 by any category you want printed.  Leave all"
	pr f "5,10,c 70,n": "items blank for numeric order."
	pr f mat flo$: mat cat$
L340: input fields mat fli$: mat catcode conv L340
	if sum(catcode)=0 then numprint=1
	pr newpage
	pr f "10,25,c 48,n": "CLIENT DIRECTORY IN PROCESS"
	pr f "23,2,c 30,n": "Press F5 to stop"
	fnopenprn(cp,0,0,process)
	form c 9,skip 0
	if numprint=1 then goto L440
	for j=1 to 10
		if catcode(j)=0 then goto L790
L440: gosub L610
L450: read #1,using L460: z$,mat a$,ph$,pno,mye,mat dd,ph2$,cm$ eof L760 ioerr L860
L460: form pos 1,c 5,5*c 30,c 12,pos 179,n 9,n 2,pos 190,10*pd 3,pos 260,c 12,pos 305,c 70
		if numprint =1 then goto L490
		if dd(j)>0 then goto L490 else goto L450
L490: pr #255,using L500: z$,a$(1),"BUS PHONE:",ph$,mye,pno
L500: form pos 2,c 5,pos 8,c 30,pos 39,c 10,pos 50,c 12,pos 70,pic(zzz),pos 76,pic(zzzzzzzzz),skip 1
		pr #255,using L520: a$(2),"HOME PHONE:",ph2$
L520: form pos 8,c 30,pos 39,c 11,pos 51,c 30,skip 1
		pr #255,using L540: a$(3),"CONTACT:",a$(4) pageoflow L580
L540: form pos 8,c 30,pos 39,c 8,pos 48,c 30,skip 1
		pr #255,using L560: cm$
L560: form pos 8,c 70,skip 2
		goto L450
L580: pr #255: newpage
		gosub L610
		goto L450
L610: p1=p1+1
		pr #255,using L630: date$,cnam$,"PAGE",p1
L630: form skip 3,pos 1,c 8,pos namtab,c 40,pos 76,c 5,n 4,skip 1
		pr #255,using L650: time$,"CLIENT DIRECTORY"
L650: form pos 1,c 8,pos 36,c 16,skip 1
		if numprint = 1 then cattab=37 else cattab=44-len(rtrm$(cat$(j)))/2
		if numprint = 1 then pr #255,using L680: "NUMERIC ORDER" else pr #255,using L680: cat$(j)
L680: form pos cattab,c 30,skip 1
		pr #255,using L700: dat
L700: form pos 40,pic(zz/zz/zz),skip 3
		pr #255,using L720: "CLIENT","YEAR","EMPLOYEE"
L720: form pos 2,c 6,pos 70,c 4,pos 77,c 8,skip 1
		pr #255,using L740: "NUMBER","NAME AND ADDRESS","CLIENT INFORMATION","END","IN CHARGE"
L740: form pos 2,c 6,pos 12,c 16,pos 42,c 19,pos 71,c 3,pos 76,c 9,skip 2
		return 
L760: if numprint=1 then goto L800
		pr #255: newpage
		restore #1,key>="     ": nokey L800
L790: next j
L800: close #1: ioerr L830
	if numprint =1 then goto L830 else let fncloseprn
	goto L840
L830: fncloseprn
L840: if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$
	goto XIT
L860: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L880
	goto L920
L880: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L910
	goto L920
L910: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L920: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY OR ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto XIT
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry 
XIT: fnxit
