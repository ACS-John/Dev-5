 
	on error goto L320
	autoLibrary
	fnTop(program$,cap$="Employee")
	fncno(cno,cnam$)
 
 
	fnconsole(1)
	dim eno$*9,prg$*20
L110: pr newpage
	pr f "8,25,c 30,r,n": "********  WARNING  ********"
	pr f "11,10,c 70": "THIS PROGRAM REMOVES ALL EMPLOYESS THAT ARE CODED"
	pr f "12,10,c 70": "AS TERMINATED.  BE SURE THAT THE FIRM PRODUCTIVITY"
	pr f "13,10,c 70": "REPORTS HAVE BEEN RUN BEFORE CONTINUING."
	pr f "15,10,c 62": "ENTER 1 TO CONTINUE; ELSE ENTER 2 TO RETURN TO THE SYSTEM MENU"
L170: input fields "15,75,N 1,UE,N": a conv L170
	on a goto L190,L310 none L110
L190: pr newpage
	pr f "10,20,c 50,n": "REMOVE TERMINATED EMPLOYEES IN PROCESS"
	open #1: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,outIn,keyed ioerr L320
L220: read #1,using L230: eno$,emp eof L270 ioerr L320
L230: form pos 1,c 9,pos 37,n 1
	if emp><9 then goto L220
	delete #1:
	goto L220
L270: close #1:
	execute "Index S:\Core\Data\acsllc\EMmstr.h[cno],S:\Core\Data\acsllc\EMIndex.h[cno],1,9,REPLACE,DupKeys"
	if uprc$(rtrm$(prg$))="S:\Client Billing\Legacy\EMAINT" then chain 'S:\Client Billing\Legacy\EMAINT'
	goto Xit
L310: goto Xit
L320: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L340
	goto L380
L340: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L370
	goto L380
L370: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L380: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto Xit
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
Xit: fnXit
