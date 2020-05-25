	on error goto L180
	autoLibrary
	dim actpd$*6,pedat$*20,tb$*32
	fnTop(program$)
	open #20: "Name=CNO.H"&wsid$,internal,input,relative
	read #20,using L80,rec=1: cno,cnam$,dat$,cp,nw,process,actpd$,pedat$
L80: form pos 1,n 2,c 40,pos 63,c 20,pos 89,2*n 1,pos 141,n 1,pos 153,c 6,pos 195,c 20
	dim ln1$*78,ln$(10)*78,shd$*60,fli$(20),cnam$*40,dat$*20
	open #1: "Name=[Q]\GLmstr\AcGLNote.h[cno],Shr",internal,outIn,relative
	open #2: "Name=test,size=0,RecL=128,replace",display,output
L120: read #1,using L130: mat ln$ eof L160 noRec L160
L130: form pos 1,10*c 78
	pr #2,using L130: mat ln$
	goto L120
L160: close #1: : close #2:
	execute "copy test [Q]\GLmstr\AcGLNote.h[cno]"
L180: fnxit
	goto L240
	pr newpage
	if err=4148 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L230
	goto L240
L230: pr f "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L240: pr f "24,1,C 80,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto L300
	pr f "23,1,C 80,N": ""
	pr f "24,1,C 80,N": ""
	retry
L300: fnxit
