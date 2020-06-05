! 
	dim tr(2)
	pr newpage
	pr f "10,15,C 60": "ENTER THE COMPANY NUMBER TO BE CHECKED: 2"
L70: input fields "10,55,N 2,UE,N": cno conv L70
	if cno=0 then stop 
! 
	open #1: "Name=[Q]\CLmstr\TRMSTR.H[cno]",internal,outIn 
	open #2: "Name=[Q]\CLmstr\TRALLOC.h[cno]",internal,outIn,relative 
L120: read #1,using L130: checkNumber$,mat tr eof END1
L130: form pos 4,c 8,pos 79,2*pd 3
	v1=val(checkNumber$) conv L120
	if v1>13070 then goto END1
	if v1<13042 or v1>13070 then goto L120
	checkNumber$=cnvrt$("N 8",v1+1)
	rewrite #1,using L130: checkNumber$
	ta=0
	r2=tr(1)
L200: if r2=0 then goto L250
	read #2,using L220,rec=r2: ok$,nta
L220: form pos 4,c 8,pos 65,pd 3
	rewrite #2,using L220,rec=r2: checkNumber$
	r2=nta: goto L200
L250: goto L120
END1: stop 
