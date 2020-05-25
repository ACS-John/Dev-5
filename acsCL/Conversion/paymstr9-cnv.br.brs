! Replace S:\acsCL\Conversion\PayMstr9-CNV
! converts any length PayMstr file to 189 length and recreateing indexes
 
	autoLibrary
	on error goto Ertn
 
	dim gl(3),ta(2)
 
	fncno(cno)
L100: gosub WIN
	pr f "11,25,Cr 27": "Company Number to Convert:"
	pr f "13,18,Cc 44,R,N": "F5: Stop"
L130: rinput fields "11,52,Nz 2,U,N": cno
	if cmdkey=5 then goto Xit
	if cno=0 then pr f "23,1,c 7,n": bell$ : goto L130
 
	gosub WIN
	pr f "11,19,Cc 42,N": "Conversion in Progress: Please wait..."
	pr f "13,18,Cc 44,R,N": "Do Not Stop"
	pause
 
	execute "Copy [Q]\CLmstr\PayMstr.h[cno],X -D -189"
	execute "Free [Q]\CLmstr\PayMstr.h[cno]"
	execute "Copy X [Q]\CLmstr\PayMstr.h[cno]"
	execute "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx1.H[cno],1,8,Replace,DupKeys"
	execute "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx2.H[cno],9,28,Replace,DupKeys"
 
	gosub WIN
	pr f "11,19,Cc 42,N": "Conversion Completed for Company Number [cno]"
	pr f "13,18,Cc 44,R,N": "Enter: Continue to Next Company  F5: Stop"
	input fields "12,18,C 1,AE,N": pause$
	if cmdkey=5 then goto DONE
	goto L100
 
DONE: close #101: ioerr Xit
Xit: fnXit
 
WIN: close #101: ioerr L390
L390: open #101: "SRow=10,SCol=18,ERow=12,ECol=61,Border=DR,Caption=PayMstr Record Length Conversion",display,outIn
	pr #101: newpage
return
 
include: Ertn
 
