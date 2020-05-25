! Replace S:\acsGL\Conversion\PRmstr-CNV
! CONVERT GL PAYROLL MASTER FILE
 
	autoLibrary
	fnTop(program$,"Convert GL Payroll Master File")
	on error goto Ertn
 
	dim pr1$*90,pr1(18),pr2(36)
 
	pr newpage
	pr f "08,08,C 32,R,N": " CONVERT GL PAYROLL MASTER FILE"
	pr f "10,5,C 60": "ENTER COMPANY NUMBER TO BE CONVERTED:"
	pr f "12,15,C 16,B,5": "PRESS F5 TO STOP"
L140: input fields "10,43,N 5,UE,N": cno conv L140
	if cmdkey=5 then goto Xit
 
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr L140
	open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,Replace",internal,output
L190: read #1,using L200: pr1$,mat pr1 eof END1
L200: form pos 1,c 90,18*pd 5.2,2*n 5
	for j=1 to 11: pr2(j)=pr1(j): next j
	pr2(13)=pr1(12)
	for j=13 to 18: pr2(j+18)=pr1(j): next j
	write #2,using L250: pr1$,mat pr2
L250: form pos 1,c 90,36*pd 5.2,2*n 5
	goto L190
 
END1: close #1:
	close #2:
	execute "COPY "&env$('Temp')&"\Work."&session$&", [Q]\GLmstr\PRmstr.h[cno]/ -n"
	execute "Index [Q]\GLmstr\PRmstr.h[cno],[Q]\GLmstr\PRIndex.h[cno],1,4,Replace,DupKeys -n"
	fnputcno(cno)
	chain "S:\acsGL\Company"
 
Xit: stop
 
include: Ertn
 
