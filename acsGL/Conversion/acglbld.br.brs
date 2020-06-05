! Replace S:\acsGL\acglBld.br
! build something
def library fnacglbld
	autoLibrary
	on error goto Ertn
	close #102: ioerr ignore
	open #102: "SRow=5,SCol=13,ERow=14,ECol=64,Border=SR,Caption=<Initial File Preparation",display,outIn
	pr #102: newpage : _
	pr #102,fields "1,1,Cc 52,R,N": env$('cnam') : _
	pr #102,fields "2,1,Cc 52,R,N": "Company Number [cno]"
	pr #102,fields "4,1,Cc 52,N": " ******************   WARNING   ******************"
	pr #102,fields "6,1,Cc 52,N": " This selection will destroy all existing records"
	pr #102,fields "7,1,Cc 52,N": " in the GL Master and Transactions File."
	pr #102,fields "9,2,C 26,N": " Enter ERASE to continue:"
	pr f "15,35,C 09,B,5": "Exit (F5)"
	L190: !
	input #102,fields "9,29,Cu 5,UT,N": pas$
	if cmdkey=5 then goto Xit
	if pas$><"ERASE" then goto L190
 
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],Size=0,RecL=416,Replace",internal,output
	close #1:
	open #1: "Name=[Q]\GLmstr\glTrans.h[cno],Size=0,RecL=73,Replace",internal,output
	write #1,using L270: 0,0,0,0,0,0,0," "," ",1
	L270: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
	close #1:
	open #1: "Name=[Q]\GLmstr\AcTrans.h[cno],Size=0,RecL=72,Replace",internal,output
	close #1:
	open #1: "Name=[Q]\GLmstr\acglPgMn.h[cno],Size=0,RecL=58,Replace",internal,outIn,relative
	for j=1 to 20 : _
		write #1,using 'Form POS 1,C 20,C 35,3*N 1',rec=j: "","",0,0,0 : _
	next j
	close #1:
	open #1: "Name=[Q]\GLmstr\gl1099.h[cno]",internal,output ioerr L370
	close #1:
	goto L390
	L370: !
	open #1: "Name=[Q]\GLmstr\gl1099.h[cno],Size=0,RecL=127",internal,output
	close #1:
	L390: !
	open #1: "Name=[Q]\GLmstr\glTr1099.h[cno]",internal,output ioerr L420
	close #1:
	goto L460
	L420: !
	open #1: "Name=[Q]\GLmstr\glTr1099.h[cno],Size=0,RecL=64",internal,output,relative
	write #1,using L440,rec=1: "",0,0,"","",1
	L440: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
	close #1:
	L460: !
	open #1: "Name=[Q]\GLmstr\glBRec.h[cno]",internal,output ioerr L490
	close #1:
	goto L510
	L490: open #1: "Name=[Q]\GLmstr\glBRec.h[cno],Size=0,RecL=68",internal,output
	close #1:
	L510: !
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno]",internal,output ioerr L530
	goto L540
	L530: !
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],Size=0,RecL=280",internal,output
	L540: !
	close #1:
	open #1: "Name=[Q]\GLmstr\acPrCks.h[cno]",internal,output ioerr L570
	goto L580
	L570: !
	open #1: "Name=[Q]\GLmstr\acPrCks.h[cno],Size=0,RecL=110",internal,output,relative
	L580: !
	close #1:
	fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[Q]\GLmstr\GLIndex.h[cno]','1 12')
	fnIndex('[Q]\GLmstr\GLmstr.h[cno]','[Q]\GLmstr\glIndx2.h[cno]','13 30')
	fnIndex('[Q]\GLmstr\gl1099.h[cno]','[Q]\GLmstr\gl109Idx.h[cno]','1 8')
	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
	fnIndex('[Q]\GLmstr\PRmstr.h[cno]','[Q]\GLmstr\PRINDEX.h[cno]','1 4')
	fnIndex('[Q]\GLmstr\AcTrans.h[cno]','[Q]\GLmstr\AcTrIdx.h[cno]','1/71/17/13 12/2/2/4')
	fnchain("S:\General Ledger\Accounts")
	goto Xit
Xit: fnend
include: Ertn
