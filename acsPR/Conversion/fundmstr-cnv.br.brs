! Replace S:\acsPR\Conversion\FundMstr-Cnv
! Convert Fund Master File
 
	autoLibrary
	on error goto Ertn
 
	dim de$*30,fgl$(3)
 
L90: pr newpage
	close #101: ioerr L110
L110: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=Convert Fund Master File",display,outIn
	pr f "10,5,C 60": "COMPANY NUMBER (0 TO STOP):"
	input fields "10,51,N 5,UE,N": cno
	if cno=0 then goto Xit
	open #1: "Name=[Q]\CLmstr\FUNDMSTR.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno]",internal,outIn,keyed ioerr NF1
	open #2: "Name=X,RecL=63,Replace",internal,output
L180: read #1,using L190: dn$,de$,mat fgl$ eof L240
L190: form pos 1,c 3,c 30,3*c 9
	write #2,using L210: dn$,de$,fgl$(1),fgl$(2),0,fgl$(3)
L210: form pos 1,c 3,c 30,2*c 9,n 3,c 9
	goto L180
 
L240: close #1:
L250: close #2:
	execute "COPY X,[Q]\CLmstr\FUNDMSTR.h[cno] -n"
	execute "Index [Q]\CLmstr\FUNDMSTR.h[cno],[Q]\CLmstr\FundIdx1.h[cno],1,3,Replace,DupKeys -n"
	execute "Index [Q]\CLmstr\FUNDMSTR.h[cno],[Q]\CLmstr\FundIdx2.h[cno],4,28,Replace,DupKeys -n"
	pr f "12,5,C 60": "Completed Converting FundMstr File for Company: [cno]"
	pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
	input fields "13,40,C 1,IAE,N": pause$
	goto L90
 
NF1: open #2: "Name=X,RecL=63,Replace",internal,output
	write #2,using L210: "  0","General Fund","","",0,""
	goto L250
 
include: Ertn
 
Xit: stop
 
