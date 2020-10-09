! Replace S:\acsPR\Conversion\CnvAll-Cnv
! CONVERT MASTER FILES
 
	autoLibrary
	on error goto Ertn
 
	dim gl(3),ta(2),k$*20,de$*30,fgl$(3)
 
L90: pr newpage
	close #101: ioerr L110
L110: open #101: "SROW=9,SCOL=9,EROW=11,ECOL=59,BORDER=DR,CAPTION=CONVERT MASTER FILES",display,outIn
	pr f "10,10,C 45": "COMPANY NUMBER TO CONVERT (0 TO STOP):"
	input fields "10,56,N 5,UE,N": cno
	if cno=0 then goto Xit
	open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\PayAlloc.h[cno],SIZE=0,RecL=56,Replace",internal,outIn,relative
L180: read #1,using 'Form POS 1,C 8,POS 147,N 3,N 6,N 3': p$,mat gl eof L280
	mat ta=(0)
! IF SUM(GL)=0 THEN GOTO 190
	lr2=lrec(2)+1
	write #2,using L230,rec=lr2: p$,mat gl,100,"",0
L230: form pos 1,c 8,n 3,n 6,n 3,pd 3.2,c 30,pd 3
	mat ta=(lr2)
	rewrite #1,using L260: mat ta
L260: form pos 147,2*pd 3
	goto L180
L280: close #1:
	close #2:
	execute "Copy [Q]\CLmstr\PayTrans.h[cno],X -D -n"
	execute "Copy X [Q]\CLmstr\PayTrans.h[cno]  -95 -n"
	execute "Index [Q]\CLmstr\PayTrans.h[cno],[Q]\CLmstr\UnPdIdx1.h[cno],1,20,Replace,DupKeys -n"
	open #1: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno]",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\UnPdAloc.h[cno],SIZE=0,RecL=70,Replace",internal,outIn,relative
L350: read #1,using L360: k$,mat gl,de$,amt eof L460
L360: form pos 1,c 20,pos 33,n 3,n 6,n 3,c 18,n 10.2
	mat ta=(0)
	lr2=lrec(2)+1
	write #2,using L400,rec=lr2: k$,mat gl,amt,de$,0
L400: form pos 1,c 20,n 3,n 6,n 3,pd 5.2,c 30,pd 3
	mat ta=(lr2)
	rewrite #1,using L430: "",mat ta
L430: form pos 33,c 12,pos 90,2*pd 3
	goto L350
 
L460: close #1:
	close #2:
	open #1: "Name=[Q]\CLmstr\FUNDMSTR.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno]",internal,outIn,keyed ioerr NF1
	open #2: "Name=X,RecL=63,Replace",internal,output
L500: read #1,using L510: dn$,de$,mat fgl$ eof L560
L510: form pos 1,c 3,c 30,3*c 9
	write #2,using L530: dn$,de$,fgl$(1),fgl$(2),0,fgl$(3)
L530: form pos 1,c 3,c 30,2*c 9,n 3,c 9
	goto L500
 
L560: close #1:
L570: close #2:
	execute "COPY X,[Q]\CLmstr\FUNDMSTR.h[cno] -n"
	execute "Index [Q]\CLmstr\FUNDMSTR.h[cno],[Q]\CLmstr\FundIdx1.h[cno],1,3,Replace,DupKeys -n"
	execute "Index [Q]\CLmstr\FUNDMSTR.h[cno],[Q]\CLmstr\FundIdx2.h[cno],4,28,Replace,DupKeys -n"
	goto L730
 
L630: pr newpage
	pr f "12,5,C 60": "COMPLETED CONVERTING FILES FOR COMPANY #: [cno]"
	pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
	input fields "13,40,C 1,IAE,N": pause$
	goto L90
 
NF1: open #2: "Name=X,RecL=63,Replace",internal,output
	write #2,using L530: "  0","GENERAL FUND","","",0,""
	goto L570
 
L730: ! Replace S:\acsCL\TRALLOC.CNV
	execute "Copy [Q]\CLmstr\TRALLOC.h[cno] X -79 -n"
	fnFree("[Q]\CLmstr\TRALLOC.h[cno]")
	execute "Rename X [Q]\CLmstr\TRALLOC.h[cno] -n"
	goto L630
 
include: ertn
 
Xit: stop
 
