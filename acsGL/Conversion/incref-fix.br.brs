autoLibrary
	dim io1$(2)
	io1$(1)="11,50,N 2,U,N"
	fnTop(program$)
	io1$(2)="12,46,N 2,U,N"
	pr newpage
	close #101: ioerr L90
L90: open #101: "SROW=10,SCOL=10,EROW=13,ECOL=52,BORDER=DR,CAPTION=FIX INCOME STATEMENT REFERENCE #'S",display,outIn 
	pr f "11,11,C 40": "ENTER THE COMPANY # WITH GOOD NUMBERS:"
	pr f "12,11,C 40": "ENTER THE COMPANY # TO BE CHANGED:"
	pr f "14,13,C 34,R,N": "PRESS F1 TO CONTINUE OR F5 TO STOP"
L130: input fields mat io1$,attr "R": cn1,cn2 conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L220 else ce=curfld
L160: ce=ce+1: if ce>udim(io1$) then ce=1
L170: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L160
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L130
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L170
L220: if cmdkey=5 then goto L350
	open #1: "Name=[Q]\GLmstr\GLmstr.H"&str$(cn1)&",KFName=[Q]\GLmstr\GLINDEX.H"&str$(cn1)&"",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLmstr.H"&str$(cn2)&",KFName=[Q]\GLmstr\GLINDEX.H"&str$(cn2)&",Shr",internal,outIn,keyed 
L270: read #1,using L280: k$,rf4 eof END1
L280: form pos 1,c 12,pos 72,pd 3
	rewrite #2,using L300,key=k$: rf4 nokey L270
L300: form pos 72,pd 3
	goto L270
END1: ! 
	close #1: 
	close #2: 
L350: stop 
