! Replace S:\acsUB\conversion\remove_blank_altadr
! remove blank alternate billing addresses (still get key= after conversion and causes blank addresses  ( on old system the addresses were just set to blank when deleted, but not the customer #
 
	autoLibrary
	on error goto Ertn
 
	fncno(cno)
 
	dim ba$(4)*30
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,outIn,keyed
	form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6
L130: ! read alternate billing address
	read #3,using L180: z$,mat ba$ eof Xit
	pr z$,mat ba$
	pause
	if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L220
L180: form pos 1,c 10,4*c 30
	read #1,using L200,key=z$: holdz$ nokey L220
L200: form pos 1,c 10
	goto L130
L220: delete #3,key=z$:
	goto L130
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L300
	execute "list -"&str$(line) : _
	pause  : _
	goto L300
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L300: execute act$
	goto ERTN
 
