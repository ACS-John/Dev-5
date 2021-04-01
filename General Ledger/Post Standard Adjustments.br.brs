! formerly S:\acsGL\StdAdj
! Post Standard Adjusting Entries
! r: setup library, dims, fnTop, on error
	autoLibrary
	on error goto Ertn
 
	dim ref$*12,des$*30,glan$(10)*12,glam(10),ml$(3)*80
	fnTop(program$)
	dat=date("mmddyy")
! /r
	fnTos("poststdadj")
	fnLbl(1,1,"Date to be used on Standard Adjusting Entries:",45,1)
	fnTxt(1,48,8,0,0,"1",0,"Enter the date to be used on the standard adjusting entries.")
	resp$(1)=str$(dat)
	fnLbl(1,60,"",1,1) ! space it over
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dat=val(resp$(1))
 
	open #2: "Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],NoShr",internal,output ioerr L240
	close #2,free:
	L240: !
	open #2: "Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],size=0,RecL=104,NoShr",internal,output
	open #3: "Name=[Q]\GLmstr\GLSTDAD.h[cno],KFName=[Q]\GLmstr\GLStdIdx.h[cno],Shr",internal,input,keyed
	net=0
	L270: !
	read #3,using L340: ref$,des$,glan$,glam eof ChainToAcGlMrge
	L340: form pos 1,c 12,c 30,c 12,pd 5.2
	if trim$(holdref$)<>"" and holdref$<>ref$ and totaldr+totalcr<>0 then
		gosub MSGBOX1
		goto L320
	else
		goto L320
	end if
L320: !
	if trim$(holdref$)<>"" and holdref$<>ref$ then totaldr=totalcr=0
	holdref$=ref$
	write #2,using L380: glan$,dat,glam,3,0,ref$,des$,""
	L380: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
	if glam<0 then totalcr+=glam ! add credits
	if glam>0 then totaldr+=glam ! add debits
goto L270
MSGBOX1: ! r: entries don't balance
	mat ml$(3)
	ml$(1)="Journal entry # "&trim$(ref$)&" does not foot."
	ml$(2)="Credits  = "&trim$(cnvrt$("pic(---,---,---.##)",totalcr))& "     Debits = "&trim$(cnvrt$("pic(---,---,---.##)",totaldr))
	ml$(3)="The entry will be posted, but it may need to be corrected!"
	fnmsgbox(mat ml$,resp$)
return ! /r
 
ChainToAcGlMrge: !
	if totaldr+totalcr<>0 then gosub MSGBOX1
	close #2:
	close #3:
fnchain("S:\General Ledger\Merge")
Xit: fnXit
include: ertn
