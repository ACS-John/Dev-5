! formerly S:\acsGL\StdAdj
! Post Standard Adjusting Entries
! r: setup library, dims, fnTop, on error
autoLibrary
on error goto Ertn
fnTop(program$)
dat=date("mmddyy")
! /r
fnTos
fnLbl(1,1,"Date to be used on Standard Adjusting Entries:",45,1)
fnTxt(1,48,8,0,0,"1",0,"Enter the date to be used on the standard adjusting entries.")
resp$(1)=str$(dat)
fnLbl(1,60,"",1,1) ! space it over
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
dat=val(resp$(1))
open #hMerge=fnH: "Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],RecL=104,Replace,NoShr",internal,output
open #hStdAdj=fnH: "Name=[Q]\GLmstr\glStdAd.h[cno],KFName=[Q]\GLmstr\glStdIdx.h[cno],Shr",internal,input,keyed
net=totalDb=totalCr=0
dim holdref$*12
holdref$=''
do
	dim ref$*12,des$*30,glan$(10)*12,glam(10)
	read #hStdAdj,using L340: ref$,des$,glan$,glam eof ChainToAcGlMrge
	L340: form pos 1,c 12,c 30,c 12,pd 5.2
	
	if trim$(holdref$)<>"" and holdref$<>ref$ and totalDb+totalCr<>0 then
		gosub MsgDoesNotFoot
	end if
	if trim$(holdref$)<>"" and holdref$<>ref$ then totalDb=totalCr=0
	holdref$=ref$
	
	write #hMerge,using L380: glan$,dat,glam,3,0,ref$,des$,""
	L380: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8
	if glam<0 then totalCr+=glam ! add credits
	if glam>0 then totalDb+=glam ! add debits
loop

MsgDoesNotFoot: ! r: entries don't balance
	dim ml$(3)*128
	mat ml$(3)
	ml$(1)="Journal entry "&trim$(ref$)&" does not foot."
	ml$(2)="Credits  = "&trim$(cnvrt$("pic(---,---,---.##)",totalCr))& "     Debits = "&trim$(cnvrt$("pic(---,---,---.##)",totalDb))
	ml$(3)="The entry will be posted, but it may need to be corrected!"
	fnmsgbox(mat ml$,resp$)
return ! /r
 
ChainToAcGlMrge: !
	if totalDb+totalCr<>0 then gosub MsgDoesNotFoot
	close #hMerge:
	close #hStdAdj:
fnchain("S:\General Ledger\Merge")
Xit: fnXit
include: ertn
