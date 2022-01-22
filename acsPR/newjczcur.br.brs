! Replace S:\acsPR\newjcZCur
! Zero Current Periond Information
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
	dim eno$*12,jno$*6,tr(9),pd$*30,cap$*128,ml$(2)*60
	dim response$(5)*1
 
	fnTop("S:\acsPR\jczcur",cap$="Zero Current Period Info")
	fncno(cno)
 
 
	mat ml$(2) : _
	ml$(1)="Are you sure you want to zero all" : _
	ml$(2)="current period information?." : _
	fnmsgbox(mat ml$,resp$,cap$,36)
 
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i,r
	read #1,using L190,rec=1: kt
L190: form pos 745,n 1
	close #1:
	if kt=1 then goto L290
 
	open #1: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output
	restore #1:
	write #1,using L260: " "," ",mat tr," ",1
L260: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	close #1:
 
L290: open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno]",i,outIn,k
	if kt=0 then goto L360
L310: read #2,using L330: a1,a2,a3 eof DONE
	rewrite #2,using L330: 0,0,0
L330: form pos 79,3*pd 7.2
	goto L310
 
L360: read #2,using L380: a1,a2,a3,a4,a5 eof DONE
	rewrite #2,using L380: 0,0,0,0,0
L380: form pos 79,3*pd 7.2,pos 118,2*pd 3
	goto L360
 
DONE: !
	close #2:
Xit: fnXit
 
include: ertn
 
