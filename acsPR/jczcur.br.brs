! Replace S:\acsPR\jcZCur
! Zero Current Periond Information

autoLibrary
on error goto Ertn

dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),tn$*6
dim eno$*12,jno$*6,tr(9),pd$*30,message$*40,msgline$(2)*60
dim response$(5)*1

fnTop(program$,'Zero Current Period Info')

msgline$(1)='Are you sure you wish to zero all'
msgline$(2)='current period information? (Y/N)'
fnoldmsgbox(mat response$,'',mat msgline$,2)
if response$(1)='N' then goto Xit


fnwait(message$='Zeroing: please wait...',0)

open #1: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i,r
read #1,using L260,rec=1: kt
L260: form pos 745,n 1
close #1:
if kt=1 then goto L360

open #1: 'Name=[Q]\PRmstr\JCTRANS.h[cno]',internal,output
restore #1:
write #1,using L330: ' ',' ',mat tr,' ',1
L330: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
close #1:

L360: !
open #2: 'Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno]',i,outIn,k
if kt=0 then 
	do
		read #2,using L450: a1,a2,a3,a4,a5 eof DONE
		L450: form pos 79,3*pd 7.2,pos 118,2*pd 3
		rewrite #2,using L450: 0,0,0,0,0
	loop
else
	do
		read #2,using L400: a1,a2,a3 eof DONE
		L400: form pos 79,3*pd 7.2
		rewrite #2,using L400: 0,0,0
	loop
end if


DONE: !
	close #2:
Xit: fnXit

include: ertn

