autoLibrary
! set all base reference # to 0 in income statement file
dim r$*5,d$*50,te$*1,ac(9),fil$(3),idx$(3)
fnTop(program$)
open #1: "Name=[Q]\GLmstr\ACGLFNSI.h[cno]",internal,outIn,relative 
for j1=1 to lrec(1)
	read #1,using L180,rec=j1: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,gp eof L210,conv NJ1,noRec NJ1
	L180: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
	rewrite #1,using L180,rec=j1: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,0
	NJ1: !
next j1
L210: !
close #1: 
