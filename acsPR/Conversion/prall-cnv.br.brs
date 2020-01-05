! Replace S:\acsPR\Conversion\prAll-Cnv
! not sure, but with a name like prAll, I bet it does alot.

library 'S:\Core\Library': fntop,fnxit, fndate_mmddyy_to_ccyymmdd
on error goto Ertn

execute "Copy [Q]\PRmstr\RPMstr.h[cno] X -196 -n"
execute "Copy X [Q]\PRmstr\RPMstr.h[cno] -D -n"
execute "Index [Q]\PRmstr\RPMstr.h[cno]"&' '&"[Q]\PRmstr\RPIndex.h[cno] 1 8 Replace DupKeys -n"
execute "Index [Q]\PRmstr\RPMstr.h[cno]"&' '&"[Q]\PRmstr\RPIndx2.h[cno] 9 30 Replace DupKeys -n"
open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn 
LoopTop: !
	read #4,using L160: d1 eof Eo4,conv L210
	L160: form pos 9,n 6
	d1=fndate_mmddyy_to_ccyymmdd(d1) ! d1=19000000+FNCD(D1)
	rewrite #4,using L190: d1
	L190: form pos 9,pd 6
	goto LoopTop
	L210: !
	read #4,using L190: d1 eof Eo4
goto LoopTop
Eo4: !
close #4: 
execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys"
open #1: "Name=[Q]\PRmstr\prCode.h[cno],SIZE=0,RecL=512,Replace",internal,output,relative 
write #1,using L270,rec=1: 0,0,0,0
L270: form pos 1,n 1,n 2,n 1,n 5
close #1: 

XIT: stop 
include: Ertn

