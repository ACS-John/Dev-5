open #trmstr=1: "Name=[Q]\CLmstr\TRMSTR.h[cno]",internal,outIn 
open #3: "Name=[Q]\CLmstr\TRALLOC.h[cno],Shr",internal,outIn 
L60: read #trmstr,using 'form pos 1,N 2,N 1,C 8': bc,tc,checkNumber$ eof END1
	ck1=val(checkNumber$) conv L60
	if bc><1 then goto L60
	if ck1<33642 or ck1>33761 then goto L60
	rewrite #trmstr,using 'form pos 1,N 2': 2
	goto L60
END1: ! 
L130: read #3,using 'form pos 1,N 2,N 1,C 8': bc,tc,checkNumber$ eof END3
	ck1=val(checkNumber$) conv L130
	if bc><1 then goto L130
	if ck1<33642 or ck1>33761 then goto L130
	rewrite #3,using 'form pos 1,N 2': 2
	goto L130
END3: ! 
	close #trmstr: 
	close #3: 
	execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX1.h[cno] 1 11 REPLACE DupKeys -n"
	execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX2.h[cno] 28/1 8/11 REPLACE DupKeys -n"
