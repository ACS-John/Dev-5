open #trmstr=1: "Name=[Q]\CLmstr\TRMSTR.h[cno]",internal,outIn
open #2: "Name=[Q]\CLmstr\TRALLOC.h[cno]",i,outi,r
L110: read #trmstr,using 'Form POS 1,N 2,N 1,C 8,POS 79,2*PD 3': bank_code,tcde,checkNumber$,mat tr eof END1
	v1=val(checkNumber$) conv L110
	if v1<7822 or v1>7848 then goto L110
	checkNumber$=cnvrt$("N 8",v1-3000)
	rewrite #trmstr,using 'Form POS 4,C 8': checkNumber$
	ta=0
	r2=tr(1)
L200: if r2=0 then goto L250
	read #2,using L220,rec=r2: ok$,nta
L220: form pos 4,c 8,pos 65,pd 3
	rewrite #2,using L220,rec=r2: checkNumber$
	r2=nta: goto L200
L250: goto L110
END1: close #trmstr:
	close #2:
	execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX1.h[cno] 1 11 REPLACE DupKeys"
	execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX2.h[cno] 1/28/4 3/8/8 REPLACE DupKeys"
stop
