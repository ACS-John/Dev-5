autoLibrary
open #trmstr=fnH: "Name=[Q]\CLmstr\TRMSTR.h[cno]",i,outi,r
for j=1 to lrec(1)
	read #trmstr,using L140,rec=j: d1 noRec NEXT_J
	L140: form pos 12,n 6
	d1=fncd(d1)
	rewrite #trmstr,using 'form pos 85,N 2,N 6',rec=j: 19,d1
	NEXT_J: !
next j
close #trmstr:
fnIndex("[Q]\CLmstr\TRMSTR.h[cno]","[Q]\CLmstr\TRIDX1.h[cno]","1 11")
fnIndex("[Q]\CLmstr\TRMSTR.h[cno]","[Q]\CLmstr\TRIDX2.h[cno]","28/1 8/11")
