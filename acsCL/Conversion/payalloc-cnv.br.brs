! Replace S:\acsCL\Conversion\PayAlloc-Cnv
! convert PayAlloc file - version 0 to version 0 
! not sure exactally what it does yet

autoLibrary

open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
open #2: "Name=[Q]\CLmstr\X.h[cno],RecL=164,Replace",i,outi,r
open #3: "Name=[Q]\CLmstr\PayAlloc.h[cno],Size=0,RecL=56,Replace",i,outi,r
do
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11
	dim gl(3),ytdp,typ
	read #1,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,N 3,N 6,N 3': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat gl eof L190
	dim ta(2)
	mat ta=(lrec(3)+1)
	write #3,using 'Form POS 1,C 8,N 3,N 6,N 3,PD 3.2,C 30,PD 3': vn$,mat gl,100,'',0
	write #2,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
loop
L190: !
close #1: : close #2: : close #3:
fnCopy('[Q]\CLmstr\X.h[cno]','[Q]\CLmstr\PayMstr.h[cno]')
fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx1.h[cno]','1 8')
fnIndex('[Q]\CLmstr\PayMstr.h[cno]','[Q]\CLmstr\PayIdx2.h[cno]','9 30')
fnFree('[Q]\CLmstr\X.h[cno]')
fnStatus("Completed converting payee file.")
Xit: fnXit
