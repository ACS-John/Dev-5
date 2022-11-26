! Replace S:\acsGL\FixRef
! trying to place income statement reference numbers back into a chart of accounts, might be handy, but don't need on menu

autoLibrary
on error goto Ertn


fnTop(program$,'Fix Reference Numbers')
open #1: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr',i,outIn,k
fil$(1)='ACGLFNSB' : idx$(1)='agfsidx4'
fil$(2)='ACGLFNSc' : idx$(2)='agfsidx1'
fil$(3)='ACGLFNSi' : idx$(3)='agfsidx3'
fil$(4)='ACGLFNSj' : idx$(4)='agfsidx2'
fil$(5)='ACGLfNSf' : idx$(5)='agfsidx5'
fil$(6)='ACGLfNSg' : idx$(6)='agfsidx6'

close #2: ioerr ignore
open #2: 'Name=[Q]\GLmstr\'&fil$(6)&'&.h[cno],KFName=[Q]\GLmstr\'&idx$(6)&'.h[cno]',i,outIn,k
do
	L240: !
	dim d$*50
	dim rf(6)
	read #1,using L250: dno,ano,sno,d$,mat rf eof L350
	L250: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
	restore #2:
	L270: !
	read #2,using L300: rno,refd$,type$ eof L240
	L300: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
	if type$<>'D' then goto L270
	if d$(1:15)=refd$(1:15) then goto L310 else goto L270 ! try to find match on descriptions


	L310: !
	rf(6)=rno ! rewrite new reference back into g/l account  ?
	rewrite #1,using L330: mat rf
	L330: form pos 63,6*pd 3
loop
L350: close #2:
stop

Xit: fnXit

include: ertn

