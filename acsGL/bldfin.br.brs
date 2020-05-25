! Replace S:\acsGL\BldFin
! ???
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
 
	dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
	dim gln(3,3),fin(3),ta(2),ac(18),te$*1
 
	fncno(cno)
 
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\GLmstr\ACGLFNSI.h[cno]",internal,output
READ_GLMSTR: !
	read #1,using L180: dno,ano,sno,d$,mat rf eof END1
	if ano<1000 then goto READ_GLMSTR
L180: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
	rno=rno+10
	ac(1)=3
	if ano<2000 then ac(5)=1 else ac(5)=0
	write #2,using L230: rno,d$,"D",mat ac
L230: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
	rf(2)=rno
	rewrite #1,using L180: dno,ano,sno,d$,mat rf
	goto READ_GLMSTR
END1: !
	close #1:
	close #2:
Xit: stop
 
include: Ertn
 
