! Replace S:\acsGL\FixBal ! moves the beginning balance to current balance if current balance is 0 used for new client who entered beginning balance and no current balance
! we have a new menu option that does a better job - could be deleted
 
	autoLibrary
	on error goto Ertn
	fnTop(program$)
 
	dim io1$(12),gln(3,3),fin(3),ta(2),ac(18),te$*1
	dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
 
	fncno(cno)
 
	open #glmstr=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
READ_GLMSTR: !
	read #glmstr,using 'Form POS 1,N 3,N 6,N 3,C 50,6*PD 3,42*PD 6.2,2*PD 3': dno,ano,sno,d$,mat rf,bb,cb eof END1
	if cb=0 then cb=bb else goto READ_GLMSTR
	rewrite #glmstr,using 'Form POS 1,N 3,N 6,N 3,C 50,6*PD 3,42*PD 6.2,2*PD 3': dno,ano,sno,d$,mat rf,bb,cb
	goto READ_GLMSTR
 
END1: !
	close #glmstr:
Xit: stop
 
include: ertn
 
