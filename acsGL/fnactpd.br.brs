! Replace S:\acsGL\fnActPd
! accounting periond from company information file
!
def library fnactpd(;actpd)
	autoLibrary
	open #tmp=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative 
	if actpd<=0 then 
		actpd=0
		read #tmp,using "Form POS 268,N 2",rec=1: actpd noRec CLOSE_TMP
	else 
		rewrite #tmp,using "Form POS 268,N 2",rec=1: actpd
	end if
	CLOSE_TMP: !
	close #tmp: 
	fnactpd=actpd
fnend 
