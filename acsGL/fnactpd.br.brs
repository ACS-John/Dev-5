! Replace S:\acsGL\fnActPd
! accounting periond from company information file
!
def library fnactpd(; actpd)
	autoLibrary
	open #h=fngethandle: 'Name=[Q]\GLmstr\Company.h[cno],Shr',internal,outIn,relative 
	if actpd<=0 then 
		read #h,using 'Form POS 268,N 2',rec=1: actpd noRec Finis
	else 
		rewrite #h,using 'Form POS 268,N 2',rec=1: actpd
	end if
	Finis: !
	close #h: 
	fnactpd=actpd
fnend 
