def library fnActPd(; actPd,___,h)
	! accounting periond from company information file
	autoLibrary
	open #h=fngethandle: 'Name=[Q]\GLmstr\Company.h[cno],Shr',internal,outIn,relative 
	if actPd<=0 then 
		read #h,using 'Form POS 268,N 2',rec=1: actPd noRec ActPdFinis
	else 
		rewrite #h,using 'Form POS 268,N 2',rec=1: actPd
	end if
	ActPdFinis: !
	close #h: 
	fnActPd=actPd
fnend
