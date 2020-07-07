def library fnActPd(; actPd,___,h)
	! Current Accounting Period (from company information file)
	autoLibrary
	! if env$('acsDeveloper')='John' then debug=1 else debug=0
	! if debug then pr 'fnActPd(; '&str$(actPd)&')'
	! if debug then pause
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
