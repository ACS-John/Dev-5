! Replace S:\acsGL\ActPd.br
! accounting periond from company information file
 
def library fnActPd(;actPd)
	autoLibrary
	! get_or_put   =   get=1 : put=2
	if actPd=0 then get_or_put=1 else get_or_put=2
	open #tmp=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
	if get_or_put=1 then 
		read #tmp,using "Form POS 268,N 2",rec=1: actPd noRec CLOSE_TMP
	else if get_or_put=2 then 
		rewrite #tmp,using "Form POS 268,N 2",rec=1: actPd
	end if
	CLOSE_TMP: !
	close #tmp:
	fnActPd=actPd
fnend
