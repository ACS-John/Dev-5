def library fncch$*20(; cch$*20)
	autoLibrary
	dim cch$*20
	get=1 : put=2
	if trim$(cch$)="" then get_or_put=get else get_or_put=put
	open #h=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
	if get_or_put=get then 
		read #h,using "Form POS 276,C 20",rec=1: cch$ noRec Finis
	end if
	if get_or_put=put then 
		rewrite #h,using "Form Pos 276,C 20",rec=1: cch$
	end if
	Finis: close #h:
	fncch$=cch$
fnend
