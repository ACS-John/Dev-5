def library fncch$*20(; cch$*20)
	autoLibrary
	dim cch$*20
	get=1 : put=2
	if trim$(cch$)="" then get_or_put=get else get_or_put=put
	open #h=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r
	if get_or_put=get then 
		read #h,using "form pos 276,C 20",rec=1: cch$ noRec Finis
	end if
	if get_or_put=put then 
		rewrite #h,using "form pos 276,C 20",rec=1: cch$
	end if
	Finis: close #h:
	fncch$=cch$
fnend
