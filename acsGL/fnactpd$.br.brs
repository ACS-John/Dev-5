! Replace S:\acsGL\fnActPd$
def library fnActPd$(; accountingPeriod$)
	autoLibrary
	open #h=fnH: 'Name=[Q]\GLmstr\Company.h[cno],Shr',i,outi,r
	if trim$(accountingPeriod$)='' then ! get
		read #h,using 'form pos 270,C 6',rec=1: accountingPeriod$ noRec Finis
	else ! put
		rewrite #h,using 'form pos 270,C 6',rec=1: accountingPeriod$
	end if
	Finis: !
	close #h:
	fnactpd$=accountingPeriod$
fnend
