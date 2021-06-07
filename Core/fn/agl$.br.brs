! Replace S:\Core\fnAGL$
! format the answer to fnQgl -
def library fnagl$*12(&x$)
	autoLibrary

	if x$='[All]' or x$='' then
		fnagl$='  0     0  0'
		goto Xit
	end if

	fnGetUseDeptAndSub(useDept,useSub)

	! strip off any description
	x$=x$(1:14)
	! find the position of the '-'s
	dash1=pos(x$,'-')
	dash2=pos(x$,'-',-1)
	! reformat it into a read key= ready format
	if dash1=0 and dash2=0 and len(x$)=12 then
	! do nothing - it is already formatted properly
	else if useDept<>0 and useSub<>0 then
		x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
	else if useDept =0 and useSub<>0 then
		x$='  0'&lpad$(trim$(x$(1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
	else if useDept =0 and useSub =0 then
		x$='  0'&lpad$(trim$(x$),6)&'  0'
	else if useDept<>0 and useSub =0 then
		x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:len(x$))),6)&'  0'
	end if

	Finis: !
		x$=lpad$(trim$(x$),12)
		fnagl$=x$(1:12)
	goto Xit

	Xit: !
fnend
