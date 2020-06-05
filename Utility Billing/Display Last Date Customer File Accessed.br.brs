! formerly S:\acsUB\accessdate
! -- Check for last date customers records were accessed
	autoLibrary
	on error goto Ertn
	fnTop(program$)
	dim gd2_date$(0)
	dim gd2_time$(0)
	fnGetDir2('[Q]\UBmstr\',mat filename$, '','CUSTOMER.H'&env$('cno'),mat gd2_date$,mat gd2_time$)
	dim access_date$*20
	access_date$=gd2_date$(1)&' '&gd2_time$(1) ! ln$(26:42)
	fnTos
	fnLbl(1,1,"Last Date Customer Files Accessed:",34,1)
	fnTxt(1,36,20,0,0,"",1)
	dim resp$(100)*128
	resp$(1)=access_date$
	fnCmdSet(41)
	fnAcs2(mat resp$,ckey)
Xit: fnXit
include: Ertn
