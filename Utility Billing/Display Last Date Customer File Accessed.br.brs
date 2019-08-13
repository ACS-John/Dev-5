! formerly S:\acsUB\accessdate
! -- Check for last date customers records were accessed

	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnAcs2,fnLbl,fnTxt
	library 'S:\Core\Library': fnTos
	library 'S:\Core\Library': fnCmdSet
	library 'S:\Core\Library': fnGetDir2
	on errror goto ERTN
	fntop(program$)
	dim resp$(100)*128
	! dim ln$*128
	dim gd2_date$(0)
	dim gd2_time$(0)
	fnGetDir2('[Q]\UBmstr\',mat filename$, '','CUSTOMER.H'&env$('cno'),mat gd2_date$,mat gd2_time$)
	dim access_date$*20
	access_date$=gd2_date$(1)&' '&gd2_time$(1) ! ln$(26:42)
	goto Scr1

Scr1: ! 
	fnTos
	mylen=34 : mypos=mylen+2
	fnLbl(1,1,"Last Date Customer Files Accessed:",34,1)
	fnTxt(1,mypos,20,0,0,"",1) 
	resp$(1)=access_date$
	fnCmdSet(41)
	fnAcs2(mat resp$,ck)
XIT: fnxit
include: ertn
