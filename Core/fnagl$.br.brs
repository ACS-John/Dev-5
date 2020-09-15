! Replace S:\Core\fnAGL$
! format the answer to fnQgl -
def library fnagl$*12(&x$)
	autoLibrary
	on error goto Ertn

	if x$="[All]" or x$='' then let fnagl$="  0     0  0" : goto Xit
	tmp_cursys$=env$('CurSys')
	if env$('CurSys')='UB' and exists("[Q]\GLmstr\Company.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
	if env$('CurSys')='UB' and exists("[Q]\UBmstr\GLmstr.h[cno]") then tmp_cursys$="UB": goto COMPANY_POST_READ
	if env$('CurSys')='PR' and exists("[Q]\GLmstr\Company.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
	if env$('CurSys')='PR' and exists("[Q]\CLmstr\Company.h[cno]") then tmp_cursys$="CL": goto COMPANY_OPEN
	if env$('CurSys')='PR' and exists("[Q]\PRmstr\glmstr.h[cno]") then tmp_cursys$="PR": goto COMPANY_OPEN
	if env$('CurSys')='CR' and exists("[Q]\GLmstr\GLmstr.h[cno]") then tmp_cursys$="GL": goto COMPANY_OPEN
	if env$('CurSys')='CR' and exists("[Q]\GLmstr\GLmstr.h[cno]")=0 then tmp_cursys$="CR": goto COMPANY_OPEN
	if env$('CurSys')='CL' then tmp_cursys$='CL' else tmp_cursys$='GL'
	! find out if I should use the department number and/or the sub account number
	COMPANY_OPEN: !
		open #company:=fnH: "Name=[Q]\"&tmp_cursys$&"mstr\Company.h[cno],Shr",internal,input ioerr COMPANY_OPEN_IOERR
		if tmp_cursys$='GL' then
			read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
		else if tmp_cursys$='CL' then
			read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
		end if
	COMPANY_POST_READ: !
		if tmp_cursys$="PR" or tmp_cursys$="UB" or tmp_cursys$="CR" then use_dept=use_sub=1
		close #company: ioerr ignore
		! strip off any description
		x$=x$(1:14)
		! find the position of the "-"s
		dash1=pos(x$,"-")
		dash2=pos(x$,"-",-1)
		! reformat it into a read key= ready format
		if dash1=0 and dash2=0 and len(x$)=12 then
		! do nothing - it is already formatted properly
		else if use_dept<>0 and use_sub<>0 then
			x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
		else if use_dept =0 and use_sub<>0 then
			x$="  0"&lpad$(trim$(x$(1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3)
		else if use_dept =0 and use_sub =0 then
			x$="  0"&lpad$(trim$(x$),6)&"  0"
		else if use_dept<>0 and use_sub =0 then
			x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:len(x$))),6)&"  0"
		end if
	! If USE_DEPT =0 AND USE_SUB =0 Then Goto 350 Else Goto 370
	! If X$(1:3)="  0" Then x$(1:3)="   " ! kj
	! If X$(10:12)="  0" Then x$(10:12)="   " ! kj
	! x$="  0"&LPAD$(TRIM$(X$),6)&"  0": Goto 390 ! kj
	FINIS: !
		x$=lpad$(trim$(x$),12)
		fnagl$=x$(1:12)
	goto Xit
	COMPANY_OPEN_IOERR: ! r:
		x$=lpad$(trim$(x$(1:dash1-1)),3)&lpad$(trim$(x$(dash1+1:dash2-1)),6)&lpad$(trim$(x$(dash2+1:len(x$))),3) ! default if gl or cl not installed
	goto FINIS ! /r
Xit: fnend
include: Ertn
