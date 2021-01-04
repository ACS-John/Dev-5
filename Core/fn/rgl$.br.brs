! Replace S:\Core\fnRgl$
! format the answer to fnQgl -
def library fnrgl$*60(x$; ReturnMaxLength)
	autoLibrary

	if ReturnMaxLength=0 then returnMaxLength=35

	! X$ should be formatted as though it were just read in and is ready
	!    for a read Key=...   ie "  0   100  0"

	dim desc$*50

	if env$('CurSys')="UB" and exists("[Q]\GLmstr\Company.h[cno]") then cursys$="GL": goto L180
	if env$('CurSys')="UB" and exists("[Q]\UBmstr\GLmstr.h[cno]") then cursys$="UB": goto L180
	if env$('CurSys')="PR" and exists("[Q]\GLmstr\Company.h[cno]") then cursys$="GL": goto L180
	if env$('CurSys')="PR" and exists("[Q]\CLmstr\Company.h[cno]") then cursys$="CL": goto L180
	if env$('CurSys')="PR" and exists("[Q]\PRmstr\glmstr.h[cno]") then cursys$="PR": goto L180
	if env$('CurSys')='CL' then cursys$='CL' else cursys$='GL'
	if env$('CurSys')='CR' and exists("[Q]\GLmstr\Company.h[cno]") then cursys$='GL'
	if env$('CurSys')='CR' and exists("[Q]\GLmstr\Company.h[cno]")=0 then cursys$='CR'
	! find out if I should use the department number and/or the sub account number
	L180: !
	if cursys$='GL' or cursys$='CL' then
		open #company=fnH: "Name=[Q]\"&cursys$&"mstr\Company.h[cno],Shr",internal,input ioerr L260
		read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub
		close #company:
	else if cursys$="PR" or cursys$="UB" or cursys$="CR" then
		L260: !
		use_dept=use_sub=1 ! default both to use
	end if
	desc$=''
	open #glmstr=fnH: "Name=[Q]\"&cursys$&"mstr\GLmstr.h[cno],KFName=[Q]\"&cursys$&"mstr\GLIndex.h[cno],Shr",internal,input,keyed ioerr L_ERR_OPEN_FOR_DESC
	read #glmstr,using "Form Pos 13,C 50",key=x$: desc$ nokey NOKEYGLMSTR ioerr NOKEYGLMSTR
	close #glmstr:
	L_ERR_OPEN_FOR_DESC: !
	! reformat it from a read key= ready format to an input ready format
	if use_sub=0 then
		x$(10:12)=""
	else
		x$(10:12)="-"&trim$(x$(10:12))
	end if
	x$(4:9)=trim$(x$(4:9))
	if use_dept =0 then
		x$(1:3)=""
	else
		x$(1:3)=trim$(x$(1:3))&"-"
	end if
	goto Finis

	NOKEYGLMSTR: !
		close #glmstr:
		x$="": desc$=""
	goto Finis

	Finis: !
	! pr ' fnRgl$ returned "'&(trim$(rpad$(x$,14)&desc$))(1:ReturnMaxLength)&'"'
	fnrgl$=(trim$(rpad$(x$,14)&desc$))(1:ReturnMaxLength)

fnend

