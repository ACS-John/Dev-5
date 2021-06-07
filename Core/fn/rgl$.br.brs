! Replace S:\Core\fnRgl$
! format the answer to fnQgl -
def library fnrgl$*60(acctIn$; returnMaxLength,___,desc$*50)
	autoLibrary

	if returnMaxLength=0 then returnMaxLength=35

	! acctIn$ should be formatted as though it were just read in and is ready
	!    for a read Key=...   ie '  0   100  0'

	fnGetUseDeptAndSub(useDept,useSub)

	open #glmstr=fnH: 'Name=[Q]\'&cursys$&'mstr\GLmstr.h[cno],KFName=[Q]\'&cursys$&'mstr\GLIndex.h[cno],Shr',internal,input,keyed ioerr L_ERR_OPEN_FOR_DESC
	read #glmstr,using 'Form Pos 13,C 50',key=acctIn$: desc$ nokey NOKEYGLMSTR ioerr NOKEYGLMSTR
	close #glmstr:
	L_ERR_OPEN_FOR_DESC: !
	! reformat it from a read key= ready format to an input ready format
	if useSub=0 then
		acctIn$(10:12)=''
	else
		acctIn$(10:12)='-'&trim$(acctIn$(10:12))
	end if
	acctIn$(4:9)=trim$(acctIn$(4:9))
	if useDept =0 then
		acctIn$(1:3)=''
	else
		acctIn$(1:3)=trim$(acctIn$(1:3))&'-'
	end if
	goto Finis

	NOKEYGLMSTR: !
		close #glmstr:
	goto Finis

	Finis: !
	! pr ' fnRgl$ returned "'&(trim$(rpad$(acctIn$,14)&desc$))(1:returnMaxLength)&'"'
	fnrgl$=(trim$(rpad$(acctIn$,14)&desc$))(1:returnMaxLength)

fnend

