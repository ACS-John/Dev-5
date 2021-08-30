! Replace S:\Core\fnRgl$
! format the answer to fnQgl -
def library fnrgl$*60(acctIn$; returnMaxLength,___,desc$*50,return$*60)
	autoLibrary

	if returnMaxLength=0 then returnMaxLength=35

	! acctIn$ should be formatted as though it were just read in and is ready
	!    for a read Key=...   ie '  0   100  0'

	fnGetUseDeptAndSub(useDept,useSub)

	open #hAcct=fnH: 'Name=[Q]\[cursys]mstr\GLmstr.h[cno],KFName=[Q]\[cursys]mstr\GLIndex.h[cno],Shr',i,i,k ioerr L_ERR_OPEN_FOR_DESC
	read #hAcct,using 'Form Pos 13,C 50',key=acctIn$: desc$ nokey NOKEYGLMSTR ioerr NOKEYGLMSTR
	close #hAcct:
	L_ERR_OPEN_FOR_DESC: !
	! reformat it from a read key= ready format to an input ready format
	if useSub then
		acctIn$(10:12)='-'&trim$(acctIn$(10:12))
	else
		acctIn$(10:12)=''
	end if
	acctIn$(4:9)=trim$(acctIn$(4:9))
	if useDept then
		acctIn$(1:3)=trim$(acctIn$(1:3))&'-'
	else
		acctIn$(1:3)=''
	end if
	goto Finis

	NOKEYGLMSTR: !
		close #hAcct:
	goto Finis

	Finis: !
	return$=(trim$(rpad$(acctIn$,14)&desc$))(1:returnMaxLength)
	if return$='--' or return$='-' then return$(1)=''
	fnrgl$=return$

fnend

