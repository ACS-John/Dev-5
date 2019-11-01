def library fnReassignNTA(filename$*256,keyForm$,ntaForm$; ___,x,key$*64,formBoth$*64,nta,recCount)
	if ~setup then let fn_setup
	fnCopy(filename$,filename$&'.beforeReassignTranAddr')
	open #hRta:=fngethandle: 'name='&filename$,internal,outin,relative
	recCount=lrec(hRta)
	! r: gather mat keys$ and mat ntas
	mat keys$(recCount)
	mat ntas(recCount)
	mat keys$=('')
	mat ntas=(0)
	formBoth$=keyForm$&','&(ntaForm$(6:inf))
	for x=1 to recCount
		read #hRta,using formBoth$,rec=x: key$,nta norec Rnta_NoRec1
		keys$(x)=key$
		ntas(x)=nta
		Rnta_NoRec1: !
	next x
	! /r
	! pr 'debug sample'
	! for x=1 to 10
	! 	pr keys$(x),ntas(x)
	! nex x
	! pause
	! r: update and rewrite the ones needing correction
	for x=1 to recCount
		key$=keys$(x)
		if key$<>'' then
			nta=srch(mat keys$,key$,x+1)
			if nta<0 then nta=0
			if ntas(x)<>nta then
				ntas(x)=nta
				rewrite #hRta,using ntaForm$,rec=x: nta
			end if
		end if
	nex x
	! /r
	close #hRta:
fnend
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fntop,fnxit
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnAddOneC,fnAddOneN
		library 'S:\Core\Library': fncreg_read,fncreg_write
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnTos,fnFlexInit1,fnFlexAdd1,fnCmdKey,fnAcs2
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnMsgBox
		library 'S:\Core\Library': fnCloseFile
		gosub Enum
	end if
fnend
include: Enum
include: fn_open