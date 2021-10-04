def library fnReassignNTA(filename$*256,keyForm$,ntaForm$; ___,x,key$*64,formBoth$*64,nta,recCount)
	if ~setup then fn_setup
	fnCopy(filename$,filename$&'.beforeReassignTranAddr')
	open #hRta=fnH: 'name='&filename$,i,outi,r
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
include: fn_setup
include: fn_open
