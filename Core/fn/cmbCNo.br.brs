! screen ace combobox of available company numbers
def library fnCmbCNo(myline,mypos; mysys$,___,dataPath$*256,a,cnam$*40,hx,temp$,kill99999)
	autoLibrary
	on error goto Ertn
	! the response$ for this has the company number in pos 43:47
	!   changed with 5 digit company numbers

	if trim$(mysys$)='' then
		dataPath$='[Q]\[cursys]mstr'
	else
		dataPath$='[Q]\'&mysys$&'mstr'
	end if

	dim filename$(0)*128
	mat filename$(0)
	fnGetDir2(dataPath$,mat filename$, '','company.h*')
	dim opt$(0)*48
	mat opt$(udim(mat filename$))
	for a=1 to udim(mat filename$)
		if filename$(a)<>'' then
			opt$(a)=filename$(a)(10:inf)
			open #hx=fnH: 'Name='&dataPath$&'\'&filename$(a),i,i
			read #hx,using 'form pos 1,c 40': cnam$
			close #hx:
			if val(opt$(a))=99999 then kill99999=1 else kill99999=0
			opt$(a)=cnam$&' ('&cnvrt$('pic(#####)',val(opt$(a)))&')'
		else
			if kill99999=1 then mat opt$(a-2) else mat opt$(a-1)
			goto ExitFor
		end if
	next a
	ExitFor: !
	fnComboA('CmbCNo-'&env$('cursys'),myline,mypos,mat opt$,'Select from currently installed companies for '&env$('cursystem'),55)
	goto Xit
	Xit: !
fnend
include: ertn
