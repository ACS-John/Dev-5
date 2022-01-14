! Replace S:\Core\CmbCNo.br
! screen ace combobox of available company numbers

def library fncmbcno(myline,mypos; mysys$,___,dataPath$*256)
	autoLibrary
	on error goto Ertn

	! the response$ for this has the company number in pos 43:47
	!   will change with 5 digit company numbers


	if trim$(mysys$)='' then 
		dataPath$='[Q]\[cursys[mstr'
	else 
		dataPath$='[Q]\'&mysys$&'mstr'
	end if 
  
	fngetdir(dataPath$,mat filename$,empty$,temp$='Company.h*')
	dim opt$(0)*48
	dim filename$(0)*18
	mat opt$(9999) : mat filename$(9999)
	for a=1 to udim(filename$)
		filename$=trim$(filename$)
		if filename$(a)<>'' then 
			end=len(filename$(a))
			opt$(a)=filename$(a)(10:end)
			open #x=fnH: 'Name='&dataPath$&'\Company.h'&opt$(a),internal,input 
			dim cnam$*40
			read #x,using 'form pos 1,c 40': cnam$
			close #x: 
			if val(opt$(a))=99999 then 
				kill99999=1
			else 
				kill99999=0
			end if 
			opt$(a)=cnam$&' ('&cnvrt$('pic(#####)',val(opt$(a)))&')'
		else 
			if kill99999=1 then 
				mat opt$(a-2)
			else 
				mat opt$(a-1)
			end if 
			goto EXITFOR
		end if 
	next a
	EXITFOR: ! 
	fncomboa('CmbCNo-'&env$('cursys'),myline,mypos,mat opt$,'Select from currently installed companies for '&env$('cursystem'),55)
	goto Xit
	Xit: !
fnend 
include: ertn
