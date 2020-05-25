! Replace S:\Core\CmbCNo.br
! screen ace combobox of available company numbers

def library fncmbcno(myline,mypos; mysys$)
	autoLibrary
	on error goto Ertn

	! the response$ for this has the company number in pos 43:47
	!   will change with 5 digit company numbers

	dim opt$(9999)*48,filename$(9999)*18
	dim resp$(10)*80
	dim sys_data_path$*256

	mat opt$(9999) : mat filename$(9999)
	if trim$(mysys$)='' then 
		sys_data_path$='[Q]\'&env$('cursys')&"mstr"
	else 
		sys_data_path$='[Q]\'&mysys$&"mstr"
	end if 
  
	fngetdir(sys_data_path$,mat filename$,empty$,temp$="Company.h*")
	for a=1 to udim(filename$)
		filename$=trim$(filename$)
		if filename$(a)<>"" then 
			end=len(filename$(a))
			opt$(a)=filename$(a)(10:end)
			open #x:=fngethandle: "Name="&sys_data_path$&"\Company.h"&opt$(a),internal,input 
			dim cnam$*40
			read #x,using "Form pos 1,c 40": cnam$
			close #x: 
			if val(opt$(a))=99999 then 
				kill99999=1
			else 
				kill99999=0
			end if 
			opt$(a)=cnam$&" ("&cnvrt$("pic(#####)",val(opt$(a)))&")"
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
	fncomboa('CmbCNo-'&env$('cursys'),myline,mypos,mat opt$,'Select from currently installed companies for the '&env$('cursys')&' system.',55)
	goto Xit
	Xit: !
fnend 
include: Ertn
