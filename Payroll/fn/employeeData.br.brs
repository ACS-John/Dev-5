def library fnEmployeeData$*64(eno,field$*64; setIt$*64,defaultIfNokey$*64,defaultIfBlank$*64)
	if ~setup then fn_setup
	fnEmployeeData$=fn_employeeData$(eno,field$, setIt$,defaultIfNokey$,defaultIfBlank$)
fnend
! todo:  logic for  defaultIfNokey$,defaultIfBlank$  is not yet complete
def fn_employeeData$*64(eno,field$*64; setIt$*64,defaultIfNokey$*64,defaultIfBlank$*64,___,key$*72,return$*64)
	gosub OpenEmployeeData
	edN(ed_no)=eno
	ed$(ed_field)=field$
	key$=fnBuildKey$('PR Employee Data',mat ed$,mat edN)
	! pr 'key$='&key$ : pause
	if setIt$<>'' then
		ed$(ed_value)=setIt$
		rewrite #hEmpData,using form$(hEmpData),key=key$: mat ed$,mat edN nokey EdNoKey
	else
		read #hEmpData,using form$(hEmpData),key=key$: mat ed$,mat edN nokey EdNoKey
		ed$(ed_value)=rtrm$(ed$(ed_value))
		if defaultIfBlank$<>'' and ed$(ed_value)='' then
			ed$(ed_value)=defaultIfBlank$
			rewrite #hEmpData,using form$(hEmpData),key=key$: mat ed$,mat edN nokey EdNoKey
		end if
	end if
	return$=ed$(ed_value)
	goto EdFinis
	EdNoKey: !
	if setIt$='' then
		return$=''
	else
		edN(ed_no)=eno
		ed$(ed_field)=field$
		ed$(ed_value)=setIt$
		write #hEmpData,using form$(hEmpData): mat ed$,mat edN
	end if
	goto EdFinis
	EdFinis: !
	fn_employeeData$=return$
fnend
OpenEmployeeData: ! r:
	if ~hEmpData then
		dim ed$(0)*128
		dim edN(0)
		hEmpData=fn_openFio('PR Employee Data',mat ed$,mat edN)
	end if
return ! /r

def library fnEmployeeDataClose
	if ~setup then fn_setup
	fnCloseFile(hEmpData,'PR Employee Data')
	hEmpData=0
fnend
Xit: fnXit
include: fn_open
include: fn_setup
