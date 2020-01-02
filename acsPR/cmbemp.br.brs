! Replace S:\acsPR\CmbEmp.br
! creates a screen ace combobox for employee records
def library fncmbemp(myline,mypos; addall,container)
	library 'S:\Core\Library': fncombof
	if addall<>1 then addall=0
	if addall=0 then 
		fen$="CeMP.h[cno]" 
	else 
		fen$="CEmpALL.h[cno]"
	end if
	fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\Employee.h[cno]",1,8,9,30,'[Q]\PRmstr\EmployeeIdx-no.h[cno]',1+addall,0,"Select from the list of employees. To add an employee, go to the Employee File.",container)
fnend
