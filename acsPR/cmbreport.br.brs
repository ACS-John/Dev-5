! Replace S:\acsPR\Cmbreport.br
! creates a screen ace combobox for employee records
def library fncmbemp(myline,mypos; addall,container,indexfile$*200)
	library 'S:\Core\Library': fncombof
	dim df$*200
	dim if$*200
	if addall<>1 then addall=0
	if addall=0 then fen$="CeMP" else fen$="CEmpALL"
	if indexfile$="" then if$="[Q]\PRmstr\EmployeeIdx-no.h[cno]" else if$=indexfile$
	fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\Employee.h[cno]",1,8,9,30,if$,1+addall,1,"Select from the list of employees. To add an employee, go to the Employee File.",container)
	indexfile$=""
fnend 

