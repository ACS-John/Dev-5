dim fen$*64

def library fnCmbSubCat(myline,mypos; addall,container)
	! creates a screen ace combobox for sub-category records
	autoLibrary
	if addall<>1 then addall=0
	if addall=0 then fen$='Subcat.h[cno]' else fen$='SubCatALL.h[cno]'
	fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\SCMStR.h[cno]',1,3,4,25,'[Q]\PRmstr\SCindEx.h[cno]',1+addall,0,'Select from the list of Sub-Categories.',container)
fnend 
def library fnCmbCat(myline,mypos; addall,container,indexfile$*200, ___,if$*200)
		! creates a screen ace combobox for category records
		autoLibrary
		if addall<>1 then addall=0
		if addall=0 then fen$='Ccat.h[cno]' else fen$='CcatALL.h[cno]'
		if indexfile$='' then if$='[Q]\PRmstr\catindx.h[cno]' else if$=indexfile$
		fnCmbCat=fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\jccat.h[cno]',1,11,12,25,if$,1+addall,1,'Select from the list of categories. To add a category record, go to the Category File.',container)
fnend
def library fnCmbEmp(myline,mypos; addall,container)
	! creates a screen ace combobox for employee records
	autoLibrary
	if addall<>1 then addall=0
	if addall=0 then fen$='CeMP.h[cno]' else fen$='CEmpALL.h[cno]'
	fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\Employee.h[cno]',1,8,9,30,'[Q]\PRmstr\EmployeeIdx-no.h[cno]',1+addall,0,'Select from the list of employees. To add an employee, go to the Employee File.',container)
fnend
def library fnCmbJob(myline,mypos; addall,container,indexfile$*200,___,if$*200)
	autoLibrary
	if addall<>1 then addall=0
	if addall=0 then fen$='CJob.h[cno]' else fen$='CJobALL.h[cno]'
	if indexfile$='' then if$='[Q]\PRmstr\jcindx.h[cno]' else if$=indexfile$
	fnCmbJob=fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\jcmstr.h[cno]',1,6,7,25,if$,1+addall,1,'Select from the list of jobs. To add a job, go to the Job Cost File.',container)
fnend
def library fnCmbCategory(myline,mypos; addall,container,indexfile$*200,___,if$*200)
	! creates a screen ace combobox for Category
	autoLibrary
	if addall<>1 then addall=0
	if addall=0 then fen$='Ccategory.h[cno]' else fen$='CcategoryALL.h[cno]'
	if indexfile$='' then if$='[Q]\PRmstr\categoryidx.h[cno]' else if$=indexfile$
	fnComboF(fen$,myline,mypos,43,'[Q]\PRmstr\category.h[cno]',1,5,6,30,if$,1+addall,0,'Select from the list of Category records. To add a Category record, take the Add option.',container)
fnend