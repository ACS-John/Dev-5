! formerly S:\acsPR\Department
! Department names for payroll
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
 
	dim mask(1),p$(1)*25,lbl$(1)*40
 
	add_count=0
	fn_add('Emp',8, '',0,30)       ! used
	fn_add('Dept',3)                ! used
	fn_add('gl no 1',3)
	fn_add('gl no 2',6)
	fn_add('gl no 3',3)
	fn_add('last review date',6)
	fn_add('next review date',6)
	fn_add('last increase date',6)
	fn_add('last payroll date',6)    !  used
	fn_add('state code',2)
	fn_add('workmans comp code',2)
	fn_add('union code',2)
	fn_add('Salary',10, 'PD',4.2)
	fn_add('Hourly Rate - Regular',10, 'PD',4.2)
	fn_add('Hourly Rate - Overtime',10, 'PD',4.2)
	fn_add('Misc 1 ',10, 'PD',4.2)
	fn_add('Misc 2 ',10, 'PD',4.2)
	fn_add('Misc 3 ',10, 'PD',4.2)
	fn_add('Misc 4 ',10, 'PD',4.2)
	fn_add('Misc 5 ',10, 'PD',4.2)
	fn_add('Misc 6 ',10, 'PD',4.2)
	fn_add('Misc 7 ',10, 'PD',4.2)
	fn_add('Misc 8 ',10, 'PD',4.2)
	fn_add('Misc 9 ',10, 'PD',4.2)
	fn_add('Misc 10',10, 'PD',4.2)
	fn_add('Misc 11',10, 'PD',4.2)
	fn_add('Misc 12',10, 'PD',4.2)
	fn_add('Misc 13',10, 'PD',4.2)
	fn_add('Misc 14',10, 'PD',4.2)
	fn_add('Misc 15',10, 'PD',4.2)
	fn_add('Misc 16',10, 'PD',4.2)
	fn_add('Misc 17',10, 'PD',4.2)
	fn_add('Misc 18',10, 'PD',4.2)
	fn_add('Misc 19',10, 'PD',4.2)
	fn_add('Misc 20',10, 'PD',4.2)
	open #1: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],use,RecL=149,kps=1/9,kln=8/3,Shr",internal,outIn,keyed
	fnHamster("department",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
	close #1:
	execute "Index [Q]\PRmstr\department.h[cno]"&' '&"[Q]\PRmstr\deptidx.h[cno] 1 11,Replace" ioerr Xit
Xit: fnXit
def fn_add(lbl$*40,fln; field_type$,storage_length,mask)
		add_count+=1
		mat lbl$(add_count)
		mat fln(add_count)
		mat p$(add_count)
		mat fltyp$(add_count)
		mat sln(add_count)
		mat mask(add_count)
		lbl$(add_count)=lbl$
		fln(add_count)=fln
		fltyp$(add_count)=field_type$
		sln(add_count)=storage_length
		mask(add_count)=mask
fnend
 
include: ertn
 
