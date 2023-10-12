def library fnDepartmentName$*25(eno; ___,h,count,return$*25,which)
	if ~setup then fn_setup
	if ~setupDepartmentName then ! r: read arrays into cache
		setupDepartmentName=1
		dim deptName$(0)*25
		dim deptNo(0)
		open #h=fnH: 'name=[Q]\PRmstr\DeptName.h[cno]',i,i
		do
			count+=1
			mat deptName$(count)
			mat deptNo(count)
			read #h,using 'form pos 1,n 3,c 25':deptNo(count),deptName$(count) eof EoH
		loop
		EoH: !
		close #h:
	end if ! /r
	which=srch(mat deptNo,eno)
	if which>0 then
		return$=deptName$(which)
	end if
	fnDepartmentName$=return$
fnend

Xit: fnXit
include: fn_setup
