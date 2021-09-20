def library fnDeptName$*25(departmentCode; ___,return$*25)
	if ~setup_deptName then
		setup_deptName=1
		library 'S:\Core\Library':  fnAddOneN,fnAddOneC
		dim dN(0)
		dim d$(0)*25
		hDeptNames=fn_openFio('PR Department Names',mat d$,mat dN, 1)
		dim deptNo(0)
		mat deptNo(0)
		dim deptName$(0)*25
		mat deptName$(0)
		do
			read #hDeptNames,using form$(hDeptNames): mat d$,mat dN eof EoDeptNames
			fnAddOneN(mat deptNo,dN(dptnam_code))
			fnAddOneC(mat deptName$,d$(dptnam_name))
		loop
		EoDeptNames: !
		close #hDeptNames:
	end if
	deptWhich=srch(mat deptNo,departmentCode)
	if deptWhich<=0 then
		return$=''
	else 
		return$=deptName$(deptWhich)
	end if
	fnDeptName$=return$
fnend
include: fn_open