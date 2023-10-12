def library fnEmployeeName$*30(eno; ___,h,count,return$*30,which)
	if ~setup then fn_setup
	if ~setupCache then ! r: read arrays into cache
		setupCache=1
		dim cName$(0)*30
		dim cNo(0)
		open #h=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KfName=[Q]\PRmstr\EmployeeIdx-no.h[cno]',i,i,k
		do
			count+=1
			mat cName$(count)
			mat cNo(count)
			read #h,using 'form pos 1,n 8,c 30':cNo(count),cName$(count) eof EoH
		loop
		EoH: !
		close #h:
	end if ! /r
	which=srch(mat cNo,eno)
	if which>0 then
		return$=cName$(which)
	end if
	fnEmployeeName$=return$
fnend

Xit: fnXit
include: fn_setup
