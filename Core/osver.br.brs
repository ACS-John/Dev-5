def library fnosver(&osver$; get_or_put)
	! Get_OR_Put   =0=1.Read only      =2.Build and Read
	autoLibrary
	dim osv_temp$*500,osv_file$*500,udf$*256
	if get_or_put=0 then get_or_put=1
	on error goto Ertn
	if get_or_put<>2 then get_or_put=1
	osv_file$=env$('temp')&"\osVer-"&wsid$
	on get_or_put gosub OSV_READ, OSV_BUILD_AND_READ
	gosub OSV_RIP_RELEASE
	goto Xit

	OSV_READ: ! r:
		open #osv_tfn:=fngethandle: "Name="&osv_file$,display,input ioerr OSV_READ_OPEN_IOERR
		do until osver$<>''
			linput #osv_tfn: osver$
		loop
		close #osv_tfn: ioerr ignore
	return ! /r
	OSV_READ_OPEN_IOERR: ! r:
		if err=4152 then gosub OSV_BUILD else goto ERTN
	return  ! /r 
	OSV_BUILD: ! r:
		execute "Free "&osv_file$&" -n" ioerr OSV_BUILD_NXT
	goto OSV_BUILD_NXT ! /r
	OSV_BUILD_NXT: ! r:
		open #osv_tfn:=fngethandle: "Name="&osv_file$&",RecL=80,replace",display,output 
		if rtrm$(env$("os"))="" then 
			osv_temp$="Microsoft Windows 95/98"
		else 
			osv_temp$="Microsoft Windows NT/2000/XP"
		end if 
		pr #osv_tfn: osv_temp$
		close #osv_tfn: 
	return ! /r
	OSV_BUILD_AND_READ: ! ! r:
		gosub OSV_BUILD
		gosub OSV_READ
	return ! /r
	OSV_RIP_RELEASE: ! ! r:
		tmp=pos(osver$,"[",1)
		if tmp>1 then osver$(tmp-1:len(osver$))=""
	return ! /r
Xit: fnend 
include: Ertn
