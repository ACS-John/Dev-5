def library fnosver(&osver$; get_or_put)
	! Get_OR_Put   =0=1.Read only      =2.Build and Read
	autoLibrary
	dim osv_temp$*500,osv_file$*500,udf$*256
	if get_or_put=0 then get_or_put=1
	on error goto Ertn
	if get_or_put<>2 then get_or_put=1
	osv_file$=env$('temp')&"\osVer-"&session$
	on get_or_put gosub OsvRead, OsvBuildAndRead
	gosub OsvRipRelease
	goto Xit

	OsvRead: ! r:
		open #osv_tfn=fnH: "Name="&osv_file$,display,input ioerr OsvReadOpenIoErr
		do until osver$<>''
			linput #osv_tfn: osver$
		loop
		close #osv_tfn: ioerr ignore
	return ! /r
	OsvReadOpenIoErr: ! r:
		if err=4152 then gosub OsvBuild else goto ERTN
	return  ! /r 
	OsvBuild: ! r:
		fnFree(osv_file$)
		open #osv_tfn=fnH: "Name="&osv_file$&",RecL=80,replace",d,o 
		if rtrm$(env$("os"))="" then 
			osv_temp$="Microsoft Windows 95/98"
		else 
			osv_temp$="Microsoft Windows NT/2000/XP"
		end if 
		pr #osv_tfn: osv_temp$
		close #osv_tfn: 
	return ! /r

	OsvBuildAndRead: ! ! r:
		gosub OsvBuild
		gosub OsvRead
	return ! /r

	OsvRipRelease: ! ! r:
		tmp=pos(osver$,"[",1)
		if tmp>1 then osver$(tmp-1:len(osver$))=""
	return ! /r

Xit: fnend 
include: ertn
