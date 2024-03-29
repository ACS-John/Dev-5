!  ! r: testing zone
!  library program$: fnMakeSurePathExists
!  fnMakeSurePathExists('[Q]\INI\Core\PrtFlex')
!  fnMakeSurePathExists('C:\Program Files (x86)\ACS 5')
!  fnMakeSurePathExists('C:\Program Files (x86)\ACS 5\')
!  fnMakeSurePathExists('C:\Program Files (x86)\ACS 5\acs.ini')
!  end
!  ! /r
! from fileio.brs - md/mkdir function fileio version is not a library 2/7/2017 except this version is a library and quote encapsulates the path$ it is making
! 8/24/2017 - more modifications to better work with UNC paths
def library fnMakeSurePathExists(fileName$*255; path$*255,___,returnN,wasFilenamesUpperCase)
	autoLibrary
	! if env$('acsDeveloper')<>'' and fileName$='[Q]\Data\' then
	! 	pr 'call to fnMakeSurePathExists("'&fileName$&'"; "'&path$&'")'
	! 	debug=1
	! 	pause
	! else
	! 	debug=0
	! end if
	fileName$=fnSrepEnv$(fileName$)
	path$    =fnSrepEnv$(path$)
	! addAt$='' ! if pos(path$(1:2),'@')>0 then addAt$=env$('at') else addAt$=''
	do while pos(fileName$,"\")
		path$=path$&fileName$(1:pos(fileName$,"\"))
		fileName$=fileName$(pos(fileName$,"\")+1:len(fileName$))
		doNotTryThisOne=0
		! r: do not try entries like  [@:]\\  nor  [@:]\\server\  nor  [@:]\\server\resource\
		if path$(1:2)='\\' or path$(1:3)=':\\' or path$(1:4)='@:\\' or path$(1:5)='@::\\' then 
			if fn_backslashCount(path$)<=4 then doNotTryThisOne=1
		end if
		if path$(1:3)='@::' and path$(5:6)=':\' and len(path$)=6 then  ! 
			doNotTryThisOne=1
		end if
		! /r
		if ~exists(path$) and doNotTryThisOne=0 then 
			! if debug then pr 'about to MKDIR '&rtrm$(path$,'\') : pause
include: filenamesPushMixedCase
			execute 'mkdir "'&rtrm$(path$,'\')&'"' err MspeErr
			! execute 'mkdir "'&addAt$&rtrm$(path$,'\')&'"' err MspeErr
include: filenamesPopUpperCase
		end if
	loop
	goto MspeFinis
		
	MspeErr: !
	returnN=-err
		pr 'fnMakeSurePathExists got an error '&str$(err)&' and continued passing back its failure to the calling program to handle.'
		pr '  path$='&path$
		pr '  filename$='&filename$
		pause ! if env$('debug')='Yes' then pause
	goto MspeFinis
	MspeFinis: !
	fnMakeSurePathExists=returnN
fnend
def fn_backslashCount(bscText$*128)
	bscCount=0
	do 
		bscPos=pos(bscText$,'\')
		if bscPos>0 then bscCount+=1
		bscText$(bscPos:bscPos)=''
	loop while bscPos>0
	fn_backslashCount=bscCount
fnend

