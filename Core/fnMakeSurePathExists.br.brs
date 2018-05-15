!  ! r: testing zone
!  library program$: fnMakeSurepathExists
!  fnMakeSurepathExists('[Q]\INI\Core\PrtFlex')
!  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5')
!  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\')
!  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\acs.ini')
!  end
!  ! /r
! from fileio.brs - md/mkdir function fileio version is not a library 2/7/2017 except this version is a library and quote encapsulates the path$ it is making
! 8/24/2017 - more modifications to better work with UNC paths
def library fnMakeSurepathExists(fileName$*255; path$*255)
  library 'S:\Core\Library': fnSrepEnv$
	! if env$('acsDeveloper')<>'' and fileName$='[Q]\Data\' then
	! 	pr 'call to fnMakeSurepathExists("'&fileName$&'"; "'&path$&'")'
	! 	debug=1
	! 	pause
	! else
	! 	debug=0
	! end if
  fileName$=fnSrepEnv$(fileName$)
  path$    =fnSrepEnv$(path$)
  do while pos(fileName$,"\")
    path$=path$&fileName$(1:pos(fileName$,"\"))
    fileName$=fileName$(pos(fileName$,"\")+1:len(fileName$))
    doNotTryThisOne=0
    ! r: do not try entries like  [@:]\\  nor  [@:]\\server\  nor  [@:]\\server\resource\
    if path$(1:2)='\\' or path$(1:3)=':\\' or path$(1:4)='@:\\' or path$(1:5)='@::\\' then 
      if fn_backslashCount(path$)<=4 then doNotTryThisOne=1
    end if
    ! /r
    if ~exists(path$) and doNotTryThisOne=0 then 
			! if debug then pr 'about to MKDIR '&rtrm$(path$,'\') : pause
      execute 'mkdir "'&rtrm$(path$,'\')&'"'
    end if
  loop
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

