14000 !  ! r: testing zone
14020 !  library program$: fnMakeSurepathExists
14040 !  fnMakeSurepathExists('[Q]\INI\Core\PrtFlex')
14060 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5')
14080 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\')
14100 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\acs.ini')
14120 !  end
14140 !  ! /r
24000 ! from fileio.brs - md/mkdir function fileio version is not a library 2/7/2017 except this version is a library and quote encapsulates the path$ it is making
24020 ! 8/24/2017 - more modifications to better work with UNC paths
26000 def library fnMakeSurepathExists(fileName$*255; path$*255)
26020   library 'S:\Core\Library': fnSrepEnv$
26040   fileName$=fnSrepEnv$(fileName$)
26060   path$    =fnSrepEnv$(path$)
26080   do while pos(fileName$,"\")
26100     path$=path$&fileName$(1:pos(fileName$,"\"))
26120     fileName$=fileName$(pos(fileName$,"\")+1:len(fileName$))
26140     doNotTryThisOne=0
26160     ! r: do not try entries like  [@:]\\  nor  [@:]\\server\  nor  [@:]\\server\resource\
26180     if path$(1:2)='\\' or path$(1:3)=':\\' or path$(1:4)='@:\\' or path$(1:5)='@::\\' then 
26200       if fn_backslashCount(path$)<=4 then doNotTryThisOne=1
26220     end if
26240     ! /r
26260     if ~exists(path$) and doNotTryThisOne=0 then 
26280       execute 'mkdir "'&path$&'"'
26300     end if
26320   loop
26340 fnend
32000 def fn_backslashCount(bscText$*128)
32020   bscCount=0
32040   do 
32060     bscPos=pos(bscText$,'\')
32080     if bscPos>0 then bscCount+=1
32100     bscText$(bscPos:bscPos)=''
32120   loop while bscPos>0
32140   fn_backslashCount=bscCount
32160 fnend

