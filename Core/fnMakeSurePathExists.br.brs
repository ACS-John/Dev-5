14000 !  ! r: testing zone
14020 !  library program$: fnMakeSurepathExists
14040 !  fnMakeSurepathExists(env$('Q')&'\INI\Core\PrtFlex')
14060 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5')
14080 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\')
14100 !  fnMakeSurepathExists('C:\Program Files (x86)\ACS 5\acs.ini')
14120 !  end
14140 !  ! /r
24000 ! from fileio.brs - md/mkdir function fileio version is not a library 2/7/2017 except this version is a library and quote encapsulates the path$ it is making
24020 ! 8/24/2017 - more modifications to better work with UNC paths
25000 def library fnMakeSurepathExists(fileName$*255; path$*255)
25020   do while pos(fileName$,"\")
25040     path$=path$&fileName$(1:pos(fileName$,"\"))
25060     fileName$=fileName$(pos(fileName$,"\")+1:len(fileName$))
25080     ! pr 'if ~exists '&path$
25100     doNotTryThisOne=0
25120     if path$(1:2)='\\' and path$(1:4)='@:\\' then
25140       posThirdSlash=pos(path$,'\',3)
25160       posLastSlash=pos(path$,'\',-1)
25180       if posLastSlash<=posThirdSlash then doNotTryThisOne=1 ! do not try entries like \\server\, but do try \\server\resource\ as \\server\ will fail the exists test but \\server\resource\ will pass (assuming it exists)
25200     end if
25220     if ~exists(path$) and path$<>'\\' and doNotTryThisOne=0 then 
25240       execute 'mkdir "'&path$&'"'
25260     end if
25280   loop
25300 fnend
