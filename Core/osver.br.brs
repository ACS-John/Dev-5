00050 def library fnosver(&osver$; get_or_put)
00060   ! Get_OR_Put   =0=1.Read only      =2.Build and Read
00080   library 'S:\Core\Library': fnerror,fngethandle
00100   dim osv_temp$*500,osv_file$*500,udf$*256
00120   if get_or_put=0 then get_or_put=1
00130   on error goto ERTN
00140   if get_or_put<>2 then get_or_put=1
00152   osv_file$=env$('temp')&"\osVer-"&wsid$
00160   on get_or_put gosub OSV_READ, OSV_BUILD_AND_READ
00170   gosub OSV_RIP_RELEASE
00172   goto XIT
00180   ! 
00190   OSV_READ: ! r:
00192     open #osv_tfn:=fngethandle: "Name="&osv_file$,display,input ioerr OSV_READ_OPEN_IOERR
00200     do until osver$<>''
00202       linput #osv_tfn: osver$
00212     loop
00220     close #osv_tfn: ioerr ignore
00232   return ! /r
00234   OSV_READ_OPEN_IOERR: ! r:
00236     if err=4152 then gosub OSV_BUILD else goto ERTN
00238   return  ! /r 
00250   OSV_BUILD: ! r:
00252     execute "Free "&osv_file$&" -n" ioerr OSV_BUILD_NXT
00258   goto OSV_BUILD_NXT ! /r
00270   OSV_BUILD_NXT: ! r:
00280     open #osv_tfn:=fngethandle: "Name="&osv_file$&",RecL=80,replace",display,output 
00290     if rtrm$(env$("os"))="" then 
00292       osv_temp$="Microsoft Windows 95/98"
00294     else 
00296       osv_temp$="Microsoft Windows NT/2000/XP"
00298     end if 
00300     pr #osv_tfn: osv_temp$
00310     close #osv_tfn: 
00320   return ! /r
00340   OSV_BUILD_AND_READ: ! ! r:
00342     gosub OSV_BUILD
00344     gosub OSV_READ
00348   return ! /r
00350   OSV_RIP_RELEASE: ! ! r:
00352     tmp=pos(osver$,"[",1)
00354     if tmp>1 then osver$(tmp-1:len(osver$))=""
00360   return ! /r
00370   ! 
00390   ! <Updateable Region: ERTN>
00400   ERTN: fnerror(program$,err,line,act$,"xit")
00410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00440   ERTN_EXEC_ACT: execute act$ : goto ERTN
00450   ! /region
00470 XIT: fnend 
