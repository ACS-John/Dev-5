00010 ! Replace S:\Core\OSVer.br
00020 ! ______________________________________________________________________
00030 FNOSVER: ! 
00040 ! ______________________________________________________________________
00050   def library fnosver(&osver$;get_or_put)
00060 ! Get_OR_Put   =0=1.Read only      =2.Build and Read
00070 ! ______________________________________________________________________
00080     library 'S:\Core\Library': fnerror,fngethandle
00090 ! ______________________________________________________________________
00100     dim osv_temp$*500,osv_file$*500,udf$*256
00110 ! ______________________________________________________________________
00120     if get_or_put=0 then let get_or_put=1
00130     on error goto OSV_ERTN
00140     if get_or_put<>2 then let get_or_put=1
00152     let osv_file$=env$('temp')&"\osVer-"&wsid$
00160     on get_or_put gosub OSV_READ, OSV_BUILD_AND_READ
00170     gosub OSV_RIP_RELEASE
00172     goto XIT
00180 ! ______________________________________________________________________
00190 OSV_READ: ! 
00192     open #osv_tfn:=fngethandle: "Name="&osv_file$,display,input ioerr OSV_READ_OPEN_IOERR
00200 OSV_LI: ! 
00202     linput #osv_tfn: osv_temp$
00210     if osv_temp$<>"" then let osver$=osv_temp$ else goto OSV_LI
00220     close #osv_tfn: ioerr OSV_LI_RETURN
00230 OSV_LI_RETURN: ! 
00232     return 
00234 OSV_READ_OPEN_IOERR: ! 
00236     if err=4152 then gosub OSV_BUILD else goto OSV_ERTN
00238     return  ! OSV_READ_OPEN_IOERR
00240 ! ______________________________________________________________________
00250 OSV_BUILD: ! 
00252     execute "Free "&osv_file$&" -n" ioerr OSV_BUILD_NXT
00258     goto OSV_BUILD_NXT
00260 ! ______________________________________________________________________
00270 OSV_BUILD_NXT: ! 
00280     open #osv_tfn:=fngethandle: "Name="&osv_file$&",RecL=80,replace",display,output 
00290     if rtrm$(env$("os"))="" then 
00292       let osv_temp$="Microsoft Windows 95/98"
00294     else 
00296       let osv_temp$="Microsoft Windows NT/2000/XP"
00298     end if 
00300     print #osv_tfn: osv_temp$
00310     close #osv_tfn: 
00320     return 
00330 ! ______________________________________________________________________
00340 OSV_BUILD_AND_READ: ! 
00342     gosub OSV_BUILD
00344     gosub OSV_READ
00348     return 
00350 OSV_RIP_RELEASE: ! 
00352     let tmp=pos(osver$,"[",1)
00354     if tmp>1 then let osver$(tmp-1:len(osver$))=""
00360     return 
00370 ! ______________________________________________________________________
00380 OSV_ERTN: ! 
00390 ! <Updateable Region: ERTN>
00400 ERTN: let fnerror(program$,err,line,act$,"xit")
00410     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00420     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00430     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00440 ERTN_EXEC_ACT: execute act$ : goto ERTN
00450 ! /region
00460 ! ______________________________________________________________________
00470 XIT: fnend 
00480 ! ______________________________________________________________________
