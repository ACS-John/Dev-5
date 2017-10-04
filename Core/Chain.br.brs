00010 ! replace S:\Core\chain.br
00020 ! ______________________________________________________________________
00030   def library fnchain(prg$*255; no_fnprg_setting,noLog)
00040     library 'S:\Core\Library': fngetpp,fnlog,fnprg,fnerror,fnmsgbox,fnxit
00050     on error goto ERTN
00060 ! 
00070     dim cap$*128,mss$(4)*128
00080     dim path$*256,prog$*256
00090 ! 
00100     fngetpp(prg$,path$,prog$,ext$)
00110     if ext$="" then ext$=".br"
00120 ! display menu : "","",""
00130 ! pr 'fnChain to '&PATH$&PROG$
00140     let prg$=path$&prog$
00150     if ~no_fnprg_setting then let fnprg(prg$,put=2)
00160 TEST: ! 
00170     let resp$=""
00180     if exists(path$&prog$&ext$)=0 then 
00190       let mss$(1)="The program ("&path$&prog$&ext$&") could not be found."
00200       let mss$(2)=""
00210       let mss$(3)="Retry will look again."
00220       let mss$(4)="Cancel will return you to the Menu."
00230       fnmsgbox(mat mss$,resp$,cap$,21)
00240     end if  ! exists(path$&prog$&ext$)=0
00250     if resp$="Cancel" then goto XIT else if resp$="Retry" then goto TEST
00260   if ~noLog then let fnlog('fnChain to '&prg$,1)
00262   chain prg$
00270 ! ______________________________________________________________________
00280 XIT: ! 
00290   chain 'S:\Core\Menu.br' ! let fnxit
00300 ! ______________________________________________________________________
00310 ! <Updateable Region: ERTN>
00320 ERTN: let fnerror(program$,err,line,act$,"xit")
00330   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00340   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00350   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00360 ERTN_EXEC_ACT: execute act$ : goto ERTN
00370 ! /region
00380 ! ______________________________________________________________________
00390 fnend 
00400 ! ______________________________________________________________________
