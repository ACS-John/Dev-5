00010 ! Replace S:\acsPR\newPrUsrDR
00020 ! pr User-Designed Reports
00030 !
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fncombof,fnTos,fnCmdSet,fnAcs
00045   on error goto Ertn
00050 !
00060   dim jcs$(40),cap$*128,rn(20),rn$(20)*74,resp$(5)*90,df$*256,if$*256
00070 !
00080   fntop(program$,cap$="User Designed Report")
00090 MAIN_SCREEN: ! 
00100   fnTos(sn$="user1") !:
        mylen=25 : mypos=mylen+2: resp=0: left=1
00110   df$="S:\acsPR\Jcreport.mst" : if$="S:\acsPR\jcreport.idx" !:
        fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1) !:
        fncombof("CRjcreportALL",1,1,80,df$,1,2,3,74,if$,2)
00115   resp$(1)=""
00150   fnCmdSet(2)
00160   fnAcs(sn$,0,mat resp$,ck)
00170   if ck=5 then goto XIT
00180   rno=val(resp$(1)(1:2))
00360 !
00370 JCPRNT: chain "S:\acsPR\jcPrnt"&str$(rno)
00380 !
00390 XIT: fnxit
00400 !
00410 ! <Updateable Region: ERTN>
00420 ERTN: fnerror(program$,err,line,act$,"xit")
00430   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 !
