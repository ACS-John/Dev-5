00010 ! Replace S:\acsPR\newPrUsrDR
00020 ! pr User-Designed Reports
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fncombof,fntos,fncmdset,fnacs
00045   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim jcs$(40),cap$*128,rn(20),rn$(20)*74,resp$(5)*90,df$*256,if$*256
00070 ! ______________________________________________________________________
00080   fntop(program$,cap$="User Designed Report")
00090 MAIN_SCREEN: ! 
00100   fntos(sn$="user1") !:
        let mylen=25 : let mypos=mylen+2: let resp=0: left=1
00110   let df$="S:\acsPR\Jcreport.mst" : let if$="S:\acsPR\jcreport.idx" !:
        fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1) !:
        fncombof("CRjcreportALL",1,1,80,df$,1,2,3,74,if$,2)
00115   let resp$(1)=""
00150   fncmdset(2)
00160   fnacs(sn$,0,mat resp$,ck)
00170   if ck=5 then goto XIT
00180   let rno=val(resp$(1)(1:2))
00360 ! ______________________________________________________________________
00370 JCPRNT: chain "S:\acsPR\jcPrnt"&str$(rno)
00380 ! ______________________________________________________________________
00390 XIT: let fnxit
00400 ! ______________________________________________________________________
00410 ! <Updateable Region: ERTN>
00420 ERTN: let fnerror(program$,err,line,act$,"xit")
00430   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
