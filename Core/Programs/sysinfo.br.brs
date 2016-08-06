00010 ! Replace R:\Core\Programs\SysInfo
00020 ! ______________________________________________________________________
00030   library 'R:\Core\Library': fnerror,fnwin3b,fngetcd,fnxit,fntop
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim tmp$(2)*70,cap$*128,mcd$*60
00070 ! ______________________________________________________________________
00080   let fngetcd(mcd$)
00090   let fntop("R:\Core\Programs\SysInfo",cap$="System Information")
00100 ! 
00110   let win_width=max(40,len(mcd$)+15)
00120   let fnwin3b(win=101,cap$,08,win_width,0,41,5)
00130   let tmp$(2)=" WB Version: "&wbversion$ !:
        let tmp$(1)="2,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00140   let tmp$(2)="WB Platform: "&wbplatform$ !:
        let tmp$(1)="3,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00150   let tmp$(2)="  WB Serial: "&str$(serial) !:
        let tmp$(1)="4,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00160   let tmp$(2)="Workstation: "&wsid$ !:
        let tmp$(1)="5,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00170   let tmp$(2)="System Date: "&date$("mm/dd/ccyy") !:
        let tmp$(1)="6,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00180   let tmp$(2)="Current Dir: "&mcd$ !:
        let tmp$(1)="7,2,C "&str$(len(tmp$(2)))&",N" !:
        print #win,fields tmp$(1): tmp$(2)
00190   input #win,fields "8,1,C 1,AE,N": pause$
00200   goto XIT
00210 ! ______________________________________________________________________
00220 XIT: let fnxit("")
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: let fnerror(cap$,err,line,act$,"xit")
00260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
