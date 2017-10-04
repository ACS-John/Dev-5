00010 ! Replace S:\acsCL\Conversion\PayTrans-v1-to-v2
00020 ! converts the CL PayTrans file from version 0 or version 1 to Version 2
00030   def library fnpaytrans_v1_to_v2
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnmsgbox,fnwait,fnstatus,fnindex_it,fnCopy
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     fncno(cno,cnam$)
00100     cap$="Checkbook update PayTrans from v1 to v2"
00110 ! ______________________________________________________________________
00120     fnstatus('updating Unpaid Invoice file')
00160 ! let fnwait(101,cap$,message$="Converting: please wait...",0)
00170 ! 
00180     open #paytrans1=1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno),internal,outin,keyed 
00190     if version(paytrans1)=2 then !:
            let msgline$(4)="PayTrans is already version 2" !:
            let msgline$(5)="press enter to continue" : let msgline$(6)="" !:
            fnmsgbox(mat msgline$,response$,cap$,1) !:
            close #paytrans1: !:
            goto XIT
00200     close #paytrans1: 
00210 ! 
00220 ! change the record length of the file
00230     fnCopy(env$('Q')&"\CLmstr\PayTrans.h"&str$(cno),env$('Q')&"\CLmstr\PayTrans.h"&str$(cno),114)
00270     open #paytrans1=1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno),internal,outin,keyed 
00280     for j=1 to lrec(paytrans1)
00290       read #paytrans1,using 'Form Pos 96,N 1,N 6': gde,pdte eof L320
00300       rewrite #paytrans1,using 'Form Pos 90,N 1,N 6,N 10.2,N 8': gde,pdte,disamt=0,ddate=0
00310     next j
00320 L320: let version(paytrans1,2)
00330     close #paytrans1: 
00340     fnindex_it(env$('Q')&"\CLmstr\PayTrans.H"&str$(cno),env$('Q')&"\CLmstr\UnPdIdx1.H"&str$(cno),"1 20")
00350     fnindex_it(env$('Q')&"\CLmstr\PayTrans.h"&str$(cno),env$('Q')&"\CLmstr\UnPdIdx2.h"&str$(cno),"31/27/1 2/4/26")
00360     goto XIT
00370 ! ______________________________________________________________________
00380 ! <Updateable Region: ERTN>
00390 ERTN: let fnerror(program$,err,line,act$,"xit")
00400     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00410     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00420     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00430 ERTN_EXEC_ACT: execute act$ : goto ERTN
00440 ! /region
00450 ! ______________________________________________________________________
00460 XIT: ! 
00470   fnend 
00480 ! ______________________________________________________________________
