00010 ! REPLACE S:\acsCL\conversion\fnTrMstr_v0_to_v1
00020 ! converts the CL TRmstr file to version 1 !:
        ! meainging the amount changes from G 10.2 to PD 10.2
00030   def library fntrmstr_v0_to_v1
00040     library 'S:\Core\Library': fncno,fnerror,fnmsgbox,fnwait,fnstatus
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1
00080 ! ______________________________________________________________________
00090     fncno(cno,cnam$)
00100     cap$="Checkbook update Trans to v1"
00110 ! ______________________________________________________________________
00120     fnstatus("Updating Transaction file.")
00160 ! fnwait(101,cap$,message$="Converting: please wait...",0)
00170     open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno),internal,outin,relative 
00180     if version(trmstr)=1 then pr "trmstr is already version 1" !:
            pr "press enter to continue" !:
            input fields "1,1,C 1,N": pause$ !:
            goto XIT
00190     version(trmstr,1)
00200     for j=1 to lrec(trmstr)
00210       read #trmstr,using 'form pos 18,n 10.2': amt eof L240
00220       rewrite #trmstr,using 'form pos 18,pd 10.2': amt
00230     next j
00240 L240: close #trmstr: 
00250     goto XIT
00260 ! ______________________________________________________________________
00270 ! <Updateable Region: ERTN>
00280 ERTN: fnerror(program$,err,line,act$,"xit")
00290     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00300     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00310     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00320 ERTN_EXEC_ACT: execute act$ : goto ERTN
00330 ! /region
00340 ! ______________________________________________________________________
00350 XIT: fnend 
00360 ! ______________________________________________________________________
