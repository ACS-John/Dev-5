00010 ! REPLACE S:\acsCL\conversion\fnTrMstr_v0_to_v1
00020 ! converts the CL TRmstr file to version 1 !:
        ! meainging the amount changes from G 10.2 to PD 10.2
00030   def library fntrmstr_v0_to_v1
00040     library 'S:\Core\Library': fnerror,fnmsgbox,fnwait,fnStatus
00050     on error goto Ertn
00060 !
00070     dim message$*40,msgline$(6)*48,response$(5)*1
00080 !
00110 !
00120     fnStatus("Checkbook update Trans to v1: Updating Transaction file.")
00160 ! fnwait(message$="Converting: please wait...",0)
00170     open #trmstr=1: "Name=[Q]\CLmstr\TrMstr.h[cno]",internal,outIn,relative 
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
00260 !
00270 ! <Updateable Region: ERTN>
00280 ERTN: fnerror(program$,err,line,act$,"xit")
00290     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00300     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00310     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00320 ERTN_EXEC_ACT: execute act$ : goto ERTN
00330 ! /region
00340 !
00350 XIT: fnend 
00360 !
