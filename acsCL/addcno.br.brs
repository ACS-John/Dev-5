00010 ! Replace R:\acsCL\AddCNo
00020 ! add a new company program for [cursys] (copies it from 99999) !:
        ! then it chains to Company Information
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fncno,fnerror,fnchain,fncursys$,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ml$(10)*80 ! message box lines
00080   let fncno(cno)
00090   execute 'copy '&fncursys$&'mstr\*.h99999 '&fncursys$&'mstr\*.h'&str$(cno)
00100   mat ml$(5) !:
        let ml$(1)='Would you like to import data from an old' !:
        let ml$(2)='ACS Accounts Payable system?' !:
        let ml$(3)='This is only chance.' !:
        let fnmsgbox(mat ml$,resp$,cap$,36)
00110   if resp$='No' then goto XIT
00120   if resp$='Yes' then let fnchain("R:\acsCL\Conversion\APmstr-Cnv")
00130 XIT: let fnchain('acs'&fncursys$&'\Company')
00140 ! ______________________________________________________________________
00150 ! <Updateable Region: ERTN>
00160 ERTN: let fnerror(cap$,err,line,act$,"xit")
00170   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00190   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00200 ERTN_EXEC_ACT: execute act$ : goto ERTN
00210 ! /region
00220 ! ______________________________________________________________________
