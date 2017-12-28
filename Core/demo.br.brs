00010 ! Replace S:\Core\Demo.br
00020 ! Demo - splash screen for disabled programs
00030   def library fndemo
00040     library 'S:\Core\Library': fnAcs,fnTos,fnLbl,fnCmdKey
00050     fnTos(sn$='Demo') !:
          lc=0
00060     fnLbl(lc+=1,1,"This program has been disabled in demos")
00070     fnCmdKey('Ok',5,1,1)
00080     fnAcs(sn$,0,mat resp$,ckey)
00090 XIT: fnend 
00100 ! ______________________________________________________________________
