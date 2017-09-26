00010 ! Replace S:\Core\Demo.br
00020 ! Demo - splash screen for disabled programs
00030   def library fndemo
00040     library 'S:\Core\Library': fnacs,fntos,fnlbl,fncmdkey
00050     let fntos(sn$='Demo') !:
          let lc=0
00060     let fnlbl(lc+=1,1,"This program has been disabled in demos")
00070     let fncmdkey('Ok',5,1,1)
00080     let fnacs(sn$,0,mat resp$,ckey)
00090 XIT: fnend 
00100 ! ______________________________________________________________________
