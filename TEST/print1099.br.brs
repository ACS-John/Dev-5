00010 ! Replace Test\Print1099
00020 ! pr 1099 Forms (From a File)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fncno,fnerror,fntop,fnxit,fnadd1099,fnprint1099
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cinfo$(6)*40,einfo$(6)*40,box(19)
00080 ! ______________________________________________________________________
00090   fncno(cno,cnam$)
00100   fntop("Test\Print1099",cap$="Print 1099s")
00101   for j=1 to 3
00110     cinfo$(1)='Company Name' !:
          cinfo$(2)='company Address (1)' !:
          cinfo$(3)='Company Address (2)' !:
          cinfo$(4)='Company CSZ' !:
          cinfo$(5)='Company Phone Number' !:
          cinfo$(6)='Company Federal ID'
00120     let einfo$(1)='Employee ID '&str$(j) !:
          let einfo$(2)='Employee Name' !:
          let einfo$(3)='Employee Address (1)' !:
          let einfo$(4)='Employee Address (2)' !:
          let einfo$(5)='Employee CSZ' !:
          let einfo$(6)='Employee Account Number'
00130     box(1)=123 : box(2)=234.3 : box(3)=123.1 : box(4)=123
00140     fnadd1099(mat cinfo$,mat einfo$,mat box)
00141   next j
00142   let lz1$='L'
00150   chain 'S:\Core\Print1099',lz1$ ! Let FNPRINT1099('L')
00160   pr ' you should some 1099'
00170   goto XIT
00180 ! ______________________________________________________________________
00190 XIT: stop 
00200 ! ______________________________________________________________________
00210 ! <Updateable Region: ERTN>
00220 ERTN: let fnerror(program$,err,line,act$,"xit")
00230   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00240   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00250   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00260 ERTN_EXEC_ACT: execute act$ : goto ERTN
00270 ! /region
00280 ! ______________________________________________________________________
