00010 ! Replace S:\acsGL\CmbBud.br
00020 ! creates a screen ace combobox for budget files
00030   def library fncmbbud(indexfile$*200)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fncno,fncombof,fnerror,fncomboa
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim ln$*200,options$(50)*200
00090 ! ______________________________________________________________________
00100     fncno(cno)
00110     execute "Dir "&env$('Q')&"\GLmstr\budget*.H"&str$(cno)&" >FlexWork.tmp" ! Ioerr 271
00120 L120: linput #13: ln$ eof L170
00130     let x=pos(ln$,"<DIR>",1)
00140     if x>0 and ln$(1:1)<>"." then goto L150 else goto L120
00150 L150: options$(j+=1)=ln$(46:len(trim$(ln$)))
00160     goto L120
00170 L170: close #13: 
00180     pause 
00190     if j<=0 then j=1
00200     mat options$(j)
00210     let fen$="CBud.h"&str$(cno)
00220     fncomboa(fen$,1,1,mat option$,"Select from the list of budget files. To add a new budget file, take the Add option.",20,container)
00230     goto XIT
00240 ! ______________________________________________________________________
00250 ! <Updateable Region: ERTN>
00260 ERTN: fnerror(program$,err,line,act$,"xit")
00270     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00280     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00290     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00300 ERTN_EXEC_ACT: execute act$ : goto ERTN
00310 ! /region
00320 ! ______________________________________________________________________
00330 XIT: fnend 
00340 ! ______________________________________________________________________
