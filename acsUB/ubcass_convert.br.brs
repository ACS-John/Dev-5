00010 ! Replace S:\acsUB\ubcass_convert
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim a$*111
00070 ! ______________________________________________________________________
00080   close #24: ioerr L90
00090 L90: close #22: ioerr L100
00100 L100: open #24: "Name=X,RecL=112,EOL=NONE,Replace",external,output 
00110   open #22: "Name=a:ubcass1.dat,RecL=111",external,input 
00120 L120: read #22,using "Form pos 1,C 111": a$ eof L160
00130   write #24,using "Form POS 1,C 111,C 1": a$,chr$(10)
00140   goto L120
00150 ! ______________________________________________________________________
00160 L160: close #24: 
00170   close #22: 
00180   execute "COPY x a:ubcass2.dat -n"
00190 ! ______________________________________________________________________
00200 XIT: stop 
00210 ! ______________________________________________________________________
00220 ! <Updateable Region: ERTN>
00230 ERTN: let fnerror(program$,err,line,act$,"xit")
00240   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00250   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00260   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00270 ERTN_EXEC_ACT: execute act$ : goto ERTN
00280 ! /region
00290 ! ______________________________________________________________________
