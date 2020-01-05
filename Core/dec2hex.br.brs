00010 ! Replace S:\Core\Dec2Hex.br
00020 ! ______________________________________________________________________
00030   def library fndec2hex(input_dec,&output_hex$)
00040     library 'S:\Core\Library': fnerror
00050     on error goto Ertn
00060 ! ______________________________________________________________________
00070     dim d2h_temp$*1024
00080 ! ______________________________________________________________________
00090     d2h_temp$=""
00100     for i=8 to 0 step -1
00110       d=int(input_dec/(16**i))
00120       if d>9 then !:
              d2h_temp$(99:0)=chr$(int(d)+55) else !:
              d2h_temp$(99:0)=str$(int(d))
00130       input_dec=input_dec-d*(16**i)
00140     next i
00150     d2h_temp$=ltrm$(d2h_temp$)
00160     if d2h_temp$="" then output_hex$="0" else !:
            output_hex$=d2h_temp$
00170     goto XIT
00180 ! ______________________________________________________________________
00190 ! <Updateable Region: ERTN>
00200 ERTN: fnerror(program$,err,line,act$,"xit")
00210     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00220     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
00270 XIT: fnend 
00280 ! ______________________________________________________________________
