00010 ! Replace S:\Core\Hex2Dec.br
00020 ! converts a hexidecimal number to a decimal number
00030 !
00040   def library fnhex2dec(input_hex$,&output_dec)
00050 !
00060     library 'S:\Core\Library': fnerror
00070     on error goto Ertn
00080 !
00090     dim h2d_temp$*1024
00100 !
00110     h2d_temp$=uprc$(ltrm$(trim$(input_hex$),"0")) !:
          dec=0 : l=len(h2d_temp$)
00120     for i=l to 1 step -1
00130       if ord(h2d_temp$(i:i))<65 then !:
              dec+=val(h2d_temp$(i:i))*(16**(l-i)) else !:
              dec+=(ord(h2d_temp$(i:i))-55)*(16**(l-i))
00140     next i
00150     output_dec=dec
00160     goto XIT
00170 !
00180 ! <Updateable Region: ERTN>
00190 ERTN: fnerror(program$,err,line,act$,"xit")
00200     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00210     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 !
00260 XIT: fnend 
00270 !
