00010 ! Replace Test\GLNumber
00020 ! test the functions that send GLNumber comboboxes to the screen and pull and format the response from it.
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fntos,fnlbl,fnacs,fncmdset,fnagl$,fnqgl,fnrgl$,fnpause
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! Dim GL$*12,C$*12,P$*30,S$*2,A(3),DCODE$*24,GLC$*24
00080   dim cap$*128,resp$(10)*50
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="General Ledger Number")
00110   right=1 : center=2
00120   fntos(sn$="GLNumber") !:
        lc=0 : mylen=30 : mypos=mylen+2
00130   fnlbl(lc+=1,1,"General Ledger Account Number:",mylen,right)
00140   fnqgl(lc,mypos) !:
        resp$(1)=fnrgl$('  0   700  0')
00150 ! pr RESP$(1) : fnPAUSE ! XXX
00160   fncmdset(2)
00170   fnacs(sn$,0,mat resp$,ckey)
00180   if ckey=5 then goto XIT
00185   let x$=fnagl$(resp$(1))
00190   pr 'This is your returned value"'&x$&'".'
00200   goto XIT
00210 ! ______________________________________________________________________
00220 XIT: fnxit
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: fnerror(program$,err,line,act$,"xit")
00260   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
