00010 ! Replace S:\acsUB\conversion\assign_sequence
00020 ! used to assign new sequence numbers if not enough room between numbers on standard conversion
00030   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnxit,fnerror,fncmdset,fntop
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim z$*10,text$*45,cap$*128
00070 ! ______________________________________________________________________
00080   fncno(cno) !:
        ! 
00090   fntop("S:\acsUB\UBZeroYt",cap$="Assign Sequence")
00100 ! ______________________________________________________________________
00110 SCREEN1: ! 
00120   sn$ = "sequence" !:
        fntos(sn$) !:
        mylen=25 : mypos=mylen+2
00130   text$='Increment by what number:' !:
        fnlbl(1,1,text$,mylen,1)
00140   fntxt(1,mypos,3,0,1,"30") !:
        resp$(1)=""
00150   fncmdset(2): fnacs(sn$,0,mat resp$,ck)
00160   if ck=5 then goto XIT
00170   incr=val(resp$(1))
00180   on fkey 5 goto DONE
00190   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00200 READ_CUSTOMER: ! !:
        read #1,using "Form POS 1743,n 7": oldseq eof DONE
00210   newseq=newseq+max(incr,10)
00220   rewrite #1,using "Form pos 1743,n 7": newseq
00230   goto READ_CUSTOMER
00240 DONE: close #1: 
00250   goto XIT
00260 ! _______________________________________________________________________
00270 ! <Updateable Region: ERTN>
00280 ERTN: fnerror(program$,err,line,act$,"xit")
00290   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00300   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00310   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00320 ERTN_EXEC_ACT: execute act$ : goto ERTN
00330 ! /region
00340 ! _______________________________________________________________________
00350 XIT: fnxit
00360 ! _______________________________________________________________________
