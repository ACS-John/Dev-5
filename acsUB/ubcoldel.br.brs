00010 ! Replace S:\acsUB\ubColDel
00020 ! -- Remove Cash Receipt Records
00030 ! ----------------------------------------------------------------------
00040 ! if the collections ever exceed 100,000 then sort wont work for the monthly receipts journal  ( one clue is the address is -2020202 instead of a real address
00050   library 'S:\Core\Library': fntop,fnxit, fnAcs,fntop,fnLbl,fnTxt,fnwait,fnopenprn,fncloseprn,fnwait,fnTos,fnerror,fncno,fnxit,fnerror,fnCmdSet
00060   on error goto ERTN
00070 ! ----------------------------------------------------------------------
00080   dim alloc(10),o(2),cnam$*40,txt$*40,cap$*128
00090 ! ----------------------------------------------------------------------
00100   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00110   def fncd2(x)=(x-int(x*.0001)*10000)*10000+int(x*.0001)
00120   fntop("S:\acsUB\ubColDel","Remove Cash Receipt Records")
00130   fncno(cno) !:
        ! 
00140   open #6: "Name="&env$('Q')&"\UBmstr\Collect.h"&env$('cno'),internal,input 
00150   open #7: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=72",internal,output 
00160 ! ----------------------------------------------------------------------
00170   gosub ASKDATE
00180 READ_COLLECT: ! 
00190   read #6,using L200: x$,m,n,mat o,adrnxt,rcpt$,mat alloc eof END1
00200 L200: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pd 3,c 9,10*pd 4.2
00210   n2=fncd(n)
00220   if int(n2*.0001)<50 then !:
          n2=n2+20000000 else !:
          n2=n2+19000000
00230   if n2<ld1 then goto READ_COLLECT
00240   write #7,using L200: x$,m,n,mat o,adrnxt,rcpt$,mat alloc
00250   goto READ_COLLECT
00260 ! ______________________________________________________________________
00270 ASKDATE: ! 
00280   sn$="ubcoldel" !:
        fnTos(sn$) !:
        mylen=37 !:
        mypos=mylen+2 !:
        respc=0
00290   fnLbl(1,1,"Lowest Date to Retained (mm\dd\ccyy):",mylen,1)
00300   fnTxt(1,mypos,10,10,0,"2") !:
        resp$(respc+=1)=""
00310   fnCmdSet(2)
00320   fnAcs(sn$,win,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   ld1=val(resp$(1)) conv ASKDATE !:
        ld1=fncd2(ld1) !:
        cutoff$=str$(ld1)
00350   return 
00360 ! ______________________________________________________________________
00370 END1: close #7: 
00380   close #6,free: 
00390   execute "Rename "&env$('temp')&"\Work."&session$&' '&env$('Q')&"\UBmstr\Collect.h"&env$('cno')&" -n"
00400 XIT: fnxit
00410 ! ______________________________________________________________________
00420 ! <Updateable Region: ERTN>
00430 ERTN: fnerror(program$,err,line,act$,"xit")
00440   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
