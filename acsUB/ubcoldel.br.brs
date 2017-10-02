00010 ! Replace S:\acsUB\ubColDel
00020 ! -- Remove Cash Receipt Records
00030 ! ----------------------------------------------------------------------
00040 ! if the collections ever exceed 100,000 then sort wont work for the monthly receipts journal  ( one clue is the address is -2020202 instead of a real address
00050   library 'S:\Core\Library': fntop,fnxit, fnacs,fntop,fnlbl,fntxt,fnwait,fnopenprn,fncloseprn,fnwait,fntos,fnerror,fncno,fnxit,fnerror,fncmdset
00060   on error goto ERTN
00070 ! ----------------------------------------------------------------------
00080   dim alloc(10),o(2),cnam$*40,txt$*40,cap$*128
00090 ! ----------------------------------------------------------------------
00100   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00110   def fncd2(x)=(x-int(x*.0001)*10000)*10000+int(x*.0001)
00120   let fntop("S:\acsUB\ubColDel","Remove Cash Receipt Records")
00130   let fncno(cno) !:
        ! 
00140   open #6: "Name="&env$('Q')&"\UBmstr\Collect.h"&str$(cno),internal,input 
00150   open #7: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=72",internal,output 
00160 ! ----------------------------------------------------------------------
00170   gosub ASKDATE
00180 READ_COLLECT: ! 
00190   read #6,using L200: x$,m,n,mat o,adrnxt,rcpt$,mat alloc eof END1
00200 L200: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pd 3,c 9,10*pd 4.2
00210   let n2=fncd(n)
00220   if int(n2*.0001)<50 then !:
          let n2=n2+20000000 else !:
          let n2=n2+19000000
00230   if n2<ld1 then goto READ_COLLECT
00240   write #7,using L200: x$,m,n,mat o,adrnxt,rcpt$,mat alloc
00250   goto READ_COLLECT
00260 ! ______________________________________________________________________
00270 ASKDATE: ! 
00280   let sn$="ubcoldel" !:
        let fntos(sn$) !:
        let mylen=37 !:
        let mypos=mylen+2 !:
        let respc=0
00290   let fnlbl(1,1,"Lowest Date to Retained (mm\dd\ccyy):",mylen,1)
00300   let fntxt(1,mypos,10,10,0,"2") !:
        let resp$(respc+=1)=""
00310   let fncmdset(2)
00320   let fnacs(sn$,win,mat resp$,ckey)
00330   if ckey=5 then goto XIT
00340   let ld1=val(resp$(1)) conv ASKDATE !:
        let ld1=fncd2(ld1) !:
        cutoff$=str$(ld1)
00350   return 
00360 ! ______________________________________________________________________
00370 END1: close #7: 
00380   close #6,free: 
00390   execute "Rename "&env$('temp')&"\Work."&session$&' '&env$('Q')&"\UBmstr\Collect.h"&str$(cno)&" -n"
00400 XIT: let fnxit
00410 ! ______________________________________________________________________
00420 ! <Updateable Region: ERTN>
00430 ERTN: let fnerror(program$,err,line,act$,"xit")
00440   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00450   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
