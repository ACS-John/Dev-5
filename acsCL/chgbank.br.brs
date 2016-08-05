00010 ! Replace R:\acsCL\chgbank
00020 ! Select Bank Account program - just updates the Company file
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fntop,fnxit,fncno,fnchain,fnerror,fntos,fnlbl,fncombof,fncmdkey,fnacs
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,resp$(1)*60
00080 ! ______________________________________________________________________
00090   let fncno(cno)
00100   let fntop(program$, cap$="Select Bank Account")
00110   let cancel=99 : let right=1 : let left=0 : let center=2 !:
        let limit_to_list=1
00120 ! ______________________________________________________________________
00130   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative: read #20,using 'Form POS 152,N 2',rec=1,release: wbc : close #20: 
00140 ASK1: ! 
00150   let fntos(sn$='ChgBank') !:
        let lc=0 : let mylen=20 : let mypos=mylen+2
00160   let fnlbl(lc+=1,1,"Working Bank:",mylen,right)
00170   let fncombof('bank',lc,mypos,33,"Q:\CLmstr\BankMstr.h"&str$(cno),1,2,3,30,"Q:\CLmstr\BankIdx1.h"&str$(cno),limit_to_list) !:
        let resp$(1)=str$(wbc)
00180   let fncmdkey('&Save',2,1,0) !:
        let fncmdkey('&Add',1,0,0,'This takes you to the Bank File') !:
        let fncmdkey('&Cancel',5,0,1)
00190   let fnacs(sn$,0,mat resp$,ckey)
00200   if ckey=5 or ckey=cancel then goto XIT else !:
          if ckey=1 then let fnchain("R:\acsCL\Bank") else !:
            if ckey=2 then let wbc=val(resp$(1)(1:2))
00210   open #20: "Name=Q:\CLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative: rewrite #20,using 'Form POS 152,N 2',rec=1: wbc : close #20: 
00220   goto XIT
00230 ! ______________________________________________________________________
00240 XIT: let fnxit
00250 ! ______________________________________________________________________
00260 ! <Updateable Region: ERTN>
00270 ERTN: let fnerror(cap$,err,line,act$,"xit")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
00330 ! ______________________________________________________________________
