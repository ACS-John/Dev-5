00010 ! Replace S:\acsUB\UBRECRTR
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fnxit,fnerror,fnwait,fnacs,fntos,fnlbl,fntxt,fncmdset,fntop
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim p$*10,z$*10,o(2),adr(2),cap$*128,txt$*40,gb(10),tg(11),d(15)
00070 ! ______________________________________________________________________
00080   fncno(cno)
00090 ! 
00100   fntop("S:\acsUB\UbRecrTr",cap$="Recreate Transaction File")
00110   let sn$="ubRecrTr" !:
        fntos(sn$) !:
        let mylen=30 !:
        let mypos=mylen+2
00120   let txt$="Transaction Date:" !:
        fnlbl(1,1,txt$,mylen,1)
00130   fntxt(1,mypos,10,0,0,"3") !:
        let resp$(1)=date$("ccyymmdd")
00140   fnlbl(3,1,"Warning ! Do not continue",mylen,1)
00150   fnlbl(4,1,"without consulting ACS",mylen,1)
00160   fnlbl(4,15,"",mylen,1)
00170   fncmdset(2): let fnacs(sn$,0,mat resp$,ckey)
00180 L180: let x=pos(resp$(1),"/",1) !:
        if x>0 then let resp$(1)(x:x)="": goto L180
00190   let trandate=val(resp$(1))
00200   if ckey=5 then goto XIT
00210   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",NoShr",internal,outin,keyed 
00220   open #2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",RecL=102,Replace",internal,output 
00230 READ_CUSTOMER: ! 
00240 L240: read #1,using L250: z$,bal,mat d,mat gb eof READ_CUSTOMER_EOF
00250 L250: form pos 1,c 10,pos 292,pd 4.2,pos 217,15*pd 5,pos 388,10*pd 5.2
00260   if bal=0 then goto L240
00270   if bal<0 then !:
          let tcode=3 else !:
          let tcode=1
00280   if tcode=3 then let tamount=-bal else let tamount=bal
00290   for j=1 to 10
00300     let tg(j)=gb(j)
00310     if tcode=3 then let tg(j)=-tg(j)
00320   next j
00330   let tbal=bal
00340   let wr=d(1): let wu=d(3): let er=d(5): let eu=d(7): let gr=d(9): let gu=d(11)
00350   write #2,using L360: z$,trandate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
00360 L360: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00370   goto READ_CUSTOMER
00380 ! ______________________________________________________________________
00390 READ_CUSTOMER_EOF: ! 
00400   close #1: 
00410   close #2: 
00420   execute "Index "&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&" 1 19 Replace DupKeys -n"
00430   goto XIT
00440 ! ______________________________________________________________________
00450 XIT: let fnxit
00460 ! ______________________________________________________________________
00470 ! <Updateable Region: ERTN>
00480 ERTN: let fnerror(program$,err,line,act$,"NO")
00490   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00500   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
00540 ! ______________________________________________________________________
