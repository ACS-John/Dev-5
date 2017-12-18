00010 ! Replace S:\acsCL\unclbank
00020 ! Unclear All Entries by Bank
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnacs,fncmdset,fncombof,fnlbl,fntos,fntxt,fndate_mmddyy_to_ccyymmdd,fncmdkey
00050 ! ______________________________________________________________________
00060   dim cnam$*40,cap$*128,resp$(2)*40
00070 ! ______________________________________________________________________
00080   fntop(program$, cap$="Unclear All Entries by Bank")
00090   fncno(cno,cnam$)
00100   cancel=99 : right=1 : center=2 : on=1 : off=0 !:
        limit_to_list=1
00110 ! ______________________________________________________________________
00120   fntos(sn$='UnClBank1') !:
        mylen=15 : mypos=mylen+3
00130   fnlbl(1,1,"Bank:",mylen,right)
00135   fncombof('Bank',1,mypos,32,env$('Q')&"\CLmstr\BankMstr.h"&env$('cno'),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno'),limit_to_list)
00140   fnlbl(2,1,"Cleared Date:",mylen,right)
00150   fntxt(2,mypos,10,0,0,"3") !:
        resp$(2)=""
00170   fncmdset(2)
00180   fnacs(sn$,0,mat resp$,ckey)
00190   if ckey=5 then goto XIT
00200   bc1=val(resp$(1)(1:2))
00210   clrdate=val(resp$(2))
00220 ! ______________________________________________________________________
00230   open #trmstr=2: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00240 READ_2: ! 
00250 L250: read #trmstr,using "Form Pos 1,N 2,pos 72,n 6": bank_code,olddate eof XIT
00260   if fndate_mmddyy_to_ccyymmdd(olddate)<>clrdate then goto L250 ! clear dates must match
00270   if bc1=bank_code then !:
          rewrite #trmstr,using "Form Pos 72,N 6": 0
00280   goto READ_2
00290 ! ______________________________________________________________________
00300 XIT: fnxit
00310 ! ______________________________________________________________________
00320 ! <Updateable Region: ERTN>
00330 ERTN: fnerror(program$,err,line,act$,"xit")
00340   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00370 ERTN_EXEC_ACT: execute act$ : goto ERTN
00380 ! /region
00390 ! ______________________________________________________________________
