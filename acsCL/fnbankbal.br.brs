00010 ! Replace R:\acsCL\fnBankBal
00020 ! Check Book Transaction File Editor
00030   def library fnbankbal(bank_code)
00040     library 'R:\Core\Library': fncno,fngethandle
00050     let fncno(cno)
00060     open #bankmstr:=fngethandle: "Name=Q:\CLmstr\BankMstr.h"&str$(cno)&",KFName=Q:\CLmstr\BankIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00070     if bank_code=0 then 
00080       let bankbal=0
00090       for j=1 to lrec(bankmstr)
00100         read #bankmstr,using 'Form Pos 45,PD 6.2',rec=j: bal norec L120
00110         let bankbal+=bal
00120 L120: next j
00130     else 
00140       let key$=cnvrt$('Pic(zz)',bank_code) !:
            read #bankmstr,using 'Form Pos 45,PD 6.2',key=key$: bankbal
00150     end if 
00160     close #bankmstr: 
00170     let fnbankbal=bankbal
00180   fnend 
