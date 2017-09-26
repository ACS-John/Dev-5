00010 ! Replace S:\acsCL\fnUpdateBankBal
00020 ! update bank balance function
00030   def library fnupdatebankbal(bank_code,modification)
00040     library 'S:\Core\Library': fncno,fngethandle
00050     let fncno(cno)
00060     open #bankmstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00070     let key$=lpad$(str$(bank_code),2) ! Let CNVRT$('Pic(99)',BANK_CODE) !:
          read #bankmstr,using 'Form Pos 45,PD 6.2',key=key$,reserve: bankbal nokey XIT
00080 ! Print ' modification amount is +'&STR$(MODIFICATION)
00090     let bankbal=bankbal+modification
00100     rewrite #bankmstr,using 'Form Pos 45,PD 6.2',release: bankbal
00110 XIT: ! 
00120     close #bankmstr: 
00130   fnend 
