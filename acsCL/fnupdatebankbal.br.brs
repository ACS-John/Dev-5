00010 ! Replace S:\acsCL\fnUpdateBankBal
00020 ! update bank balance function
00030   def library fnupdatebankbal(bank_code,modification)
00040     library 'S:\Core\Library': fncno,fngethandle
00050     fncno(cno)
00060     open #bankmstr:=fngethandle: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed 
00070     key$=lpad$(str$(bank_code),2) ! cNVRT$('Pic(99)',BANK_CODE) !:
          read #bankmstr,using 'Form Pos 45,PD 6.2',key=key$,reserve: bankbal nokey XIT
00080 ! pr ' modification amount is +'&STR$(MODIFICATION)
00090     bankbal=bankbal+modification
00100     rewrite #bankmstr,using 'Form Pos 45,PD 6.2',release: bankbal
00110 XIT: ! 
00120     close #bankmstr: 
00130   fnend 
