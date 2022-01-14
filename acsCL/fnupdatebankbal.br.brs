! Replace S:\acsCL\fnUpdateBankBal
! update bank balance function
def library fnupdatebankbal(bank_code,modification)
	autoLibrary
	open #bankmstr=fnH: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed
	key$=lpad$(str$(bank_code),2) ! cNVRT$('Pic(99)',BANK_CODE) 
	read #bankmstr,using 'form pos 45,PD 6.2',key=key$,reserve: bankbal nokey Xit
	! pr ' modification amount is +'&STR$(MODIFICATION)
	bankbal=bankbal+modification
	rewrite #bankmstr,using 'form pos 45,PD 6.2',release: bankbal
	Xit: !
	close #bankmstr:
fnend
