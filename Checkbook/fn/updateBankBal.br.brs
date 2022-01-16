def library fnUpdateBankBal(bankCode,modification; ___,hBank,key$,bal)
	autoLibrary
	open #hBank=fnH: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',internal,outIn,keyed
	key$=lpad$(str$(bankCode),2)
	read #hBank,using 'form pos 45,PD 6.2',key=key$,reserve: bal nokey UbbFinis
	bal+=modification
	rewrite #hBank,using 'form pos 45,PD 6.2',release: bal
	UbbFinis: !
	close #hBank:
fnend
