! Replace S:\acsCL\fnBankBal
! Checkbook Transaction File Editor
def library fnbankbal(bankCode; ___,returnN,key$*2,bal)
	autoLibrary
	open #hBank=fnH: "Name=[Q]\CLmstr\Bankmstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",i,i,k
	if bankCode then
		key$=cnvrt$('Pic(zz)',bankCode)
		read #hBank,using 'Form Pos 45,PD 6.2',key=key$: returnN
	else
		do
			read #hBank,using 'Form Pos 45,PD 6.2': bal eof EohBank
			returnN+=bal
		loop
		EohBank: !
	end if
	close #hBank:
	fnbankbal=returnN
fnend
