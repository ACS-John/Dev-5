! Replace S:\acsUB\CmbAct.br
! creates a screen ace combobox for [Q]\UBmstr accounts
def library fncmbact(myline,mypos; addall,container,indexfile$*256)
	library 'S:\Core\Library': fncombof
	if addall<>1 then addall=0
	if addall=0 then 
		fen$="CAct"
	else 
		fen$="CActALL"
	end if 
	dim if$*256
	if indexfile$="" then 
		if$="[Q]\UBmstr\ubIndex.h[cno]" ! env$('cno')
	else 
		if$=indexfile$
	end if 
	fncmbact=fncombof(fen$,myline,mypos,43,"[Q]\UBmstr\Customer.h[cno]",1,10,41,30,if$,1+addall,1,"Select from the list of accounts, to add an account go to the Customer File.",container)
fnend 
