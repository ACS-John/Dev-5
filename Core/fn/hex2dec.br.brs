def library fnhex2dec(input_hex$; ___,returnN,tmp$*1024)
	! converts a hexidecimal number to a decimal number
	tmp$=uprc$(ltrm$(trim$(input_hex$),"0")) 
	returnN=0 : l=len(tmp$)
	for i=l to 1 step -1
		if ord(tmp$(i:i))<65 then 
			returnN+=val(tmp$(i:i))*(16**(l-i)) 
		else 
			returnN+=(ord(tmp$(i:i))-55)*(16**(l-i))
		end if
	next i
	fnhex2dec=returnN
fnend 

