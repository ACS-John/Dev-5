autoLibrary
dim resp$(1)*80
fncursys$(cursys$='UB') ! sets the current system to Utility Billing
fnTop(program$)
 
fnTos
fncmbcno(1,1)
resp$(1)="1"
fnCmdKey("Ok",1,1,1)
ckey=fnAcs(mat resp$)
 
pr resp$(1)(43:47)
