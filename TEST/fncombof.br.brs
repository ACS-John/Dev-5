autoLibrary
 
fnTos
respc=0
fnFra(1,1,6,87,"Check"," ")
fnLbl(5,1,"Payee:",8,1,0,1)
fncombof("Paymstr",5,10,30,"[Q]\CLmstr\paymstr.h[cno]",1,8,9,30,"[Q]\CLmstr\Payidx1.h[cno]",0,pas, "Enter the payee number or simply enter the payee name if no vendor record exits",1)
resp$(respc+=1)=holdpayee$
! fnCmdSet
fnAcs2(mat resp$,ck)
