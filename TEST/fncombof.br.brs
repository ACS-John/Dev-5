00010 ! 
10030   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fnTos,fnLbl,fnTxt,fncomboa,fnChk,fnCmdSet,fnAcs,fncombof,fnFra,fnmsgbox,fnButton,fnOpt,fnCmdKey,fnaddpayee,fnqgl,fnagl$,fnrgl$

14930     fnTos(sn$="ckprt-3")
14940     respc=0
14950     fnFra(1,1,6,87,"Check"," ")
15010     fnLbl(5,1,"Payee:",8,1,0,1)
15020     fncombof("Paymstr",5,10,30,env$('Q')&"\CLmstr\paymstr.h"&env$('cno'),1,8,9,30,env$('Q')&"\CLmstr\Payidx1.h"&env$('cno'),0,pas, "Enter the payee number or simply enter the payee name if no vendor record exits",1)
15030     resp$(respc+=1)=holdpayee$
15320     fnAcs(sn$,0,mat resp$,ck)