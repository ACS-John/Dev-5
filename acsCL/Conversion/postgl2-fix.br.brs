! REPLACE S:\acsCL\conversion\postgl2-fix
autoLibrary
	dim dat$*20,cnam$*40,vnam$*30,de$*30,tr(2),tbc(99,2),io1$(8),pde$*30,glwk$*20
	io1$(1)="10,44,N 2,U,N"
	io1$(2)="12,46,N 6,U,N"
	pr newpage
	pr f "10,15,C 60": "ENTER COMPANY NUMBER TO FIX:"
	pr f "12,10,C 60": "ENTER THE LAST DATE POSTED THROUGH:"
	pr f "14,10,C 34,R,N": "PRESS F1 TO CONTINUE OR F5 TO STOP"
L110: input fields mat io1$,attr "R": cno,dt1 conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L200 else ce=curfld
L140: ce=ce+1: if ce>udim(io1$) then ce=1
L150: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L140
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L110
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L150
L200: if cmdkey=5 then stop 


	open #1: "Name=[Q]\CLmstr\TRMSTR.H[cno],KFName=[Q]\CLmstr\TRIDX1.H[cno],Shr",internal,outIn,keyed 
	open #3: "Name=[Q]\CLmstr\TRALLOC.h[cno],Shr",internal,outIn,relative 
L250: read #1,using L260: ck$,pd,ca1,vn$,de$,pcde,scd,mat tr eof END1
L260: form pos 4,c 8,n 6,pd 10.2,pos 28,c 8,c 30,pos 71,n 1,x 6,n 1,2*pd 3
! pr CK$
	if scd=4 then goto L250
	if fncd(pd)<=fncd(dt1) then goto L250
	adr=tr(1)
L310: if adr=0 then goto L410
	read #3,using L330,rec=adr: bank_code,tcde,gl$,amt,iv$,ivd,nta,gde
L330: form pos 1,n 2,n 1,pos 12,c 12,pd 5.2,c 12,x 18,n 6,pd 3,pos 80,n 1
	if gde><3 then goto L400
	rewrite #3,using L360,rec=adr: 2
L360: form pos 80,n 1
! pr #255,USING 380: CK$,IVD,PD,AMT
	form pos 1,c 8,2*pic(zz/zz/zz),n 12.2
	if pcde=1 or pcde=3 then pcde=pcde-1
L400: adr=nta : goto L310
L410: rewrite #1,using L420: pcde
L420: form pos 71,n 1
	goto L250
END1: stop 
