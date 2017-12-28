00010 ! REPLACE S:\acsCL\conversion\postgl2-fix
00020 ! 
00030   dim dat$*20,cnam$*40,vnam$*30,de$*30,tr(2),tbc(99,2),io1$(8),pde$*30,glwk$*20
00040   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00050   io1$(1)="10,44,N 2,U,N"
00060   io1$(2)="12,46,N 6,U,N"
00070   pr newpage
00080   pr f "10,15,C 60": "ENTER COMPANY NUMBER TO FIX:"
00090   pr f "12,10,C 60": "ENTER THE LAST DATE POSTED THROUGH:"
00100   pr f "14,10,C 34,R,N": "PRESS F1 TO CONTINUE OR F5 TO STOP"
00110 L110: input fields mat io1$,attr "R": cno,dt1 conv CONV1
00120   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00130   if cmdkey>0 then goto L200 else ce=curfld
00140 L140: ce=ce+1: if ce>udim(io1$) then ce=1
00150 L150: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L140
00160   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L110
00170 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
00180   ce=cnt+1
00190 ERR1: pr f "24,78,C 1": bell : goto L150
00200 L200: if cmdkey=5 then stop 
00210   pr #255: dt1
00220 ! 
00230   open #1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TRIDX1.H"&env$('cno')&",Shr",internal,outIn,keyed 
00240   open #3: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&env$('cno')&",Shr",internal,outIn,relative 
00250 L250: read #1,using L260: ck$,pd,ca1,vn$,de$,pcde,scd,mat tr eof END1
00260 L260: form pos 4,c 8,n 6,pd 10.2,pos 28,c 8,c 30,pos 71,n 1,x 6,n 1,2*pd 3
00270 ! pr CK$
00280   if scd=4 then goto L250
00290   if fncd(pd)<=fncd(dt1) then goto L250
00300   adr=tr(1)
00310 L310: if adr=0 then goto L410
00320   read #3,using L330,rec=adr: bank_code,tcde,gl$,amt,iv$,ivd,nta,gde
00330 L330: form pos 1,n 2,n 1,pos 12,c 12,pd 5.2,c 12,x 18,n 6,pd 3,pos 80,n 1
00340   if gde><3 then goto L400
00350   rewrite #3,using L360,rec=adr: 2
00360 L360: form pos 80,n 1
00370 ! pr #255,USING 380: CK$,IVD,PD,AMT
00380   form pos 1,c 8,2*pic(zz/zz/zz),n 12.2
00390   if pcde=1 or pcde=3 then pcde=pcde-1
00400 L400: adr=nta : goto L310
00410 L410: rewrite #1,using L420: pcde
00420 L420: form pos 71,n 1
00430   goto L250
00440 END1: stop 
