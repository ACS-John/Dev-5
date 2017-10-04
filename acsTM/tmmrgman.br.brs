00020 ! chained to from S:\acstm\tminpinv 
00030   on error goto ERTN
00040   library 'S:\Core\Library': fnopenprn,fncloseprn,fnconsole,fntop,fncno,fnxit,fnsearch
00050   fncno(cno)
00060   dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4,ivr(6),arta(2),ga(10),pgl$*12
00070   dim cde$(10)*6,gl$*12,des$*20
00080   pr newpage
00090   pr fields "10,20,c 60,h,n": "T/M MERGE INVOICES IN PROCESS"
00100   open #3: "Name="&env$('Q')&"\TMmstr\TMWk2"&wsid$&".H"&str$(cno)&",NoShr",internal,input 
00110   open #12: "Name="&env$('Q')&"\TMmstr\ARTrans.h"&str$(cno)&",Shr",internal,outin,relative 
00120   open #2: "Name="&env$('Q')&"\TMmstr\TMTRANS.H"&str$(cno)&",Shr",internal,outin,relative 
00130 L130: form pos 54,pd 3
00140   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00150   open #4: "Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&str$(cno)&",Shr",internal,outin,relative 
00160 ! open #h_armotran:=5: "Name="&env$('Q')&"\TMmstr\ARMoTran.h"&str$(cno)&",Shr",internal,output 
00170   open #6: "Name="&env$('Q')&"\TMmstr\Company.h"&str$(cno)&",Shr",internal,input 
00180   read #6,using L190: pgl$ 
00190 L190: form pos 190,c 12
00200   close #6: 
00210 L210: form pos 1,pd 3
00220 L220: read #3,using L230: k$,b(7),b(3),b(4),b(5),gl$,b(8),iv$ eof L780 
00230 L230: form pos 1,c 5,n 1,pd 4.2,n 6,n 2,c 12,n 2,c 12
00240   if rtrm$(k$)="0" or rtrm$(k$)="" then goto L220
00250   if b(7)=3 and rtrm$(iv$)="" then let iv$="WRITE OFF"
00260   let iv$=lpad$(rtrm$(iv$),12)
00270   b(7)=-b(7)
00280   if b(7)=-1 then let des$="PARTIAL BILLING"
00290   if b(7)=-2 then let des$="FINAL BILLING"
00300   if b(7)=-3 then let des$="WRITE-OFF"
00310   if b(8)=0 then b8=25 else b8=b(8)
00320   read #1,using L330,key=k$: e$,mat sc,mat ca,ar1,mat arta nokey L770 
00330 L330: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
00340 L340: let lta=lrec(2)+1
00350   write #2,using L360,rec=lta,reserve: k$," ",mat b,sc$,iv$,0 duprec L340 
00360 L360: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3
00370   rewrite #2,using L130,rec=1,release: lta
00380 L380: let lar=lrec(12)+1
00390   write #12,using L400,rec=lar,reserve: k$,iv$,b(4),b(3),b(3),0,1,0,des$,0 duprec L380
00400 L400: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00410   if arta(2)>0 then rewrite #12,using L450,rec=arta(2): lar
00420   arta(2)=lar
00430   if arta(1)=0 then arta(1)=lar
00440   rewrite #12,using L450,rec=1,release: lar
00450 L450: form pos 58,pd 3
00460   ar1=ar1+b(3)
00470   if ca(b(5))=0 then goto L570
00480   let p1=1+(b8-1)*6
00490   let p2=150+b8
00500   read #4,using L510,rec=ca(b(5)),reserve: ta1,ta2,fb1 norec L770 
00510 L510: form pos p1,2*pd 3,pos p2,n 1
00520   if ta2><0 then rewrite #2,using L130,rec=ta2: lta else let ta1=lta
00530   if fb1<2 then let fb1=abs(b(7))
00540   if ta1=0 then let ta1=lta
00550   rewrite #4,using L510,rec=ca(b(5)),release: ta1,lta,fb1
00560   goto L690 ! 6/24/87
00570 L570: let lta4=lrec(4)+1
00580   mat ta=(0)
00590   mat fb=(0)
00600   ca(b(5))=lta4
00610   let ta(b8,1)=lta
00620   let ta(b8,2)=lta
00630   if b(7)=-2 then let fb(b8)=2
00640   if fb(b8)=2 then goto L660
00650   if b(7)=-1 then let fb(b8)=1
00660 L660: write #4,using L670,rec=lta4,reserve: mat ta,mat fb duprec L570
00670 L670: form pos 1,50*pd 3,25*n 1
00680   rewrite #4,using L210,rec=1,release: lta4
00690 L690: let igl$(1)=gl$
00700   let ga(1)=b(3)
00710 ! write #h_armotran,using F_armotran: k$,iv$,b(4),b(3),b(3),0,1,0,des$,pgl$,mat igl$,mat ga
00720 ! F_armotran: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,11*c 12,10*pd 5.2
00730   if b(7)=-2 then let sc(b(5))=2
00740   rewrite #1,using L750,key=k$: mat sc,mat ca,ar1,mat arta
00750 L750: form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
00760   let ga(1)=b(3)
00770 L770: goto L220
00780 L780: close #1: 
00790   close #2: 
00800   close #3: 
00810   close #4: 
00820 XIT: let fnxit
00830 ERTN: if err=61 then pr fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L850
00840   goto L890
00850 L850: pr newpage
00860   if err=4148 then pr fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L880
00870   goto L890
00880 L880: pr fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00890 L890: pr fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00900   input fields "24,60,C 1,N": quitcode$
00910   if rtrm$(uprc$(quitcode$))="Q" then goto L950
00920   pr fields "23,3,C 78,N": ""
00930   pr fields "24,3,C 78,N": ""
00940   retry 
00950 L950: goto XIT
