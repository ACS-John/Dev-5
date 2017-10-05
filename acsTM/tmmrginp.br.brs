00020   on error goto L820
00030   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1
00040   fntop(program$,cap$="Merge")
00050   fncno(cno,cnam$)
00060   pr newpage
00070   fnconsole(1)
00080   dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4,gl$*12,ivr(6),ca(10)
00090   dim e(4,30),sc(2),scc(10),des$*30
00100   pr newpage
00110   pr f "10,20,c 60,h,n": "T/M MERGE INPUT IN PROCESS"
00120   open #3: "Name="&env$('Q')&"\TMmstr\TMWK"&wsid$&".H"&str$(cno)&",NoShr",internal,input ioerr L820
00130   open #2: "Name="&env$('Q')&"\TMmstr\TMTRANS.H"&str$(cno)&",Shr",internal,outin,relative ioerr L820
00140 L140: form pos 54,pd 3
00150   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L820
00160   open #4: "Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&str$(cno)&",Shr",internal,outin,relative ioerr L820
00170   open #5: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L820
00180   open #6: "Name="&env$('Q')&"\TMmstr\SCMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\SCIndex.H"&str$(cno)&",Shr",internal,outin,keyed ioerr L820
00190 L190: form pos 1,pd 3
00200 L200: read #3,using L280: k$,e$,mat b,sc$,iv$,nta,des$ eof L750 ioerr L820
00210   if b(7)=0 then goto L200
00220   iv$=lpad$(rtrm$(iv$),12)
00230   if b(8)=0 then b8=25 else b8=b(8)
00240   if rtrm$(des$)="" then read #6,using L250,key=sc$: des$ nokey L260 ioerr L820
00250 L250: form pos 5,c 30
00260 L260: lta=lrec(2)+1
00270   write #2,using L280,rec=lta,reserve: k$,e$,mat b,sc$,iv$,0,des$ duprec L260
00280 L280: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
00290   rewrite #2,using L140,rec=1,release: lta
00300   if b(5)>10 then goto L580
00310   if val(k$)=0 then goto L580
00320   read #1,using L330,key=k$: mat scc,mat ca nokey L580 ioerr L820
00330 L330: form pos 220,10*n 1,10*pd 3
00340   if ca(b(5))=0 then goto L440
00350   p1=1+(b8-1)*6
00360   p2=150+b8
00370   read #4,using L380,rec=ca(b(5)): ta1,ta2,fb1 norec L580 ioerr L820
00380 L380: form pos p1,2*pd 3,pos p2,n 1
00390   if ta2>0 then rewrite #2,using L140,rec=ta2: lta
00400   if b(7)=-2 then let fb1=1
00410   if ta1=0 then ta1=lta
00420   rewrite #4,using L380,rec=ca(b(5)): ta1,lta,fb1
00430   if scc(b(5))=0 and b(7)>0 then goto L560 else goto L580
00440 L440: lta4=lrec(4)+1
00450   mat ta=(0)
00460   mat fb=(0)
00470   ca(b(5))=lta4
00480   ta(b8,1)=lta
00490   ta(b8,2)=lta
00500   if b(7)=-2 then let fb(b8)=2
00510   if fb(b8)=2 then goto L530
00520   if b(7)=-1 then let fb(b8)=1
00530 L530: write #4,using L540,rec=lta4,reserve: mat ta,mat fb duprec L440
00540 L540: form pos 1,50*pd 3,25*n 1
00550   rewrite #4,using L190,rec=1,release: lta4
00560 L560: scc(b(5))=1
00570   rewrite #1,using L330,key=k$: mat scc,mat ca
00580 L580: if val(e$)=0 then goto L660 ! EMPLOYEE
00590   read #5,using L600,key=e$: mat e nokey L660 ioerr L820
00600 L600: form pos 38,60*pd 4.2,60*pd 5.2
00610   e(1,b(5))=e(1,b(5))+b(1)
00620   e(2,b(5))=e(2,b(5))+b(1)
00630   e(3,b(5))=e(3,b(5))+b(3)
00640   e(4,b(5))=e(4,b(5))+b(3)
00650   rewrite #5,using L600,key=e$: mat e
00660 L660: if ltrm$(sc$)="0" then goto L200
00670   read #6,using L680,key=sc$: mat sc nokey L730 ioerr L820
00680 L680: form pos 35,pd 4.2,pd 5.2
00690   sc(1)=sc(1)+b(1)
00700   sc(2)=sc(2)+b(3)
00710   rewrite #6,using L680,key=sc$: mat sc nokey L730
00720   goto L200
00730 L730: pr #255: "SERVICE CODE ";sc$;" IS NOT ON FILE"
00740   goto L200
00750 L750: close #1: 
00760   close #2: 
00770   close #3: 
00780   close #4: 
00790   close #5: 
00800   close #6: 
00810 XIT: fnxit
00820 L820: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L840
00830   goto L880
00840 L840: pr newpage
00850   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L870
00860   goto L880
00870 L870: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00880 L880: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00890   input fields "24,60,C 1,N": quitcode$
00900   if rtrm$(uprc$(quitcode$))="Q" then goto L940
00910   pr f "23,3,C 78,N": ""
00920   pr f "24,3,C 78,N": ""
00930   retry 
00940 L940: goto XIT
