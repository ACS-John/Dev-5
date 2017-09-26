00010 ! REPLACE S:\acsTM\TMMRGINV
00030   on error goto ERTN
00040   library 'S:\Core\Library': fnerror,fnxit
00050 ! let fntop(program$,cap$="Merge Invoices written to temp file "&env$('Q')&"\TMmstr\TMWk1.h"&env$('cno') ")
00070   dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4
00071   dim arta(2) ! ,ga(10)
00072 ! clmstr dims
00080   dim ca(10),sc(10)
00090   print newpage
00100   print fields "10,20,Cc 60,h,n": "T/M Merging Invoices..."
00108   open #h_tmwk1:=3: "Name="&env$('Q')&"\TMmstr\TMWk1.h"&env$('cno')&",NoShr",internal,input 
00109 F_TMWK1: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 55,30*pd 5.2,30*n 2,30*n 2
00112   dim cde$(30)*6,id$(30)*55,inv_amt(30),tmwk1_sc(30),ct(30)
00120   open #h_artrans:=12: "Name="&env$('Q')&"\TMmstr\ARTrans.h"&env$('cno')&",Shr",internal,outin,relative 
00130   open #h_tmtrans:=2: "Name="&env$('Q')&"\TMmstr\TMTRANS.H"&env$('cno')&",Shr",internal,outin,relative 
00140   open #h_clmstr:=1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00150   open #h_tmtraddr:=4: "Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&env$('cno')&",Shr",internal,outin,relative 
00202   do  ! r: main loop
00204 READ_TMWK1: ! 
00210     read #h_tmwk1,using F_TMWK1: k$,b(7),b(4),iv$,mat cde$,mat id$,mat inv_amt,mat ct,mat tmwk1_sc eof FINIS
00212 ! pr k$ : pause
00230     if rtrm$(k$)="" or rtrm$(k$)="0" then goto READ_TMWK1
00240     read #h_clmstr,using F_CLMSTR,key=k$: e$,mat sc,mat ca,ar1,mat arta nokey READ_TMWK1
00250 F_CLMSTR: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
00260     if b(7)=3 and rtrm$(iv$)="" then let iv$="WRITE OFF"
00270     let iv$=lpad$(rtrm$(iv$),12)
00280     let b(7)=-b(7)
00290 !   mat ga=(0) ! 8/18/88
00300     for j=1 to udim(mat id$)
00310       if inv_amt(j)=0 then goto NEXT_ONE
00320       let amt=amt+inv_amt(j)
00330       let b(3)=inv_amt(j)
00340       let b(5)=ct(j) ! inv_amt(j+10) ! Category i.e. 6,2
00350       let b(8)=tmwk1_sc(j) ! System Code
00360 !     let gl$=igl$(j)
00370 !     let ga(j)=inv_amt(j)
00380       if b(8)=0 then let b8=25 else let b8=b(8)
00390 L390: let lta=lrec(h_tmtrans)+1
00400       write #h_tmtrans,using 'form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30',rec=lta,reserve: k$," ",mat b,sc$,iv$,0,id$(j)(1:30) duprec L390
00420       rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=1,release: lta
00440       if b(5)=0 or ca(b(5))=0 then goto THAT_STEP ! added b(5)=0 on 2/1/2012
00442       if b8>25 then let b8=25 ! goto NEXT_ONE
00450       let p1=1+(b8-1)*6
00460       let p2=150+b8
00470 !     if ta2<0 then let ta2=0
00471 !     if ta1<0 then let ta1=0
00472       read #h_tmtraddr,using F_TMTRADDR,rec=ca(b(5)),reserve: ta1,ta2,fb1 norec NEXT_ONE
00480 F_TMTRADDR: form pos p1,2*pd 3,pos p2,n 1
00490       if ta2><0 then rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=ta2: lta else let ta1=lta
00500       if fb1<2 then let fb1=abs(b(7))
00510       if ta1=0 then let ta1=lta
00520       rewrite #h_tmtraddr,using F_TMTRADDR,rec=ca(b(5)),release: ta1,lta,fb1
00530       goto NEXT_ONE
00532 THAT_STEP: ! 
00540       let lta4=lrec(4)+1
00550       mat ta=(0)
00560       mat fb=(0)
00570       if b(5)>0 then let ca(b(5))=lta4 ! added b(5)>0 on 2/1/2012
00580       let ta(b8,1)=lta
00590       let ta(b8,2)=lta
00600       if b(7)=-2 then let fb(b8)=2
00610       if fb(b8)=2 then goto L630
00620       if b(7)=-1 then let fb(b8)=1
00630 L630: write #h_tmtraddr,using 'form pos 1,50*pd 3,25*n 1',rec=lta4,reserve: mat ta,mat fb duprec THAT_STEP
00650       rewrite #h_tmtraddr,using 'form pos 1,pd 3',rec=1,release: lta4
00668 NEXT_ONE: ! 
00670     next j
00680     if abs(b(7))=3 then goto L800 ! SKIP AR IF WRITE OFF
00690 L690: let lar=lrec(h_artrans)+1
00700     write #h_artrans,using 'form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3',rec=lar,reserve: k$,iv$,b(4),amt,amt,0,1,0,"CHARGE",0 duprec L690
00720     if arta(2)>0 then rewrite #h_artrans,using 'form pos 58,pd 3',rec=arta(2): lar
00730     let arta(2)=lar
00740     if arta(1)=0 then let arta(1)=lar
00750     rewrite #h_artrans,using 'form pos 58,pd 3',rec=1,release: lar
00770     let ar1=ar1+amt
00800 L800: if b(7)=-2 and b(5)>0 then let sc(b(5))=2 ! added b(5)>0 on 2/1/2012
00810     rewrite #h_clmstr,using 'form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3',key=k$: mat sc,mat ca,ar1,mat arta
00830     let amt=0
00840   loop  ! /r
00850 FINIS: ! r:
00852   close #h_clmstr: 
00860   close #h_tmtrans: 
00870   close #h_tmwk1: 
00880   close #h_tmtraddr: 
00890 ! close #h_armotran:
00900 XIT: let fnxit ! /r
75060 IGNORE: continue 
76020 ! <updateable region: ertn>
76040 ERTN: let fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>
