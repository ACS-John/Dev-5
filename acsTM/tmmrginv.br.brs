! REPLACE S:\acsTM\TMMRGINV
on error goto Ertn
library 'S:\Core\Library': fnxit,fngethandle
! fntop(program$,cap$="Merge Invoices written to temp file S:\Core\Data\acsllc\tmpInvoice.h[cno]" ")
dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4
dim arta(2) ! ,ga(10)
! clmstr dims
dim ca(10),sc(10)
pr newpage
pr f "10,20,Cc 60,h,n": "T/M Merging Invoices..."
open #h_tmwk2:=3: "Name=S:\Core\Data\acsllc\tmpInvoice.h[cno],NoShr",internal,input
F_TMWK2: form pos 1,c 5,n 1,n 6,c 12,30*c 6,30*c 128,30*pd 5.2,30*n 2,30*n 2
dim cde$(30)*6
dim id$(30)*128
dim inv_amt(30)
dim tmwk_sc(30)
dim ct(30)
dim tmwk2_sc(30)
open #h_artrans:=fngethandle: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative
open #h_tmtrans:=fngethandle: "Name=S:\Core\Data\acsllc\TMTRANS.H[cno],Shr",internal,outIn,relative
open #h_clmstr:=1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed
open #h_tmtraddr:=4: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",internal,outIn,relative
do  ! r: main loop
	READ_TMWK: !
	read #h_tmwk2,using F_TMWK2: k$,b(7),b(4),iv$,mat cde$,mat id$,mat inv_amt,mat ct,mat tmwk2_sc eof FINIS
	! pr k$ : pause
	if rtrm$(k$)="" or rtrm$(k$)="0" then goto READ_TMWK
	read #h_clmstr,using F_CLMSTR,key=k$: e$,mat sc,mat ca,ar1,mat arta nokey READ_TMWK
	F_CLMSTR: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
	if b(7)=3 and rtrm$(iv$)="" then iv$="WRITE OFF"
	iv$=lpad$(rtrm$(iv$),12)
	b(7)=-b(7)
	!   mat ga=(0) ! 8/18/88
	for j=1 to udim(mat id$)
		if inv_amt(j)=0 then goto NEXT_ONE
		amt=amt+inv_amt(j)
		b(3)=inv_amt(j)
		b(5)=ct(j) ! inv_amt(j+10) ! Category i.e. 6,2
		b(8)=tmwk2_sc(j) ! System Code
		!     gl$=igl$(j)
		!     ga(j)=inv_amt(j)
		if b(8)=0 then b8=25 else b8=b(8)
		L390: !
		lta=lrec(h_tmtrans)+1
		write #h_tmtrans,using 'form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30',rec=lta,reserve: k$," ",mat b,sc$,iv$,0,id$(j)(1:30) duprec L390
		rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=1,release: lta
		if b(5)=0 or ca(b(5))=0 then goto THAT_STEP ! added b(5)=0 on 2/1/2012
		if b8>25 then b8=25 ! goto NEXT_ONE
		p1=1+(b8-1)*6
		p2=150+b8
		!     if ta2<0 then ta2=0
		!     if ta1<0 then ta1=0
		read #h_tmtraddr,using F_TMTRADDR,rec=ca(b(5)),reserve: ta1,ta2,fb1 noRec NEXT_ONE
		F_TMTRADDR: form pos p1,2*pd 3,pos p2,n 1
		if ta2><0 then rewrite #h_tmtrans,using 'form pos 54,pd 3',rec=ta2: lta else ta1=lta
		if fb1<2 then fb1=abs(b(7))
		if ta1=0 then ta1=lta
		rewrite #h_tmtraddr,using F_TMTRADDR,rec=ca(b(5)),release: ta1,lta,fb1
		goto NEXT_ONE
		THAT_STEP: !
		lta4=lrec(4)+1
		mat ta=(0)
		mat fb=(0)
		if b(5)>0 then ca(b(5))=lta4 ! added b(5)>0 on 2/1/2012
		ta(b8,1)=lta
		ta(b8,2)=lta
		if b(7)=-2 then fb(b8)=2
		if fb(b8)=2 then goto L630
		if b(7)=-1 then fb(b8)=1
		L630: !
		write #h_tmtraddr,using 'form pos 1,50*pd 3,25*n 1',rec=lta4,reserve: mat ta,mat fb duprec THAT_STEP
		rewrite #h_tmtraddr,using 'form pos 1,pd 3',rec=1,release: lta4
		NEXT_ONE: !
	next j
	if abs(b(7))<>3 then  ! SKIP AR IF WRITE OFF
		L690: !
		lar=lrec(h_artrans)+1
		write #h_artrans,using 'form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3',rec=lar,reserve: k$,iv$,b(4),amt,amt,0,1,0,"CHARGE",0 duprec L690
		! if arta(2)>0 then rewrite #h_artrans,using 'form pos 58,pd 3',rec=arta(2): lar
		! arta(2)=lar
		! if arta(1)=0 then arta(1)=lar
		! rewrite #h_artrans,using 'form pos 58,pd 3',rec=1,release: lar norec ignores
		ar1=ar1+amt
	end if
	L800: !
	if b(7)=-2 and b(5)>0 then sc(b(5))=2 ! added b(5)>0 on 2/1/2012
	rewrite #h_clmstr,using 'form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3',key=k$: mat sc,mat ca,ar1,mat arta
	amt=0
loop  ! /r

FINIS: ! r:
	close #h_clmstr:
	close #h_tmtrans:
	close #h_tmwk2:
	close #h_tmtraddr:
! close #h_armotran:
XIT: fnxit ! /r
include: ertn
