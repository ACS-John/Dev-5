!
	autoLibrary
	fnTop("S:\acsTM\tmunblwk",cap$="Unbilled Work In Process")
	on fkey 5 goto L450
	on error goto Ertn
!
	dim cxno$*5,cna$*30,en$*9,e$*25,l$(50)*9,d(8),l(50,6),t(6)
	dim cat$(30)*30,cap$*128
	namtab=66-int(len(rtrm$(env$('cnam')))/2)
 
	fnRead30Categories(mat cat$)
 
	open #1: "Name=S:\Core\Data\acsllc\Work2.H"&wsid$&",NoShr",internal,input ioerr ERTN
	open #2: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
	pr newpage
	pr f "10,10,Cc 60,n": "Printing Unbilled Aging by Partner..."
	pr f "12,30,Cc 20,B,5": "Cancel (F5)"
	fnopenprn(cp,58,220,process)
	gosub L520
L250: read #1,using L260: cxno$,cna$,en$,mat d eof L300 ioerr ERTN
L260: form pos 1,c 5,c 30,c 9,n 2,n 6,pd 4.2,5*pd 4.2
	gosub L750
	gosub L870
	goto L250
L300: close #1,free:
	close #2:
	open #3: "Name=S:\Core\Data\acsllc\EMmstr.H[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
	gosub L350
	goto L420
L350: pr #255,using L360: "_________   _________  _________  _________  _________  _________"
L360: form pos 36,c 65,skip 1
	pr #255,using L380: mat t
L380: form x 20,"Total",pos 34,n 11.2,x 1,5*n 11.2,skip 1
	pr #255,using L360: "=========   =========  =========  =========  =========  ========="
	return
!
L420: gosub L640
	gosub L980
	gosub L350
L450: close #1: ioerr L460
L460: close #2: ioerr L470
L470: close #6: ioerr L480
L480: fncloseprn
Xit: fnXit
!
L510: pr #255: newpage
L520: pr #255,using L530: date$,env$('cnam')
L530: form skip 2,pos 1,c 8,pos namtab,c 40,skip 1
	pr #255,using L550: time$,"Unbilled Work in Process - Aging Summary"
L550: form pos 1,c 8,pos 46,c 40,skip 1
	pr #255,using L570: dat
L570: form pos 62,pic(zz/zz/zz),skip 2
	pr #255,using L590: "Client     Client ","Accumulated","Current","30 - 60"," 60 - 90"," Over 90","Oldest    Category"
L590: form pos 3,c 18,pos 43,c 11,pos 61,c 7,pos 72,c 7,pos 82,c 8,pos 93,c 8,pos 103,c 18,skip 1
	pr #255,using L610: "Number      Name","Hours      Amount","Days","Days","Days","Item"
L610: form pos 3,c 16,pos 40,c 17,pos 74,c 4,pos 84,c 4,pos 95,c 4,pos 104,c 4,skip 2
	return
!
L640: !
	pr #255: newpage
	pr #255,using L660: env$('cnam'),"Unbilled Work in Process Summarized by Partner in Charge"
	L660: form skip 2,pos namtab,c 40,skip 1,pos 39,c 56,skip 1
	pr #255,using L570: dat
	pr #255,using L690: "Partner Name","Accumulated","Current","30 - 60"," 60 - 90"," Over 90"
	L690: form pos 3,c 18,pos 42,c 11,pos 61,c 7,pos 72,c 7,pos 81,c 8,pos 95,c 8,pos 103,c 18,skip 1
	pr #255,using L710: "Hours      Amount","Days","Days","Days"
	L710: form pos 40,c 17,pos 74,c 4,pos 84,c 4,pos 95,c 4,skip 2
	pr #255:
return
!
L750: d7=int(d(2)/10000)
	d8=int((d(2)-d7*10000)/100)
	d9=d(2)-d7*10000-d8*100
	if d(1)=0 then d(1)=1 ! to prevent error
	pr #255,using L790: cxno$,cna$,d(3),d(4),d(5),d(6),d(7),d(8),d8,"/",d9,"/",d7,cat$(d(1))(1:22) pageoflow L810
L790: form pos 2,c 5,pos 8,c 30,pos 38,n 7.2,pos 46,5*n 11.2,x 1,n 2,c 1,pic(##),c 1,n 2,x 1,c 22,skip 1
	goto L820
L810: gosub L510
L820: for j=1 to 6
		t(j)=t(j)+d(j+2)
	next j
	return
!
L870: for j=1 to 50
		if l$(j)=en$ then goto L930
		if rtrm$(l$(j))="" then goto L920
	next j
	goto L930
L920: l$(j)=en$
L930: for k=1 to 6
		l(j,k)=l(j,k)+d(k+2)
	next k
	return
!
L980: mat t=(0)
	for j=1 to 50
		if rtrm$(l$(j))="" then goto L1110
		read #3,using L1020,key=lpad$(rtrm$(l$(j)),9): e$ nokey L1040 ioerr ERTN
L1020: form pos 10,c 25
		goto L1050
L1040: e$="UNASSIGNED"
L1050: pr #255,using L1060: e$,l(j,1),l(j,2),l(j,3),l(j,4),l(j,5),l(j,6)
L1060: form pos 5,c 25,pos 38,n 7.2,x 1,5*n 11.2,skip 1
		for j1=1 to 6
			t(j1)=t(j1)+l(j,j1)
		next j1
	next j
L1110: !
return
include: Ertn
