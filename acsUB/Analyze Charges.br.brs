autoLibrary
on error goto Ertn
 
dim rate(18,20),usage(18,20),cde(20),d(12),t(18,2),a(4),message$*40
dim usagtot(18,20)
dim ratetot(18,20)
dim customer(18,20)
dim fa$(5),sa$(4),fb$(1),fc$(1),sb$(1)*38,fd$(1),z$(4)*11,svce$*11
dim io2$(38),code$(4)
 
	fndat(bdate)
	fnTop(program$)
MAIN: !
	fnTos
	mylen=20
	mypos=mylen+2
	fnLbl(1,1,"Billing Date:",mylen,1)
	fnTxt(1,mypos,8,8,0,"1")
	resp$(1)=str$(bdate)
	fnLbl(2,1,"Type of Service:",mylen,1)
	code$(1)="Water"
	code$(2)="Sewer"
	code$(3)="Electric"
	code$(4)="Gas"
	fnComboA("Service",2,mylen+3,mat code$,"",16)
	fnLbl(2,1,"Rate Code",mylen,1)
	fnTxt(3,mypos,3,3,0,'30')
	resp$(3)=""
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	bdate= val(resp$(1))
	if resp$(2)="Water" then          svce=1
	if resp$(2)="Sewer" then          svce=2
	if resp$(3)="Sewer" then          svce=3
	if resp$(4)="Sewer" then          svce=4
	fnopenprn
	open #1: "Name=[Q]\UBmstr\ubMaster.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	L510: !
	for k9=1 to 20
		pr f "20,13,C 11,N": "Rate Code:"
		pr f "22,11,C 22,N": "(Blank when complete)"
		pr f "24,8,C 13,B,4;24,23,c 9,B,5": "Complete (F4)","Exit (F5)"
		cde(k9)=0
		rinput fields "20,24,Nz 2,UT,N": cde(k9)
		if cmdkey=4 then goto L620
		if cmdkey=5 then goto Xit
		L620: !
		if cde(k9)>0 then goto L640
		if cde(k9)=0 then goto L930
		L640: !
		if cde(k9)<1 or cde(k9)>20 then goto L510
		close #105: ioerr ignore
		open #105: "SROW=2,SCOL=42,EROW=23,ECOL=77,BORDER=SR,CAPTION=<Print Analysis Report",display,outIn
		pr #105: newpage
		pr f "3,47,C 27,H,N": "  Beginning Usage   Rate"
		for j=1 to 18
			pr f str$(j+4)&",47,C 4,N": str$(j)&"."
			io2$(j*2-1)=str$(j+4)&",53,N 6,UT,N"
			io2$(j*2)=str$(j+4)&",65,N 9.5,UT,N"
		next j
		pr f "24,43,C 09,B,1": "Next (F1)"
		pr f "24,53,C 09,B,5": "Exit (F5)"
L760: input fields mat io2$: mat t conv CONV2
		if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
		if cmdkey>0 or curfld>36 then goto L850 else ce=curfld
L790: ce=ce+1: if ce>udim(io2$) then ce=1
L800: io2$(ce)=rtrm$(uprc$(io2$(ce))) : ce1=pos(io2$(ce),"U",1) :          if ce1=0 then goto L790
		ce2=ce1+1 : io2$(ce)(ce1:ce1)="UC" : goto L760
CONV2: if ce>0 then io2$(ce)(ce1:ce2)="U"
		ce=cnt+1
ERR2: pr f "24,78,C 1": bell : goto L800
L850: if cmdkey=5 then goto Xit
		close #105: ioerr L870
L870: for k8=1 to 18
			usage(k8,k9)=t(k8,1)
			rate(k8,k9)=t(k8,2)
		next k8
		mat t=(0)
	next k9
L930: message$="Printing: Please wait..." :         fnwait(message$,1)
	on fkey 5 goto DONE
	fnopenprn
L960: read #1,using L970: mat a,mat d,f eof L1250
L970: form pos 143,4*pd 2,pos 217,12*pd 5,pos 296,pd 4
	if f<>bdate then goto L960
	if a(svce)<>cde(1) then goto L960
	numbcust=numbcust+1
	s9=3
	if svce<3 then goto L1060
	s9=s9+4
	if svce=3 then goto L1060
	s9=s9+4
L1060: for k9=1 to 20
		if a(svce)=cde(k9) then goto L1100
	next k9
	goto L960
L1100: if d(s9)<usage(1,k9) then goto L1220
	for k7=1 to 17
		if d(s9)>=usage(k7,k9) and d(s9)<usage(k7+1,k9) then goto L1160
		if d(s9)>=usage(k7,k9) and usage(k7+1,k9)=0 then goto L1160
	next k7
	k7=18
L1160: ds9=d(s9)
	for j7=1 to k7-1
		usagtot(j7,k9)=usagtot(j7,k9)+(usage(j7+1,k9)-usage(j7,k9))
		ds9=ds9-(usage(j7+1,k9)-usage(j7,k9))
	next j7
	usagtot(k7,k9)=usagtot(k7,k9)+ds9
L1220: customer(k7,k9)=customer(k7,k9)+1
	goto L960
!
L1250: for k5=1 to 18
		for k3=1 to 20
			ratetot(k5,k3)=usagtot(k5,k3)*rate(k5,k3)
		next k3
	next k5
	for k2=1 to 20
		if cde(k2)=0 then goto L1540
		pr #255,using L1330: "Utility Billing Rare and Usage Analyzer"
L1330: form skip 3,pos 47,c 39,skip 2
		on svce goto L1360,L1370,L1380,L1390 none L1400
L1360: svce$="Water"       :     goto L1410
L1370: svce$="Sewer"       :     goto L1410
L1380: svce$="Electricity" :     goto L1410
L1390: svce$="Gas"         :     goto L1410
L1400: svce$=" "
L1410: pr #255,using L1420: "Service Analyzed - ",svce$
L1420: form pos 51,c 19,c 11,skip 2
		pr #255,using L1440: "Rate Code",cde(k2)
L1440: form pos 60,c 9,pos 73,pic(zz)
		pr #255,using L1460: "Beginning Usage","Rate","Dollars Generated","# of Customers"
L1460: form pos 25,c 15,pos 45,c 4,pos 65,c 17,x 3,c 15,skip 2
		for k1=1 to 18
			if usage(k1,k2)=0 and rate(k1,k2)=0 then goto L1510
			pr #255,using L1500: usage(k1,k2),rate(k1,k2),ratetot(k1,k2),customer(k1,k2)
L1500: form pos 25,pic(zzz,zzz,zz#),pos 43,pic(zz.#####),pos 65,pic(zzz,zzz,zzz.##),x 7,pic(zzzzzzzzzzzzz),skip 1
L1510: next k1
		pr #255: newpage
	next k2
L1540: pr #255,using L1550: "Number of Customers ",numbcust
L1550: form skip 1,c 20,pos 45,pic(zzz,zzz,zzz),skip 1
	pr #255,using L1570: "Number of Customers under Minumum Usage",mincust
L1570: form skip 1,c 40,pos 45,pic(zzz,zzz,zzz),skip 1
DONE: close #1: ioerr ignore
fncloseprn
Xit: fnXit
include: ertn
