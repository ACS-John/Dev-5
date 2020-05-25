! REPLACE S:\acsGL\Conversion\glAll-CNV
	dim tr(7),tr$*12,td$*30,cnam$*40
	autoLibrary
	pr newpage
	fnTop(program$)
L70: !
	pr f "10,24,C 32": "COMPANY NUMBER TO CONVERT:"
	pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
L100: input fields "10,57,N 5,UE,N",attr "R": cno conv L100
	if cmdkey=5 then stop
 
	open #1: "Name=[Q]\GLmstr\AcTrans.h[cno]",internal,input ioerr L260
	pr f "14,32,C 16,BR,N": "   IN PROCESS"
	open #2: "Name=X,size=0,RecL=72,REPLACE",internal,output
L160: read #1,using L190: mat tr,tr$,td$ eof L220
	if tr(1)+tr(2)+tr(3)=0 then goto L160
	actpd=int(tr(4)*.0001)
L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
	write #2,using L190: mat tr,tr$,td$,actpd
	goto L160
L220: close #1,free:
	close #2:
	execute "Rename X [Q]\GLmstr\AcTrans.h[cno]"
	execute "Index [Q]\GLmstr\AcTrans.h[cno]"&' '&"[Q]\GLmstr\AcTrIdx.h[cno] 1/71/17/13 12/2/2/4 REPLACE DupKeys"
L260: ! S:\acsGL\PRmstr.CNV
	dim pr1$*90,pr1(18),pr2(36)
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr L480
	open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,REPLACE",internal,output
L300: read #1,using L310: pr1$,mat pr1 eof END3
L310: form pos 1,c 90,18*pd 5.2,2*n 5
	for j=1 to 11: pr2(j)=pr1(j): next j
	pr2(13)=pr1(12)
	for j=13 to 18: pr2(j+18)=pr1(j): next j
	write #2,using L360: pr1$,mat pr2
L360: form pos 1,c 90,36*pd 5.2,2*n 5
	goto L300
END3: close #1:
	close #2:
	execute "COPY "&env$('Temp')&"\Work."&session$&", [Q]\GLmstr\PRmstr.h[cno]"
	execute "Index [Q]\GLmstr\PRmstr.h[cno],[Q]\GLmstr\PRIndex.h[cno],1,4,REPLACE,DupKeys"
	open #1: "Name=CNO.H"&wsid$,internal,outIn,relative
	rewrite #1,using L440,rec=1: cno
L440: form pos 1,n 2
	close #1:
	open #1: "Name=[Q]\GLmstr\ACPRCKS.h[cno],SIZE=0,RecL=110,REPLACE",internal,output
	close #1:
L480: close #1: ioerr L490
L490: open #1: "Name=[Q]\GLmstr\Company.h[cno]",internal,outIn,relative ioerr L560
	read #1,using L510,rec=1: gl1$,gl2$
L510: form pos 298,2*c 12
! write yes to bank rec and accumulate transactions
	rewrite #1,using L540,rec=1: gl2$,gl1$,"Y",1
L540: form pos 298,2*c 12,pos 406,g 1,pos 417,n 1
	close #1:
L560: ! S:\acsGL\FINSTMT.CNV
	dim cnam$*40,id$(6)*40,fil$(6),idx$(6)
	id$(1)=" 1 = BALANCE SHEET FILE": fil$(1)="ACGLFNSB": idx$(1)="agfsidx4"
	id$(2)=" 2 = INCOME STATEMENT FILE": fil$(2)="ACGLFNSI": idx$(2)="agfsidx3"
	id$(3)=" 3 = FUND STMT / CASH FLOW FILE": fil$(3)="ACGLFNSF": idx$(3)="agfsidx5"
	id$(4)=" 4 = SECONDARY BALANCE SHEET FILE": fil$(4)="ACGLFNSC": idx$(4)="agfsidx1"
	id$(5)=" 5 = SECONDARY INCOME STATEMENT FILE": fil$(5)="ACGLFNSJ": idx$(5)="agfsidx2"
	id$(6)=" 6 = SECONDARY FUND / CASH FLOW FILE": fil$(6)="ACGLFNSG": idx$(6)="agfsidx6"
	pr newpage
	for j=1 to 6
		execute "Copy [Q]\GLmstr\"&fil$(j)&".h[cno] "&env$('Temp')&"\Work."&session$&" -83" ioerr L950
		execute "COPY  "&env$('Temp')&"\Work."&session$&' '&"[Q]\GLmstr\"&fil$(j)&".h[cno]"
		if j=2 or j=5 then goto L690 else goto L950
L690: open #1: "Name=[Q]\GLmstr\"&fil$(j)&".h[cno],KFName=[Q]\GLmstr\"&idx$(j)&".h[cno]",internal,outIn,keyed
		end1=st1=st2=rno=rnp=0
L710: gosub FIND1
		restore #1,key>=lpad$(str$(st1),5): nokey END2
L730: read #1,using L830: rno,ic eof END2
		if rno<st2 then goto L780
		if end1=1 then goto END2
		rnp=0
		goto L710
L780: rewrite #1,using L790: rnp
L790: form pos 79,n 5
		goto L730
FIND1: st1=rno : st2=99999 : rnp=0
L820: read #1,using L830: rno,ic eof END1,conv L880
L830: form pos 1,g 5,pos 75,n 1
		if ic=0 then goto L820
		if ic=1 then rnp=rno
		if ic=2 then st2=rno : goto L930
		goto L820
L880: read #1,using L900: rno$ eof END1
		delete #1:
L900: form pos 1,c 5,pos 75,n 1
		goto L820
END1: end1=1
L930: return
END2: close #1:
L950: next j
	chain "S:\acsGL\Company"
