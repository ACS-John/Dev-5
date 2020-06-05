autoLibrary

	dim d$*50,rf(6),bc(12),bp(12),bm(12),ta(2),tr(7),tr$*12,td$*30
	fnTop(program$)
	dim k$(3)*25,ss$*11,m(18)
	dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,procdat$*20
	dim dat$*20,ch$*20,prgl(21),co(7),acctmo$*6,vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,adr(2),revb(13)
	open #1: "Name=CNO.H"&wsid$,internal,outIn,relative 
	pr newpage
	pr f "8,5,C 70,HRB,N": "   WARNING THIS PROGRAM CAN ONLY BE RUN ONE TIME FOR EACH COMPANY #"
	pr f "10,9,C 60": "ENTER THE COMPANY # TO BE CONVERTED OR 0 TO STOP:"
L130: input fields "10,60,N 2,UE,N": cno conv L130
	if cno=0 then stop 
	pr newpage
	pr f "10,5,C 60": "CONVERSION FOR COMPANY #[cno] IN PROCESS"
! 
	rewrite #1,using L190,rec=1: cno
L190: form pos 1,n 2
	close #1: 
	open #2: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed 
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],REPLACE,RecL=338",internal,output 
L230: read #2,using L240: dno,ano,sno,d$,rf(1),rf(3),rf(5),bb,cb,mat bc,mat bp,mat bm,pbp,rf(2),rf(4),rf(6) eof L280
L240: form n 3,n 6,n 3,c 50,3*pd 3,39*pd 6.2,3*pd 3
	write #1,using L260: dno,ano,sno,d$,mat rf,bb,cb,mat bc,0,mat bp,0,mat bm,0,pbp,0,0,mat revb
L260: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
	goto L230
L280: close #1: : close #2,free: 
	open #2: "Name=[Q]\GLmstr\ACGLTRAN.h[cno]",internal,input,relative 
	open #1: "Name=[Q]\GLmstr\GLTrans.h[cno],REPLACE,RecL=73",internal,outIn,relative 
	write #1,using L320,rec=1: 0,0,0,0,0,0,0," "," ",lrec(2)
L320: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
	for j=1 to lrec(2)
		read #2,using L320,rec=j: mat tr,tr$,td$ noRec L360,conv L360
		write #1,using L320: mat tr,tr$,td$,0
L360: next j
	close #1: : close #2,free: 
	open #1: "Name=[Q]\GLmstr\ACGLPGMN.h[cno],REPLACE,RecL=58",internal,outIn,relative 
	for j=1 to 20
		write #1,using L410,rec=j: "","",0,0,0
L410: form pos 1,c 20,c 35,3*n 1
	next j
	close #1: 
	open #2: "Name=[Q]\GLmstr\PRmstr.h[cno]",internal,outIn,relative ioerr L530
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],REPLACE,RecL=190",internal,output 
	for j=1 to lrec(2)
		read #2,using L480,rec=j: eno,mat k$,ss$,mat m,mat ta eof L520,conv L510,noRec L510
L480: form pos 1,n 4,3*c 25,c 11,18*pd 5.2,2*n 5
		if eno=0 then goto L510
		write #1,using L480: eno,mat k$,ss$,mat m,0,0
L510: next j
L520: close #2,free: 
L530: if lrec(1)=0 then close #1,free: else close #1: 
	open #1: "Name=[Q]\GLmstr\Company.h[cno]",internal,outIn,relative 
	read #1,using L560,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$
L560: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30
	close #1,free: 
	open #1: "Name=[Q]\GLmstr\Company.h[cno],SIZE=0,RecL=512",internal,outIn,relative 
	open #2: "Name=[Q]\GLmstr\PRGLNUMB.h[cno]",internal,input ioerr L630
	read #2,using L610: mat prgl
L610: form pos 1,21*pd 4
	close #2,free: 
L630: open #2: "Name=[Q]\GLmstr\GLREC.h[cno]",internal,input ioerr L670
	read #2,using L650: reccode
L650: form pos 1,n 1
	close #2,free: 
L670: open #2: "Name=S:\acsGL\ACGLCODE.H[cno]",internal,input ioerr L710
	read #2,using L690: c1,c2
L690: form pos 1,n 1,n 2
	close #2,free: 
L710: open #2: "Name=[Q]\GLmstr\ACGLDATE.h[cno]",internal,input 
	read #2,using L730: procdat$,dat$,acctmo,acctmo$,ch$,lmu
L730: form pos 1,c 20,c 20,n 2,c 6,c 20,n 2
	close #2,free: 
	write #1,using L760,rec=1: mat a$,mat b$,c$,mat d,mat e$,lastact$,ucm,tb$,c2,c1,procdat$,dat$,acctmo,acctmo$,ch$,lmu,mat prgl,0,reccode,nap,ficarate,ficawage,feducrat,feducwag
L760: form pos 1,3*c 40,2*c 12,c 5,2*n 1,3*c 12,pd 7.2,c 30,n 2,n 1,2*c 20,n 2,c 6,c 20,n 2,21*pd 4,2*n 1,n 2,pd 5.3,pd 5.2,pd 5.3,pd 5.2
	close #1: 
	open #1: "Name=[Q]\GLmstr\GLBREC.h[cno],NoShr",internal,input ioerr L800
	close #1,free: 
L800: open #1: "Name=[Q]\GLmstr\GLBREC.h[cno],size=0,RecL=68,Shru",internal,outIn,relative 
	close #1: 
	gosub L890 ! CONVERT VENDOR FILES
	execute "Index [Q]\GLmstr\GLmstr.h[cno]"&' '&"[Q]\GLmstr\GLIndex.h[cno] 1 12 REPLACE DupKeys"
	execute "Index [Q]\GLmstr\GL1099.h[cno]"&' '&"[Q]\GLmstr\GL109IDX.h[cno] 1 8 REPLACE DupKeys"
	fnIndex("[Q]\GLmstr\GLBREC.h[cno]","[Q]\GLmstr\GLRecIdx.h[cno]","1 24")
	execute "Index [Q]\GLmstr\PRmstr.h[cno]"&' '&"[Q]\GLmstr\PRIndex.h[cno] 1 4 REPLACE DupKeys" ioerr L870
L870: execute "Index [Q]\GLmstr\GL1099.h[cno]"&' '&"[Q]\GLmstr\GL109IDX.h[cno] 1 8 REPLACE DupKeys"
	goto L1130
L890: open #3: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno]",internal,outIn,relative ioerr L920
	close #2,free: 
L920: open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno],SIZE=0,RecL=64,NoShr",internal,outIn,relative 
	write #2,using L970,rec=1: "",0,0,"","",1
	open #1: "Name=[Q]\GLmstr\GL1099N.H[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed ioerr L960
	close #1,free: 
L960: open #1: "Name=[Q]\GLmstr\GL1099N.H[cno],size=0,RecL=127,NoShr",internal,outIn,relative 
L970: form pos 1,c 8,n 6,pd 5.2,c 12,c 30,pd 3
L980: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
L990: read #3,using L980: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$ eof L1070
	mat adr=(0)
	if ytdp=0 then goto L1050
	rec2=lrec(2)+1
	write #2,using L970,rec=rec2: vn$,dat,ytdp,"","BEGINNING BALANCE",0
	mat adr=(rec2)
L1050: write #1,using L980: vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat adr
	goto L990
L1070: close #1: ioerr L1080
L1080: close #2: ioerr L1090
L1090: close #3: ioerr L1100
L1100: execute "Copy [Q]\GLmstr\GL1099N.H[cno]"&' '&"[Q]\GLmstr\GL1099.h[cno]"
	fnFree("[Q]\GLmstr\GL1099N.H[cno]")
	return 
L1130: ! REASS.CNV
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLTrans.h[cno]",internal,outIn,relative 
	pr newpage
	pr f "10,15,c 60,h,n": "REASSIGN GL ADDRESSES IN PROCESS"
L1180: form pos 333,2*pd 3
	lr2=lrec(2)
	rewrite #2,using L1300,rec=1: lr2
	for j=1 to lr2
		read #2,using L1230,rec=j: k$,nta noRec L1310
L1230: form pos 1,c 12,pos 71,pd 3
		read #1,using L1180,key=k$: mat ta nokey L1310
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #2,using L1300,rec=ta(2): j
		ta(2)=j
		rewrite #1,using L1180,key=k$: mat ta
		rewrite #2,using L1300,rec=j: 0
L1300: form pos 71,pd 3
L1310: next j
	close #1: 
	close #2: 
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr L1540
	open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno]",internal,outIn,relative ioerr L1540
	pr newpage
	pr f "10,15,c 60,h,n": "REASSIGN PR ADDRESSES IN PROCESS"
L1380: form pos 181,2*n 5
	lr2=lrec(2)
	rewrite #2,using L1500,rec=1: lr2
	for j=1 to lr2
		read #2,using L1430,rec=j: en$,nta noRec L1510,conv L1510
L1430: form pos 1,c 4,pos 68,pd 3
		read #1,using L1380,key=en$: mat ta nokey L1510,conv L1510
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #2,using L1500,rec=ta(2): j
		ta(2)=j
		rewrite #1,using L1380,key=en$: mat ta
		rewrite #2,using L1500,rec=j: 0
L1500: form pos 68,pd 3
L1510: next j
	close #1: 
	close #2: 
L1540: ! 
	open #1: "Name=[Q]\GLmstr\AcTrans.h[cno]",internal,input,relative ioerr L1690
	pr f "14,32,C 16,BR,N": "   IN PROCESS"
	open #2: "Name=X,RecL=72,REPLACE",internal,output 
	for j=1 to lrec(1)
		read #1,using L1620,rec=j: mat tr,tr$,td$ conv L1640
		if tr(1)+tr(2)+tr(3)=0 then goto L1640
		actpd=int(tr(4)*.0001)
L1620: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
		write #2,using L1620: mat tr,tr$,td$,actpd
L1640: next j
	close #1,free: 
	close #2: 
	execute "Rename X [Q]\GLmstr\AcTrans.h[cno]"
	execute "Index [Q]\GLmstr\AcTrans.h[cno]"&' '&"[Q]\GLmstr\AcTrIdx.h[cno] 1/71/17/13 12/2/2/4 REPLACE DupKeys"
L1690: ! S:\acsGL\PRmstr.CNV
	dim pr1$*90,pr1(18),pr2(36)
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno]",internal,outIn,keyed ioerr L1910
	open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=280,REPLACE",internal,output 
L1730: read #1,using L1740: pr1$,mat pr1 eof END1
L1740: form pos 1,c 90,18*pd 5.2,2*n 5
	for j=1 to 11: pr2(j)=pr1(j): next j
	pr2(13)=pr1(12)
	for j=13 to 18: pr2(j+18)=pr1(j): next j
	write #2,using L1790: pr1$,mat pr2
L1790: form pos 1,c 90,36*pd 5.2,2*n 5
	goto L1730
END1: close #1: 
	close #2: 
	execute "COPY "&env$('Temp')&"\Work."&session$&", [Q]\GLmstr\PRmstr.h[cno]"
	execute "Index [Q]\GLmstr\PRmstr.h[cno],[Q]\GLmstr\PRIndex.h[cno],1,4,REPLACE,DupKeys"
	open #1: "Name=CNO.H"&wsid$,internal,outIn,relative 
	rewrite #1,using L1870,rec=1: cno
L1870: form pos 1,n 2
	close #1: 
	open #1: "Name=[Q]\GLmstr\ACPRCKS.h[cno],RecL=110,REPLACE",internal,output 
	close #1: 
L1910: open #1: "Name=[Q]\GLmstr\Company.h[cno]",internal,outIn,relative ioerr L1960
	read #1,using L1930,rec=1: gl1$,gl2$
L1930: form pos 298,2*c 12
	rewrite #1,using L1930,rec=1: gl2$,gl1$
	close #1: 
L1960: end1=0 ! 
	dim id$(6)*40,fil$(6),idx$(6)
	id$(1)=" 1 = BALANCE SHEET FILE": fil$(1)="ACGLFNSB": idx$(1)="agfsidx4"
	id$(2)=" 2 = INCOME STATEMENT FILE": fil$(2)="ACGLFNSI": idx$(2)="agfsidx3"
	id$(3)=" 3 = FUND STMT / CASH FLOW FILE": fil$(3)="ACGLFNSF": idx$(3)="agfsidx5"
	id$(4)=" 4 = SECONDARY BALANCE SHEET FILE": fil$(4)="ACGLFNSC": idx$(4)="agfsidx1"
	id$(5)=" 5 = SECONDARY INCOME STATEMENT FILE": fil$(5)="ACGLFNSJ": idx$(5)="agfsidx2"
	id$(6)=" 6 = SECONDARY FUND / CASH FLOW FILE": fil$(6)="ACGLFNSG": idx$(6)="agfsidx6"
	for j=1 to 6
		execute "Copy [Q]\GLmstr\"&fil$(j)&".h[cno] "&env$('Temp')&"\Work."&session$&" -83" ioerr L2300
		execute "COPY  "&env$('Temp')&"\Work."&session$&' '&"[Q]\GLmstr\"&fil$(j)&".h[cno]"
		if j=2 or j=5 then goto L2080 else goto L2300
L2080: open #1: "Name=[Q]\GLmstr\"&fil$(j)&".h[cno],KFName=[Q]\GLmstr\"&idx$(j)&".h[cno]",internal,outIn,keyed 
		end1=st1=st2=rno=rnp=0
L2100: gosub FIND1
		restore #1,key>=lpad$(str$(st1),5): nokey END2
L2120: read #1,using L2220: rno,ic eof END2
		if rno<st2 then goto L2170
		if end1=1 then goto END2
		rnp=0
		goto L2100
L2170: rewrite #1,using L2180: rnp
L2180: form pos 79,n 5
		goto L2120
FIND1: st1=rno : st2=99999 : rnp=0
L2210: read #1,using L2220: rno,ic eof END21
L2220: form pos 1,g 5,pos 75,n 1
		if ic=0 then goto L2210
		if ic=1 then rnp=rno
		if ic=2 then st2=rno : goto L2280
		goto L2210
END21: end1=1
L2280: return 
END2: close #1: 
L2300: next j
	chain "S:\acsGL\Company"
