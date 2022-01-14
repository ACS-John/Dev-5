! Replace S:\acsPR\jcPrtDET
! pr Job Cost Report
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,holdjn$*6,n$*40,a$(3)*30,b(4),totall(7)
	dim sc1$(6),sd1$(6),se1$(6)*50,prtj$(100)*6,cnam$*40,npj$(2)*6
	dim jtot$(100)*30,jobtot(100),tottot$(100)*30,tottot(100),totjob(7)
	dim dcode$(100)*3,desc$(100)*30,cdesc$(100)*30,cattot(100)
	dim cn$*11,holdcn$*11,cnt$*5,k$*25,l(13),ta(2),eno$*12,jno$*6,tr(9)
	dim io1$(6)*21,pd$*30,message$*40,cap$*128
 
	fnTop(program$,cap$="Job Cost Report")
	fncno(cno)
	fnconsole(1)
	dat1=date("mmddyy")
 
	prtjob$="N" : prtdet$="N" : sumcat$="N" : sumjob$="N" : _
	prtpag$="N" ! setup defaults to answers (also used by fnprocess=1)
 
	open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",i,i,k
	for j=1 to 100
		read #1,using 'form pos 1,C 3,C 30': dcode$(j),desc$(100) eof L250
		desc$(val(dcode$(j)))=desc$(100) conv L240
L240: next j
L250: close #1:
	desc$(100)="Unassigned"
 
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i,r  : _
	read #20,using 'form pos 1,C 40,pos 746,2*C 6',rec=1: cnam$,mat npj$ : _
	close #20:
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",i,i,k
	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,i,k
	open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",i,i,r
 
	pr newpage
	if fnprocess=1 then goto L640
	fnopenwin(win=102,7,15,16,65,cap$)
	pr #win,fields "4,2,Cr 42,N": "Date for Job Cost Detail Listing (mmddyy):"
	pr #win,fields "5,2,Cr 42,N": "Print all Jobs (Y/N):"
	pr #win,fields "6,2,Cr 42,N": "Print Details (Y/N):"
	pr #win,fields "7,2,Cr 42,N": "Summarize by Category (Y/N):"
	pr #win,fields "8,2,Cr 42,N": "Summarize by Job (Y/N):"
	pr #win,fields "9,2,Cr 42,N": "Start Jobs on a new page (Y/N):"
	io1$(1)="4,45,pic(zzzzzz),UT,N"
	for j=2 to 6
		io1$(j)=str$(j+3)&",45,Cu 1,UT,N"
	next j
	pr f "17,30,C 09,B,1": "Next (F1)"
	pr f "17,41,C 09,B,5": "Exit (F5)"
L480: rinput #win,fields mat io1$: dat1,prtjob$,prtdet$,sumcat$,sumjob$,prtpag$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L570 else ce=curfld
L510: ce=ce+1: if ce>udim(io1$) then ce=1
L520: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L510
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L480
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L520
L570: if cmdkey=5 then goto Xit
	if dat1<10100 or dat1>123199 then ce=1: goto ERR1
	if prtjob$<>"Y" and prtjob$<>"N" then ce=2: goto ERR1
	if prtdet$<>"Y" and prtdet$<>"N" then ce=3: goto ERR1
	if sumcat$<>"Y" and sumcat$<>"N" then ce=4: goto ERR1
	if sumjob$<>"Y" and sumjob$<>"N" then ce=5: goto ERR1
	if prtpag$<>"Y" and prtpag$<>"N" then ce=6: goto ERR1
L640: if prtdet$="N" and sumcat$="N" and sumjob$="N" then goto L650 else noread=1
L650: if prtjob$="Y" then goto L810
	for k=1 to 100
		pr newpage
		fnopenwin(win=102,10,20,15,59,cap$)
		if k=1 then goto L710
		if k>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Job Number entered was "&prtj$(k-1)
L710: pr #win,fields "4,2,C 20,N": "Job Number to print:"
		pr f "16,34,C 11,B,2": "Print (F2)"
L730: input #win,fields "4,23,C 6,UT,N": prtj$(k) conv L730
		if cmdkey=2 then goto L800
		if rtrm$(prtj$(k))="" or ltrm$(rtrm$(prtj$(k)))="0" then goto L730
		prtj$(k)=lpad$(rtrm$(prtj$(k)),6)
	next k
	goto L810
 
L800: k=k-1
L810: pr newpage
	fnwait(message$="Printing: please wait...",1)
	on fkey 5 goto DONE
	fnopenprn
L850: if prtjob$="Y" then goto L940
L860: j1=j1+1
	if j1<=k then goto L910
	eofc=2
	goto L1890
 
L910: read #1,using L920,key=prtj$(j1): jn$,n$,mat a$,mat b nokey L860
L920: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	goto L960
L940: read #1,using L920: jn$,n$,mat a$,mat b eof L1860
	if jn$>=npj$(1) and jn$<=npj$(2) then goto L940
L960: if hd=1 then goto L1010
	gosub HDR
	hd=1
	goto L1090
 
L1010: if prtpag$="N" then goto L1080
	pr #255: newpage
	hd=0
	gosub HDR
	hd=1
	goto L1090
 
L1080: gosub HDR
L1090: cnt$="    0"
L1100: read #2,using L1120,key>=jn$&cnt$: cn$,k$,mat l,mat ta nokey L1860
	detcd=0
L1120: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
	tr56=0
	tr8=0
	tr9=0
	tr89=0
	fstdet=0
	if prtjob$="Y" then goto L1220
	if cn$(1:6)><prtj$(j1) then goto L1670
	goto L1230
 
L1220: if cn$(1:6)><jn$ then goto L1670
L1230: if noread=0 then goto L1330
	if ta(1)=0 and ta(2)=0 then goto L1510
	nta=ta(1)
L1260: read #3,using L1280,rec=nta: eno$,jno$,mat tr,pd$,nta
	if tr(2)=0 then tr(2)=100
L1280: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	if prtdet$="N" then goto L1330
	gosub PRINTDETAILLINE
	goto L1360
 
L1330: if detcd=1 then goto L1360
	detcd=1
	gosub TOTALLINEWITHOUTDETAILS
L1360: if sumcat$="N" and subjob$="N" then goto L1390
	if noread=0 then goto L1530
	gosub ACCUMULATEFORCATEGORYSUMMARY
L1390: if nta=0 then goto L1420
	goto L1260
 
L1420: if prtdet$="N" then goto L1530
	if tr8+tr9+tr89=l(4)+l(6) and tr56=l(5) then goto L1460
	pr #255,using L1450: "Previous Balance",l(5)-tr56,l(4)-tr9,l(6)-(tr8+tr89)
L1450: form skip 1,pos 20,c 16,pos 36,n 9.2,2*n 14.2,skip 1
L1460: pr #255,using L1470: "________","____________","____________"
L1470: form pos 37,c 10,2*c 14,skip 1
	pr #255,using L1490: "Total "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L1490: form pos 10,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
	goto L1530
L1510: pr #255,using L1520: cn$(7:11)&"   "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L1520: form pos 1,c 28,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
L1530: totjob(1)=totjob(1)+l(5)
	totjob(2)=totjob(2)+l(4)
	totjob(3)=totjob(3)+l(6)
	totjob(4)=totjob(4)+l(1)
	totjob(5)=totjob(5)+l(3)
	totjob(6)=totjob(6)+(l(1)-l(4))
	totjob(7)=totjob(7)+(l(3)-l(6))
	if noread=0 then goto L1640
	form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14,skip 2
	if ta(1)=0 and ta(2)=0 then goto L1640
	gosub PRINTCATEGORYSUMMARY
L1640: cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
	goto L1100
 
L1670: if eofc=2 then goto L1690
	goto L1720
L1690: pr #255: newpage
	hd=0
	gosub HDR
L1720: pr #255,using L1790: "---------","-----------","-----------","------------","------------","------------","------------"
	if eofc=2 then goto L1760
	pr #255,using L1770: "Total by Job",mat totjob
	goto L1770
L1760: pr #255,using L1770: "Totals For All Jobs",mat totjob
L1770: form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2
	pr #255,using L1790: "=========","===========","===========","============","============","============","============" pageoflow NWPGE
L1790: form pos 35,c 13,2*c 14,pos 84,4*c 14,skip 2
	if eofc=2 then goto L1950
	mat totall=totall+totjob
	mat totjob=(0)
	if sumjob$="N" then goto L1850
	gosub PRINTSUMMARYBYJOB
L1850: if eofc=1 then goto L1890 else goto L850
L1860: eofc=1
	goto L1670
 
L1890: if sumjob$="N" or prtdet$="N" then goto L1920 ! END OF JOB ROUTINE
	if eofc=1 or eofc=2 then goto L1920
	gosub PRINTSUMMARYBYJOB
L1920: eofc=2
	mat totjob=totall
	goto L1670
L1950: if sumjob$="N" then goto L1970
	gosub PRINTSUMMARYOFALLJOBS
L1970: close #1:
	close #2:
	close #3:
DONE: !
	fncloseprn
	goto Xit
 
HDR: !
	if hd=1 then goto L2080
	pr #255,using L2070: cnam$,"Job Cost Detail Listing","As of",dat1
L2070: form pos 1,cc 122,skip 1,pos 55,c 23,skip 1,pos 60,c 6,pic(zz/zz/zz),skip 1
L2080: if eofc=0 then pr #255,using L2090: "Job Number "&jn$,n$ : _
		! pr SUB-HEADING
L2090: form skip 1,pos 30,c 17,pos 65,c 40,skip 2
	pr #255,using L2110: "Category","Category","Reference","Labor","Other","Estimated Cost","Over/Under"
L2110: form pos 1,c 8,pos 11,c 8,pos 21,c 9,pos 54,c 5,pos 68,c 5,pos 92,c 14,pos 124,c 10,skip 1
	pr #255,using L2130: "Number","Description","Number","Date","Hours","Cost","Cost","Sub-Category","Labor","Other","Labor","Other" pageoflow NWPGE
L2130: form pos 1,c 6,pos 9,c 11,pos 22,c 6,pos 30,c 4,pos 40,c 5,pos 55,c 4,pos 69,c 4,pos 76,c 12,pos 91,c 5,pos 105,c 5,pos 119,c 5,pos 133,c 5,skip 2
return
 
PRINTDETAILLINE: !
	if fstdet=1 then goto L2210
	fstdet=1
	pr #255,using L2200: tr(1),k$(1:20)
L2200: form pos 1,n 5,pos 9,c 20,skip 1
L2210: if tr(5)+tr(6)><0 then goto L2290
	if rtrm$(pd$)="" then goto L2250
	pr #255,using L2260: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&pd$(1:25) pageoflow NWPGE
	goto L2260
L2250: pr #255,using L2260: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
L2260: form skip 1,pos 15,c 12,pos 28,pic(zz/zz/zz),pos 36,n 9.2,2*n 14.2,pos 74,n 4,c 28,skip 1
	tr89=tr89+tr(8)+tr(9)
	goto L2360
L2290: if rtrm$(pd$)="" then goto L2320
	pr #255,using L2260: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&pd$(1:25) pageoflow NWPGE
	goto L2330
L2320: pr #255,using L2260: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
L2330: tr56=tr56+tr(5)+tr(6)
	tr9=tr9+tr(9)
	tr8=tr8+tr(8)
L2360: return
 
ACCUMULATEFORCATEGORYSUMMARY: !
	cattot(tr(2))=cattot(tr(2))+tr(8)+tr(9)
	cdesc$(tr(2))=desc$(tr(2))
	if lcat=0 and hcat=0 then goto L2420 else goto L2450
L2420: lcat=tr(2)
	hcat=lcat
	goto L2470
L2450: if tr(2)<lcat then lcat=tr(2)
	if tr(2)>hcat then hcat=tr(2)
L2470: return
 
PRINTCATEGORYSUMMARY: !
	if sumcat$="N" then goto L2590 else pr #255,using L2510: "************** Summary by Category *************"
L2510: form pos 90,cc 50,skip 1
L2520: form pos 90,c 50,skip 2
	for j=lcat to hcat
		if cattot(j)=0 then goto L2570
		pr #255,using L2560: cdesc$(j),cattot(j) pageoflow NWPGE
L2560: form pos 90,c 30,pos 124,n 14.2,skip 1
L2570: next j
	pr #255,using L2520: "************************************************"
L2590: mat jobtot=jobtot+cattot
	for j=lcat to hcat
		if rtrm$(cdesc$(j))="" then goto L2630
		jtot$(j)=cdesc$(j)
L2630: next j
	mat cattot=(0)
	lcat=0
	hcat=0
return
 
PRINTSUMMARYBYJOB: !
	if sumjob$="N" then goto L2790
	pr #255,using L2510: "***************** SUMMARY BY JOB ***************"
	form skip 1,pos 90,c 27,skip 2
	for j=1 to 100
		if jobtot(j)=0 then goto L2770
		pr #255,using L2760: jtot$(j),jobtot(j) pageoflow NWPGE
L2760: form pos 90,c 30,pos 124,n 14.2,skip 1
L2770: next j
	pr #255,using L2520: "************************************************"
L2790: mat tottot=tottot+jobtot
	for j=1 to 100
		if rtrm$(jtot$(j))="" then goto L2830
		tottot$(j)=jtot$(j)
L2830: next j
	mat jobtot=(0)
return
 
TOTALLINEWITHOUTDETAILS: !
	pr #255,using L2890: val(cn$(7:11)),k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L2890: form pos 1,n 5,pos 9,c 20,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
return
 
PRINTSUMMARYOFALLJOBS: !
	if sumcat$="N" and sumjob$="N" then goto L3020
	pr #255,using L2510: "*************** Summary of All Jobs ************"
	form skip 1,pos 90,c 27,skip 2
	for j=1 to 100
		if tottot(j)=0 then goto L3000
		pr #255,using L2990: tottot$(j),tottot(j) pageoflow NWPGE
L2990: form pos 90,c 30,pos 124,n 14.2,skip 1
L3000: next j
	pr #255,using L2520: "************************************************"
L3020: return
 
NWPGE: !
	pr #255: newpage
	hd=0
	gosub HDR
	hd=1
	continue
 
Xit: fnXit
 
include: ertn
 
