! Replace S:\acsPR\prRpt4
! Report File - Proof List
 
	autoLibrary
	on error goto Ertn
 
	dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20),cnam$*40
	dim message$*40,cap$*128
 
	fnTop("S:\acsPR\prRpt4",cap$="Report File - Proof List")
	fncno(cno,cnam$)
	open #1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\prrptidx.h[cno],Shr",internal,input,keyed
	fnopenprn
	nametab=66-len(rtrm$(cnam$))/2
 
	pr newpage
	message$="Printing:  please wait..."
	on fkey 5 goto L560
	fnwait(message$,1)
 
L210: read #1,using L220: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti eof L560
L220: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
	pr #255,using L240: date$,cnam$,time$,"Payroll Report File Proof List"
L240: form skip 2,pos 1,c 8,pos nametab,c 40,skip 1,pos 1,c 8,pos 51,c 30,skip 2
	pr #255,using L260: "Report Number",rn
L260: form pos 1,c 13,pos 20,pic(zz),skip 1
	pr #255,using L280: "Report Title",rt$
L280: form pos 1,c 12,pos 20,c 78,skip 1
	pr #255,using L300: "Column Headings",ch$(1)
L300: form pos 1,c 15,skip 2,c 132,skip 1
	pr #255,using L320: ch$(2)
L320: form pos 1,c 132,skip 2
	pr #255,using L340: "Item # for Prt Sel",ips
L340: form pos 1,c 18,pos 30,pic(zz#),skip 1
	pr #255,using L360: "Summarize Departments",sd
L360: form pos 1,c 26,pos 32,pic(#),skip 1
	pr #255,using L360: "Use Condensed Print",cp
	for j=1 to 100
		if psc(j)=0 then goto L490
		if j><48 then goto L450
		pr #255: newpage
		pr #255,using L430: "Print Selection Criteria",psc(j)
L430: form skip 3,pos 1,c 24,pos 30,pic(---------.###),skip 1
		goto L470
L450: pr #255,using L460: "PRINT SELECTION CRITERIA",psc(j)
L460: form pos 1,c 24,pos 30,pic(---------.###),skip 1
L470: next j
	pr #255: newpage
L490: for j=1 to 20
		if inp(j)+pp(j)+ti(j)=0 then goto L540
		pr #255,using L520: "Item Number to Print",inp(j), "Print Position",pp(j), "Total this Item",ti(j)
L520: form pos 1,c 20,pos 25,pic(zzz),pos 35,c 14,pos 49,pic(zzz),pos 65,c 15,pos 85,pic(#),skip 1
	next j
L540: pr #255: newpage
	goto L210
L560: close #1: ioerr L570
L570: fncloseprn
 
Xit: fnXit
 
include: Ertn
 
