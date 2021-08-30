! Replace S:\acsPR\newprRpt4
 
	autoLibrary
	on error goto Ertn
 
	dim rt$*78,ch$(2)*132,psc(100),inp(20),pp(20),ti(20),cnam$*40
	dim message$*40,cap$*128
 
	fnTop(program$,cap$="User Designed Reports Proof List")
	fncno(cno,cnam$)
	open #1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\prrptidx.h[cno],Shr",i,i,k
	fnopenprn
 
	do
		read #1,using 'form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1': rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti eof L560
		pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
		pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
		pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
		pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
		pr #255: "\ql   "
! form skip 2,pos 1,c 8,pos nametab,c 40,skip 1,pos 1,c 8,pos 51,c 30,skip 2
		pr #255,using 'form pos 1,c 13,pos 20,pic(zz)': "Report Number",rn
		pr #255,using 'form pos 1,c 12,pos 20,c 78': "Report Title",rt$
		pr #255,using 'form pos 1,c 15,skip 2,c 132': "Column Headings",ch$(1)
		pr #255,using 'form pos 1,c 132': ch$(2)
		pr #255: ''
		pr #255,using 'form pos 1,c 25,pos 30,pic(zz#)': "Item Number for pr Sel",ips
		pr #255,using 'form pos 1,c 26,pos 32,pic(#)': "Summarize Departments",sd
! pr #255,Using 360: "Use Condensed Print",CP
		for j=1 to 100
			if psc(j)=0 then goto L490
			if j><48 then goto L450
			pr #255: newpage
			pr #255: ''
			pr #255: ''
			pr #255,using L460: "Print Selection Criteria",psc(j)
			goto L470
L450: !
			pr #255,using L460: "PRINT SELECTION CRITERIA",psc(j)
L460: form pos 1,c 24,pos 30,pic(---------.###)
L470: !
		next j
		pr #255: newpage
L490: !
		for j=1 to 20
			if inp(j)+pp(j)+ti(j)=0 then goto L540
			pr #255,using 'form pos 1,c 20,pos 25,pic(zzz),pos 35,c 14,pos 49,pic(zzz),pos 65,c 15,pos 85,pic(#)': "Item Number to Print",inp(j), "Print Position",pp(j), "Total this Item",ti(j)
		next j
L540: !
		pr #255: newpage
	loop
L560: !
	close #1: ioerr ignore
L570: !
	fncloseprn
Xit: fnXit

include: ertn
 
