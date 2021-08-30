! Replace S:\acsPR\newjcPrtDET
! pr Job Cost Report
autoLibrary
on error goto Ertn
 
dim jn$*6,holdjn$*6,n$*40,a$(3)*30,b(4),totall(7)
dim sc1$(6),sd1$(6),se1$(6)*50,prtj$(100)*6,cnam$*40,npj$(2)*6
dim jtot$(100)*30,jobtot(100),tottot$(100)*30,tottot(100),totjob(7)
dim dcode$(100)*3,desc$(100)*30,cdesc$(100)*30,cattot(100)
dim cn$*11,holdcn$*11,cnt$*5,k$*25,l(13),ta(2),eno$*12,jno$*6,tr(9)
dim io1$(6)*21,pd$*30,message$*40,cap$*128,resp$(6)*50
 
fnTop(program$,cap$="Job Cost Report")
dat1=date("mmddyy")
 
prtjob$=prtdet$=sumcat$=sumjob$=prtpag$="N" ! setup defaults to answers (also used by fnprocess=1)
 
open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",i,i,k
for j=1 to 100
	read #1,using 'Form POS 1,C 3,C 30': dcode$(j),desc$(100) eof L250
	desc$(val(dcode$(j)))=desc$(100) conv ignore
next j
L250: !
close #1:
desc$(100)="Unassigned"
 
open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i,r
read #20,using 'Form POS 1,C 40,POS 746,2*C 6',rec=1: cnam$,mat npj$
close #20:
open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",i,i,k
open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,i,k
open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",i,i,r
 
L330: !
fnTos
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,37,"",1)
	fnLbl(1,1,"Date of LIsting:",23,left)
	fnTxt(1,mypos,8,0,1,"1")
	resp$(resp+=1)=str$(dat1)
	fnChk(2,mypos,"Print all Jobs:",left)
	resp$(resp+=1)="False"
	fnChk(3,mypos,"Print Details:",left)
	resp$(resp+=1)="False"
	fnChk(4,mypos,"Summarize by Category:",left)
	resp$(resp+=1)="False"
	fnChk(5,mypos,"Summarize by Job:",left)
	resp$(resp+=1)="False"
	fnChk(6,mypos,"Start Jobs On a New Page:",left)
	resp$(resp+=1)="False"
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat1=val(resp$(1)) ! date
	if resp$(2)="True" then prtjob$="Y" else prtjob$="N"
	if resp$(3)="True" then prtdet$="Y" else prtdet$="N"
	if resp$(4)="True" then sumcat$="Y" else sumcat$="N"
	if resp$(5)="True" then sumjob$="Y" else sumjob$="N"
	if resp$(6)="True" then prtpag$="Y" else prtpag$="N"
	if prtdet$="N" and sumcat$="N" and sumjob$="N" then goto L520 else noread=1
L520: if prtjob$="Y" then goto L700
 
ASK_JOB: ! r:
	for k=1 to 100
		fnTos
		mylen=12 : mypos=mylen+3: resp=0: left=1
		fnLbl(1,1,"Job Number:",mylen,1)
		fncmbjob(1,mypos)
		resp$(respc+=1)=jn$
		if k=1 then goto L600
		if k>1 then let fnLbl(3,1,"Last Job Number entered was "&prtj$(k-1),50,1)
L600: fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto L690
		prtj$(k)=lpad$(trim$(resp$(1)(1:6)),6)
		if rtrm$(prtj$(k))="" or ltrm$(rtrm$(prtj$(k)))="0" then goto L330
		prtj$(k)=lpad$(rtrm$(prtj$(k)),6)
	next k
goto L700 ! /r
 
L690: !
	k=k-1
L700: !
	on fkey 5 goto DONE
	fnopenprn
L720: !
	if prtjob$="Y" then goto L810
L730: !
	j1=j1+1
	if j1<=k then goto L780
	eofc=2
goto L1760
!
L780: !
	read #1,using L790,key=prtj$(j1): jn$,n$,mat a$,mat b nokey L730
	L790: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
goto L830
L810: !
	read #1,using L790: jn$,n$,mat a$,mat b eof L1730
	if jn$>=npj$(1) and jn$<=npj$(2) then goto L810
L830: !
	if hd=1 then goto L880
	gosub HDR
	hd=1
goto L960
 
L880: !
	if prtpag$="N" then goto L950
	pr #255: newpage
	hd=0
	gosub HDR
	hd=1
goto L960
 
L950: !
	gosub HDR
L960: !
	cnt$="    0"
L970: !
	read #2,using L990,key>=jn$&cnt$: cn$,k$,mat l,mat ta nokey L1730
	detcd=0
	L990: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
	tr56=0
	tr8=0
	tr9=0
	tr89=0
	fstdet=0
	if prtjob$="Y" then goto L1090
	if cn$(1:6)><prtj$(j1) then goto L1540
goto L1100
 
L1090: if cn$(1:6)><jn$ then goto L1540
L1100: if noread=0 then goto L1200
	if ta(1)=0 and ta(2)=0 then nta=0: goto L1200
	nta=ta(1)
L1130: read #3,using L1150,rec=nta: eno$,jno$,mat tr,pd$,nta
	if tr(2)=0 then tr(2)=100
L1150: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
	if prtdet$="N" then goto L1200
	gosub PRINTDETAILLINE
	goto L1230
!
L1200: if detcd=1 then goto L1230
	detcd=1
	gosub TOTALLINEWITHOUTDETAILS
L1230: if sumcat$="N" and subjob$="N" then goto L1260
	if noread=0 then goto L1400
	gosub ACCUMULATEFORCATEGORYSUMMARY
L1260: if nta=0 then goto L1290
	goto L1130
!
L1290: if prtdet$="N" then goto L1400
	if tr8+tr9+tr89=l(4)+l(6) and tr56=l(5) then goto L1330
	pr #255,using L1320: "Previous Balance",l(5)-tr56,l(4)-tr9,l(6)-(tr8+tr89)
L1320: form skip 1,pos 20,c 16,pos 36,n 9.2,2*n 14.2,skip 1
L1330: pr #255,using L1340: "________","____________","____________"
L1340: form pos 37,c 10,2*c 14,skip 1
	pr #255,using L1360: "Total "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L1360: form pos 10,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
	goto L1400
	pr #255,using L1390: cn$(7:11)&"   "&k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L1390: form pos 1,c 28,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
L1400: totjob(1)=totjob(1)+l(5)
	totjob(2)=totjob(2)+l(4)
	totjob(3)=totjob(3)+l(6)
	totjob(4)=totjob(4)+l(1)
	totjob(5)=totjob(5)+l(3)
	totjob(6)=totjob(6)+(l(1)-l(4))
	totjob(7)=totjob(7)+(l(3)-l(6))
	if noread=0 then goto L1510
	form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14,skip 2
	if ta(1)=0 and ta(2)=0 then goto L1510
	gosub PRINTCATEGORYSUMMARY
L1510: cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
	goto L970
!
L1540: if eofc=2 then goto L1560
	goto L1590
L1560: pr #255: newpage
	hd=0
	gosub HDR
L1590: pr #255,using L1660: "---------","-----------","-----------","------------","------------","------------","------------"
	if eofc=2 then goto L1630
	pr #255,using L1640: "Total by Job",mat totjob
	goto L1640
L1630: pr #255,using L1640: "Totals For All Jobs",mat totjob
L1640: form pos 9,c 26,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2
	pr #255,using L1660: "=========","===========","===========","============","============","============","============" pageoflow NWPGE
L1660: form pos 35,c 13,2*c 14,pos 84,4*c 14,skip 2
	if eofc=2 then goto L1820
	mat totall=totall+totjob
	mat totjob=(0)
	if sumjob$="N" then goto L1720
	gosub PRINTSUMMARYBYJOB
L1720: if eofc=1 then goto L1760 else goto L720
L1730: eofc=1
	goto L1540
!
L1760: if sumjob$="N" or prtdet$="N" then goto L1790 ! END OF JOB ROUTINE
	if eofc=1 or eofc=2 then goto L1790
	gosub PRINTSUMMARYBYJOB
L1790: eofc=2
	mat totjob=totall
	goto L1540
L1820: if sumjob$="N" then goto L1840
	gosub PRINTSUMMARYOFALLJOBS
L1840: close #1:
	close #2:
	close #3:
DONE: !
	fncloseprn
	goto Xit
!
HDR: !
	if hd=1 then goto L1980
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zz/zz/zz)",dat1)&"}"
	pr #255: "\ql   "
L1980: if eofc=0 then pr #255,using L1990: "Job Number "&jn$,n$           ! pr SUB-HEADING
L1990: form skip 1,pos 30,c 17,pos 65,c 40,skip 2
	pr #255,using L2010: "Category","Category","Reference","Labor","Other","Estimated Cost","Over/Under"
L2010: form pos 1,c 8,pos 11,c 8,pos 21,c 9,pos 54,c 5,pos 68,c 5,pos 92,c 14,pos 124,c 10,skip 1
	pr #255,using L2030: "Number","Description","Number","Date","Hours","Cost","Cost","Sub-Category","Labor","Other","Labor","Other" pageoflow NWPGE
L2030: form pos 1,c 6,pos 9,c 11,pos 22,c 6,pos 30,c 4,pos 40,c 5,pos 55,c 4,pos 69,c 4,pos 76,c 12,pos 91,c 5,pos 105,c 5,pos 119,c 5,pos 133,c 5,skip 2
	return
!
PRINTDETAILLINE: !
	if fstdet=1 then goto L2110
	fstdet=1
	pr #255,using L2100: tr(1),k$(1:20)
L2100: form pos 1,n 5,pos 9,c 20,skip 1
L2110: if tr(5)+tr(6)><0 then goto L2190
	if rtrm$(pd$)="" then goto L2150
	pr #255,using L2160: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&pd$(1:25) pageoflow NWPGE
	goto L2160
L2150: pr #255,using L2160: eno$,tr(4),0,0,tr(8)+tr(9),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
L2160: form skip 1,pos 15,c 12,pos 28,pic(zz/zz/zz),pos 36,n 9.2,2*n 14.2,pos 74,n 4,c 28,skip 1
	tr89=tr89+tr(8)+tr(9)
	goto L2260
L2190: if rtrm$(pd$)="" then goto L2220
	pr #255,using L2160: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&pd$(1:25) pageoflow NWPGE
	goto L2230
L2220: pr #255,using L2160: eno$,tr(4),tr(5)+tr(6),tr(9),tr(8),tr(2)," - "&desc$(tr(2))(1:25) pageoflow NWPGE
L2230: tr56=tr56+tr(5)+tr(6)
	tr9=tr9+tr(9)
	tr8=tr8+tr(8)
L2260: return
!
ACCUMULATEFORCATEGORYSUMMARY: !
	cattot(tr(2))=cattot(tr(2))+tr(8)+tr(9)
	cdesc$(tr(2))=desc$(tr(2))
	if lcat=0 and hcat=0 then goto L2320 else goto L2350
L2320: lcat=tr(2)
	hcat=lcat
	goto L2370
L2350: if tr(2)<lcat then lcat=tr(2)
	if tr(2)>hcat then hcat=tr(2)
L2370: return
!
PRINTCATEGORYSUMMARY: !
	if sumcat$="N" then goto L2490 else pr #255,using L2410: "************** Summary by Category *************"
L2410: form pos 90,cc 50,skip 1
L2420: form pos 90,c 50,skip 2
	for j=lcat to hcat
		if cattot(j)=0 then goto L2470
		pr #255,using L2460: cdesc$(j),cattot(j) pageoflow NWPGE
L2460: form pos 90,c 30,pos 124,n 14.2,skip 1
L2470: next j
	pr #255,using L2420: "************************************************"
L2490: mat jobtot=jobtot+cattot
	for j=lcat to hcat
		if rtrm$(cdesc$(j))="" then goto L2530
		jtot$(j)=cdesc$(j)
L2530: next j
	mat cattot=(0)
	lcat=0
	hcat=0
	return
!
PRINTSUMMARYBYJOB: !
	if sumjob$="N" then goto L2690
	pr #255,using L2410: "***************** SUMMARY BY JOB ***************"
	form skip 1,pos 90,c 27,skip 2
	for j=1 to 100
		if jobtot(j)=0 then goto L2670
		pr #255,using L2660: jtot$(j),jobtot(j) pageoflow NWPGE
L2660: form pos 90,c 30,pos 124,n 14.2,skip 1
L2670: next j
	pr #255,using L2420: "************************************************"
L2690: mat tottot=tottot+jobtot
	for j=1 to 100
		if rtrm$(jtot$(j))="" then goto L2730
		tottot$(j)=jtot$(j)
L2730: next j
	mat jobtot=(0)
	return
!
TOTALLINEWITHOUTDETAILS: !
	pr #255,using L2790: val(cn$(7:11)),k$(1:20),l(5),l(4),l(6),l(1),l(3),l(1)-l(4),l(3)-l(6) pageoflow NWPGE
L2790: form pos 1,n 5,pos 9,c 20,pos 36,n 9.2,2*n 14.2,pos 82,4*n 14.2,skip 2
	return
!
PRINTSUMMARYOFALLJOBS: !
	if sumcat$="N" and sumjob$="N" then goto L2920
	pr #255,using L2410: "*************** Summary of All Jobs ************"
	form skip 1,pos 90,c 27,skip 2
	for j=1 to 100
		if tottot(j)=0 then goto L2900
		pr #255,using L2890: tottot$(j),tottot(j) pageoflow NWPGE
L2890: form pos 90,c 30,pos 124,n 14.2,skip 1
L2900: next j
	pr #255,using L2420: "************************************************"
L2920: return
 
NWPGE: ! r:
	pr #255: newpage
	hd=0
	gosub HDR
	hd=1
continue ! /r
 
Xit: fnXit
include: ertn
