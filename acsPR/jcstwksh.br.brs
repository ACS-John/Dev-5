! Replace S:\acsPR\jcStWkSh
! pr Job Status Worksheet
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,next$*5,cn$*11,cnt$*5,k$*25,response$(5)*1
	dim sc1$(3),sd1$(3),se1$(3)*50,prtj$(100)*6,dat$*20,message$*40
	dim msgline$(2)*60
 
	fnTop(program$,"Job Status Worksheet")
	fndat(dat$)
 
	fnconsole(1)
	prtjob$="N" : perpag$="N"
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",i,i,k
	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,i,k
 
	if fnprocess=1 then goto L590 ! goto "asdf"
	pr newpage
	fnopenwin(win=101,09,14,15,65,'')
	pr #win,fields "4,2,Cr 29,N": "Report Heading Date:"
	pr #win,fields "5,2,Cr 29,N": "Print All Jobs (Y/N):"
	pr #win,fields "6,2,Cr 29,N": "Print One Job Per Page (Y/N):"
	io1$(1)="4,32,C 20,UT,N"
	io1$(2)="5,32,Cu 1,UT,N"
	io1$(3)="6,32,Cu 1,UT,N"
	pr f "16,30,C 09,B,1": "Next (F1)"
	pr f "16,41,C 09,B,5": "Exit (F5)"
	prtjob$="Y"
	perpag$="Y"
L310: rinput #win,fields mat io1$: dat$,prtjob$,perpag$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L400 else ce=curfld
L340: ce=ce+1: if ce>udim(io1$) then ce=1
L350: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L340
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L310
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L350
L400: if cmdkey=5 then goto Xit
	if prtjob$<>"Y" and prtjob$<>"N" then goto L420 else goto L440
L420: ce=2
	goto CONV1
L440: if perpag$<>"Y" and perpag$<>"N" then goto L450 else goto L480
L450: ce=3
	goto CONV1
 
L480: fndat(dat$,2)
	form pos 63,c 20
	if prtjob$="N" then goto L590 ! "asdf"
	if fnprocess=1 then goto L580
 
	pr newpage
	msgline$(1)="Do you wish to skip all"
	msgline$(2)="completed Jobs? (Y/N)"
	fnoldmsgbox(mat response$,env$('program_caption'),mat msgline$,2)
	skpcom$=response$(1)
L580: goto L730
L590: pr newpage
	fnopenwin(win=103,08,20,13,59,env$('program_caption'))
	for j=1 to 100
		pr #win,fields "4,2,Cr 20,n": "Job Number to print:"
		if j>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Job Number entered was "&ltrm$(prtj$(j-1))
		pr f "14,34,C 11,B,2": "Print (F2)"
L650: input #win,fields "4,23,C 6,UT,N": prtj$(j) conv L650
		if cmdkey=2 then goto L710
		if rtrm$(prtj$(j))="" or ltrm$(rtrm$(prtj$(j)))="0" then goto L650
		prtj$(j)=lpad$(rtrm$(prtj$(j)),6)
	next j
	goto L730
L710: j=j-1
 
L730: pr newpage
	on fkey 5 goto DONE
	fnwait(message$,1)
	fnopenprn : _
	if file$(255)(1:3)<>"PRN" then jbskip=1
	gosub HDR
L780: if prtjob$="Y" then goto L830
L790: if j1+=1>j then goto DONE
	read #1,using L810,key=prtj$(j1): jn$,n$,b4 nokey L790
L810: form pos 1,c 6,c 40,pos 157,n 2
	goto L840
L830: read #1,using L810: jn$,n$,b4 eof DONE
L840: if skpcom$="Y" and b4=9 then goto L780
	gosub L1080
	cnt$="    0"
L870: read #2,using L880,key>=jn$&cnt$: cn$,k$,l12,l13 eof L780,nokey L780
L880: form pos 1,c 11,c 25,pos 114,2*pd 2
	if cn$(1:6)><jn$ then goto L780
	gosub L1170
	cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
	goto L870
 
DONE: close #1:
	close #2:
	fncloseprn
	goto Xit
 
HDR: !
	pr #255,using L1010: "Job Status Worksheet",dat$
L1010: form pos 56,c 20,skip 1,pos 1,cc 132,skip 2
	pr #255,using L1030: "Job       Job Name  ","Category   Description","Old %    Percent     Units"
L1030: form pos 2,c 20,pos 47,c 22,pos 88,c 26,skip 1
	pr #255,using L1050: "Number","Number","Complete Complete   Complete"
L1050: form pos 1,c 6,pos 48,c 6,pos 87,c 28,skip 2
return
 
L1080: if fst=1 then goto L1090 else goto L1120
L1090: if perpag$="N" then goto L1130
	pr #255: newpage
	gosub HDR
L1120: fst=1
L1130: pr #255,using L1140: jn$,n$
L1140: form pos 1,c 6,pos 8,c 40,skip jbskip
return
 
L1170: pr #255,using L1210: cn$(7:11),k$,"LABOR",l12,"%","___%","_____.__" pageoflow L1190
	goto L1220
L1190: pr #255: newpage
	gosub HDR
L1210: form pos 49,c 5,pos 55,c 25,pos 81,c 5,pos 89,n 3,c 1,pos 97,c 4,pos 107,c 8,skip 1
L1220: pr #255,using L1210: " "," ","OTHER",l13,"%","___%","        " pageoflow L1250
	pr #255: pageoflow L1250
	goto L1270
L1250: pr #255: newpage
	gosub HDR
L1270: return
 
Xit: fnXit
 
include: ertn
 
