! Replace S:\acsPR\jcNamLst
! pr Name and Number List
 
	autoLibrary
	on error goto Ertn
 
	dim dat$*20,jn$*6,n$*40,cn$*11,cnt$*5,k$*25,cap$*128,p$(20)*50,io1$(2)
	dim cnam$*40,message$*40
 
	fnTop(program$,cap$="Name and Number List")
	fncno(cno,cnam$) : _
	fndat(dat$)
 
	fnconsole(1)
 
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	prtcat$="N"
	if fnprocess=1 then goto L460
 
	pr newpage
	close #101: ioerr L220
L220: open #101: "SRow=9,SCol=14,ERow=14,ECol=66,Border=DR,Caption=<"&cap$,display,outIn
	pr #101: newpage
	pr #101,fields "1,1,Cc 53,R,N": cnam$
	pr #101,fields "2,1,Cc 53,R,N": "Company Number [cno]"
	pr #101,fields "4,2,Cr 30,N": "Report Heading Date:"
	pr #101,fields "5,2,Cr 30,n": "Print Category Names (Y/N):"
	io1$(1)="4,33,C 20,UT,N"
	io1$(2)="5,33,Cu 1,UT,N"
	pr f "15,30,C 10,B,1": "Print (F1)"
	pr f "15,41,C 09,B,5": "Exit (F5)"
L320: rinput #101,fields mat io1$: dat$,prtcat$ conv L320
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L410 else ce=curfld
L350: ce=ce+1: if ce>udim(io1$) then ce=1
L360: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",9) : _
	if ce1=0 then goto L350
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L320
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L360
L410: if cmdkey=5 then goto Xit
	if prtcat$<>"Y" and prtcat$<>"N" then ce=2: goto ERR1
	dattab=60-len(rtrm$(dat$))/2
	fndat(dat$,2)
 
L460: pr newpage
	message$="Printing: please wait"
	fnwait(message$,1)
	on fkey 5 goto L710
	fnopenprn
	gosub HDR
L520: read #1,using L530: jn$,n$ eof L680
L530: form pos 1,c 6,c 40
	first=0
	form skip 2,pos 1,c 6,pos 10,c 40,skip 0
	goto L590
	pr #255: newpage
	gosub HDR
L590: if prtcat$="N" then goto L520
	cnt$="    0"
	read #2,using L640,key>=jn$&cnt$: cn$,k$ exit L680
	goto L650
L630: read #2,using L640: cn$,k$ exit L680
L640: form pos 1,c 11,c 25
L650: if cn$(1:6)><jn$ then goto L520
	gosub L840
	goto L630
L680: exit eof L690,nokey L690
L690: close #1:
	close #2:
L710: fncloseprn
	goto Xit
 
HDR: !
	pr #255,using L760: "Job Name and Number Listing",dat$
L760: form skip 3,pos 47,c 34,skip 1,pos dattab,c 20,skip 1
	if prtcat$="N" then pr #255,using L780: "  Job #   Job Name"
L780: form pos 1,c 50,skip 0
	if prtcat$<>"N" then pr #255,using L800: "  Job #   Job Name","Category #   Category Name"
L800: form pos 1,c 50,pos 47,c 50,skip skh
	skh=1
return
 
L840: if first=0 then pr #255,using L842: jn$,n$,cn$(7:11),k$ pageoflow L870: first =1: goto L860
L842: form skip 2,pos 1,c 6,pos 10,c 40,x 1,c 5,pos 60,c 25,skip 1
	pr #255,using L850: cn$(7:11),k$ pageoflow L870
L850: form pos 51,c 5,pos 60,c 25
L860: goto L890
L870: pr #255: newpage
	gosub HDR
L890: return
 
Xit: fnXit
 
include: Ertn
 
