! Replace S:\acsPR\newjcNamLst
! pr Name and Number List
 
	autoLibrary
	on error goto Ertn
 
	dim dat$*20,jn$*6,n$*40,cn$*11,cnt$*5,k$*25,cap$*128,p$(20)*50,io1$(2)
	dim cnam$*40,message$*40
 
	fnTop("S:\acsPR\NEWjcNamLst",cap$="Name and Number List")
	fncno(cno,cnam$) : _
	fndat(dat$)
 
 
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	prtcat$="N"
	if fnprocess=1 then goto L330
 
MAIN_SCREEN: !
	fnTos(sn$="namlst1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,1,"Report Heading Date:",23,left)
	fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") : _
	resp$(resp+=1)=dat$
	fnChk(2,mypos,"Print Category Names:",left) : _
	resp$(resp+=1)="False"
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck=5 then goto Xit
	dat$=resp$(1) ! heading date
	if resp$(2)="True" then prtcat$="Y" else prtcat$="N"
	dattab=60-len(rtrm$(dat$))/2
	fndat(dat$,2)
 
L330: on fkey 5 goto L540
	fnopenprn
	gosub HDR
L360: read #1,using L370: jn$,n$ eof L510
L370: form pos 1,c 6,c 40
	first=0
	goto L420
	pr #255: newpage
	gosub HDR
L420: if prtcat$="N" then goto L360
	cnt$="    0"
	read #2,using L470,key>=jn$&cnt$: cn$,k$ exit L510
	goto L480
L460: read #2,using L470: cn$,k$ exit L510
L470: form pos 1,c 11,c 25
L480: if cn$(1:6)><jn$ then goto L360
	gosub L700
	goto L460
L510: exit eof L520,nokey L520
L520: close #1:
	close #2:
L540: fncloseprn
	fnXit
 
HDR: !
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	if prtcat$="N" then pr #255,using L640: "  Job #   Job Name"
L640: form pos 1,c 50,skip 0
	if prtcat$<>"N" then pr #255,using L660: "  Job #   Job Name","Category #   Category Name"
L660: form pos 1,c 50,pos 47,c 50,skip skh
	skh=1
return
 
L700: if first=0 then pr #255,using L710: jn$,n$,cn$(7:11),k$ pageoflow L750: first =1: goto L740
L710: form skip 2,pos 1,c 6,pos 10,c 40,x 1,c 5,pos 60,c 25,skip 1
	pr #255,using L730: cn$(7:11),k$ pageoflow L750
L730: form pos 51,c 5,pos 60,c 25
L740: goto L770
L750: pr #255: newpage
	gosub HDR
L770: return
 
Xit: fnXit
 
include: Ertn
 
