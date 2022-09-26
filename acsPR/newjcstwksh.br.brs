! Replace S:\acsPR\newjcStWkSh
! pr Job Status Worksheet
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,next$*5,cn$*11,cnt$*5,k$*25,cap$*128,resp$(3)*40,cnam$*40
	dim sc1$(3),sd1$(3),se1$(3)*50,prtj$(100)*6,dat$*20
	dim ml$(1)*80
 
	fnTop(program$,cap$="Job Status Worksheet")
	fncno(cno,cnam$) : _
	fndat(dat$)
 
	prtjob$="N" : perpag$="N"
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",i,i,k
	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,i,k
 
	if fnprocess=1 then goto ASKJOB
MAIN_SCREEN: !
	fnTos(sn$="namlst1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,1,"Report Heading Date:",23,left)
	fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") : _
	resp$(resp+=1)=dat$
	fnChk(2,mypos,"Print All Jobs:",left) : _
	resp$(resp+=1)='False'
	fnChk(3,mypos,"Print One Job Per Page:",left) : _
	resp$(resp+=1)='False'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dat$=resp$(1) ! heading date
	if resp$(2)='True' then prtjob$="Y" else prtjob$="N"
	if resp$(3)='True' then perpag$="Y" else perpad$="N"
 
	fndat(dat$,2)
	if prtjob$="N" then goto ASKJOB
	if fnprocess=1 then goto L510
 
	mat ml$(1) : _
	ml$(1)="Do you wish to skip all completed jobs?" : _
	fnMsgBox(mat ml$,resp$,cap$,4)
	if resp$="Yes" then skpcom$="Y" else skpcom$="N"
	goto L510
ASKJOB: !
	for j=1 to 100
		fnTos(sn$="prtdet2") : _
		mylen=12 : mypos=mylen+3: resp=0: left=1 : _
		fnLbl(1,1,"Job Number:",mylen,1) : _
		fncmbjob(1,mypos) : _
		resp$(respc+=1)=jn$
		prtj$(j)=lpad$(rtrm$(prtj$(j)),6)
		fnCmdKey("&Next",1,1,0,"Print this job." ) : _
		fnCmdKey("&Complete",5,0,1,"No more jobs. Release the print.")
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto L490
		prtj$(j)=lpad$(rtrm$(resp$(1)(1:6)),6)
	next j
	goto L510
L490: j=j-1
 
L510: on fkey 5 goto DONE
	fnopenprn : _
	if file$(255)(1:3)<>"PRN" then jbskip=1
	gosub HDR
L540: if prtjob$="Y" then goto L590
L550: j1+=1: if j1>j then goto DONE
	read #1,using L570,key=prtj$(j1): jn$,n$,b4 nokey L550
L570: form pos 1,c 6,c 40,pos 157,n 2
	goto L600
L590: read #1,using L570: jn$,n$,b4 eof DONE
L600: if skpcom$="Y" and b4=9 then goto L540
	gosub L870
	cnt$="    0"
L630: read #2,using L640,key>=jn$&cnt$: cn$,k$,l12,l13 eof L540,nokey L540
L640: form pos 1,c 11,c 25,pos 114,2*pd 2
	if cn$(1:6)><jn$ then goto L540
	gosub L960
	cnt$=lpad$(rtrm$(str$(val(cn$(7:11))+1)),5)
	goto L630
 
DONE: close #1:
	close #2:
	fncloseprn
	goto Xit
 
HDR: !
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	form pos 56,c 20,skip 1,pos 1,cc 132,skip 2
	pr #255: "{\b  Job       Job Name                           Category   Description                 Old %   Percent     Units}"
	pr #255: "{\b Number                                         Number                               Complete Complete  Complete}"
	pr #255: "\ql   "
return
 
L870: if fst=1 then goto L880 else goto L910
L880: if perpag$="N" then goto L920
	pr #255: newpage
	gosub HDR
L910: fst=1
L920: pr #255,using L930: jn$,n$
L930: form pos 1,c 6,pos 8,c 40,skip jbskip
return
 
L960: pr #255,using L1000: cn$(7:11),k$,"LABOR",l12,"%","___%","_____.__" pageoflow L980
	goto L1010
L980: pr #255: newpage
	gosub HDR
L1000: form pos 49,c 5,pos 55,c 25,pos 81,c 5,pos 89,n 3,c 1,pos 97,c 4,pos 107,c 8,skip 1
L1010: pr #255,using L1000: " "," ","OTHER",l13,"%","___%","        " pageoflow L1040
	pr #255: pageoflow L1040
	goto L1060
L1040: pr #255: newpage
	gosub HDR
L1060: return
 
Xit: fnXit
 
include: ertn
 
