! Replace S:\acsPR\newjcRemove
! Remove Job Cost Payroll Jobs
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),eno$*12,jno$*6
	dim contact$*30,ph$*12,email$*60
	dim tr(9),pd$*30,tn$*6,n$*40,cap$*128,ml$(1)*70
	dim resp$(1)*60
 
	fnTop("S:\acsPR\newjcRemove",cap$="Remove Completed Jobs")
	fncno(cno)
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno]",internal,outIn: close #1:
 
	execute "Copy [Q]\PRmstr\JCMSTR.h[cno] JCMSTR.X -n"
	execute "Copy [Q]\PRmstr\JCTRANS.h[cno] JCTRANS.X -n"
	execute "Copy [Q]\PRmstr\JCCAT.H[cno] JCCAT.X -n"
 
	open #1: "Name=JCMSTR.X,KFName=[Q]\PRmstr\JCIndx.h[cno]",internal,outIn,keyed
 
ASKJOB: !
	fnTos(sn$="jccpr1J") : _
	respc=0
	fnLbl(1,1,"Job #:",8,1)
	fncmbjob(1,11) : _
	resp$(respc+=1)=jn$
	if trim$(jn$)<>"" then let fnLbl(3,1,"Last job processed:"&trim$(jn$),35,1)
	fnCmdKey("&Next",1,1,0,"Process the job" ) : _
	fnCmdKey("Com&plete",2,0,0,"Finished with all jobs.") : _
	fnCmdKey("&Cancel",5,0,1,"Cancel without deleting any jobs.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if ckey=2 then goto DELETE_THEM
	jn$=lpad$(trim$(resp$(1)(1:6)),6)
	mat ml$(1) : _
	ml$(1)="Do you really want to delete job # "&jn$ : _
	fnmsgbox(mat ml$,resp$,cap$,36)
	if resp$="Yes" then goto L350 else goto ASKJOB
L350: rewrite #1,using 'Form POS 157,N 2',key=jn$: 9 nokey ASKJOB
	goto ASKJOB
 
DELETE_THEM: !
	restore #1:
	open #2: "Name=JCCAT.X,KFName=[Q]\PRmstr\CatIndx.h[cno]",internal,input,keyed
	open #3: "Name=JCTRANS.X",internal,input,relative
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno]",internal,output
	close #11,free:
	open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],SIZE=0,RecL=300",internal,output
	open #12: "Name=[Q]\PRmstr\JCCAT.H[cno]",internal,output
	close #12,free:
	open #12: "Name=[Q]\PRmstr\JCCAT.H[cno],SIZE=0,RecL=123",internal,output
	open #13: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output
	close #13,free:
	open #13: "Name=[Q]\PRmstr\JCTRANS.h[cno],SIZE=0,RecL=88",internal,outIn,relative
	ot4=1
	write #13,using L530,rec=1: " ","",mat tr," ",ot4
L530: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
L540: read #1,using "Form POS 1,C 6,C 40,3*C 30,N 6,2*PD 7.2,N 2,C 30,C 12,C 60": jn$,n$,mat a$,mat b,contact$,ph$,email$ eof EOF1
	form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	if b(4)=9 then goto L540
	cn$=jn$&"     "
	read #2,using L590,key>=cn$: cn$,k$,mat l,mat ta nokey L540
L590: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
	goto L620
L610: read #2,using L590: cn$,k$,mat l,mat ta eof L790
L620: if jn$><cn$(1:6) then goto L790
	if ta(1)=0 then goto L770
	adr=ta(1)
	mat ta=(0)
L660: read #3,using L530,rec=adr: eno$,jno$,mat tr,pd$,nta
	ot4=ot4+1
	if nta>0 then ota=ot4+1 else ota=0
	write #13,using L530,rec=ot4: eno$,jno$,mat tr,pd$,ota
	rewrite #13,using L710,rec=1: ot4
L710: form pos 86,pd 3
	if ta(1)=0 then ta(1)=ot4
	ta(2)=ot4
	if nta=0 then goto L770
	adr=nta
	goto L660
L770: write #12,using L590: cn$,k$,mat l,mat ta
	goto L610
L790: write #11,using "Form POS 1,C 6,C 40,3*C 30,N 6,2*PD 7.2,N 2,C 30,C 12,C 60": jn$,n$,mat a$,mat b,contact$,ph$,email$
	goto L540
 
EOF1: close #1,free:
	close #2,free:
	close #3,free:
	close #11:
	close #12:
	close #13:
	execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -n"
	execute "Index [Q]\PRmstr\JCCAT.H[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys -n"
	df$="[Q]\PRmstr\jcmstr.h[cno]" : if$="[Q]\PRmstr\jcindx.h[cno]" : _
	fncombof("CJob.h[cno]",lyne,mypos,43,df$,1,6,7,25,if$,1)
	goto Xit
 
Xit: fnXit
 
include: ertn
 
