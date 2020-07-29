! formerly S:\acsPR\newJCCPR1
 
autoLibrary
on error goto Ertn
 
dim jn$*6,cn$*11,ta(2),tr(9),io1$(2),n$*40,tn$*6,dr(7),en$*12
dim resp$(3)*50,ml$(2)*60
 
fnTop(program$)
 
open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,input,relative
open #4: "Name=[Temp]\Work."&session$&",SIZE=0,RecL=55,Replace",internal,output
 
MENU1: !
	fnTos
	respc=0
	fnLbl(1,47," ",1,1)
	fnLbl(1,1,"Beginning Date:",20,1)
	fnTxt(1,23,8,0,0,"1",0,"First day of week being printed.")
	resp$(respc+=1)=str$(df)
	fnLbl(2,1,"Ending Date:",20,1)
	fnTxt(2,23,8,0,0,"1",0,"Week ending date.")
	resp$(respc+=1)=str$(dt)
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	df=val(resp$(1)) ! beginning date
	dt=val(resp$(2)) ! ending date
 
	mat dr=(0)
	dr(1)=int(df*.01)
	dr(7)=int(dt*.01)
	df1=df
	dt1=dt
	df=fndate_mmddyy_to_ccyymmdd(df)
	dt=fndate_mmddyy_to_ccyymmdd(dt)
	if dt<df then goto MENU1
	for j=2 to 7
		if dr(9-j)-int(dr(9-j)*.01)*100-1=0 then goto L410
		dr(8-j)=dr(7)-j+1
	next j
	L410: !
	if dr(1)=0 then dr(1)=int(df1*.01)
	for j=2 to 6
		if dr(j)>0 then goto L460
		dr(j)=dr(1)+j-1
	next j
	L460: !
	write #4,using L470: df1,dt1,mat dr
	L470: form pos 1,2*n 6,7*pd 3
goto ASKJOB
 
ASKJOB: !
	fnTos
	respc=0
	fnLbl(1,1,"Job #:",8,1)
	fncmbjob(1,11)
	resp$(respc+=1)=jn$
	if trim$(jn$)<>"" then let fnLbl(3,1,"Last job processed:"&trim$(jn$),35,1)
	fnCmdKey("&Next",1,1,0,"Process the job" )
	fnCmdKey("&Complete",2,0,0,"Start printing")
	fnAcs(mat resp$,ckey)
	if ckey=2 then goto PRINT_REPORT
	jn$=lpad$(trim$(resp$(1)(1:6)),6)
	rw=0
	cn$=jn$&"     "
	read #2,using L620,key>=cn$: cn$,mat ta nokey L640
	L620: form pos 1,c 11,pos 118,2*pd 3
goto L670
L640: !
	if rw>0 then goto ASKJOB
	mat ml$(2)
	ml$(1)="No Transactions exist for Job Number "&ltrm$(jn$)
	ml$(2)="within the specified date range."
	fnmsgbox(mat ml$,resp$,'',0)
	goto ASKJOB
	L660: !
	read #2,using L620: cn$,mat ta eof L640
	L670: !
	if jn$><cn$(1:6) then goto L640
	if ta(1)=0 then goto L660
	adr=ta(1)
	L700: !
	read #3,using L710,rec=adr: en$,tn$,mat tr,nta
	L710: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,x 30,pd 3
	if tr(5)+tr(6)=0 then goto L780
	tr4=fndate_mmddyy_to_ccyymmdd(tr(4))
	if tr4<df or tr4>dt then goto L780
	write #4,using L760: en$,tn$,mat tr
	L760: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
	rw=rw+1
	L780: !
	if nta=0 then goto L660
	adr=nta
goto L700
PRINT_REPORT: !
	if rw=0 then goto Xit
	close #2:
	close #3:
	close #4:
	open #1: "Name=[Temp]\Control."&session$,internal,output
	restore #1:
	write #1,using L880: "FILE [Temp]\Work."&session$&",,,[Temp]\Addr."&session$&",,,acsPR,,A,N"
	L880: form pos 1,c 128
	write #1,using L880: "MASK 13,6,c,a,1,12,c,a,33,2,c,a,29,4,c,a"
	close #1:
	execute "FREE [Temp]\Addr."&session$&" -n"
	execute "Sort [Temp]\Control."&session$&" -n"
fnchain ("S:\Payroll\Job Cost\Certified Payroll Register (Part 2)")
 
Xit: fnXit
include: Ertn
