! Replace S:\acsPR\newJCMaint
! Job Cost Master File
autoLibrary
on error goto Ertn
 
dim fl1$(9),sc1$(9)*20,io1$(9),hd$(2)*78,msgline$(2)*60,response$(5)*1
dim fl2$(15),io2$(15),sc3$(12)
dim dup$*11
dim jn$*6
dim hjn$*6
dim n$*40
dim a$(3)*30
dim b(4)
dim cn$*11,k$*25,l(13),ta(2),wrd1$(5)*38,cap$*128,message$*40
dim ph$*12,email$*60,ml$(3)*80,resp$(20)*60
dim eno$*12,jno$*6,tr(9),pd$*30,ln$(2,3)*30,ln(13,3)
dim cm2$(13),item2$(13)*30
 
fnTop(program$,cap$="Job Cost")
! r: opens
open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed ioerr L4890
open #4: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,outIn,keyed ioerr ignore
 
open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed ioerr ignore
open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,outIn,relative ioerr ignore
open #16: "Name=[Q]\PRmstr\Category.H[cno],KFName=[Q]\PRmstr\categoryIDX.H[cno],Shr",internal,outIn,keyed ioerr ignore
! /r
MENU1: !
	ndep=0
ASKJOB: ! r:
	addjob=0 ! add code - used to tell other parts of the program,
	! that I am currently adding an job record.
	fnTos
	respc=0
	fnLbl(1,1,"Job #:",7,right)
	fncmbjob(1,9)
	if hact$="" then           resp$(respc+=1)="" else      resp$(respc+=1)=hact$
	fnCmdKey("&Add",1,0,0,"Add a new job" )
	fnCmdKey("E&dit",2,1,0,"Access the highlited record")
	fnCmdKey("&Next Record",3,0,0,"Access next record injob # order")
	fnCmdKey("&Search",8,0,0,"Search forjob record")
	fnCmdKey("&Proof",11,0,0,"Prints proof listing of all joub information.")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information")
	fnCmdKey("E&Xit",6,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey) ! ask job #
	hact$=lpad$(rtrm$(resp$(1)(1:6)),6)
	jn$=lpad$(rtrm$(resp$(1)(1:6)),6)
 
	if ckey=1 then
		ti1=addjob=1
		goto ADDREC
	else if ckey=2 then
		goto EDITREC
	else if ckey=3 then
		dim contact$*30
		read #1,using L1350: jn$,n$,mat a$,mat b,contact$,ph$,email$ eof L660
		jn$=lpad$(trim$(jn$),6)
		holdjn$=jn$
		goto L670
	else if ckey=8 then
		fnjob_srch(x$,fixgrid)
		jn$=x$
		goto EDITREC
	else if ckey=6 then
		goto Xit
	else if ckey=7 then
		gosub RECREATE_GRID
		goto ASKJOB
	else if ckey=11 then
		goto JOB_LISTING
	end if
goto ASKJOB ! /r
 
ADDREC: ! r:
	addjob=1: mat ta=(0): dup=0: jn$="": n$="": mat a$=(''): mat b=(0)
	contact$="": ph$="": email$=""
	fnTos
	respc=0 : frac=0
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,"Job Number:",mylen,1)
	fnTxt(1,mylen+3,6,6,1,"",0,"Enter the job number to be assigned to this new job.")
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Enter job information.")
	fnCmdKey("&Cancel",5,0,1,"Returns to main screen.")
	fnAcs(mat resp$,ckey) ! ASK NEW JOB #
 
	if ckey=5 then goto ASKJOB
	addjob=1
	jn$=lpad$(trim$(resp$(1)(1:6)),6)
	read #1,using L1350,key=jn$: tempjn$ nokey L570
	mat ml$(2)
	ml$(1)="A record with this number already exists!"
	ml$(2)="Select a different job number."
	fnmsgbox(mat ml$,resp$,cap$,48)
goto ADDREC ! /r
L570: ! r:
	k$=resp$(1)(7:36)
	mat ln=(0)
	mat ta=(0)
goto L670 ! /r
EDITREC: ! r:
	if trim$(jn$)="" or trim$(jn$)="0" then goto ASKJOB
	tjn$=jn$ : hjn$=""
	jn$=lpad$(trim$(jn$),6)
	read #1,using L1350,key=jn$: hjn$,n$,mat a$,mat b,contact$,ph$,email$ nokey L660
	holdjn$=jn$
	goto L670
	L660: !
		mat ml$(2)
		ml$(1)="A record with this number does not exist!"
		ml$(2)="Select a differentjob number."
		fnmsgbox(mat ml$,resp$,cap$,48)
goto ASKJOB ! /r
L670: ! r:
	fnTos
	respc=0 : frac=0
	mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Job Number:",mylen,1)
	! L690: !
	fnTxt(1,mylen+3,6,6,1,"",0,"")
	resp$(respc+=1)=jn$
	fnLbl(2,1,"Job Name:",mylen,1)
	fnTxt(2,mylen+3,40,40,0,"",0,"Any name to identify the job.")
	resp$(respc+=1)=n$
	fnLbl(3,1,"Job Address:",mylen,1)
	fnTxt(3,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=a$(1)
	fnLbl(4,1,"Job Address:",mylen,1)
	fnTxt(4,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=a$(2)
	fnLbl(5,1,"City, State Zip:",mylen,1)
	fnTxt(5,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=a$(3)
	fnLbl(6,1,"Est Completion Date:",mylen,1)
	fnTxt(6,mylen+3,10,10,0,"1",0,"The estimated completion date is only used on some reports and is optional.")
	resp$(respc+=1)=str$(b(1))
	fnLbl(7,1,"Contract Amount:",mylen,1)
	fnTxt(7,mylen+3,12,12,0,"10",0,"")
	resp$(respc+=1)=str$(b(2))
	fnLbl(8,1,"Billings to Date:",mylen,1)
	fnTxt(8,mylen+3,12,12,0,"10",0,"The billings to date field will be updated each time an invoice is processed.")
	resp$(respc+=1)=str$(b(3))
	fnLbl(9,1,"Billing Status:",mylen,1)
	fnTxt(9,mylen+3,2,2,0,"30",0,"The status code should be ????.")
	resp$(respc+=1)=str$(b(4))
	fnLbl(10,1,"Contact Name:",mylen,1)
	fnTxt(10,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=contact$
	fnLbl(11,1,"Phone Number:",mylen,1)
	fnTxt(11,mylen+3,12,12,0,"",0,"")
	resp$(respc+=1)=ph$
	fnLbl(12,1,"E-mail Address:",mylen,1)
	fnTxt(12,mylen+3,60,60,0,"",0,"")
	resp$(respc+=1)=email$
	picture=0
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("&Review Category Records",10,0,0,"Review category records assigned to this job.")
	if addjob=0 then
		fnCmdKey("De&lete",4,0,0,"Deletes this job.")
	end if
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnAcs(mat resp$,ckey) ! detail job screen     editrec
	if ckey=5 then goto ASKJOB
	jn$=lpad$(trim$(resp$(1)(1:6)),6)
	if ckey=4 then goto DELETE_ENTIRE_JOB
	n$=resp$(2) ! name
	a$(1)=resp$(3) ! address
	a$(2)=resp$(4) ! address
	a$(3)=resp$(5) ! city, st zip
	b(1)=val(resp$(6)) ! est completion date
	b(2)=val(resp$(7)) ! contrcat amount
	b(3)=val(resp$(8)) ! Billing date
	b(4)=val(resp$(9)) ! Billing status
	contact$=resp$(10)
	ph$=resp$(11)
	email$=resp$(12) ! e-mail address
	if addjob=1 then goto L1150
	rewrite #1,using L1350,key=jn$: jn$,n$,mat a$,mat b,contact$,ph$,email$ nokey L1150
	goto L1160
	L1150: !
	write #1,using L1350: jn$,n$,mat a$,mat b,contact$,ph$,email$
	L1160: !
if ckey=10 then goto GET_CATEGORY_LISTING else goto L1250 ! /r
GET_CATEGORY_LISTING: ! r:
	cn$=jn$: fncat_srch2(cn$,ckey,x)
	if ckey=97 then holdckey=ckey: goto ADDCAT
	if ckey=98 then holdckey=ckey: goto EDITCATEGORY
	if ckey=95 then cn$(1:6)=lpad$(rtrm$(jn$),6): gosub REVIEW_DETAILS : goto GET_CATEGORY_LISTING
	if ckey=96 then gosub DELETE_CATEGORY : goto GET_CATEGORY_LISTING
	if ckey=12 then gosub DUPLICATE_CATEGORIES: goto GET_CATEGORY_LISTING
	if ckey=6 then goto EDITREC
L1250: goto ASKJOB
! wRD1$(5)="5. Reassign Transaction Addresses"  KJ  what to do with this
! /r
DONE: ! r:
	close #1: ioerr ignore
	close #4: ioerr ignore
	close #2: ioerr ignore
	if addjob=1 or cont=1 then goto IndexAndXit
goto Xit ! /r
 
L1350: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2,c 30,c 12,c 60
L1360: form pos 118,2*pd 3
L1370: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
L1380: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
L1390: form pos 86,pd 3
!
DELETE_ENTIRE_JOB: !
	mat ml$(2)
	ml$(1)="You chosen to Delete job number "&jn$
	ml$(2)="Did you wish to continue?"
	fnmsgbox(mat ml$,resp$,cap$,35)
if resp$="Yes" then goto L1440 else goto ASKJOB
L1440: ! r:
	delete #1,key=hjn$: nokey L1580
	restore #2,key>=hjn$&"     ": nokey L1580
	L1460: !
	read #2,using L1370: cn$,k$,mat l,mat ta eof L1580
	ojn$=cn$(1:6)
	if hjn$<>ojn$ then goto L1580
	delete #2,key=cn$: nokey L1460
	gosub L1510
goto L1460 ! /r
L1510: ! r:
	adr=ta(1)
	do
		if adr=0 then goto L1580
		read #3,using L1390,rec=adr: nta noRec L1580
		delete #3, rec=adr:
		form pos 13,c 6
		adr=nta
	loop
	L1580: !
return ! /r
 
 
 
JOB_LISTING: ! r:
	fst=0
	fnopenprn
	L1900: !
	read #1,using L1350,release: jn$,n$,mat a$,mat b,contact$,ph$,email$ eof L2620
	if fst=1 then goto L1940
	fst=1
	goto L1950
	L1940: !
	pr #255: newpage
	L1950: !
	jcp=1
	x2=0
	gosub L2330
	restore #2,key>=jn$&"     ": nokey L2080
	L1990: !
	read #2,using L1370,release: cn$,k$,mat l eof L1900
	if cn$(1:6)><jn$ then goto L2080
	x1=x1+1
	ln$(1,x1)=cn$(7:11)
	ln$(2,x1)=k$
	for j=1 to 13
		ln(j,x1)=l(j)
	next j
	if x1<3 then goto L1990
	L2080: !
	if x1=0 then goto L2260
	gosub L2110
	goto L2260
	L2110: !
	x1=0
	for j=1 to 2
		dim sc2$(15)*20
		pr #255,using L2140: sc2$(j),ln$(j,1),ln$(j,2),ln$(j,3)
		L2140: form pos 1,c 20,pos 23,3*c 35,skip 1
	next j
	for j=1 to 13
		pr #255,using L2180: sc2$(j+2),ln(j,1),ln(j,2),ln(j,3)
		L2180: form pos 1,c 20,pos 23,n 10.2,pos 58,n 10.2,pos 93,n 10.2,skip 1
	next j
	pr #255:
	x2=x2+1
	mat ln$=("")
	mat ln=(0)
return ! /r
 
L2260: ! r:
	if x2<3 then goto L2310
	pr #255: newpage
	x2=0
	jcp=0
	gosub L2330
	L2310: !
	if cn$(1:6)<=jn$ then goto L1990
goto L1900 ! /r
L2330: ! r:
	gosub HDR
	if jcp=0 then goto L2490
	pr #255,using L2360: sc1$(1),jn$
	L2360: form pos 1,c 20,pos 23,c 6,skip 1
	pr #255,using L2380: sc1$(2),n$
	L2380: form pos 1,c 20,pos 23,c 40,skip 1
	for j=1 to 3
		pr #255,using L2410: sc1$(j+2),a$(j)
		L2410: form pos 1,c 20,pos 23,c 30,skip 1
	next j
	for j=1 to 4
		pr #255,using L2450: sc1$(j+5),b(j)
		L2450: form pos 1,c 21,pos 23,n 12.2,skip 1
	next j
	pr #255:
	pr #255:
L2490: return ! /r
L2510: ! r:
	fnTos
	respc=0
	fnLbl(1,1,"Job # to Start:",16,right)
	fncmbjob(1,19,1)
	if hact$="" then resp$(respc+=1)="" else    resp$(respc+=1)=hact$
	fnCmdKey("&Next",1,1,0,"Duplicate this job")
	fnCmdKey("&Search",8,0,0,"Search forjob record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information")
	fnCmdKey("&Cancel",5,0,1,"Returns to previous screen")
	fnAcs(mat resp$,ckey) ! ask job # to start printing
	if ckey=5 then goto ASKJOB
	jn$=lpad$(rtrm$(resp$(1)(1:6)),6)
	restore #1,key>=jn$: nokey L2510
goto JOB_LISTING ! /r
L2620: ! r:
	if x1>0 then gosub L2110
	on fkey 5 ignore
	fncloseprn
	if fnprocess=1 then goto Xit
goto MENU1 ! /r
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f201 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f221 \fs22 \b Job Cost Proof List}"
	pr #255: "\qc  {\f181 \fs16 \b "&cnvrt$("pic(zzzz/zz/zz)",dat)&"}"
	pr #255: "\ql   "
return ! /r
REASSIGN: ! r: pr NEWPAGE
	restore #2,key>="           ": eof ignore
	do
		read #2,using L1360: mat ta eof L2800
		rewrite #2,using L1360: 0,0
	loop
	L2800: !
	lr3=lrec(3)
	rewrite #3,using L1390,rec=1: lr3
	for j=2 to lr3
		read #3,using L2840,rec=j: cn$,nta noRec L2910
		L2840: form pos 13,c 11,pos 86,pd 3
		read #2,using L1360,key=cn$: mat ta nokey L2910
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #3,using L1390,rec=ta(2): j
		ta(2)=j
		rewrite #2,using L1360,key=cn$: mat ta
		rewrite #3,using L1390,rec=j: 0
		L2910: !
	next j
goto MENU1 ! /r
!
DUPLICATE_CATEGORIES: ! r:
	fnTos
	respc=0
	fnLbl(1,1,"Job # to Duplicate:",20,right)
	fncmbjob(1,23)
	if hact$="" then  resp$(respc+=1)="" else      resp$(respc+=1)=hact$
	fnCmdKey("&Next",1,1,0,"Duplicate this job")
	fnCmdKey("&Search",8,0,0,"Search forjob record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information")
	fnCmdKey("&Cancel",5,0,1,"Returns to previous screen")
	fnAcs(mat resp$,ckey) ! ask job # to duplicate
	if ckey=5 then goto ASKJOB
	djn$=lpad$(rtrm$(resp$(1)(1:6)),6)
	dup$=lpad$(rtrm$(djn$),6)&"     "
	restore #2,key>=dup$: nokey DUPLICATE_CATEGORIES
L3050: read #2,using L1370,release: dupcn$,k$,mat l,mat ta eof GET_CATEGORY_LISTING
	if dupcn$(1:6)<>djn$ then goto GET_CATEGORY_LISTING
	cn$=lpad$(jn$&dupcn$(7:11),11)
	mat l=(0): mat ta=(0)
	read #2,using L1370,key=cn$: cn$,k$,mat l,mat ta nokey L3110 ! check for duplicate catergory records
	goto L3120
L3110: write #2,using L1370: cn$,k$,mat l,mat ta
L3120: goto L3050
!
EDITCAT: !
	addcat=0 ! add code - used to tell other parts of the program,
	! that I am currently adding a category record.
	fnTos
	respc=0
	fnLbl(1,1,"Category #:",10,right)
	fncmbcat(1,12)
	resp$(respc+=1)=cn$
	fnCmdKey("&Add",1,0,0,"Add a new category record." )
	fnCmdKey("E&dit",2,1,0,"Access this record")
	fnCmdKey("&Next Record",3,0,0,"Access next record in category file.")
	fnCmdKey("&Search",8,0,0,"Search for category record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information")
	fnCmdKey("&Complete",6,0,1,"Returns to job screen.")
	fnAcs(mat resp$,ckey) ! ask category #
	hcat$=lpad$(rtrm$(resp$(1)(1:11)),11)
	cn$=lpad$(rtrm$(resp$(1)(1:11)),11)
	if ckey=1 then
		goto ADDCAT
	else if ckey=2 then
		cn$(1:6)=lpad$(rtrm$(jn$),6)
		goto EDITCATEGORY
	else if ckey=3 then
		read #2,using L1370: cn$,k$,mat l,mat ta eof L670: cn$=lpad$(trim$(cn$),11)   !    replaced L690 with L670 because it just looked so wrong.  went back to acs 4 and it was L690 there...  5/20/20 jb
		holdcn$=cn$
		goto EDITCATEGORY
	else if ckey=8 then
		fncategory_srch(x$,fixgrid)
		cn$=x$: goto EDITCAT
	else if ckey=6 then
		goto ASKJOB
	else if ckey=7 then
		gosub RECREATE_CAT_GRID
		goto EDITCAT
	end if
ADDCAT: !
	addcat=1
	fnTos
	respc=0
	fnLbl(1,1,"Category # to Add:",17,right)
	fncmbcategory(1,20)
	!  fnTxt(1,20,5,5,1,"30",0,"Category number must be numeric between 1 and 99999.")
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Adds the new category record." )
	fnCmdKey("&Cancel",5,0,1,"Stops without adding this category record and returns to category listing.")
	fnAcs(mat resp$,ckey) ! ask new category #
	if ckey=5 then goto GET_CATEGORY_LISTING
	cn$=jn$&lpad$(trim$(resp$(1)(1:5)),5)
	k$=resp$(1)(7:36)
	read #2,using L1370,key=cn$: tempcn$ nokey L3380
	mat ml$(2)
	ml$(1)="A category record with this number already exists!"
	ml$(2)="Select a different category number."
	fnmsgbox(mat ml$,resp$,cap$,48)
	goto ADDCAT
	L3380: !
	mat l=(0): mat ta=(0)
	write #2,using L1370: cn$,k$,mat l,mat ta
goto EDITCATEGORY ! /r
EDITCATEGORY: ! r:
	if trim$(cn$)="" or trim$(cn$)="0" then goto EDITCAT
	tcn$=cn$ : hcn$=""
	cnkey$=cn$
	read #2,using L1370,key=cnkey$: cn$,k$,mat l,mat ta eof EDITCAT nokey EDITCAT
	fnTos
	respc=0 : frac=0
	mylen=28 : mypos=mylen+2
	holdcn$=cn$
	fnLbl(1,1,"Category Number:",mylen,1)
	fnTxt(1,mylen+3,5,5,1,"",0,"")
	resp$(respc+=1)=cn$(7:11)
	fnLbl(2,1,"Description:",mylen,1)
	fnTxt(2,mylen+3,25,25,0,"",0,"Any name to identify the category.")
	resp$(respc+=1)=k$
	fnLbl(3,1,"Labor Estimate:",mylen,1)
	fnTxt(3,mylen+3,14,14,0,"10",0,"Labor estimate for this job category.")
	resp$(respc+=1)=str$(l(1))
	fnLbl(4,1,"Hours Estimate:",mylen,1)
	fnTxt(4,mylen+3,14,14,0,"10",0,"Hours estimate for this job category.")
	resp$(respc+=1)=str$(l(2))
	fnLbl(5,1,"Other Estimate:",mylen,1)
	fnTxt(5,mylen+3,14,14,0,"10",0,"Other estimate for this job category.")
	resp$(respc+=1)=str$(l(3))
	fnLbl(6,1,"Labor to Date:",mylen,1)
	fnTxt(6,mylen+3,14,14,0,"10",0,"Labor to Date for this job category.")
	resp$(respc+=1)=str$(l(4))
	fnLbl(7,1,"Hours to Date:",mylen,1)
	fnTxt(7,mylen+3,14,14,0,"10",0,"Hours to Date for this job category.")
	resp$(respc+=1)=str$(l(5))
	fnLbl(8,1,"Other to Date:",mylen,1)
	fnTxt(8,mylen+3,14,14,0,"10",0,"Other to Date for this job category.")
	resp$(respc+=1)=str$(l(6))
	fnLbl(9,1,"Labor Current Period:",mylen,1)
	fnTxt(9,mylen+3,14,14,0,"10",0,"Labor Current Period for this job category.")
	resp$(respc+=1)=str$(l(7))
	fnLbl(10,1,"Hours Current Period:",mylen,1)
	fnTxt(10,mylen+3,14,14,0,"10",0,"Hours Current Period for this job category.")
	resp$(respc+=1)=str$(l(8))
	fnLbl(11,1,"Other Current Period:",mylen,1)
	fnTxt(11,mylen+3,14,14,0,"10",0,"Other Current Period for this job category.")
	resp$(respc+=1)=str$(l(9))
	fnLbl(12,1,"Units Used or Completed:",mylen,1)
	fnTxt(12,mylen+3,14,14,0,"10",0,"Units required for this job category.")
	resp$(respc+=1)=str$(l(10))
	fnLbl(13,1,"Estimated Units:",mylen,1)
	fnTxt(13,mylen+3,14,14,0,"30",0,"Estimated units used so far.")
	resp$(respc+=1)=str$(l(11))
	fnLbl(14,1,"Labor Percent Complete:",mylen,1)
	fnTxt(14,mylen+3,5,5,0,"10",0,"% complete for labor.")
	resp$(respc+=1)=str$(l(12))
	fnLbl(15,1,"Other Percent Complete:",mylen,1)
	fnTxt(15,mylen+3,5,5,0,"10",0,"% complete for other.")
	resp$(respc+=1)=str$(l(13))
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnAcs(mat resp$,ckey) ! full edit on category  edit_category
	if ckey=5 then goto GET_CATEGORY_LISTING
	cn$=jn$&lpad$(trim$(resp$(1)(1:5)),5)
	k$=resp$(2) ! name
	if holdcn$=cn$ then goto L3870
	mat ml$(2)
	ml$(1)="You have chosen to change the category number from "&holdcn$
	ml$(2)="to "&cn$&". Take OK to change, else Cancel."
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L3870 else goto EDITCAT
L3870: l(1)=val(resp$(3)) ! labor estimate
	l(2)=val(resp$(4)) ! hours estimate
	l(3)=val(resp$(5)) ! other estimate
	l(4)=val(resp$(6)) ! labor to date
	l(5)=val(resp$(7)) ! hours to date
	l(6)=val(resp$(8)) ! other to date
	l(7)=val(resp$(9)) ! labor currrent period
	l(8)=val(resp$(10)) ! hours currrent period
	l(9)=val(resp$(11)) ! other currrent period
	l(10)=val(resp$(12)) ! units
	l(11)=val(resp$(13)) ! estimated units
	l(12)=val(resp$(14)) ! labor % complete
	l(13)=val(resp$(15)) ! labor % complete
	rewrite #2,using L1370: cn$,k$,mat l,mat ta
	if holdckey=98 then holdckey=0: goto GET_CATEGORY_LISTING ! return to caterory grid listing from editcategory
	if holdckey=99 then holdckey=0: goto GET_CATEGORY_LISTING
	if addcat=1 then goto ADDCAT
goto ASKJOB ! /r
REVIEW_DETAILS: ! r:
	fnTos
	dim ch2$(13)
	ch2$(1)="Rec #": ch2$(2)="Reference #": ch2$(3)="Job #"
	ch2$(4)="Cat #"
	ch2$(5)="Sub #": ch2$(6)="P/R Dept": ch2$(7)="Date"
	ch2$(8)="Reg Hrs": ch2$(9)="OT Hours": ch2$(10)="Units"
	ch2$(11)="Pay Tax": ch2$(12)="Amount"
	ch2$(13)="Description"
	mat ch2$(13) ! : Mat CM2$(13) : Mat ITEM2$(13)
	cm2$(1)="30": cm2$(2)="": cm2$(3)="30"
	cm2$(4)="30"
	cm2$(5)="30": cm2$(6)="30": cm2$(7)="3"
	cm2$(8)="32": cm2$(9)="32": cm2$(10)="10"
	cm2$(11)="10": cm2$(12)="10": cm2$(13)=""
	fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
	! Restore #2,Key>=JN$&CN$: Nokey 4200
	catkey$=cn$ ! $(TRIM$(JN$),6)&LPAD$(TRIM$(CN$),5)
	READ_FILE: !
	read #2,using L1360,key=catkey$: mat ta nokey L4160
	goto L4170
	L4160: !
		mat ml$(2)
		ml$(1)="There no transactions for this category!"
		ml$(2)="Select a different category."
		fnmsgbox(mat ml$,resp$,cap$,48)
	goto GET_CATEGORY_LISTING
	L4170: !
	nta=ta(1)
	L4180: !
	if nta then
		read #3,using L1380,rec=nta: eno$,jno$,mat tr,pd$,nta
		item2$(1)=str$(rec(3)): item2$(2)=eno$: item2$(3)=jno$: item2$(4)=str$(tr(1))
		item2$(5)=str$(tr(2)): item2$(6)=str$(tr(3))
		item2$(7)=str$(tr(4))
		item2$(8)=str$(tr(5))
		item2$(9)=str$(tr(6)): item2$(10)=str$(tr(7))
		item2$(11)=str$(tr(8)) : item2$(12)=str$(tr(9))
		item2$(13)=pd$
		fnflexadd1(mat item2$)
		goto L4180
	end if
	fnCmdKey("&Add",1,0,0,"Add a new category record." )
	fnCmdKey("&Delete",4,0,0,"Deletes the highlited record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information")
	fnCmdKey("E&Xit",5,0,1,"Returns to main screen.")
	fnAcs(mat resp$,ckey) ! review_details  grid of transactions
	if ckey=5 then goto GET_CATEGORY_LISTING
	detaileditrec=val(resp$(1))
	if ckey=1 then adddetails=1: mat tr=(0): eno$="": jno$=jn$: goto EDIT_DETAILS
	if ckey=2 then read #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta : editdetails=1 : goto EDIT_DETAILS
	if ckey=4 then
		read #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta
		eno$=jno$=pd$="": mat tr=(0)
		rewrite #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta
		goto GET_CATEGORY_LISTING
	end if
goto GET_CATEGORY_LISTING ! /r
EDIT_DETAILS: ! r:
	fnTos
	respc=0 : frac=0
	mylen=28 : mypos=mylen+2
	holdcn$=cn$
	fnLbl(1,1,"Ref/Employee #:",mylen,1)
	fnTxt(1,mylen+3,12,12,1,"",0,"Can contain a check number, an employee number, or some other reference number.")
	resp$(respc+=1)=eno$
	fnLbl(2,1,"Job #:",mylen,1)
	fnTxt(2,mylen+3,6,6,0,"",0,"Job # to which this charge or cost was posted.")
	resp$(respc+=1)=jno$
	fnLbl(3,1,"Category #:",mylen,1)
	fnTxt(3,mylen+3,6,6,0,"",0,"Labor estimate for this job category.")
	resp$(respc+=1)=str$(tr(1))
	fnLbl(4,1,"Sub-Category:",mylen,1)
	fnTxt(4,mylen+3,2,2,0,"10",0,"Sub category to which it should be classified")
	resp$(respc+=1)=str$(tr(2))
	fnLbl(5,1,"P/R Dept:",mylen,1)
	fnTxt(5,mylen+3,3,3,0,"30",0,"Other estimate for this job category.")
	resp$(respc+=1)=str$(tr(3))
	fnLbl(6,1,"Date:",mylen,1)
	fnTxt(6,mylen+3,8,8,0,"1",0,"Labor to Date for this job category.")
	resp$(respc+=1)=str$(tr(4))
	fnLbl(7,1,"Reg Hrs:",mylen,1)
	fnTxt(7,mylen+3,10,10,0,"32",0,"Regular hours associated with this cost.")
	resp$(respc+=1)=str$(tr(5))
	fnLbl(8,1,"O/T Hrs:",mylen,1)
	fnTxt(8,mylen+3,10,10,0,"32",0,"Overtime hours associated with is cost.")
	resp$(respc+=1)=str$(tr(6))
	fnLbl(9,1,"Units:",mylen,1)
	fnTxt(9,mylen+3,10,10,0,"30",0,"Only applicable if units are being tracked.")
	resp$(respc+=1)=str$(tr(7))
 
	fnLbl(10,1,"Payroll Tax:",mylen,1)
	fnTxt(10,mylen+3,14,14,0,"10",0,"If this cost is for earnings, the associated payroll tax expense should be entered here.")
	resp$(respc+=1)=str$(tr(8))
	fnLbl(11,1,"Amount:",mylen,1)
	fnTxt(11,mylen+3,14,14,0,"10",0,"Amount of cost being charged to the job.")
	resp$(respc+=1)=str$(tr(9))
	fnLbl(12,1,"Description:",mylen,1)
	fnTxt(12,mylen+3,30,30,0,"",0,"Brief description of cost.")
	resp$(respc+=1)=pd$
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnAcs(mat resp$,ckey) ! full edit details
	if ckey=5 then goto REVIEW_DETAILS
	eno$=resp$(1) ! employee #/ ref #
	jno$=resp$(2) ! job number
	tr(1)=val(resp$(3)) ! category
	tr(2)=val(resp$(4)) ! sub-category
	tr(3)=val(resp$(5)) ! pr dept
	tr(4)=val(resp$(6)) ! date
	tr(5)=val(resp$(7)) ! regular hours
	tr(6)=val(resp$(8)) ! ot hours
	tr(7)=val(resp$(9)) ! units
	tr(8)=val(resp$(10)) ! paroll taxes
	tr(9)=val(resp$(11)) ! amount
	pd$=resp$(12) ! description
	if editdetails=1 then rewrite #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$ : editdetails=0: goto REVIEW_DETAILS
	if adddetails=1 then goto L4780 else goto L4860
	L4780: !
	read #2,using L1360,key=cn$: mat ta
	L4790: !
	lrec3=lrec(3)+1
	write #3,using L1380,rec=lrec3: eno$,jno$,mat tr,pd$,0 ioerr L4790
	if ta(2)>0 then rewrite #3,using L1390,rec=ta(2): lrec3
	if ta(1)=0 then ta(1)=ta(2)=lrec3
	if ta(2)=0 then ta(2)=lrec3
	rewrite #2,using L1360,key=cn$: mat ta
	adddetails=0: goto REVIEW_DETAILS
	L4860: !
goto REVIEW_DETAILS ! /r
 
L4890: !
	if err=4152 then goto L1640 else goto Ertn
	if err=4152 then goto L1690 else goto Ertn
	if err=4152 then goto L1750 else goto Ertn
	if err=4152 then goto L1800 else goto Ertn
	if err=4152 then goto IndexAndXit else goto Ertn
 
L1640: ! r:
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],SIZE=0,RecL=300",internal,output
	cont=1
	close #2: ioerr ignore
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno]",internal,input ioerr L1690
	close #2,free:
	L1690: !
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],SIZE=0,RecL=123",internal,output
	close #2:
	close #3: ioerr ignore
	open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output ioerr L1750
	close #3,free:
	L1750: !
	open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],SIZE=0,RecL=88",internal,output
	write #3,using L1380: " ","",mat tr," ",1
	close #3:
	open #1: "Name=[Q]\PRmstr\JCPRH1.H[cno]",internal,output ioerr L1800
	close #1,free:
	L1800: !
	open #1: "Name=[Q]\PRmstr\JCPRH1.H[cno],SIZE=0,RecL=40",internal,output
	close #1:
	IndexAndXit: !
	fnIndex('[Q]\PRmstr\JCMSTR.h[cno]','[Q]\PRmstr\JCIndx.h[cno]','1,6')
	fnIndex('[Q]\PRmstr\JCMSTR.h[cno]','[Q]\PRmstr\JCINDX2.H[cno]','7,25')
	fnIndex('[Q]\PRmstr\JCCAT.H[cno]' ,'[Q]\PRmstr\CatIndx.h[cno]','1,11')
goto Xit ! /r
Xit: fnXit
 
 
 
RECREATE_GRID: ! r:
	close #1: ioerr ignore
	close #4: ioerr ignore
	fnjob_srch(x$,99)
	fncombof("CJob.h[cno]",lyne,mypos,43,"[Q]\PRmstr\jcmstr.h[cno]",1,6,7,25,"[Q]\PRmstr\jcindx.h[cno]" ,1)
	fncombof("CJobALL.h[cno]",lyne,mypos,43,"[Q]\PRmstr\jcmstr.h[cno]",1,6,7,25,"[Q]\PRmstr\jcindx.h[cno]" ,2)
	execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -N" ioerr ignore
	execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCINDX2.H[cno],7,25,Replace,DupKeys -N" ioerr ignore
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed ioerr Xit
	open #4: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,outIn,keyed ioerr Xit
return ! /r
RECREATE_CAT_GRID: ! r:
	close #2: ioerr ignore
	L5190: !
	fncategory_srch(x$,99)
	fncombof("CCat.h[cno]",lyne,mypos,43,"[Q]\PRmstr\jccat.h[cno]",1,11,12,25,"[Q]\PRmstr\catindx.h[cno]" ,1)
	fncombof("CCatALL.h[cno]",lyne,mypos,43,"[Q]\PRmstr\jccat.h[cno]",1,11,12,25,"[Q]\PRmstr\catindx.h[cno]" ,2)
	execute "Index [Q]\PRmstr\JCCAT.H[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys -N" ioerr L5220
	L5220: !
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed ioerr Xit
return ! /r
DELETE_CATEGORY: ! r:
	read #2,using L1360,key=cn$: mat ta nokey L5360
	if sum(ta)=0 then goto L5290
	mat ml$(2)
	ml$(1)="This category contains detail transactions about the job!"
	ml$(2)="Are you sure you wish to delete it?"
	fnmsgbox(mat ml$,resp$,cap$,33)
	if resp$="OK" then goto L5290 else goto GET_CATEGORY_LISTING
	L5290: !
	delete #2,key=cn$: ioerr GET_CATEGORY_LISTING
	if sum(ta)>0 then goto L5310 else goto L5360
	L5310: nta=ta(1)
	do
		if nta>0 then
			holdnta=nta
			goto L5330
		else
			goto L5360
		end if
		L5330: read #3,using L2840,rec=nta: cn$,nta noRec L5360
		delete #3,rec=holdnta: ioerr ignore
	loop
L5360: !
return ! /r
include: Ertn
