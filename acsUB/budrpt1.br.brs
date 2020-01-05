! Replace S:\acsUB\BudRpt1
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit, fnget_services
	library 'S:\Core\Library': fnopenprn,fncloseprn, fndate_mmddyy_to_ccyymmdd, fnxit,fnAcs,fnTos,fnLbl,fnTxt,fnCmdSet
	on error goto Ertn
! ______________________________________________________________________
	dim ba(13),bt1(14,2),badr(2),n$*25,txt$*40
	dim t1(11),t2(11),t3(11),cap$*128,message$*40,serviceName$(10)*20,service$(10)*2,hdr$*255,underline$*255,budget$*255
	fntop(program$, cap$="Worksheet")
! ______________________________________________________________________
	fnget_services(mat serviceName$,mat service$)
	hdr$="{\ul  Date   }"
	underline$="          "
	for j=1 to 10
		if trim$(serviceName$(j))<>"" then 
			hdr$=hdr$&"  {\ul "&lpad$(rtrm$(serviceName$(j)(1:8)),8)&"}" 
			underline$=underline$&"{\ul         }  " 
			totserv=totserv+1
		end if
	next j
	hdr$=hdr$&"  {\ul Net Bill}" 
	underline$=underline$&"{\ul         }  " 
	totserv1=totserv+2 
	mat t1(totserv1) : mat t2(totserv1) : mat t3(totserv1)
! ______________________________________________________________________
BUD1: ! INITILIZE BUDGET FILE
	bud1=bg1=bg2=0
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed 
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative 
	bud1=1
	sn$="BudRpt1" 
	fnTos(sn$) 
	mylen=32 : mypos=mylen+2
	txt$="Starting Date (blank for all):" 
	fnLbl(1,1,txt$,mylen,1)
	fnTxt(1,mypos,8,0,0,"1") 
	resp$(1)=""
	txt$="Ending Date (blank for all):" 
	fnLbl(2,1,txt$,mylen,1)
	fnTxt(2,mypos,8,0,0,"1") 
	resp$(2)=""
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	d1=val(resp$(1)) conv L350
L350: d2=val(resp$(2)) conv L360
L360: ! ______________________________________________________________________
	on fkey 5 goto XIT
! On Pageoflow Goto NEWPGE
	fnopenprn
	gosub HEADING
goto READ_BUDMSTR
! ______________________________________________________________________
READ_BUDMSTR: ! 
	read #81,using L490: z$,mat ba,mat badr eof DONE
	if env$('client')="Findlay" then ba(8)=0 ! don't show the penalty budget on form
	totba=totalbudget=totactual=0
	if ba(12)>0 then totba=ba(12): goto L490 ! if net bill in budget, use it
	for j=2 to 11: totba=totba+ba(j): next j
	L490: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	read #1,using L510,key=z$: n$,bal nokey READ_BUDMSTR
	L510: form pos 26,c 25,pos 292,pd 4.2
	pr #255,using "Form POS 1,C 12,C 25": z$,n$ pageoflow NEWPGE 
	pr #255: "" pageoflow NEWPGE
	pr #255: hdr$ pageoflow NEWPGE
	ta1=badr(1)
	mat badr=(0)
	bt1=btdue=0
	L570: !
	if ta1=0 then goto L810
	read #82,using L600,rec=ta1: x$,mat bt1,nba noRec L810
	if sum(bt1)=0 then goto L800 ! skip any blank records
	L600: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	y=bt1(1,2) : y=fndate_mmddyy_to_ccyymmdd(y)
	x=0 : if d1>0 then x=d1 : x=fndate_mmddyy_to_ccyymmdd(x)
	if y<x then goto L800
	x=0 : if d2>0 then x=d2 : x=fndate_mmddyy_to_ccyymmdd(x) else goto L660
	if y>x then goto L800
	L660: !
	service=1
	t1(1)=bt1(1,1) ! date
	for j=1 to 10
		if trim$(serviceName$(j))="" then goto L720
		service=service+1
		t1(service)=bt1(j+1,2) 
		totalbudget=totalbudget+bt1(j+1,1) 
		totactual=totactual+bt1(j+1,2)
		L720: !
	next j
	t1(service+1)=bt1(12,2)
	pr #255,using "Form POS 1,PIC(ZZ/ZZ/ZZ),TOTSERV1*N 10.2": mat t1 pageoflow NEWPGE
	t1(1)=0
	mat t2=t2+t1 : mat t3=t3+t1
	t2+=1 : t3+=1
	if bt1(14,1)=0 then bt1=bt1+1: else goto L800 
		! Total Budget Bills Not Paid
	for j=1 to 10 : btdue=btdue+bt1(j+1,1) : next j
	L800: !
	ta1=nba
	goto L570
	L810: !
	pr #255: underline$ ! "{\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }" Pageoflow NEWPGE
	pr #255,using "Form POS 1,C 7,PIC(Z),TOTSERV1*N 10.2": "Total",mat t2 pageoflow NEWPGE
	if t2<2 then goto L870
	for j=1 to udim(t2)
		if t2(j)>0 then t2(j)=t2(j)/t2
	next j
	L870: !
	pr #255: underline$ ! "{\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }  {\ul         }" Pageoflow NEWPGE
	pr #255,using "Form POS 1,C 7,PIC(Z),TOTSERV1*N 10.2": "Average",mat t2 pageoflow NEWPGE
	pr #255: "" pageoflow NEWPGE
	budget$="Current Budget Amounts: "
	for j=1 to 10
		if trim$(serviceName$(j))="" then goto L950
		if ba(j+1)=0 then goto L950
		budget$=budget$&"  "&trim$(serviceName$(j))&"="&trim$(cnvrt$("pic($$$$$.$$",ba(j+1)))
		L950: !
	next j
	pr #255: budget$ pageoflow NEWPGE
	pr #255: "Current Balance:"&cnvrt$("PIC($$$,$$$.$$ CR",bal) pageoflow NEWPGE
	pr #255: "Number of Budget Payments Not Received:"&cnvrt$("PIC(ZZ#)",bt1) pageoflow NEWPGE
	pr #255: "Total Budget Payments Not Received:"&cnvrt$("PIC($$$,$$$.$$ CR",btdue) pageoflow NEWPGE
	pr #255: "Excess Budget Billings Over Actual Billing (Under=Cr):"&cnvrt$("PIC($$$,$$$.$$ CR",(totalbudget-totactual)) pageoflow NEWPGE
	pr #255, using "Form Pos 1,C 78": "{\ul \strike "&rpt$(" ",58)&"}" pageoflow NEWPGE
	mat t2=(0) : t2=0
goto READ_BUDMSTR

NEWPGE: ! r:
	pr #255: newpage 
	gosub HEADING 
	continue ! /r

HEADING: ! r:
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zz/zz/zz)",d1)&"To: "&cnvrt$("pic(zz/zz/zz)",d2)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: ""
return ! /r

DONE: ! 
	fncloseprn
XIT: fnxit
include: ertn
