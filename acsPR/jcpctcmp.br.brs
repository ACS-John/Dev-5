! Replace S:\acsPR\jcPctCmp
! Enter Percent Complete
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,msgline$(2)*60,response$(5)*1
	dim jn$*6,jno$*6,n$*40,cn$*11,cnt$*5,k$*25
 
	fnTop("S:\acsPR\jcPctCmp",cap$="Enter Percent Complete")
	fncno(cno)
 
	fnconsole(1)
 
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed
 
L170: pr newpage
	fnopenwin(win=101,08,07,16,72,cap$)
	pr #win,fields "4,2,Cr 25,N": "Job Number:"
	pr #win,fields "5,2,Cr 25,N": "Cost Category:"
	pr #win,fields "6,2,Cr 25,N": "Percent Complete (Labor):"
	pr #win,fields "7,2,Cr 25,N": "Percent Complete (Other):"
	pr #win,fields "8,2,Cr 25,N": "Total Units Complete:"
	io1$(1)="4,28,C 6,UT,N"
	io1$(2)="5,28,N 5,UT,N"
	io1$(3)="6,28,N 3,UT,N"
	io1$(4)="7,28,N 3,UT,N"
	io1$(5)="8,28,N 7,UT,N"
	pr f "17,30,C 09,B,1": "Next (F1)"
	pr f "17,41,C 09,B,5": "Exit (F5)"
L310: input #win,fields mat io1$: jn$,cn,l12,l13,l10 conv CONV1
	if rtrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto DONE
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L490 else ce=curfld
	if ce<>1 then goto L390
	read #1,using L370,key=lpad$(rtrm$(jn$),6): n$ nokey L630
L370: form pos 7,c 40
	pr #win,fields "4,36,C 30,N": n$(1:30)
L390: if ce<>2 then goto L430
	cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
	read #2,using L600,key=cn$: k$,rl10,rl12,rl13 nokey L630
	pr #win,fields "5,36,C 25,N": k$
L430: ce=ce+1: if ce>udim(io1$) then ce=1
L440: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L430
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L310
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L440
L490: if cndkey=5 then goto DONE
	if rtrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto DONE
	cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(cn),5)
	read #2,using L600,key=cn$: k$,rl10,rl12,rl13 nokey L630
	if l10=0 then goto L540 else goto L550
L540: l10=rl10
L550: if l12=0 then goto L560 else goto L570
L560: l12=rl12
L570: if l13=0 then goto L580 else goto L590
L580: l13=rl13
L590: rewrite #2,using L600,key=cn$: k$,l10,l12,l13 nokey L630
L600: form pos 12,c 25,pos 100,pd 7.2,pos 114,2*pd 2
	goto L170
 
L630: msgline$(1)="Invalid Job Number or Category Number"
	msgline$(2)="Please reselect."
	fnoldmsgbox(mat response$,cap$,mat msgline$,1)
	ce=1
	goto ERR1
 
	goto L310
 
DONE: !
	close #2:
	goto Xit
 
include: Ertn
 
Xit: fnXit
 
