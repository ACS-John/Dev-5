! Replace S:\acsPR\pr401k
! Electronic 401k Filing

	library 'S:\Core\Library': fntop,fnxit, fnopenwin, fndate_mmddyy_to_ccyymmdd,fncreg_read
	library 'S:\Core\Library': fngethandle
	on error goto Ertn

	dim a$*40,em$(3)*30,ta(2),cp(22),tcp(22),hc(5),thc(5),d$*20,whc(10)
	dim dedcode(10),calcode(10),dedfed(10),k$(20)*30,em$*30
	dim dv$*1,message$*40

	fntop(program$)
	fncreg_read('calculation date',tmp$) : ppd=val(tmp$) 
	fncreg_read('calculation date text',d$)
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input  
	read #1,using 'Form POS 1,C 40,POS 618,30*N 1': a$,mat dedcode,mat calcode,mat dedfed : close #1: 
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],Shr",internal,input,relative 
	open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,input,relative 
	open #hpraddr:=fngethandle: "Name=[Q]\PRmstr\praddr1.h[cno]",internal,input 
	open #4: "Name=[Q]\PRmstr\PR401K.DAT,RecL=235,Replace",display,output 
	ReadPrAddr1: !
	read #hpraddr,using 'Form POS 1,PD 3': address eof END1
	read #1,using L240,rec=address: eno,mat em$,ss$,em16,lpd,mat ta,bd noRec ReadPrAddr1
	L240: form pos 1,n 8,3*c 30,c 11,pos 156,2*n 6,pos 173,2*pd 3,pos 191,n 6
	if lpd><ppd then goto ReadPrAddr1
	a=pos (rtrm$(em$(1))," ",1)
	b=pos (rtrm$(em$(1))," ",a+1)
	c=a+1
	em$=ltrm$(rtrm$(em$(1)(max(a,b):30))&", "&em$(1)(1:a))
	if c<b then em$=em$&em$(1)(c:c)
	csz$=em$(3) : gosub CSZ
	ss$=ltrm$(rtrm$(ss$))
	for j=1 to len(ss$)
		if ss$(j:j)<"0" or ss$(j:j)>"9" then ss$(j:j)=""
	next j
	ss=0
	ss=val(ss$) conv ignore
	em16=fndate_mmddyy_to_ccyymmdd(em16)
	bd=fndate_mmddyy_to_ccyymmdd(bd)
	mat thc=(0)
	mat tcp=(0)
	adr=ta(1)
	do
	read #2,using L440,rec=adr: lpd,mat hc,mat cp,nta
	L440: form pos 42,n 6,pos 150,5*pd 3.2,pos 358,22*pd 5.2,pos 468,pd 3
	if lpd=ppd then 
		mat tcp=tcp+cp
		mat thc=thc+hc
	end if
	if nta=0 then goto L520
	adr=nta
	loop
L520: ! r:
	k$(1)=cnvrt$("PIC(#########)",ss)
	k$(2)=cnvrt$("PIC(#########.##)",tcp(12))
	k$(3)=cnvrt$("PIC(#########.##)",0)
	k$(4)=cnvrt$("PIC(#########.##)",tcp(13))
	k$(5)=cnvrt$("PIC(##)",0)
	k$(6)=cnvrt$("PIC(#######.##)",tcp(11))
	k$(7)=cnvrt$("PIC(##)",0)
	k$(8)=cnvrt$("PIC(#######.##)",0)
	k$(9)=em$
	k$(10)=em$(2)
	k$(12)=city$
	k$(13)=state$
	k$(14)=zip5$
	k$(15)=cnvrt$("PIC(########)",bd)
	k$(16)=cnvrt$("PIC(########)",em16)
	k$(17)=cnvrt$("PIC(########)",0)
	k$(18)=cnvrt$("PIC(########)",0)
	k$(19)=cnvrt$("PIC(######.##)",tcp(21))
	k$(20)=cnvrt$("PIC(########)",0)
	pr #4,using L730: mat k$
	L730: form pos 1,c 9,3*c 12,c 2,c 10,c 2,c 10,3*c 30,c 20,c 2,c 5,4*c 8,c 9,c 8
goto ReadPrAddr1 ! /r

CSZ: ! r: EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
	dim csz$*30
	csz$=uprc$(rtrm$(csz$))
L790: p1=pos(csz$,".",1)
	if p1>0 then csz$(p1:p1)="": goto L790
L810: p1=pos(csz$,"  ",1)
	if p1>0 then csz$(p1:p1)="": goto L810
	p1=pos(csz$,",",1)-1
	if p1=-1 then p1=pos(csz$," ",1)-1
	if csz$(p1+2:p1+2)><" " then csz$(p1:p1+1)=csz$(p1:p1+1)&" "
	p2=pos(csz$," ",p1+3)
	city$=uprc$(rtrm$(csz$(1:p1))(1:15))
	if city$(1:3)="FT " then city$(1:3)="FORT "
	if city$(1:3)="FT. " then city$(1:3)="FORT "
	state$=uprc$(rtrm$(csz$(p2-2:p2))(1:2))
	l1=len(csz$)
	zip$=uprc$(ltrm$(rtrm$(csz$(p2+1:l1))))
	zip5$=zip$(1:5)
	zip4$=""
	l2=len(zip$)
	if l2<9 then goto L980
	zip4$=zip$(l2-3:l2)
	L980: !
return ! /r
END1: ! 
	close #1: 
	close #2: 
	close #hpraddr: 
	close #4: 
L1050: pr newpage ! COPY TO DISKETTE
	fnopenwin(win=101,7,8,11,44,env$('program_caption'))
	io1$(1)="8,42,CU 1,UET,N"
	if driv=1 then pr #win,fields "3,2,Cc 30,n": "Drive Not Ready!"
	pr #win,fields "4,2,C 30,n": "Insert 401k Diskette in Drive:"
	pr f "12,15,C 9,B,1": "Next (F1)"
	pr f "12,26,C 11,B,5": "Cancel (F5)"
	if dv$="" then dv$="A"
L1130: rinput #win,fields "4,33,Cu 1,UET,N": dv$
	if cmdkey=5 then goto XIT
	if dv$="A" or dv$="B" then goto L1160 else goto L1130
L1160: execute "Copy [Q]\PRmstr\PR401K.DAT "&dv$&": -N" ioerr L1180
XIT: fnxit
L1180: !
	driv=1 ! drive not ready
goto L1050
include: Ertn
