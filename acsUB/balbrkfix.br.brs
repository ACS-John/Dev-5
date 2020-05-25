! (foundone is special for montincello. Take out refereence to foundone on any others,, they have 3 cycles.
	autoLibrary
	form pos 1,c 9,skip 0
	fnTop(program$,"Fix Balance Breakdown")
	on error goto Ertn
	dim dat$*20,ln$*132,sde$*30,cb(13),a$(61)*30,u(61),gb(10),a(7)
	dim o(2),alloc(10),g(10),answer(10,3)
	dim adr(2),gb(10),tgb(10),a$(61)*30,u(61)
	dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
	dim t1$(11)*25,ta(2),t2$(4)*25,dt1(31),dt2(31)
	fnopenprn
	gosub SCR1
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
	fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto,mat apply)
	for j=1 to 10
		serviceName$(j)=lpad$(rtrm$(serviceName$(j)(1:8)),8)
	next j
	gosub NWPGE
L270: read #1,using L280: z$,mat a,bal,mat g,mat gb eof DONE
L280: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
	foundone=0

	if bal=sum(gb) then goto L270
	if bal=0 then mat gb=(0): cause$="one": goto Xrew
	if bal<0 and a(1)>0 then mat gb=(0) : gb(1)=bal : cause$="two": goto Xrew
	if bal<0 and a(2)>0 then mat gb=(0) : gb(2)=bal : cause$="three": goto Xrew
	if bal<0 and a(3)>0 then mat gb=(0) : gb(3)=bal : cause$="three": goto Xrew
	if bal<0 and a(4)>0 then mat gb=(0) : gb(4)=bal : cause$="three": goto Xrew
	if bal<0 then mat gb=(0) : gb(5)=bal : cause$="three": goto Xrew ! if credit balance and no water,sewer,elec,or gas then credit show in gb(5)
	runtotal=0
	mat gb=(0)
	for j=1 to 10 ! types of charges  (OWE CURRENT BILL)
		if penalty$(j)="Y" and tcode=1 then goto L450 ! SKIP PENALTY RECORDS
		gb(j)=min(g(j),(bal-runtotal))
		if sum(gb)=bal then goto Xrew
		runtotal=runtotal+gb(j)
L450: next j
	if sum(gb)= bal then goto Xrew
	restore #2,key>=z$&"         ": nokey L570 ! FIND IN TRANSACTION HISTORY
	mat answer=(0): mat gb=(0): runtotal=0
L490: read #2,using L500: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L650
L500: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>z$ then goto L650 : _
		! history record must belong to this customer
	if tcode=1 or tcode=2 then goto L530 else goto L490 ! charge and penalty transaction
L530: for j=1 to 3
		if str$(tdate)=resp$(j) then foundone=1: goto L570
		if str$(tdate)=resp$(j+3) then goto L570 ! penalties
		goto L630
L570: if sum(gb)= bal then goto Xrew
		for j2=1 to 10
! If PENALTY$(J2)="Y" AND TCODE=1 Then Goto 570 ! SKIP PENALTY RECORDS
			answer(j2,j)+=tg(j2)
		next j2
		goto L490 ! read next transaction
L630: next j
	goto L490
L650: x=1
L660: for k=1 to 10
		if apply(k)=0 then goto L710
		gb(k)+=min(answer(k,x),max(0,(bal-runtotal)))
		if sum(gb)=bal then goto Xrew
		runtotal=runtotal+min(answer(k,x),max(0,(bal-runtotal)))
L710: next k
	if x<3 then x+=1: goto L660
	cause$="four"
	for z=1 to 10
		if trim$(service$(z))<>"" then gb(z)+=bal-runtotal: pr #255,using "form pos 1,c 50": "Plugging first service by "&str$(bal-runtotal): goto L780 ! plug anything LEFT OVER TO first service  (change <>"" to ="PN" to plug to penalty
	next z
! Next J
L780: if foundone=0 then goto L820 ! one of dates does not match the last billing date don't change
	goto Xrew
L800: pr #255,using L810: z$,bal,sum(gb),mat gb
L810: form pos 1,c 10,x 1,12*n 9.2
L820: goto L270
NWPGE: ! pr #255: NEWPAGE
	pr #255: "Account    Balance   TotBrk   "&serviceName$(1)(1:8)&" "&serviceName$(2)(1:8)&" "&serviceName$(3)(1:8)&" "&serviceName$(4)(1:8)&" "&serviceName$(5)(1:8)&" "&serviceName$(6)(1:8)&" "&serviceName$(7)(1:8)&" "&serviceName$(8)(1:8)&" "&serviceName$(9)(1:8)&" "&serviceName$(10)(1:8)&" "
	pr #255: "__________ _________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________"
return
DONE: fncloseprn
Xit: fnXit
Xrew: !
	rewrite #1,using L910,key=z$: mat gb
	L910: form pos 388,10*pd 5.2
	! pr #255: "rewrote next record "&CAUSE$
	goto L800
SCR1: !
	fnTos
	mylen=62 : mypos=50
	fnLbl(1,1,"Billing Dates for last three months:",mylen,1)
	for j=1 to 3
		fnTxt(j+1,mypos,10,0,0,"3",0,"Put your most recent billing date first and then in order from there.")
		resp$(j)=""
	next j
	fnLbl(5,1,"Penalty Dates for last three months:",mylen,1)
	for j=1 to 3
		fnTxt(j+5,mypos,10,0,0,"3",0,"Put your most recent penalty date first and then in order from there.")
		resp$(j+3)=""
	next j
	fnCmdSet(2): fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 6
		L1030: !
		x=pos(resp$(j),"/",1)
		if x>0 then resp$(j)(x:x)="": goto L1030
	next j
	for j=1 to 6 
		cd1(j)=val(resp$(j)) conv SCR1 
	next j
	if cd1(1)=0 then 
		mat message$(1)
		message$(1)="You must enter at least one date!" 
		fnmsgbox(mat message$,resp$,'',0) 
		goto SCR1
	end if
return
include: Ertn