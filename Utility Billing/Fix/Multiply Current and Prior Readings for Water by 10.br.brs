! -- for all billing dates that match - put the lower of prior/current water readings into prior and the higher into current

	autoLibrary
	on error goto Ertn

	dim x$*10,x(15),w(5),r(4),gb(10),rt(10,3),ba(13),da(2),txt$(3)*80,txt$*50
	dim a(7),xb(11),c(4),d(15),g(12)
	dim p$*10,tg(11),transkey$*19,meteradr$*30,custname$*30
	dim serviceName$(10)*20,serviceCode$(10)*2,tax_code$(10)*1,work$*80
	dim penatly$(10)*1,subjectto(10)
	dim extra(23),extra$(11)*30,client$*30
	dim work$*80,work_addr$*80

	fnTop(program$,"Swap Current and Prior Readings for Water")
	fnLastBillingDate(d1)
	fnGetServices(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
	for j=1 to udim(serviceName$)
		serviceName$(j)=trim$(serviceName$(j))
	next j
!
	fnTos(sn$='SwapCurPri1')
	mylen=22 : mypos=mylen+2
	fnLbl(1,1,"Billing Date (mmddyy):",mylen,1)
	fnTxt(1,mypos,8,0,1,"1001")
	resp$(1)=str$(d1)
L440: !
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=val(resp$(1))
	open #customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k
F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
!
	do
		read #customer,using F_CUSTOMER: meteradr$,custname$,mat a,mat xb,mat c,mat d, bal,f,mat g,mat gb,mat extra eof Xit
		! if f=d1 then ! else recalculation reduce balances
			water_reading_prior=min(d(1),d(2))
			water_reading_cur=max(d(1),d(2))
			d(1)=d(1)*10
			d(2)=d(2)*10
			rewrite #customer,using F_CUSTOMER: meteradr$,custname$,mat a,mat xb,mat c,mat d,bal,f,mat g,mat gb,mat extra
		! end if  ! f=d1
	loop
Xit: !
	fnXit
!
include: ertn No
!
