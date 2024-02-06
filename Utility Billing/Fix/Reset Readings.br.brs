! -- reset current or prior reading to what it was on a given transaction date.

autoLibrary
on error goto Ertn

dim x$*10,gb(10)
dim a(7),b(11),c(4),d(15),g(12)
dim p$*10,o(2),tg(11),meteradr$*30,custname$*30
dim extra(23)

fnTop(program$)
dim srvnam$(10)*20,srv$(10)*2
fnGetServices(mat srvnam$,mat srv$)

SCREEN1: !
	fnTos
	mylen=22 : mypos=mylen+2
	fnLbl(1,1,"Transaction Date (mmddyy):",mylen,1)
	fnTxt(1,mypos,8,0,1,"1001")      : resp$(1)='' ! '070611'
	fnLbl(3,1,"Reading to Reset:",mylen,1)
	fnOpt(3,mypos,"Current")         : resp$(2)='True'
	fnOpt(4,mypos,"Prior")           : resp$(3)='False'
	fnChk(6,mypos,srvnam$(1),1)      : resp$(4)='True'
	fnChk(7,mypos,srvnam$(4),1)      : resp$(5)='True'
	fnChk(9,mypos,"Update Usages",1) : resp$(6)='False'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d1=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
	if resp$(2)='True' then do_current=1 else do_current=0
	if resp$(4)='True' then do_water=1   else do_water=0
	if resp$(5)='True' then do_gas=1     else do_gas=0
	if resp$(6)='True' then do_usages=1  else do_usages=0
	execute "Index [Q]\UBmstr\UBTransVB.h[cno]"&' '&"[Q]\UBmstr\UTV_Date.h[cno] 11 8 Replace DupKeys -n"
	open #h_trans=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UTV_Date.h[cno],Shr",i,i,k
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k
	F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30

	restore #h_trans,key=str$(d1): nokey SCREEN1
	do
		read #h_trans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,_wr,wu,er,eu,gr,gu,tbal,pcode eof Xit
		if tdate<>d1 then goto Xit
		read #hCustomer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra nokey NEXT_ONE
		! if trim$(p$)='100002.00' then pause
		if do_water then fn_reading_fix(d(1),d(2),d(3),d(4),_wr) ! water
		if do_gas then fn_reading_fix(d(9),d(10),d(11),d(12),gr) ! gas
		rewrite #hCustomer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra
		NEXT_ONE: !
	loop
Xit: !
fnXit


def fn_reading_fix(&reading_current,&reading_prior,&usage_current,&usage_ytd,reading_new)
	rf_reading_prior=reading_prior
	rf_reading_cur=reading_current
	if do_current then
		rf_reading_cur=reading_new
	else
		rf_reading_prior=reading_new
	end if
	reading_current=rf_reading_cur
	reading_prior=rf_reading_prior
	if do_usages then
		usage_ytd-=usage_current
		usage_current=max(0,reading_current-reading_prior)
		usage_ytd+=usage_current
	end if  ! do_usages
fnend
include: ertn No