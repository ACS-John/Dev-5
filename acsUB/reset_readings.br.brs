! Replace S:\acsUB\reset_readings
! -- reset current or prior reading to what it was on a given transaction date.
 
	autoLibrary
	on error goto Ertn
 
	dim x$*10,x(15),w(5),r(4),gb(10),rt(10,3),ba(13),da(2),txt$(3)*80,txt$*50
	dim a(7),b(11),c(4),d(15),g(12),rw(22,13),d$*6,dat$*20,bt1(14,2)
	dim p$*10,o(2),bt2(14,2),badr(2),dp$*60,tg(11),transkey$*19,meteradr$*30,custname$*30
	dim watuse(12),watdat(12),elecuse(12),elecdat(12),gasuse(12),gasdat(12)
	dim serviceName$(10)*20,serviceCode$(10)*2,tax_code$(10)*1,work$*80
	dim penatly$(10)*1,subjectto(10)
	dim extra(23),extra$(11)*30,client$*30
	dim cap$*128,work$*80,work_addr$*80
 
	fnTop("S:\acsUB\reset_readings",cap$="Reset Readings")
	dim srvnam$(10)*20,srv$(10)*2
	fnget_services(mat srvnam$,mat srv$)
 
SCREEN1: !
	fnTos(sn$='resetreadings3')
	mylen=22 : mypos=mylen+2
	fnLbl(1,1,"Transaction Date (mmddyy):",mylen,1)
	fnTxt(1,mypos,8,0,1,"1001") : resp$(1)='' ! '070611'
	fnLbl(3,1,"Reading to Reset:",mylen,1)
	fnOpt(3,mypos,"Current") : resp$(2)='True'
	fnOpt(4,mypos,"Prior") : resp$(3)='False'
	fnChk(6,mypos,srvnam$(1),1) : resp$(4)='True'
	fnChk(7,mypos,srvnam$(4),1) : resp$(5)='True'
	fnChk(9,mypos,"Update Usages",1) : resp$(6)='False'
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck=5 then goto Xit
	d1=fndate_mmddyy_to_ccyymmdd(val(resp$(1)))
	if resp$(2)='True' then do_current=1 else do_current=0
	if resp$(4)='True' then do_water=1 else do_water=0
	if resp$(5)='True' then do_gas=1 else do_gas=0
	if resp$(6)='True' then do_usages=1 else do_usages=0
	execute "Index [Q]\UBmstr\UBTransVB.h[cno]"&' '&"[Q]\UBmstr\UTV_Date.h[cno] 11 8 Replace DupKeys -n"
	open #h_trans=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UTV_Date.h[cno],Shr",internal,input,keyed
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
 
	restore #h_trans,key=str$(d1): nokey SCREEN1
	do
		read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,_wr,wu,er,eu,gr,gu,tbal,pcode eof Xit
		if tdate<>d1 then goto Xit
		read #h_customer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra nokey NEXT_ONE
! if trim$(p$)='100002.00' then pause
		if do_water then let fn_reading_fix(d(1),d(2),d(3),d(4),_wr) ! water
		if do_gas then let fn_reading_fix(d(9),d(10),d(11),d(12),gr) ! gas
! <Updateable Region: ERTN>
		rewrite #h_customer,using F_CUSTOMER,key=p$: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra
NEXT_ONE: !
	loop
Xit: !
	fnXit
 
include: Ertn No
 
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
fnend  ! fn_reading_fix
