! modifies customer records depending upon how the program is configured, routes services, etc
! r: setup library, dims, on err, fnTop, etc
	autoLibrary
	on errror goto ERTN
	fnTop(program$)
 ! /r
	! gosub OldWorkFromFixedWidthList
	! r: primary loop setup
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7,gb(10),extra$(11)*30
	dim extra(23)
	dim df$*1
	open #h_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno]",i,outi,r
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1712,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	! /r
	do ! r: primary loop
		read_count+=1
		read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ eof PrimaryFinis
		didChange=0
		didChange+=fn_divideRoutesByTen(extra(1))
		didChange+=fn_servicesRateCodeAdjust
		if didChange then
			rewrite #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$
			write_count+=1
		end if
	loop ! /r
	PrimaryFinis: !
		pr 'read_count=';read_count
		pr 'write_count=';write_count : pause
	Xit: fnXit
include: ertn
OldWorkFromFixedWidthList: ! r: change route and sequence numbers from a text file
	dim ln$*128
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=chg\newrouteseq.txt",display,input
	fnopenprn
	READ_CUSTOMER: !
	linput #2: ln$ eof OWFFL_Finis
	! z$=LPAD$(RTRM$(LN$(17:26)),10)
	z$=lpad$(rtrm$(ln$(1:9)),10)
	! rOUTE=VAL(LN$(1:7))
	route=val(ln$(81:87))
	! sEQUENCE=VAL(LN$(9:15))
	sequence=val(ln$(73:79))
	read #1,using "Form POS 1,c 10,pos 1741,n 2,pos 1743,n 7",key=z$: oldz$,oldroute,oldsequence nokey L250
	rewrite #1,using "Form pos 1741,n 2,pos 1743,n 7": route,sequence
	goto READ_CUSTOMER
 
	L250: !
	pr #255,using "form pos 1,c 50": "Account "&z$&" not found"
	goto READ_CUSTOMER
	OWFFL_Finis: !
	fncloseprn
	close #1:
	fnub_index_customer ! execute "Index [Q]\UBmstr\Customer.h[cno]"&' '&"[Q]\UBmstr\UBIndx5.h[cno] 1741/1743 2/7 Replace DupKeys -n"
return ! /r
def fn_divideRoutesByTen(&routeNumber)
	drbtReturn=0
	if len(str$(routeNumber))<>2 then
		pr bell;'route number ('&str$(routeNumber)&') wrong length to convert.'
		pause
	else if str$(routeNumber)(2:2)<>'0' then
		pr bell;'route number ('&str$(routeNumber)&') does not end with a zero.'
		pause
	else
		routeNumber=routeNumber/10
		drbtReturn=1
	end if
	fn_divideRoutesByTen=drbtReturn
fnend
def fn_servicesRateCodeAdjust
	srcaReturn=0
	! r: Sanitation Standard Charge - they should all be ZERO
	!   b(5)=0
	!   srcaReturn=1
	! /r
	! r: super dynamic - just change the code LOL
		! if a(4)=2 then ! service 4 (gas) has a rate code of 2 then
		! if a(2)>0 then ! if Serive 2 has a rate code
		if a(1)>0 then ! if water (service 1) rate code (any)
			! a(3)=1     ! Service 3 (Electric or Lawn Meter) – Rate Code
			extra(11)=1 ! service 6 (fire protection) gets a rate code of 1
			! a(6)=2     ! service 9 (sales tax) gets a rate code of 2
		else
			extra(11)=9
		end if
		srcaReturn=1 ! tells the calling routine to write the record
	! /r
	fn_servicesRateCodeAdjust=srcaReturn
fnend
 
