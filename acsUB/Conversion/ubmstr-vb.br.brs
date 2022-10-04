! replace S:\acsUB\conversion\UBmstr-vb
! always start conversion process with bld_trans
def library fnub_cnv_ubmstr_vb
	autoLibrary
	on error goto Ertn

	fnStatus('Customer File Conversion (S:\acsUB\conversion\ubmstr-vb)')

	! fnCopy('[Q]\UBmstr\ubcoinfo.h[cno]','[Q]\UBmstr\Company.h[cno]',133) ! this should already be done.
	if exists('[Q]\UBmstr\ubMaster.h[cno]') then
		fnFree('[Q]\UBmstr\customer.h[cno]')
		fnCopy('[Q]\UBmstr\ubMaster.h[cno]','[Q]\UBmstr\Customer.h[cno]')
		fnFree('[Q]\UBmstr\ubMaster.h[cno]')
	end if
	fnCopy('[Q]\UBmstr\Customer.h[cno]','[Q]\UBmstr\Customer.h[cno]',2067)
	fnub_index_customer
	fnIndex('[Q]\UBmstr\UBAdrBil.h[cno]','[Q]\UBmstr\adrIndex.h[cno]','1 10')

	open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]',i,outIn,k
	F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	if version(1)=1 then goto Xit
	open #h81=fnH: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr,Use,RecL=80,KPs=1,KLn=10',i,outIn,k
	h82=fnOpenBudTrans
	do
		ReadCustomer: !
		dim extra(23),extra$(11)*30
		dim a(7)
		dim xb(11)
		dim xd(15)
		dim gb(10)
		dim z$*10,e$(4)*30,f$(3)*12
		dim rw4(22,13)
		dim adr(2),alp$*7
		dim c(4),g(12)
		read #hCustomer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat xb,mat c,mat xd,bal,xf,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$ eof EoCustomer
		extra(1)=val(z$(1:2)) conv ReadCustomer ! route
		extra(2)=val(z$(3:7)) conv ReadCustomer ! sequence
		if extra(2)=0 then extra(2)=1 ! don't allow zero sequence #
		extra$(6)=f$(3) : f$(3)='' ! bulk sort code
		extra$(3)=f$(2) : f$(2)='' ! meter module #  (water meter serial)
		! eXTRA(11)=xB(6): xb(6)=0 ! note payable amount
		! extra(17)=c(4) : c(4)=0 ! final billing code
		! extra(3)=xd(5) : xd(5)=0 ! date meter read current
		! extra(4)=xd(6): xd(6)=0 ! date meter read prior
		! xd(7)=0
		! eXTRA$(2)=STR$(xd(8)): xd(8)=0 ! phone #
		! xd(11)=0 ! city bank account will be in bank draft program
		! dA$=STR$(xd(12)): xd(12)=0 ! customer bank account
		! eXTRA$(7)=STR$(xd(15)): xd(15)=0 ! test cycle code
		xd(13)=0 ! set # units to 0 if not used xxx-prb 05/18/11
		xd(14)=0 ! don't know but has value on old system
		! gB(5)=GB(3)+GB(4): gB(3)=GB(4)=0 ! add rep parts and rep labor together
		! If G(5)=9.25 Then g(6)=9.25: gB(6)=GB(5): g(5)=0: gB(5)=0 ! KINCAID ONLY
		! gB(9)=GB(7): gB(7)=0 ! move sfc for service 7 to service 9
		! eXTRA(18)=xd(7): xd(7)=0 ! average sewer usage
		! a(6)=A(7) ! penalty codes (was only 1 code but charges listed seperate
		! If A(2)>0 Then eXTRA(14)=xd(13) ! make sewer units same as water units if have sewer
		for j=1 to udim(extra)
			if extra(j)<-99999 then extra(j)=0
		next j
		rewrite #hCustomer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat xb,mat c,mat xd,bal,xf,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
		if xb(10)<>0 then  ! updating the budget billing file
			dim badr(2)
			dim ba(12)
			mat badr=(0) : ba(12)=xb(10)
			write #h81,using 'form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3': z$,mat ba,mat badr
		end if
	loop
	EoCustomer: !
	version(hCustomer,1)
	version(h81,1)
	version(h82,1)
	close #hCustomer: ioerr ignore
	close #h81: ioerr ignore
	close #h82: ioerr ignore
	fnIndex('[Q]\UBmstr\BudMstr.h[cno]','[Q]\UBmstr\BudIdx1.h[cno]', '1 10')
	! L640: ! Goto 70
	Xit: !
fnend  ! chain 'S:\acsUB\conversion\note-cnv' ! fnXit
include: ertn