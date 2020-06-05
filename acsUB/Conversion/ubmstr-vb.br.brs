! replace S:\acsUB\conversion\UBmstr-vb
! always start conversion process with bld_trans
def library fnub_cnv_ubmstr_vb
	autoLibrary
	on error goto Ertn
	dim b(11),a(7),d(15),alpha$*7,f2$*12,extra(23),extra$(11)*30,ba(12)
	dim custname$*30,badr(2)
	dim z$*10,e$(4)*30,f$(3)*12,c(4),g(12),adr(2),alp$*7,gb(10)
	dim x$*10,p$*10,rw4(22,13)
	fnStatus('Customer File Conversion (S:\acsUB\conversion\ubmstr-vb)')

	! fnCopy("[Q]\UBmstr\ubcoinfo.h[cno]","[Q]\UBmstr\Company.h[cno]",133) ! this should already be done.
	if exists("[Q]\UBmstr\ubMaster.h[cno]") then 
		fnFree("[Q]\UBmstr\customer.h[cno]")
		fnCopy("[Q]\UBmstr\ubMaster.h[cno]","[Q]\UBmstr\Customer.h[cno]")
		fnFree("[Q]\UBmstr\ubMaster.h[cno]")
	end if 
	fnCopy("[Q]\UBmstr\Customer.h[cno]","[Q]\UBmstr\Customer.h[cno]",2067)
	fnub_index_customer
	fnIndex("[Q]\UBmstr\UBAdrBil.h[cno]","[Q]\UBmstr\adrIndex.h[cno]","1 10")

	open #h_customer:=fnGetHandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",internal,outIn,keyed 
	if version(1)=1 then goto Xit
	open #h81:=fnGetHandle: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr,Use,RecL=80,KPs=1,KLn=10",internal,outIn,keyed 
	open #h82:=fnGetHandle: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr,Use,RecL=149",internal,outIn,relative 
	do
		ReadCustomer: !
		read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$ eof EoCustomer
		form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
		extra(1)=val(z$(1:2)) conv ReadCustomer ! route
		extra(2)=val(z$(3:7)) conv ReadCustomer ! sequence
		if extra(2)=0 then extra(2)=1 ! don't allow zero sequence #
		extra$(6)=f$(3) : f$(3)="" ! bulk sort code
		extra$(3)=f$(2) : f$(2)="" ! meter module #  (water meter serial)
		! eXTRA(11)=B(6): b(6)=0 ! note payable amount
		! extra(17)=c(4) : c(4)=0 ! final billing code
		! extra(3)=d(5) : d(5)=0 ! date meter read current
		! extra(4)=d(6): d(6)=0 ! date meter read prior
		! d(7)=0
		! eXTRA$(2)=STR$(D(8)): d(8)=0 ! phone #
		! d(11)=0 ! city bank account will be in bank draft program
		! dA$=STR$(D(12)): d(12)=0 ! customer bank account
		! eXTRA$(7)=STR$(D(15)): d(15)=0 ! test cycle code
		d(13)=0 ! set # units to 0 if not used xxx-prb 05/18/11
		d(14)=0 ! don't know but has value on old system
		! gB(5)=GB(3)+GB(4): gB(3)=GB(4)=0 ! add rep parts and rep labor together
		! If G(5)=9.25 Then g(6)=9.25: gB(6)=GB(5): g(5)=0: gB(5)=0 ! KINCAID ONLY
		! gB(9)=GB(7): gB(7)=0 ! move sfc for service 7 to service 9
		! eXTRA(18)=D(7): d(7)=0 ! average sewer usage
		! If env$('client')="Monticello" AND A(2)>9 Then eXTRA(18)=D(7)=0 ! don't average sewer rate codes 10 or greater
		! a(6)=A(7) ! penalty codes (was only 1 code but charges listed seperate
		! If A(2)>0 Then eXTRA(14)=D(13) ! make sewer units same as water units if have sewer
		for j=1 to udim(extra)
			if extra(j)<-99999 then extra(j)=0
		next j
		rewrite #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
		if b(10)<>0 then  ! updating the budget billing file
			mat badr=(0): ba(12)=b(10)
			write #h81,using 'form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3': z$,mat ba,mat badr
		end if
	loop
	EoCustomer: !
	version(h_customer,1)
	version(h81,1)
	version(h82,1)
	close #h_customer: ioerr ignore
	close #h81: ioerr ignore
	close #h82: ioerr ignore
	fnIndex("[Q]\UBmstr\BudMstr.h[cno]","[Q]\UBmstr\BudIdx1.h[cno]", '1 10')
	! L640: ! Goto 70
	Xit: !
fnend  ! chain "S:\acsUB\conversion\note-cnv" ! fnXit
include: Ertn