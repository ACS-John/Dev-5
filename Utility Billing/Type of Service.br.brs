fn_setup
dim resp$(60)*20
dim serviceName$(10)*20
dim serviceCode$(10)*2
dim taxCode$(10)*1

fnTop(program$)
fn_readService
fnTos ! r:
	fnLbl(2,13,"Full Name",20,2)
	fnLbl(2,34,"Code",4,0)
	fnLbl(2,39,"Taxable",7,0)
	fnLbl(2,47,"Penalty",7,0)
	fnLbl(1,55,"Subject",7,2)
	fnLbl(2,55,"To",7,2)
	fnLbl(1,66,"Order to apply",14,2)
	fnLbl(2,66,"Collection",14,2)
	for a=1 to 10
		fnLbl(a+2,1,"Service "&str$(a)&":",11,1)
		resp$(a*6-5)=serviceName$(a)
		fnTxt(a+2,13,20)
		resp$(a*6-4)=serviceCode$(a)
		fnTxt(a+2,34,3)
		fnChk(a+2,41,"",align=0,container=0,tabcon=0)
		if taxCode$(a)="Y" then resp$(a*6-3)='True' else resp$(a*6-3)='False'
		fnChk(a+2,49,"",align=0,container=0,tabcon=0)
		if penalty$(a)="Y" then resp$(a*6-2)='True' else resp$(a*6-2)='False'
		resp$(a*6-1)=str$(subjectTo(a))
		fnTxt(a+2,58,2,0,0,'30')
		resp$(a*6)=str$(orderToApply(a))
		fnTxt(a+2,72,2,0,0,'30')
	next a
	fnCmdSet(4)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		for a=1 to 10
			serviceName$(a)=resp$(a*6-5)
			serviceCode$(a)=uprc$(resp$(a*6-4))
			if resp$(a*6-3)='True' then
				taxCode$(a)="Y"
			else
				taxCode$(a)="N"
			end if
			if resp$(a*6-2)='True' then
				penalty$(a)="Y"
			else
				penalty$(a)="N"
			end if
			subjectTo(a)=val(resp$(a*6-1))
			orderToApply(a)=val(resp$(a*6))
		next a
		open #hService=fnH: "Name=[Q]\UBmstr\ubData\Service.h[cno],RecL=280,use",i,outi,r
		rewrite #hService,using F_service,rec=1: mat serviceName$,mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply
		close #hService:
	end if
goto Xit ! /r
Xit: fnXit

def fn_readService
	if readServiceSetup<>val(env$('cno')) then
		readServiceSetup=val(env$('cno'))
		dim cacheServiceName$(10)*20
		dim cacheServiceCode$(10)*2
		dim cacheTaxCode$(10)*1
		dim cacheePenalty$(10)*1
		dim cacheSubjectTo(10)
		dim cacheOrderToApply(10)
		mat cacheServiceName$ =('')
		mat cacheServiceCode$ =('')
		mat cacheTaxCode$     =('')
		mat cacheePenalty$    =('')
		mat cacheSubjectTo    =(0)
		mat cacheOrderToApply =(0)
		if ~exists('[Q]\UBmstr\ubData\Service.h[cno]') and exists ('[Q]\UBmstr\ubData\Service.h1') then
			if exists('[Q]\UBmstr\ubData\RateMst.h[cno]') then 
				! rate file exists   so   just copy the service file from  from company 1
				fnCopy('[Q]\UBmstr\ubData\Service.h1','[Q]\UBmstr\ubData\Service.h[cno]')
			else
				! rate file does NOT exists   so   copy the service file, rate file and indexes from company 1
				fnCopy('[Q]\UBmstr\UBData\*.h1','[Q]\UBmstr\UBData\*.h[cno]')
			end if
		else if ~exists('[Q]\UBmstr\ubData\Service.h[cno]') then
			fnCopy('S:\Utility Billing\mstr\ubData\*.h99999','[Q]\UBmstr\UBData\*.h[cno]')
		end if
		open #hService=fnH: "Name=[Q]\UBmstr\ubData\Service.h[cno],RecL=280,use",i,outi,r
		if lrec(hService)<1 then !  this should not happen because it should be copied in from company #1 above
			write #hService,using F_service,rec=1: mat cacheServiceName$,mat cacheServiceCode$,mat cacheTaxCode$,mat cacheePenalty$,mat cacheSubjectTo,mat cacheOrderToApply
			pr 'A new empty Type of Service file was created.  Only ACS can edit this file type.  Type GO and press Enter to continue.' : pause
			
		end if
		read #hService,using F_service,rec=1: mat cacheServiceName$,mat cacheServiceCode$,mat cacheTaxCode$,mat cacheePenalty$,mat cacheSubjectTo,mat cacheOrderToApply
		F_service: form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*n 2,10*n 2
		close #hService:
	end if
	mat serviceName$(udim(mat cacheServiceName$)) : mat serviceName$=cacheServiceName$
	if fnArrayWasPassedC( mat serviceCode$) then mat serviceCode$(udim(mat cacheServiceCode$)) : mat serviceCode$=cacheServiceCode$
	if fnArrayWasPassedC( mat taxCode$    ) then mat taxCode$    (udim(mat cacheeTaxCode$   )) : mat taxCode$    =cacheTaxCode$
	if fnArrayWasPassedC( mat penalty$    ) then mat penalty$    (udim(mat cacheePenalty$   )) : mat penalty$    =cacheePenalty$
	if fnArrayWasPassedN( mat subjectTo   ) then mat subjectTo   (udim(mat cacheSubjectTo   )) : mat subjectTo   =cacheSubjectTo
	if fnArrayWasPassedN( mat orderToApply) then mat orderToApply(udim(mat cacheOrderToApply)) : mat orderToApply=cacheOrderToApply
fnend
def library fnGetServices(mat serviceName$; mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
	fn_setup
	fnGetServices=fn_get_services(mat serviceName$, mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
fnend
def fn_get_services(mat serviceName$; mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
	fn_readService
fnend
def library fnGetServiceCodesMetered(mat serviceCodeMetered$)
	fn_setup
	fn_readService
	mat serviceCodeMetered$(0)
	for srv_item=1 to udim(mat serviceCode$)
		if (srv_item=1 and trim$(serviceCode$(srv_item))<>'') or (serviceName$(srv_item)="GAS" or serviceCode$(srv_item)="GA") or serviceCode$(srv_item)='EL' or serviceName$(srv_item)="Lawn Meter" then ! if it is a metered service
			fnAddOneC(mat serviceCodeMetered$,serviceCode$(srv_item))
			! pr 'found metered service: '&serviceCode$(srv_item)
		end if
	next srv_item
fnend
def library fnservice_other
	fn_setup
	fnservice_other=fn_service_other
fnend
def fn_service_other
	!  this function returns en enumeration of the OTHER balance breakdown service
	!   also returns  mat serviceName$ and mat srv$
	dim so_serviceName$(10)*20,so_srv$(10)*2
	fn_get_services(mat so_serviceName$,mat so_srv$)
	so_return=srch(mat so_srv$,'OT')
	if so_return<=0 then so_return=srch(mat so_srv$,'OC') ! Other Charge
	if so_return<=0 then so_return=srch(mat so_serviceName$,'Other') ! Other Charge
	if so_return<=0 then so_return=srch(mat so_serviceName$,rpad$('Other',20)) ! Other Charge
	if so_return<=0 then
		pr "OT (Other) nor OC (Other Charge) not found in Service Code abbreviations"
		pr "(nor was Other found in Service Code names"
		so_return=0
		pause
	end if
	fn_service_other=so_return
fnend
include: fn_setup
