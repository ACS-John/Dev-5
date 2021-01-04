! formerly S:\acsUB\ubRate
! -- Rate File editor
fn_setup
fnTop(program$)
open #hRate1=fnH: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Use,RecL=374,KPs=1,KLn=4,Shr",internal,outIn,keyed 
open #hRate2=fnH: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx2.h[cno],Use,RecL=374,KPs=5,KLn=25,Shr",internal,outIn,keyed 
goto ScreenGrid ! program starts with flex grid of all rates currently in file
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		! 
		dim k$*25,k$(20)*25,rt$(35)*50,option$(10),msgline$(5)*40,snm$(10)*20
		dim item$(4)*30,resp$(40)*50,resp$*50,resp$(35)*50
		! 
		on error goto Ertn
		!
		fnGetServices(mat snm$,mat srv$)
		dim serviceCodeMetered$(0)*2
		fnGetServiceCodesMetered(mat serviceCodeMetered$)
		optionCount=0
		for j=1 to 10
			if trim$(snm$(j))<>"" then option$(optionCount+=1)=srv$(j)
		next j
		mat option$(optionCount)
		!
		library 'S:\Core\Library': fnFileioEnums
		dim fioEnum$(1)*1024 ! "The maximum length of a BR statement is now 2000 characters" -brwiki
		fnFileioEnums('UB Customer',mat fioEnum$)
		for index=1 to udim(mat fioEnum$)
			execute fioEnum$(index)
		next index
	end if
fnend
ScreenGrid: ! r:
	fnTos
	myline=1 : mypos=1 : height=10 : width=50
	colhdr$(1)="Code"
	colhdr$(2)="Description"
	colhdr$(3)="Minimum"
	colhdr$(4)="1st Rate"
	mat colhdr$(4)
	colmask$(1)=colmask$(2)=""
	colmask$(3)="10"
	colmask$(4)="36"
	mat colmask(4)
	fnflexinit1("ubrate",myline,mypos,height,width,mat colhdr$,mat colmask$,1)
	restore #hRate1: 
	do 
		read #hRate1,using 'Form POS 1,C 2,G 2,C 50,32*G 10': mat rt$ eof L1010
		item$(1)=rt$(1)&rt$(2)
		item$(2)=rt$(3)(1:30)
		item$(3)=rt$(4)
		item$(4)=rt$(8)
		fnflexadd1(mat item$)
	loop 
L1010: ! 
	fnLbl(11,50,"",0,0)
	fnCmdKey("Edit",1,1,0,"Allows you to access the record that is highlited")
	fnCmdKey("&Add",2,0,0,"Add new rates")
	fnCmdKey("&Delete",4,0,0,"Deletes highlighted record")
	fnCmdKey("&Print",3,0,0,"Prints rate file proof list")
	fnCmdKey("&Complete",5,0,1,"Return to menu")
	fnAcs(mat resp$,ckey) ! CALL FLEXGRID
	k$=rpad$(resp$(1),4)
	if ckey=5 then 
		goto Xit
	else if ckey=2 then 
		goto AddNewRecord
	else if ckey=3 then 
		gosub PrintProof
	else if ckey=1 then 
		goto RateEdit
	else if ckey=4 then 
		gosub DeleteRec
	end if 
	goto ScreenGrid
	goto Xit
! /r
DeleteRec: ! r:
	mat msgline$(2)
	msgline$(1)="Delete this rate record?"
	msgline$(2)=k$ ! rt$(1)&rt$(2)
	fnmsgbox(mat msgline$,resp$,'',36)
	if uprc$(resp$)(1:1)="Y" then 
		delete #hRate1,key=k$: ! rt$(1)&rt$(2):
	end if 
return  ! /r
AddNewRecord: ! r:
	mat rt$=("")
	fnTos
	mat resp$=("")
	fnLbl(1,1,"Service Type:",20,1)
	fnLbl(1,29,"Rate Code:",10,1)
	fncomboa("rate_type",1,22,mat option$,"All codes must be between 1 and 99",2)
	resp$(1)=""
	fnTxt(1,40,2,0,0,"",0,"All codes must be between 1 and 99")
	fnCmdSet(2)
	fnAcs(mat resp$,ckey) ! CALL ADD NEW RECORD
	if ckey=5 then goto ScreenGrid
	rt$=uprc$(resp$(1)) ! service type
	if rtrm$(rt$)="" then 
		mat msgline$(1)
		msgline$(1)="Invalid Service Type"
		fnmsgbox(mat msgline$,resp$,'',16)
		goto AddNewRecord
	end if 

	g1=0 : g1=val(resp$(2)) conv ignore ! rate code
	if g1=0 then 
		mat msgline$(1)
		msgline$(1)="Rate codes must be from 1 to 99!"
		fnmsgbox(mat msgline$,resp$,'',16)
		goto AddNewRecord
	end if 

	rt$=rt$
	mat rt$=("")
	rt$(1)=rt$
	for j=1 to udim(option$)
		if rt$(1)=option$(j) then goto ANR_SERVICE_TYPE_IS_VALID
	next j
	mat msgline$(1)
	msgline$(1)="Invalid Service Type"
	fnmsgbox(mat msgline$,resp$,'',16)
	goto AddNewRecord
	ANR_SERVICE_TYPE_IS_VALID: ! 

	rt$(2)=lpad$(str$(g1),2)
	k$=rt$(1)&rt$(2)
	read #hRate1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$ nokey L470
	goto RateEdit ! existing record
	L470: ! 
	write #hRate1,using 'Form POS 1,C 2,G 2,C 50,32*G 10': mat rt$
goto RateEdit ! /r
RateEdit: ! r: maintain rate file
	read #hRate1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$ nokey ignore
	fnTos
	c1=20 : c2=32 : c3=44
	fnLbl(1,1,"Service Type:",20,1)
	! fncomboa("ubrate3",1,22,mat option$,"All codes must be between 1 and 99",2)
	fnTxt(1,22,2,0,0,"",1)
	fnLbl(1,29,"Rate Code:",10,1)
	fnTxt(1,40,2,0,0,"30",1)

	fnLbl(2,1,"Description:",20,1)
	fnTxt(2,22,50)
	fnLbl(3,1,"Minimum Charge:",20,1)
	fnTxt(3,22,9,0,1,"32")
	fnLbl(3,30,"Minimum Usage:",20,1)
	fnTxt(3,51,9,0,1,"30")

	fnLbl(5,c1,"Usage",9,2)
	fnLbl(5,c2,"Usage",9,2)
	fnLbl(5,c3," Charge",9,2)
	fnLbl(6,c1," From",9,2)
	fnLbl(6,c2," To",9,2)
	fnLbl(6,c3,"Per Unit",9,2)
	! x=7
	for lin=6 to 15
		fnTxt(lin+1,c1,9,10,1,"30",0) ! 0 decimal
		fnTxt(lin+1,c2,9,10,1,"30",0)
		fnTxt(lin+1,c3,9,9,1,"36")
		! x=x+3
	next lin
	fnCmdSet(4)
	mat resp$(udim(mat rt$))
	mat resp$=rt$
	fnAcs(mat resp$,ckey) !        ! CALLS RATE MAINTENANCE
	mat resp$(udim(mat rt$))
	mat rt$=resp$
	if ckey=5 then goto ScreenGrid

	rt$(1)=uprc$(rt$(1)) ! service type
	if rtrm$(rt$(1))="" then 
		mat msgline$(1)
		msgline$(1)="Invalid Service Type: "&rt$(1)
		fnmsgbox(mat msgline$,resp$,'',16)
		goto RateEdit
	end if 
	g1=0 : g1=val(rt$(2)) conv ignore ! rate code
	if g1=0 then 
		mat msgline$(1)
		msgline$(1)="Rate codes must be from 1 to 99!"
		fnmsgbox(mat msgline$,resp$,'',48)
		goto RateEdit
	end if 
	for j=1 to udim(option$)
		if rt$(1)=uprc$(option$(j)) then goto RM_SERVICE_TYPE_VALID
	next j
	mat msgline$(1)
	msgline$(1)="Invalid Service Type: "&rt$(1)
	fnmsgbox(mat msgline$,resp$,'',16)
	RM_SERVICE_TYPE_VALID: ! 
	rt$(2)=lpad$(str$(g1),2)
	k$=rt$(1)&rt$(2)
	rewrite #hRate1,using 'Form POS 1,C 2,G 2,C 50,32*G 10',key=k$: mat rt$
goto ScreenGrid ! /r
PrintProof: ! r:
	fnTos
	fnOpt(1,14,"Code Sequence")
	resp$(1)="True"
	fnOpt(2,14,"Name Sequence")
	resp$(2)="False"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey) ! CALLS PROOF LIST
	if ckey=5 then goto ScreenGrid
	ti2=hRate1 ! default to code sequence
	if uprc$(resp$(1))=uprc$("True") then ti2=hRate1: k$="    " ! code sequence
	if uprc$(resp$(2))=uprc$("True") then ti2=hRate2: k$=rpt$(chr$(0),25) ! name sequence
	restore #ti2,key>=k$: ! Nokey ScreenGrid
	fnopenprn
	pg=0
	do 
		read #ti2,using 'Form POS 1,C 2,G 2,C 50,32*G 10',release: mat rt$ eof PRINT_PROOF_FINIS
		gosub PrintOneRate
	loop 
	PRINT_PROOF_FINIS: ! 
	fncloseprn
return  ! /r
PrintOneRate: ! r:
	pr #255: "Service Code: ";rt$(1);"   Rate Code: ";rt$(2)
	pr #255: "Description: ";rt$(3)
	pr #255: "Minimum Charge: ";ltrm$(rt$(4));"   Minimum Usage: ";ltrm$(rt$(5))
	pr #255: ""
	pr #255: "Usage From     Usage To    Rate Per Unit"
	pr #255: "__________    __________   _____________"
	for j=1 to udim(option$)
		if trim$(rt$(j*3+3))<>"" or trim$(rt$(j*3+4))<>"" or trim$(rt$(j*3+5))<>"" then 
			if trim$(rt$(j*3+3))<>"0" or trim$(rt$(j*3+4))<>"0" or trim$(rt$(j*3+5))<>"0" then 
				pr #255,using 'Form POS 1,3*C 14': rt$(j*3+3),rt$(j*3+4),rt$(j*3+5)
			end if 
		end if 
	next j
	pr #255: ""
	pr #255: ""
	if pg+=1<3 then goto P1R_FINIS
	pr #255: newpage
	pg=0
	P1R_FINIS: ! 
return  ! /r
Xit: fnXit
def library fnapplyDefaultRatesFio(mat customerN)
	if ~setup then fn_setup
	fnapplyDefaultRatesFio=fn_applyDefaultRatesFio(mat customerN)
fnend 
def fn_applyDefaultRatesFio(mat customerN)
	! r: get legacy from fileio variables
	dim extra(23),a(7)
	mat extra=(0)
	extra(1 )=customerN(c_route            )
	extra(2 )=customerN(c_sequence         )
	extra(3 )=customerN(c_meterReadDateCur )
	extra(4 )=customerN(c_meterReadDatePri )
	extra(5 )=customerN(c_sewerReduction   )
	extra(6 )=customerN(c_s03securityLight )
	extra(7 )=customerN(c_s03lightCount    )
	extra(8 )=customerN(c_s03multiplier    )
	extra(9 )=customerN(c_extra_09N        )
	extra(10)=customerN(c_s04multiplier    )
	extra(11)=customerN(c_s06rate          )
	extra(12)=customerN(c_s07rate          )
	extra(13)=customerN(c_s08rate          )
	extra(14)=customerN(c_s02units         )
	extra(15)=customerN(c_s03units         )
	extra(16)=customerN(c_s04units         )
	extra(17)=customerN(c_finalBilling     )
	extra(18)=customerN(c_s02averageUsage  )
	extra(19)=customerN(c_estimationDate   )
	extra(20)=customerN(c_unused09         )
	extra(21)=customerN(c_unused10         )
	extra(22)=customerN(c_enableAltBillAddr)
	extra(23)=customerN(c_unused11         )
	a(1)=customerN(c_s01rate)
	a(2)=customerN(c_s02rate)
	a(3)=customerN(c_s03rate)
	a(4)=customerN(c_s04rate)
	a(5)=customerN(c_s05rate)
	a(6)=customerN(c_s09rate)
	a(7)=customerN(c_s10rate)

	! /r
	fnapply_default_rates(mat extra, mat a)
	! r: put legacy back in fileio variables
	customerN(c_route            )=extra(1 )
	customerN(c_sequence         )=extra(2 )
	customerN(c_meterReadDateCur )=extra(3 )
	customerN(c_meterReadDatePri )=extra(4 )
	customerN(c_sewerReduction   )=extra(5 )
	customerN(c_s03securityLight )=extra(6 )
	customerN(c_s03lightCount    )=extra(7 )
	customerN(c_s03multiplier    )=extra(8 )
	customerN(c_extra_09N        )=extra(9 )
	customerN(c_s04multiplier    )=extra(10)
	customerN(c_s06rate          )=extra(11)
	customerN(c_s07rate          )=extra(12)
	customerN(c_s08rate          )=extra(13)
	customerN(c_s02units         )=extra(14)
	customerN(c_s03units         )=extra(15)
	customerN(c_s04units         )=extra(16)
	customerN(c_finalBilling     )=extra(17)
	customerN(c_s02averageUsage  )=extra(18)
	customerN(c_estimationDate   )=extra(19)
	customerN(c_unused09         )=extra(20)
	customerN(c_unused10         )=extra(21)
	customerN(c_enableAltBillAddr)=extra(22)
	customerN(c_unused11         )=extra(23)
	customerN(c_s01rate)=a(1)
	customerN(c_s02rate)=a(2)
	customerN(c_s03rate)=a(3)
	customerN(c_s04rate)=a(4)
	customerN(c_s05rate)=a(5)
	customerN(c_s09rate)=a(6)
	customerN(c_s10rate)=a(7)
	! /r
fnend
include: ertn