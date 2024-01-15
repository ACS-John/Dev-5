! we use this library to tell programs which client is using the system
if env$('enableClientSelection')='Yes' then goto ClientSelect
!   ! r: sandbox for testing local functions
!     if env$('ACSDeveloper')<>'' then fnSetEnv('acsclient','BRCorp')
!     fn_setup
!     pr 'fn_clientHasMat returns ';fn_clientHasMat(mat tmp$)
!     pr mat tmp$
!     pr 'fn_client_support returns ';fn_client_support(mat tmp_system_id$,mat tmp_system_support_end_date,mat tmp_on_support)
!     for x=1 to udim(mat tmp_system_id$)
!       pr tmp_system_id$(x); tmp_system_support_end_date(x); tmp_on_support(x)
!     nex x
!     pr 'fn_client_has_on_support_list(mat tmp$,45) returns ';fn_client_has_on_support_list(mat tmp$,45);' with 45 grace days'
!     pr mat tmp$
!     pr 'fn_client_has_on_support_list(mat tmp$,0) returns ';fn_client_has_on_support_list(mat tmp$,0);' with 0 grace days'
!     pr mat tmp$
!   end ! /r
!   !  it is now ...  !!!     pr program$&' is not intended to be run directly' : end
ClientSelect: ! r:
	fn_setup
	fnTop(program$)
	fn_clientSelect
goto Xit ! /r
Xit: fnXit
def library fnClientSelect
	fn_setup
	fnClientSelect=fn_clientSelect
fnend
def fn_clientSelect
	fnTos('clientSelect')
	dim csCol$(6)*256
	csCol$(1)='Name'
	csCol$(2)='Number'
	csCol$(3)='brSerial'
	csCol$(4)='Licenses'
	csCol$(5)='Last Selection Date'
	csCol$(6)='PR'
	mat csMask$(6)
	csMask$(1)=''
	csMask$(2)=''
	csMask$(3)=''
	csMask$(4)=''
	csMask$(5)=''
	csMask$(6)=''
	fnFlexInit1('clientSelect1',2,1,10,10,mat csCol$,mat csMask$)
	for clientItem=1 to udim(mat client_name$)
		mat csCol$(6)=('')
		csCol$(1)=client_name$(clientItem)
		csCol$(2)=client_cno$(clientItem)
		csCol$(3)=str$(client_brserial(clientItem))
		csCol$(4)=client_name$(clientItem)
		fnmcreg_read('lastSelection for '&client_name$(clientItem),csCol$(5),'')
		csCol$(6)=fn_payroll_client_state$(client_name$(clientItem))
		fnFlexAdd1(mat csCol$)
	nex clientItem
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=1 then ! r: select that client
		fn_setClient(resp$(1))
	end if ! /r
fnend
def library fnSetClient(scClient$*128)
	if ~setup then fn_setup
	fnSetClient=fn_setClient(scClient$)
fnend
def fn_setClient(scClient$*128)
	dim dataNew$*256
	fnSetEnv('Client',scClient$) ! pr 'env$ client set to '&env$('client') : pause
	fnSetEnv('clientSelected',env$('Client'))
	fnmcreg_write('clientSelected',env$('clientSelected'))
	fnmcreg_write('lastSelection for '&env$('clientSelected'),date$('mm/dd/ccyy')&' - '&time$)
	if env$('enableDataFolderByClient')='Yes' then
		dataNew$=rtrm$(env$('QBase'),'\')&'\'&env$('client') ! &'\'
		fnMakeSurePathExists(dataNew$)
		fnSetEnv('data',dataNew$) ! pr 'env$ client set to '&env$('client') : pause
		fnreg_close
		fnSetQ(dataNew$)
		fncursys$( '',1)
		fnCno
		fnapply_theme
	end if
fnend
def fn_setup
	if ~setup_library then
		setup_library=1
		autoLibrary
	end if
	if ~setup_client then fn_setupClient
fnend
	def fn_setupClient ! ** set up for new clients
		if ~setup_client then
			setup_client=1
			dim client_name$(1)*18
			client_count=0
			mat client_name$(client_count)
			mat client_cno$(client_count)
			mat client_brserial(client_count)
			!  fn_setupClient_add('ACS',1,0) ! TEMP

			fn_setupClient_add('ACS'													,   '420',    34660) ! 58650
			fn_setupClient_add('AJJMaplewood'								,   'ajj',        0)
			fn_setupClient_add('Ash Grove'										,   '286',    19016)
			fn_setupClient_add('Bethany' 											,   '380',    34326)
			fn_setupClient_add('Billings'											,   '440',    33534)
			fn_setupClient_add('Blucksberg' 									,   '465',    34564)
			fn_setupClient_add('Brier Lake'										,   '578',    20306)
			fn_setupClient_add('Campbell'											,   '700',    33942)
			fn_setupClient_add('Cerro Gordo V'								,   '850',    34508) ! 33994)
			fn_setupClient_add('Cerro Gordo T'								,   '970',    34508)
			fn_setupClient_add('Chatom'												,   '911',    15678)
			fn_setupClient_add('Choctaw'											,   '918',    34214)
			fn_setupClient_add('Crockett County'							,  '1141',    15110)
			fn_setupClient_add('Divernon'											,  '1350',    33698)
			fn_setupClient_add('Dorothy Salch'								,  '3812',    34494)
			fn_setupClient_add('Ed Horton'										,  '5535',        0) ! Ed processes like ACS
			fn_setupClient_add('Edinburg'											,  '1478',    34022)
			fn_setupClient_add('Edison'												,  '1480',    34022)
			fn_setupClient_add('Evelyn Pareya'								,  '3385',    34366)
			fn_setupClient_add('Exeter'												,  '1615',    31210)
			fn_setupClient_add('Findlay'											,  '1700',    34132)
			fn_setupClient_add('Framemasters'								,  '1864',        0)
			fn_setupClient_add('Franklin Co Hosp'						,  '1876',    33668)
			fn_setupClient_add('French Settlement'						,  '1880',    33380)
			fn_setupClient_add('Galena'												,  '1945',    34566)
			fn_setupClient_add('Grandview'										,  '2050',    34040)
			fn_setupClient_add('GreeneCo'											,  '2070',    33910)
			fn_setupClient_add('Hope Welty'										,   '851',    34152)
			fn_setupClient_add('Payroll Done Right'					,  '3393',        0)
			fn_setupClient_add('Peter Engler'								,   'ped',        0)
			fn_setupClient_add('Kathys Bookkeeping'					,  '3979',    33672)
			fn_setupClient_add('Kincaid'											,  '2532',    33652)
			fn_setupClient_add('Laco Vinyl'										,  '2222',        0)
			fn_setupClient_add('Millry'												,  '3025',    33968)
			fn_setupClient_add('Moweaqua'											,  '3045',    34594) ! 200032790) ! 33986 <--??  I don't know where that came from - 200032790 was their 4.13 version
			fn_setupClient_add('Morrisonville'								,  '3050',    34408) ! 32242  <-- that's white hall's but there was a mistake in license file for a while
			fn_setupClient_add('Omaha'												,  '3320',    33346)
			fn_setupClient_add('Pennington'										,  '3431',    33332)
			fn_setupClient_add('Purdy'												,  '3610',    34570)
			fn_setupClient_add('Raymond'											,  '3660',    32798)
			fn_setupClient_add('RecoverysUnlimited'					,  '3670',        0)
			fn_setupClient_add('R R Crawford'								,   '760',    12466)
			fn_setupClient_add('Thomas Richardson'						,  '3720',     7718)
			fn_setupClient_add('Scottville Rural'						,  '3840',    33390)
			fn_setupClient_add('Stern and Stern'							,  '4132',200014280)
			fn_setupClient_add('Thayer'												,  '4245',    32800)
			fn_setupClient_add('Thomasboro'										,  '4260',    34068)
			fn_setupClient_add('Unity'												,  '4380',    34478)
			fn_setupClient_add('White Hall'										,  '4625',    32242)
			fn_setupClient_add('World Wide'										,  '4650',    33604)
			fn_setupClient_add('Zaleski'											,  '4710',    34164)
			! r: removed
			! fn_setupClient_add('Alien Electric'								, 'alien',        0)
			! fn_setupClient_add('Carr Plumbing'								,   '780',    34610)   retired 1/8/2024
			! fn_setupClient_add('Allendale','200',0)
			! fn_setupClient_add('BRCorp','7000',50775) ! 50775 is actually DAVID KALINSKI PERSONAL COPY, but it is what Gordon is using.
			! fn_setupClient_add('Crane','1120',0)
			! fn_setupClient_add('Durden','1406',16410)
			! fn_setupClient_add('Fulton','1890',33720) ! Utilities Board
			! fn_setupClient_add('Garrity','1950',0)
			! fn_setupClient_add('Halfway','2130',33768)
			! fn_setupClient_add('Laco Vinyl',  '2222',0)
			! fn_setupClient_add('Starr County Gas','4127',33390)
			! fn_setupClient_add('Lamar' ,1,33854)
			! fn_setupClient_add('Albany' ,190,15376) ! notifed me 9/22/15 that they were switching UB providers
			! fn_setupClient_add('Ashland' ,300,33584)
			! fn_setupClient_add('Battlefield' ,369,33306)
			! fn_setupClient_add('Brazeal' ,570,34418)
			! fn_setupClient_add('Brumbaugh' ,7007,200033202) ! Limited BR license
			! fn_setupClient_add('Carrizo' ,800,34416)
			! fn_setupClient_add('Colyell' ,980,33948)
			! fn_setupClient_add('Community Dev' ,982,34156)
			! fn_setupClient_add('Diamond' ,1345,0)
			! fn_setupClient_add('Eldorado' ,1500,33352)
			! fn_setupClient_add('Energy Exchanger' ,1550,10172)
			! fn_setupClient_add('FirstBaptist' ,1695,33380) ! <-- note it's the same as French Settlement - one of them is wrong, but First Baptist of Frnaklinton's license is 4.1 and not currently necessary, so just commenting them out for now.
			! fn_setupClient_add('Franklin and Son' ,1870,32454)
			! fn_setupClient_add('Franklinton' ,1876,0) ! Town of
			! fn_setupClient_add('Gilbertown',1985,0)
			! fn_setupClient_add('Granby',2040,34098) ! not using ACS as of 6/13/2016
			! fn_setupClient_add('Schachtner Portnoy' ,3828,200008100)
			! fn_setupClient_add('Illiopolis',2340,0)
			! fn_setupClient_add('Kimberling',2530,19212)
			! fn_setupClient_add('Lovington',2689,32720)
			! fn_setupClient_add('Loma Linda',2690,33244)
			! fn_setupClient_add('Nancy Mouser',2795,34318)
			! fn_setupClient_add('Merriam Woods',2900,31702) support ended 12312020
			! fn_setupClient_add('Miller Hardware',3005,14668)
			! fn_setupClient_add('Monticello',3040,12196)
			! fn_setupClient_add('Northwest',3241,11176 )
			! fn_setupClient_add('Oakland',3250,34260)
			! fn_setupClient_add('Petromark',3535,33620)
			! fn_setupClient_add('Philo',3534,34150)
			! fn_setupClient_add('PiattCO',3536,20832)
			! fn_setupClient_add('Sheila',770,0)
			! fn_setupClient_add('Riverside',3725,18332)
			! fn_setupClient_add('Sangamon',3815,34066)
			! fn_setupClient_add('Washington Parrish',4510,34116)
			! fn_setupClient_add('Waverly',4515,34430)
			! fn_setupClient_add('West Accounting',4560,30176)   retired as of 02/22/2018
			! fn_setupClient_add('West Rest Haven',4567,34032)
			! fn_setupClient_add('Willard',4650,33514)
			! fn_setupClient_add('Demo',20001,34450)
			! fn_setupClient_add('Demo',20002,34450)
			! fn_setupClient_add('Demo',20003,34450)
			! fn_setupClient_add('Demo',20004,34450)
			! /r
		end if
	fnend
		def fn_setupClient_add(sca_name$*128,sca_customerId$,sca_brSerialNumber)
		client_count+=1
		mat client_name$(client_count)
		mat client_cno$(client_count)
		mat client_brserial(client_count)
		client_name$(client_count)=sca_name$
		client_cno$(client_count)=sca_customerId$
		client_brserial(client_count)=sca_brSerialNumber
	fnend

def library fnClient$*18
	fn_setup
	fnClient$=fn_client$
fnend
def fn_client$*18
	! if env$('ACSDeveloper')<>'' then pr 'on the way in env client$ is '&env$('client')
	on error goto Ertn
	dim clientReturn$*18
	! r: derive client
	loginNameWhich=fnsrch_case_insensitive(mat client_name$,login_name$)
	serialWhich=srch(mat client_brserial,serial)
	clientWhich=0
	if env$('clientSelected')<>'' then ! it is specified in drive.sys - default to it
		clientWhich=srch(mat client_name$,env$('clientSelected'))
		if clientWhich<=0 then pr 'invalid env clientSelected.' : pause
	else if env$('acsClient')<>'' then ! it is specified in drive.sys - default to it
		clientWhich=srch(mat client_name$,env$('acsClient'))
		if clientWhich<=0 then pr 'invalid env acsClient.' : pause
	else if loginNameWhich>0 then
		clientWhich=loginNameWhich
	else if serialWhich>0 then
		clientWhich=serialWhich
	end if
	fnSetEnv('Client',client_name$(clientWhich))
	fnSetEnv('Client_ID',client_cno$(clientWhich))
	if env$('client')='' then
		pr "env$('client') is blank." : pause
	end if
	! /r
	fn_getClientLicense(mat clientHas$)
	if srch(mat client_name$,login_name$)>0 then
		clientReturn$=login_name$
	else
		clientReturn$=env$('client')
		client_which=srch(mat client_name$,env$('client'))
		if client_which>0 then
			fnSetEnv('Client_ID',client_cno$(client_which))
		else
			pr 'env: Client: "'&env$('client')&'" did not match any entries Mat client_name$.  env: Client_ID could not be set'
			pause
			fnSetEnv('Client_ID','')
		end if
	end if
	! if env$('ACSDeveloper')<>'' then pr 'clientReturn$='&clientReturn$ : pause
	fn_client$=clientReturn$
fnend
def library fnClientNameShort$*18(; clientId$,___,return$*18,which)
	if ~setup then fn_setup

	if clientId$='' then
		return$=env$('client')
	else
		which=srch(mat client_cno$,clientId$)
		if which<=0 then
			pr 'could not locate client ID: '&clientId$&'.'
			pr '  Will return blank instead'
			if env$('acsDeveloper')<>'' then
				pause
			end if
		else
			return$=client_name$(which)
		end if
	end if
	fnClientNameShort$=return$
fnend

def fn_getClientLicense(mat clientHas$)
	if setup_client_has$<>env$('client') then
		setup_client_has$=env$('client')
		mat clientHas$(0)
		! r: big if statement
		client_has_system_count=0
		if env$('client')='ACS' then
			! fn_getClientLicense_add('UB')
			! fn_getClientLicense_add('U4') ! U4 Utility Billing Hand Held Add-On
			! fn_getClientLicense_add('CL')
			! fn_getClientLicense_add('PR')
			! fn_getClientLicense_add('GL')
			fn_getClientLicense_add('Client Billing')	! fn_getClientLicense_add('TM')
			fn_getClientLicense_add('EM')
		! else if env$('client')='Albany' then ! demo undelivered - not on support but needed to debug Past Due Trun Off List from ACS 4 - test in ACS 5 locally
		!   fn_userLimit(1)
		!   if days(date)<=days(20151231,'ccyymmdd') then  fn_getClientLicense_add('UB') : fn_setUbLimit(500)
		!   if days(date)<=days(20151231,'ccyymmdd') then fn_getClientLicense_add('U4')
		else if env$('client')='Ed Horton' then
			fn_userLimit(1)
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('G2') ! G2 Accountant's General Ledger
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') ! U4 Utility Billing Hand Held Add-On
		! else if env$('client')='Alien Electric' then
		! 	if days(date$)<=days('02/28/2022','mm/dd/ccyy') then
		! 		fn_userLimit(1)
		! 		fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		! 	end if
		else if env$('client')='Ash Grove' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
			! canceled 2/7/2018 as per Debbie  -   fn_getClientLicense_add('PR')
			! canceled 2/7/2018 as per Debbie  -   fn_getClientLicense_add('GL')
			! canceled 2/7/2018 as per Debbie  -   fn_getClientLicense_add('CL')

		else if env$('client')='Bethany' then
			fn_userLimit(2) ! second user added 9/22/2021
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Itron FC300' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('CL')
		else if env$('client')='Brier Lake' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		else if env$('client')='Billings' and (env$('Unique_Computer_Id')='58973139-FC9B-1A95-F234-C145E2B22211' or env$('Unique_Computer_Id')='50A59A38-38BF-A82F-9868-04C4E5DD281A') then ! Limit to only UB stuff for (Katrina or Gale)
			fn_userLimit(2) ! actually licensed for 3 users, but has two separate installations
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('UB-EFT')
		else if env$('client')='Billings' and env$('Unique_Computer_Id')='BD04113D-C102-BA29-78AC-D23201FDC70C' then ! Limit to NOT UB stuff for Chris Hopkins
			fn_userLimit(1) ! actually licensed for 3 users, but has two separate installations
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
		else if env$('client')='Billings' then
			fn_userLimit(3)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('UB-EFT')
		else if env$('client')='Blucksberg' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(9999) ! U1 Utility Billing (no discount)
			fn_getClientLicense_add('U4') : u4_device$='Itron FC300' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Campbell' then
			fn_userLimit(4)
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Badger Beacon'
			fn_getClientLicense_add('UB-EFT')
		! else if env$('client')='Carr Plumbing' then
		! 	fn_userLimit(1)
		! 	fn_getClientLicense_add('PR')
		else if env$('client')='Chatom' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Cerro Gordo V' then
			fn_userLimit(2)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Cerro Gordo T' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
		else if env$('client')='Choctaw' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			! fn_getClientLicense_add('GL')  -  removed from support as of 4/30/19
		else if env$('client')='Crockett County' then
			fn_userLimit(1)
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('GB')  !  GB is General Ledger Budget Management Add-On
		! else if env$('client')='Diamond' and days(date$)<=days('05/31/2020','mm/dd/ccyy') then
		! 	fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		! 	fn_getClientLicense_add('GL')
		! 	fn_getClientLicense_add('PR')
		! 	fn_getClientLicense_add('CL')
		! 	! fn_getClientLicense_add('U4') : u4_device$='Boson' ! ACEECA MEZ 1500 ! U4 Utility Billing Hand Held Add-On

		else if env$('client')='Edinburg' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! ACEECA MEZ 1500 ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Edison' then
			fn_userLimit(4) ! 4th user added 12/23/2023 jb
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
			! if days(date$)<=days('12/31/2022','mm/dd/ccyy') then
			fn_getClientLicense_add('U4') : u4_device$='Master Meter' ! ! U4 Utility Billing Hand Held Add-On
			! implement a trial until they've paid.
			! end if
		else if env$('client')='Exeter' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		! else if env$('client')='Energy Exchanger' then
		! 	fn_userLimit(1)
		! 	fn_getClientLicense_add('PR')
		else if env$('client')='Dorothy Salch' then ! 12/6/23 dropped Dorothy from Qt support as she never calls and her business seems to be sunsetting, free updates for life for Dorothy Salch. -JB
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('G2') ! G2 Accountant's General Ledger
		else if env$('client')='Evelyn Pareya' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('G2') ! G2 Accountant's General Ledger
		else if env$('client')='Findlay' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Itron FC300' ! U4 Utility Billing Hand Held Add-On
			!   else if env$('client')='Franklin and Son' then
			!     fn_userLimit(1)
			!     fn_getClientLicense_add('PR')
		else if env$('client')='Franklin Co Hosp' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('CL')
		else if env$('client')='French Settlement' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(9999) ! U1 Utility Billing (no discount)
		else if env$('client')='Galena' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			!   else if env$('client')='Granby' then
			!     fn_userLimit(2)
			!     fn_getClientLicense_add('UB') : fn_setUbLimit(9999) ! U1 Utility Billing (no discount)
		else if env$('client')='Grandview' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
		else if env$('client')='Payroll Done Right' then
			if days(date$)<=days('12/31/2023','mm/dd/ccyy') then 
				fn_userLimit(2)
			else
				fn_userLimit(1)
			end if
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
		! else if env$('client')='Schachtner Portnoy' then
		! 	fn_userLimit(76)
		! 	fn_getClientLicense_add('CM')
		else if env$('client')='GreeneCo' then
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='EZReader' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Hope Welty' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
		else if env$('client')='Kathys Bookkeeping' then
			fn_userLimit(2)
			fn_getClientLicense_add('GL')
			! fn_getClientLicense_add('G2') ! G2 Accountant's General Ledger
			fn_getClientLicense_add('PR')
			!     fn_getClientLicense_add('P4')
		else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='1478AEE0-5BCB-11D9-B0AC-BCAEC5EA1947' then
			fn_userLimit(1)
			fn_getClientLicense_add('PR')
			!   else if env$('client')='Kincaid' and and env$('Unique_Computer_Id')='XXX need to do XXX' then
			!     fn_userLimit(1)
			!     fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
		else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='03000200-0400-0500-0006-000700080009' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Boson' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('U5') ! UB External Collections Processing - given freely on 5/27/21
		else if env$('client')='Kincaid' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Badger Beacon' ! formerly 'Boson' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('U5') ! UB External Collections Processing - given freely on 5/27/21
			!     fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
		! else if env$('client')='Lovington' then
		! 	fn_userLimit(1)
		! 	fn_getClientLicense_add('UB') : fn_setUbLimit(9999) ! U1 Utility Billing (no discount)
		! 	fn_getClientLicense_add('U4') : u4_device$='Sensus' ! U4 Utility Billing Hand Held Add-On
		! 	fn_getClientLicense_add('GL')
		! 	fn_getClientLicense_add('PR')
		! 	fn_getClientLicense_add('CL')
		! canceled		else if env$('client')='Merriam Woods' then
		! canceled			fn_userLimit(2)
		! canceled			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
		! canceled			fn_getClientLicense_add('U5') ! UB External Collections Processing
		! canceled			!     fn_getClientLicense_add('CR')
		! canceled			!     fn_getClientLicense_add('GL')
		! canceled			!     fn_getClientLicense_add('PR')
		! canceled			!     fn_getClientLicense_add('CL')
		else if env$('client')='Millry' then
			fn_userLimit(4)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Itron FC300' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Morrisonville' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='EZReader' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Moweaqua' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(1000) ! U2 Utility Billing (500-1000 customers)
			fn_getClientLicense_add('U4') : u4_device$='Badger Beacon' ! 'Badger Connect C' 7/14/21 switched to newer Badger interface (Beacon) from Connect C     ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Pennington' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U5') ! UB External Collections Processing (purchased 12/2022)
		else if env$('client')='Peter Engler' then
			fn_getClientLicense_add('CL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('GL')
		else if env$('client')='Purdy' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='Aclara' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Omaha' then
			fn_getClientLicense_add('UB') : fn_setUbLimit(9999) ! U1 Utility Billing (no discount)
			fn_getClientLicense_add('U4') : u4_device$='READy Water'
		else if env$('client')='Raymond' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='R R Crawford' then
			fn_userLimit(1)
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('GL')
		else if env$('client')='Scottville Rural' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		! else if env$('client')='Starr County Gas' then
		! 	fn_userLimit(1)
		! 	if days(date$)<=days('04/15/2018','mm/dd/ccyy') then fn_getClientLicense_add('UB') : fn_setUbLimit(9999)
		else if env$('client')='Stern and Stern' then
			fn_userLimit(99) ! unknown
			fn_getClientLicense_add('CM')
		else if env$('client')='Thayer' then
			fn_userLimit(1)
			fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			fn_getClientLicense_add('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On
		else if env$('client')='Thomasboro' then
			fn_userLimit(1)
			! seperated out 10/3/22   fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
			! fn_getClientLicense_add('U4') : u4_device$='Badger Connect C' ! switched to Beacon on 7/2/20 JB
			! seperated out 10/3/22   fn_getClientLicense_add('U4') : u4_device$='Badger Beacon' ! U4 Utility Billing Hand Held Add-On
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('CL')
			! fn_getClientLicense_add('UB-EFT')
		else if env$('client')='Thomas Richardson' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('PR')
		!   else if env$('client')='Waverly' then
		!     fn_userLimit(1)
		!     fn_getClientLicense_add('UB') : fn_setUbLimit(500) ! U3 Utility Billing (<500 Customers)
		! else if env$('client')='West Accounting' then
		!   fn_userLimit(1)
		!   fn_getClientLicense_add('PR')
		else if env$('client')='White Hall' then
			fn_userLimit(2)
			fn_getClientLicense_add('UB') !   1000+  -  fn_setUbLimit     do they not have one???
			!   else if env$('client')='Willard' then
			!     fn_userLimit(1)
			!     fn_getClientLicense_add('GL')
			!     fn_getClientLicense_add('PR')
			!     fn_getClientLicense_add('CL')
		else if env$('client')='World Wide' then
			fn_userLimit(1)
			fn_getClientLicense_add('GL')
		else if env$('client')='Zaleski' then
			fn_userLimit(1)
			fn_getClientLicense_add('PR')
			fn_getClientLicense_add('GL')
			fn_getClientLicense_add('G2') ! G2 Accountant's General Ledger
		end if
		! /r
	end if
fnend
	def fn_getClientLicense_add(ch_item$*256)
		client_has_system_count=udim(mat clientHas$)
		if client_has_system_count=0 then
			client_has_system_count=2
			dim clientHas$(0)*256
			mat clientHas$(1)
			clientHas$(1)='CO'
		else
			client_has_system_count+=1
		end if
		mat clientHas$(client_has_system_count)
		clientHas$(client_has_system_count)=ch_item$
	fnend
def library fnclient_has_mat(mat c_has$) ! returns a list of system each client owns
	fn_setup
	fnclient_has_mat=fn_clientHasMat(mat c_has$)
fnend
def fn_clientHasMat(mat c_has$)
	if env$('client')='' then pr 'fn_clientHasMat called but env client not set.' : pause
	fn_getClientLicense(mat clientHas$)
	mat c_has$(udim(mat clientHas$))
	mat c_has$=clientHas$
	fn_clientHasMat=client_has_system_count
fnend
def library fnClientHas(ch_sys$*256)
	fn_setup
	fnClientHas=fn_clientHas(ch_sys$)
fnend
def fn_clientHas(ch_sys$*256; ___,returnN)
	fn_getClientLicense(mat clientHas$)
	if srch(mat clientHas$,uprc$(ch_sys$))>0 or srch(mat clientHas$,ch_sys$)>0 then returnN=1
	fn_clientHas=returnN
fnend
def fn_userLimit(userLimit)
	if env$('acsProduct')='ACS Online' then
		userCount=fn_userCount
		! if env$('acsDeveloper')<>'' then pause
		if userCount>userLimit then
			user_limit_exceeded=1
			msgbox('Maximum number of licensed concurrent users ('&str$(userLimit)&') exceeded.')
			execute 'system logoff'
		end if
	else if val(env$('user_limit'))<>userLimit then
		execute 'config option 9 '&str$(userLimit)
		setEnv('user_limit',str$(userLimit))
	end if

fnend
def fn_userCount(; ___,hStUsers,returnN,ucListStarted,ucLine$*256)
	exec 'status users >"'&env$('temp')&'\acsUsers[session].tmp"' ! don't use [temp] yet - called before it is set by core\start.br
	open #hStUsers=fnH: 'name='&env$('temp')&'\acsUsers[session].tmp',d,input ! don't use [temp] yet - called before it is set by core\start.br
	do
		linput #hStUsers: ucLine$ eof UcEof
		if lwrc$(trim$(ucLine$))='current users on network:' then
			ucListStarted=1
		else if ucListStarted then
			! fnaddonec(mat activeUsers$,ucLine$)
			if lwrc$(trim$(ucline$(pos(ucline$,' '):len(ucline$))))=lwrc$(env$('client')) then
				returnN+=1
			end if
		end if
	loop
	UcEof: !
	close #hStUsers:
	fnfree(env$('temp')&'\acsUsers[session].tmp') ! don't use [temp] yet - called before it is set by core\start.br
	fn_userCount=returnN
fnend
! r: def library fnuser_limit_exceeded
! fn_setup
! fn_getClientLicense(mat clientHas$)
!   fnuser_limit_exceeded=user_limit_exceeded
! /r fnend

def library fnhand_held_device$*20
	fn_setup
	fn_getClientLicense(mat clientHas$)
	fnhand_held_device$=fn_hand_held_device$
fnend
def fn_hand_held_device$*20
	dim u4_device$*20,u4_deviceDefault$*20
	u4_deviceDefault$=u4_device$
	fnreg_read('Hand Held Device',u4_device$,u4_deviceDefault$)
	if u4_device$='' then u4_device$=u4_deviceDefault$ ! in case it's been set and then blanked out to force default for client
	fn_hand_held_device$=u4_device$
fnend
def fn_setUbLimit(x)
	gUbLimit=x
	setEnv('UB_Limit',str$(gUbLimit))
fnend
def library fnub_printbill_program$*256
	if ~upp_setup then ! r:
		upp_setup=1
		fn_setup
		dim upp_return$*256
		dim ub_printbill_client$(1)*18
		dim ub_printbill_program$(1)*128
		mat ub_printbill_client$(999)
		mat ub_printbill_program$(999)
		ub_printbill_count=0

		fn_upp_add('Ash Grove'         	,'ubprtfull_ashgrove'    	)
		fn_upp_add('Bethany'           	,'ubprtbl1_Bethany'      	)  ! on 12/17/18 I cleaned it up a little but didn't move it into (basic) yet - it could be though -john
		fn_upp_add('Chatom'            	,'ubprtbl1_chatom'       	)
		fn_upp_add('Divernon'          	,'ubprtbl1_div'          	)
		fn_upp_add('Findlay'           	,'ubprtbl1_fin'          	)
		fn_upp_add('Grandview'         	,'ubprtbl1_gra'          	)
		fn_upp_add('Kincaid'           	,'ubprtbl1_kin'          	)
		fn_upp_add('Millry'            	,'ubprtbl1_millry'       	)
		fn_upp_add('Morrisonville'    	,'ubprtbl1_morrisonville'	)
		fn_upp_add('Moweaqua'          	,'PrintBill_Moweaqua'    	)
		fn_upp_add('Purdy'             	,'ubprtbl1_purdy'        	)
		fn_upp_add('Scottville Rural' 	,'ubprtbl1_scottville'   	)
		fn_upp_add('Thayer'            	,'ubprtbl1_thayer'       	)
		! fn_upp_add('Thomasboro'        	,'ubprtbl1_tho'          	)
		fn_upp_add('White Hall'        	,'ubprtbl1_wh'           	)
		fn_upp_add('Brier Lake'        	,'ubprtthree_Brier'      	)
		fn_upp_add('Cerro Gordo V'     	,'ubprtlas_cerro'        	)

		fn_upp_add('Campbell'          	,'(basic)'	)  ! derived from printbill_french_settlement_gas which should still work too
		fn_upp_add('Edinburg'          	,'(basic)'	) ! 'ubprtbl1_edi'
		fn_upp_add('Edison'            	,'(basic)'	)
		fn_upp_add('Exeter'            	,'(basic)'	)
		fn_upp_add('French Settlement'	,'(basic)'	) ! 'printbill_french_settlement_gas'
		fn_upp_add('GreeneCo'          	,'(basic)'	) ! 'ubprtbl1_GreeneCo'
		fn_upp_add('Raymond'           	,'(basic)'	) ! 'ubprtbl1_Raymond'
		fn_upp_add('Galena'            	,'(basic)'	) ! 'ubprtbl1_galena')
		fn_upp_add('Billings'          	,'(basic)'	) ! ubprtthree_bill
		fn_upp_add('Choctaw'           	,'(basic)'	) ! 'ubprtlas_choctaw'
		fn_upp_add('Omaha'             	,'(basic)'	) ! 'ubprtthree_Omaha'
		fn_upp_add('Pennington'        	,'(basic)'	) ! PrintBill_Pennington ! atlantis format - hits preprinted stock
		fn_upp_add('Blucksberg'        	,'(basic)'	) ! 'PrintBill_Blucksberg'

		! r: old removed lines
			! fn_upp_add('Ashland','ubprtbl1_ashland')
			! fn_upp_add('Franklinton','ubprtbl1_Franklinton')
			! fn_upp_add('Kimberling','ubprtbl1_Kimberling') ! these are unused but also a nice 4 per page bill that looks pretty comprehensive - move the logic to (basic) if used elsewhere
			! fn_upp_add('Illiopolis','ubprtbl1_Illiopolis')
			! fn_upp_add('Oakland','ubprtbl1_Oakland')
			! fn_upp_add('Gilbertown','ubprtbl1_Gilbertown')
			! fn_upp_add('Waverly','ubprtbl1_Waverly')
			! >>Bills-Laser (3 per page) ^ ubPrtThree
			! fn_upp_add('Ash Grove','ubprtprace_ash')  removed as it was a duplicate line and this one was ignored anyway - also removed ignored program from updates.  5/20/20
			! fn_upp_add('Albany','ubprtthree_Albany')
			! fn_upp_add('Colyell','ubprtlas_colyell')
			! fn_upp_add('Carrizo','ubprtthree_Carrizo')
			! fn_upp_add('Ed','ubprtthree_barcode')
			! fn_upp_add('Gilbertown','ubprtthree_Gilb')
			! fn_upp_add('Granby','ubprt3prace_Granby')
			! fn_upp_add('Riverside','ubprtthree_River')
			! fn_upp_add('Sangamon','ubprtthree_san')
			! >>Bills-Dot Matrix 4x6 ^ S:\acsUB\ubPrtBl14X6
			! >>Bills-Dot Matrix 3.5x6 ^ S:\acsUB\Bill35X6
			! >>Bills-Dot Matrix 3.5x7.5 ^ S:\acsUB\Bill35X75
			! >>Bills-Dot Matrix Double Wide ^ S:\acsUB\billDouble
			! >>Bills-Full Page ^ S:\acsUB\Ubprtfull
			! >>Bills-Miscellaneous ^ S:\acsUB\Ubprtful
		! /r
		mat ub_printbill_client$(ub_printbill_count)
		mat ub_printbill_program$(ub_printbill_count)

	end if  ! /r
	upp_return$='S:\Core\Menu.br'
		ua_which=srch(mat ub_printbill_client$,env$('Client'))
		if ua_which>0 then
			upp_return$=ub_printbill_program$(ua_which)
		else
			msgbox('Your Utility Bill settings could not be determined.  Please contact ACS at 1-800-643-6318.')
		end if
	!        (basic) should be fine.   end if
	! if env$('acsDeveloper')<>'' then pr 'upp_return$='&upp_return$ : pause
	fnub_printbill_program$=upp_return$
fnend
	def fn_upp_add(ua_client$,ua_program$*128)
		ub_printbill_count+=1
		ub_printbill_client$(ub_printbill_count)=ua_client$
		if ua_program$='(basic)' then
			ub_printbill_program$(ub_printbill_count)=ua_program$
		else
			ub_printbill_program$(ub_printbill_count)='S:\acsUB\'&ua_program$&'.br'
		end if
	fnend
def library fnpayroll_client_state$*2(; client$*64,___,return$*2,which)
		if ~setup then fn_setup
		fnpayroll_client_state$=fn_payroll_client_state$( client$)
fnend
def fn_payroll_client_state$*2(; client$*64,___,return$*2,which)
	if ~pcs_setup then ! r:
		pcs_setup=1
		dim pr_clientstate_client$(0)*64
		dim pr_clientstate_state$(0)*2
		mat pr_clientstate_client$(0)
		mat pr_clientstate_state$(0)
		pr_clientstate_count=0
		fn_pcs_add('ACS'                     	,'NJ')
		fn_pcs_add('Ash Grove'              	,'MO')
		fn_pcs_add('Bethany'                	,'IL')
		fn_pcs_add('Billings'               	,'MO')
		fn_pcs_add('Campbell'               	,'MO')
		! fn_pcs_add('Carr Plumbing'         	,'AR')
		fn_pcs_add('Cerro Gordo V'         	,'IL')
		fn_pcs_add('Cerro Gordo T'         	,'IL')
		fn_pcs_add('Crockett County'       	,'TX')
		fn_pcs_add('Divernon'               	,'IL')
		fn_pcs_add('Edinburg'               	,'IL')
		fn_pcs_add('Edison'                 	,'GA')
		fn_pcs_add('Ed Horton'              	,'IL')
		fn_pcs_add('Galena'                 	,'MO')
		fn_pcs_add('Hope Welty'             	,'IL')
		fn_pcs_add('Kincaid'                	,'IL')
		fn_pcs_add('Kathys Bookkeeping'    	,'OK')
		fn_pcs_add('Oklahoma'               	,'OK')
		fn_pcs_add('Payroll Done Right'    	,'OR')
		fn_pcs_add('Peter Engler'           	,'MO')
		fn_pcs_add('R R Crawford'           	,'KY')
		fn_pcs_add('Thomas Richardson'     	,'LA')
		fn_pcs_add('Thomasboro'             	,'IL')
		fn_pcs_add('Unity'                  	,'IL')
		fn_pcs_add('Zaleski'                	,'TX')
		!   fn_upp_add('Ash Grove','ubprtlas_ashgrove')
		!   fn_pcs_add('Lamar','MS')
		!   fn_pcs_add('Battlefield','MO')
		!   fn_pcs_add('Community Dev','TN')
		!   fn_pcs_add('Diamond','MO')
		!   fn_pcs_add('Durden','LA')
		!   fn_pcs_add('Energy Exchanger','OK')  dropped support of april 2019, removed april 2020
		!   fn_pcs_add('Franklin and Son','AR')
		!   fn_pcs_add('Franklinton','LA')
		!   fn_pcs_add('GreeneCo','MO')
		!   fn_pcs_add('Kimberling','MO')
		!   fn_pcs_add('Lovington','IL')
		!   fn_pcs_add('Merriam Woods','MO')
		!   fn_pcs_add('Monticello','IL')
		!   fn_pcs_add('Nancy Mouser','OK')
		!   fn_pcs_add('Northwest','AR')
		!   fn_pcs_add('Philo','IL')
		!   fn_pcs_add('PiattCO','IL')
		!   fn_pcs_add('Raymond','IL')
		!   fn_pcs_add('Riverside','IN') ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
		!   fn_pcs_add('Sheila','MO')
		!   fn_pcs_add('Washington Parrish','LA')
		!   fn_pcs_add('West Rest Haven','')
		!   fn_pcs_add('West Accounting','OR')

	end if  ! /r

	if client$='' then client$=env$('client')
	which=srch(mat pr_clientstate_client$,client$)
	if which>0 then
		return$=pr_clientstate_state$(which)
	else
		return$='--'
	end if
	fn_payroll_client_state$=return$
fnend
def fn_pcs_add(pa_client$*128,pa_state$*2; ___,pr_clientstate_count)
	! builds local: mat pr_clientstate_client$ and mat pr_clientstate_state$
	pr_clientstate_count=udim(mat pr_clientstate_client$)+1
	mat pr_clientstate_client$(pr_clientstate_count)
	mat pr_clientstate_state$(pr_clientstate_count)
	pr_clientstate_client$(pr_clientstate_count)=pa_client$
	pr_clientstate_state$(pr_clientstate_count)=pa_state$
fnend
def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
	fn_setup
	fnclient_has_on_support_list=fn_client_has_on_support_list(mat chosl_list$, chosl_grace_days)
fnend
def fn_client_has_on_support_list(mat chosl_list$; chosl_grace_days)
	dim chosl_owns_list$(0)*256
	chosl_count=0
	chosl_owns_count=fn_clientHasMat(mat chosl_owns_list$)
	for chosl_item=1 to chosl_owns_count
		if fn_client_has_on_support_item(chosl_owns_list$(chosl_item), chosl_grace_days) then
			chosl_count+=1
			mat chosl_list$(chosl_count)
			chosl_list$(chosl_count)=chosl_owns_list$(chosl_item)
		end if
	next chosl_item
	fn_client_has_on_support_list=chosl_count
fnend
def library fnclient_has_on_support_item(chosi_item$*256; days_grace)
	fn_setup
	fnclient_has_on_support_item=fn_client_has_on_support_item(chosi_item$, days_grace)
fnend
def fn_client_has_on_support_item(chosi_item$*256; days_grace)
	client_id=val(env$('Client_ID'))
	fn_client_support_setup(env$('Client_ID'),mat chosi_system_id$,mat chosi_system_support_end_date,mat chosi_on_support, days_grace)
	chosi_retun=0
	chosi_which=srch(mat chosi_system_id$,chosi_item$)
	if chosi_which>0 then
		chosi_retun=chosi_on_support(chosi_which)
	else
		pr 'system ('&chosi_item$&') is not owned by client number '&env$('Client_ID')
		if env$('ACSDeveloper')<>'' then pause
	end if
	fn_client_has_on_support_item=chosi_retun
fnend
def library fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
	fn_setup
	fnclient_support=fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
fnend
def fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
	fn_client_support=fn_client_support_setup(env$('Client_ID'),mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
fnend
def fn_client_support_setup(client_id$,mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_days_grace)
	! css_days_grace=day grace period to allow users to update after support has expired.
	if css_setup$<>client_id$ then ! r:
		cache_css_client_owns_count=fn_clientHasMat(mat css_client_owns$)
		mat css_client_owns$(cache_css_client_owns_count)
		mat css_system_id$(cache_css_client_owns_count)
		mat css_system_id$=css_client_owns$

		mat cache_css_system_id$(0)
		mat cache_css_system_sup_end_date(0)
		cache_css_system_count=0
		open #h_support=fnH: 'Name=S:\Core\Data\acsllc\support.h420,Version=2,KFName=S:\Core\Data\acsllc\support-idx.h420,version=0,Shr',i,i,k
		restore #h_support: ! ,key>==lpad$(trim$(client_id$),kln(h_support)):
		do
			read #h_support,using F_SUPPORT: cln$,scode$,sdt1,stm$,sup_exp_date,scst eof CSS_SUPPORT_EOF
			F_SUPPORT: form pos 1,v 6,pos 9,c 2,n 8,c 2,n 8,n 10.2,4*c 50
			! cln=val(cln$) conv ignore
			if cln$=rtrm$(client_id$) then
				if srch(mat css_client_owns$,fn_system_code_standardize$(scode$))>0 then
					cache_css_system_count+=1
					mat cache_css_system_id$(cache_css_system_count)
					mat cache_css_system_sup_end_date(cache_css_system_count)
					cache_css_system_id$(cache_css_system_count)=fn_system_code_standardize$(scode$)
					cache_css_system_sup_end_date(cache_css_system_count)=sup_exp_date
				end if
			end if
		loop
		CSS_SUPPORT_EOF: !
		close #h_support:
		css_setup$=client_id$
	end if  ! /r
	! r: move cache_* arrays into passed in and out arrays
		mat css_system_id$(cache_css_system_count)
		mat css_system_id$=cache_css_system_id$
		mat css_system_support_end_date(cache_css_system_count)
		mat css_system_support_end_date=cache_css_system_sup_end_date
	! /r
	! r: determine if on support
		for css_item=1 to cache_css_client_owns_count ! udim(mat css_system_id$)
			if css_item>udim(mat css_system_support_end_date) then
				css_on_support(css_item)=0 ! pause
			else if days(date('ccyymmdd'),'ccyymmdd')<=days(css_system_support_end_date(css_item),'ccyymmdd')+css_days_grace then
				css_on_support(css_item)=1
			else
				css_on_support(css_item)=0
			end if
		next css_item
	! /r
fnend
def library fnsystem_code_standardize$(st_code$*256)
	fn_setup
	fnsystem_code_standardize$=fn_system_code_standardize$(st_code$)
fnend
def fn_system_code_standardize$(st_code$*256)
	! this function is to translate (to top level system) from 'CO Systems 2.sys_ID' to 'CO Systems 2.sys_Parent' value (or leave at id if no parent)
	! cursys type codes
	!
	st_code$=uprc$(st_code$)
	! if st_code$='G1' then st_code$='GL'
	if st_code$='G1' then st_code$='GL'
	if st_code$='G3' then st_code$='G2' ! Accountant's GL Add On
	if st_code$='U1' then st_code$='UB' ! UB 1000+ NoDiscount
	if st_code$='U2' then st_code$='UB' ! UB 500-1000 Customers
	if st_code$='U3' then st_code$='UB' ! UB <500 Customers
	! U4 is UB handheld add on in both
	if st_code$='P1' then st_code$='PR'
	if st_code$='P2' then st_code$='P4' ! Job Cost Payroll Add On
	fn_system_code_standardize$=st_code$
fnend
def library fnclient_is_converting
	fn_setup
	fnclient_is_converting=fn_client_is_converting
fnend
def fn_client_is_converting(; ___,returnN)
	if env$('ACSDeveloper')<>'' then
		returnN=1
	else if env$('client')='R R Crawford'       and days(date$)<=days('12/31/2022','mm/dd/ccyy') then
		returnN=1
	! else if env$('client')='Peter Engler'       and days(date$)<=days('12/31/2022','mm/dd/ccyy') then
	! 	returnN=1
	! else if env$('client')='Kathys Bookkeeping' and days(date$)<=days('3/31/2021','mm/dd/ccyy')  then
	! 	returnN=1
	end if
	fn_client_is_converting=returnN
fnend

include: ertn No
