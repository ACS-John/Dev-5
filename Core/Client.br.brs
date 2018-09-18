! we use this library to tell programs which client is using the system
if env$('enableClientSelection')='Yes' then goto ClientSelect
!   ! r: sandbox for testing local functions
!     if env$('ACSDeveloper')<>'' then let setenv('acsclient','BRCorp')
!     fn_setup
!     pr 'fn_client_has_mat returns ';fn_client_has_mat(mat tmp$)
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
  fntop(program$)
  fn_clientSelect
goto XIT ! /r
XIT: fnXit
def library fnClientSelect
  fn_setup
  ! fn_getClientLicense(mat client_has$)
  fnClientSelect=fn_clientSelect
fnend
def fn_clientSelect
  fnTos('clientSelect')
  dim flexItem$(4)*256
  mat clientSelectHeading$(4)
  clientSelectHeading$(1)='Name'
  clientSelectHeading$(2)='Number'
  clientSelectHeading$(3)='brSerial'
  clientSelectHeading$(4)='Licenses'
  mat clientSelectMask$(4)
  clientSelectMask$(1)=''
  clientSelectMask$(2)=''
  clientSelectMask$(3)=''
  clientSelectMask$(4)=''
  mat flexItem$(4)
  fnflexinit1('clientSelect1',2,1,10,10,mat clientSelectHeading$,mat clientSelectMask$)
  for clientItem=1 to udim(mat client_name$)
    flexItem$(1)=client_name$(clientItem)
    flexItem$(2)=str$(client_cno(clientItem))
    flexItem$(3)=str$(client_brserial(clientItem))
    flexItem$(4)=client_name$(clientItem)
    fnflexadd1(mat flexItem$)
  nex clientItem
  fnCmdSet(2)
  fnAcs('clientSelect',0,mat resp$,ckey)
  if ckey=1 then ! r: select that client
    fn_setClient(resp$(1))
  end if ! /r 
fnend
def library fnSetClient(scClient$*128)
  if ~setup then let fn_setup
  fnSetClient=fn_setClient(scClient$)
fnend
def fn_setClient(scClient$*128)
  dim dataNew$*256
  setenv('Client',scClient$) ! pr 'env$ client set to '&env$('client') : pause
  setenv('clientSelected',env$('Client'))
  fnmcreg_write('clientSelected',env$('clientSelected'))
  if env$('enableDataFolderByClient')='Yes' then
    dataNew$=rtrm$(env$('QBase'),'\')&'\'&env$('client') ! &'\'
    fnmakesurepathexists(dataNew$)
    setenv('data',dataNew$) ! pr 'env$ client set to '&env$('client') : pause
    fnreg_close
    ! fnMapToVirturalDrive(dataNew$,'Q:') 
    fnSetQ(dataNew$)
    fncursys$( '',1)
    fncno(unused)
  end if
fnend
def fn_setup
  if ~setup_library then 
    setup_library=1
    library 'S:\Core\Library': fnerror,fngethandle,fnreg_read
    library 'S:\Core\Library': fnTos,fnflexinit1,fnflexadd1,fnCmdSet,fntop,fnAcs
    library 'S:\Core\Library': fnXit
    library 'S:\Core\Library': fnreg_close
    library 'S:\Core\Library': fnSetQ
    library 'S:\Core\Library': fnsrch_case_insensitive
    library 's:\Core\Library': fnmakesurepathexists
    library 's:\Core\Library': fnmcreg_write
    library 's:\Core\Library': fnfree
    library 's:\Core\Library': fncursys$
    library 's:\Core\Library': fncno
  end if 
  fn_setup_client
fnend 
def library fnclient$*18
  fn_setup
  fnclient$=fn_client$
fnend 
def fn_client$*18
  ! if env$('ACSDeveloper')<>'' then pr 'on the way in env client$ is '&env$('client')
  on error goto ERTN
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
  setenv('Client',client_name$(clientWhich))
  setenv('Client_ID',str$(client_cno(clientWhich)))
  if env$('client')='' then
    pr "env$('client') is blank." : pause
  end if
  ! /r 
  fn_getClientLicense(mat client_has$)
  if srch(mat client_name$,login_name$)>0 then
    clientReturn$=login_name$
  else
    clientReturn$=env$('client')
    client_which=srch(mat client_name$,env$('client'))
    if client_which>0 then 
      setenv('Client_ID',str$(client_cno(client_which)))
    else
      pr 'env: Client: "'&env$('client')&'" did not match any entries Mat client_name$.  env: Client_ID could not be set'
      pause
      setenv('Client_ID','')
    end if
  end if
  ! if env$('ACSDeveloper')<>'' then pr 'clientReturn$='&clientReturn$ : pause
  fn_client$=clientReturn$
fnend 
! <Updateable Region: ERTN>
ERTN: fnerror(program$,err,line,act$,"NO")
  if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
  execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
def fn_setup_client ! ** set up for new clients
  if ~setup_client then 
    setup_client=1
    dim client_name$(1)*18
    client_count=0
    mat client_name$(client_count)
    mat client_cno(client_count)
    mat client_brserial(client_count)
    !  fn_setup_client_add("ACS",1,0) ! TEMP
    ! 
    fn_setup_client_add("ACS" ,420,34660) ! 58650
    fn_setup_client_add("Ed Horton" ,5535,0)! Ed processes like ACS
    ! 
    fn_setup_client_add("Lamar" ,1,33854)
    !   fn_setup_client_add("Albany" ,190,15376) ! notifed me 9/22/15 that they were switching UB providers
    fn_setup_client_add("Ash Grove" ,286,19016)
    !   fn_setup_client_add("Ashland" ,300,33584)
    !   fn_setup_client_add("Battlefield" ,369,33306)
    fn_setup_client_add("Bethany" ,380,34326)
    fn_setup_client_add("Billings" ,440,33534)
    fn_setup_client_add("Blucksberg" ,465,34564)
    !   fn_setup_client_add("Brazeal" ,570,34418)
    fn_setup_client_add("Brier Lake" ,578,20306)
    fn_setup_client_add("BRCorp" ,7000,50775) ! 50775 is actually DAVID KALINSKI PERSONAL COPY, but it is what Gordon is using.
    fn_setup_client_add("Campbell" ,700,33942)
    fn_setup_client_add("Carr Plumbing" ,780,34610)
    ! fn_setup_client_add("Carrizo" ,800,34416)
    fn_setup_client_add("Cerro Gordo" ,850,34508) ! 33994)
    fn_setup_client_add("Cerro Gordo T" ,970,34508)
    fn_setup_client_add("Chatom" ,911,15678)
    fn_setup_client_add("Choctaw" ,918,34214)
    ! fn_setup_client_add("Colyell" ,980,33948)
    ! fn_setup_client_add("Community Dev" ,982,34156)
    fn_setup_client_add("Divernon" ,1350,33698)
    fn_setup_client_add("Dorothy Salch" ,3812,34494)
    fn_setup_client_add("Durden" ,1406,16410)
    fn_setup_client_add("Edinburg" ,1478,34022)
    fn_setup_client_add("Edison" ,1480,34022)
    ! fn_setup_client_add("Eldorado" ,1500,33352)
    fn_setup_client_add("Evelyn Pareya" ,3385,34366)
    fn_setup_client_add("Exeter" ,1615,31210)
    fn_setup_client_add("Energy Exchanger" ,1550,10172)
    !   fn_setup_client_add("FirstBaptist" ,1695,33380) ! <-- note it's the same as French Settlement - one of them is wrong, but First Baptist of Frnaklinton's license is 4.1 and not currently necessary, so just commenting them out for now.
    fn_setup_client_add("Findlay" ,1700,34132)
    !   fn_setup_client_add("Franklin and Son" ,1870,32454)
    fn_setup_client_add("Franklin Co Hosp" ,1876,33668)
    !   fn_setup_client_add("Franklinton" ,1876,0) ! Town of
    fn_setup_client_add("French Settlement" ,1880,33380)
    fn_setup_client_add("Fulton" ,1890,33720) ! Utilities Board
    fn_setup_client_add("Galena" ,1945,34566)
    fn_setup_client_add("Garrity",1950,0)
!   fn_setup_client_add("Gilbertown",1985,0)
!   fn_setup_client_add("Granby",2040,34098) ! no longer using as of 6/13/2016
    fn_setup_client_add("Grandview",2050,34040)
    fn_setup_client_add("GreenCo",2070,33910)
    fn_setup_client_add("Halfway" ,2130,33768)
    fn_setup_client_add("Hope Welty" ,851,34152)
    fn_setup_client_add("Payroll Done Right" ,3393,0)
    ! fn_setup_client_add("Illiopolis",2340,0)
    fn_setup_client_add("Kathys Bookkeeping",3979,33672)
    ! fn_setup_client_add("Kimberling",2530,19212)
    fn_setup_client_add("Kincaid",2532,33652)
    fn_setup_client_add("Lovington",2689,32720)
    fn_setup_client_add("Loma Linda",2690,33244)
!   fn_setup_client_add("Nancy Mouser",2795,34318)
    fn_setup_client_add("Merriam Woods",2900,31702)
!   fn_setup_client_add("Miller Hardware",3005,14668)
    fn_setup_client_add("Millry",3025,33968)
!   fn_setup_client_add("Monticello",3040,12196)
    fn_setup_client_add("Moweaqua",3045,34594) ! 200032790) ! 33986 <--??  I don't know where that came from - 200032790 was their 4.13 version
    fn_setup_client_add("Morrisonville",3050,34408) ! 32242  <-- that's white hall's but there was a mistake in license file for a while
!   fn_setup_client_add("Northwest",3241,11176 )
!   fn_setup_client_add("Oakland",3250,34260)
    fn_setup_client_add("Omaha",3320,33346)
    fn_setup_client_add("Pennington",3431,33332)
!   fn_setup_client_add("Petromark",3535,33620)
    fn_setup_client_add("Philo",3534,34150)
!   fn_setup_client_add("PiattCO",3536,20832)
    fn_setup_client_add("Purdy",3610,34570)
    fn_setup_client_add("Raymond",3660,32798)
    fn_setup_client_add('R R Crawford',760,12466)  ! owns a system, but is stalling support until their old stuff breaks.
    fn_setup_client_add('Sheila',770,0)
    fn_setup_client_add("Thomas Richardson",3720,7718)
!   fn_setup_client_add("Riverside",3725,18332)
!   fn_setup_client_add("Sangamon",3815,34066)
    fn_setup_client_add("Scottville Rural",3840,33390)
    fn_setup_client_add("Starr County Gas",4127,33390)
    fn_setup_client_add("Thayer",4245,32800)
    fn_setup_client_add("Thomasboro",4260,34068)
    fn_setup_client_add("Unity",4380,34478)
!   fn_setup_client_add("Washington Parrish",4510,34116)
!   fn_setup_client_add("Waverly",4515,34430)
!   fn_setup_client_add("West Accounting",4560,30176)   retired as of 02/22/2018
!   fn_setup_client_add("West Rest Haven",4567,34032)
    fn_setup_client_add("White Hall",4625,32242)
!   fn_setup_client_add("Willard",4650,33514)
    fn_setup_client_add("World Wide",4650,33604)
    fn_setup_client_add("Zaleski",4710,34164)
    ! fn_setup_client_add("Demo",20001,34450)
    ! fn_setup_client_add("Demo",20002,34450)
    ! fn_setup_client_add("Demo",20003,34450)
    ! fn_setup_client_add("Demo",20004,34450)
  end if  ! ~setup_client
  ! 
fnend
def fn_getClientLicense(mat client_has$)
  if setup_client_has$<>env$('client') then
    setup_client_has$=env$('client')
    mat client_has$(0)
    ! r: big if statement 
    client_has_system_count=0
    if env$('client')='ACS' then 
      ! fn_add_ch_sys('UB')
      ! fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
      ! fn_add_ch_sys('CL')
      ! fn_add_ch_sys('PR')
      ! fn_add_ch_sys('GL')
      ! fn_add_ch_sys('OE')
      fn_add_ch_sys('TM')
    ! else if env$('client')='Albany' then ! demo undelivered - not on support but needed to debug Past Due Trun Off List from ACS 4 - test in ACS 5 locally
    !   fn_user_limit(1)
    !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('UB') : fn_set_ub_limit(500)
    !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('U4')
    else if env$('client')='BRCorp' then 
      fn_user_limit(99)
      fn_add_ch_sys('OE') 
    else if env$('client')='Ed Horton' then 
      fn_user_limit(1)
      fn_add_ch_sys('CL')
      fn_add_ch_sys('GL')
      fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
      fn_add_ch_sys('PR')
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Ash Grove' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
      ! canceled 2/7/2018 as per Debbie  -   fn_add_ch_sys('PR')
      ! canceled 2/7/2018 as per Debbie  -   fn_add_ch_sys('GL')
      ! canceled 2/7/2018 as per Debbie  -   fn_add_ch_sys('CL')
    else if env$('client')='Bethany' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
      fn_add_ch_sys('PR')
      fn_add_ch_sys('GL')
      fn_add_ch_sys('CL')
    else if env$('client')='Brier Lake' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
    else if env$('client')='Billings' and (env$('Unique_Computer_Id')="58973139-FC9B-1A95-F234-C145E2B22211" or env$('Unique_Computer_Id')="50A59A38-38BF-A82F-9868-04C4E5DD281A") then ! Limit to only UB stuff for (Katrina or Gale)
      fn_user_limit(2) ! actually licensed for 3 users, but has two separate installations
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
      !     fn_add_ch_sys('CR')
    else if env$('client')='Billings' and env$('Unique_Computer_Id')="BD04113D-C102-BA29-78AC-D23201FDC70C" then ! Limit to NOT UB stuff for Chris Hopkins
      fn_user_limit(1) ! actually licensed for 3 users, but has two separate installations
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Billings' then 
      fn_user_limit(3)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
      !     fn_add_ch_sys('CR')
    else if env$('client')='Blucksberg' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
      fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Campbell' then 
      fn_user_limit(4)
      fn_add_ch_sys('CL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Badger Beacon"
    else if env$('client')='Carr Plumbing' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR')
    else if env$('client')='Chatom' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Cerro Gordo' then 
      fn_user_limit(2)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Cerro Gordo T' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Choctaw' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('GL')
    else if env$('client')='Edinburg' then 
      fn_user_limit(2)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
      fn_add_ch_sys('U4') : u4_device$="Boson" ! ACEECA MEZ 1500 ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Edison' then 
      fn_user_limit(3)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Exeter' then 
      fn_user_limit(2)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
    else if env$('client')='Energy Exchanger' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR')
      ! if days(date$)<=days('05/18/2017','mm/dd/ccyy') then let fn_add_ch_sys('P4')
      if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('CL')
      if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('GL')
    else if env$('client')='Dorothy Salch' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
    else if env$('client')='Evelyn Pareya' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
    else if env$('client')='Findlay' then 
      fn_user_limit(2)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
      !   else if env$('client')='Franklin and Son' then 
      !     fn_user_limit(1)
      !     fn_add_ch_sys('PR')
    else if env$('client')='Franklin Co Hosp' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('CL')
    else if env$('client')='French Settlement' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
    else if env$('client')='Galena' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      !   else if env$('client')='Granby' then 
      !     fn_user_limit(2)
      !     fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
    else if env$('client')='Grandview' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
    else if env$('client')='Payroll Done Right' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
    else if env$('client')='GreenCo' then 
			! if days(date$)<=days('08/31/2018','mm/dd/ccyy') then 
				fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
				fn_add_ch_sys('U4') : u4_device$="EZReader" ! U4 Utility Billing Hand Held Add-On
			! end if
    else if env$('client')='Hope Welty' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Kathys Bookkeeping' then 
      fn_user_limit(2)
      fn_add_ch_sys('GL')
      !     fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
      fn_add_ch_sys('PR')
      !     fn_add_ch_sys('P4')
    else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='1478AEE0-5BCB-11D9-B0AC-BCAEC5EA1947' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR')
      !   else if env$('client')='Kincaid' and and env$('Unique_Computer_Id')='XXX need to do XXX' then
      !     fn_user_limit(1)
      !     fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
    else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='03000200-0400-0500-0006-000700080009' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Kincaid' then 
      fn_user_limit(2)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
      !     fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
    else if env$('client')='Lovington' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
      fn_add_ch_sys('U4') : u4_device$="Sensus" ! U4 Utility Billing Hand Held Add-On
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Merriam Woods' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U5') ! UB External Collections Processing
      !     fn_add_ch_sys('CR')
      !     fn_add_ch_sys('GL')
      !     fn_add_ch_sys('PR')
      !     fn_add_ch_sys('CL')
    else if env$('client')='Millry' then 
      fn_user_limit(4)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Morrisonville' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$="EZReader" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Moweaqua' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
      fn_add_ch_sys('U4') : u4_device$="Badger Connect C" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Pennington' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
    else if env$('client')='Purdy' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$="Aclara" ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Sheila' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$="Aclara" ! U4 Utility Billing Hand Held Add-On
      fn_add_ch_sys('CL')
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
    else if env$('client')='Omaha' then 
      if days(date$)<=days('03/03/2018','mm/dd/ccyy') then let fn_user_limit(3) else let fn_user_limit(1) ! 2 user bonus for 60 days
      fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
    else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0043-4210-8058-C8C04F423432' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On          FREE TRIAL PERIOD
    else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0032-5910-804C-B3C04F585131' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR')
    else if env$('client')='Raymond' and env$('Unique_Computer_Id')='C55D3F13-A162-E111-8430-DC0EA14AC3F6' then ! ACS Test Laptop QOSMIO X775
      fn_user_limit(1)
      fn_add_ch_sys('PR')
    else if env$('client')='Raymond' then 
      fn_user_limit(2)
      fn_add_ch_sys('PR')
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='R R Crawford' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR')   ! R R Crawford decided to cancel/stall their conversoin until their old system breaks.  At which time they must resume support and may need check format converted.
      fn_add_ch_sys('GL')   ! R R Crawford decided to cancel/stall their conversoin until their old system breaks.  At which time they must resume support and may need check format converted.
    else if env$('client')='Scottville Rural' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
    else if env$('client')='Starr County Gas' then 
      fn_user_limit(1)
      if days(date$)<=days('04/15/2018','mm/dd/ccyy') then let fn_add_ch_sys('UB') : fn_set_ub_limit(9999)
    else if env$('client')='Thayer' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On
    else if env$('client')='Thomasboro' then 
      fn_user_limit(1)
      fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
      fn_add_ch_sys('U4') : u4_device$='Badger Connect C' ! U4 Utility Billing Hand Held Add-On
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
      fn_add_ch_sys('CL')
    else if env$('client')='Thomas Richardson' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
      fn_add_ch_sys('PR')
    !   else if env$('client')='Waverly' then 
    !     fn_user_limit(1)
    !     fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
    ! else if env$('client')='West Accounting' then 
    !   fn_user_limit(1)
    !   fn_add_ch_sys('PR')
    else if env$('client')='White Hall' then 
      fn_user_limit(2)
      fn_add_ch_sys('UB')
      !   else if env$('client')='Willard' then 
      !     fn_user_limit(1)
      !     fn_add_ch_sys('GL')
      !     fn_add_ch_sys('PR')
      !     fn_add_ch_sys('CL')
    else if env$('client')='World Wide' then 
      fn_user_limit(1)
      fn_add_ch_sys('GL')
    else if env$('client')='Zaleski' then 
      fn_user_limit(1)
      fn_add_ch_sys('PR') 
      fn_add_ch_sys('GL') 
      fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
    end if 
    ! /r
  end if
fnend
def fn_setup_client_add(sca_name$*18,sca_customer_number,sca_brserial_number)
  client_count+=1
  mat client_name$(client_count)
  mat client_cno(client_count)
  mat client_brserial(client_count)
  client_name$(client_count)=sca_name$
  client_cno(client_count)=sca_customer_number
  client_brserial(client_count)=sca_brserial_number
fnend 
def library fnclient_has_mat(mat c_has$) ! returns a list of system each client owns
  fn_setup
  fnclient_has_mat=fn_client_has_mat(mat c_has$)
fnend 
def fn_client_has_mat(mat c_has$)
  if env$('client')='' then pr 'fn_client_has_mat called but env client not set.' : pause 
  fn_getClientLicense(mat client_has$)
  mat c_has$(udim(mat client_has$))
  mat c_has$=client_has$
  fn_client_has_mat=client_has_system_count
fnend 
def library fnclient_has(ch_sys$*2)
  fn_setup
  fnclient_has=fn_client_has(ch_sys$)
fnend 
def fn_client_has(ch_sys$*2)
  fn_getClientLicense(mat client_has$)
  ch_return=0
  if srch(mat client_has$,uprc$(ch_sys$))>0 then ch_return=1
  fn_client_has=ch_return
fnend 
def fn_user_limit(userLimit)
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
    setenv('user_limit',str$(userLimit))
  end if 
  ! 
fnend 
def fn_userCount
  ucReturn=0
  exec 'status users >'&env$('temp')&'\acsUsers'&session$&'.tmp'
  open #hStUsers:=fngethandle: 'name='&env$('temp')&'\acsUsers'&session$&'.tmp',d,input
  dim ucLine$*256
  ucListStarted=0
  do
    linput #hStUsers: ucLine$ eof UcEof
    if lwrc$(trim$(ucLine$))='current users on network:' then 
      ucListStarted=1
    else if ucListStarted then
      ! fnaddonec(mat activeUsers$,ucLine$)
      if lwrc$(trim$(ucline$(pos(ucline$,' '):len(ucline$))))=lwrc$(env$('client')) then
        ucReturn+=1
      end if
    end if
  loop
  UcEof: !
  close #hStUsers:
  fnfree(env$('temp')&'\acsUsers'&session$&'.tmp')
  fn_userCount=ucReturn
fnend
! r: def library fnuser_limit_exceeded
! fn_setup
! fn_getClientLicense(mat client_has$)
!   fnuser_limit_exceeded=user_limit_exceeded
! /r fnend
def fn_add_ch_sys(ch_item$)
  client_has_system_count=udim(mat client_has$)
  if client_has_system_count=0 then 
    client_has_system_count=2
    mat client_has$(1)
    client_has$(1)='CO'
  else 
    client_has_system_count+=1
  end if 
  mat client_has$(client_has_system_count)
  client_has$(client_has_system_count)=ch_item$
fnend 
def library fnregistered_for_hh
  fn_setup
  fn_getClientLicense(mat client_has$)
  fnregistered_for_hh=fn_client_has('U4') ! fn_registered_for_hh
fnend 
def library fnregistered_for_job_cost_pr
  fn_setup
  fn_getClientLicense(mat client_has$)
  fnregistered_for_job_cost_pr=fn_client_has('P4') ! fn_registered_for_job_cost_pr
fnend
def library fnhand_held_device$*20
  fn_setup
  fn_getClientLicense(mat client_has$)
  fnhand_held_device$=fn_hand_held_device$
fnend 
def fn_hand_held_device$*20
  dim u4_device$*20,u4_deviceDefault$*20
  u4_deviceDefault$=u4_device$
  fnreg_read('Hand Held Device',u4_device$,u4_deviceDefault$)
  if u4_device$='' then u4_device$=u4_deviceDefault$ ! in case it's been set and then blanked out to force default for client
  fn_hand_held_device$=u4_device$
fnend 
def fn_set_ub_limit(x)
  gUbLimit=x
  setenv('UB_Limit',str$(gUbLimit))
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
    fn_upp_add("Ash Grove","ubprtfull_ashgrove")
  !     fn_upp_add("Ashland","ubprtbl1_ashland")
    fn_upp_add("Bethany","ubprtbl1_Bethany")
    fn_upp_add("Campbell","(basic)")  ! derived from printbill_french_settlement_gas which should still work too
    fn_upp_add("Chatom","ubprtbl1_chatom")
    fn_upp_add("Divernon","ubprtbl1_div")
    fn_upp_add("Edinburg","(basic)") ! "ubprtbl1_edi")
    fn_upp_add("Edison","(basic)")
    fn_upp_add("Exeter","(basic)")
    fn_upp_add("Findlay","ubprtbl1_fin")
!   fn_upp_add("Franklinton","ubprtbl1_Franklinton")
    fn_upp_add('French Settlement','(basic)') ! 'printbill_french_settlement_gas'
    fn_upp_add("Grandview","ubprtbl1_gra")
    fn_upp_add("GreenCo",'(basic)') ! "ubprtbl1_greenco"
    fn_upp_add("Kincaid","ubprtbl1_kin")
    ! fn_upp_add("Kimberling","ubprtbl1_Kimberling") ! these are unused but also a nice 4 per page bill that looks pretty comprehensive - move the logic to (basic) if used elsewhere
    ! fn_upp_add("Illiopolis","ubprtbl1_Illiopolis")
    fn_upp_add("Lovington","ubprtbl1_Lovington")
    fn_upp_add("Loma Linda","ubprtbl1_ll")
    fn_upp_add("Merriam Woods","(basic)") ! "PrintBill_Merriam_Woods" ! "ubprtbl1_Franklinton")
    fn_upp_add("Millry","ubprtbl1_millry")
!   fn_upp_add("Monticello","ubprtbl1_montic")
    fn_upp_add("Moweaqua","PrintBill_Moweaqua")
!   fn_upp_add("Oakland","ubprtbl1_Oakland")
    fn_upp_add("Raymond",'(basic)') ! "ubprtbl1_Raymond")
    fn_upp_add("Thomasboro","ubprtbl1_tho")
    fn_upp_add("Thayer","ubprtbl1_thayer")
    fn_upp_add("Scottville Rural","ubprtbl1_scottville")
!   fn_upp_add("Gilbertown","ubprtbl1_Gilbertown")
!   fn_upp_add("Waverly","ubprtbl1_Waverly")
    fn_upp_add("White Hall","ubprtbl1_wh")
    fn_upp_add("Morrisonville","ubprtbl1_morrisonville")
    fn_upp_add("Purdy","ubprtbl1_purdy")
    fn_upp_add("Galena","ubprtbl1_galena")
! >>Bills-Laser (3 per page) ^ ubPrtThree
    fn_upp_add("Ash Grove","ubprtprace_ash")
!   fn_upp_add("Albany","ubprtthree_Albany")
    fn_upp_add("Brier Lake","ubprtthree_Brier")
    fn_upp_add("Billings","(basic)") ! ubprtthree_bill
    fn_upp_add("Cerro Gordo","ubprtlas_cerro")
    fn_upp_add("Choctaw",'(basic)') ! "ubprtlas_choctaw"
    ! fn_upp_add("Colyell","ubprtlas_colyell")
    ! fn_upp_add("Carrizo","ubprtthree_Carrizo")
    ! fn_upp_add("Ed","ubprtthree_barcode")
    ! fn_upp_add("Gilbertown","ubprtthree_Gilb")
    ! fn_upp_add("Granby","ubprt3prace_Granby")
    ! fn_upp_add("Riverside","ubprtthree_River")
    fn_upp_add("Omaha",'(basic)') ! "ubprtthree_Omaha")
    !   fn_upp_add("Sangamon","ubprtthree_san")
    ! >>Bills-Dot Matrix 4x6 ^ S:\acsUB\ubPrtBl14X6
    ! >>Bills-Dot Matrix 3.5x6 ^ S:\acsUB\Bill35X6
    ! >>Bills-Dot Matrix 3.5x7.5 ^ S:\acsUB\Bill35X75
    fn_upp_add("Pennington","(basic)") ! PrintBill_Pennington ! atlantis format - hits preprinted stock
    ! >>Bills-Dot Matrix Double Wide ^ S:\acsUB\billDouble
    ! >>Bills-Full Page ^ S:\acsUB\Ubprtfull
    fn_upp_add("Blucksberg",'(basic)') ! "PrintBill_Blucksberg")
    ! >>Bills-Miscellaneous ^ S:\acsUB\Ubprtful
    ! 
    mat ub_printbill_client$(ub_printbill_count)
    mat ub_printbill_program$(ub_printbill_count)
    ! 
  end if  ! /r
  upp_return$='S:\Core\Menu.br'
    ua_which=srch(mat ub_printbill_client$,env$('Client'))
    if ua_which>0 then 
      upp_return$=ub_printbill_program$(ua_which)
    else 
      msgbox('Your Utility Bill settings could not be determined.  Please contact ACS at 1-800-643-6318.')
    end if 
  !        (basic) should be fine.   end if 
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
def library fnpayroll_client_state$*2
  if ~pcs_setup then ! r:
    pcs_setup=1
    fn_setup
    dim pcs_return$*2
    dim pr_clientstate_client$(1)*128
    dim pr_clientstate_state$(1)*2
    mat pr_clientstate_client$(999)
    mat pr_clientstate_state$(999)
    pr_clientstate_count=0
    !   fn_upp_add("Ash Grove","ubprtlas_ashgrove")
    fn_pcs_add("ACS","AR")
    fn_pcs_add("Lamar",'MS')
    fn_pcs_add("Ash Grove",'MO')
    !   fn_pcs_add("Battlefield",'MO')
    fn_pcs_add("Bethany",'IL')
    fn_pcs_add("Billings",'MO')
    fn_pcs_add("Campbell",'MO')
    fn_pcs_add("Carr Plumbing",'AR')
    fn_pcs_add("Cerro Gordo",'IL')
    fn_pcs_add("Cerro Gordo T",'IL')
    !   fn_pcs_add("Community Dev",'TN')
    fn_pcs_add("Divernon",'IL')
    fn_pcs_add("Durden",'LA')
    fn_pcs_add("Edinburg",'IL')
    fn_pcs_add("Edison",'GA')
    fn_pcs_add("Ed Horton",'IL')
    fn_pcs_add("Energy Exchanger",'OK')
!   fn_pcs_add("Franklin and Son",'AR')
!   fn_pcs_add("Franklinton",'LA')
    fn_pcs_add("Galena",'MO')
!   fn_pcs_add("GreenCo",'MO')
    fn_pcs_add("Hope Welty",'IL')
    fn_pcs_add("Kincaid",'IL')
    ! fn_pcs_add("Kimberling",'MO')
    fn_pcs_add("Kathys Bookkeeping",'OK')
    fn_pcs_add("Lovington",'IL')
    fn_pcs_add("Merriam Woods",'MO')
!   fn_pcs_add("Monticello",'IL')
!   fn_pcs_add("Nancy Mouser",'OK')
!   fn_pcs_add("Northwest",'AR')
    fn_pcs_add("Oklahoma",'OK')
    fn_pcs_add("Payroll Done Right",'OR')
    fn_pcs_add("Philo",'IL')
!   fn_pcs_add("PiattCO",'IL')
    fn_pcs_add("Raymond",'IL')
    fn_pcs_add("R R Crawford",'KY')
!   fn_pcs_add("Riverside",'IN') ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
    fn_pcs_add("Sheila",'MO')
    fn_pcs_add("Thomas Richardson",'LA')
    fn_pcs_add("Thomasboro",'IL')
    fn_pcs_add("Unity",'IL')
!   fn_pcs_add("Washington Parrish",'LA')
!   fn_pcs_add("West Rest Haven",'')
!   fn_pcs_add("West Accounting",'OR')
    fn_pcs_add("Zaleski",'TX')
    mat pr_clientstate_client$(pr_clientstate_count)
    mat pr_clientstate_state$(pr_clientstate_count)
  !
  end if  ! /r
  pcs_return$=''
  pcs_which=srch(mat pr_clientstate_client$,env$('Client'))
  if pcs_which>0 then 
    pcs_return$=pr_clientstate_state$(pcs_which)
  else 
    msgbox('Your Payroll State could not be determined.  Please contact ACS at 1-800-643-6318.')
  end if 
  !
  fnpayroll_client_state$=pcs_return$
fnend 
def fn_pcs_add(pa_client$*128,pa_state$*2)
  pr_clientstate_count+=1
  pr_clientstate_client$(pr_clientstate_count)=pa_client$
  pr_clientstate_state$(pr_clientstate_count)=pa_state$
fnend 
def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
  fn_setup
  fnclient_has_on_support_list=fn_client_has_on_support_list(mat chosl_list$, chosl_grace_days)
fnend 
def fn_client_has_on_support_list(mat chosl_list$; chosl_grace_days)
  dim chosl_owns_list$(0)*2
  chosl_count=0
  chosl_owns_count=fn_client_has_mat(mat chosl_owns_list$)
  for chosl_item=1 to chosl_owns_count
    if fn_client_has_on_support_item(chosl_owns_list$(chosl_item), chosl_grace_days) then 
      chosl_count+=1
      mat chosl_list$(chosl_count)
      chosl_list$(chosl_count)=chosl_owns_list$(chosl_item)
    end if 
  next chosl_item
  fn_client_has_on_support_list=chosl_count
fnend 
def library fnclient_has_on_support_item(chosi_item$*2; days_grace)
  fn_setup
  fnclient_has_on_support_item=fn_client_has_on_support_item(chosi_item$, days_grace)
fnend 
def fn_client_has_on_support_item(chosi_item$*2; days_grace)
  client_id=val(env$('Client_ID'))
  fn_client_support_setup(client_id,mat chosi_system_id$,mat chosi_system_support_end_date,mat chosi_on_support, days_grace)
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
  client_id=val(env$('Client_ID'))
  fn_client_support=fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
fnend 
def fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_days_grace)
  ! css_days_grace=day grace period to allow users to update after support has expired.
  if css_setup<>client_id then ! r:
    cache_css_client_owns_count=fn_client_has_mat(mat css_client_owns$)
        mat css_client_owns$(cache_css_client_owns_count)
        mat css_system_id$(cache_css_client_owns_count)
        mat css_system_id$=css_client_owns$
    !
    mat cache_css_system_id$(0)
    mat cache_css_system_sup_end_date(0)
    cache_css_system_count=0
    open #h_support:=fngethandle: "Name=S:\Core\Data\ACS_Sup.dat,Version=2,KFName=S:\Core\Data\ACS_Sup.idx,version=0,Shr",internal,input,keyed 
    restore #h_support: ! ,key>==lpad$(trim$(client_id$),kln(h_support)):
    do 
      read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst eof CSS_SUPPORT_EOF
      F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
      cln=val(cln$)
      if cln=client_id then 
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
    css_setup=client_id
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
def library fnsystem_code_standardize$(st_code$*2)
  fn_setup
  fnsystem_code_standardize$=fn_system_code_standardize$(st_code$)
fnend 
def fn_system_code_standardize$(st_code$*2)
  ! this function is to translate from systems.h420 to
  ! cursys type codes
  ! 
  st_code$=uprc$(st_code$)
  ! if st_code$='G1' then st_code$='GL'
  if st_code$='G1' then st_code$='GL'
  if st_code$='G3' then st_code$='G2' ! Accountant's GL Add On
  if st_code$='U1' then st_code$='UB' ! UB No Discount
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
def fn_client_is_converting
  cic_return=0
  if env$('ACSDeveloper')<>'' then
    cic_return=1
  else if env$('client')='R R Crawford'       and days(date$)<=days('12/31/2020','mm/dd/ccyy') then ! just testing
    cic_return=1
  else if env$('client')='Payroll Done Right' and days(date$)<=days('07/01/2018','mm/dd/ccyy') then ! just testing
    cic_return=1
  else if env$('client')='Kathys Bookkeeping' and days(date$)<=days('01/15/2019','mm/dd/ccyy')  then
    cic_return=1
  end if 
  fn_client_is_converting=cic_return
fnend 
