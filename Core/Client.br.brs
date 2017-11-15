02000 ! Replace S:\Core\Client.br
02020 ! we use this library to tell programs which client is using the system
02040 if env$('enableClientSelection')='Yes' then goto ClientSelect
02500 !   ! r: sandbox for testing local functions
03000 !     if env$('ACSDeveloper')<>'' then let setenv('acsclient','BRCorp')
03020 !     fn_setup
03400 !     pr 'fn_client_has_mat returns ';fn_client_has_mat(mat tmp$)
03420 !     pr mat tmp$
03500 !     pr 'fn_client_support returns ';fn_client_support(mat tmp_system_id$,mat tmp_system_support_end_date,mat tmp_on_support)
03520 !     for x=1 to udim(mat tmp_system_id$)
03540 !       pr tmp_system_id$(x); tmp_system_support_end_date(x); tmp_on_support(x)
03560 !     nex x
04000 !     pr 'fn_client_has_on_support_list(mat tmp$,45) returns ';fn_client_has_on_support_list(mat tmp$,45);' with 45 grace days'
04020 !     pr mat tmp$
04040 !     pr 'fn_client_has_on_support_list(mat tmp$,0) returns ';fn_client_has_on_support_list(mat tmp$,0);' with 0 grace days'
04060 !     pr mat tmp$
05000 !   end ! /r
06000 !   !  it is now ...  !!!     pr program$&' is not intended to be run directly' : end 
11000 ClientSelect: ! r:
11020   fntop(program$)
11040   fn_setup
11060   fn_clientSelect
11070 goto XIT ! /r
11080 XIT: fnXit
13000 def library fnClientSelect
13020   fn_setup
13060   ! fn_getClientLicense(mat client_has$)
13080   fnClientSelect=fn_clientSelect
13100 fnend
14000 def fn_clientSelect
14020   fntos('clientSelect') ! r:
14040   dim flexItem$(4)*256
14060   mat clientSelectHeading$(4)
14080   clientSelectHeading$(1)='Name'
14100   clientSelectHeading$(2)='Number'
14120   clientSelectHeading$(3)='brSerial'
14140   clientSelectHeading$(4)='Licenses'
14160   mat clientSelectMask$(4)
14180   clientSelectMask$(1)=''
14200   clientSelectMask$(2)=''
14220   clientSelectMask$(3)=''
14240   clientSelectMask$(4)=''
14260   mat flexItem$(4)
14280   fnflexinit1('clientSelect1',2,1,10,10,mat clientSelectHeading$,mat clientSelectMask$)
14300   for clientItem=1 to udim(mat client_name$)
14320     flexItem$(1)=client_name$(clientItem)
14340     flexItem$(2)=str$(client_cno(clientItem))
14360     flexItem$(3)=str$(client_brserial(clientItem))
14380     flexItem$(4)=client_name$(clientItem)
14400     fnflexadd1(mat flexItem$)
14420   nex clientItem
14440   fncmdset(2)
14460   fnacs('clientSelect',0,mat resp$,ckey) ! /r
15000   if ckey=1 then ! r: select that client
15060     dim dataNew$*256
15100     setenv('Client',resp$(1)) ! pr 'env$ client set to '&env$('client') : pause
15102     setenv('clientSelected',env$('Client'))
15104     fnmcreg_write('clientSelected',env$('clientSelected'))
15110     if env$('enableDataFolderByClient')='Yes' then
15120       dataNew$=rtrm$(env$('QBase'),'\')&'\'&env$('client') ! &'\'
15176       fnmakesurepathexists(dataNew$)
15180       setenv('data',dataNew$) ! pr 'env$ client set to '&env$('client') : pause
15190       fnreg_close
15200       ! fnMapToVirturalDrive(dataNew$,'Q:') 
15202       fnSetQ(dataNew$)
15280     end if
15300   end if ! /r 
15320 fnend
16000 def fn_setup
16020   if ~setup_library then 
16030     setup_library=1
16032     library 'S:\Core\Library': fnerror,fngethandle,fnreg_read
16034     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fncmdset,fntop,fnacs
16036     library 'S:\Core\Library': fnXit
16038     library 'S:\Core\Library': fnreg_close
16040     library 'S:\Core\Library': fnSetQ
16042     library 'S:\Core\Library': fnsrch_case_insensitive
16044     library 's:\Core\Library': fnmakesurepathexists
16046     library 's:\Core\Library': fnmcreg_write
16048     library 's:\Core\Library': fnfree
16080   end if 
16090   fn_setup_client
16100 fnend 
18000 def library fnclient$*18
18020   fn_setup
18040   fnclient$=fn_client$
18060 fnend 
20000 def fn_client$*18
20020   ! if env$('ACSDeveloper')<>'' then pr 'on the way in env client$ is '&env$('client')
20040   on error goto ERTN
20060   dim clientReturn$*18
20080   ! r: derive client
20100   loginNameWhich=fnsrch_case_insensitive(mat client_name$,login_name$)
20120   serialWhich=srch(mat client_brserial,serial)
20140   clientWhich=0
20142   if env$('clientSelected')<>'' then ! it is specified in drive.sys - default to it
20144     clientWhich=srch(mat client_name$,env$('clientSelected'))
20146     if clientWhich<=0 then pr 'invalid env clientSelected.' : pause
20160   else if env$('acsClient')<>'' then ! it is specified in drive.sys - default to it
20180     clientWhich=srch(mat client_name$,env$('acsClient'))
20200     if clientWhich<=0 then pr 'invalid env acsClient.' : pause
20220   else if loginNameWhich>0 then
20240     clientWhich=loginNameWhich
20260   else if serialWhich>0 then
20280     clientWhich=serialWhich
20300   end if
20320   setenv('Client',client_name$(clientWhich))
20340   setenv('Client_ID',str$(client_cno(clientWhich)))
20360   if env$('client')='' then
20380     pr "env$('client') is blank." : pause
20400   end if
20420   ! /r 
20440   fn_getClientLicense(mat client_has$)
20460   if srch(mat client_name$,login_name$)>0 then
20480     clientReturn$=login_name$
20500   else
20520     clientReturn$=env$('client')
20540     client_which=srch(mat client_name$,env$('client'))
20560     if client_which>0 then 
20580       setenv('Client_ID',str$(client_cno(client_which)))
20600     else
20620       pr 'env: Client: "'&env$('client')&'" did not match any entries Mat client_name$.  env: Client_ID could not be set'
20640       pause
20660       setenv('Client_ID','')
20680     end if
20700   end if
20720   ! if env$('ACSDeveloper')<>'' then pr 'clientReturn$='&clientReturn$ : pause
20740   fn_client$=clientReturn$
20760 fnend 
24000 ! <Updateable Region: ERTN>
24020 ERTN: fnerror(program$,err,line,act$,"NO")
24040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
24060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
24080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
24100 ERTN_EXEC_ACT: execute act$ : goto ERTN
24120 ! /region
26000 def fn_setup_client ! ** set up for new clients
26010   if ~setup_client then 
26020     setup_client=1
26030     dim client_name$(1)*18
26040     client_count=0
26050     mat client_name$(client_count)
26060     mat client_cno(client_count)
26070     mat client_brserial(client_count)
26090     !  fn_setup_client_add("ACS",1,0) ! TEMP
26100     ! 
26110     fn_setup_client_add("ACS" ,420,34660) ! 58650
26120     fn_setup_client_add("Ed Horton" ,5535,0)! Ed processes like ACS
26130     ! 
26140     fn_setup_client_add("Lamar" ,1,33854)
26150     !   fn_setup_client_add("Albany" ,190,15376) ! notifed me 9/22/15 that they were switching UB providers
26160     fn_setup_client_add("Ash Grove" ,286,19016)
26170     !   fn_setup_client_add("Ashland" ,300,33584)
26180     !   fn_setup_client_add("Battlefield" ,369,33306)
26190     fn_setup_client_add("Bethany" ,380,34326)
26200     fn_setup_client_add("Billings" ,440,33534)
26210     fn_setup_client_add("Blucksberg" ,465,34564)
26220     !   fn_setup_client_add("Brazeal" ,570,34418)
26230     fn_setup_client_add("Brier Lake" ,578,20306)
26240     fn_setup_client_add("BRCorp" ,7000,50775) ! 50775 is actually DAVID KALINSKI PERSONAL COPY, but it is what Gordon is using.
26250     fn_setup_client_add("Campbell" ,700,33942)
26260     fn_setup_client_add("Carr Plumbing" ,780,34610)
26270     ! fn_setup_client_add("Carrizo" ,800,34416)
26280     fn_setup_client_add("Cerro Gordo" ,850,34508) ! 33994)
26290     fn_setup_client_add("Cerro Gordo T" ,970,34508)
26300     fn_setup_client_add("Chatom" ,911,15678)
26310     fn_setup_client_add("Choctaw" ,918,34214)
26320     ! fn_setup_client_add("Colyell" ,980,33948)
26330     ! fn_setup_client_add("Community Dev" ,982,34156)
26340     fn_setup_client_add("Divernon" ,1350,33698)
26350     fn_setup_client_add("Dorothy Salch" ,3812,34494)
26360     fn_setup_client_add("Durden" ,1406,16410)
26370     fn_setup_client_add("Edinburg" ,1478,34022)
26380     fn_setup_client_add("Edison" ,1480,34022)
26390     ! fn_setup_client_add("Eldorado" ,1500,33352)
26400     fn_setup_client_add("Evelyn Pareya" ,3385,34366)
26410     fn_setup_client_add("Exeter" ,1615,31210)
26420     fn_setup_client_add("Energy Exchanger" ,1550,10172)
26430     !   fn_setup_client_add("FirstBaptist" ,1695,33380) ! <-- note it's the same as French Settlement - one of them is wrong, but First Baptist of Frnaklinton's license is 4.1 and not currently necessary, so just commenting them out for now.
26440     fn_setup_client_add("Findlay" ,1700,34132)
26450     !   fn_setup_client_add("Franklin and Son" ,1870,32454)
26460     fn_setup_client_add("Franklin Co Hosp" ,1876,33668)
26470     !   fn_setup_client_add("Franklinton" ,1876,0) ! Town of
26480     fn_setup_client_add("French Settlement" ,1880,33380)
26490     fn_setup_client_add("Fulton" ,1890,33720) ! Utilities Board
26500     fn_setup_client_add("Galena" ,1945,34566)
26510     fn_setup_client_add("Garrity",1950,0)
26520 !   fn_setup_client_add("Gilbertown",1985,0)
26530 !   fn_setup_client_add("Granby",2040,34098) ! no longer using as of 6/13/2016
26540     fn_setup_client_add("Grandview",2050,34040)
26550 !   fn_setup_client_add("GreenCo",2070,33910)
26560     fn_setup_client_add("Hope Welty" ,851,34152)
26562     fn_setup_client_add("Payroll Done Right" ,3393,0)
26570     ! fn_setup_client_add("Illiopolis",2340,0)
26580     fn_setup_client_add("Kathys Bookkeeping",3979,33672)
26590     ! fn_setup_client_add("Kimberling",2530,19212)
26600     fn_setup_client_add("Kincaid",2532,33652)
26610     fn_setup_client_add("Lovington",2689,32720)
26620     fn_setup_client_add("Loma Linda",2690,33244)
26630 !   fn_setup_client_add("Nancy Mouser",2795,34318)
26640     fn_setup_client_add("Merriam Woods",2900,31702)
26650 !   fn_setup_client_add("Miller Hardware",3005,14668)
26660     fn_setup_client_add("Millry",3025,33968)
26670 !   fn_setup_client_add("Monticello",3040,12196)
26680     fn_setup_client_add("Moweaqua",3045,34594) ! 200032790) ! 33986 <--??  I don't know where that came from - 200032790 was their 4.13 version
26690     fn_setup_client_add("Morrisonville",3050,34408) ! 32242  <-- that's white hall's but there was a mistake in license file for a while
26700 !   fn_setup_client_add("Northwest",3241,11176 )
26710 !   fn_setup_client_add("Oakland",3250,34260)
26720     fn_setup_client_add("Omaha",3320,33346)
26730     fn_setup_client_add("Pennington",3431,33332)
26740 !   fn_setup_client_add("Petromark",3535,33620)
26750     fn_setup_client_add("Philo",3534,34150)
26760 !   fn_setup_client_add("PiattCO",3536,20832)
26770     fn_setup_client_add("Purdy",3610,34570)
26780     fn_setup_client_add("Raymond",3660,32798)
26790     fn_setup_client_add('R R Crawford',760,12466)
26800     fn_setup_client_add("Thomas Richardson",3720,7718)
26810 !   fn_setup_client_add("Riverside",3725,18332)
26820 !   fn_setup_client_add("Sangamon",3815,34066)
26830     fn_setup_client_add("Scottville Rural",3840,33390)
26840     fn_setup_client_add("Thayer",4245,32800)
26850     fn_setup_client_add("Thomasboro",4260,34068)
26860     fn_setup_client_add("Unity",4380,34478)
26870 !   fn_setup_client_add("Washington Parrish",4510,34116)
26880 !   fn_setup_client_add("Waverly",4515,34430)
26890     fn_setup_client_add("West Accounting",4560,30176)
26900 !   fn_setup_client_add("West Rest Haven",4567,34032)
26910     fn_setup_client_add("White Hall",4625,32242)
26920 !   fn_setup_client_add("Willard",4650,33514)
26930     fn_setup_client_add("World Wide",4650,33604)
26940     fn_setup_client_add("Zaleski",4710,34164)
26950     ! fn_setup_client_add("Demo",20001,34450)
26960     ! fn_setup_client_add("Demo",20002,34450)
26970     ! fn_setup_client_add("Demo",20003,34450)
26980     ! fn_setup_client_add("Demo",20004,34450)
26990   end if  ! ~setup_client
27050   ! 
27060 fnend
28000 def fn_getClientLicense(mat client_has$)
28010   if setup_client_has$<>env$('client') then
28020     setup_client_has$=env$('client')
28030     mat client_has$(0)
28040     ! r: big if statement 
28042     client_has_system_count=0
28050     if env$('client')='ACS' then 
28051       ! fn_add_ch_sys('UB')
28052       ! fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
28053       ! fn_add_ch_sys('CL')
28054       ! fn_add_ch_sys('PR')
28055       ! fn_add_ch_sys('GL')
28056       ! fn_add_ch_sys('OE')
28058       fn_add_ch_sys('TM')
28090     ! else if env$('client')='Albany' then ! demo undelivered - not on support but needed to debug Past Due Trun Off List from ACS 4 - test in ACS 5 locally
28100     !   fn_user_limit(1)
28110     !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('UB') : fn_set_ub_limit(500)
28120     !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('U4')
28130     else if env$('client')='BRCorp' then 
28140       fn_user_limit(99)
28150       fn_add_ch_sys('OE') 
28160       ! else if env$('client')='Ed Horton' then 
28170       !   fn_user_limit(1)
28180       !   fn_add_ch_sys('CL')
28190       !   fn_add_ch_sys('GL')
28200       !   fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
28210       !   fn_add_ch_sys('PR')
28220       !   fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28230       !   fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
28240     else if env$('client')='Ash Grove' and (env$('Unique_Computer_Id')="27932D73-8B8B-4D40-943D-08EFB9E5CFE9" or env$('Unique_Computer_Id')="4C4C4544-0037-3610-8058-C8C04F4E5731") then ! Melissa on Windows XP mode and Local Server Machine only uses UB
28250       fn_user_limit(1)
28260       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28270       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28280     else if env$('client')='Ash Grove' and env$('Unique_Computer_Id')="4C4C4544-0054-3710-8058-C7C04F4E5731" then ! Debbie
28290       fn_user_limit(1)
28300       fn_add_ch_sys('PR')
28310       fn_add_ch_sys('GL')
28320       fn_add_ch_sys('CL')
28330     else if env$('client')='Ash Grove' then 
28340       fn_user_limit(1)
28350       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28360       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28370       fn_add_ch_sys('PR')
28380       fn_add_ch_sys('GL')
28390       fn_add_ch_sys('CL')
28400     else if env$('client')='Bethany' then 
28410       fn_user_limit(1)
28420       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28430       fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
28440       fn_add_ch_sys('PR')
28450       fn_add_ch_sys('GL')
28460       fn_add_ch_sys('CL')
28470     else if env$('client')='Brier Lake' then 
28480       fn_user_limit(1)
28490       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28500     else if env$('client')='Billings' and (env$('Unique_Computer_Id')="58973139-FC9B-1A95-F234-C145E2B22211" or env$('Unique_Computer_Id')="50A59A38-38BF-A82F-9868-04C4E5DD281A") then ! Limit to only UB stuff for (Katrina or Gale)
28510       fn_user_limit(2) ! actually licensed for 3 users, but has two separate installations
28520       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28530       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28540       !     fn_add_ch_sys('CR')
28550     else if env$('client')='Billings' and env$('Unique_Computer_Id')="BD04113D-C102-BA29-78AC-D23201FDC70C" then ! Limit to NOT UB stuff for Chris Hopkins
28560       fn_user_limit(1) ! actually licensed for 3 users, but has two separate installations
28570       fn_add_ch_sys('GL')
28580       fn_add_ch_sys('PR')
28590       fn_add_ch_sys('CL')
28600     else if env$('client')='Billings' then 
28610       fn_user_limit(3)
28620       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28630       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28640       fn_add_ch_sys('GL')
28650       fn_add_ch_sys('PR')
28660       fn_add_ch_sys('CL')
28670       !     fn_add_ch_sys('CR')
28680     else if env$('client')='Blucksberg' then 
28690       fn_user_limit(1)
28700       fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
28710       fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
28720     else if env$('client')='Campbell' then 
28730       fn_user_limit(4)
28740       fn_add_ch_sys('CL')
28750       fn_add_ch_sys('PR')
28760       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28770     else if env$('client')='Carr Plumbing' then 
28780       fn_user_limit(1)
28790       fn_add_ch_sys('PR')
28800     else if env$('client')='Chatom' then 
28810       fn_user_limit(1)
28820       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28830       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28840     else if env$('client')='Cerro Gordo' then 
28850       fn_user_limit(1)
28860       fn_add_ch_sys('GL')
28870       fn_add_ch_sys('PR')
28880       fn_add_ch_sys('CL')
28890       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28900       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28910     else if env$('client')='Cerro Gordo T' then 
28912       fn_user_limit(1)
28920       fn_add_ch_sys('GL')
28930       fn_add_ch_sys('PR')
28940       fn_add_ch_sys('CL')
28950     else if env$('client')='Choctaw' then 
28960       fn_user_limit(1)
28970       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28980       fn_add_ch_sys('GL')
28990     else if env$('client')='Edinburg' then 
29000       fn_user_limit(2)
29010       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29020       fn_add_ch_sys('GL')
29030       fn_add_ch_sys('PR')
29040       fn_add_ch_sys('CL')
29050       fn_add_ch_sys('U4') : u4_device$="Boson" ! ACEECA MEZ 1500 ! U4 Utility Billing Hand Held Add-On
29060     else if env$('client')='Edison' then 
29070       fn_user_limit(2)
29080       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U3 Utility Billing (<500 Customers)
29090       fn_add_ch_sys('GL')
29100       fn_add_ch_sys('PR')
29110       fn_add_ch_sys('CL')
29120     else if env$('client')='Exeter' then 
29130       fn_user_limit(2)
29140       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29150     else if env$('client')='Energy Exchanger' then 
29160       fn_user_limit(1)
29170       fn_add_ch_sys('PR')
29180       ! if days(date$)<=days('05/18/2017','mm/dd/ccyy') then let fn_add_ch_sys('P4')
29190       if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('CL')
29200       if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('GL')
29210     else if env$('client')='Dorothy Salch' then 
29220       fn_user_limit(1)
29230       fn_add_ch_sys('GL')
29240       fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29250     else if env$('client')='Evelyn Pareya' then 
29260       fn_user_limit(1)
29270       fn_add_ch_sys('GL')
29280       fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29290     else if env$('client')='Findlay' then 
29300       fn_user_limit(2)
29310       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29320       fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
29330       !   else if env$('client')='Franklin and Son' then 
29340       !     fn_user_limit(1)
29350       !     fn_add_ch_sys('PR')
29360     else if env$('client')='Franklin Co Hosp' then 
29370       fn_user_limit(1)
29380       fn_add_ch_sys('GL')
29390       fn_add_ch_sys('CL')
29400     else if env$('client')='French Settlement' then 
29410       fn_user_limit(1)
29420       fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29430     else if env$('client')='Galena' then 
29440       fn_user_limit(1)
29450       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29460       !   else if env$('client')='Granby' then 
29470       !     fn_user_limit(2)
29480       !     fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29490     else if env$('client')='Grandview' then 
29492       fn_user_limit(1)
29494       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29500     else if env$('client')='Payroll Done Right' then 
29502       fn_user_limit(1)
29504       fn_add_ch_sys('GL')
29506       fn_add_ch_sys('PR')
29520     else if env$('client')='Hope Welty' then 
29530       fn_user_limit(1)
29540       fn_add_ch_sys('GL')
29550       fn_add_ch_sys('PR')
29560       fn_add_ch_sys('CL')
29570     else if env$('client')='Kathys Bookkeeping' then 
29580       fn_user_limit(2)
29590       fn_add_ch_sys('GL')
29600       !     fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29610       fn_add_ch_sys('PR')
29620       !     fn_add_ch_sys('P4')
29630     else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='1478AEE0-5BCB-11D9-B0AC-BCAEC5EA1947' then 
29640       fn_user_limit(1)
29650       fn_add_ch_sys('PR')
29660       !   else if env$('client')='Kincaid' and and env$('Unique_Computer_Id')='XXX need to do XXX' then
29670       !     fn_user_limit(1)
29680       !     fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29690     else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='03000200-0400-0500-0006-000700080009' then 
29700       fn_user_limit(1)
29710       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29720       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
29730     else if env$('client')='Kincaid' then 
29740       fn_user_limit(2)
29750       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29760       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
29770       !     fn_add_ch_sys('GL')
29780       fn_add_ch_sys('PR')
29790     else if env$('client')='Lovington' then 
29800       fn_user_limit(1)
29810       fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29820       fn_add_ch_sys('U4') : u4_device$="Sensus" ! U4 Utility Billing Hand Held Add-On
29830       fn_add_ch_sys('GL')
29840       fn_add_ch_sys('PR')
29850       fn_add_ch_sys('CL')
29860     else if env$('client')='Merriam Woods' then 
29870       fn_user_limit(1)
29880       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29890       fn_add_ch_sys('U5') ! UB External Collections Processing
29900       !     fn_add_ch_sys('CR')
29910       !     fn_add_ch_sys('GL')
29920       !     fn_add_ch_sys('PR')
29930       !     fn_add_ch_sys('CL')
29940     else if env$('client')='Millry' then 
29950       fn_user_limit(4)
29960       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29970       fn_add_ch_sys('U4') : u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
29980     else if env$('client')='Morrisonville' then 
29990       fn_user_limit(1)
30000       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30002       fn_add_ch_sys('U4') : u4_device$="EZReader" ! U4 Utility Billing Hand Held Add-On
30010     else if env$('client')='Moweaqua' then 
30020       fn_user_limit(1)
30030       fn_add_ch_sys('UB') : fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
30040       fn_add_ch_sys('U4') : u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30050     else if env$('client')='Pennington' then 
30060       fn_user_limit(1)
30070       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30080     else if env$('client')='Purdy' then 
30090       fn_user_limit(1)
30100       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30110       fn_add_ch_sys('U4') : u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
30120     else if env$('client')='Omaha' then 
30130       fn_user_limit(1)
30140       fn_add_ch_sys('UB') : fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
30150     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0043-4210-8058-C8C04F423432' then 
30160       fn_user_limit(1)
30170       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30180       fn_add_ch_sys('U4') : u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On          FREE TRIAL PERIOD
30190     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0032-5910-804C-B3C04F585131' then 
30200       fn_user_limit(1)
30210       fn_add_ch_sys('PR')
30220     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='C55D3F13-A162-E111-8430-DC0EA14AC3F6' then ! ACS Test Laptop QOSMIO X775
30230       fn_user_limit(1)
30240       fn_add_ch_sys('PR')
30250     else if env$('client')='Raymond' then 
30260       fn_user_limit(2)
30270       fn_add_ch_sys('PR')
30280       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30290       fn_add_ch_sys('U4') : u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30300     else if env$('client')='R R Crawford' then 
30310       fn_user_limit(1)
30320       fn_add_ch_sys('PR')
30330       fn_add_ch_sys('GL')
30340     else if env$('client')='Scottville Rural' then 
30350       fn_user_limit(1)
30360       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30370     else if env$('client')='Thayer' then 
30380       fn_user_limit(1)
30390       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30400       fn_add_ch_sys('U4') : u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30410     else if env$('client')='Thomasboro' then 
30420       fn_user_limit(1)
30430       fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30440       fn_add_ch_sys('U4') : u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30450       fn_add_ch_sys('GL')
30460       fn_add_ch_sys('PR')
30470       fn_add_ch_sys('CL')
30480     else if env$('client')='Thomas Richardson' then 
30490       fn_user_limit(1)
30500       fn_add_ch_sys('GL')
30510       fn_add_ch_sys('PR')
30520       !   else if env$('client')='Waverly' then 
30530       !     fn_user_limit(1)
30540       !     fn_add_ch_sys('UB') : fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30550     else if env$('client')='West Accounting' then 
30560       fn_user_limit(1)
30570       fn_add_ch_sys('PR')
30580     else if env$('client')='White Hall' then 
30590       fn_user_limit(2)
30600       fn_add_ch_sys('UB')
30610       !   else if env$('client')='Willard' then 
30620       !     fn_user_limit(1)
30630       !     fn_add_ch_sys('GL')
30640       !     fn_add_ch_sys('PR')
30650       !     fn_add_ch_sys('CL')
30660     else if env$('client')='World Wide' then 
30670       fn_user_limit(1)
30680       fn_add_ch_sys('GL')
30690     else if env$('client')='Zaleski' then 
30700       fn_user_limit(1)
30710       fn_add_ch_sys('PR') 
30720       fn_add_ch_sys('GL') 
30730       fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
30740     end if 
30750     ! /r
30760   end if
30770 fnend
31000 def fn_setup_client_add(sca_name$*18,sca_customer_number,sca_brserial_number)
31020   client_count+=1
31040   mat client_name$(client_count)
31060   mat client_cno(client_count)
31080   mat client_brserial(client_count)
31100   client_name$(client_count)=sca_name$
31120   client_cno(client_count)=sca_customer_number
31140   client_brserial(client_count)=sca_brserial_number
31160 fnend 
32000 def library fnclient_has_mat(mat c_has$) ! returns a list of system each client owns
32020   fn_setup
32040   fnclient_has_mat=fn_client_has_mat(mat c_has$)
32060 fnend 
34000 def fn_client_has_mat(mat c_has$)
34022   if env$('client')='' then pr 'fn_client_has_mat called but env client not set.' : pause 
34023   fn_getClientLicense(mat client_has$)
34040   mat c_has$(udim(mat client_has$))
34060   mat c_has$=client_has$
34100   fn_client_has_mat=client_has_system_count
34120 fnend 
36000 def library fnclient_has(ch_sys$*2)
36020   fn_setup
36040   fnclient_has=fn_client_has(ch_sys$)
36060 fnend 
36080 def fn_client_has(ch_sys$*2)
36093   fn_getClientLicense(mat client_has$)
36120   ch_return=0
36140   if srch(mat client_has$,uprc$(ch_sys$))>0 then ch_return=1
36160   fn_client_has=ch_return
36180 fnend 
38000 def fn_user_limit(userLimit)
38020   if env$('acsProduct')='ACS Online' then
38040     userCount=fn_userCount
38042 if env$('acsDeveloper')<>'' then pause
38060     if userCount>userLimit then
38080       user_limit_exceeded=1
38100       msgbox('Maximum number of licensed concurrent users ('&str$(userLimit)&') exceeded.')
38120       execute 'system logoff'
38140     end if
38160   else if val(env$('user_limit'))<>userLimit then
38200     execute 'config option 9 '&str$(userLimit)
38220     setenv('user_limit',str$(userLimit))
38240   end if 
38260   ! 
38280 fnend 
40000 def fn_userCount
40020   ucReturn=0
40040   exec 'status users >'&env$('temp')&'\acsUsers'&session$&'.tmp'
40060   open #hStUsers:=fngethandle: 'name='&env$('temp')&'\acsUsers'&session$&'.tmp',d,input
40080   dim ucLine$*256
40100   ucListStarted=0
40120   do
40140     linput #hStUsers: ucLine$ eof UcEof
40160     if lwrc$(trim$(ucLine$))='current users on network:' then 
40180       ucListStarted=1
40200     else if ucListStarted then
40220       ! fnaddonec(mat activeUsers$,ucLine$)
40240       if lwrc$(trim$(ucline$(pos(ucline$,' '):len(ucline$))))=lwrc$(env$('client')) then
40260         ucReturn+=1
40280       end if
40300     end if
40320   loop
40330   UcEof: !
40340   close #hStUsers:
40360   fnfree(env$('temp')&'\acsUsers'&session$&'.tmp')
40380   fn_userCount=ucReturn
40400 fnend
42000 ! r: def library fnuser_limit_exceeded
42020 ! fn_setup
42023 ! fn_getClientLicense(mat client_has$)
42040 !   fnuser_limit_exceeded=user_limit_exceeded
42060 ! /r fnend
44000 def fn_add_ch_sys(ch_item$)
44020   client_has_system_count=udim(mat client_has$)
44040   if client_has_system_count=0 then 
44060     client_has_system_count=2
44080     mat client_has$(1)
44100     client_has$(1)='CO'
44120   else 
44140     client_has_system_count+=1
44160   end if 
44180   mat client_has$(client_has_system_count)
44200   client_has$(client_has_system_count)=ch_item$
44220 fnend 
46000 def library fnregistered_for_hh
46002   fn_setup
46013   fn_getClientLicense(mat client_has$)
46020   fnregistered_for_hh=fn_client_has('U4') ! fn_registered_for_hh
46040 fnend 
48000 def library fnregistered_for_job_cost_pr
48002   fn_setup
48013   fn_getClientLicense(mat client_has$)
48020   fnregistered_for_job_cost_pr=fn_client_has('P4') ! fn_registered_for_job_cost_pr
48040 fnend
52000 def library fnhand_held_device$*20
52010   fn_setup
52023   fn_getClientLicense(mat client_has$)
52040   fnhand_held_device$=fn_hand_held_device$
52060 fnend 
52080 def fn_hand_held_device$*20
52082   dim u4_device$*20,u4_deviceDefault$*20
52084   u4_deviceDefault$=u4_device$
52090   fnreg_read('Hand Held Device',u4_device$,u4_deviceDefault$)
52092   if u4_device$='' then u4_device$=u4_deviceDefault$ ! in case it's been set and then blanked out to force default for client
52560   fn_hand_held_device$=u4_device$
52580 fnend 
54000 def fn_set_ub_limit(x)
54020   gUbLimit=x
54030   setenv('UB_Limit',str$(gUbLimit))
54040 fnend 
56000 def library fnub_printbill_program$*256
56020   if ~upp_setup then ! r:
56040     upp_setup=1
56060     fn_setup
56080     dim upp_return$*256
56100     dim ub_printbill_client$(1)*18
56120     dim ub_printbill_program$(1)*128
56140     mat ub_printbill_client$(999)
56160     mat ub_printbill_program$(999)
56180     ub_printbill_count=0
56200     fn_upp_add("Ash Grove","ubprtfull_ashgrove")
56220   !     fn_upp_add("Ashland","ubprtbl1_ashland")
56240     fn_upp_add("Bethany","ubprtbl1_Bethany")
56260     fn_upp_add("Campbell","(basic)")  ! derived from printbill_french_settlement_gas which should still work too
56280     fn_upp_add("Chatom","ubprtbl1_chatom")
56300     fn_upp_add("Divernon","ubprtbl1_div")
56320     fn_upp_add("Edinburg","(basic)") ! "ubprtbl1_edi")
56340     fn_upp_add("Edison","(basic)")
56360     fn_upp_add("Exeter","(basic)")
56380     fn_upp_add("Findlay","ubprtbl1_fin")
56400 !   fn_upp_add("Franklinton","ubprtbl1_Franklinton")
56420     fn_upp_add('French Settlement','(basic)') ! 'printbill_french_settlement_gas'
56440     fn_upp_add("Grandview","ubprtbl1_gra")
56460 !   fn_upp_add("GreenCo","ubprtbl1_greenco")
56480     fn_upp_add("Kincaid","ubprtbl1_kin")
56500     ! fn_upp_add("Kimberling","ubprtbl1_Kimberling") ! these are unused but also a nice 4 per page bill that looks pretty comprehensive - move the logic to (basic) if used elsewhere
56520     ! fn_upp_add("Illiopolis","ubprtbl1_Illiopolis")
56540     fn_upp_add("Lovington","ubprtbl1_Lovington")
56560     fn_upp_add("Loma Linda","ubprtbl1_ll")
56580     fn_upp_add("Merriam Woods","(basic)") ! "PrintBill_Merriam_Woods" ! "ubprtbl1_Franklinton")
56600     fn_upp_add("Millry","ubprtbl1_millry")
56620 !   fn_upp_add("Monticello","ubprtbl1_montic")
56640     fn_upp_add("Moweaqua","PrintBill_Moweaqua")
56660 !   fn_upp_add("Oakland","ubprtbl1_Oakland")
56680     fn_upp_add("Raymond",'(basic)') ! "ubprtbl1_Raymond")
56700     fn_upp_add("Thomasboro","ubprtbl1_tho")
56720     fn_upp_add("Thayer","ubprtbl1_thayer")
56740     fn_upp_add("Scottville Rural","ubprtbl1_scottville")
56760 !   fn_upp_add("Gilbertown","ubprtbl1_Gilbertown")
56780 !   fn_upp_add("Waverly","ubprtbl1_Waverly")
56800     fn_upp_add("White Hall","ubprtbl1_wh")
56820     fn_upp_add("Morrisonville","ubprtbl1_morrisonville")
56840     fn_upp_add("Purdy","ubprtbl1_purdy")
56860     fn_upp_add("Galena","ubprtbl1_galena")
56880 ! >>Bills-Laser (3 per page) ^ ubPrtThree
56900     fn_upp_add("Ash Grove","ubprtprace_ash")
56920 !   fn_upp_add("Albany","ubprtthree_Albany")
56940     fn_upp_add("Brier Lake","ubprtthree_Brier")
56960     fn_upp_add("Billings","(basic)") ! ubprtthree_bill
56980     fn_upp_add("Cerro Gordo","ubprtlas_cerro")
57000     fn_upp_add("Choctaw",'(basic)') ! "ubprtlas_choctaw"
57020     ! fn_upp_add("Colyell","ubprtlas_colyell")
57040     !  fn_upp_add("Carrizo","ubprtthree_Carrizo")
57060 !   fn_upp_add("Ed","ubprtthree_barcode")
57080 !   fn_upp_add("Gilbertown","ubprtthree_Gilb")
57100 !   fn_upp_add("Granby","ubprt3prace_Granby")
57120 !   fn_upp_add("Riverside","ubprtthree_River")
57140     fn_upp_add("Omaha",'(basic)') ! "ubprtthree_Omaha")
57160 !   fn_upp_add("Sangamon","ubprtthree_san")
57180 ! >>Bills-Dot Matrix 4x6 ^ S:\acsUB\ubPrtBl14X6
57200 ! >>Bills-Dot Matrix 3.5x6 ^ S:\acsUB\Bill35X6
57220 ! >>Bills-Dot Matrix 3.5x7.5 ^ S:\acsUB\Bill35X75
57240     fn_upp_add("Pennington","(basic)") ! PrintBill_Pennington ! atlantis format - hits preprinted stock
57260 ! >>Bills-Dot Matrix Double Wide ^ S:\acsUB\billDouble
57280 ! >>Bills-Full Page ^ S:\acsUB\Ubprtfull
57300     fn_upp_add("Blucksberg","PrintBill_Blucksberg")
57320 ! >>Bills-Miscellaneous ^ S:\acsUB\Ubprtful
57340 ! 
57360     mat ub_printbill_client$(ub_printbill_count)
57380     mat ub_printbill_program$(ub_printbill_count)
57400 ! 
57420   end if  ! /r
57440   upp_return$='S:\Core\Menu.br'
57840     ua_which=srch(mat ub_printbill_client$,env$('Client'))
57860     if ua_which>0 then 
57880       upp_return$=ub_printbill_program$(ua_which)
57900     else 
57920       msgbox('Your Utility Bill settings could not be determined.  Please contact ACS at 1-800-643-6318.')
57940     end if 
57960   !        (basic) should be fine.   end if 
57970   fnub_printbill_program$=upp_return$
57980 fnend 
58000 def fn_upp_add(ua_client$,ua_program$*128)
58020   ub_printbill_count+=1
58040   ub_printbill_client$(ub_printbill_count)=ua_client$
58060   if ua_program$='(basic)' then
58080     ub_printbill_program$(ub_printbill_count)=ua_program$
58100   else
58120     ub_printbill_program$(ub_printbill_count)='S:\acsUB\'&ua_program$&'.br'
58140   end if
58160 fnend 
62000 def library fnpayroll_client_state$*2
62020   if ~pcs_setup then ! r:
62040     pcs_setup=1
62060     fn_setup
62080     dim pcs_return$*2
62100     dim pr_clientstate_client$(1)*128
62120     dim pr_clientstate_state$(1)*2
62140     mat pr_clientstate_client$(999)
62160     mat pr_clientstate_state$(999)
62180     pr_clientstate_count=0
62200     !   fn_upp_add("Ash Grove","ubprtlas_ashgrove")
62220     fn_pcs_add("ACS","AR")
62240     fn_pcs_add("Lamar",'MS')
62260     fn_pcs_add("Ash Grove",'MO')
62280     !   fn_pcs_add("Battlefield",'MO')
62300     fn_pcs_add("Bethany",'IL')
62320     fn_pcs_add("Billings",'MO')
62340     fn_pcs_add("Campbell",'MO')
62360     fn_pcs_add("Carr Plumbing",'AR')
62380     fn_pcs_add("Cerro Gordo",'IL')
62400     fn_pcs_add("Cerro Gordo T",'IL')
62420     !   fn_pcs_add("Community Dev",'TN')
62440     fn_pcs_add("Divernon",'IL')
62460     fn_pcs_add("Durden",'LA')
62480     fn_pcs_add("Edinburg",'IL')
62500     fn_pcs_add("Edison",'GA')
62520     fn_pcs_add("Ed Horton",'IL')
62540     fn_pcs_add("Energy Exchanger",'OK')
62560 !   fn_pcs_add("Franklin and Son",'AR')
62580 !   fn_pcs_add("Franklinton",'LA')
62600     fn_pcs_add("Galena",'MO')
62620 !   fn_pcs_add("GreenCo",'MO')
62640     fn_pcs_add("Hope Welty",'IL')
62660     fn_pcs_add("Kincaid",'IL')
62680     ! fn_pcs_add("Kimberling",'MO')
62700     fn_pcs_add("Kathys Bookkeeping",'OK')
62720     fn_pcs_add("Lovington",'IL')
62740     fn_pcs_add("Merriam Woods",'MO')
62760 !   fn_pcs_add("Monticello",'IL')
62780 !   fn_pcs_add("Nancy Mouser",'OK')
62800 !   fn_pcs_add("Northwest",'AR')
62820     fn_pcs_add("Oklahoma",'OK')
62822     fn_pcs_add("Payroll Done Right",'OR')
62840     fn_pcs_add("Philo",'IL')
62860 !   fn_pcs_add("PiattCO",'IL')
62880     fn_pcs_add("Raymond",'IL')
62900     fn_pcs_add("R R Crawford",'KY')
62920 !   fn_pcs_add("Riverside",'IN') ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
62940     fn_pcs_add("Thomas Richardson",'LA')
62960     fn_pcs_add("Thomasboro",'IL')
62980     fn_pcs_add("Unity",'IL')
63000 !   fn_pcs_add("Washington Parrish",'LA')
63020 !   fn_pcs_add("West Rest Haven",'')
63040     fn_pcs_add("West Accounting",'OR')
63060     fn_pcs_add("Zaleski",'TX')
63080     mat pr_clientstate_client$(pr_clientstate_count)
63100     mat pr_clientstate_state$(pr_clientstate_count)
63120   !
63140   end if  ! /r
63160   pcs_return$=''
63180   pcs_which=srch(mat pr_clientstate_client$,env$('Client'))
63200   if pcs_which>0 then 
63220     pcs_return$=pr_clientstate_state$(pcs_which)
63240   else 
63260     msgbox('Your Payroll State could not be determined.  Please contact ACS at 1-800-643-6318.')
63280   end if 
63300   !
63320   fnpayroll_client_state$=pcs_return$
63340 fnend 
64000 def fn_pcs_add(pa_client$*128,pa_state$*2)
64020   pr_clientstate_count+=1
64040   pr_clientstate_client$(pr_clientstate_count)=pa_client$
64060   pr_clientstate_state$(pr_clientstate_count)=pa_state$
64080 fnend 
66000 def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
66020   fn_setup
66040   fnclient_has_on_support_list=fn_client_has_on_support_list(mat chosl_list$, chosl_grace_days)
66060 fnend 
68000 def fn_client_has_on_support_list(mat chosl_list$; chosl_grace_days)
68020   dim chosl_owns_list$(0)*2
68040   chosl_count=0
68060   chosl_owns_count=fn_client_has_mat(mat chosl_owns_list$)
68080   for chosl_item=1 to chosl_owns_count
68100     if fn_client_has_on_support_item(chosl_owns_list$(chosl_item), chosl_grace_days) then 
68120       chosl_count+=1
68140       mat chosl_list$(chosl_count)
68160       chosl_list$(chosl_count)=chosl_owns_list$(chosl_item)
68180     end if 
68200   next chosl_item
68220   fn_client_has_on_support_list=chosl_count
68240 fnend 
68260 def library fnclient_has_on_support_item(chosi_item$*2; days_grace)
68280   fn_setup
68300   fnclient_has_on_support_item=fn_client_has_on_support_item(chosi_item$, days_grace)
68320 fnend 
72000 def fn_client_has_on_support_item(chosi_item$*2; days_grace)
72020   client_id=val(env$('Client_ID'))
72040   fn_client_support_setup(client_id,mat chosi_system_id$,mat chosi_system_support_end_date,mat chosi_on_support, days_grace)
72060   chosi_retun=0
72080   chosi_which=srch(mat chosi_system_id$,chosi_item$)
72100   if chosi_which>0 then 
72120     chosi_retun=chosi_on_support(chosi_which)
72140   else 
72160     pr 'system ('&chosi_item$&') is not owned by client number '&env$('Client_ID')
72180     if env$('ACSDeveloper')<>'' then pause 
72200   end if 
72220   fn_client_has_on_support_item=chosi_retun
72240 fnend 
74000 def library fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
74020   fn_setup
74060   fnclient_support=fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
74080 fnend 
75000 def fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
75040   client_id=val(env$('Client_ID'))
75060   fn_client_support=fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
75080 fnend 
76000 def fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_days_grace)
76020   ! css_days_grace=day grace period to allow users to update after support has expired.
76040   if css_setup<>client_id then ! r:
76060     cache_css_client_owns_count=fn_client_has_mat(mat css_client_owns$)
76080         mat css_client_owns$(cache_css_client_owns_count)
76100         mat css_system_id$(cache_css_client_owns_count)
76120         mat css_system_id$=css_client_owns$
76140     !
76160     mat cache_css_system_id$(0)
76180     mat cache_css_system_sup_end_date(0)
76200     cache_css_system_count=0
76220     open #h_support:=fngethandle: "Name=S:\Core\Data\ACS_Sup.dat,Version=2,KFName=S:\Core\Data\ACS_Sup.idx,version=0,Shr",internal,input,keyed 
76240     restore #h_support: ! ,key>==lpad$(trim$(client_id$),kln(h_support)):
76260     do 
76280       read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst eof CSS_SUPPORT_EOF
76300       F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
76320       cln=val(cln$)
76340       if cln=client_id then 
76360         if srch(mat css_client_owns$,fn_system_code_standardize$(scode$))>0 then
76380           cache_css_system_count+=1
76400           mat cache_css_system_id$(cache_css_system_count)
76420           mat cache_css_system_sup_end_date(cache_css_system_count)
76440           cache_css_system_id$(cache_css_system_count)=fn_system_code_standardize$(scode$)
76460           cache_css_system_sup_end_date(cache_css_system_count)=sup_exp_date
76480         end if 
76500       end if 
76520     loop 
76540     CSS_SUPPORT_EOF: ! 
76560     close #h_support: 
76580     css_setup=client_id
76600   end if  ! /r
76620   ! r: move cache_* arrays into passed in and out arrays
76640     mat css_system_id$(cache_css_system_count)
76660     mat css_system_id$=cache_css_system_id$
76680     mat css_system_support_end_date(cache_css_system_count)
76700     mat css_system_support_end_date=cache_css_system_sup_end_date
76720     !  removed - caused problems on 3/3/2017   !!!   ! if cache_css_system_count>0 then
76740     !  removed - caused problems on 3/3/2017   !!!   !   cache_css_system_id$(cache_css_system_count)=fn_system_code_standardize$(scode$)
76760     !  removed - caused problems on 3/3/2017   !!!   !   cache_css_system_sup_end_date(cache_css_system_count)=sup_exp_date
76780     !  removed - caused problems on 3/3/2017   !!!   ! end if
76800   ! /r
78000   ! r: determine if on support
78040     for css_item=1 to cache_css_client_owns_count ! udim(mat css_system_id$)
78082       if css_item>udim(mat css_system_support_end_date) then 
78084         css_on_support(css_item)=0 ! pause 
78100       else if days(date('ccyymmdd'),'ccyymmdd')<=days(css_system_support_end_date(css_item),'ccyymmdd')+css_days_grace then 
78120         css_on_support(css_item)=1
78140       else 
78160         css_on_support(css_item)=0
78180       end if 
78240     next css_item
79000   ! /r
79120 fnend
80000 def library fnsystem_code_standardize$(st_code$*2)
80020   fn_setup
80040   fnsystem_code_standardize$=fn_system_code_standardize$(st_code$)
80060 fnend 
82000 def fn_system_code_standardize$(st_code$*2)
82020   ! this function is to translate from systems.h420 to
82040   ! cursys type codes
82060   ! 
82080   st_code$=uprc$(st_code$)
82100   ! if st_code$='G1' then st_code$='GL'
82120   if st_code$='G1' then st_code$='GL'
82140   if st_code$='G3' then st_code$='G2' ! Accountant's GL Add On
82160   if st_code$='U1' then st_code$='UB' ! UB No Discount
82180   if st_code$='U2' then st_code$='UB' ! UB 500-1000 Customers
82200   if st_code$='U3' then st_code$='UB' ! UB <500 Customers
82220   ! U4 is UB handheld add on in both
82240   if st_code$='P1' then st_code$='PR'
82260   if st_code$='P2' then st_code$='P4' ! Job Cost Payroll Add On
82280   fn_system_code_standardize$=st_code$
82300 fnend 
97000 def library fnclient_is_converting
97020   fn_setup
97060   fnclient_is_converting=fn_client_is_converting
97080 fnend 
97100 def fn_client_is_converting
97120   cic_return=0
97140   if env$('ACSDeveloper')<>'' then
97160     cic_return=1
97220   else if env$('client')='Kathys Bookkeeping' then
97240     cic_return=1
97340   end if 
97360   fn_client_is_converting=cic_return
97380 fnend 
98000 def library fnacs_version$
98020   if ~setup then let fn_setup
98040   open #hBuild:=fngethandle: 'name=S:\Core\Build.txt',d,i
98060   linput #hBuild: build$
98080   close #hBuild:
98100   fnacs_version$='5.'&rtrm$(build$) ! '5.7211'
98120 fnend 

