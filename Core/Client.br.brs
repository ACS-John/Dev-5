02000 ! Replace S:\Core\Client.br
02020 ! we use this library to tell programs which client is using the system
02040 if env$('enableClientSelection')='Yes' then goto ClientSelect
02500 !   ! r: sandbox for testing local functions
03000 !     if env$('ACSDeveloper')<>'' then let setenv('acsclient','BRCorp')
03020 !     let fn_setup
03400 !     print 'fn_client_has_mat returns ';fn_client_has_mat(mat tmp$)
03420 !     print mat tmp$
03500 !     print 'fn_client_support returns ';fn_client_support(mat tmp_system_id$,mat tmp_system_support_end_date,mat tmp_on_support)
03520 !     for x=1 to udim(mat tmp_system_id$)
03540 !       pr tmp_system_id$(x); tmp_system_support_end_date(x); tmp_on_support(x)
03560 !     nex x
04000 !     print 'fn_client_has_on_support_list(mat tmp$,45) returns ';fn_client_has_on_support_list(mat tmp$,45);' with 45 grace days'
04020 !     print mat tmp$
04040 !     print 'fn_client_has_on_support_list(mat tmp$,0) returns ';fn_client_has_on_support_list(mat tmp$,0);' with 0 grace days'
04060 !     print mat tmp$
05000 !   end ! /r
06000 !   !  it is now ...  !!!     print program$&' is not intended to be run directly' : end 
11000 ClientSelect: ! r:
11020   fntop(program$)
11040   fn_setup
11060   fn_clientSelect
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
14480   ! pr mat resp$
15000   if ckey=1 then 
15020     dim clientPrior$*256
15040     dim dataPrior$*256
15060     dim dataNew$*256
15080     clientPrior$=env$('client')
15100     setenv('Client',resp$(1)) ! pr 'env$ client set to '&env$('client') : pause
15102     if env$('enableDataFolderByClient')='Yes' then
15110       dataPrior$=rtrm$(env$('data'),'\')
15120       dataNew$=rtrm$(env$('QBase'),'\')&'\'&env$('client') ! &'\'
15130       ! pr 'clientPrior$: '&clientPrior$
15160       ! pr 'dataPrior$: '&dataPrior$
15170       ! pr '  dataNew$: '&dataNew$
15171       ! pr '  env Q: '&env$('Q') : pause
15172       ! pause
15174       library 's:\Core\Library': fnmakesurepathexists
15176       fnmakesurepathexists(dataNew$)
15180       setenv('data',dataNew$) ! pr 'env$ client set to '&env$('client') : pause
15190       fnreg_close
15200       ! fnMapToVirturalDrive(dataNew$,'Q:') 
15210       setenv('Q',rtrm$(dataNew$,'\'))
15212       execute 'config substitute [Q] "'&env$('Q')&'"'
15220       fnmakesurepathexists(env$('Q')&'\Data\')
15222       fnmakesurepathexists(env$('Q')&'\'&env$('CurSys')&'mstr\')
15280     end if
15300   end if ! /r 
15320 fnend
16000 def fn_setup
16020   if ~setup_library then 
16030     setup_library=1
16040     library 'S:\Core\Library': fnerror,fngethandle,fnreg_read
16060     library 'S:\Core\Library': fntos,fnflexinit1,fnflexadd1,fncmdset,fntop,fnacs
16070     library 'S:\Core\Library': fnXit
16072     library 'S:\Core\Library': fnreg_close
16074     ! library 'S:\Core\Library': fnMapToVirturalDrive
16080   end if 
16090   fn_setup_client
16100 fnend 
18000 def library fnclient$*18
18020   let fn_setup
18040   let fnclient$=fn_client$
18060 fnend 
20000 def fn_client$*18
20020   ! if env$('ACSDeveloper')<>'' then pr 'on the way in env client$ is '&env$('client')
20040   on error goto ERTN
20060   dim clientReturn$*18
20080   ! r: derive client
20100   loginNameWhich=srch(mat client_name$,login_name$)
20120   serialWhich=srch(mat client_brserial,serial)
20140   clientWhich=0
20160   if env$('acsClient')<>'' then ! it is specified in drive.sys - default to it
20180     clientWhich=srch(mat client_name$,env$('acsClient'))
20200     if clientWhich<=0 then pr 'invalid env acsClient.' : pause
20220   else if loginNameWhich>0 then
20240     clientWhich=loginNameWhich
20260   else if serialWhich>0 then
20280     clientWhich=serialWhich
20300   end if
20320   let setenv('Client',client_name$(clientWhich))
20340   let setenv('Client_ID',str$(client_cno(clientWhich)))
20360   if env$('client')='' then
20380     pr "env$('client') is blank." : pause
20400   end if
20420   ! /r 
20440   fn_getClientLicense(mat client_has$)
20460   if srch(mat client_name$,login_name$)>0 then
20480     clientReturn$=login_name$
20500   else
20520     clientReturn$=env$('client')
20540     let client_which=srch(mat client_name$,env$('client'))
20560     if client_which>0 then 
20580       setenv('Client_ID',str$(client_cno(client_which)))
20600     else
20620       pr 'env: Client: "'&env$('client')&'" did not match any entries Mat client_name$.  env: Client_ID could not be set'
20640       pause
20660       setenv('Client_ID','')
20680     end if
20700   end if
20720   ! if env$('ACSDeveloper')<>'' then pr 'clientReturn$='&clientReturn$ : pause
20740   let fn_client$=clientReturn$
20760 fnend 
24000 ! <Updateable Region: ERTN>
24020 ERTN: let fnerror(program$,err,line,act$,"NO")
24040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
24060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
24080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
24100 ERTN_EXEC_ACT: execute act$ : goto ERTN
24120 ! /region
26000 def fn_setup_client ! ** set up for new clients
26010   if ~setup_client then 
26020     setup_client=1
26030     dim client_name$(1)*18
26040     let client_count=0
26050     mat client_name$(client_count)
26060     mat client_cno(client_count)
26070     mat client_brserial(client_count)
26090     !  fn_setup_client_add("ACS",1,0) ! TEMP
26100     ! 
26110     let fn_setup_client_add("ACS" ,420,58650)
26120     let fn_setup_client_add("Ed Horton" ,5535,0)! Ed processes like ACS
26130     ! 
26140     let fn_setup_client_add("Lamar" ,1,33854)
26150     !   fn_setup_client_add("Albany" ,190,15376) ! notifed me 9/22/15 that they were switching UB providers
26160     let fn_setup_client_add("Ash Grove" ,286,19016)
26170     !   fn_setup_client_add("Ashland" ,300,33584)
26180     !   fn_setup_client_add("Battlefield" ,369,33306)
26190     let fn_setup_client_add("Bethany" ,380,34326)
26200     let fn_setup_client_add("Billings" ,440,33534)
26210     let fn_setup_client_add("Blucksberg" ,465,34564)
26220     !   fn_setup_client_add("Brazeal" ,570,34418)
26230     let fn_setup_client_add("Brier Lake" ,578,20306)
26240     let fn_setup_client_add("BRCorp" ,7000,50775) ! 50775 is actually DAVID KALINSKI PERSONAL COPY, but it is what Gordon is using.
26250     let fn_setup_client_add("Campbell" ,700,33942)
26260     let fn_setup_client_add("Carr Plumbing" ,780,34610)
26270     ! let fn_setup_client_add("Carrizo" ,800,34416)
26280     let fn_setup_client_add("Cerro Gordo" ,850,34508) ! 33994)
26290     let fn_setup_client_add("Cerro Gordo T" ,970,34508)
26300     let fn_setup_client_add("Chatom" ,911,15678)
26310     let fn_setup_client_add("Choctaw" ,918,34214)
26320     ! fn_setup_client_add("Colyell" ,980,33948)
26330     ! fn_setup_client_add("Community Dev" ,982,34156)
26340     let fn_setup_client_add("Divernon" ,1350,33698)
26350     let fn_setup_client_add("Dorothy Salch" ,3812,34494)
26360     let fn_setup_client_add("Durden" ,1406,16410)
26370     let fn_setup_client_add("Edinburg" ,1478,34022)
26380     let fn_setup_client_add("Edison" ,1480,34022)
26390     ! let fn_setup_client_add("Eldorado" ,1500,33352)
26400     let fn_setup_client_add("Evelyn Pareya" ,3385,34366)
26410     let fn_setup_client_add("Exeter" ,1615,31210)
26420     let fn_setup_client_add("Energy Exchanger" ,1550,10172)
26430     !   fn_setup_client_add("FirstBaptist" ,1695,33380) ! <-- note it's the same as French Settlement - one of them is wrong, but First Baptist of Frnaklinton's license is 4.1 and not currently necessary, so just commenting them out for now.
26440     let fn_setup_client_add("Findlay" ,1700,34132)
26450     !   fn_setup_client_add("Franklin and Son" ,1870,32454)
26460     let fn_setup_client_add("Franklin Co Hosp" ,1876,33668)
26470     !   fn_setup_client_add("Franklinton" ,1876,0) ! Town of
26480     let fn_setup_client_add("French Settlement" ,1880,33380)
26490     let fn_setup_client_add("Fulton" ,1890,33720) ! Utilities Board
26500     let fn_setup_client_add("Galena" ,1945,34566)
26510     let fn_setup_client_add("Garrity",1950,0)
26520 !   let fn_setup_client_add("Gilbertown",1985,0)
26530 !   let fn_setup_client_add("Granby",2040,34098) ! no longer using as of 6/13/2016
26540     let fn_setup_client_add("Grandview",2050,34040)
26550 !   let fn_setup_client_add("GreenCo",2070,33910)
26560     let fn_setup_client_add("Hope Welty" ,851,34152)
26570     ! fn_setup_client_add("Illiopolis",2340,0)
26580     let fn_setup_client_add("Kathys Bookkeeping",3979,33672)
26590     ! fn_setup_client_add("Kimberling",2530,19212)
26600     let fn_setup_client_add("Kincaid",2532,33652)
26610     let fn_setup_client_add("Lovington",2689,32720)
26620     let fn_setup_client_add("Loma Linda",2690,33244)
26630 !   let fn_setup_client_add("Nancy Mouser",2795,34318)
26640     let fn_setup_client_add("Merriam Woods",2900,31702)
26650 !   let fn_setup_client_add("Miller Hardware",3005,14668)
26660     let fn_setup_client_add("Millry",3025,33968)
26670 !   let fn_setup_client_add("Monticello",3040,12196)
26680     let fn_setup_client_add("Moweaqua",3045,34594) ! 200032790) ! 33986 <--??  I don't know where that came from - 200032790 was their 4.13 version
26690     let fn_setup_client_add("Morrisonville",3050,34408) ! 32242  <-- that's white hall's but there was a mistake in license file for a while
26700 !   let fn_setup_client_add("Northwest",3241,11176 )
26710 !   let fn_setup_client_add("Oakland",3250,34260)
26720     let fn_setup_client_add("Omaha",3320,33346)
26730     let fn_setup_client_add("Pennington",3431,33332)
26740 !   let fn_setup_client_add("Petromark",3535,33620)
26750     let fn_setup_client_add("Philo",3534,34150)
26760 !   let fn_setup_client_add("PiattCO",3536,20832)
26770     let fn_setup_client_add("Purdy",3610,34570)
26780     let fn_setup_client_add("Raymond",3660,32798)
26790     let fn_setup_client_add('R R Crawford',760,12466)
26800     let fn_setup_client_add("Thomas Richardson",3720,7718)
26810 !   let fn_setup_client_add("Riverside",3725,18332)
26820 !   let fn_setup_client_add("Sangamon",3815,34066)
26830     let fn_setup_client_add("Scottville Rural",3840,33390)
26840     let fn_setup_client_add("Thayer",4245,32800)
26850     let fn_setup_client_add("Thomasboro",4260,34068)
26860     let fn_setup_client_add("Unity",4380,34478)
26870 !   let fn_setup_client_add("Washington Parrish",4510,34116)
26880 !   let fn_setup_client_add("Waverly",4515,34430)
26890     let fn_setup_client_add("West Accounting",4560,30176)
26900 !   let fn_setup_client_add("West Rest Haven",4567,34032)
26910     let fn_setup_client_add("White Hall",4625,32242)
26920 !   let fn_setup_client_add("Willard",4650,33514)
26930     let fn_setup_client_add("World Wide",4650,33604)
26940     let fn_setup_client_add("Zaleski",4710,34164)
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
28051       let fn_add_ch_sys('UB')
28052       let fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
28053       let fn_add_ch_sys('CL')
28054       let fn_add_ch_sys('PR')
28055       let fn_add_ch_sys('GL')
28056       let fn_add_ch_sys('OE')
28058       let fn_add_ch_sys('TM')
28090     ! else if env$('client')='Albany' then ! demo undelivered - not on support but needed to debug Past Due Trun Off List from ACS 4 - test in ACS 5 locally
28100     !   let fn_user_limit(1)
28110     !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('UB') : let fn_set_ub_limit(500)
28120     !   if days(date)<=days(20151231,'ccyymmdd') then let fn_add_ch_sys('U4')
28130     else if env$('client')='BRCorp' then 
28140       let fn_user_limit(99)
28150       let fn_add_ch_sys('OE') 
28160       ! else if env$('client')='Ed Horton' then 
28170       !   let fn_user_limit(1)
28180       !   let fn_add_ch_sys('CL')
28190       !   let fn_add_ch_sys('GL')
28200       !   let fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
28210       !   let fn_add_ch_sys('PR')
28220       !   let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28230       !   let fn_add_ch_sys('U4') ! U4 Utility Billing Hand Held Add-On
28240     else if env$('client')='Ash Grove' and (env$('Unique_Computer_Id')="27932D73-8B8B-4D40-943D-08EFB9E5CFE9" or env$('Unique_Computer_Id')="4C4C4544-0037-3610-8058-C8C04F4E5731") then ! Melissa on Windows XP mode and Local Server Machine only uses UB
28250       let fn_user_limit(1)
28260       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28270       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28280     else if env$('client')='Ash Grove' and env$('Unique_Computer_Id')="4C4C4544-0054-3710-8058-C7C04F4E5731" then ! Debbie
28290       let fn_user_limit(1)
28300       let fn_add_ch_sys('PR')
28310       let fn_add_ch_sys('GL')
28320       let fn_add_ch_sys('CL')
28330     else if env$('client')='Ash Grove' then 
28340       let fn_user_limit(1)
28350       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28360       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28370       let fn_add_ch_sys('PR')
28380       let fn_add_ch_sys('GL')
28390       let fn_add_ch_sys('CL')
28400     else if env$('client')='Bethany' then 
28410       let fn_user_limit(1)
28420       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28430       let fn_add_ch_sys('U4') : let u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
28440       let fn_add_ch_sys('PR')
28450       let fn_add_ch_sys('GL')
28460       let fn_add_ch_sys('CL')
28470     else if env$('client')='Brier Lake' then 
28480       let fn_user_limit(1)
28490       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28500     else if env$('client')='Billings' and (env$('Unique_Computer_Id')="58973139-FC9B-1A95-F234-C145E2B22211" or env$('Unique_Computer_Id')="50A59A38-38BF-A82F-9868-04C4E5DD281A") then ! Limit to only UB stuff for (Katrina or Gale)
28510       let fn_user_limit(2) ! actually licensed for 3 users, but has two separate installations
28520       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28530       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28540       !     let fn_add_ch_sys('CR')
28550     else if env$('client')='Billings' and env$('Unique_Computer_Id')="BD04113D-C102-BA29-78AC-D23201FDC70C" then ! Limit to NOT UB stuff for Chris Hopkins
28560       let fn_user_limit(1) ! actually licensed for 3 users, but has two separate installations
28570       let fn_add_ch_sys('GL')
28580       let fn_add_ch_sys('PR')
28590       let fn_add_ch_sys('CL')
28600     else if env$('client')='Billings' then 
28610       let fn_user_limit(3)
28620       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28630       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28640       let fn_add_ch_sys('GL')
28650       let fn_add_ch_sys('PR')
28660       let fn_add_ch_sys('CL')
28670       !     let fn_add_ch_sys('CR')
28680     else if env$('client')='Blucksberg' then 
28690       let fn_user_limit(1)
28700       let fn_add_ch_sys('UB') : let fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
28710       let fn_add_ch_sys('U4') : let u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
28720     else if env$('client')='Campbell' then 
28730       let fn_user_limit(4)
28740       let fn_add_ch_sys('CL')
28750       let fn_add_ch_sys('PR')
28760       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28770     else if env$('client')='Carr Plumbing' then 
28780       let fn_user_limit(1)
28790       let fn_add_ch_sys('PR')
28800     else if env$('client')='Chatom' then 
28810       let fn_user_limit(1)
28820       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28830       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28840     else if env$('client')='Cerro Gordo' then 
28850       let fn_user_limit(1)
28860       let fn_add_ch_sys('GL')
28870       let fn_add_ch_sys('PR')
28880       let fn_add_ch_sys('CL')
28890       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
28900       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
28910     else if env$('client')='Cerro Gordo T' then 
28920       let fn_add_ch_sys('GL')
28930       let fn_add_ch_sys('PR')
28940       let fn_add_ch_sys('CL')
28950     else if env$('client')='Choctaw' then 
28960       let fn_user_limit(1)
28970       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
28980       let fn_add_ch_sys('GL')
28990     else if env$('client')='Edinburg' then 
29000       let fn_user_limit(2)
29010       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29020       let fn_add_ch_sys('GL')
29030       let fn_add_ch_sys('PR')
29040       let fn_add_ch_sys('CL')
29050       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! ACEECA MEZ 1500 ! U4 Utility Billing Hand Held Add-On
29060     else if env$('client')='Edison' then 
29070       let fn_user_limit(2)
29080       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U3 Utility Billing (<500 Customers)
29090       let fn_add_ch_sys('GL')
29100       let fn_add_ch_sys('PR')
29110       let fn_add_ch_sys('CL')
29120     else if env$('client')='Exeter' then 
29130       let fn_user_limit(2)
29140       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29150     else if env$('client')='Energy Exchanger' then 
29160       let fn_user_limit(1)
29170       let fn_add_ch_sys('PR')
29180       ! if days(date$)<=days('05/18/2017','mm/dd/ccyy') then let fn_add_ch_sys('P4')
29190       if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('CL')
29200       if days(date$)<=days('05/31/2017','mm/dd/ccyy') then let fn_add_ch_sys('GL')
29210     else if env$('client')='Dorothy Salch' then 
29220       let fn_user_limit(1)
29230       let fn_add_ch_sys('GL')
29240       let fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29250     else if env$('client')='Evelyn Pareya' then 
29260       let fn_user_limit(1)
29270       let fn_add_ch_sys('GL')
29280       let fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29290     else if env$('client')='Findlay' then 
29300       let fn_user_limit(2)
29310       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29320       let fn_add_ch_sys('U4') : let u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
29330       !   else if env$('client')='Franklin and Son' then 
29340       !     let fn_user_limit(1)
29350       !     let fn_add_ch_sys('PR')
29360     else if env$('client')='Franklin Co Hosp' then 
29370       let fn_user_limit(1)
29380       let fn_add_ch_sys('GL')
29390       let fn_add_ch_sys('CL')
29400     else if env$('client')='French Settlement' then 
29410       let fn_user_limit(1)
29420       let fn_add_ch_sys('UB') : let fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29430     else if env$('client')='Galena' then 
29440       let fn_user_limit(1)
29450       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
29460       !   else if env$('client')='Granby' then 
29470       !     let fn_user_limit(2)
29480       !     let fn_add_ch_sys('UB') : let fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29490     else if env$('client')='Grandview' then 
29500       let fn_user_limit(1)
29510       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29520     else if env$('client')='Hope Welty' then 
29530       let fn_user_limit(1)
29540       let fn_add_ch_sys('GL')
29550       let fn_add_ch_sys('PR')
29560       let fn_add_ch_sys('CL')
29570     else if env$('client')='Kathys Bookkeeping' then 
29580       let fn_user_limit(2)
29590       let fn_add_ch_sys('GL')
29600       !     let fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
29610       let fn_add_ch_sys('PR')
29620       !     let fn_add_ch_sys('P4')
29630     else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='1478AEE0-5BCB-11D9-B0AC-BCAEC5EA1947' then 
29640       let fn_user_limit(1)
29650       let fn_add_ch_sys('PR')
29660       !   else if env$('client')='Kincaid' and and env$('Unique_Computer_Id')='XXX need to do XXX' then
29670       !     let fn_user_limit(1)
29680       !     let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29690     else if env$('client')='Kincaid' and env$('Unique_Computer_Id')='03000200-0400-0500-0006-000700080009' then 
29700       let fn_user_limit(1)
29710       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29720       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
29730     else if env$('client')='Kincaid' then 
29740       let fn_user_limit(2)
29750       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29760       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
29770       !     let fn_add_ch_sys('GL')
29780       let fn_add_ch_sys('PR')
29790     else if env$('client')='Lovington' then 
29800       let fn_user_limit(1)
29810       let fn_add_ch_sys('UB') : let fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
29820       let fn_add_ch_sys('U4') : u4_device$="Sensus" ! U4 Utility Billing Hand Held Add-On
29830       let fn_add_ch_sys('GL')
29840       let fn_add_ch_sys('PR')
29850       let fn_add_ch_sys('CL')
29860     else if env$('client')='Merriam Woods' then 
29870       let fn_user_limit(1)
29880       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29890       let fn_add_ch_sys('U5') ! UB External Collections Processing
29900       !     let fn_add_ch_sys('CR')
29910       !     let fn_add_ch_sys('GL')
29920       !     let fn_add_ch_sys('PR')
29930       !     let fn_add_ch_sys('CL')
29940     else if env$('client')='Millry' then 
29950       let fn_user_limit(4)
29960       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
29970       let fn_add_ch_sys('U4') : let u4_device$="Itron FC300" ! U4 Utility Billing Hand Held Add-On
29980     else if env$('client')='Morrisonville' then 
29990       let fn_user_limit(1)
30000       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30010     else if env$('client')='Moweaqua' then 
30020       let fn_user_limit(1)
30030       let fn_add_ch_sys('UB') : let fn_set_ub_limit(1000) ! U2 Utility Billing (500-1000 customers)
30040       let fn_add_ch_sys('U4') : let u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30050     else if env$('client')='Pennington' then 
30060       let fn_user_limit(1)
30070       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30080     else if env$('client')='Purdy' then 
30090       let fn_user_limit(1)
30100       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30110       let fn_add_ch_sys('U4') : let u4_device$="Boson" ! U4 Utility Billing Hand Held Add-On
30120     else if env$('client')='Omaha' then 
30130       let fn_user_limit(1)
30140       let fn_add_ch_sys('UB') : let fn_set_ub_limit(9999) ! U1 Utility Billing (no discount)
30150     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0043-4210-8058-C8C04F423432' then 
30160       let fn_user_limit(1)
30170       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30180       let fn_add_ch_sys('U4') : let u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On          FREE TRIAL PERIOD
30190     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='4C4C4544-0032-5910-804C-B3C04F585131' then 
30200       let fn_user_limit(1)
30210       let fn_add_ch_sys('PR')
30220     else if env$('client')='Raymond' and env$('Unique_Computer_Id')='C55D3F13-A162-E111-8430-DC0EA14AC3F6' then ! ACS Test Laptop QOSMIO X775
30230       let fn_user_limit(1)
30240       let fn_add_ch_sys('PR')
30250     else if env$('client')='Raymond' then 
30260       let fn_user_limit(2)
30270       let fn_add_ch_sys('PR')
30280       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30290       let fn_add_ch_sys('U4') : let u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30300     else if env$('client')='R R Crawford' then 
30310       let fn_user_limit(1)
30320       let fn_add_ch_sys('PR')
30330       let fn_add_ch_sys('GL')
30340     else if env$('client')='Scottville Rural' then 
30350       let fn_user_limit(1)
30360       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30370     else if env$('client')='Thayer' then 
30380       let fn_user_limit(1)
30390       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30400       let fn_add_ch_sys('U4') : let u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30410     else if env$('client')='Thomasboro' then 
30420       let fn_user_limit(1)
30430       let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30440       let fn_add_ch_sys('U4') : let u4_device$="Badger" ! U4 Utility Billing Hand Held Add-On
30450       let fn_add_ch_sys('GL')
30460       let fn_add_ch_sys('PR')
30470       let fn_add_ch_sys('CL')
30480     else if env$('client')='Thomas Richardson' then 
30490       let fn_user_limit(1)
30500       let fn_add_ch_sys('GL')
30510       let fn_add_ch_sys('PR')
30520       !   else if env$('client')='Waverly' then 
30530       !     let fn_user_limit(1)
30540       !     let fn_add_ch_sys('UB') : let fn_set_ub_limit(500) ! U3 Utility Billing (<500 Customers)
30550     else if env$('client')='West Accounting' then 
30560       let fn_user_limit(1)
30570       let fn_add_ch_sys('PR')
30580     else if env$('client')='White Hall' then 
30590       let fn_user_limit(2)
30600       let fn_add_ch_sys('UB')
30610       !   else if env$('client')='Willard' then 
30620       !     let fn_user_limit(1)
30630       !     let fn_add_ch_sys('GL')
30640       !     let fn_add_ch_sys('PR')
30650       !     let fn_add_ch_sys('CL')
30660     else if env$('client')='World Wide' then 
30670       let fn_user_limit(1)
30680       let fn_add_ch_sys('GL')
30690     else if env$('client')='Zaleski' then 
30700       let fn_user_limit(1)
30710       let fn_add_ch_sys('PR') 
30720       let fn_add_ch_sys('GL') 
30730       let fn_add_ch_sys('G2') ! G3 Accountant's General Ledger
30740     end if 
30750     ! /r
30760   end if
30770 fnend
31000 def fn_setup_client_add(sca_name$*18,sca_customer_number,sca_brserial_number)
31020   let client_count+=1
31040   mat client_name$(client_count)
31060   mat client_cno(client_count)
31080   mat client_brserial(client_count)
31100   let client_name$(client_count)=sca_name$
31120   let client_cno(client_count)=sca_customer_number
31140   let client_brserial(client_count)=sca_brserial_number
31160 fnend 
32000 def library fnclient_has_mat(mat c_has$) ! returns a list of system each client owns
32020   let fn_setup
32040   let fnclient_has_mat=fn_client_has_mat(mat c_has$)
32060 fnend 
34000 def fn_client_has_mat(mat c_has$)
34022   if env$('client')='' then pr 'fn_client_has_mat called but env client not set.' : pause 
34023   fn_getClientLicense(mat client_has$)
34040   mat c_has$(udim(mat client_has$))
34060   mat c_has$=client_has$
34100   let fn_client_has_mat=client_has_system_count
34120 fnend 
36000 def library fnclient_has(ch_sys$*2)
36020   let fn_setup
36040   let fnclient_has=fn_client_has(ch_sys$)
36060 fnend 
36080 def fn_client_has(ch_sys$*2)
36093   fn_getClientLicense(mat client_has$)
36120   let ch_return=0
36140   if srch(mat client_has$,uprc$(ch_sys$))>0 then let ch_return=1
36160   let fn_client_has=ch_return
36180 fnend 
38000 def fn_user_limit(user_count)
38020   !   if user_count>val(wsid$) then
38040   !     user_limit_exceeded=1
38060   !     msgbox('Workstation ID ('&wsid$&') is higher than the licensed number of users ('&str$(user_count)&').')
38080   !     execute 'system'
38100   !   end if
38120   if val(env$('user_limit'))<>user_count then
38140     execute 'config option 9 '&str$(user_count)
38160     let setenv('user_limit',str$(user_count))
38180   end if 
38200   ! 
38220 fnend 
42000 ! r: def library fnuser_limit_exceeded
42020 ! fn_setup
42023 ! fn_getClientLicense(mat client_has$)
42040 !   fnuser_limit_exceeded=user_limit_exceeded
42060 ! /r fnend
44000 def fn_add_ch_sys(ch_item$)
44020   let client_has_system_count=udim(mat client_has$)
44040   if client_has_system_count=0 then 
44060     let client_has_system_count=2
44080     mat client_has$(1)
44100     let client_has$(1)='CO'
44120   else 
44140     let client_has_system_count+=1
44160   end if 
44180   mat client_has$(client_has_system_count)
44200   let client_has$(client_has_system_count)=ch_item$
44220 fnend 
46000 def library fnregistered_for_hh
46002   fn_setup
46013   fn_getClientLicense(mat client_has$)
46020   let fnregistered_for_hh=fn_client_has('U4') ! fn_registered_for_hh
46040 fnend 
48000 def library fnregistered_for_job_cost_pr
48002   fn_setup
48013   fn_getClientLicense(mat client_has$)
48020   let fnregistered_for_job_cost_pr=fn_client_has('P4') ! fn_registered_for_job_cost_pr
48040 fnend
52000 def library fnhand_held_device$*20
52010   fn_setup
52023   fn_getClientLicense(mat client_has$)
52040   let fnhand_held_device$=fn_hand_held_device$
52060 fnend 
52080 def fn_hand_held_device$*20
52082   dim u4_device$*20,u4_deviceDefault$*20
52084   u4_deviceDefault$=u4_device$
52090   fnreg_read('Hand Held Device',u4_device$,u4_deviceDefault$)
52092   if u4_device$='' then let u4_device$=u4_deviceDefault$ ! in case it's been set and then blanked out to force default for client
52560   let fn_hand_held_device$=u4_device$
52580 fnend 
54000 def fn_set_ub_limit(x)
54020   let gUbLimit=x
54030   setenv('UB_Limit',str$(gUbLimit))
54040 fnend 
56000 def library fnub_printbill_program$*256
56020   if ~upp_setup then ! r:
56040     let upp_setup=1
56060     let fn_setup
56080     dim upp_return$*256
56100     dim ub_printbill_client$(1)*18
56120     dim ub_printbill_program$(1)*128
56140     mat ub_printbill_client$(999)
56160     mat ub_printbill_program$(999)
56180     let ub_printbill_count=0
56200     let fn_upp_add("Ash Grove","ubprtfull_ashgrove")
56220   !     let fn_upp_add("Ashland","ubprtbl1_ashland")
56240     let fn_upp_add("Bethany","ubprtbl1_Bethany")
56260     let fn_upp_add("Campbell","(basic)")  ! derived from printbill_french_settlement_gas which should still work too
56280     let fn_upp_add("Chatom","ubprtbl1_chatom")
56300     let fn_upp_add("Divernon","ubprtbl1_div")
56320     let fn_upp_add("Edinburg","(basic)") ! "ubprtbl1_edi")
56340     let fn_upp_add("Edison","(basic)")
56360     let fn_upp_add("Exeter","(basic)")
56380     let fn_upp_add("Findlay","ubprtbl1_fin")
56400 !   let fn_upp_add("Franklinton","ubprtbl1_Franklinton")
56420     let fn_upp_add('French Settlement','(basic)') ! 'printbill_french_settlement_gas'
56440     let fn_upp_add("Grandview","ubprtbl1_gra")
56460 !   let fn_upp_add("GreenCo","ubprtbl1_greenco")
56480     let fn_upp_add("Kincaid","ubprtbl1_kin")
56500     ! fn_upp_add("Kimberling","ubprtbl1_Kimberling") ! these are unused but also a nice 4 per page bill that looks pretty comprehensive - move the logic to (basic) if used elsewhere
56520     ! fn_upp_add("Illiopolis","ubprtbl1_Illiopolis")
56540     let fn_upp_add("Lovington","ubprtbl1_Lovington")
56560     let fn_upp_add("Loma Linda","ubprtbl1_ll")
56580     let fn_upp_add("Merriam Woods","(basic)") ! "PrintBill_Merriam_Woods" ! "ubprtbl1_Franklinton")
56600     let fn_upp_add("Millry","ubprtbl1_millry")
56620 !   let fn_upp_add("Monticello","ubprtbl1_montic")
56640     let fn_upp_add("Moweaqua","PrintBill_Moweaqua")
56660 !   let fn_upp_add("Oakland","ubprtbl1_Oakland")
56680     let fn_upp_add("Raymond",'(basic)') ! "ubprtbl1_Raymond")
56700     let fn_upp_add("Thomasboro","ubprtbl1_tho")
56720     let fn_upp_add("Thayer","ubprtbl1_thayer")
56740     let fn_upp_add("Scottville Rural","ubprtbl1_scottville")
56760 !   let fn_upp_add("Gilbertown","ubprtbl1_Gilbertown")
56780 !   let fn_upp_add("Waverly","ubprtbl1_Waverly")
56800     let fn_upp_add("White Hall","ubprtbl1_wh")
56820     let fn_upp_add("Morrisonville","ubprtbl1_morrisonville")
56840     let fn_upp_add("Purdy","ubprtbl1_purdy")
56860     let fn_upp_add("Galena","ubprtbl1_galena")
56880 ! >>Bills-Laser (3 per page) ^ ubPrtThree
56900     let fn_upp_add("Ash Grove","ubprtprace_ash")
56920 !   let fn_upp_add("Albany","ubprtthree_Albany")
56940     let fn_upp_add("Brier Lake","ubprtthree_Brier")
56960     let fn_upp_add("Billings","(basic)") ! ubprtthree_bill
56980     let fn_upp_add("Cerro Gordo","ubprtlas_cerro")
57000     let fn_upp_add("Choctaw",'(basic)') ! "ubprtlas_choctaw"
57020     ! fn_upp_add("Colyell","ubprtlas_colyell")
57040     !  fn_upp_add("Carrizo","ubprtthree_Carrizo")
57060 !   let fn_upp_add("Ed","ubprtthree_barcode")
57080 !   let fn_upp_add("Gilbertown","ubprtthree_Gilb")
57100 !   let fn_upp_add("Granby","ubprt3prace_Granby")
57120 !   let fn_upp_add("Riverside","ubprtthree_River")
57140     let fn_upp_add("Omaha",'(basic)') ! "ubprtthree_Omaha")
57160 !   let fn_upp_add("Sangamon","ubprtthree_san")
57180 ! >>Bills-Dot Matrix 4x6 ^ S:\acsUB\ubPrtBl14X6
57200 ! >>Bills-Dot Matrix 3.5x6 ^ S:\acsUB\Bill35X6
57220 ! >>Bills-Dot Matrix 3.5x7.5 ^ S:\acsUB\Bill35X75
57240     let fn_upp_add("Pennington","(basic)") ! PrintBill_Pennington ! atlantis format - hits preprinted stock
57260 ! >>Bills-Dot Matrix Double Wide ^ S:\acsUB\billDouble
57280 ! >>Bills-Full Page ^ S:\acsUB\Ubprtfull
57300     let fn_upp_add("Blucksberg","PrintBill_Blucksberg")
57320 ! >>Bills-Miscellaneous ^ S:\acsUB\Ubprtful
57340 ! 
57360     mat ub_printbill_client$(ub_printbill_count)
57380     mat ub_printbill_program$(ub_printbill_count)
57400 ! 
57420   end if  ! /r
57440   let upp_return$='S:\Core\Menu.br'
57840     let ua_which=srch(mat ub_printbill_client$,env$('Client'))
57860     if ua_which>0 then 
57880       let upp_return$=ub_printbill_program$(ua_which)
57900     else 
57920       let msgbox('Your Utility Bill settings could not be determined.  Please contact ACS at 1-800-643-6318.')
57940     end if 
57960   !        (basic) should be fine.   end if 
57970   let fnub_printbill_program$=upp_return$
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
62040     let pcs_setup=1
62060     let fn_setup
62080     dim pcs_return$*2
62100     dim pr_clientstate_client$(1)*128
62120     dim pr_clientstate_state$(1)*2
62140     mat pr_clientstate_client$(999)
62160     mat pr_clientstate_state$(999)
62180     let pr_clientstate_count=0
62200     !   fn_upp_add("Ash Grove","ubprtlas_ashgrove")
62220     let fn_pcs_add("ACS","AR")
62240     let fn_pcs_add("Lamar",'MS')
62260     let fn_pcs_add("Ash Grove",'MO')
62280     !   fn_pcs_add("Battlefield",'MO')
62300     let fn_pcs_add("Bethany",'IL')
62320     let fn_pcs_add("Billings",'MO')
62340     let fn_pcs_add("Campbell",'MO')
62360     let fn_pcs_add("Carr Plumbing",'AR')
62380     let fn_pcs_add("Cerro Gordo",'IL')
62400     let fn_pcs_add("Cerro Gordo T",'IL')
62420     !   fn_pcs_add("Community Dev",'TN')
62440     let fn_pcs_add("Divernon",'IL')
62460     let fn_pcs_add("Durden",'LA')
62480     let fn_pcs_add("Edinburg",'IL')
62500     let fn_pcs_add("Edison",'GA')
62520     let fn_pcs_add("Ed Horton",'IL')
62540     let fn_pcs_add("Energy Exchanger",'OK')
62560 !   let fn_pcs_add("Franklin and Son",'AR')
62580 !   let fn_pcs_add("Franklinton",'LA')
62600     let fn_pcs_add("Galena",'MO')
62620 !   let fn_pcs_add("GreenCo",'MO')
62640     let fn_pcs_add("Hope Welty",'IL')
62660     let fn_pcs_add("Kincaid",'IL')
62680     ! fn_pcs_add("Kimberling",'MO')
62700     let fn_pcs_add("Kathys Bookkeeping",'OK')
62720     let fn_pcs_add("Lovington",'IL')
62740     let fn_pcs_add("Merriam Woods",'MO')
62760 !   let fn_pcs_add("Monticello",'IL')
62780 !   let fn_pcs_add("Nancy Mouser",'OK')
62800 !   let fn_pcs_add("Northwest",'AR')
62820     let fn_pcs_add("Oklahoma",'OK')
62840     let fn_pcs_add("Philo",'IL')
62860 !   let fn_pcs_add("PiattCO",'IL')
62880     let fn_pcs_add("Raymond",'IL')
62900     let fn_pcs_add("R R Crawford",'KY')
62920 !   let fn_pcs_add("Riverside",'IN') ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
62940     let fn_pcs_add("Thomas Richardson",'LA')
62960     let fn_pcs_add("Thomasboro",'IL')
62980     let fn_pcs_add("Unity",'IL')
63000 !   let fn_pcs_add("Washington Parrish",'LA')
63020 !   let fn_pcs_add("West Rest Haven",'')
63040     let fn_pcs_add("West Accounting",'OR')
63060     let fn_pcs_add("Zaleski",'TX')
63080     mat pr_clientstate_client$(pr_clientstate_count)
63100     mat pr_clientstate_state$(pr_clientstate_count)
63120   !
63140   end if  ! /r
63160   let pcs_return$=''
63180   let pcs_which=srch(mat pr_clientstate_client$,env$('Client'))
63200   if pcs_which>0 then 
63220     let pcs_return$=pr_clientstate_state$(pcs_which)
63240   else 
63260     let msgbox('Your Payroll State could not be determined.  Please contact ACS at 1-800-643-6318.')
63280   end if 
63300   !
63320   let fnpayroll_client_state$=pcs_return$
63340 fnend 
64000 def fn_pcs_add(pa_client$*128,pa_state$*2)
64020   let pr_clientstate_count+=1
64040   let pr_clientstate_client$(pr_clientstate_count)=pa_client$
64060   let pr_clientstate_state$(pr_clientstate_count)=pa_state$
64080 fnend 
66000 def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
66020   let fn_setup
66040   let fnclient_has_on_support_list=fn_client_has_on_support_list(mat chosl_list$, chosl_grace_days)
66060 fnend 
68000 def fn_client_has_on_support_list(mat chosl_list$; chosl_grace_days)
68020   dim chosl_owns_list$(0)*2
68040   let chosl_count=0
68060   let chosl_owns_count=fn_client_has_mat(mat chosl_owns_list$)
68080   for chosl_item=1 to chosl_owns_count
68100     if fn_client_has_on_support_item(chosl_owns_list$(chosl_item), chosl_grace_days) then 
68120       let chosl_count+=1
68140       mat chosl_list$(chosl_count)
68160       let chosl_list$(chosl_count)=chosl_owns_list$(chosl_item)
68180     end if 
68200   next chosl_item
68220   let fn_client_has_on_support_list=chosl_count
68240 fnend 
68260 def library fnclient_has_on_support_item(chosi_item$*2; days_grace)
68280   let fn_setup
68300   let fnclient_has_on_support_item=fn_client_has_on_support_item(chosi_item$, days_grace)
68320 fnend 
72000 def fn_client_has_on_support_item(chosi_item$*2; days_grace)
72020   let client_id=val(env$('Client_ID'))
72040   let fn_client_support_setup(client_id,mat chosi_system_id$,mat chosi_system_support_end_date,mat chosi_on_support, days_grace)
72060   let chosi_retun=0
72080   let chosi_which=srch(mat chosi_system_id$,chosi_item$)
72100   if chosi_which>0 then 
72120     let chosi_retun=chosi_on_support(chosi_which)
72140   else 
72160     print 'system ('&chosi_item$&') is not owned by client number '&env$('Client_ID')
72180     if env$('ACSDeveloper')<>'' then pause 
72200   end if 
72220   let fn_client_has_on_support_item=chosi_retun
72240 fnend 
74000 def library fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
74020   let fn_setup
74060   let fnclient_support=fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
74080 fnend 
75000 def fn_client_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
75040   let client_id=val(env$('Client_ID'))
75060   let fn_client_support=fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
75080 fnend 
76000 def fn_client_support_setup(client_id,mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_days_grace)
76020   ! css_days_grace=day grace period to allow users to update after support has expired.
76040   if css_setup<>client_id then ! r:
76060     let cache_css_client_owns_count=fn_client_has_mat(mat css_client_owns$)
76080         mat css_client_owns$(cache_css_client_owns_count)
76100         mat css_system_id$(cache_css_client_owns_count)
76120         mat css_system_id$=css_client_owns$
76140     !
76160     mat cache_css_system_id$(0)
76180     mat cache_css_system_sup_end_date(0)
76200     let cache_css_system_count=0
76220     open #h_support:=fngethandle: "Name=S:\Core\Data\ACS_Sup.dat,Version=2,KFName=S:\Core\Data\ACS_Sup.idx,version=0,Shr",internal,input,keyed 
76240     restore #h_support: ! ,key>==lpad$(trim$(client_id$),kln(h_support)):
76260     do 
76280       read #h_support,using F_SUPPORT: cln$,scode,scode$,sdt1,stm$,sup_exp_date,scst eof CSS_SUPPORT_EOF
76300       F_SUPPORT: form pos 1,g 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
76320       let cln=val(cln$)
76340       if cln=client_id then 
76360         if srch(mat css_client_owns$,fn_system_code_standardize$(scode$))>0 then
76380           let cache_css_system_count+=1
76400           mat cache_css_system_id$(cache_css_system_count)
76420           mat cache_css_system_sup_end_date(cache_css_system_count)
76440           let cache_css_system_id$(cache_css_system_count)=fn_system_code_standardize$(scode$)
76460           let cache_css_system_sup_end_date(cache_css_system_count)=sup_exp_date
76480         end if 
76500       end if 
76520     loop 
76540     CSS_SUPPORT_EOF: ! 
76560     close #h_support: 
76580     let css_setup=client_id
76600   end if  ! /r
76620   ! r: move cache_* arrays into passed in and out arrays
76640     mat css_system_id$(cache_css_system_count)
76660     mat css_system_id$=cache_css_system_id$
76680     mat css_system_support_end_date(cache_css_system_count)
76700     mat css_system_support_end_date=cache_css_system_sup_end_date
76720     !  removed - caused problems on 3/3/2017   !!!   ! if cache_css_system_count>0 then
76740     !  removed - caused problems on 3/3/2017   !!!   !   let cache_css_system_id$(cache_css_system_count)=fn_system_code_standardize$(scode$)
76760     !  removed - caused problems on 3/3/2017   !!!   !   let cache_css_system_sup_end_date(cache_css_system_count)=sup_exp_date
76780     !  removed - caused problems on 3/3/2017   !!!   ! end if
76800   ! /r
78000   ! r: determine if on support
78040     for css_item=1 to cache_css_client_owns_count ! udim(mat css_system_id$)
78082       if css_item>udim(mat css_system_support_end_date) then 
78084         let css_on_support(css_item)=0 ! pause 
78100       else if days(date('ccyymmdd'),'ccyymmdd')<=days(css_system_support_end_date(css_item),'ccyymmdd')+css_days_grace then 
78120         let css_on_support(css_item)=1
78140       else 
78160         let css_on_support(css_item)=0
78180       end if 
78240     next css_item
79000   ! /r
79120 fnend
80000 def library fnsystem_code_standardize$(st_code$*2)
80020   let fn_setup
80040   let fnsystem_code_standardize$=fn_system_code_standardize$(st_code$)
80060 fnend 
82000 def fn_system_code_standardize$(st_code$*2)
82020   ! this function is to translate from systems.h420 to
82040   ! cursys type codes
82060   ! 
82080   let st_code$=uprc$(st_code$)
82100   ! if st_code$='G1' then let st_code$='GL'
82120   if st_code$='G1' then let st_code$='GL'
82140   if st_code$='G3' then let st_code$='G2' ! Accountant's GL Add On
82160   if st_code$='U1' then let st_code$='UB' ! UB No Discount
82180   if st_code$='U2' then let st_code$='UB' ! UB 500-1000 Customers
82200   if st_code$='U3' then let st_code$='UB' ! UB <500 Customers
82220   ! U4 is UB handheld add on in both
82240   if st_code$='P1' then let st_code$='PR'
82260   if st_code$='P2' then let st_code$='P4' ! Job Cost Payroll Add On
82280   let fn_system_code_standardize$=st_code$
82300 fnend 
97000 def library fnclient_is_converting
97020   let fn_setup
97060   let fnclient_is_converting=fn_client_is_converting
97080 fnend 
97100 def fn_client_is_converting
97120   let cic_return=0
97140   if env$('ACSDeveloper')<>'' then
97160     let cic_return=1
97220   else if env$('client')='Kathys Bookkeeping' then
97240     let cic_return=1
97340   end if 
97360   let fn_client_is_converting=cic_return
97380 fnend 
98000 def library fnacs_version$
98020   if ~setup then let fn_setup
98040   open #hBuild:=fngethandle: 'name=S:\Core\Build.txt',d,i
98060   linput #hBuild: build$
98080   close #hBuild:
98100   let fnacs_version$='5.'&rtrm$(build$) ! '5.7211'
98120 fnend 

