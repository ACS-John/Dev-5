00010   enableFavorites=1
00020   fn_setup
00030   fn_setup_once
00040   fn_main
00050   if menu$='Exit and Logout' then
00053     execute "System Logoff"
00056   end if
00060   goto XIT
11000 def fn_setup
11020   if ~setup then
11040     setup=1
11060     library 'S:\Core\Library': fntop
11080     library 'S:\Core\Library': fnSystemName$
11100     library 'S:\Core\Library': fncompany_name
11120     library 'S:\Core\Library': fnflexinit1,fnflexadd1,fnAcs,fnCmdKey,fnButton,fnLbl,fnTos
11140     library 'S:\Core\Library': fnFra
11160     library 'S:\Core\Library': fnbutton_or_disabled
11180     library 'S:\Core\Library': fnmsgbox
11200     library 'S:\Core\Library': fncursys$
11220     library 'S:\Core\Library': fncno
11240     library 'S:\Core\Library': fnputcno
11260     library 'S:\Core\Library': fnchain
11280     library 'S:\Core\Library': fngethandle
11300     library 'S:\Core\Library': fnreg_read
11320     library 'S:\Core\Library': fnreg_write
11340     library 'S:\Core\Library': fnureg_read
11360     library 'S:\Core\Library': fnureg_write
11380     library 'S:\Core\Library': fncreg_read
11400     library 'S:\Core\Library': fncreg_write
11420     library 'S:\Core\Library': fnreg_close
11440     library 'S:\Core\Library': fnerror
11460     library 'S:\Core\Library': fnprogram_properties
11480     library 'S:\Core\Library': fnclient_has
11500     library 'S:\Core\Library': fnclient_has_mat
11520     library 'S:\Core\Library': fnpayroll_client_state$
11540     library 'S:\Core\Library': fndat
11560     library 'S:\Core\Library': fnLastBillingDate
11580     library 'S:\Core\Library': fnclient_is_converting
11600     library 'S:\Core\Library': fnclear_menu
11620     library 'S:\Core\Library': fndisplay_menu
11640     library 'S:\Core\Library': fndecimal_assumed
11680     library 'S:\Core\Library': fntotal_ar
11700     library 'S:\Core\Library': fnindex_sys
11720     library 'S:\Core\Library': fncheckfileversion
11740     library 'S:\Core\Library': fnreport_cache_folder_current$
11760     library 'S:\Core\Library': fnAddOneC
11780     library 'S:\Core\Library': fnFM
11800     library 'S:\Core\Library': fnHamsterFio,fnEditFile
11820     library 'S:\Core\Library': fnxit
11840     library 'S:\Core\Library': fnFavoriteAdd,fnFavoriteList,fnFavoriteDel
11860     library 'S:\Core\Library': fnFileSaveAs,fnOpenPartial
11880     library 'S:\Core\Library': fnClearLayoutCache
11900     library 'S:\Core\Library': fnClientSelect
11920     library 'S:\Core\Programs\PrintAce_Test': fnPrintAceTest
11940     dim system_abbr_list$(1)*20,ml$(1)*128,last_update$*128,last_save$*128
11960     dim resp$(1)*255
11980   end if
11990 fnend
12000 def fn_setup_once
12160   if ~fnclient_has_mat(mat client_has$) then
12180     mat ml$(3)
12200     ml$(1)='Client '&env$('client')&' has nothing licensed.'
12220     ml$(2)='If an update does not solve the issue please contact ACS at 1-800-643-6318'
12240     ml$(3)='Perform an Update now?'
12260     fnmsgbox(mat ml$,resp$,'',16+4)
12280     if uprc$(resp$)=uprc$("Yes") then
12300       chain 'S:\Core\Programs\Update'
12320     else
12340       goto XIT
12360     end if
12380   end if
12400   fn_get_system_abbr_list(mat system_abbr_list$)
12420   if ~udim(mat system_abbr_list$) then
12440     mat ml$(4)
12460     ml$(1)='No systems detected.  Please perform an update.'
12480     ml$(2)='If you have already performed an update and are'
12500     ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
12520     ml$(4)='Perform an Update now?'
12540     fnmsgbox(mat ml$,resp$,'',16+4)
12560     if uprc$(resp$)=uprc$("Yes") then
12580       chain 'S:\Core\Programs\Update'
12600     else
12610       if env$('acsDebug')<>'' then pause
12620       goto XIT
12640     end if
12660   end if
12680   !
12700   dim cursys$*2
12720   cursys$=fncursys$(cursys$)
12740   !   h_plus=fn_open_plus_initial
12760   fndecimal_assumed
12780   fnreg_read('Enable Save Company As',enableSaveCompanyAs$, 'False')
12800   ! fnreg_read('Enable Open Partial',enableOpenPartial$, 'False')
12820   ! fnreg_read('Enable Backup Report Cache',enableBackupReportCache$, 'False')
12840   fn_setup_on_cursys_change
12860   !
12880   dim temp$*2048
12900   temp$=''
12920   fndat(temp$,2) ! set default report heading date to today
12940   ! fnlog('Menu started (Session '&session$&'), wbversion='&wbversion$)
12960   !
12980   !
13020   fntop(program$)
13040   !
13060   fnreg_read('Last Update',last_update$)
13080   if last_update$='' then last_update$='(never updated)'
13100   !     setenv('Icon','S:\Core\Icon\ACS-v5.ico')
13120   if env$('guimode')='OFF' then execute 'config gui on'
13140   if env$('FavoritesOpen')='' then
13160     fnureg_read('FavoritesOpen',FavoritesOpen$, 'False')
13180     setenv('FavoritesOpen',FavoritesOpen$)
13200   end if
13220   fn_grid_setup
13240   fn_checkFileVersionIfNecessary
13260   ! fnreg_read('Report_Cache',report_cache$)
13280   fncreg_write('Company Last Accessed Date and Time',date$('mm/dd/ccyy')&' '&time$)
13300   setenv('ForceScreenIOUpdate','')
13900 fnend
16000 def fn_checkFileVersionIfNecessary
16020   ! r: if necessary detect if this company needs any automatic conversions
16040   fncreg_read('last version used',company_last_version$) ! reads the last version of ACS used to access this particular company
16060   version_current$=env$('acsVersion')
16080   if company_last_version$<version_current$ then
16100     fncheckfileversion
16120     fncreg_write('last version used',version_current$)
16140   end if
16160   ! /r
16180 fnend
17000 def fn_grid_setup
17040   screen_height=35 : grid_height=screen_height-5-dashboard_height
17060   if dashboard_height>0 then grid_height=grid_height-1
17080   ! filter_line=2
17100   ! grid_height$=str$(grid_height)
17140   screen_width=115
17160   ! dim program_grid_spec$*128
17180   info_col_width=18 ! minimum of 15!!!
17200   program_grid_line=2+dashboard_height
17210   if enableFavorites and env$('FavoritesOpen')='True' then
17220     favorite_width=35
17230     program_grid_col=info_col_width+favorite_width+2+2+1
17232     favorite_height=grid_height-1
17240   else
17250     program_grid_col=info_col_width+2
17260   end if
17262   favorite_left=info_col_width+2
17270   if dashboard_height>0 then program_grid_line=program_grid_line+1
17280   ! program_grid_spec$=str$(program_grid_line)&','&str$(program_grid_col)&',List '&grid_height$&'/63'
17290   mat headings$(5)
17300   headings$(1)='Selection'
17310   headings$(2)='+'
17320   headings$(3)='Program'
17340   headings$(4)='File'
17360   headings$(5)='ss_text$'
17380   grid_width=80-favorite_width
17400   !
17420   mat column_mask$(5)
17440   if env$('ACSDeveloper')<>'' then
17460     mat column_mask$=('80')
17480   else
17500     mat column_mask$=('1080')
17520   end if
17540   !   column_mask$(2)='81'
17560   column_mask$(1)='1080'
17580   column_mask$(2)='81'
17600   column_mask$(3)='80'
17620 fnend
18000 def fn_setup_on_cursys_change
18020   dim program_plus$(1)*128,program_name$(1)*80,program_file$(1)*80,program_name_trim$(1)*80,ss_text$(1)*256
18040   fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
18100   fncno(cno)
18120   if cno=0 then
18140     cno=1
18160     fnputcno(cno)
18180     fncno(cno)
18200   end if
18260   ! if ~exists(env$('Q')&'\INI\acs'&env$('cursys')) then execute 'mkdir '&env$('Q')&'\INI\acs'&env$('cursys')
18280   if env$('cursys')='UB' then
18340     fnureg_read('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$)
18480   end if
18500   !
18520   dashboard_height=fn_dashboard_height
18540   fn_grid_setup !
18560   if ~exists(env$('Q')&'\'&env$('cursys')&"mstr\Company.h"&env$('cno')) then
18580     chain "S:\Core\Programs\Select Company.br"
18600   end if
18620 fnend
19000 def fn_caption_update
19020   setenv('Program_Caption',fnSystemName$(env$('cursys')))
19040   fncompany_name(0,screen_width)
19060 fnend
19500 def fn_main
19520   dim program_selection$*256,menu_option$*128
19540   do
19580     if env$('ExitNow')<>'yes' then
19640       fnTos(screen_name$:='Menu') : frameCount=0
19680       fnflexinit1('menu',program_grid_line,program_grid_col,grid_height,grid_width,mat headings$,mat column_mask$)
19700   !
19720       fn_caption_update
19740       fn_update_program_grid
19760       fn_display_buttons
19770       if ~enableFavorites then
19780         fnLbl(program_grid_line,1,'Filter:',info_col_width,1)
19790       end if
19820       fnreg_read('Last Save Date',last_save_date$)
19830       fnreg_read('Last Save File',last_save$)
19840       dim tmp_tooltip$*2048
19850       tmp_tooltip$='Last succesful Save As was to the file\n'&last_save$&'\n on '&last_save_date$
19860       fnLbl(program_grid_line+2,1,last_save_date$(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
19870       fnLbl(program_grid_line+3,1,last_save$(pos(last_save$,'\',-1)+1:len(last_save$))(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
19880       fnLbl(program_grid_line+4,1,login_name$(1:info_col_width),info_col_width,2,0,0,0,'Login Name is "'&login_name$&'"')
19890       fnbutton_or_disabled(env$('enableClientSelection')=='Yes',program_grid_line+5,1,env$('client')(1:info_col_width),fkey_client:=5201, 'Client Name is "'&env$('client')&'"',info_col_width)
19930       ! end if
19940       if env$('Decimal_Assumed')<>'Decimal Required' then
19960         fnLbl(program_grid_line+6,1,env$('Decimal_Assumed'),info_col_width,2)
19980       end if
20000       if env$('ACSDeveloper')<>'' then
20020         fnLbl(program_grid_line+8,1,"ACS Developer"(1:info_col_width),info_col_width,2)
20040         fnLbl(program_grid_line+9,1,env$('ACSDeveloper')(1:info_col_width),info_col_width,2)
20060       end if
20080       if env$('BR_MODEL')='CLIENT/SERVER' then
20100         fnLbl(program_grid_line+11,1,'Client'(1:info_col_width),info_col_width,2)
20120       end if
20130       if env$('BR_XP_Mode')='True' then
20132         fnLbl(program_grid_line+13,1,'XP Compatibility'(1:info_col_width),info_col_width,2)
20134       end if
20160       tmp_tooltip$="ACS was last updated on "&last_update$&'\n to version '&version_current$&'.'
20180       fnLbl(screen_height-8,1,'ACS '&rtrm$(version_current$,'0'),info_col_width,2,0,0,0,tmp_tooltip$)
20190       if env$('acsProduct')<>'ACS Online' then 
20200         fnLbl(screen_height-7,1,last_update$(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
20201       end if 
20202       fnLbl(screen_height-5,1,'BR! '&wbversion$,info_col_width,2)
20220       fn_dashboard_draw
20230       if enableFavorites then let fn_favoritesDraw
20240       fn_display_menu
20260   !
20300       setenv('tmp_acs_back_arrow','S:\Core\Icon\Red_X.png')
20310       fnreg_close ! allow backups to happen while this screen is open...  i think this will work - added 9/14/2017
20320       fnAcs(screen_name$,0,mat resp$,fkey_value,0,0,0)
20340       setenv('tmp_acs_back_arrow','')
20360       program_selection$=resp$(1)
20380       program_selection_id=val(program_selection$(2:pos(program_selection$,']')-1))
20400       program_selection$(1:pos(program_selection$,']'))=''
20420       curfld_value=curfld
20440     end if
20460   !
20480     if fkey_value=93 or fkey_value=99 or (fkey_value=98 and lwrc$(menu$)='exit') or env$('ExitNow')='yes' or menu$='Exit and Logout' then
20500   !  removed 1/21/2017 -  does not seem necessary - it does it when it is selected       fncursys$(env$('cursys'))
20520   !       fn_put_plus(env$('cursys'),mat program_file$,mat program_plus$)
20530       fnureg_write('FavoritesOpen',env$('FavoritesOpen'))
20540       goto XIT_MAIN
20560     else
20580       fn_session_reg_write(env$('cursys')&'.CurProg',str$(program_selection_id))
20600     end if
20620   !
20640   !  if fkey_value=1 then !
20660   ! help button on the bottom
20680   !
20700     if fkey_value=98 then ! r: drop down menu
20720       menu_option$=menu$
20740       if lwrc$(menu_option$(len(menu_option$)-3:len(menu_option$)))='.prc' then
20760         fnclear_menu
20780         execute 'proc '&menu_option$
20800       else if lwrc$(menu_option$(len(menu_option$)-2:len(menu_option$)))='.br' then
20810         ! if uprc$(env$('cursys'))='P4' then
20820         !   menu_option$=srep$(menu_option$,'[cursys]','PR')
20830         ! else
20840           menu_option$=srep$(menu_option$,'[cursys]',env$('cursys'))
20850         ! end if
20860         fnclear_menu
20870         fn_chain(menu_option$)
20880       else if lwrc$(menu_option$(len(menu_option$)-3:len(menu_option$)))='.cmd' then
20890         execute 'Sy -c "'&trim$(menu_option$)&'"'
20900       else if menu_option$(1:8)='Notepad:' then
20910         execute 'Sy -c c:\Windows\notepad.exe "'&trim$(menu_option$(9:inf))&'"'
21000       else if menu_option$(1:6)='FileIO' or menu_option$(1:8)='ScreenIO' then
21120         if menu_option$(1:8)='ScreenIO' then
21140           execute 'cd S:'
21150           library 'S:\Core\ScreenIO\screenio.br': fnDesignScreen
21152           fnDesignScreen
21154           chain 's:\core\start'
21160           ! fn_chain('S:\Core\ScreenIO\screenio.br')
21180         else if menu_option$(1:6)='FileIO' then
21190           if menu_option$='FileIO (update and launch)' then
21191             msgbox( 'obsoleted by local administrative execution of cmd:   mklink /J "C:\Users\John\OneDrive\ACS\Dev-5 Data\Core\FileIO\Layout" "C:\ACS\Dev-5\Core\FileIO\Layout" ')
21192             ! fnCopy('S:\Core\FileIO\Layout\*.*'        ,env$('QBase')&'\Core\FileIO\Layout\*.*'        )
21194             ! fnCopy('S:\Core\FileIO\Layout\version\*.*',env$('QBase')&'\Core\FileIO\Layout\version\*.*')
21196           end if
21200           open  #hProc:=fngethandle: 'name='&env$('temp')&'\fileioproc'&session$&'.$$$,replace',display,output
21210           pr #hProc: 'load S:\Core\FileIO\fileio.br'
21220           pr #hProc: 'cd S:'
21230           pr #hProc: 'Run'
21240           pr #hProc: 'cd C:'
21260           pr #hProc: 'load S:\Core\Menu.br'
21280           pr #hProc: 'run'
21300           close #hProc:
21320           execute 'proc '&env$('temp')&'\fileioproc'&session$&'.$$$'
21340         else
21360           pr 'unrecognized syntax: '&menu_option$ : pause
21380         end if
21400         !
21420       else if lwrc$(menu_option$(1:4))='http' then
21440         execute 'Sy -M -c start '&menu_option$
21460       else if lwrc$(menu_option$(1:5))='file:' then
21480         menu_option$(1:5)=''
21490         if menu_option$='Open' then
21510           fnOpenPartial
21540         else if menu_option$='Save All Data As' then
21560           fnclear_menu
21580           fnFileSaveAs('*.*')
21600         else if menu_option$='Save Company As' then
21610           fnclear_menu
21620           fnFileSaveAs('*h'&env$('cno'))
21630         end if
21640       else if lwrc$(ltrm$(menu_option$)(1:20))='editinwordprocessor:' then
21642         fn_callEditInWordProcessor(menu_option$)
21660       else if lwrc$(ltrm$(menu_option$)(1:11))='hamsterfio:' then
21662         fn_callHamsterFio(menu_option$)
21670       else if menu_option$(1:5)='fnFM(' then
21674         fnFM(menu_option$(6:len(menu_option$)-1))
21680       else if menu_option$(1:14)='fnPrintAceTest' then
21700         fnPrintAceTest(menu_option$(16:len(menu_option$)-1))
21720       else if menu_option$='Index Company' then
21740         fnindex_sys(val(env$('cno')))
21760       else if menu_option$='Restart' then
21770         fnClearLayoutCache
21772         setenv('ForceScreenIOUpdate','yes')
21780         open #h_tmp:=fngethandle: 'Name='&env$('temp')&'\acs_Restart_'&session$&'.prc,replace',display,output
21800         pr #h_tmp: "Stop"
21820         pr #h_tmp: "clear resident"
21860         pr #h_tmp: "chain 'S:\Core\Start'"
21880         close #h_tmp:
21900         execute 'proc '&env$('temp')&'\acs_Restart_'&session$&'.prc'
21920       else if menu_option$='Index System' then
21940         fnindex_sys
21960       else if lwrc$(menu_option$(1:8))='[cursys=' then
22000         cursys$=menu_option$(9:10)
22060         fncursys$(cursys$)
22100         fn_setup_on_cursys_change
22110         fn_checkFileVersionIfNecessary
22120         fn_update_program_grid
22140         fn_caption_update
22220       else if lwrc$(menu_option$(len(menu_option$):len(menu_option$)))='\' then ! it is a Folder - just open it
22240         menu_option$=srep$(menu_option$,'%report_cache_folder_current%',fnreport_cache_folder_current$)
22260         execute 'sy -c -w explorer "'&os_filename$(menu_option$(1:len(menu_option$)-1))&'"'
22280       else
22300         pr 'menu_option$=';menu_option$
22320       end if
22340     !  else if fkey_value=67 then
22360     !      /r  fkey=98
22380     else if fkey_value=3 then
22400       fnclear_menu
22410       fnprogram_properties(trim$(program_name$(program_selection_id))) ! (program_selection$) ! 1 is the flag to chain back to main menu
22420       chain program$
22440     else if fkey_value=209 then
22460       curfld(curfld,fkey) : pr 'fkey 209 encountered (combobox)'
22720     else
23000       ! r: system specific fkey button actions
23020       fkey_favorite_add=1450
23040       fkey_favorite_open=1451
23060       fkey_favorite_close=1452
23080       fkey_favorite_del=1455
23100       fkey_favorite_program_base=1460
23120       if enableFavorites and fkey_value=>fkey_favorite_add and fkey_value<=fkey_favorite_add+50 then
23140         if fkey_value=fkey_favorite_add then
23160           ! pr 'program_selection$="'&program_selection$&'"'
23200           if trim$(program_selection$)='' then
23220             msgbox('Category Headers may not be added to Favorites.')
23240           else
23260             faProgramWhich=srch(mat program_file$,trim$(program_selection$))
23280             if faProgramWhich<=0 then
23300               msgbox('Could not find "'&trim$(program_selection$)&'" to add it to the favorites.')
23320               ! pause
23340             else
23380               fnFavoriteAdd(program_name_trim$(faProgramWhich))
23400             end if
23420           end if
23440           chain program$
23460         else if fkey_value=fkey_favorite_open then
23480           setenv('FavoritesOpen','True')
23500           chain program$
23520         else if fkey_value=fkey_favorite_close then
23540           setenv('FavoritesOpen','False')
23560           chain program$
23580         else if fkey_value=fkey_favorite_del then
23600           favoriteDeleteMode$='True'
23610         else if fkey_value>fkey_favorite_program_base then
23620           ! pr trim$(favorite$(fkey_value-fkey_favorite_program_base))
23630           if favoriteDeleteMode$='True' then ! delete it from the list
23640             fnFavoriteDel(favorite$(fkey_value-fkey_favorite_program_base))
23642             favoriteDeleteMode$='False'
23650           else ! launch it
23660             which=srch(mat program_name_trim$,trim$(favorite$(fkey_value-fkey_favorite_program_base)))
23680             if which<=0 then
23700               msgbox('Could not find "'&trim$(favorite$(fkey_value-fkey_favorite_program_base))&'" on your current menu.')
23720             else
23740               fnchain('S:\'&program_file$(which))
23760             end if
23770           end if
23780         else if env$('acsDeveloper')<>'' then
23800           pr 'fkey_value=';fkey_value
23820           pause
23840         end if
23860 !
24000       else if env$('cursys')='GL' and fkey_value<>0 then
24010         if fnclient_has('G2') and fkey_value=fkey_g2_employee then
24020             fnchain('S:\General Ledger\Accountants\Employee')
24030         else if fkey_value=fkey_gl_Transactions  then
24040           fnchain('S:\General Ledger\Enter Transactions')
24050         else if fkey_value=fkey_gl_accounts  then
24060           fnchain('S:\General Ledger\Accounts')
24070         else if fkey_value=fkey_gl_periodEndingDate  then
24080           fnchain('S:\General Ledger\Period Ending Dates')
24090         end if
24100       else if env$('cursys')='CL' and fkey_value<>0 then
24110         if fkey_value=fkey_cl_unpaid_invoice  then
24120           fnchain('S:\Checkbook\Unpaid Invoice')
24130         else if fkey_value=fkey_cl_print_checks  then
24140           fnchain('S:\Checkbook\Print Checks')
24150         else if fkey_value=fkey_cl_payee  then
24160           fnchain('S:\Checkbook\Payee')
24170         end if
24180       else if env$('cursys')='PR' and fkey_value<>0 then
24190         if fkey_value=fkey_pr_employee  then
24200           fnchain('S:\Payroll\Employee')
24210         else if fkey_value=fkey_pr_enter_time  then
24220           fnchain('S:\Payroll\Enter Time Sheets')
24230         else if fkey_value=fkey_pr_payroll_registers  then
24240           fnchain('S:\acsPR\newprReg1')
24250         else if fkey_value=fkey_pr_print_checks  then
24260           fnchain('S:\Payroll\Print Payroll Checks')
24270         end if
24280       else if env$('cursys')='UB' and fkey_value<>0 then
24290         if fkey_value=fkey_ub_collection then
24300           fnchain('S:\Utility Billing\Collections')
24310         else if fkey_value=fkey_ub_customer then
24320           fnchain('S:\Utility Billing\Customer')
24330         else if fkey_value=fkey_change_billing_date then
24340           fnchain('S:\acsub\Company')
24350         end if
24360       else if env$('cursys')='TM' and fkey_value<>0 then
24370         if fkey_value=fkey_tm_collections then
24380           fnchain('S:\acsTM\arinput')
24390         end if
24400       end if
24410       ! /r
24412       if fkey_client<>0 and fkey_value=fkey_client then
24414           fnClientSelect
24416           fnchain(program$)
24420       else if (curfld_value=1 and fkey_value=201) or fkey_value=4 then
24430         ! if program_plus$(program_selection_id)='+' then
24440         !   program_plus$(program_selection_id)='-'
24450         ! else if program_plus$(program_selection_id)='-' then
24460         !   program_plus$(program_selection_id)='+'
24462         if lwrc$(ltrm$(program_selection$)(1:11))='hamsterfio:' then  ! else if
24464           fn_callHamsterFio(program_selection$)
24470         else if lwrc$(ltrm$(program_selection$)(1:20))='editinwordprocessor:' then  ! else if
24480           fn_callEditInWordProcessor(program_selection$)
24490         else if program_selection$<>'' then
24500           fn_chain('S:\'&trim$(program_selection$))
24510         end if
24520       end if
24530     end if
24540 !
24550   loop
24560   XIT_MAIN: !
24570 fnend
25000 def fn_callHamsterFio(tmpCap$*128)
25020   tmpCap$=trim$(tmpCap$)
25040   tmpCap$=tmpCap$(12:len(tmpCap$))
25060   if uprc$(tmpCap$(1:3))='CO ' then !
25080     tmpCap$(1:3)=''
25100     tmpCursys$='CO'
25120   else
25140     tmpCursys$=env$('cursys')
25160   end if
25180   fntop('S:\'&fnSystemName$&'\'&tmpCap$&'.br',tmpCap$)
25200   fnHamsterFio(tmpCursys$&' '&tmpCap$)
25220   fnxit
25240 fnend
25500 def fn_callEditInWordProcessor(programSelection$*256)
25510   programSelection$=trim$(programSelection$)
25520   programSelection$(1:20)=''
25530   dim fileToEditInWp$*256,options$*256
25540   if (cewPosSpace1:=pos(programSelection$,' '))>0 then 
25550     fileToEditInWp$=programSelection$(1:cewPosSpace1-1)
25560     options$=programSelection$(cewPosSpace1+1:inf)
25570   else
25580     fileToEditInWp$=programSelection$
25590   end if
25600   fileToEditInWp$=srep$(fileToEditInWp$,'[Q]',env$('Q'))
25610   fileToEditInWp$=srep$(fileToEditInWp$,'[cno]',env$('cno'))
25620   fileToEditInWp$=srep$(fileToEditInWp$,'[CNo]',env$('cno'))
25630   if pos(options$&' ','makeIfNecessary ')>0 then cewMakeIfNecessary=1 else cewMakeIfNecessary=0
25640   if pos(options$&' ','forceAtlantis ')>0 then cewForceAtlantis=1 else cewForceAtlantis=0
25650   if ~cewForceAtlantis and cewMakeIfNecessary then
25660     pr 'can only MakeIfNEcessary if also forceAtlantis.  Please consider enhancing the code.' : pause
25670   end if
25680   if cewForceAtlantis then cewForce$='atlantis' else cewForce$='wordprocessor'
25700   !
25710   fn_callEditInWordProcessor=fnEditFile(cewForce$,fileToEditInWp$)
25720 fnend
26000 def fn_dashboard_height
26020   if env$('cursys')="OE" then
26040     dhReturn=1
26060   else if env$('cursys')="CL" then
26080     dhReturn=1
26100   else if env$('cursys')="PR" then
26120     dhReturn=1
26140   else if env$('cursys')="GL" then
26150     if fnclient_has('G2') then
26160       dhReturn=2
26162     else
26164       dhReturn=1
26170     end if
26180   else if env$('cursys')="UB" then
26200     dhReturn=1
26220   else if env$('cursys')="TM" then
26240     dhReturn=3
26260   else
26280     dhReturn=0
26300   end if
26320   !
26340   if enableFavorites and dhReturn<2 then
26360     dhReturn=2
26380   end if
26400   !
26410   if env$('ACSDeveloper')<>'' then dhReturn=5
26420   fn_dashboard_height=dhReturn
26440 fnend
27000 def fn_ddAddButton(buttonText$,btnFkey,btnItem,tmp_btn_width; buttonLine,tooltip$*150) ! buttons are added and counted (btnItem) from right to left
27010   if buttonLine=0 then buttonLine=1
27020   if btnItem=1 then
27040     fnButton(buttonLine,dashboard_width-tmp_btn_width,buttonText$,btnFkey,tooltip$,1,tmp_btn_width,fraDashboard)
27060   else if btnItem>1 then
27080     fnButton(buttonLine,dashboard_width-(tmp_btn_width*btnItem+(btnItem-1)),buttonText$,btnFkey,tooltip$,1,tmp_btn_width,fraDashboard)
27100   else
27120     pr 'btnItem=';btnItem;' and it is currently required by fn_ddAddButton'
27140   end if
27160 fnend
28000 def fn_favoritesDraw
28020   if env$('FavoritesOpen')='True' then
28040     ! fnFra(program_grid_line,favorite_left,grid_height-1,favorite_width,'Favorites')
28060     fnFra(dashboard_height+3,favorite_left,favorite_height,favorite_width,'Favorites') :  : frameCount+=1 : fraFavorites=frameCount
28080     dim favorite$(0)*128
28100     fnFavoriteList(mat favorite$) 
28120     fnButton(1,1,'Close',fkey_favorite_close:=1452,'Close Favorites',0,6,fraFavorites)
28140     fnbutton_or_disabled(favoriteDeleteMode$<>'True',1,15,'Delete',fkey_favorite_del:=1455,'To remove a favorite, click this "Delete" button and then click the favorite.',6,fraFavorites)
28160     fnbutton_or_disabled(1,1,favorite_width-6,'Add',fkey_favorite_add:=1450,'To add a favorite, highlite a menu option and click this "add" button.',6,fraFavorites)
28180     if favoriteDeleteMode$='True' then
28200       fnLbl(2,1,'Select Favorite to Delete',favorite_width,2,0,fraFavorites)
28220     end if
28240     for favItem=1 to min(udim(mat favorite$),favorite_height-2)
28250       fkey_favorite_program_base:=1460
28260       fnButton(favItem+2,1,favorite$(favItem),fkey_favorite_program_base+favItem,'',1,favorite_width-1,fraFavorites)
28280     nex favItem
28300   end if
28320 fnend
29000 def fn_dashboard_draw
29010   if dashboard_height>0 then
29020     dashboard_width=screen_width-4
29030     fnFra(1,1,dashboard_height,dashboard_width,'Dashboard') : frameCount+=1 : fraDashboard=frameCount
29032     if enableFavorites then
29034       fnbutton_or_disabled(env$('FavoritesOpen')<>'True',2,favorite_left,'Favorites',fkey_favorite_open:=1451,'',20,fraDashboard)
29036     end if
29040     if env$('cursys')="CL" then
29050       tmp_btn_width=14 : tmpBtnItem=0
29060       fn_ddAddButton('Unpaid Invoice',fkey_cl_unpaid_invoice:=5001,tmpBtnItem+=1,tmp_btn_width)
29070       fn_ddAddButton('Payee',fkey_cl_payee:=5003,tmpBtnItem+=1,tmp_btn_width)
29080       fn_ddAddButton('Print Checks',fkey_cl_print_checks:=5002,tmpBtnItem+=1,tmp_btn_width)
29090     else if env$('cursys')="PR" then
29100       fnLbl(1,1,'Payroll State:',15,1,0,fraDashboard)
29110       fnLbl(1,17,fnpayroll_client_state$,4,0,0,fraDashboard)
29120       tmp_btn_width=10 : tmpBtnItem=0
29130       fn_ddAddButton('Checks',fkey_pr_print_checks:=5004,tmpBtnItem+=1,tmp_btn_width)
29140       fn_ddAddButton('Registers',fkey_pr_payroll_registers:=5003,tmpBtnItem+=1,tmp_btn_width)
29150       fn_ddAddButton('Enter Time',fkey_pr_enter_time:=5002,tmpBtnItem+=1,tmp_btn_width)
29160       fn_ddAddButton('Employee',fkey_pr_employee:=5001,tmpBtnItem+=1,tmp_btn_width)
29170     else if env$('cursys')="GL" then
29180       library 'S:\Core\Library': fnpedat$
29190       open #h_tmp:=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outIn,relative ioerr DD_GL_XIT
29200       read #h_tmp,using 'Form Pos 296,n 2',rec=1: lmu
29210       close #h_tmp:
29220       fnLbl(1,1,'Last Period Closed:',19,1,0,fraDashboard)
29230       fnLbl(1,21,str$(lmu),4,0,0,fraDashboard)
29240       fnLbl(1,26,'Pay Ending Date:',16,1,0,fraDashboard)
29250       dim pedat$*20
29260       pedat$=fnpedat$
29270       if pedat$='' then pedat$='(not set)'
29380       fnButton(1,44,pedat$,fkey_gl_periodEndingDate:=5003,'',1,20,fraDashboard) ! fnLbl(1,47,fnpedat$,4,0,0,1)
29400     DD_GL_XIT: !
29420       tmp_btn_width=10 : tmpBtnItem=0
29440       fn_ddAddButton('Accounts',fkey_gl_accounts:=5001,tmpBtnItem+=1,tmp_btn_width,1,'General Ledger Master')
29460       fn_ddAddButton('Transactions',fkey_gl_Transactions:=5002,tmpBtnItem+=1,tmp_btn_width,1,'Enter Transactions')
29480       if fnClient_has('G2') then
29500         tmpBtnItem=0
29520         fn_ddAddButton('Employee',fkey_g2_employee:=5011,tmpBtnItem+=1,tmp_btn_width, 2)
29540       end if
30000     else if env$('cursys')="UB" then
30010       fnLastBillingDate(d1) : d1$=date$(days(d1,'mmddyy'),'mm/dd/ccyy')
30020     !           fnLbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon)
30030       fnLbl(1,1,'Last Billing Date:',18,1,0,1)
30040       fnLbl(1,20,d1$,4,0,0,1)
30050       if env$("ACSDeveloper")<>"" then
30060         fkey_change_billing_date=5001
30070         fnButton(1,32,'Change',fkey_change_billing_date,'Select a new current Billing Date',1,6,fraDashboard)
30080       end if
30090       tmp_btn_width=11 : tmpBtnItem=0
30100       fn_ddAddButton('Collections',fkey_ub_collection:=5002,tmpBtnItem+=1,tmp_btn_width)
30110       fn_ddAddButton('Customer',fkey_ub_customer:=5003,tmpBtnItem+=1,tmp_btn_width)
30120       if ub_total_ar_on_dashboard$='True' then
30130         fnLbl(1,40,'Total Accounts Receivable:',26,1,0,1)
30140         fnLbl(1,68,str$(fntotal_ar),4,0,0,1)
30150       end if
30500     else if env$('cursys')="TM" then
30510       tmp_btn_width=11 : tmpBtnItem=0
30520       fn_ddAddButton('Collections',fkey_tm_collections:=5001,tmpBtnItem+=1,tmp_btn_width)
30530       fnLbl(1,1,'Time Management is for Advanced Computer Services LLC only.',0,0,0,fraDashboard)
30540     end if
30550     ! if env$('acsDeveloper')<>'' then
30560     !   fnLbl(3,1,'---Developer---',0,0,0,fraDashboard)
30570     !   fnLbl(4,1,'QBase: "'&env$('QBase')&'"',(dashboard_width/2)-1,0,0,fraDashboard)
30571     !   fnLbl(4,int(dashboard_width/2)+1,'env$(Q): "'&env$('Q')&'"',int(dashboard_width/2)-1,0,0,fraDashboard)
30572     !   fnLbl(5,1,' Data: "'&env$('Data')&'"',0,0,0,fraDashboard)
30580     ! end if
30590   end if
30600 fnend
32000 def fn_display_buttons
32040   fnCmdKey('OK' ,4,1,0,'Press "OK" to launch the selected program')
32050   fnCmdKey('Help' ,1,0,0,'Press "Help" to launch the help page about this program')
32060   fnCmdKey('Properties',3,0,0,'Press "Properties" to view the properties this program')
32070   fnCmdKey('Exit',99,0,1,'Press "Exit" to quit')
32080 fnend  ! fn_display_buttons
34000 def fn_session_reg_read(ls_field_name$*128,&ls_field_value$)
34020   fn_session_reg_read=fnreg_read(session$&'.'&ls_field_name$,ls_field_value$)
34040 fnend
34060 def fn_session_reg_write(ls_field_name$*128,ls_field_value$*256)
34080   fn_session_reg_write=fnreg_write(session$&'.'&ls_field_name$,ls_field_value$)
34100 fnend
42000 def fn_get_system_abbr_list(mat system_abbr_list$)
42010   dim system_name$(0)*40
42020   mat system_name$(0)
42030   mat system_abbr_list$(0)

42200   ! if env$('ACSDeveloper')<>'' and exists('S:\Time Management\Menu.mnu') then
42220   !   fnAddOneC(mat system_abbr_list$,'TM')
42240   !   fnAddOneC(mat system_name$,fnSystemName$('TM'))
42260   ! end if
42270   fn_add_if_licensed('TM')
42280   fn_add_if_licensed('OE')
42300   fn_add_if_licensed('CL')
42320   fn_add_if_licensed('GL')
42340   fn_add_if_licensed('PR')
42360   fn_add_if_licensed('UB')
42380 fnend
43000 def fn_add_if_licensed(sysCode$)
43020   if (fnclient_has(sysCode$) or env$('acsDeveloper')<>'') and exists('S:\'&fnSystemName$(sysCode$)&'\Menu.mnu') then
43040     fnAddOneC(mat system_abbr_list$,sysCode$)
43060     fnAddOneC(mat system_name$,fnSystemName$(sysCode$))
43080   end if
43100 fnend
44000 def library fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
44020   if ~setup then let fn_setup
44040   fnGetProgramList=fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
44060 fnend
44080 def fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
44100   mat program_plus$(0) : mat program_name$(0) : mat program_name_trim$(0) : mat program_file$(0) : mat ss_text$(0)
44120   glpa_program_count=0
44140   fn_getProgramList_add('S:\'&fnSystemName$(env$('cursys'))&'\Menu.mnu')
44160   if env$("ACSDeveloper")<>"" then
44180     fn_getProgramList_add('S:\'&env$('CurSystem')&'\Programmer.mnu')
44200   end if  ! serial=env$('ACSDeveloper')<>''
44220 fnend
46000 def fn_getProgramList_add(gpla_file$*256;___,sign$)
46020   dim ss_category$(1)*80
46030   dim program_item$(1)*512
46040   !
46060   open #1: 'Name='&gpla_file$,display,input ioerr GPLA_XIT
46080   linput #1: temp$ eof GPLA_EOF ! just consume first line
46100   do
46120     linput #1: temp$ eof GPLA_EOF
46140     if trim$(temp$)<>'' and trim$(temp$)(1:1)<>'!' then
46160       str2mat(temp$,mat program_item$,'^')
46180       if udim(mat program_item$)>=3 then
46200         requirment$=trim$(program_item$(3))
46220       else
46240         requirment$=''
46260       end if
46280       if requirment$='' or fnclient_has(requirment$) then
46680         glpa_program_count+=1
46700         program_item_count=udim(mat program_item$)
46720         mat program_plus$(glpa_program_count)
46740         mat program_name$(glpa_program_count)
46742         mat program_name_trim$(glpa_program_count)
46760         mat program_file$(glpa_program_count)
46780         mat ss_text$(glpa_program_count)
46800         mat program_level(glpa_program_count)
46820         program_level(glpa_program_count)=fn_program_level(program_item$(1))
46840         !
46860         program_name$(glpa_program_count)=srep$(rtrm$(program_item$(1)),'>','         ')
46870         program_name_trim$(glpa_program_count)=trim$(program_name$(glpa_program_count))
46880         !
46900         if program_item_count>1 then program_file$(glpa_program_count)=trim$(program_item$(2))
46920         !
46940         if trim$(program_file$(glpa_program_count))='' then
46960           program_plus$(glpa_program_count)='**' ! fn_get_one_plus$(h_plus,env$('cursys'),program_file$(glpa_program_count))
46980         end if
47000         !
47020         ss_text$(glpa_program_count)='' ! ss_text$(glpa_program_count)&cnvrt$('Pic(#####)',glpa_program_count)
47040         gt_count=len(program_item$(1)(1:10))-len(srep$(program_item$(1)(1:10),'>',''))
47060         !
47080         for gt_item=1 to 10
47100           if gt_count=gt_item-1 then mat ss_category$(gt_item) : ss_category$(gt_item)=program_name$(glpa_program_count)
47120         next gt_item
47140         for ss_cat_item=1 to udim(mat ss_category$)
47160           ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&' - '&ltrm$(ss_category$(ss_cat_item))
47180         next ss_cat_item
47200         ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&ltrm$(program_plus$(glpa_program_count))
47220         !
47240         if program_item_count>1 then ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&' ~ '&program_item$(2)
47250       end if
47260     end if
47280   loop
47300   GPLA_EOF: !
47320   close #1: ioerr ignore
47340   GPLA_XIT: !
47360 fnend
50000 def fn_program_level(tmp$*512) !
50020   chr_pos=0
50080   do
50120     chr_pos+=1
50220   loop while tmp$(chr_pos:chr_pos)='>'
50240   fn_program_level=chr_pos
50260 fnend
60000 def fn_update_program_grid
60020   col_return=1
60040   col_plus=2
60060   col_name=3
60080   col_file=4
60100   col_ss_text=5
60120   dim program_grid_row$(5)*255
60121   dim program_selection_id$*256
60122   setenv('current_grid_row',str$(1))
60130   program_selection_id=0
60132   fn_session_reg_read(env$('cursys')&'.CurProg',program_selection_id$) : program_selection_id=val(program_selection_id$) conv ignore
60140   if udim(mat program_name$) then
60160     hide_level=0
60180     hiding=0
61000     for upg_item=1 to udim(mat program_name$)
61020       if hiding and program_level(upg_item)=hide_level and program_plus$(upg_item)='+' then
61040         fn_upg_show_it(upg_item)
61060       else if hiding and program_level(upg_item)<=hide_level then
61080         hiding=0
61100         pr '  turning off hide at '&program_name$(upg_item)
61120         hide_level=0
61140       else if ~hiding and program_plus$(upg_item)='+' then
61160         hide_level=program_level(upg_item)
61180         hiding=1
61200         pr '--' : pr '  turning ON hide  after '&program_name$(upg_item)
61220         fn_upg_show_it(upg_item)
61240       end if
61260       if hiding then
61300       else if ~hiding then
61320         fn_upg_show_it(upg_item)
61340       end if
61360     next upg_item
61380   end if
61400 fnend
68620 def fn_upg_show_it(upg_item)
68640   program_grid_row$(col_return)='['&str$(upg_item)&']'&program_file$(upg_item)
68660   program_grid_row$(col_plus)=program_plus$(upg_item)
68680   program_grid_row$(col_name)=program_name$(upg_item)
68700   program_grid_row$(col_file)=program_file$(upg_item)
68720   if program_selection_id=upg_item then
68750     setenv('current_grid_row',str$(upg_item))
68760   end if
68780   program_grid_row$(col_ss_text)=ss_text$(upg_item)
68800   fnflexadd1(mat program_grid_row$)
68840 fnend
70000 def fn_display_menu
70020   if ~dm_setup then
70040     dm_setup=1
70060     dim m_a$(1)*256,m_b$(1)*256,m_c$(1)*256
70080     mat m_a$(0) : mat m_b$(0) : mat m_c$(0)
70100     dim menu_option$*255
70120     x=5000
70140     fn_dm_add('&File',str$(x+=1))
70180     fn_dm_add(' &Open...','File:Open')
70182     ! if enableOpenPartial$='True' then
70184     !   fn_dm_add(' Open Partial...','File:Open Partial')
70186     ! end if
70220     fn_dm_add(' &Save As...','File:Save All Data As')
70230     if enableSaveCompanyAs$='True' then
70240       fn_dm_add(' Save Company As...','File:Save Company As')
70250     end if
70260     fn_dm_add(' -')
70280     fn_dm_add(' Preferences','S:\Core\Programs\Preferences.br')
70300     ! if report_cache$='True' then
70320     fn_dm_add(' Open &Report Cache','%report_cache_folder_current%\')
70340     ! end if
70360     fn_dm_add(' -')
70400     fn_dm_add(' E&xit'&chr$(9)&'Alt+F4','Exit')
70402     if env$('BR_MODEL')='CLIENT/SERVER' then
70410       fn_dm_add(' E&xit and Logout','Exit and Logout')
70412     end if
70420     if udim(mat system_name$)>1 then
70440       fn_dm_add('&System',str$(x+=1))
70460       for system_abbr_list_item=1 to udim(mat system_abbr_list$)
70480         if system_abbr_list$(system_abbr_list_item)<>'P2' and system_abbr_list$(system_abbr_list_item)<>'G2' then
70500           fn_dm_add(' '&system_name$(system_abbr_list_item),'[cursys='&system_abbr_list$(system_abbr_list_item)&']')
70520         end if
70540       next system_abbr_list_item
70560     end if
70580     fn_dm_add('&Company',str$(x+=1))
70600     fn_dm_add(' &Select','S:\Core\Programs\Select Company.br')
70610     if exists('S:\'&fnSystemName$&'\Company.br') then
70620       fn_dm_add(' Configure','S:\'&fnSystemName$&'\Company.br')
70630     else
70640       fn_dm_add(' Configure','S:\acs[cursys]\Company.br')
70650     end if
70700     if fnclient_is_converting then
70720       fn_dm_add(' -')
70740       fn_dm_add(' Import','S:\Core\Company Import.br')
70760     end if
70770     fn_dm_add('&Utilities')
70772     fn_dm_add(' Grids','S:\Core\PrtFlex\PrtFlex1.br')
70774     fn_dm_add(' City State Zip','HamsterFio:CO City State Zip')
70776     fn_dm_add(' -')
70778     ! fn_dm_add(' Registry')
70780     ! fn_dm_add('  Company','S:\Core\CReg_Hamster.br')
70782     ! fn_dm_add('  Standard and User','S:\Core\Reg_Hamster.br')
70784     ! fn_dm_add('  System','HamsterFio:CO System Registry')
70790     !  fn_dm_add(' PDF Test','S:\Core\PDF_Test.br')
70792     fn_dm_add(' PrintAce Test - &PDF','fnPrintAceTest(PDF)')
70794     fn_dm_add(' PrintAce Test - Print&Ace','fnPrintAceTest(PrintAce)')
70796     !  fn_dm_add(' PrintAce &Test','S:\Core\Programs\PrintAce_Test.br')
70800     fn_dm_add(' Recreate &Indexes')
70820     fn_dm_add('  &Current '&env$('cursys')&' Company','Index Company')
70840     fn_dm_add('  &All '&env$('cursys')&' Companies','Index System')
70860     fn_dm_add(' &Check File Versions','S:\Core\Check File Versions.br')
70880     !  fn_dm_add(' PrintAce')
70920     !  fn_dm_add('  -')
70940     fn_dm_add(' PrintAce Install &Dependencies','S:\Core\Programs\PrintAce_Setup.br')
70960     if env$('BR_MODEL')<>'CLIENT/SERVER' then
70980       fn_dm_add(' Client Server','S:\Core\Client_Server.br')
71000     end if
71020     fn_dm_add(' Restart','Restart')
71040     fn_dm_add('&Help')
71060     fn_dm_add(' Help','http://planetacs.net/help/')
71080     fn_dm_add(' ACS Website','http://planetacs.net')
71100     if env$('BR_MODEL')<>'CLIENT/SERVER' then
71120       fn_dm_add(' Update','S:\Core\Programs\Update.br')
71140     end if
71160     !  fn_dm_add(' -')
71180     !  fn_dm_add(' Install Host for Live Support','http://get.teamviewer.com/acsllc') ! 'S:\Core\ACS_Support.exe')
71200     fn_dm_add(' -')
71220     fn_dm_add(' About','S:\Core\Programs\About.br')
71240     if env$('ACSDeveloper')<>'' then
71260       fn_dm_add('De&veloper')
71280       if env$('acsEnableComplier')='Yes' then
71300         fn_dm_add(' Re&compile','S:\Core\Start.br') : setenv('compile_without_asking','Yes') ! Recompile changed source
71320         fn_dm_add(' Retry last compile proc','C:\ACS\Dev-5\(import)\compile.prc') : setenv('compile_without_asking','Yes') ! Recompile changed source
71340       end if
71350       fn_dm_add(' Release Notes','Notepad: C:\ACS\Dev-5\Core\Release_Notes.txt')
71351       fn_dm_add(' Increase Build Version','C:\ACS\Util\Build Update.cmd')
71352       fn_dm_add(' Refresh N++ Function CallTips','S:\Dev\Notepad++ CallTip Refresh.br')
71360       ! fn_dm_add(' -')
71361       ! fn_dm_add(' &ScreenIO Test - MeterAddressSelect','fnFM(MeterAddressSelect)')
71362       fn_dm_add(' -')
71370       fn_dm_add(' Client','S:\Core\Client.br')
71380       fn_dm_add(' Registry')
71400       fn_dm_add('  Company','S:\Core\CReg_Hamster.br')
71420       fn_dm_add('  Standard and User','S:\Core\Reg_Hamster.br')
71430       fn_dm_add('  System','HamsterFio:CO System Registry')
71440       fn_dm_add(' -')
71450       fn_dm_add(' FileIO','FileIO')
71460       !   fn_dm_add(' FileIO (update and launch)','FileIO (update and launch)')
71500       fn_dm_add(' ScreenIO','ScreenIO')
71510       fn_dm_add(' -')
71512       fn_dm_add(' Locate 1','S:\Core\Locate.br')
71520     end if
71540     if env$('ACSDeveloper')<>'' and exists(env$('Q')&'\tmmstr\Company.h420') then ! trim$(env$("ACSDeveloper"))<>""
71560       fn_dm_add('ACS LLC')
71580       fn_dm_add(' Client 420','S:\acsTM\Client.br')
71600       fn_dm_add(' Support 420','S:\acsTM\Support.br')
71620     end if
71640   end if  ! ~dm_setup
71660   fndisplay_menu(mat m_a$,mat m_b$,mat m_c$)
71680 fnend
73000 def fn_dm_add(a$*256; b$*256,c$*1)
73020   mat m_a$(udim(mat m_a$)+1) : m_a$(udim(mat m_a$))=a$
73040   mat m_b$(udim(mat m_b$)+1) : m_b$(udim(mat m_b$))=b$
73050   if c$='' then c$='E'
73060   mat m_c$(udim(mat m_c$)+1) : m_c$(udim(mat m_c$))=c$
73080 fnend
74000 def fn_chain(c_program$*128)
74020   pr newpage
74040   fnchain(c_program$)
74060 fnend
75000 XIT: execute "System"
75060 IGNORE: continue
76020 ! <updateable region: ertn>
76040 ERTN: fnerror(program$,err,line,act$,"xit")
76060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76120 ERTN_EXEC_ACT: execute act$ : goto ERTN
76140 ! </updateable region: ertn>


