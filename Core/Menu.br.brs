  enableFavorites=1
  fn_setup
  fn_setup_once
  fn_main
  if menu$='Exit and Logout' then
    execute "System Logoff"
  end if
  goto XIT
def fn_setup
  if ~setup then
    setup=1
    ! {coreLibrary}
    library 'S:\Core\Library': fntop
    library 'S:\Core\Library': fnSystemName$
    library 'S:\Core\Library': fncompany_name
    library 'S:\Core\Library': fnflexinit1,fnflexadd1,fnAcs,fnCmdKey,fnButton,fnLbl,fnTos,fnchk
    library 'S:\Core\Library': fnFra
    library 'S:\Core\Library': fnbutton_or_disabled
    library 'S:\Core\Library': fnmsgbox
    library 'S:\Core\Library': fncursys$
    library 'S:\Core\Library': fncno
    library 'S:\Core\Library': fnputcno
    library 'S:\Core\Library': fnchain
    library 'S:\Core\Library': fngethandle
    library 'S:\Core\Library': fnreg_read
    library 'S:\Core\Library': fnreg_write
    library 'S:\Core\Library': fnureg_read
    library 'S:\Core\Library': fnureg_write
    library 'S:\Core\Library': fncreg_read
    library 'S:\Core\Library': fncreg_write
    library 'S:\Core\Library': fnreg_close
    library 'S:\Core\Library': fnerror
    library 'S:\Core\Library': fnprogram_properties
    library 'S:\Core\Library': fnclient_has
    library 'S:\Core\Library': fnclient_has_mat
    library 'S:\Core\Library': fnpayroll_client_state$
    library 'S:\Core\Library': fndat
    library 'S:\Core\Library': fnLastBillingDate
    library 'S:\Core\Library': fnclient_is_converting
    library 'S:\Core\Library': fnclear_menu
    library 'S:\Core\Library': fndisplay_menu
    library 'S:\Core\Library': fndecimal_assumed
    library 'S:\Core\Library': fntotal_ar
    library 'S:\Core\Library': fnindex_sys
    library 'S:\Core\Library': fncheckfileversion
    library 'S:\Core\Library': fnreport_cache_folder_current$
    library 'S:\Core\Library': fnAddOneC
    library 'S:\Core\Library': fnFM
    library 'S:\Core\Library': fnHamsterFio,fnEditFile
    library 'S:\Core\Library': fnxit
    library 'S:\Core\Library': fnFavoriteAdd,fnFavoriteList,fnFavoriteDel
    library 'S:\Core\Library': fnFileSaveAs,fnOpenPartial
    library 'S:\Core\Library': fnClearLayoutCache
    library 'S:\Core\Library': fnClientSelect
    library 'S:\Core\Programs\PrintAce_Test': fnPrintAceTest
    dim system_abbr_list$(1)*20,ml$(1)*128,last_update$*128,last_save$*128
    dim resp$(32)*255
  end if
fnend
def fn_setup_once
  if ~fnclient_has_mat(mat client_has$) then
    mat ml$(3)
    ml$(1)='Client '&env$('client')&' has nothing licensed.'
    ml$(2)='If an update does not solve the issue please contact ACS at 1-800-643-6318'
    ml$(3)='Perform an Update now?'
    fnmsgbox(mat ml$,resp$,'',16+4)
    if uprc$(resp$)=uprc$("Yes") then
      chain 'S:\Core\Programs\Update'
    else
      goto XIT
    end if
  end if
  fn_get_system_abbr_list(mat system_abbr_list$)
  if ~udim(mat system_abbr_list$) then
    mat ml$(4)
    ml$(1)='No systems detected.  Please perform an update.'
    ml$(2)='If you have already performed an update and are'
    ml$(3)='still receiving this message contact ACS at 1-800-643-6318'
    ml$(4)='Perform an Update now?'
    fnmsgbox(mat ml$,resp$,'',16+4)
    if uprc$(resp$)=uprc$("Yes") then
      chain 'S:\Core\Programs\Update'
    else
      if env$('acsDebug')<>'' then pause
      goto XIT
    end if
  end if
  !
  dim cursys$*2
  cursys$=fncursys$(cursys$)
  !   h_plus=fn_open_plus_initial
  fndecimal_assumed
  fnreg_read('Enable Save Company As',enableSaveCompanyAs$, 'False')
  ! fnreg_read('Enable Open Partial',enableOpenPartial$, 'False')
  ! fnreg_read('Enable Backup Report Cache',enableBackupReportCache$, 'False')
  fn_setup_on_cursys_change
  !
  dim temp$*2048
  temp$=''
  fndat(temp$,2) ! set default report heading date to today
  ! fnlog('Menu started (Session '&session$&'), wbversion='&wbversion$)
  !
  !
  fntop(program$)
  !
  fnreg_read('Last Update',last_update$)
  if last_update$='' then last_update$='(never updated)'
  !     setenv('Icon','S:\Core\Icon\ACS-v5.ico')
  if env$('guimode')='OFF' then execute 'config gui on'
  if env$('FavoritesOpen')='' then
    fnureg_read('FavoritesOpen',FavoritesOpen$, 'False')
    setenv('FavoritesOpen',FavoritesOpen$)
  end if
  fn_grid_setup
  fn_checkFileVersionIfNecessary
  ! fnreg_read('Report_Cache',report_cache$)
  fncreg_write('Company Last Accessed Date and Time',date$('mm/dd/ccyy')&' '&time$)
  setenv('ForceScreenIOUpdate','')
fnend
def fn_checkFileVersionIfNecessary
  ! if necessary detect if this company needs any automatic conversions
  fncreg_read('last version used',company_last_version$) ! reads the last version of ACS used to access this particular company
  version_current$=env$('acsVersion')
  if company_last_version$<version_current$ then
    fncheckfileversion
    fncreg_write('last version used',version_current$)
  end if
fnend
def fn_grid_setup
  screen_height=35 : grid_height=screen_height-5-dashboard_height
  if dashboard_height>0 then grid_height=grid_height-1
  ! filter_line=2
  ! grid_height$=str$(grid_height)
  screen_width=115
  ! dim program_grid_spec$*128
  info_col_width=18 ! minimum of 15!!!
  program_grid_line=2+dashboard_height
  if enableFavorites and env$('FavoritesOpen')='True' then
    favorite_width=35
    program_grid_col=info_col_width+favorite_width+2+2+1
    favorite_height=grid_height-1
  else
    program_grid_col=info_col_width+2
  end if
  favorite_left=info_col_width+2
  if dashboard_height>0 then program_grid_line=program_grid_line+1
  ! program_grid_spec$=str$(program_grid_line)&','&str$(program_grid_col)&',List '&grid_height$&'/63'
  mat headings$(5)
  headings$(1)='Selection'
  headings$(2)='+'
  headings$(3)='Program'
  headings$(4)='File'
  headings$(5)='ss_text$'
  grid_width=80-favorite_width
  !
  mat column_mask$(5)
  if env$('ACSDeveloper')<>'' then
    mat column_mask$=('80')
  else
    mat column_mask$=('1080')
  end if
  !   column_mask$(2)='81'
  column_mask$(1)='1080'
  column_mask$(2)='81'
  column_mask$(3)='80'
fnend
def fn_setup_on_cursys_change
  dim program_plus$(1)*128,program_name$(1)*80,program_file$(1)*80,program_name_trim$(1)*80,ss_text$(1)*256
  fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
  fncno(cno)
  if cno=0 then
    cno=1
    fnputcno(cno)
    fncno(cno)
  end if
  ! if ~exists('[Q]\INI\acs'&env$('cursys')) then execute 'mkdir [Q]\INI\acs'&env$('cursys')
  if env$('cursys')='UB' then
    fnureg_read('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$)
  end if
  !
  dashboard_height=fn_dashboard_height
  fn_grid_setup !
  if ~exists('[Q]\'&env$('cursys')&"mstr\Company.h[cno]") then
    chain "S:\Core\Programs\Select Company.br"
  end if
fnend
def fn_caption_update
  setenv('Program_Caption',fnSystemName$(env$('cursys')))
  fncompany_name(0,screen_width)
fnend
def fn_main
  dim program_selection$*256,menu_option$*128
  do
    if env$('ExitNow')<>'yes' then
      fnTos(screen_name$:='Menu') : frameCount=0
      fnflexinit1('menu',program_grid_line,program_grid_col,grid_height,grid_width,mat headings$,mat column_mask$)
  !
      fn_caption_update
      fn_update_program_grid
      fn_display_buttons
      if ~enableFavorites then
        fnLbl(program_grid_line,1,'Filter:',info_col_width,1)
      end if
      fnreg_read('Last Save Date',last_save_date$)
      fnreg_read('Last Save File',last_save$)
      dim tmp_tooltip$*2048
      tmp_tooltip$='Last successful Save As was to the file\n'&last_save$&'\n on '&last_save_date$
      fnLbl(program_grid_line+2,1,last_save_date$(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
      fnLbl(program_grid_line+3,1,last_save$(pos(last_save$,'\',-1)+1:len(last_save$))(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
      fnLbl(program_grid_line+4,1,login_name$(1:info_col_width),info_col_width,2,0,0,0,'Login Name is "'&login_name$&'"')
      fnbutton_or_disabled(env$('enableClientSelection')=='Yes',program_grid_line+5,1,env$('client')(1:info_col_width),fkey_client:=5201, 'Client Name is "'&env$('client')&'"',info_col_width)
      ! end if
      if env$('Decimal_Assumed')<>'Decimal Required' then
        fnLbl(program_grid_line+6,1,env$('Decimal_Assumed'),info_col_width,2)
      end if
      if env$('ACSDeveloper')<>'' then
        fnLbl(program_grid_line+8,1,"ACS Developer"(1:info_col_width),info_col_width,2)
        fnLbl(program_grid_line+9,1,env$('ACSDeveloper')(1:info_col_width),info_col_width,2)
      end if
      if env$('BR_MODEL')='CLIENT/SERVER' then
        fnLbl(program_grid_line+11,1,'Client'(1:info_col_width),info_col_width,2)
      end if
      if env$('BR_XP_Mode')='True' then
        fnLbl(program_grid_line+13,1,'XP Compatibility'(1:info_col_width),info_col_width,2)
      end if
      tmp_tooltip$="ACS was last updated on "&last_update$&'\n to version '&version_current$&'.'
      fnLbl(screen_height-8,1,'ACS '&rtrm$(version_current$,'0'),info_col_width,2,0,0,0,tmp_tooltip$)
      if env$('acsProduct')<>'ACS Online' then 
        fnLbl(screen_height-7,1,last_update$(1:info_col_width),info_col_width,2,0,0,0,tmp_tooltip$)
      end if 
      fnLbl(screen_height-5,1,'BR! '&wbversion$,info_col_width,2)
      fn_dashboard_draw
      if enableFavorites then let fn_favoritesDraw
      fn_display_menu
      ! if env$('cursys')="PR" then
      !   fnchk(1,65,'Enable 2018 Federal Withholdings (for testing)', 1,fraDashboard) 
      !   if env$('taxYear')='2018' then resp$(2)='^' else resp$(2)='False'
      ! end if
  !
      setenv('tmp_acs_back_arrow','S:\Core\Icon\Red_X.png')
      fnreg_close ! allow backups to happen while this screen is open...  i think this will work - added 9/14/2017
      fnAcs(screen_name$,0,mat resp$,fkey_value,0,0,0)
      setenv('tmp_acs_back_arrow','')
      program_selection$=resp$(1)
      program_selection_id=val(program_selection$(2:pos(program_selection$,']')-1))
      program_selection$(1:pos(program_selection$,']'))=''
      curfld_value=curfld
      ! if env$('cursys')="PR" then
      !   if resp$(2)='True' or resp$(2)='^' then let setenv('taxYear','2018') else let setenv('taxYear','') 
      ! end if
    end if
  !
    if fkey_value=93 or fkey_value=99 or (fkey_value=98 and lwrc$(menu$)='exit') or env$('ExitNow')='yes' or menu$='Exit and Logout' then
  !  removed 1/21/2017 -  does not seem necessary - it does it when it is selected       fncursys$(env$('cursys'))
  !       fn_put_plus(env$('cursys'),mat program_file$,mat program_plus$)
      fnureg_write('FavoritesOpen',env$('FavoritesOpen'))
      goto XIT_MAIN
    else
      fn_session_reg_write(env$('cursys')&'.CurProg',str$(program_selection_id))
    end if
  !
  !  if fkey_value=1 then !
  ! help button on the bottom
  !
    if fkey_value=98 then ! r: drop down menu
      menu_option$=menu$
      if lwrc$(menu_option$(len(menu_option$)-3:len(menu_option$)))='.prc' then
        fnclear_menu
        execute 'proc '&menu_option$
      else if lwrc$(menu_option$(len(menu_option$)-2:len(menu_option$)))='.br' then
        menu_option$=srep$(menu_option$,'[cursys]',env$('cursys'))
        fnclear_menu
        fn_chain(menu_option$)
      else if lwrc$(menu_option$(len(menu_option$)-3:len(menu_option$)))='.cmd' then
        execute 'Sy -c "'&trim$(menu_option$)&'"'
      else if menu_option$(1:8)='Notepad:' then
        execute 'Sy -c c:\Windows\notepad.exe "'&trim$(menu_option$(9:inf))&'"'
      else if menu_option$(1:6)='FileIO' or menu_option$(1:8)='ScreenIO' then
        if menu_option$(1:8)='ScreenIO' then
          execute 'cd S:'
          library 'S:\Core\ScreenIO\screenio.br': fnDesignScreen
          fnDesignScreen
          chain 's:\core\start'
          ! fn_chain('S:\Core\ScreenIO\screenio.br')
        else if menu_option$(1:6)='FileIO' then
          if menu_option$='FileIO (update and launch)' then
            msgbox( 'obsoleted by local administrative execution of cmd:   mklink /J "C:\Users\John\OneDrive\ACS\Dev-5 Data\Core\FileIO\Layout" "C:\ACS\Dev-5\Core\FileIO\Layout" ')
            ! fnCopy('S:\Core\FileIO\Layout\*.*'        ,env$('QBase')&'\Core\FileIO\Layout\*.*'        )
            ! fnCopy('S:\Core\FileIO\Layout\version\*.*',env$('QBase')&'\Core\FileIO\Layout\version\*.*')
          end if
          open  #hProc:=fngethandle: 'name='&env$('temp')&'\fileioproc'&session$&'.$$$,replace',display,output
          pr #hProc: 'load S:\Core\FileIO\fileio.br'
          pr #hProc: 'cd S:'
          pr #hProc: 'Run'
          pr #hProc: 'cd C:'
          pr #hProc: 'load S:\Core\Menu.br'
          pr #hProc: 'run'
          close #hProc:
          execute 'proc '&env$('temp')&'\fileioproc'&session$&'.$$$'
        else
          pr 'unrecognized syntax: '&menu_option$ : pause
        end if
        !
      else if lwrc$(menu_option$(1:4))='http' then
        execute 'Sy -M -c start '&menu_option$
      else if lwrc$(menu_option$(1:5))='file:' then
        menu_option$(1:5)=''
        if menu_option$='Open' then
          fnOpenPartial
        else if menu_option$='Save All Data As' then
          fnclear_menu
          fnFileSaveAs('*.*')
        else if menu_option$='Save Company As' then
          fnclear_menu
          fnFileSaveAs('*h[cno]')
        end if
      else if lwrc$(ltrm$(menu_option$)(1:20))='editinwordprocessor:' then
        fn_callEditInWordProcessor(menu_option$)
      else if lwrc$(ltrm$(menu_option$)(1:11))='hamsterfio:' then
        fn_callHamsterFio(menu_option$)
      else if menu_option$(1:5)='fnFM(' then
        fnFM(menu_option$(6:len(menu_option$)-1))
      else if menu_option$(1:14)='fnPrintAceTest' then
        fnPrintAceTest(menu_option$(16:len(menu_option$)-1))
      else if menu_option$='Index Company' then
        fnindex_sys(val(env$('cno')))
      else if menu_option$='Restart' then
        fnClearLayoutCache
        setenv('ForceScreenIOUpdate','yes')
        open #h_tmp:=fngethandle: 'Name='&env$('temp')&'\acs_Restart_'&session$&'.prc,replace',display,output
        pr #h_tmp: "Stop"
        pr #h_tmp: "clear resident"
        pr #h_tmp: "chain 'S:\Core\Start'"
        close #h_tmp:
        execute 'proc '&env$('temp')&'\acs_Restart_'&session$&'.prc'
      else if menu_option$='Index System' then
        fnindex_sys
      else if lwrc$(menu_option$(1:8))='[cursys=' then
        cursys$=menu_option$(9:10)
        fncursys$(cursys$)
        fn_setup_on_cursys_change
        fn_checkFileVersionIfNecessary
        fn_update_program_grid
        fn_caption_update
      else if lwrc$(menu_option$(len(menu_option$):len(menu_option$)))='\' then ! it is a Folder - just open it
        menu_option$=srep$(menu_option$,'%report_cache_folder_current%',fnreport_cache_folder_current$)
        execute 'sy -c -w explorer "'&os_filename$(menu_option$(1:len(menu_option$)-1))&'"'
      else
        pr 'menu_option$=';menu_option$
      end if
    !  else if fkey_value=67 then
    !      /r  fkey=98
    else if fkey_value=3 then
      fnclear_menu
      fnprogram_properties(trim$(program_name$(program_selection_id))) ! (program_selection$) ! 1 is the flag to chain back to main menu
      chain program$
    else if fkey_value=209 then
      curfld(curfld,fkey) : pr 'fkey 209 encountered (combobox)'
    else
      ! r: system specific fkey button actions
      fkey_favorite_add=1450
      fkey_favorite_open=1451
      fkey_favorite_close=1452
      fkey_favorite_del=1455
      fkey_favorite_program_base=1460
      if enableFavorites and fkey_value=>fkey_favorite_add and fkey_value<=fkey_favorite_add+50 then
        if fkey_value=fkey_favorite_add then
          ! pr 'program_selection$="'&program_selection$&'"'
          if trim$(program_selection$)='' then
            msgbox('Category Headers may not be added to Favorites.')
          else
            faProgramWhich=srch(mat program_file$,trim$(program_selection$))
            if faProgramWhich<=0 then
              msgbox('Could not find "'&trim$(program_selection$)&'" to add it to the favorites.')
              ! pause
            else
              fnFavoriteAdd(program_name_trim$(faProgramWhich))
            end if
          end if
          chain program$
        else if fkey_value=fkey_favorite_open then
          setenv('FavoritesOpen','True')
          chain program$
        else if fkey_value=fkey_favorite_close then
          setenv('FavoritesOpen','False')
          chain program$
        else if fkey_value=fkey_favorite_del then
          favoriteDeleteMode$='True'
        else if fkey_value>fkey_favorite_program_base then
          ! pr trim$(favorite$(fkey_value-fkey_favorite_program_base))
          if favoriteDeleteMode$='True' then ! delete it from the list
            fnFavoriteDel(favorite$(fkey_value-fkey_favorite_program_base))
            favoriteDeleteMode$='False'
          else ! launch it
            which=srch(mat program_name_trim$,trim$(favorite$(fkey_value-fkey_favorite_program_base)))
            if which<=0 then
              msgbox('Could not find "'&trim$(favorite$(fkey_value-fkey_favorite_program_base))&'" on your current menu.')
            else
              if lwrc$(ltrm$(program_file$(which))(1:11))='hamsterfio:' then
                fn_callHamsterFio(program_file$(which))
              else
                fnchain('S:\'&program_file$(which))
              end if
            end if
          end if
        else if env$('acsDeveloper')<>'' then
          pr 'fkey_value=';fkey_value
          pause
        end if
!
      else if env$('cursys')='GL' and fkey_value<>0 then
        if fnclient_has('G2') and fkey_value=fkey_g2_employee then
            fnchain('S:\General Ledger\Accountants\Employee')
        else if fkey_value=fkey_gl_Transactions  then
          fnchain('S:\General Ledger\Enter Transactions')
        else if fkey_value=fkey_gl_accounts  then
          fnchain('S:\General Ledger\Accounts')
        else if fkey_value=fkey_gl_periodEndingDate  then
          fnchain('S:\General Ledger\Period Ending Dates')
        end if
      else if env$('cursys')='CL' and fkey_value<>0 then
        if fkey_value=fkey_cl_unpaid_invoice  then
          fnchain('S:\Checkbook\Unpaid Invoice')
        else if fkey_value=fkey_cl_print_checks  then
          fnchain('S:\Checkbook\Print Checks')
        else if fkey_value=fkey_cl_payee  then
          fnchain('S:\Checkbook\Payee')
        end if
      else if env$('cursys')='PR' and fkey_value<>0 then
        if fkey_value=fkey_pr_employee  then
          fnchain('S:\Payroll\Employee')
        else if fkey_value=fkey_pr_enter_time  then
          fnchain('S:\Payroll\Enter Time Sheets')
        else if fkey_value=fkey_pr_payroll_registers  then
          fnchain('S:\acsPR\newprReg1')
        else if fkey_value=fkey_pr_print_checks  then
          fnchain('S:\Payroll\Print Payroll Checks')
        end if
      else if env$('cursys')='UB' and fkey_value<>0 then
        if fkey_value=fkey_ub_collection then
          fnchain('S:\Utility Billing\Collections')
        else if fkey_value=fkey_ub_customer then
          fnchain('S:\Utility Billing\Customer')
        else if fkey_value=fkey_change_billing_date then
          fnchain('S:\acsub\Company')
        end if
      else if env$('cursys')='TM' and fkey_value<>0 then
        if fkey_value=fkey_tm_collections then
          fnchain('S:\acsTM\arinput')
        end if
      end if
      ! /r
      if fkey_client<>0 and fkey_value=fkey_client then
          fnClientSelect
          fnchain(program$)
      else if (curfld_value=1 and fkey_value=201) or fkey_value=4 then
        ! if program_plus$(program_selection_id)='+' then
        !   program_plus$(program_selection_id)='-'
        ! else if program_plus$(program_selection_id)='-' then
        !   program_plus$(program_selection_id)='+'
        if lwrc$(ltrm$(program_selection$)(1:11))='hamsterfio:' then  ! else if
          fn_callHamsterFio(program_selection$)
        else if lwrc$(ltrm$(program_selection$)(1:20))='editinwordprocessor:' then  ! else if
          fn_callEditInWordProcessor(program_selection$)
        else if program_selection$<>'' then
          fn_chain('S:\'&trim$(program_selection$))
        end if
      end if
    end if
!
  loop
  XIT_MAIN: !
fnend
def fn_callHamsterFio(tmpCap$*128)
  tmpCap$=trim$(tmpCap$)
  tmpCap$=tmpCap$(12:len(tmpCap$))
  if tmpCap$(1:3)=uprc$(tmpCap$(1:2))&' ' then !  if the first two letters are uppercase and the third is a space then assume it includes it's own system code
    tmpCursys$=tmpCap$(1:2)
    tmpCap$(1:3)=''
  else
    tmpCursys$=env$('cursys')
  end if
  fntop('S:\'&fnSystemName$&'\'&tmpCap$&'.br',tmpCap$)
  fnHamsterFio(tmpCursys$&' '&tmpCap$)
  fnxit
fnend
def fn_callEditInWordProcessor(programSelection$*256)
  programSelection$=trim$(programSelection$)
  programSelection$(1:20)=''
  dim fileToEditInWp$*256,options$*256
  if (cewPosSpace1:=pos(programSelection$,' '))>0 then 
    fileToEditInWp$=programSelection$(1:cewPosSpace1-1)
    options$=programSelection$(cewPosSpace1+1:inf)
  else
    fileToEditInWp$=programSelection$
  end if
  fileToEditInWp$=srep$(fileToEditInWp$,'[Q]',env$('Q'))
  fileToEditInWp$=srep$(fileToEditInWp$,'[cno]',env$('cno'))
  fileToEditInWp$=srep$(fileToEditInWp$,'[CNo]',env$('cno'))
  if pos(options$&' ','makeIfNecessary ')>0 then cewMakeIfNecessary=1 else cewMakeIfNecessary=0
  if pos(options$&' ','forceAtlantis ')>0 then cewForceAtlantis=1 else cewForceAtlantis=0
  if ~cewForceAtlantis and cewMakeIfNecessary then
    pr 'can only MakeIfNEcessary if also forceAtlantis.  Please consider enhancing the code.' : pause
  end if
  if cewForceAtlantis then cewForce$='atlantis' else cewForce$='wordprocessor'
  !
  fn_callEditInWordProcessor=fnEditFile(cewForce$,fileToEditInWp$)
fnend
def fn_dashboard_height
  if env$('cursys')="OE" then
    dhReturn=1
  else if env$('cursys')="CL" then
    dhReturn=1
  else if env$('cursys')="PR" then
    dhReturn=1
  else if env$('cursys')="GL" then
    if fnclient_has('G2') then
      dhReturn=2
    else
      dhReturn=1
    end if
  else if env$('cursys')="UB" then
    dhReturn=1
  else if env$('cursys')="TM" then
    dhReturn=3
  else
    dhReturn=0
  end if
  !
  if enableFavorites and dhReturn<2 then
    dhReturn=2
  end if
  !
  if env$('ACSDeveloper')<>'' then dhReturn=5
  fn_dashboard_height=dhReturn
fnend
def fn_ddAddButton(buttonText$,btnFkey,btnItem,tmp_btn_width; buttonLine,tooltip$*150) ! buttons are added and counted (btnItem) from right to left
  if buttonLine=0 then buttonLine=1
  if btnItem=1 then
    fnButton(buttonLine,dashboard_width-tmp_btn_width,buttonText$,btnFkey,tooltip$,1,tmp_btn_width,fraDashboard)
  else if btnItem>1 then
    fnButton(buttonLine,dashboard_width-(tmp_btn_width*btnItem+(btnItem-1)),buttonText$,btnFkey,tooltip$,1,tmp_btn_width,fraDashboard)
  else
    pr 'btnItem=';btnItem;' and it is currently required by fn_ddAddButton'
  end if
fnend
def fn_favoritesDraw
  if env$('FavoritesOpen')='True' then
    ! fnFra(program_grid_line,favorite_left,grid_height-1,favorite_width,'Favorites')
    fnFra(dashboard_height+3,favorite_left,favorite_height,favorite_width,'Favorites') :  : frameCount+=1 : fraFavorites=frameCount
    dim favorite$(0)*128
    fnFavoriteList(mat favorite$) 
    fnButton(1,1,'Close',fkey_favorite_close:=1452,'Close Favorites',0,6,fraFavorites)
    fnbutton_or_disabled(favoriteDeleteMode$<>'True',1,15,'Delete',fkey_favorite_del:=1455,'To remove a favorite, click this "Delete" button and then click the favorite.',6,fraFavorites)
    fnbutton_or_disabled(1,1,favorite_width-6,'Add',fkey_favorite_add:=1450,'To add a favorite, highlite a menu option and click this "add" button.',6,fraFavorites)
    if favoriteDeleteMode$='True' then
      fnLbl(2,1,'Select Favorite to Delete',favorite_width,2,0,fraFavorites)
    end if
    for favItem=1 to min(udim(mat favorite$),favorite_height-2)
      fkey_favorite_program_base:=1460
      fnButton(favItem+2,1,favorite$(favItem),fkey_favorite_program_base+favItem,'',1,favorite_width-1,fraFavorites)
    nex favItem
  end if
fnend
def fn_dashboard_draw
  if dashboard_height>0 then
    dashboard_width=screen_width-4
    fnFra(1,1,dashboard_height,dashboard_width,'Dashboard') : frameCount+=1 : fraDashboard=frameCount
    if enableFavorites then
      fnbutton_or_disabled(env$('FavoritesOpen')<>'True',2,favorite_left,'Favorites',fkey_favorite_open:=1451,'',20,fraDashboard)
    end if
    if env$('cursys')="CL" then
      tmp_btn_width=14 : tmpBtnItem=0
      fn_ddAddButton('Unpaid Invoice',fkey_cl_unpaid_invoice:=5001,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Payee',fkey_cl_payee:=5003,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Print Checks',fkey_cl_print_checks:=5002,tmpBtnItem+=1,tmp_btn_width)
    else if env$('cursys')="PR" then
      fnLbl(1,1,'Payroll State:',15,1,0,fraDashboard)
      fnLbl(1,17,fnpayroll_client_state$,4,0,0,fraDashboard)
      tmp_btn_width=10 : tmpBtnItem=0
      fn_ddAddButton('Checks',fkey_pr_print_checks:=5004,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Registers',fkey_pr_payroll_registers:=5003,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Enter Time',fkey_pr_enter_time:=5002,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Employee',fkey_pr_employee:=5001,tmpBtnItem+=1,tmp_btn_width)
    else if env$('cursys')="GL" then
      library 'S:\Core\Library': fnpedat$
      open #h_tmp:=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative ioerr DD_GL_XIT
      read #h_tmp,using 'Form Pos 296,n 2',rec=1: lmu
      close #h_tmp:
      fnLbl(1,1,'Last Period Closed:',19,1,0,fraDashboard)
      fnLbl(1,21,str$(lmu),4,0,0,fraDashboard)
      fnLbl(1,26,'Pay Ending Date:',16,1,0,fraDashboard)
      dim pedat$*20
      pedat$=fnpedat$
      if pedat$='' then pedat$='(not set)'
      fnButton(1,44,pedat$,fkey_gl_periodEndingDate:=5003,'',1,20,fraDashboard) ! fnLbl(1,47,fnpedat$,4,0,0,1)
    DD_GL_XIT: !
      tmp_btn_width=10 : tmpBtnItem=0
      fn_ddAddButton('Accounts',fkey_gl_accounts:=5001,tmpBtnItem+=1,tmp_btn_width,1,'General Ledger Master')
      fn_ddAddButton('Transactions',fkey_gl_Transactions:=5002,tmpBtnItem+=1,tmp_btn_width,1,'Enter Transactions')
      if fnClient_has('G2') then
        tmpBtnItem=0
        fn_ddAddButton('Employee',fkey_g2_employee:=5011,tmpBtnItem+=1,tmp_btn_width, 2)
      end if
    else if env$('cursys')="UB" then
      fnLastBillingDate(d1) : d1$=date$(days(d1,'mmddyy'),'mm/dd/ccyy')
    !           fnLbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon)
      fnLbl(1,1,'Last Billing Date:',18,1,0,1)
      fnLbl(1,20,d1$,4,0,0,1)
      if env$("ACSDeveloper")<>"" then
        fkey_change_billing_date=5001
        fnButton(1,32,'Change',fkey_change_billing_date,'Select a new current Billing Date',1,6,fraDashboard)
      end if
      tmp_btn_width=11 : tmpBtnItem=0
      fn_ddAddButton('Collections',fkey_ub_collection:=5002,tmpBtnItem+=1,tmp_btn_width)
      fn_ddAddButton('Customer',fkey_ub_customer:=5003,tmpBtnItem+=1,tmp_btn_width)
      if ub_total_ar_on_dashboard$='True' then
        fnLbl(1,40,'Total Accounts Receivable:',26,1,0,1)
        fnLbl(1,68,str$(fntotal_ar),4,0,0,1)
      end if
    else if env$('cursys')="TM" then
      tmp_btn_width=11 : tmpBtnItem=0
      fn_ddAddButton('Collections',fkey_tm_collections:=5001,tmpBtnItem+=1,tmp_btn_width)
      fnLbl(1,1,'Time Management is for Advanced Computer Services LLC only.',0,0,0,fraDashboard)
    end if
    ! if env$('acsDeveloper')<>'' then
    !   fnLbl(3,1,'---Developer---',0,0,0,fraDashboard)
    !   fnLbl(4,1,'QBase: "'&env$('QBase')&'"',(dashboard_width/2)-1,0,0,fraDashboard)
    !   fnLbl(4,int(dashboard_width/2)+1,'env$(Q): "[Q]"',int(dashboard_width/2)-1,0,0,fraDashboard)
    !   fnLbl(5,1,' Data: "'&env$('Data')&'"',0,0,0,fraDashboard)
    ! end if
  end if
fnend
def fn_display_buttons
  fnCmdKey('OK' ,4,1,0,'Press "OK" to launch the selected program')
  fnCmdKey('Help' ,1,0,0,'Press "Help" to launch the help page about this program')
  fnCmdKey('Properties',3,0,0,'Press "Properties" to view the properties this program')
  fnCmdKey('Exit',99,0,1,'Press "Exit" to quit')
fnend  ! fn_display_buttons
def fn_session_reg_read(ls_field_name$*128,&ls_field_value$)
  fn_session_reg_read=fnreg_read(session$&'.'&ls_field_name$,ls_field_value$)
fnend
def fn_session_reg_write(ls_field_name$*128,ls_field_value$*256)
  fn_session_reg_write=fnreg_write(session$&'.'&ls_field_name$,ls_field_value$)
fnend
def fn_get_system_abbr_list(mat system_abbr_list$)
  dim system_name$(0)*40
  mat system_name$(0)
  mat system_abbr_list$(0)

  ! if env$('ACSDeveloper')<>'' and exists('S:\Time Management\Menu.mnu') then
  !   fnAddOneC(mat system_abbr_list$,'TM')
  !   fnAddOneC(mat system_name$,fnSystemName$('TM'))
  ! end if
  fn_add_if_licensed('TM')
  fn_add_if_licensed('OE')
  fn_add_if_licensed('CL')
  fn_add_if_licensed('GL')
  fn_add_if_licensed('PR')
  fn_add_if_licensed('UB')
fnend
def fn_add_if_licensed(sysCode$)
  if (fnclient_has(sysCode$) or env$('acsDeveloper')<>'') and exists('S:\'&fnSystemName$(sysCode$)&'\Menu.mnu') then
    fnAddOneC(mat system_abbr_list$,sysCode$)
    fnAddOneC(mat system_name$,fnSystemName$(sysCode$))
  end if
fnend
def library fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
  if ~setup then let fn_setup
  fnGetProgramList=fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
fnend
def fn_getProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
  mat program_plus$(0) : mat program_name$(0) : mat program_name_trim$(0) : mat program_file$(0) : mat ss_text$(0)
  glpa_program_count=0
  fn_getProgramList_add('S:\'&fnSystemName$(env$('cursys'))&'\Menu.mnu')
  if env$("ACSDeveloper")<>"" then
    fn_getProgramList_add('S:\'&env$('CurSystem')&'\Programmer.mnu')
  end if  ! serial=env$('ACSDeveloper')<>''
fnend
def fn_getProgramList_add(gpla_file$*256;___,sign$)
  dim ss_category$(1)*80
  dim program_item$(1)*512
  !
  open #1: 'Name='&gpla_file$,display,input ioerr GPLA_XIT
  linput #1: temp$ eof GPLA_EOF ! just consume first line
  do
    linput #1: temp$ eof GPLA_EOF
    if trim$(temp$)<>'' and trim$(temp$)(1:1)<>'!' then
      str2mat(temp$,mat program_item$,'^')
      if udim(mat program_item$)>=3 then
        requirment$=trim$(program_item$(3))
      else
        requirment$=''
      end if
      if requirment$='' or fnclient_has(requirment$) then
        glpa_program_count+=1
        program_item_count=udim(mat program_item$)
        mat program_plus$(glpa_program_count)
        mat program_name$(glpa_program_count)
        mat program_name_trim$(glpa_program_count)
        mat program_file$(glpa_program_count)
        mat ss_text$(glpa_program_count)
        mat program_level(glpa_program_count)
        program_level(glpa_program_count)=fn_program_level(program_item$(1))
        !
        program_name$(glpa_program_count)=srep$(rtrm$(program_item$(1)),'>','         ')
        program_name_trim$(glpa_program_count)=trim$(program_name$(glpa_program_count))
        !
        if program_item_count>1 then program_file$(glpa_program_count)=trim$(program_item$(2))
        !
        if trim$(program_file$(glpa_program_count))='' then
          program_plus$(glpa_program_count)='**' ! fn_get_one_plus$(h_plus,env$('cursys'),program_file$(glpa_program_count))
        end if
        !
        ss_text$(glpa_program_count)='' ! ss_text$(glpa_program_count)&cnvrt$('Pic(#####)',glpa_program_count)
        gt_count=len(program_item$(1)(1:10))-len(srep$(program_item$(1)(1:10),'>',''))
        !
        for gt_item=1 to 10
          if gt_count=gt_item-1 then mat ss_category$(gt_item) : ss_category$(gt_item)=program_name$(glpa_program_count)
        next gt_item
        for ss_cat_item=1 to udim(mat ss_category$)
          ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&' - '&ltrm$(ss_category$(ss_cat_item))
        next ss_cat_item
        ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&ltrm$(program_plus$(glpa_program_count))
        !
        if program_item_count>1 then ss_text$(glpa_program_count)=ss_text$(glpa_program_count)&' ~ '&program_item$(2)
      end if
    end if
  loop
  GPLA_EOF: !
  close #1: ioerr ignore
  GPLA_XIT: !
fnend
def fn_program_level(tmp$*512) !
  chr_pos=0
  do
    chr_pos+=1
  loop while tmp$(chr_pos:chr_pos)='>'
  fn_program_level=chr_pos
fnend
def fn_update_program_grid
  col_return=1
  col_plus=2
  col_name=3
  col_file=4
  col_ss_text=5
  dim program_grid_row$(5)*255
  dim program_selection_id$*256
  setenv('current_grid_row',str$(1))
  program_selection_id=0
  fn_session_reg_read(env$('cursys')&'.CurProg',program_selection_id$) : program_selection_id=val(program_selection_id$) conv ignore
  if udim(mat program_name$) then
    hide_level=0
    hiding=0
    for upg_item=1 to udim(mat program_name$)
      if hiding and program_level(upg_item)=hide_level and program_plus$(upg_item)='+' then
        fn_upg_show_it(upg_item)
      else if hiding and program_level(upg_item)<=hide_level then
        hiding=0
        pr '  turning off hide at '&program_name$(upg_item)
        hide_level=0
      else if ~hiding and program_plus$(upg_item)='+' then
        hide_level=program_level(upg_item)
        hiding=1
        pr '--' : pr '  turning ON hide  after '&program_name$(upg_item)
        fn_upg_show_it(upg_item)
      end if
      if hiding then
      else if ~hiding then
        fn_upg_show_it(upg_item)
      end if
    next upg_item
  end if
fnend
def fn_upg_show_it(upg_item)
  program_grid_row$(col_return)='['&str$(upg_item)&']'&program_file$(upg_item)
  program_grid_row$(col_plus)=program_plus$(upg_item)
  program_grid_row$(col_name)=program_name$(upg_item)
  program_grid_row$(col_file)=program_file$(upg_item)
  if program_selection_id=upg_item then
    setenv('current_grid_row',str$(upg_item))
  end if
  program_grid_row$(col_ss_text)=ss_text$(upg_item)
  fnflexadd1(mat program_grid_row$)
fnend
def fn_display_menu
  if ~dm_setup then
    dm_setup=1
    dim m_a$(1)*256,m_b$(1)*256,m_c$(1)*256
    mat m_a$(0) : mat m_b$(0) : mat m_c$(0)
    dim menu_option$*255
    x=5000
    fn_dm_add('&File',str$(x+=1))
    fn_dm_add(' &Open...','File:Open')
    ! if enableOpenPartial$='True' then
    !   fn_dm_add(' Open Partial...','File:Open Partial')
    ! end if
    fn_dm_add(' &Save As...','File:Save All Data As')
    if enableSaveCompanyAs$='True' then
      fn_dm_add(' Save Company As...','File:Save Company As')
    end if
    fn_dm_add(' -')
    fn_dm_add(' Preferences','S:\Core\Programs\Preferences.br')
    ! if report_cache$='True' then
    fn_dm_add(' Open &Report Cache','%report_cache_folder_current%\')
    ! end if
    fn_dm_add(' -')
    fn_dm_add(' E&xit'&chr$(9)&'Alt+F4','Exit')
    if env$('BR_MODEL')='CLIENT/SERVER' then
      fn_dm_add(' E&xit and Logout','Exit and Logout')
    end if
    if udim(mat system_name$)>1 then
      fn_dm_add('&System',str$(x+=1))
      for system_abbr_list_item=1 to udim(mat system_abbr_list$)
        if system_abbr_list$(system_abbr_list_item)<>'P2' and system_abbr_list$(system_abbr_list_item)<>'G2' then
          fn_dm_add(' '&system_name$(system_abbr_list_item),'[cursys='&system_abbr_list$(system_abbr_list_item)&']')
        end if
      next system_abbr_list_item
    end if
    fn_dm_add('&Company',str$(x+=1))
    fn_dm_add(' &Select','S:\Core\Programs\Select Company.br')
    if exists('S:\'&fnSystemName$&'\Company.br') then
      fn_dm_add(' Configure','S:\'&fnSystemName$&'\Company.br')
    else
      fn_dm_add(' Configure','S:\acs[cursys]\Company.br')
    end if
    if fnclient_is_converting then
      fn_dm_add(' -')
      fn_dm_add(' Import','S:\Core\Company Import.br')
    end if
    fn_dm_add('&Utilities')
    fn_dm_add(' Grids','S:\Core\PrtFlex\PrtFlex1.br')
    fn_dm_add(' City State Zip','HamsterFio:CO City State Zip')
    fn_dm_add(' -')
    ! fn_dm_add(' Registry')
    ! fn_dm_add('  Company','S:\Core\CReg_Hamster.br')
    ! fn_dm_add('  Standard and User','S:\Core\Reg_Hamster.br')
    ! fn_dm_add('  System','HamsterFio:CO System Registry')
    !  fn_dm_add(' PDF Test','S:\Core\PDF_Test.br')
    fn_dm_add(' PrintAce Test - &PDF','fnPrintAceTest(PDF)')
    fn_dm_add(' PrintAce Test - Print&Ace','fnPrintAceTest(PrintAce)')
    !  fn_dm_add(' PrintAce &Test','S:\Core\Programs\PrintAce_Test.br')
    fn_dm_add(' Recreate &Indexes')
    fn_dm_add('  &Current '&env$('cursys')&' Company','Index Company')
    fn_dm_add('  &All '&env$('cursys')&' Companies','Index System')
    fn_dm_add(' &Check File Versions','S:\Core\Check File Versions.br')
    !  fn_dm_add(' PrintAce')
    !  fn_dm_add('  -')
    fn_dm_add(' PrintAce Install &Dependencies','S:\Core\Programs\PrintAce_Setup.br')
    if env$('BR_MODEL')<>'CLIENT/SERVER' then
      fn_dm_add(' Client Server','S:\Core\Client_Server.br')
    end if
    fn_dm_add(' Restart','Restart')
    fn_dm_add('&Help')
    fn_dm_add(' Help','http://planetacs.net/help/')
    fn_dm_add(' ACS Website','http://planetacs.net')
    if env$('BR_MODEL')<>'CLIENT/SERVER' then
      fn_dm_add(' Update','S:\Core\Programs\Update.br')
    end if
    !  fn_dm_add(' -')
    !  fn_dm_add(' Install Host for Live Support','http://get.teamviewer.com/acsllc') ! 'S:\Core\ACS_Support.exe')
    fn_dm_add(' -')
    fn_dm_add(' About','S:\Core\Programs\About.br')
    if env$('ACSDeveloper')<>'' then
      fn_dm_add('De&veloper')
      if env$('acsEnableComplier')='Yes' then
        fn_dm_add(' Re&compile','S:\Core\Start.br') : setenv('compile_without_asking','Yes') ! Recompile changed source
        fn_dm_add(' Retry last compile proc','C:\ACS\Dev-5\(import)\compile.prc') : setenv('compile_without_asking','Yes') ! Recompile changed source
      end if
      fn_dm_add(' Release Notes','Notepad: C:\ACS\Dev-5\Core\Release_Notes.txt')
      fn_dm_add(' Increase Build Version','C:\ACS\Util\Build Update.cmd')
      fn_dm_add(' Refresh N++ Function CallTips','S:\Dev\Notepad++ CallTip Refresh.br')
      fn_dm_add(' -')
      fn_dm_add(' Client','S:\Core\Client.br')
      fn_dm_add(' Registry')
      fn_dm_add('  Company','S:\Core\CReg_Hamster.br')
      fn_dm_add('  Standard and User','S:\Core\Reg_Hamster.br')
      fn_dm_add('  System','HamsterFio:CO System Registry')
      fn_dm_add(' -')
      fn_dm_add(' FileIO','FileIO')
      !   fn_dm_add(' FileIO (update and launch)','FileIO (update and launch)')
      fn_dm_add(' ScreenIO','ScreenIO')
      fn_dm_add(' -')
      fn_dm_add(' Locate 1','S:\Core\Locate.br')
    end if
    if env$('ACSDeveloper')<>'' and exists('[Q]\tmmstr\Company.h420') then ! trim$(env$("ACSDeveloper"))<>""
      fn_dm_add('ACS LLC')
      fn_dm_add(' Client 420','S:\acsTM\Client.br')
      fn_dm_add(' Support 420','S:\acsTM\Support.br')
    end if
  end if  ! ~dm_setup
  fndisplay_menu(mat m_a$,mat m_b$,mat m_c$)
fnend
def fn_dm_add(a$*256; b$*256,c$*1)
  mat m_a$(udim(mat m_a$)+1) : m_a$(udim(mat m_a$))=a$
  mat m_b$(udim(mat m_b$)+1) : m_b$(udim(mat m_b$))=b$
  if c$='' then c$='E'
  mat m_c$(udim(mat m_c$)+1) : m_c$(udim(mat m_c$))=c$
fnend
def fn_chain(c_program$*128)
  pr newpage
  fnchain(c_program$)
fnend
XIT: execute "System"
IGNORE: continue
include: ertn