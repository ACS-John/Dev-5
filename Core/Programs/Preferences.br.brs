10000 ! Replace S:\Core\Programs\Preferences
10020 ! maintain ACS Core system settings
12000   if ~setup then let fn_setup
12040   fntop(program$, cap$="Preferences")
13000   win_height=20
14000 ! r: read all the setting here (unless read and set on the fly)
14010   fnreg_read('Enable Save Company As',enableSaveCompanyAs$, 'False')
14012   ! fnreg_read('Enable Open Partial',enableOpenPartial$, 'False')
14014   fnreg_read('Enable Backup Report Cache',enableBackupReportCache$, 'False')
14020   ! fnreg_read('Report_Cache',report_cache$, 'False')
14030   fnreg_read('PrintAce.Max Pages',pa_max_pages$)
14031   fnreg_read('formsFormat',formsFormat$, pdf_or_printace$(1))
14032 ! fnureg_read('CS Client Report Cache',client_report_cache$) : if client_report_cache$='' then client_report_cache$=report_cache$
14040   fnureg_read('Background_Picture',background_picture$,background_picture_default$)
14042   fnureg_read('Min_FontSize_Height',min_fontsize_height$,default_min_fontsize_height$)
14044   fnureg_read('Min_FontSize_Width',min_fontsize_width$,default_min_fontsize_width$)
14060   fn_get_atlantis(atlantis_exe$) : atlantis_exe$=trim$(atlantis_exe$,'"')
14070   fnureg_read('Word Path',word_exe$,atlantis_exe$)
14072   fnureg_read('Default to Use Word',use_word$,'True')
14074   fnureg_read('Default to Use Atlantis',use_atlantis$,'False')
14080   fnureg_read('Text_Editor',text_editor$,fn_text_editor_default$)
14120   background_picture$=os_filename$(background_picture$)
14140   fnureg_read('Decimal',decimal_assumed$,'False')
14160   fnureg_read('Disable_MultiSession',disable_multisession$) : if disable_multisession$<>'True' then disable_multisession$='False'
14180   ! fnureg_read('wait_wp_close',wait_wp_close$) : if wait_wp_close$<>'False' then wait_wp_close$='True'
14200   save_path$=fn_save_as_path$
14300   if save_path$(1:2)='@:' then save_path$(1:2)=''
15000 ! 
15020   dim receipt_printer$*256
15040   fnureg_read('Printer.Receipt',receipt_printer$)
15060   if fnclient_has('U4') then
15080     dim hhto$*256
15100     fnureg_read('Hand Held To File',hhto$)
15120     dim hhfro$*256
15140     fnureg_read('Hand Held From File',hhfro$)
15142     dim u4_deviceOption$(0)*20,u4_device$*20
15144     fnHandHeldList(mat u4_deviceOption$)
15145     fnaddonec(mat u4_deviceOption$,'[Ask]')
15146     fnreg_read('Hand Held Device',u4_device$, fnhand_held_device$)
15148     fnreg_read('Meter Address Enable',u4_meterAddress$, 'False')
15160   end if
15180   if fnclient_has('UB') then
15190     fnreg_read('Collections pr in Account Order',ub_collPrintInAccountOrder$,'False')
15200     fnreg_read('Collections Disable Deposit List',ub_collDisableDepositList$,'False')
15210     fnureg_read('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$,'False')
15220   end if
15240   if fnclient_has('GL') then
15260     fnreg_read('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$,'False')
15280   end if
15990 ! /r
30000 !  main loops - build and display screens, get, save, apply settings, etc
32000 DO_SCREEN_MAIN: ! r:
32020   do 
32040     fnTos(sn$="Settings_Main")
32060     fn_nav_buttons
32080     col1_width=33 : col2_pos=col1_width+2 : lc=rc=0 : win_width=75
32100     fnLbl(lc+=1,1,"** System Settings **",win_width,2)
32120     lc+=1
32140     fnLbl(lc+=1,1,"ACS Client Name:",col1_width,1)
32160     fnTxt(lc,col2_pos,54,0,0,'',1,'If this is not correct contact ACS Support') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
32180     resp$(rc+=1)=env$('Client')
32200 ! 
32220     fnLbl(lc+=1,1,"BR Serial:",col1_width,1)
32240     fnTxt(lc,col2_pos,54,0,0,'',1,'Serial Number assigned by Business Rules Corp license') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
32260     resp$(rc+=1)=str$(serial)
32280 ! 
32300     fnLbl(lc+=1,1,"Data Folder:",col1_width,1)
32320     fnTxt(lc,col2_pos,40,255,0,"",1)
32340     resp$(rc+=1)=os_filename$(env$('Q')&'\')
32360     fnButton(lc,col2_pos+42+5,'Open',15) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32380     lc+=1
32400     fnChk(lc+=1,col2_pos,'Include Report Cache in Save As',1)
32420     resp$(resp_enableBackupReportCache:=rc+=1)=enableBackupReportCache$
32440     fnChk(lc+=1,col2_pos,'Enable Save Company As',1)
32460     resp$(resp_enableSaveCompanyAs:=rc+=1)=enableSaveCompanyAs$
32480     fnLbl(lc   ,col2_pos+3,'Saves Only the Current Company Number related files across all ACS systems')
32500     fnLbl(lc+=1,col2_pos+3,'Does not save settings, reports.')
32520     ! fnChk(lc+=1,col2_pos,'Enable Open Partial',1)
32540     ! resp$(resp_enableOpenPartial:=rc+=1)=enableOpenPartial$
32560     fnLbl(lc   ,col2_pos+3,'Extracts a single company for a single system from a Save')
32580     fnLbl(lc+=1,col2_pos+3,'Does not restore report cache, global nor system settings.')
32600     lc+=1
32620     fnLbl(lc+=1,1,"** User Settings **",win_width,2)
32640     lc+=1
32660 ! 
32680     fnLbl(lc+=1,1,"Text Editor Executable:",col1_width,1)
32700     fnTxt(lc,col2_pos,42,80,0,'70',0,'Select an executable for editing text files.')
32720     resp$(resp_text_editor:=rc+=1)=text_editor$
32740     fnButton(lc,col2_pos+42+5,'Default',14) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32760 ! 
32780     lc+=1
32800 ! 
32820     fnLbl(lc+=1,1,"Save (Company) As Folder Default:",col1_width,1)
32840     fnTxt(lc,col2_pos,42,80,0,'',0,'')
32860     resp$(resp_save_path:=rc+=1)=save_path$
32880     fnButton(lc,col2_pos+42+5,'Default',16) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32900 ! 
32920     lc+=1
32940 ! 
32960     fnChk(lc+=1,col2_pos,'Assume decimal place',1)
32980     fnLbl(lc ,col2_pos+3,'if checked    examples:  1234= 12.34   80= 0.80  5.=5.00  4=0.04')
33000     fnLbl(lc+1,col2_pos+3,'if unchecked  examples:  1234=1234.00  80=80.00  5.=5.00  4=4.00')
33020     resp$(resp_decimal_assumed:=rc+=1)=decimal_assumed$
33040     lc+=2
33060 ! 
33080     fnChk(lc+=1,col2_pos,'Disable multiple sessions',1)
33100     fnLbl(lc,col2_pos+3,'If checked only allow one session at a time will be allowed.')
33120     resp$(resp_disable_multisession:=rc+=1)=disable_multisession$
33140     lc+=1
33160 ! 
33180     fnCmdKey("&Save",1,1)
33200     fnCmdKey("Apply",2,0)
33220     fnCmdKey("&Cancel",5,0,1)
33240     fnAcs(sn$,0,mat resp$,ck)
33260     if ck=5 then 
33280       goto XIT
33300     else 
33320       if text_editor$<>'' then text_editor$=os_filename$(env$('at')&resp$(resp_text_editor))
33340       decimal_assumed$=resp$(resp_decimal_assumed)
33360       disable_multisession$=resp$(resp_disable_multisession)
33380       save_path$=resp$(resp_save_path)
33400       enableSaveCompanyAs$=resp$(resp_enableSaveCompanyAs)
33420       ! enableOpenPartial$=resp$(resp_enableOpenPartial)
33440       enableBackupReportCache$=resp$(resp_enableBackupReportCache)
33460     end if 
33480 ! 
33500 ! 
33520     if ck=>screen_ck_low and ck<=screen_ck_high then 
33540       goto SCREEN_CK_GOTO
33560     else if ck=15 then 
33580       execute 'sy -c -w explorer "'&os_filename$(env$('Q')&'\')&'"'
33600     else if ck=16 then 
33620       fnureg_write('Save As Path',os_filename$(env$('userprofile')&'\Desktop'))
33640     else if ck=14 then 
33660       fnureg_write('Text_Editor','')
33680       text_editor$=fn_text_editor_default$
33700     else ! Save and Apply
33720       fn_save
33740       if ck<>2 then goto XIT
33760     end if 
33780   loop  ! /r
38000 DO_SCREEN_THEME: ! r:
38020   do 
38040     fnTos(sn$="Settings_Theme")
38060     fn_nav_buttons
38080     col1_width=19 : col2_pos=col1_width+2 : col3_pos=col2_pos+12 : lc=0 : win_width=75
38100     fnLbl(lc+=1,1,"** User Settings **",win_width,2)
38120     lc+=1
38140     fnLbl(lc+=1,1,"Background Picture:",col1_width,1)
38160     fnTxt(lc,col2_pos,42,256,0,'70',0,'Select any picture to be used as your ACS background image.')
38180     resp$(resp_background_picture:=1)=os_filename$(env$('at')&background_picture$)
38200     fnButton(lc,col2_pos+42+5,'Default',11) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
38203     lc+=1
38205     fnLbl(lc+=1,1,"Minimum Font Size:",col1_width,1)
38207     fnLbl(lc,col2_pos+3,"x",1,2)
38209     fnTxt(lc,col2_pos,2,0,0,'30',0,'Height')
38210     if min_fontsize_height$='' or trim$(min_fontsize_height$)='0' then min_fontsize_height$=default_min_fontsize_height$
38211     resp$(resp_min_fontsize_height:=2)=min_fontsize_height$
38213     fnTxt(lc,col2_pos+5,2,0,0,'30',0,'Width')
38214     if min_fontsize_width$='' or trim$(min_fontsize_width$)='0' then min_fontsize_width$=default_min_fontsize_width$
38215     resp$(resp_min_fontsize_width:=3)=min_fontsize_width$
38217     fnButton(lc,col2_pos+9,'Default',12) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
38219 ! Min_FontSize 14x6
38220     lc+=1
38240     fnLbl(lc+=1,1,"*** Colors ***",win_width,2) : rc_color=rc_color_zero=3
38260     fnLbl(lc+=1,col2_pos,"Foreground",10,2)
38280     fnLbl(lc,col3_pos,"Background",10,2)
38300 ! 
38320     fn_do_screen_theme_add_theme('Screen','#000000','#E7EDF5')
38340     fnLbl(lc,col3_pos+12,'Base for all settings, including buttons that are not cancel nor default')
38360     fn_do_screen_theme_add_theme('ScreenHeader','#000000','#FFFFFF')
38380     fn_do_screen_theme_add_theme('TextBoxes','#000000','#FFFFFF')
38400     fn_do_screen_theme_add_theme('Labels','#000000','#B0C4DE')
38420     fn_do_screen_theme_add_theme('Buttons','#000000','#74DF00')
38440     fnLbl(lc,col3_pos+12,'Default Button')
38460     fn_do_screen_theme_add_theme('ButtonCancel','#000000','#CD5C5C')
38480     fnLbl(lc,col3_pos+12,'Cancel Button')
38500     fn_do_screen_theme_add_theme('GridHeaders','#000000','#FFFFFF')
38520 ! 
38540 ! 
38560     fnCmdKey("&Save",1,1)
38580     fnCmdKey("Apply",2,0)
38600     fnCmdKey("&Cancel",5,0,1)
38620     fnAcs(sn$,0,mat resp$,ck)
40000     if ck=5 then 
40020       goto XIT
40040     else 
40060       background_picture$=resp$(resp_background_picture)
40070       min_fontsize_height$=resp$(resp_min_fontsize_height) : if min_fontsize_height$='' then min_fontsize_height$=default_min_fontsize_height$
40072       min_fontsize_width$=resp$(resp_min_fontsize_width) : if min_fontsize_width$='' then min_fontsize_width$=default_min_fontsize_width$
40080       rc_color=rc_color_zero
40100       fnureg_write('color.[screen].foreground',resp$(rc_color+=1))
40120       fnureg_write('color.[screen].background',resp$(rc_color+=1))
40140       fnureg_write('color.[screenheader].foreground',resp$(rc_color+=1))
40160       fnureg_write('color.[screenheader].background',resp$(rc_color+=1))
40180       fnureg_write('color.[textboxes].foreground',resp$(rc_color+=1))
40200       fnureg_write('color.[textboxes].background',resp$(rc_color+=1))
40220       fnureg_write('color.[labels].foreground',resp$(rc_color+=1))
40240       fnureg_write('color.[labels].background',resp$(rc_color+=1))
40260       fnureg_write('color.[buttons].foreground',resp$(rc_color+=1))
40280       fnureg_write('color.[buttons].background',resp$(rc_color+=1))
40300       fnureg_write('color.[buttoncancel].foreground',resp$(rc_color+=1))
40320       fnureg_write('color.[buttoncancel].background',resp$(rc_color+=1))
40340       fnureg_write('color.[gridheaders].foreground',resp$(rc_color+=1))
40360       fnureg_write('color.[gridheaders].background',resp$(rc_color+=1))
40380     end if 
40400 ! 
40420 ! 
40430     if ck=>screen_ck_low and ck<=screen_ck_high then 
40440       goto SCREEN_CK_GOTO
40490     else if ck=11 then 
40500       background_picture$=background_picture_default$
40502     else if ck=12 then 
40504       min_fontsize_height$=default_min_fontsize_height$
40506       min_fontsize_width$=default_min_fontsize_width$
40520     else ! Save and Apply
40540       fn_save
40560       if ck<>2 then goto XIT
40580     end if 
40600   loop  ! /r
41000 def fn_do_screen_theme_add_theme(attribute$,foreground_default$,background_default$)
41020   lc+=1
41040   fnLbl(lc+=1,1,attribute$&":",col1_width,1)
41060   fnTxt(lc,col2_pos,10,7,0,'',0,attribute$&' Foreground: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
41080   fnureg_read('color.['&lwrc$(attribute$)&'].foreground',resp$(rc_color+=1)) : if resp$(rc_color)='' then resp$(rc_color)=foreground_default$
41100   fnTxt(lc,col3_pos,10,7,0,'',0,attribute$&' Background: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
41120   fnureg_read('color.['&lwrc$(attribute$)&'].background',resp$(rc_color+=1)) : if resp$(rc_color)='' then resp$(rc_color)=background_default$
41140 fnend 
50000 DO_SCREEN_PRINTER: ! r:
50020   do 
50040     fnTos(sn$="Settings_Printer")
50060     fn_nav_buttons
50080     col1_width=33 : col2_pos=col1_width+2 : lc=0 : win_width=75 : dsp_rc=0
50100     fnLbl(lc+=1,1,"** System Settings **",win_width,2)
50120     lc+=1
50140     ! fnChk(lc+=1,5,"Enable Report Cache",1) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
50160     ! resp$(resp_report_cache:=dsp_rc+=1)=report_cache$
50180     fnTxt(lc,35,40,256,0,'',1,'') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
50200     resp$(dsp_rc+=1)=os_filename$(env$('Q')&'\Report Cache')
50210     fnButton(lc,30,'Open',12) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50220     lc+=1
50230     fnLbl(lc+=1,1,"PrintAce Max Pages:",col1_width,1)
50240     fnTxt(lc,col2_pos,3,3,0,'30',0,'Use to break up large PrintAce type pr jobs into smaller batches. 0 disables feature.')
50250     resp$(resp_pa_max_pages:=dsp_rc+=1)=pa_max_pages$
50251     lc+=1
50252     fnLbl(lc+=1,1,"Forms Format:",col1_width,1)
50254     fncomboa('formsFormat',lc,col2_pos,mat pdf_or_printace$,'Select a format for special forms',10) ! 42,80,0,'70',0,'Select a printer to be used when printing receipts.')
50256     resp$(resp_formsFormat:=dsp_rc+=1)=formsFormat$
50260     lc+=1
50270     fnLbl(lc+=1,1,"** User Settings **",win_width,2)
50280 !   lc+=1
50290 !   fnLbl(lc+=1,1,"Client's path to Report Cache:",col1_width,1)
50300 !   fnTxt(lc,col2_pos,40,256,0,'',0,'client path to '&os_filename$(env$('Q')&'\Report Cache')&'\nOnly necessary if using Client/Server.') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
50310 !   resp$(resp_client_report_cache:=dsp_rc+=1)=client_report_cache$
50320     lc+=1
50330     fnLbl(lc+=1,1,"Word Executable:",col1_width,1,0,0,0,'Or default word processor')
50340     fnTxt(lc,col2_pos,42,80,0,'70',0,'Select an executable for Word or your default Word Processor to display and pr your RTF reports.')
50350     resp$(resp_word:=dsp_rc+=1)=word_exe$
50360     fnButton(lc,col2_pos+42+5,'Default',15) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50365     fnOpt(lc,col2_pos+42+5+7+2,"Use Word as Default") : resp$(resp_use_word:=dsp_rc+=1)=use_word$
50370     fnLbl(lc+=1,1,"Atlantis Executable:",col1_width,1)
50380     fnTxt(lc,col2_pos,42,80,0,'70',0,'Select the executable for your Atlantis Word Processor.')
50390     resp$(resp_atlantis:=dsp_rc+=1)=atlantis_exe$ ! os_filename$(atlantis_exe$)
50400     fnButton(lc,col2_pos+42+5,'Default',13) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50402     fnOpt(lc,col2_pos+42+5+7+2,"Use Atlantis as Default") : resp$(resp_use_atlantis:=dsp_rc+=1)=use_atlantis$
50410 ! 
50420     ! lc+=1
50430     ! fnChk(lc+=1,55,"Wait for word processor to close before continuing",1)
50450     ! resp$(resp_wait_wp_close:=dsp_rc+=1)=wait_wp_close$
50460     lc+=1
50470 ! 
50480     fnLbl(lc+=1,1,"Receipt Printer:",col1_width,1)
50490     fncomboa('printer',lc,col2_pos,mat printer_list$,'Select a printer to be used to pr receipts.',42) ! 42,80,0,'70',0,'Select a printer to be used when printing receipts.')
50500     resp$(resp_receipt_printer:=dsp_rc+=1)=receipt_printer$
50510     fnButton(lc,col2_pos+42+5,'Test',14)
50520     fnCmdKey("&Save",1,1)
50530     fnCmdKey("Apply",2,0)
50540     fnCmdKey("&Cancel",5,0,1)
50550     fnAcs(sn$,0,mat resp$,ck)
50560     if ck=5 then 
50570       goto XIT
50580     else 
50600       ! report_cache$=resp$(resp_report_cache)
50610       ! wait_wp_close$=resp$(resp_wait_wp_close)
50612       pa_max_pages$=resp$(resp_pa_max_pages)
50620       atlantis_exe$=resp$(resp_atlantis)
50622       word_exe$=resp$(resp_word)
50624       use_word$=resp$(resp_use_word)
50626       use_atlantis$=resp$(resp_use_atlantis)
50630       receipt_printer$=resp$(resp_receipt_printer)
50632       formsFormat$=resp$(resp_formsFormat)
50640     end if 
50660 ! 
50680 ! 
50700     if ck=>screen_ck_low and ck<=screen_ck_high then 
50720       goto SCREEN_CK_GOTO
50803     else if ck=14 then 
50804       fnureg_write('Printer.Receipt',receipt_printer$)
50806       fnopen_receipt_printer
50808       pr #255: 'ACS Receipt Printer Test'
50810       pr #255: ''
50812       pr #255: ''
50814       pr #255: ''
50816       fnclose_receipt_printer
50820     else if ck=13 then 
50840       fnureg_write('Atlantis Path','')
50860       fn_get_atlantis(atlantis_exe$) : atlantis_exe$=trim$(atlantis_exe$,'"')
50862     else if ck=15 then 
50864       dim office_word_exe$*256
50866       if fn_get_office_word(office_word_exe$) then 
50868         word_exe$=office_word_exe$
50870       end if 
50880     else if ck=12 then 
50900       execute 'sy -c -w explorer "'&os_filename$(env$('Q')&'\Report Cache')&'"'
50920     else ! Save and Apply
50940       fn_save
50960       if ck<>2 then goto XIT
50980     end if 
51000   loop  ! /r
52000 DO_SCREEN_HH: ! r:
52020 do
52040   fnTos(sn$="Settings_HH")
52060   fn_nav_buttons
52080   col1_width=25 : col2_pos=col1_width+2 : lc=0 : win_width=75 : dsh_rc=0
52100   fnLbl(lc+=1,1,"** System Settings **",win_width,2)
52120   lc+=1 ! fnLbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
52140   fnLbl(lc+=1,1,"Device Type:",col1_width,1,0,0,0,'Select device type - leave blank to reset to default')
52160   fncomboa('u4Device',lc,col2_pos,mat u4_deviceOption$,'Select device type - leave blank to reset to default')
52180   resp$(resp_u4_device:=dsh_rc+=1)=u4_device$
52200   lc+=1
52220   fnChk(lc+=1,col2_pos,'Enable Meter Location table',1)
52240 ! fnLbl(lc,col2_pos+3,'(extra explanation)')
52260   resp$(resp_u4_meterAddress:=dsh_rc+=1)=u4_meterAddress$
52280   lc+=1
52300   fnLbl(lc+=1,1,"** User Settings **",win_width,2)
52320   lc+=1
52340   fnLbl(lc+=1,1,"File to Create:",col1_width,1,0,0,0,'Select a path and file for ACS to create output files for hand helds.')
52360   fnTxt(lc,col2_pos,42,80,0,'70',0,'Select a path and file for ACS to create output files for hand helds.')
52380   resp$(resp_hhto:=dsh_rc+=1)=hhto$
52400   fnLbl(lc+=1,1,"File to Retrieve:",col1_width,1,0,0,0,'Select a path and file for ACS to read input files from hand helds.')
52420   fnTxt(lc,col2_pos,42,80,0,'70',0,'Select a path and file for ACS to read input files from hand helds.')
52440   resp$(resp_hhfro:=dsh_rc+=1)=hhfro$ 
52450   fnLbl(lc,col2_pos+45,"(set to [Ask] to ask every time)")
52460   fnCmdKey("&Save",1,1)
52480   fnCmdKey("Apply",2,0)
52500   fnCmdKey("&Cancel",5,0,1)
52520   fnAcs(sn$,0,mat resp$,ck)
52540   if ck=5 then 
52560     goto XIT
52580   else 
52600     u4_device$=resp$(resp_u4_device)
52620     u4_meterAddress$=resp$(resp_u4_meterAddress)
52640     hhto$=resp$(resp_hhto)
52660     hhfro$=resp$(resp_hhfro)
52680   end if 
52700   if ck=>screen_ck_low and ck<=screen_ck_high then 
52720     goto SCREEN_CK_GOTO
52740   else ! Save and Apply
52760     fn_save
52780     if ck<>2 then goto XIT
52800   end if 
52820 loop ! /r
54000 DO_SCREEN_UB: ! r:
54020 do
54040   fnTos(sn$="Settings_UB")
54060   fn_nav_buttons
54080   col1_width=46 : col2_pos=col1_width+2 : lc=0 : win_width=75 : ub_rc=0
54090   fnLbl(lc+=1,1,"** System Settings **",win_width,2)
54092   lc+=1
54094   fnLbl(lc+=1,1,'Collections',win_width,2)
54100   fnChk(lc+=1,col2_pos,'Print in Account Number order',1)
54110   fnLbl(lc,col2_pos+3,'Instead of order entered')
54120   resp$(resp_ub_cpiao:=ub_rc+=1)=ub_collPrintInAccountOrder$
54122   fnChk(lc+=1,col2_pos,'Disable Deposit Listing',1)
54124   fnLbl(lc,col2_pos+3,'Receipt Listing only')
54126   resp$(resp_ub_collDisableDepositList:=ub_rc+=1)=ub_collDisableDepositList$
54130   lc+=1
54140   fnLbl(lc+=1,1,"** User Settings **",win_width,2)
54150   lc+=1
54152   fnChk(lc+=1,col2_pos,'Display Total Accounts Receivable on Dashboard',1)
54160   fnLbl(lc,col2_pos+3,'May increase load time.')
54170   resp$(resp_ub_total_ar:=ub_rc+=1)=ub_total_ar_on_dashboard$
54180   !
54360   fnCmdKey("&Save",1,1)
54380   fnCmdKey("Apply",2,0)
54400   fnCmdKey("&Cancel",5,0,1)
54420   fnAcs(sn$,0,mat resp$,ck)
54440   if ck=5 then 
54460     goto XIT
54480   else 
54500     ub_total_ar_on_dashboard$=resp$(resp_ub_total_ar)
54510     ub_collPrintInAccountOrder$=resp$(resp_ub_cpiao)
54520     ub_collDisableDepositList$=resp$(resp_ub_collDisableDepositList)
54560   end if 
54580   if ck=>screen_ck_low and ck<=screen_ck_high then 
54600     goto SCREEN_CK_GOTO
54620   else ! Save and Apply
54640     fn_save
54660     if ck<>2 then goto XIT
54680   end if
54700 loop ! /r
56000 DO_SCREEN_GL: ! r:
56020 do
56040   fnTos(sn$="Settings_GL")
56060   fn_nav_buttons
56080   col1_width=46 : col2_pos=col1_width+2 : lc=0 : win_width=75 : gl_rc=0
56090   fnLbl(lc+=1,1,"** System Settings **",win_width,2)
56092   lc+=1
56100   fnChk(lc+=1,col2_pos,'Enter Transactions - retain some fields between additions',1)
56120   resp$(resp_gl_retainFieldsDuringAdd:=gl_rc+=1)=gl_retainFieldsDuringAdd$
56130   lc+=1
56140   ! fnLbl(lc+=1,1,"** User Settings **",win_width,2)
56150   ! lc+=1
56180   !
56360   fnCmdKey("&Save",1,1)
56380   fnCmdKey("Apply",2,0)
56400   fnCmdKey("&Cancel",5,0,1)
56420   fnAcs(sn$,0,mat resp$,ck)
56440   if ck=5 then 
56460     goto XIT
56480   else 
56500     gl_retainFieldsDuringAdd$=resp$(resp_gl_retainFieldsDuringAdd)
56560   end if 
56580   if ck=>screen_ck_low and ck<=screen_ck_high then 
56600     goto SCREEN_CK_GOTO
56620   else ! Save and Apply
56640     fn_save
56660     if ck<>2 then goto XIT
56680   end if
56700 loop ! /r
64000 SCREEN_CK_GOTO: ! r:
64020   if ck=1001 then 
64040     screen=screen_main : goto DO_SCREEN_MAIN
64060   else if ck=1002 then 
64080     screen=screen_theme : goto DO_SCREEN_THEME
64100   else if ck=1003 then 
64120     screen=screen_print : goto DO_SCREEN_PRINTER
64130   else if ck=1004 then 
64132     screen=screen_hh : goto DO_SCREEN_HH
64140   else if ck=1005 then 
64150     screen=screen_ub : goto DO_SCREEN_UB
64172   else if ck=1006 then 
64174     screen=screen_gl : goto DO_SCREEN_GL
64220   else
64240     pr 'SCREEN_CK_GOTO does not know how to handle ck='&str$(ck)&'.'
64260     pause
64280   end if 
64300 ! /r
66000 def fn_save
66010   fnreg_write('Enable Save Company As',enableSaveCompanyAs$)
66012   ! fnreg_write('Enable Open Partial',enableOpenPartial$)
66014   fnreg_write('Enable Backup Report Cache',enableBackupReportCache$)
66020   ! fnreg_write('Report_Cache',report_cache$)
66040   fnreg_write('PrintAce.Max Pages',pa_max_pages$)
66050   fnreg_write('formsFormat',formsFormat$)
66080   fnureg_write('Background_Picture',br_filename$(background_picture$))
66100   fnureg_write('Min_FontSize_Height',min_fontsize_height$)
66120   fnureg_write('Min_FontSize_Width',min_fontsize_width$)
66140   fn_apply_theme
66160   ! fnureg_write('wait_wp_close',wait_wp_close$)
66180   fnureg_write('Atlantis Path',atlantis_exe$)
66200   fnureg_write('Word Path',word_exe$)
66202   fnureg_write('Default to Use Word',use_word$)
66204   fnureg_write('Default to Use Atlantis',use_atlantis$)
66220   fnureg_write('Text_Editor',text_editor$)
66240   fnureg_write('Decimal',decimal_assumed$)
66260   fnureg_write('Disable_MultiSession',disable_multisession$)
66280   fnureg_write('Save As Path',save_path$)
66300   fnureg_write('Printer.Receipt',receipt_printer$)
66320   if fnclient_has('U4') then
66330     fnreg_write('Hand Held Device',u4_device$)
66332     fnreg_write('Meter Address Enable',u4_meterAddress$)
66340     fnureg_write('Hand Held To File',hhto$)
66360     fnureg_write('Hand Held From File',hhfro$)
66380   end if
66400   if fnclient_has('UB') then
66410     fnreg_write('Collections pr in Account Order',ub_collPrintInAccountOrder$)
66412     fnreg_write('Collections Disable Deposit List',ub_collDisableDepositList$)
66420     fnureg_write('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$)
66440   end if
66460   if fnclient_has('GL') then
66480     fnreg_write('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$)
66500   end if
66990 fnend 
68000 def fn_nav_buttons
68020   if ~setup_nav_buttons then 
68040     setup_nav_buttons=1
68060     screen_main=1
68080     screen_theme=2
68100     screen_print=3
68110     screen_hh=4
68120     screen_ub=5
68130     screen_gl=6
68160     screen_ck_low=1001
68180     screen_ck_high=1006
68200   end if 
68220   if screen=0 then screen=screen_main
68240   nb_lc=0 : nb_pos=110 : nb_len=15
68260   fnLbl(win_height,nb_len,'') ! forces all the windows for each screen to be at least the hight specified by win_height (set toward the top of this program)
68280   fnbutton_or_disabled(screen<>screen_main,nb_lc+=1,nb_pos,'Main',1001, '',nb_len)
68300   fnbutton_or_disabled(screen<>screen_theme,nb_lc+=1,nb_pos,'Theme',1002, '',nb_len)
68320   fnbutton_or_disabled(screen<>screen_print,nb_lc+=1,nb_pos,'Printer',1003, '',nb_len)
68340   nb_lc+=1
68342   if fnclient_has('GL') then
68344     fnbutton_or_disabled(screen<>screen_gl,nb_lc+=1,nb_pos,'General Ledger',1006, '',nb_len)
68346   end if
68360   if fnclient_has('UB') then
68380     fnbutton_or_disabled(screen<>screen_ub,nb_lc+=1,nb_pos,'Utility Billing',1005, '',nb_len)
68400   end if
68420   if fnclient_has('U4') then
68440     fnbutton_or_disabled(screen<>screen_hh,nb_lc+=1,nb_pos,'(UB) Hand Held',1004, '',nb_len)
68460   end if
68480   fnLbl(22,1,'')
68999 fnend 
70000 def fn_setup
70020   if ~setup then 
70040     setup=1
70060     library 'S:\Core\Library': fntop,fnxit, fnAcs,fnLbl,fnTxt ,fnerror,fnTos,fnChk,fnreg_read,fnreg_write,fnButton,fnCmdKey,fnureg_read,fnureg_write,fncomboa,fnbutton_or_disabled,fnopen_receipt_printer,fnclose_receipt_printer,fnclient_has,fnMsExe$
70080     library 'S:\Core\Library': fnHandHeldList,fnhand_held_device$,fnOpt,fnGetPp,fncopyfile
70100     library 'S:\Core\Library': fnWaitForShellCloseStart,fnWaitForShellCloseEnd,fnmakesurepathexists
70110     library 'S:\Core\Library': fnaddonec
70120     on error goto ERTN
70140     dim resp$(20)*256,cap$*128,background_picture$*256,atlantis_exe$*80,word_exe$*256,save_path$*256
70160     dim text_editor$*256
70180     default_min_fontsize_height$='14'
70200     default_min_fontsize_width$='6'
70220     dim background_picture_default$*256
70240     background_picture_default$=os_filename$('S:\Core\wallpaper\301H.jpg')
70260     !
70280     dim printer_list$(1)*256
70300     printer_list(mat printer_list$) ! printer_count=printer_list(mat printer_list$)
70320     !
70340     dim pdf_or_printace$(2)*12
70360     pdf_or_printace$(1)='PrintAce'
70380     pdf_or_printace$(2)='PDF'
70400     !
70420     if env$('BR_MODEL')='CLIENT/SERVER' then clientServer=1 else clientServer=0
70440     !
70460   end if 
70990 fnend 
72000 IGNORE: continue 
74000 ! <Updateable Region: ERTN>
74040 ERTN: fnerror(program$,err,line,act$,"xit")
74060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
74080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
74100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
74120 ERTN_EXEC_ACT: execute act$ : goto ERTN
74140 ! /region
74160 XIT: fnxit
76000 def library fnapply_theme(; disableConScreenOpenDflt)
76020   if ~setup then let fn_setup
76040   fnapply_theme=fn_apply_theme( disableConScreenOpenDflt)
76060 fnend 
78000 def fn_apply_theme(; disableConScreenOpenDflt)
78020   dim background_picture$*256
78040   fnureg_read('Background_Picture',background_picture$)
78042   fnureg_read('Min_FontSize_Height',min_fontsize_height$)
78044   fnureg_read('Min_FontSize_Width',min_fontsize_width$)
78046   execute 'config Min_FontSize '&min_fontsize_height$&'x'&min_fontsize_width$ error ERR_MIN_FONTSIZE
78060   if background_picture$='' or ~exists(background_picture$) then background_picture$=background_picture_default$
78080   setenv('background_picture',background_picture$)
78100   fn_set_color('[screen]','#000000','#E7EDF5')
78120   fn_set_color('[screenheader]','#000000','#FFFFFF')
78140   fn_set_color('[textboxes]','#000000','#FFFFFF')
78160   fn_set_color('[gridheaders]','#000000','#FFFFFF')
78180   fn_set_color('[labels]','#000000','#B0C4DE')
78200   fn_set_color('[buttons]','#000000','#74DF00') ! '#F0F8FF')
78220   fn_set_color('[buttoncancel]','#000000','#CD5C5C')
78230   if ~disableConScreenOpenDflt then
78240     execute 'Config Screen OpenDflt "Rows=35, Cols=115, Picture='&env$('background_picture')&',border=S:[screen],N=[screen]"'
78250   end if
78260 fnend 
78270 ERR_MIN_FONTSIZE: ! r:
78280   execute 'config Min_FontSize '&default_min_fontsize_height$&'x'&default_min_fontsize_width$ ! pr 'ERR_MIN_FONTSIZE' : pause
78290 continue ! /r
78320 def fn_set_color(attribute$,foreground_default$,background_default$)
78340   fnureg_read('color.'&attribute$&'.foreground',foreground$) : if foreground$='' then foreground$=foreground_default$
78360   fnureg_read('color.'&attribute$&'.background',background$) : if background$='' then background$=background_default$
78380   execute 'Config Attribute '&attribute$&' /'&foreground$&':'&background$ error ignore ! pr 'config attribute '&attribute$&' /'&foreground$&':'&background$ : pause
78400 fnend
80000 def library fnEditFile(efEditorType$,fileToEdit$*256)
80020   if ~setup then let fn_setup
80040   fnEditFile=fn_editFile(efEditorType$,fileToEdit$)
80060 fnend
82000 def fn_editFile(efEditorType$,efFileToEdit$*256)
82020   ! efEditorType$ - wordprocessor, atlantis, word or text
82040   efWaitText$=''
82060   efForce$=''
82080   efSwitches$=''
82100   if lwrc$(efEditorType$)=lwrc$('atlantis') then
82120     efEditorType$='wordprocessor'
82140     efForce$='atlantis'
82160     efWaitText$='Atlantis'
82180   else if lwrc$(efEditorType$)=lwrc$('word') then
82190     efEditorType$='wordprocessor'
82200     efForce$='word'
82220     efWaitText$='Microsoft Word'
82240   end if
82260   ! r: get the executable and set any switches
82280   if lwrc$(efEditorType$)=lwrc$('wordprocessor') then
82300     dim efExe$*256
82320     fn_get_wordprocessor_exe(efExe$, efForce$)
82340     if efWaitText$='' then efWaitText$='Word Processor'
82360     if pos(lwrc$(efExe$),'atlantis.exe')>0 or pos(lwrc$(efExe$),'awp.exe')>0 then
82380       efSwitches$=' -n'
82400     end if
82412   else if lwrc$(efEditorType$)=lwrc$('text') then
82414     fnureg_read('Text_Editor',efExe$,fn_text_editor_default$)
82416     efWaitText$='Text Editor'
82420   else 
82430     pr 'unrecognized Editor Type: '&efEditorType$ : pause
82480   end if
82500   ! /r
82520   ! r: determine efEditOnClientCopyOfFile$
82540   dim efEditOnClientCopyOfFile$*256
82560   if clientServer then
82580     dim efFilePath$*256,efFileName$*128,efFileExt$*128
82600     fnGetPp(efFileToEdit$,efFilePath$,efFileName$,efFileExt$)
82620     efEditOnClientCopyOfFile$=env$('at')&'C:\ProgramData\ACS\Temp\Session'&session$&'\'&efFileName$&efFileExt$
82640     fnmakesurepathexists(efEditOnClientCopyOfFile$)
82650     if exists(efFileToEdit$) then
82660       fncopyfile(efFileToEdit$,efEditOnClientCopyOfFile$)
82670     end if
82680   else
82700     efEditOnClientCopyOfFile$=efFileToEdit$
82720   end if
82740   ! /r
82760   fnWaitForShellCloseStart(efWaitText$)
82780   exe 'Sy -w -@ '&efExe$&' "'&os_filename$(efEditOnClientCopyOfFile$)&'"'&efSwitches$
82800   fnWaitForShellCloseEnd
82820   if clientServer then
82840     ! sleep(.2)  <maybe this would be a good idea if copies start to error or fail.  it'd give the wp a little longer to complete save...  but in a perfect world it isn't necessary
82860     fncopyfile(efEditOnClientCopyOfFile$,efFileToEdit$)
82880   end if
82900 fnend
84000 def fn_text_editor_default$*256
84080   dim atlantis_path$*256,text_editor$*256
84100   if exists(env$('at')&":C:\Windows\Notepad.exe") then 
84120     text_editor$='C:\Windows\Notepad.exe'
84140   else if exists(env$('at')&":C:\Program Files (x86)\Atlantis\Atlantis.exe") then 
84160     text_editor$='C:\Program Files (x86)\Atlantis\Atlantis.exe'
84180   else if exists(env$('at')&":C:\Program Files\Atlantis\Atlantis.exe") then 
84200     text_editor$='C:\Program Files\Atlantis\Atlantis.exe'
84220   else 
84240     text_editor$=os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')
84260   end if 
84280   ! if ~exists(br_filename$(trim$(text_editor$,'"'))) then 
84300   !   text_editor$='"'&os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')&'"'
84320   ! end if 
84340   text_editor$=trim$(text_editor$,'"')
84350   fn_text_editor_default$=text_editor$
84360 fnend 
85002 def library fnget_wordprocessor_exe(&wordprocessor_exe$; force$)
85004   if ~setup then let fn_setup
85006   fnget_wordprocessor_exe=fn_get_wordprocessor_exe(wordprocessor_exe$, force$)
85008 fnend
85030 def fn_get_wordprocessor_exe(&wordprocessor_exe$; force$)
85040   ! fnureg_read('Default to Use Word',use_word$,'True')  ! it's the default
85060   if force$='' then
85080     fnureg_read('Default to Use Atlantis',use_atlantis$,'False')
85100   end if
85120   if lwrc$(force$)=lwrc$('atlantis') or trim$(use_atlantis$)='True' then 
85140     fn_get_atlantis(wordprocessor_exe$)
85160   else 
85180     fnureg_read('Word Path',wordprocessor_exe$)
85200     if trim$(wordprocessor_exe$)='' then 
85220       if fn_get_office_word(wordprocessor_exe$) then
85240         fnureg_write('Word Path',wordprocessor_exe$)
85260       else
85280         msgbox("Microsoft Word Executable could not be automaticaly detected.  It must be set manually in File > Preferences > Printer")
85300       end if !
85320     end if
85340   end if
85360 fnend 
86100 def fn_get_atlantis(&atlantis_exe$)
86120   dim atlantis_path$*256
86140   fnureg_read('Atlantis Path',atlantis_path$)
86160   if trim$(atlantis_path$)<>'' then 
86180     if ~exists(env$('at')&atlantis_path$) and exists(env$('at')&srep$(lwrc$(atlantis_path$),'atlantis.exe','awp.exe')) then
86200       atlantis_path$=srep$(lwrc$(atlantis_path$),'atlantis.exe','awp.exe')
86220     end if
86240     if ~exists(env$('at')&atlantis_path$) and exists(env$('at')&srep$(lwrc$(atlantis_path$),'awp.exe','atlantis.exe')) then
86260       atlantis_path$=srep$(lwrc$(atlantis_path$),'awp.exe','atlantis.exe')
86280     end if
86300     atlantis_exe$=atlantis_path$
86320   else 
86340     dim atl_check_path$(5)*256
86360     atl_check_path$(1)="C:\Program Files (x86)\Atlantis\Atlantis.exe"
86380     atl_check_path$(2)="C:\Program Files\Atlantis\Atlantis.exe"
86400     !     atl_check_path$(3)=''&os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')
86420     atl_check_path$(3)="C:\Program Files (x86)\Atlantis\awp.exe"
86440     atl_check_path$(4)="C:\Program Files\Atlantis\awp.exe"
86460     atl_check_path$(5)=env$('local_program_dir')&('\Core\Atlantis Nova\Atlantis.exe')
86480     if atl_check_path$(5)(1:2)='@:' then atl_check_path$(5)(1:2)=''
86500     atl_which=fn_first_exists_in_list(mat atl_check_path$)
86520     if atl_which>0 then 
86540       atlantis_exe$=atl_check_path$(atl_which)
86560       fnureg_write('Atlantis Path',atlantis_exe$)
86580     else 
86600       atlantis_exe$='' ! atl_check_path$(3)
86620     end if 
86640   end if 
86660 fnend 
87000 def fn_get_office_word(&office_word_exe$)
87020   gow_which=0
87040   office_word_exe$=fnMsExe$("winword.exe")
87060   if office_word_exe$<>'Key not found' and exists(env$('at')&office_word_exe$) then
87080     gow_which=1
87100   end if
87320   fn_get_office_word=gow_which
87340 fnend 
88000 def fn_first_exists_in_list(mat feil_list$)
88020   feil_return=0
88040   feil_item=0
88060   feil_count=udim(mat feil_list$)
88080   do until feil_return or feil_item=>feil_count
88100     feil_item+=1
88120     dim feil_tmp_test$*1024
88140     feil_tmp_test$=feil_list$(feil_item)
88160     if feil_tmp_test$(1:2)<>'@:' then feil_tmp_test$(0:0)='@:'
88180     if exists(feil_tmp_test$) then 
88200       feil_return=feil_item
88220     end if 
88240   loop 
88260   fn_first_exists_in_list=feil_return
88280 fnend 
89000 def library fndecimal_assumed
89020   if ~setup then let fn_setup
89040   fndecimal_assumed=fn_decimal_assumed
89060 fnend 
90000 def fn_decimal_assumed
90020   fnureg_read('Decimal',tmp$,'False')
90040   if tmp$='False' and env$('decimal_assumed')<>'Decimal Required' then 
90060     execute 'Config Decimal Required'
90080     setenv('decimal_assumed','Decimal Required')
90100   else if tmp$='True' and env$('decimal_assumed')<>'Decimal Assumed' then
90120     execute 'Config Decimal Assumed'
90140     setenv('decimal_assumed','Decimal Assumed')
90160   end if 
90180 fnend 
92000 def library fnsave_as_path$*256
92020   if ~setup then let fn_setup
92040   fnsave_as_path$=fn_save_as_path$
92060 fnend 
94000 def fn_save_as_path$*256
94020   dim sap_return$*256
94040   fnureg_read('Save As Path',sap_return$)
94060   if sap_return$(1:2)='@:' then sap_return$(1:2)=''
94080   if sap_return$='' or ~exists(env$('at')&br_filename$(sap_return$)) then 
94100     sap_return$=os_filename$(env$('userprofile')&'\Desktop')
94120   end if 
94140   fn_save_as_path$=env$('at')&sap_return$
94160 fnend 
