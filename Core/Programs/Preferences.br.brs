! Replace S:\Core\Programs\Preferences
! maintain ACS Core system settings
if ~setup then fn_setup
fnTop(program$)
win_height=20
! r: read all the setting here (unless read and set on the fly)
	fnreg_read('Enable Save Company As',enableSaveCompanyAs$, 'False')
	! fnreg_read('Enable Open Partial',enableOpenPartial$, 'False')
	fnreg_read('Enable Backup Report Cache',enableBackupReportCache$, 'False')
	! fnreg_read('Report_Cache',report_cache$, 'False')
	fnreg_read('PrintAce.Max Pages',pa_max_pages$)
	fnreg_read('formsFormat',formsFormat$, pdf_or_printace$(1))
! fnureg_read('CS Client Report Cache',client_report_cache$) : if client_report_cache$='' then client_report_cache$=report_cache$
	fnureg_read('Background_Picture',background_picture$,background_picture_default$)
	fnureg_read('Min_FontSize_Height',min_fontsize_height$,default_min_fontsize_height$)
	fnureg_read('Min_FontSize_Width',min_fontsize_width$,default_min_fontsize_width$)
	fn_get_atlantis(atlantis_exe$) : atlantis_exe$=trim$(atlantis_exe$,'"')
	fnureg_read('Word Path',word_exe$,atlantis_exe$)
	fnureg_read('Default to Use Word',use_word$,'True')
	fnureg_read('Default to Use Atlantis',use_atlantis$,'False')
	fnureg_read('Reset Word Executable persistently',resetWordExePersistently$,'False')
	fnureg_read('Text_Editor',text_editor$,fn_text_editor_default$)
	! background_picture$=fnOsFilename$(background_picture$)
	fnureg_read('Decimal',decimal_assumed$,'False')
	fnureg_read('Disable_MultiSession',disable_multisession$) : if disable_multisession$<>'True' then disable_multisession$='False'
	! fnureg_read('wait_wp_close',wait_wp_close$) : if wait_wp_close$<>'False' then wait_wp_close$='True'
	save_path$=fn_save_as_path$
	if save_path$(1:2)='@:' then save_path$(1:2)=''

	dim receipt_printer$*256
	fnureg_read('Printer.Receipt',receipt_printer$)
	if fnClientHas('U4') then
		dim hhto$*256
		fnureg_read('Hand Held To File',hhto$)
		dim hhfro$*256
		fnureg_read('Hand Held From File',hhfro$)
		dim hhrun$*256
		fnureg_read('Hand Held Run File',hhrun$)
		dim u4_deviceOption$(0)*20,u4_device$*20
		fnHandHeldList(mat u4_deviceOption$)
		fnaddonec(mat u4_deviceOption$,'[Ask]')
		fnreg_read('Hand Held Device',u4_device$, fnhand_held_device$)
		fnreg_read('Meter Location Id Sequential',u4_meterLocationIdSequential$, 'True')
		fnreg_read('Hand Held includeFinalBilled',u4_includeFinalBilled$, 'False')
	end if
	if fnClientHas('UB') then
		fnreg_read('Collections pr in Account Order',ub_collPrintInAccountOrder$,'False')
		fnreg_read('Collections Disable Deposit List',ub_collDisableDepositList$,'False')
		fnureg_read('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$,'False')
	end if
	if fnClientHas('GL') then
		fnreg_read('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$,'False')
	end if
	if fnClientHas('PR') then
		fnreg_read('Check History - enable long names when printing',pr_ckHstEnableLongNames$,'False')
		fnreg_read('Print Payroll Checks - Print checks which net zero',pr_prNetZeroChecks$,fnPrPrintNetZeroDefault$)
		if fnClientHas('CL') then
			fnreg_read('Post to Checkbook - Populate Checkbook Payee from Payroll Employee',pr_prEmpToClPayee$,'True')
			! fnreg_read('Post to Checkbook - Prefix (optional)',pr_clPayeePrefix$)
		end if
	end if
	if fnClientHas('EM') then
		dim em_emailReplyTo$*128
		dim em_smtpServer$*128
		dim em_emailFrom$*128
		dim em_emailFromPassword$*128
		dim em_emailPort$*128
		fnreg_read('email.ReplyTo',em_emailReplyTo$)
		fnreg_read('email.smtpServer',em_smtpServer$)
		fnreg_read('email.From',em_emailFrom$,'acs-billing@utilitybilling.us')
		fnreg_read('email.FromPassword',em_emailFromPassword$)
		fnreg_read('email.Port',em_emailPort$,'25')

	end if
! /r
!  main loops - build and display screens, get, save, apply settings, etc
DO_SCREEN_MAIN: ! r:
	do
		fnTos
		fn_nav_buttons
		col1_width=33 : col2_pos=col1_width+2 : lc=rc=0 : win_width=75
		fnLbl(lc+=1,1,"** System Settings **",win_width,2)
		lc+=1
		fnLbl(lc+=1,1,"ACS Client Name:",col1_width,1)
		fnTxt(lc,col2_pos,54,0,0,'',1,'If this is not correct contact ACS Support') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
		resp$(rc+=1)=env$('Client')

		fnLbl(lc+=1,1,"BR Serial:",col1_width,1)
		fnTxt(lc,col2_pos,54,0,0,'',1,'Serial Number assigned by Business Rules Corp license') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
		resp$(rc+=1)=str$(serial)

		fnLbl(lc+=1,1,"Data Folder:",col1_width,1)
		fnTxt(lc,col2_pos,40,255,0,"",1)
		resp$(rc+=1)=os_filename$('[Q]\')
		fnButton(lc,col2_pos+42+5,'Open',15) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		lc+=1
		fnChk(lc+=1,col2_pos,'Include Report Cache in Save As',1)
		resp$(resp_enableBackupReportCache:=rc+=1)=enableBackupReportCache$
		fnChk(lc+=1,col2_pos,'Enable Save Company As',1)
		resp$(resp_enableSaveCompanyAs:=rc+=1)=enableSaveCompanyAs$
		fnLbl(lc   ,col2_pos+3,'Saves only Current Company Number')
		fnLbl(lc+=1,col2_pos+3,'Does not save settings, reports.')
		! fnChk(lc+=1,col2_pos,'Enable Open Partial',1)
		! resp$(resp_enableOpenPartial:=rc+=1)=enableOpenPartial$
		fnLbl(lc   ,col2_pos+3,'Extracts a single company for a single system from a Save')
		fnLbl(lc+=1,col2_pos+3,'Does not restore report cache, global nor system settings.')
		lc+=1
		fnLbl(lc+=1,1,"** User Settings **",win_width,2)
		lc+=1
		fnLbl(lc+=1,1,"Text Editor Executable:",col1_width,1)
		fnTxt(lc,col2_pos,42,80,0,'70',0,'Select an executable for editing text files.')
		resp$(resp_text_editor:=rc+=1)=text_editor$
		fnButton(lc,col2_pos+42+5,'Default',14) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		lc+=1
		fnLbl(lc+=1,1,"Save (Company) As Folder Default:",col1_width,1)
		fnTxt(lc,col2_pos,42,80,0,'',0,'')
		resp$(resp_save_path:=rc+=1)=save_path$
		fnButton(lc,col2_pos+42+5,'Default',16) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		lc+=1
		fnChk(lc+=1,col2_pos,'Assume decimal place',1)
		fnLbl(lc ,col2_pos+3,'if checked    examples:  1234= 12.34   80= 0.80  5.=5.00  4=0.04')
		fnLbl(lc+1,col2_pos+3,'if unchecked  examples:  1234=1234.00  80=80.00  5.=5.00  4=4.00')
		resp$(resp_decimal_assumed:=rc+=1)=decimal_assumed$
		lc+=2
		fnChk(lc+=1,col2_pos,'Disable multiple sessions',1)
		fnLbl(lc,col2_pos+3,'If checked only allow one session at a time will be allowed.')
		resp$(resp_disable_multisession:=rc+=1)=disable_multisession$
		lc+=1
		fnCmdKey("&Save",1,1)
		fnCmdKey("Apply",2,0)
		fnCmdKey("&Cancel",5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=5 then
			goto Xit
		else
			if text_editor$<>'' then text_editor$=os_filename$(env$('at')&resp$(resp_text_editor))
			decimal_assumed$=resp$(resp_decimal_assumed)
			disable_multisession$=resp$(resp_disable_multisession)
			save_path$=resp$(resp_save_path)
			enableSaveCompanyAs$=resp$(resp_enableSaveCompanyAs)
			! enableOpenPartial$=resp$(resp_enableOpenPartial)
			enableBackupReportCache$=resp$(resp_enableBackupReportCache)
		end if
		
		if ckey=>screen_ck_low and ckey<=screen_ck_high then
			goto SCREEN_CK_GOTO
		else if ckey=15 then
			execute 'sy -c -w explorer "'&os_filename$('[Q]\')&'"'
		else if ckey=16 then
			fnureg_write('Save As Path',os_filename$(env$('Desktop')))
		else if ckey=14 then
			fnureg_write('Text_Editor','')
			text_editor$=fn_text_editor_default$
		else ! Save and Apply
			fn_save
			if ckey<>2 then goto Xit
		end if
	loop  ! /r
DO_SCREEN_THEME: ! r:
	do
		fnTos
		fn_nav_buttons
		col1_width=19 : col2_pos=col1_width+2 : col3_pos=col2_pos+12 : lc=0 : win_width=75
		fnLbl(lc+=1,1,"** User Settings **",win_width,2)
		lc+=1
		fnLbl(lc+=1,1,"Background Picture:",col1_width,1)
		fnTxt(lc,col2_pos,42,256,0,'70',0,'Select any picture to be used as your ACS background image.')
		resp$(resp_background_picture:=1)=background_picture$ ! os_filename$(env$('at')&background_picture$)
		fnButton(lc,col2_pos+42+5,'Default',11) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		lc+=1
		fnLbl(lc+=1,1,"Minimum Font Size:",col1_width,1)
		fnLbl(lc,col2_pos+3,"x",1,2)
		fnTxt(lc,col2_pos,2,0,0,'30',0,'Height')
		if min_fontsize_height$='' or trim$(min_fontsize_height$)='0' then min_fontsize_height$=default_min_fontsize_height$
		resp$(resp_min_fontsize_height:=2)=min_fontsize_height$
		fnTxt(lc,col2_pos+5,2,0,0,'30',0,'Width')
		if min_fontsize_width$='' or trim$(min_fontsize_width$)='0' then min_fontsize_width$=default_min_fontsize_width$
		resp$(resp_min_fontsize_width:=3)=min_fontsize_width$
		fnButton(lc,col2_pos+9,'Default',12) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
! Min_FontSize 14x6
		lc+=1
		fnLbl(lc+=1,1,"*** Colors ***",win_width,2) : rc_color=rc_color_zero=3
		fnLbl(lc+=1,col2_pos,"Foreground",10,2)
		fnLbl(lc,col3_pos,"Background",10,2)
		fnButton(lc   ,col3_pos+20,'Light Defaults',ck_lightMode:=1201)
		fnButton(lc   ,col3_pos+40,'Dark Defaults' ,ck_darkMode:=1202)

		fn_do_screen_theme_add_theme('Screen'      ,colorDefaultForeground$(1),colorDefaultBackground$(1))  :  fnLbl(lc,col3_pos+12,'Base for all settings, including buttons that are not cancel nor default')
		fn_do_screen_theme_add_theme('ScreenHeader',colorDefaultForeground$(2),colorDefaultBackground$(2))
		fn_do_screen_theme_add_theme('TextBoxes'   ,colorDefaultForeground$(3),colorDefaultBackground$(3))
		fn_do_screen_theme_add_theme('Labels'      ,colorDefaultForeground$(4),colorDefaultBackground$(4))
		fn_do_screen_theme_add_theme('Buttons'     ,colorDefaultForeground$(5),colorDefaultBackground$(5))  :  fnLbl(lc,col3_pos+12,'Default Button')
		fn_do_screen_theme_add_theme('ButtonCancel',colorDefaultForeground$(6),colorDefaultBackground$(6))  :  fnLbl(lc,col3_pos+12,'Cancel Button')
		fn_do_screen_theme_add_theme('GridHeaders' ,colorDefaultForeground$(7),colorDefaultBackground$(7))


		fnCmdKey("&Save",1,1)
		fnCmdKey("Apply",2,0)
		fnCmdKey("&Cancel",5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=5 then
			goto Xit
		else
			modeShift=0
			if ckey=ck_lightMode then
				fn_set_color_defaults(0)
				modeShift=1
				ckey=2
			else if ckey=ck_darkMode then
				fn_set_color_defaults(1)
				modeShift=1
				ckey=2
			end if
			background_picture$=resp$(resp_background_picture)
			min_fontsize_height$=resp$(resp_min_fontsize_height) : if min_fontsize_height$='' then min_fontsize_height$=default_min_fontsize_height$
			min_fontsize_width$=resp$(resp_min_fontsize_width) : if min_fontsize_width$='' then min_fontsize_width$=default_min_fontsize_width$
			rc_color=rc_color_zero
			if modeShift then
				rc_tmp=rc_color_zero
				resp$(rc_tmp+=1)=colorDefaultForeground$(1) : resp$(rc_tmp+=1)=colorDefaultBackground$(1)
				resp$(rc_tmp+=1)=colorDefaultForeground$(2) : resp$(rc_tmp+=1)=colorDefaultBackground$(2)
				resp$(rc_tmp+=1)=colorDefaultForeground$(3) : resp$(rc_tmp+=1)=colorDefaultBackground$(3)
				resp$(rc_tmp+=1)=colorDefaultForeground$(4) : resp$(rc_tmp+=1)=colorDefaultBackground$(4)
				resp$(rc_tmp+=1)=colorDefaultForeground$(5) : resp$(rc_tmp+=1)=colorDefaultBackground$(5)
				resp$(rc_tmp+=1)=colorDefaultForeground$(6) : resp$(rc_tmp+=1)=colorDefaultBackground$(6)
				resp$(rc_tmp+=1)=colorDefaultForeground$(7) : resp$(rc_tmp+=1)=colorDefaultBackground$(7)
				ckey=2
			end if
			fnureg_write('color.[screen].foreground',resp$(rc_color+=1))
			fnureg_write('color.[screen].background',resp$(rc_color+=1))
			fnureg_write('color.[screenheader].foreground',resp$(rc_color+=1))
			fnureg_write('color.[screenheader].background',resp$(rc_color+=1))
			fnureg_write('color.[textboxes].foreground',resp$(rc_color+=1))
			fnureg_write('color.[textboxes].background',resp$(rc_color+=1))
			fnureg_write('color.[labels].foreground',resp$(rc_color+=1))
			fnureg_write('color.[labels].background',resp$(rc_color+=1))
			fnureg_write('color.[buttons].foreground',resp$(rc_color+=1))
			fnureg_write('color.[buttons].background',resp$(rc_color+=1))
			fnureg_write('color.[buttoncancel].foreground',resp$(rc_color+=1))
			fnureg_write('color.[buttoncancel].background',resp$(rc_color+=1))
			fnureg_write('color.[gridheaders].foreground',resp$(rc_color+=1))
			fnureg_write('color.[gridheaders].background',resp$(rc_color+=1))
		end if
		!
		if ckey=>screen_ck_low and ckey<=screen_ck_high then
			goto SCREEN_CK_GOTO
		else if ckey=11 then
			background_picture$=background_picture_default$
		else if ckey=12 then
			min_fontsize_height$=default_min_fontsize_height$
			min_fontsize_width$=default_min_fontsize_width$
		else ! Save and Apply
			fn_save
			if ckey<>2 then goto Xit
		end if
	loop  ! /r
def fn_do_screen_theme_add_theme(attribute$,foreground_default$,background_default$)
	lc+=1
	fnLbl(lc+=1,1,attribute$&":",col1_width,1)
	fnTxt(lc,col2_pos,10,7,0,'',0,attribute$&' Foreground: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
	fnureg_read('color.['&lwrc$(attribute$)&'].foreground',resp$(rc_color+=1),foreground_default$,1) ! if resp$(rc_color)='' then resp$(rc_color)=foreground_default$
	fnTxt(lc,col3_pos,10,7,0,'',0,attribute$&' Background: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
	fnureg_read('color.['&lwrc$(attribute$)&'].background',resp$(rc_color+=1),background_default$,1) ! if resp$(rc_color)='' then resp$(rc_color)=background_default$
fnend
def fn_set_color_defaults(darkMode)
	if darkMode then
		colorDefaultForeground$(1)='#FFFFFF' : colorDefaultBackground$(1)='#283033' ! 'Screen'
		colorDefaultForeground$(2)='#FFFFFF' : colorDefaultBackground$(2)='#283033' ! 'ScreenHeader'
		colorDefaultForeground$(3)='#FFFFFF' : colorDefaultBackground$(3)='#283033' ! 'TextBoxes'
		colorDefaultForeground$(4)='#FFFFFF' : colorDefaultBackground$(4)='#283033' ! 'Labels'
		colorDefaultForeground$(5)='#4eba52' : colorDefaultBackground$(5)='#283033' ! 'Buttons'
		colorDefaultForeground$(6)='#ff4760' : colorDefaultBackground$(6)='#283033' ! 'ButtonCancel'
		colorDefaultForeground$(7)='#FFFFFF' : colorDefaultBackground$(7)='#283033' ! 'GridHeaders'
	else
		colorDefaultForeground$(1)='#000000' : colorDefaultBackground$(1)='#E7EDF5' ! 'Screen'
		colorDefaultForeground$(2)='#000000' : colorDefaultBackground$(2)='#FFFFFF' ! 'ScreenHeader'
		colorDefaultForeground$(3)='#000000' : colorDefaultBackground$(3)='#FFFFFF' ! 'TextBoxes'
		colorDefaultForeground$(4)='#000000' : colorDefaultBackground$(4)='#B0C4DE' ! 'Labels'
		colorDefaultForeground$(5)='#000000' : colorDefaultBackground$(5)='#74DF00' ! 'Buttons'
		colorDefaultForeground$(6)='#000000' : colorDefaultBackground$(6)='#CD5C5C' ! 'ButtonCancel'
		colorDefaultForeground$(7)='#000000' : colorDefaultBackground$(7)='#FFFFFF' ! 'GridHeaders'
	end if
fnend
DO_SCREEN_PRINTER: ! r:
	do
		fnTos
		fn_nav_buttons
		col1_width=33 : col2_pos=col1_width+2 : lc=0 : win_width=75 : dsp_rc=0
		fnLbl(lc+=1,1,"** System Settings **",win_width,2)
		lc+=1
		! fnChk(lc+=1,5,"Enable Report Cache",1) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
		! resp$(resp_report_cache:=dsp_rc+=1)=report_cache$
		fnTxt(lc,35,40,256,0,'',1,'') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
		resp$(dsp_rc+=1)=os_filename$('[Q]\Report Cache')
		fnButton(lc,30,'Open',12) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		lc+=1
		fnLbl(lc+=1,1,"PrintAce Max Pages:",col1_width,1)
		fnTxt(lc,col2_pos,3,3,0,'30',0,'Use to break up large PrintAce type pr jobs into smaller batches. 0 disables feature.')
		resp$(resp_pa_max_pages:=dsp_rc+=1)=pa_max_pages$
		lc+=1
		fnLbl(lc+=1,1,"Forms Format:",col1_width,1)
		fnComboA('formsFormat',lc,col2_pos,mat pdf_or_printace$,'Select a format for special forms',10) ! 42,80,0,'70',0,'Select a printer to be used when printing receipts.')
		resp$(resp_formsFormat:=dsp_rc+=1)=formsFormat$
		lc+=1
		fnLbl(lc+=1,1,"** User Settings **",win_width,2)
!   lc+=1
!   fnLbl(lc+=1,1,"Client's path to Report Cache:",col1_width,1)
!   fnTxt(lc,col2_pos,40,256,0,'',0,'client path to '&os_filename$('[Q]\Report Cache')&'\nOnly necessary if using Client/Server.') ! fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
!   resp$(resp_client_report_cache:=dsp_rc+=1)=client_report_cache$
		lc+=1
		fnLbl(lc+=1,1,"Word Executable:",col1_width,1,0,0,0,'Or default word processor')
		fnTxt(lc,col2_pos,42,256,0,'70',0,'Select an executable for Word or your default Word Processor to display and pr your RTF reports.')
		resp$(resp_word:=dsp_rc+=1)=word_exe$
		fnButton(lc,col2_pos+42+5,'Default',15) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		fnOpt(lc,col2_pos+42+5+7+2,"Use Word as Default") : resp$(resp_use_word:=dsp_rc+=1)=use_word$
		fnChk(lc+=1,col2_pos+42,"Reset Word Executable persistently",1)
		resp$(resp_useWordDefault:=dsp_rc+=1)=resetWordExePersistently$
		fnLbl(lc+=1,1,"Atlantis Executable:",col1_width,1)
		fnTxt(lc,col2_pos,42,256,0,'70',0,'Select the executable for your Atlantis Word Processor.')
		resp$(resp_atlantis:=dsp_rc+=1)=atlantis_exe$ ! os_filename$(atlantis_exe$)
		fnButton(lc,col2_pos+42+5,'Default',13) ! fnButton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
		fnOpt(lc,col2_pos+42+5+7+2,"Use Atlantis as Default") : resp$(resp_use_atlantis:=dsp_rc+=1)=use_atlantis$

		! lc+=1
		! fnChk(lc+=1,55,"Wait for word processor to close before continuing",1)
		! resp$(resp_wait_wp_close:=dsp_rc+=1)=wait_wp_close$
		lc+=1

		fnLbl(lc+=1,1,"Receipt Printer:",col1_width,1)
		fnComboA('printer',lc,col2_pos,mat printer_list$,'Select a printer to be used to pr receipts.',42) ! 42,80,0,'70',0,'Select a printer to be used when printing receipts.')
		resp$(resp_receipt_printer:=dsp_rc+=1)=receipt_printer$
		fnButton(lc,col2_pos+42+5,'Test',14)
		fnCmdKey("&Save",1,1)
		fnCmdKey("Apply",2,0)
		fnCmdKey("&Cancel",5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=5 then
			goto Xit
		else
			! report_cache$=resp$(resp_report_cache)
			! wait_wp_close$=resp$(resp_wait_wp_close)
			pa_max_pages$		=resp$(resp_pa_max_pages)
			atlantis_exe$		=resp$(resp_atlantis)
			word_exe$				=resp$(resp_word)
			resetWordExePersistently$=resp$(resp_useWordDefault)
			use_word$				=resp$(resp_use_word)
			use_atlantis$		=resp$(resp_use_atlantis)
			receipt_printer$=resp$(resp_receipt_printer)
			formsFormat$			=resp$(resp_formsFormat)
		end if

		!
		if ckey=>screen_ck_low and ckey<=screen_ck_high then
			goto SCREEN_CK_GOTO
		else if ckey=14 then
			fnureg_write('Printer.Receipt',receipt_printer$)
			fnOpenReceiptPrinter
			pr #255: 'ACS Receipt Printer Test'
			pr #255: ''
			pr #255: ''
			pr #255: ''
			fnCloseReceiptPrinter
		else if ckey=13 then
			fnureg_write('Atlantis Path','')
			fn_get_atlantis(atlantis_exe$) : atlantis_exe$=trim$(atlantis_exe$,'"')
		else if ckey=15 then
			dim office_word_exe$*256
			if fn_get_office_word(office_word_exe$) then
				word_exe$=office_word_exe$
			end if
		else if ckey=12 then
			execute 'sy -c -w explorer "'&os_filename$('[Q]\Report Cache')&'"'
		else ! Save and Apply
			fn_save
			if ckey<>2 then goto Xit
		end if
	loop  ! /r
DO_SCREEN_HH: ! r:
do
	fnTos
	fn_nav_buttons
	col1_width=25 : col2_pos=col1_width+2 : lc=0 : win_width=75 : dsh_rc=0
	fnLbl(lc+=1,1,"** System Settings **",win_width,2)
	lc+=1 ! fnLbl(myline,mypos,txt$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
	fnLbl(lc+=1,1,"Device Type:",col1_width,1,0,0,0,'Select device type - leave blank to reset to default')
	fnComboA('u4Device',lc,col2_pos,mat u4_deviceOption$,'Select device type - leave blank to reset to default')
	resp$(resp_u4_device:=dsh_rc+=1)=u4_device$
	lc+=1
	fnChk(lc+=1,col2_pos,'Meter Location Id Sequential',1)
! fnLbl(lc,col2_pos+3,'(extra explanation)')
	resp$(resp_u4_MeterLocIdSequential:=dsh_rc+=1)=u4_meterLocationIdSequential$
	fnChk(lc+=1,col2_pos,'Include Final Billed',1)
! fnLbl(lc,col2_pos+3,'(extra explanation)')
	resp$(resp_u4_uncludeFinalBilled:=dsh_rc+=1)=u4_includeFinalBilled$
	lc+=1
	fnLbl(lc+=1,1,"** User Settings **",win_width,2)
	lc+=1
	fnLbl(lc+=1,1,"File to Create:",col1_width,1,0,0,0,'Select a path and file for ACS to create output files for hand helds.')
	fnTxt(lc,col2_pos,42,80,0,'70',0,'Select a path and file for ACS to create output files for hand helds.')
	resp$(resp_hhto:=dsh_rc+=1)=hhto$
	fnLbl(lc+=1,1,"File to Retrieve:",col1_width,1,0,0,0,'Select a path and file for ACS to read input files from hand helds.')
	fnTxt(lc,col2_pos,42,80,0,'70',0,'Select a path and file for ACS to read input files from hand helds.')
	resp$(resp_hhfro:=dsh_rc+=1)=hhfro$
	fnLbl(lc,col2_pos+45,"(set to [Ask] to ask every time)")
	lc+=1
	fnLbl(lc+=1,1,"Command to run afterward:",col1_width,1,0,0,0,'Select a command to run after the hand held file is made.')
	fnTxt(lc,col2_pos,42,80,0,'70',0,'Select a path and file for ACS to read input files from hand helds.')
	fnLbl(lc,col2_pos+45,"(use [file] to indicate the created file)")
	resp$(resp_hhrun:=dsh_rc+=1)=hhrun$

	fnCmdKey("&Save",1,1)
	fnCmdKey("Apply",2,0)
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		u4_device$=resp$(resp_u4_device)
		u4_meterLocationIdSequential$=resp$(resp_u4_MeterLocIdSequential)
		u4_includeFinalBilled$=resp$(resp_u4_uncludeFinalBilled)
		hhto$=resp$(resp_hhto)
		hhfro$=resp$(resp_hhfro)
		hhrun$=resp$(resp_hhrun)
	end if
	if ckey=>screen_ck_low and ckey<=screen_ck_high then
		goto SCREEN_CK_GOTO
	else ! Save and Apply
		fn_save
		if ckey<>2 then goto Xit
	end if
loop ! /r
DO_SCREEN_UB: ! r:
do
	fnTos
	fn_nav_buttons
	col1_width=46 : col2_pos=col1_width+2 : lc=0 : win_width=75 : ub_rc=0
	fnLbl(lc+=1,1,"** System Settings **",win_width,2)
	lc+=1
	fnLbl(lc+=1,1,'Collections',win_width,2)
	fnChk(lc+=1,col2_pos,'Print in Account Number order',1)
	fnLbl(lc,col2_pos+3,'Instead of order entered')
	resp$(resp_ub_cpiao:=ub_rc+=1)=ub_collPrintInAccountOrder$
	fnChk(lc+=1,col2_pos,'Disable Deposit Listing',1)
	fnLbl(lc,col2_pos+3,'Receipt Listing only')
	resp$(resp_ub_collDisableDepositList:=ub_rc+=1)=ub_collDisableDepositList$
	lc+=1
	fnLbl(lc+=1,1,"** User Settings **",win_width,2)
	lc+=1
	fnChk(lc+=1,col2_pos,'Display Total Accounts Receivable on Dashboard',1)
	fnLbl(lc,col2_pos+3,'May increase load time.')
	resp$(resp_ub_total_ar:=ub_rc+=1)=ub_total_ar_on_dashboard$
	!
	fnCmdKey("&Save",1,1)
	fnCmdKey("Apply",2,0)
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		ub_total_ar_on_dashboard$=resp$(resp_ub_total_ar)
		ub_collPrintInAccountOrder$=resp$(resp_ub_cpiao)
		ub_collDisableDepositList$=resp$(resp_ub_collDisableDepositList)
	end if
	if ckey=>screen_ck_low and ckey<=screen_ck_high then
		goto SCREEN_CK_GOTO
	else ! Save and Apply
		fn_save
		if ckey<>2 then goto Xit
	end if
loop ! /r
DO_SCREEN_GL: ! r:
do
	fnTos
	fn_nav_buttons
	col1_width=46 : col2_pos=col1_width+2 : lc=0 : win_width=75 : gl_rc=0
	fnLbl(lc+=1,1,"** System Settings **",win_width,2)
	lc+=1
	fnChk(lc+=1,col2_pos,'Enter Transactions - retain some fields between additions',1)
	resp$(resp_gl_retainFieldsDuringAdd:=gl_rc+=1)=gl_retainFieldsDuringAdd$
	lc+=1
	! fnLbl(lc+=1,1,"** User Settings **",win_width,2)
	! lc+=1
	!
	fnCmdKey("&Save",1,1)
	fnCmdKey("Apply",2,0)
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		gl_retainFieldsDuringAdd$=resp$(resp_gl_retainFieldsDuringAdd)
	end if
	if ckey=>screen_ck_low and ckey<=screen_ck_high then
		goto SCREEN_CK_GOTO
	else ! Save and Apply
		fn_save
		if ckey<>2 then goto Xit
	end if
loop ! /r
DO_SCREEN_PR: ! r:
do
	fnTos
	fn_nav_buttons
	col1_width=46 : col2_pos=col1_width+2 : lc=0 : win_width=75 : gl_rc=0
	fnLbl(lc+=1,1,"** System Settings **",win_width,2)
	lc+=1
	fnChk(lc+=1,col2_pos,'Check History - enable long names when printing',1)
	resp$(resp_pr_ckHstEnableLongNames:=gl_rc+=1)=pr_ckHstEnableLongNames$
	lc+=1
	fnLbl(lc+=1,1,'Print Payroll Checks',win_width,2)
	fnChk(lc+=1,col2_pos,'Print checks which net zero',1)
	resp$(resp_pr_prNetZeroChecks:=gl_rc+=1)=pr_prNetZeroChecks$
	if fnClientHas('CL') then
		lc+=1
		fnLbl(lc+=1,1,'Post to Checkbook',win_width,2)
		fnChk(lc+=1,col2_pos,'Populate Checkbook Payee from Payroll Employee',1)
		resp$(resp_pr_prEmpToClPayee:=gl_rc+=1)=pr_prEmpToClPayee$
		! fnLbl(lc+=1,1,'Prefix (optional):',col1_width,1,0,0,0,'')
		! fnTxt(lc,col2_pos,3,0,0,'',0,'This would be added to the beginning of the Payroll Employee Number when setting the Checkbook Payee Key')
		! resp$(resp_pr_clPayeePrefix:=gl_rc+=1)=pr_clPayeePrefix$
	end if

	! fnLbl(lc+=1,1,"** User Settings **",win_width,2)
	! lc+=1
	!
	fnCmdKey("&Save",1,1)
	fnCmdKey("Apply",2,0)
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		pr_ckHstEnableLongNames$=resp$(resp_pr_ckHstEnableLongNames)
		pr_prNetZeroChecks$=resp$(resp_pr_prNetZeroChecks)
		if fnClientHas('CL') then
			pr_prEmpToClPayee$=resp$(resp_pr_prEmpToClPayee)
			! pr_clPayeePrefix$=resp$(resp_pr_clPayeePrefix)
		end if
	end if
	if ckey=>screen_ck_low and ckey<=screen_ck_high then
		goto SCREEN_CK_GOTO
	else ! Save and Apply
		fn_save
		if ckey<>2 then goto Xit
	end if
loop ! /r
Do_Screen_Em: ! r:
do
	fnTos
	fn_nav_buttons
	col1_width=25 : col2_pos=col1_width+2 : lc=0 : win_width=75 : _rc=0
	fnLbl(lc+=1,1,"** System Settings **",win_width,2)
	lc+=1
	fnLbl(lc+=1,1,'Email Reply To',col1_width,1,0,0,0,'')
	fnTxt(lc,col2_pos,40,128,0,'',0,'')
	resp$(resp_em_emailReplyTo:=_rc+=1)=em_emailReplyTo$
	lc+=1
	fnLbl(lc+=1,1,"SMTP Server:",col1_width,1,0,0,0,'')
	fnTxt(lc,col2_pos,40,128,0,'',0,'')
	resp$(resp_em_smtpServer:=_rc+=1)=em_smtpServer$
	fnLbl(lc+=1,1,"Email From:",col1_width,1,0,0,0,'')
	fnTxt(lc,col2_pos,40,128,0,'',0,'')
	resp$(resp_em_emailFrom:=_rc+=1)=em_emailFrom$
	fnLbl(lc+=1,1,"Email From Password:",col1_width,1,0,0,0,'')
	fnTxt(lc,col2_pos,20,64,0,'',0,'')
	resp$(resp_em_emailPassword:=_rc+=1)=em_emailFromPassword$
	fnLbl(lc+=1,1,"Email Port:",col1_width,1,0,0,0,'')
	fnTxt(lc,col2_pos,5,0,0,'',0,'')
	resp$(resp_em_emailPort:=_rc+=1)=em_emailPort$

	fnCmdKey("&Save",1,1)
	fnCmdKey("Apply",2,0)
	fnCmdKey("&Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		em_emailReplyTo$=resp$(resp_em_emailReplyTo)
		em_smtpServer$=resp$(resp_em_smtpServer)
		em_emailFrom$=resp$(resp_em_emailFrom)
		em_emailFromPassword$=resp$(resp_em_emailPassword)
		em_emailPort$=resp$(resp_em_emailPort)
	end if
	if ckey=>screen_ck_low and ckey<=screen_ck_high then
		goto SCREEN_CK_GOTO
	else ! Save and Apply
		fn_save
		if ckey<>2 then goto Xit
	end if
loop ! /r
SCREEN_CK_GOTO: ! r:
	if ckey=1001 then
		screen=screen_main : goto DO_SCREEN_MAIN
	else if ckey=1002 then
		screen=screen_theme : goto DO_SCREEN_THEME
	else if ckey=1003 then
		screen=screen_print : goto DO_SCREEN_PRINTER
	else if ckey=1004 then
		screen=screen_hh : goto DO_SCREEN_HH
	else if ckey=1005 then
		screen=screen_ub : goto DO_SCREEN_UB
	else if ckey=1006 then
		screen=screen_gl : goto DO_SCREEN_GL
	else if ckey=1007 then
		screen=screen_pr : goto DO_SCREEN_PR
	else if ckey=1008 then
		screen=screen_em : goto Do_Screen_Em

	else
		pr 'SCREEN_CK_GOTO does not know how to handle ckey='&str$(ckey)&'.'
		pause
	end if
! /r
def fn_save
	fnreg_write('Enable Save Company As',enableSaveCompanyAs$)
	! fnreg_write('Enable Open Partial',enableOpenPartial$)
	fnreg_write('Enable Backup Report Cache',enableBackupReportCache$)
	! fnreg_write('Report_Cache',report_cache$)
	fnreg_write('PrintAce.Max Pages',pa_max_pages$)
	fnreg_write('formsFormat',formsFormat$)
	fnureg_write('Background_Picture',background_picture$) ! fnBrFilename$(background_picture$))
	fnureg_write('Min_FontSize_Height',min_fontsize_height$)
	fnureg_write('Min_FontSize_Width',min_fontsize_width$)
	fn_apply_theme
	! fnureg_write('wait_wp_close',wait_wp_close$)
	fnureg_write('Atlantis Path',atlantis_exe$)
	fnureg_write('Word Path',word_exe$)
	fnureg_write('Default to Use Word',use_word$)
	fnureg_write('Default to Use Atlantis',use_atlantis$)
	fnureg_write('Reset Word Executable persistently',resetWordExePersistently$)
	fnureg_write('Text_Editor',text_editor$)
	fnureg_write('Decimal',decimal_assumed$)
	fnureg_write('Disable_MultiSession',disable_multisession$)
	fnureg_write('Save As Path',save_path$)
	fnureg_write('Printer.Receipt',receipt_printer$)
	if fnClientHas('U4') then
		fnreg_write('Hand Held Device',u4_device$)
		fnreg_write('Meter Location Id Sequential',u4_meterLocationIdSequential$)
		fnreg_write('Hand Held includeFinalBilled',u4_includeFinalBilled$)
		fnureg_write('Hand Held To File',hhto$)
		fnureg_write('Hand Held From File',hhfro$)
		fnureg_write('Hand Held Run File',hhrun$)
	end if
	if fnClientHas('UB') then
		fnreg_write('Collections pr in Account Order',ub_collPrintInAccountOrder$)
		fnreg_write('Collections Disable Deposit List',ub_collDisableDepositList$)
		fnureg_write('ub_total_ar_on_dashboard',ub_total_ar_on_dashboard$)
	end if
	if fnClientHas('GL') then
		fnreg_write('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$)
	end if
	if fnClientHas('PR') then
		fnreg_write('Check History - enable long names when printing',pr_ckHstEnableLongNames$)
		fnreg_write('Print Payroll Checks - Print checks which net zero',pr_prNetZeroChecks$)
		if fnClientHas('CL') then
			fnreg_write('Post to Checkbook - Populate Checkbook Payee from Payroll Employee',pr_prEmpToClPayee$)
			! fnreg_write('Post to Checkbook - Prefix (optional)',pr_clPayeePrefix$)
		end if
	end if
	if fnClientHas('EM') then
		fnreg_write('email.ReplyTo',em_emailReplyTo$)
		fnreg_write('email.smtpServer',em_smtpServer$)
		fnreg_write('email.From',em_emailFrom$)
		fnreg_write('email.FromPassword',em_emailFromPassword$)
		fnreg_write('email.Port',em_emailPort$)
	end if

fnend
def fn_nav_buttons
	if ~setup_nav_buttons then
		setup_nav_buttons=1
		screen_main=1
		screen_theme=2
		screen_print=3
		screen_hh=4
		screen_ub=5
		screen_gl=6
		screen_pr=7
		screen_em=8
		screen_ck_low=1001
		screen_ck_high=1008
	end if
	if screen=0 then screen=screen_main
	nb_lc=0 : nb_pos=110 : nb_len=15
	fnLbl(win_height,nb_len,'') ! forces all the windows for each screen to be at least the hight specified by win_height (set toward the top of this program)
	fnButtonOrDisabled(screen<>screen_main,nb_lc+=1,nb_pos,'Main',1001, '',nb_len)
	fnButtonOrDisabled(screen<>screen_theme,nb_lc+=1,nb_pos,'Theme',1002, '',nb_len)
	fnButtonOrDisabled(screen<>screen_print,nb_lc+=1,nb_pos,'Printer',1003, '',nb_len)
	nb_lc+=1
	if fnClientHas('PR') then
		fnButtonOrDisabled(screen<>screen_pr,nb_lc+=1,nb_pos,'Payroll',1007, '',nb_len)
	end if
	if fnClientHas('GL') then
		fnButtonOrDisabled(screen<>screen_gl,nb_lc+=1,nb_pos,'General Ledger',1006, '',nb_len)
	end if
	if fnClientHas('UB') then
		fnButtonOrDisabled(screen<>screen_ub,nb_lc+=1,nb_pos,'Utility Billing',1005, '',nb_len)
	end if

	if fnClientHas('U4') then
		fnButtonOrDisabled(screen<>screen_hh,nb_lc+=1,nb_pos,'(UB) Hand Held',1004, '',nb_len)
	end if

	if fnClientHas('EM') then
		fnButtonOrDisabled(screen<>screen_em,nb_lc+=1,nb_pos,'Email',1008, '',nb_len)
	end if
	fnLbl(22,1,'')
fnend
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		dim resp$(20)*256
		dim background_picture$*256,atlantis_exe$*90,word_exe$*256,save_path$*256
		dim text_editor$*256
		default_min_fontsize_height$='18' ! '15'   ! '14'
		default_min_fontsize_width$=  '7' !  '8'   !  '6'
		dim background_picture_default$*256
		background_picture_default$=fnOsFilename$('S:\Core\wallpaper\LauraVisitsKilimanjaro2019.jpg')

		dim printer_list$(1)*256
		printer_list(mat printer_list$) ! printer_count=printer_list(mat printer_list$)

		dim pdf_or_printace$(2)*12
		pdf_or_printace$(1)='PrintAce'
		pdf_or_printace$(2)='PDF'

		if env$('BR_MODEL')='CLIENT/SERVER' then clientServer=1 else clientServer=0

	end if
fnend

Xit: fnXit
def library fnapply_theme(; disableConScreenOpenDflt)
	if ~setup then fn_setup
	fnapply_theme=fn_apply_theme( disableConScreenOpenDflt)
fnend
def fn_apply_theme(; disableConScreenOpenDflt)
	fnureg_read('Background_Picture',background_picture$)
	if background_picture$='' or ~exists(background_picture$) then background_picture$=background_picture_default$
	fnSetEnv('background_picture',background_picture$)
	fnureg_read('Min_FontSize_Height',min_fontsize_height$)
	fnureg_read('Min_FontSize_Width',min_fontsize_width$)
	execute 'config Min_FontSize '&min_fontsize_height$&'x'&min_fontsize_width$ error ERR_MIN_FONTSIZE
	fn_set_color('[screen]','#000000','#E7EDF5')
	fn_set_color('[screenheader]','#000000','#FFFFFF')
	fn_set_color('[textboxes]','#000000','#FFFFFF')
	fn_set_color('[gridheaders]','#000000','#FFFFFF')
	fn_set_color('[labels]','#000000','#B0C4DE')
	fn_set_color('[buttons]','#000000','#74DF00') ! '#F0F8FF')
	fn_set_color('[buttoncancel]','#000000','#CD5C5C')
	if ~disableConScreenOpenDflt then
		execute 'Config Screen OpenDflt "Rows=35, Cols=115, Picture='&env$('background_picture')&',border=S:[screen],N=[screen]"'
	end if
fnend
ERR_MIN_FONTSIZE: ! r:
	execute 'config Min_FontSize '&default_min_fontsize_height$&'x'&default_min_fontsize_width$ ! pr 'ERR_MIN_FONTSIZE' : pause
continue ! /r
def fn_set_color(attribute$,foreground_default$,background_default$)
	fnureg_read('color.'&attribute$&'.foreground',foreground$) : if foreground$='' then foreground$=foreground_default$
	fnureg_read('color.'&attribute$&'.background',background$) : if background$='' then background$=background_default$
	execute 'Config Attribute '&attribute$&' /'&foreground$&':'&background$ error ignore ! pr 'config attribute '&attribute$&' /'&foreground$&':'&background$ : pause
fnend
def library fnEditFile(efEditorType$,fileToEdit$*256)
	if ~setup then fn_setup
	fnEditFile=fn_editFile(efEditorType$,fileToEdit$)
fnend
def fn_editFile(efEditorType$,efFileToEdit$*256)
	! efEditorType$ - wordprocessor, atlantis, word or text
	efWaitText$=''
	efForce$=''
	efSwitches$=''
	if lwrc$(efEditorType$)=lwrc$('atlantis') then
		efEditorType$='wordprocessor'
		efForce$='atlantis'
		efWaitText$='Atlantis'
	else if lwrc$(efEditorType$)=lwrc$('word') then
		efEditorType$='wordprocessor'
		efForce$='word'
		efWaitText$='Microsoft Word'
	end if
	! r: get the executable and set any switches
	if lwrc$(efEditorType$)=lwrc$('wordprocessor') then
		dim efExe$*256
		fn_get_wordprocessor_exe(efExe$, efForce$)
		if efWaitText$='' then efWaitText$='Word Processor'
		if pos(lwrc$(efExe$),'atlantis.exe')>0 or pos(lwrc$(efExe$),'awp.exe')>0 then
			efSwitches$=' -n'
		end if
	else if lwrc$(efEditorType$)=lwrc$('text') then
		fnureg_read('Text_Editor',efExe$,fn_text_editor_default$)
		efWaitText$='Text Editor'
	else
		pr 'unrecognized Editor Type: '&efEditorType$ : pause
	end if
	! /r
	! r: determine efEditOnClientCopyOfFile$
	dim efEditOnClientCopyOfFile$*256
	if clientServer then
		dim efFilePath$*256,efFileName$*128,efFileExt$*128
		fnGetPp(efFileToEdit$,efFilePath$,efFileName$,efFileExt$)
		efEditOnClientCopyOfFile$=env$('at')&fnProgramDataDir$&'Temp\Session'&session$&'\'&efFileName$&efFileExt$
		fnMakeSurePathExists(efEditOnClientCopyOfFile$)
		if exists(efFileToEdit$) then
			fncopyfile(efFileToEdit$,efEditOnClientCopyOfFile$)
		end if
	else
		efEditOnClientCopyOfFile$=efFileToEdit$
	end if
	! /r
	fnWaitForShellCloseStart(efWaitText$)
	exe 'Sy -w -@ '&efExe$&' "'&os_filename$(efEditOnClientCopyOfFile$)&'"'&efSwitches$
	fnWaitForShellCloseEnd
	if clientServer then
		! sleep(.2)  <maybe this would be a good idea if copies start to error or fail.  it'd give the wp a little longer to complete save...  but in a perfect world it isn't necessary
		fncopyfile(efEditOnClientCopyOfFile$,efFileToEdit$)
	end if
fnend
def fn_text_editor_default$*256
	dim atlantis_path$*256,text_editor$*256
	if exists(env$('at')&":C:\Windows\Notepad.exe") then
		text_editor$='C:\Windows\Notepad.exe'
	else if exists(env$('at')&":C:\Program Files (x86)\Atlantis\Atlantis.exe") then
		text_editor$='C:\Program Files (x86)\Atlantis\Atlantis.exe'
	else if exists(env$('at')&":C:\Program Files (x86)\Atlantis\awp.exe") then
		text_editor$='C:\Program Files (x86)\Atlantis\Atlantis.exe'
	else if exists(env$('at')&":C:\Program Files\Atlantis\Atlantis.exe") then
		text_editor$='C:\Program Files\Atlantis\Atlantis.exe'
	else
		text_editor$=os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')
	end if
	! if ~exists(br_filename$(trim$(text_editor$,'"'))) then
	!   text_editor$='"'&os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')&'"'
	! end if
	text_editor$=trim$(text_editor$,'"')
	fn_text_editor_default$=text_editor$
fnend
def library fnget_wordprocessor_exe(&wordprocessor_exe$; force$)
	if ~setup then fn_setup
	fnget_wordprocessor_exe=fn_get_wordprocessor_exe(wordprocessor_exe$, force$)
fnend
def fn_get_wordprocessor_exe(&wordprocessor_exe$; force$)
	if resetWordExePersistently$='' then
		fnureg_read('Reset Word Executable persistently',resetWordExePersistently$,'False')
	end if
	! fnureg_read('Default to Use Word',use_word$,'True')  ! it's the default
	if force$='' then
		fnureg_read('Default to Use Atlantis',use_atlantis$,'False')
	end if
	if lwrc$(force$)=lwrc$('atlantis') or trim$(use_atlantis$)='True' then
		fn_get_atlantis(wordprocessor_exe$)
	else
		fnureg_read('Word Path',wordprocessor_exe$)
		if trim$(wordprocessor_exe$)='' or resetWordExePersistently$='True' then
			if fn_get_office_word(wordprocessor_exe$) then
				fnureg_write('Word Path',wordprocessor_exe$)
			else
				msgbox("Microsoft Word Executable could not be automaticaly detected.  It must be set manually in File > Preferences > Printer")
			end if !
		end if
	end if
fnend
def fn_get_atlantis(&atlantis_exe$)
	dim atlantis_path$*256
	fnureg_read('Atlantis Path',atlantis_path$)
	if trim$(atlantis_path$)<>'' then
		if ~exists(env$('at')&atlantis_path$) and exists(env$('at')&srep$(lwrc$(atlantis_path$),'atlantis.exe','awp.exe')) then
			atlantis_path$=srep$(lwrc$(atlantis_path$),'atlantis.exe','awp.exe')
		end if
		if ~exists(env$('at')&atlantis_path$) and exists(env$('at')&srep$(lwrc$(atlantis_path$),'awp.exe','atlantis.exe')) then
			atlantis_path$=srep$(lwrc$(atlantis_path$),'awp.exe','atlantis.exe')
		end if
		atlantis_exe$=atlantis_path$
	else
		dim atl_check_path$(5)*256
		atl_check_path$(1)="C:\Program Files (x86)\Atlantis\Atlantis.exe"
		atl_check_path$(2)="C:\Program Files\Atlantis\Atlantis.exe"
		!     atl_check_path$(3)=''&os_filename$('S:\Core\Atlantis Nova\Atlantis.exe')
		atl_check_path$(3)="C:\Program Files (x86)\Atlantis\awp.exe"
		atl_check_path$(4)="C:\Program Files\Atlantis\awp.exe"
		atl_check_path$(5)=env$('local_program_dir')&('\Core\Atlantis Nova\Atlantis.exe')
		if atl_check_path$(5)(1:2)='@:' then atl_check_path$(5)(1:2)=''
		atl_which=fn_first_exists_in_list(mat atl_check_path$)
		if atl_which>0 then
			atlantis_exe$=atl_check_path$(atl_which)
			fnureg_write('Atlantis Path',atlantis_exe$)
		else
			atlantis_exe$='' ! atl_check_path$(3)
		end if
	end if
fnend
def fn_get_office_word(&office_word_exe$)
	gow_which=0
	office_word_exe$=fnMsExe$("winword.exe")
	if office_word_exe$<>'Key not found' and exists(env$('at')&office_word_exe$) then
		gow_which=1
	end if
	fn_get_office_word=gow_which
fnend
def fn_first_exists_in_list(mat feil_list$)
	feil_return=0
	feil_item=0
	feil_count=udim(mat feil_list$)
	do until feil_return or feil_item=>feil_count
		feil_item+=1
		dim feil_tmp_test$*1024
		feil_tmp_test$=feil_list$(feil_item)
		if feil_tmp_test$(1:2)<>'@:' then feil_tmp_test$(0:0)='@:'
		if exists(feil_tmp_test$) then
			feil_return=feil_item
		end if
	loop
	fn_first_exists_in_list=feil_return
fnend
def library fndecimal_assumed
	if ~setup then fn_setup
	fndecimal_assumed=fn_decimal_assumed
fnend
def fn_decimal_assumed
	fnureg_read('Decimal',tmp$,'False')
	if tmp$='False' and env$('decimal_assumed')<>'Decimal Required' then
		execute 'Config Decimal Required'
		setenv('decimal_assumed','Decimal Required')
	else if tmp$='True' and env$('decimal_assumed')<>'Decimal Assumed' then
		execute 'Config Decimal Assumed'
		setenv('decimal_assumed','Decimal Assumed')
	end if
fnend
def library fnsave_as_path$*256
	if ~setup then fn_setup
	fnsave_as_path$=fn_save_as_path$
fnend
def fn_save_as_path$*256
	dim sap_return$*256
	fnureg_read('Save As Path',sap_return$)
	if sap_return$(1:2)='@:' then sap_return$(1:2)=''
	if sap_return$='' or ~exists(env$('at')&sap_return$) then
		sap_return$=os_filename$(env$('Desktop'))
	end if
	fn_save_as_path$=env$('at')&sap_return$
fnend
include: ertn