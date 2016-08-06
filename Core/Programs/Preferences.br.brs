10000 ! Replace R:\Core\Programs\Settings
10020 ! maintain ACS Core system settings
12000   if ~setup then let fn_setup
12020   dim resp$(10)*256,cap$*128,background_picture$*256,wp_exe$*80,save_path$*256 ! ,client_report_cache$*256
12040   let fntop(program$, cap$="Preferences")
13000   let win_height=17
13010   dim printer_list$(1)*256
13020   let printer_count=printer_list(mat printer_list$)
14000 ! r: read all the setting here (unless read and set on the fly)
14020   let fnreg_read('Report_Cache',report_cache$) : if report_cache$='' then let report_cache$='False'
14030   let fnreg_read('PrintAce.Max Pages',pa_max_pages$)
14032 ! let fnureg_read('CS Client Report Cache',client_report_cache$) : if client_report_cache$='' then let client_report_cache$=report_cache$
14040   let fnureg_read('Background_Picture',background_picture$) : if background_picture$='' then let background_picture$='R:\Core\Background.png'
14042   let fnureg_read('Min_FontSize_Height',min_fontsize_height$) : if min_fontsize_height$='' then let min_fontsize_height$=default_min_fontsize_height$
14044   let fnureg_read('Min_FontSize_Width',min_fontsize_width$) : if min_fontsize_width$='' then let min_fontsize_width$=default_min_fontsize_width$
14060   let fn_get_atlantis(wp_exe$) : let wp_exe$=trim$(wp_exe$,'"')
14080   let fnureg_read('Text_Editor',text_editor$)
14100   if text_editor$='' then let fn_text_editor_default(text_editor$)
14120   let background_picture$=os_filename$(background_picture$)
14140   let fnureg_read('Decimal',decimal_assumed$) : if decimal_assumed$<>'False' then let decimal_assumed$='True'
14160   let fnureg_read('Disable_MultiSession',disable_multisession$) : if disable_multisession$<>'True' then let disable_multisession$='False'
14180   let fnureg_read('wait_wp_close',wait_wp_close$) : if wait_wp_close$<>'False' then let wait_wp_close$='True'
14200   let save_path$=fn_save_as_path$
14300   if save_path$(1:2)='@:' then let save_path$(1:2)=''
15000 ! 
15020   dim receipt_printer$*256
15040   let fnureg_read('Printer.Receipt',receipt_printer$)
15060 ! /r
30000 !  main loops - build and display screens, get, save, apply settings, etc
32000 DO_SCREEN_MAIN: ! r:
32020   do 
32040     let fntos(sn$="Settings_Main")
32060     let fn_nav_buttons
32080     let col1_width=33 : let col2_pos=col1_width+2 : let lc=0 : let win_width=75
32100     let fnlbl(lc+=1,1,"ACS Client Name:",col1_width,1)
32120     let fntxt(lc,col2_pos,54,0,0,'',1,'If this is not correct contact ACS Support') ! fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
32140     let resp$(1)=env$('Client')
32160 ! 
32180     let fnlbl(lc+=1,1,"BR Serial:",col1_width,1)
32200     let fntxt(lc,col2_pos,54,0,0,'',1,'Serial Number assigned by Business Rules Corp license') ! fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
32220     let resp$(2)=str$(serial)
32240 ! 
32260     let fnlbl(lc+=1,1,"Data Folder:",col1_width,1)
32280     let fntxt(lc,col2_pos,40,255,0,"",1)
32300     let resp$(3)=os_filename$('Q:\')
32320     let fnbutton(lc,col2_pos+42+5,'Open',15) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32340     let lc+=1
32360     let fnlbl(lc+=1,1,"** User Settings **",win_width,2)
32380     let lc+=1
32400 ! 
32420     let fnlbl(lc+=1,1,"Text Editor Executable:",col1_width,1)
32440     let fntxt(lc,col2_pos,42,80,0,'70',0,'Select an executable for editing text files.')
32460     let resp$(resp_text_editor:=4)=text_editor$
32480     let fnbutton(lc,col2_pos+42+5,'Default',14) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32500 ! 
32520     let lc+=1
32540 ! 
32560     let fnlbl(lc+=1,1,"Save (Company) As Folder Default:",col1_width,1)
32580     let fntxt(lc,col2_pos,42,80,0,'',0,'')
32600     let resp$(resp_save_path:=5)=save_path$
32620     let fnbutton(lc,col2_pos+42+5,'Default',16) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
32640 ! 
32660     let lc+=1
32680 ! 
32700     let fnchk(lc+=1,col2_pos,'Assume decimal place',1)
32702     let fnlbl(lc ,col2_pos+3,'if checked    examples:  1234= 12.34   80= 0.80  5.=5.00  4=0.04')
32710     let fnlbl(lc+1,col2_pos+3,'if unchecked  examples:  1234=1234.00  80=80.00  5.=5.00  4=4.00')
32720     let resp$(resp_decimal_assumed:=6)=decimal_assumed$
32740     let lc+=2
32760 ! 
32780     let fnchk(lc+=1,col2_pos,'Disable multiple sessions',1)
32782     let fnlbl(lc,col2_pos+3,'If checked only allow one session at a time will be allowed.')
32800     let resp$(resp_disable_multisession:=7)=disable_multisession$
32820     let lc+=1
33040 ! 
33060     let fncmdkey("&Save",1,1)
33080     let fncmdkey("Apply",2,0)
33100     let fncmdkey("&Cancel",5,0,1)
33120     let fnacs(sn$,0,mat resp$,ck)
33140     if ck=5 then 
33160       goto XIT
33180     else 
33260       if text_editor$<>'' then let text_editor$=os_filename$(resp$(resp_text_editor))
33270       let decimal_assumed$=resp$(resp_decimal_assumed)
33280       let disable_multisession$=resp$(resp_disable_multisession)
33290       let save_path$=resp$(resp_save_path)
33300     end if 
33320 ! 
33340 ! 
33350     if ck=1001 then 
33360       let screen=screen_main : goto DO_SCREEN_MAIN
33370     else if ck=1002 then 
33380       let screen=screen_theme : goto DO_SCREEN_THEME
33390     else if ck=1003 then 
33400       let screen=screen_print : goto DO_SCREEN_PRINTER
33410     else if ck=15 then 
33420       execute 'sy -c -w explorer "'&os_filename$('Q:\')&'"'
33440     else if ck=16 then 
33460       let fnureg_write('Save As Path',os_filename$(env$('userprofile')&'\Desktop'))
33480     else if ck=14 then 
33500       let fnureg_write('Text_Editor','')
33520       let fn_text_editor_default(text_editor$)
33680     else ! Save and Apply
33700       let fn_save
33720       if ck<>2 then goto XIT
33740     end if 
33760   loop  ! /r
38000 DO_SCREEN_THEME: ! r:
38020   do 
38040     let fntos(sn$="Settings_Theme")
38060     let fn_nav_buttons
38080     let col1_width=19 : let col2_pos=col1_width+2 : let col3_pos=col2_pos+12 : let lc=0 : let win_width=75
38100     let fnlbl(lc+=1,1,"** User Settings **",win_width,2)
38120     let lc+=1
38140     let fnlbl(lc+=1,1,"Background Picture:",col1_width,1)
38160     let fntxt(lc,col2_pos,42,256,0,'70',0,'Select any picture to be used as your ACS background image.')
38180     let resp$(resp_background_picture:=1)=os_filename$(background_picture$)
38200     let fnbutton(lc,col2_pos+42+5,'Default',11) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
38203     let lc+=1
38205     let fnlbl(lc+=1,1,"Minimum Font Size:",col1_width,1)
38207     let fnlbl(lc,col2_pos+3,"x",1,2)
38209     let fntxt(lc,col2_pos,2,0,0,'30',0,'Height')
38210     if min_fontsize_height$='' or trim$(min_fontsize_height$)='0' then let min_fontsize_height$=default_min_fontsize_height$
38211     let resp$(resp_min_fontsize_height:=2)=min_fontsize_height$
38213     let fntxt(lc,col2_pos+5,2,0,0,'30',0,'Width')
38214     if min_fontsize_width$='' or trim$(min_fontsize_width$)='0' then let min_fontsize_width$=default_min_fontsize_width$
38215     let resp$(resp_min_fontsize_width:=3)=min_fontsize_width$
38217     let fnbutton(lc,col2_pos+9,'Default',12) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
38219 ! Min_FontSize 14x6
38220     let lc+=1
38240     let fnlbl(lc+=1,1,"*** Colors ***",win_width,2) : let rc_color=rc_color_zero=3
38260     let fnlbl(lc+=1,col2_pos,"Foreground",10,2)
38280     let fnlbl(lc,col3_pos,"Background",10,2)
38300 ! 
38320     let fn_do_screen_theme_add_theme('Screen','#000000','#E7EDF5')
38340     let fnlbl(lc,col3_pos+12,'Base for all settings, including buttons that are not cancel nor default')
38360     let fn_do_screen_theme_add_theme('ScreenHeader','#000000','#FFFFFF')
38380     let fn_do_screen_theme_add_theme('TextBoxes','#000000','#FFFFFF')
38400     let fn_do_screen_theme_add_theme('Labels','#000000','#B0C4DE')
38420     let fn_do_screen_theme_add_theme('Buttons','#000000','#74DF00')
38440     let fnlbl(lc,col3_pos+12,'Default Button')
38460     let fn_do_screen_theme_add_theme('ButtonCancel','#000000','#CD5C5C')
38480     let fnlbl(lc,col3_pos+12,'Cancel Button')
38500     let fn_do_screen_theme_add_theme('GridHeaders','#000000','#FFFFFF')
38520 ! 
38540 ! 
38560     let fncmdkey("&Save",1,1)
38580     let fncmdkey("Apply",2,0)
38600     let fncmdkey("&Cancel",5,0,1)
38620     let fnacs(sn$,0,mat resp$,ck)
40000     if ck=5 then 
40020       goto XIT
40040     else 
40060       let background_picture$=resp$(resp_background_picture)
40070       let min_fontsize_height$=resp$(resp_min_fontsize_height) : if min_fontsize_height$='' then let min_fontsize_height$=default_min_fontsize_height$
40072       let min_fontsize_width$=resp$(resp_min_fontsize_width) : if min_fontsize_width$='' then let min_fontsize_width$=default_min_fontsize_width$
40080       let rc_color=rc_color_zero
40100       let fnureg_write('color.[screen].foreground',resp$(rc_color+=1))
40120       let fnureg_write('color.[screen].background',resp$(rc_color+=1))
40140       let fnureg_write('color.[screenheader].foreground',resp$(rc_color+=1))
40160       let fnureg_write('color.[screenheader].background',resp$(rc_color+=1))
40180       let fnureg_write('color.[textboxes].foreground',resp$(rc_color+=1))
40200       let fnureg_write('color.[textboxes].background',resp$(rc_color+=1))
40220       let fnureg_write('color.[labels].foreground',resp$(rc_color+=1))
40240       let fnureg_write('color.[labels].background',resp$(rc_color+=1))
40260       let fnureg_write('color.[buttons].foreground',resp$(rc_color+=1))
40280       let fnureg_write('color.[buttons].background',resp$(rc_color+=1))
40300       let fnureg_write('color.[buttoncancel].foreground',resp$(rc_color+=1))
40320       let fnureg_write('color.[buttoncancel].background',resp$(rc_color+=1))
40340       let fnureg_write('color.[gridheaders].foreground',resp$(rc_color+=1))
40360       let fnureg_write('color.[gridheaders].background',resp$(rc_color+=1))
40380     end if 
40400 ! 
40420 ! 
40430     if ck=1001 then 
40440       let screen=screen_main : goto DO_SCREEN_MAIN
40450     else if ck=1002 then 
40460       let screen=screen_theme : goto DO_SCREEN_THEME
40470     else if ck=1003 then 
40480       let screen=screen_print : goto DO_SCREEN_PRINTER
40490     else if ck=11 then 
40500       let background_picture$='R:\Core\Background.png'
40502     else if ck=12 then 
40504       let min_fontsize_height$=default_min_fontsize_height$
40506       let min_fontsize_width$=default_min_fontsize_width$
40520     else ! Save and Apply
40540       let fn_save
40560       if ck<>2 then goto XIT
40580     end if 
40600   loop  ! /r
41000   def fn_do_screen_theme_add_theme(attribute$,foreground_default$,background_default$)
41020     let lc+=1
41040     let fnlbl(lc+=1,1,attribute$&":",col1_width,1)
41060     let fntxt(lc,col2_pos,10,7,0,'',0,attribute$&' Foreground: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
41080     let fnureg_read('color.['&lwrc$(attribute$)&'].foreground',resp$(rc_color+=1)) : if resp$(rc_color)='' then let resp$(rc_color)=foreground_default$
41100     let fntxt(lc,col3_pos,10,7,0,'',0,attribute$&' Background: Must be a valid hex color beginning with a #.  i.e. #000000 is black, #FFFFFF is white. Leave blank to restore default.')
41120     let fnureg_read('color.['&lwrc$(attribute$)&'].background',resp$(rc_color+=1)) : if resp$(rc_color)='' then let resp$(rc_color)=background_default$
41140   fnend 
50000 DO_SCREEN_PRINTER: ! r:
50020   do 
50040     let fntos(sn$="Settings_Printer")
50060     let fn_nav_buttons
50080     let col1_width=33 : let col2_pos=col1_width+2 : let lc=0 : let win_width=75 : let dsp_rc=0
50100     let fnlbl(lc+=1,1,"** System Settings **",win_width,2)
50120     let lc+=1
50140     let fnchk(lc+=1,5,"Enable Report Cache",1) ! fnchk(lyne,ps,txt$*196; align,contain,tabcon)
50160     let resp$(resp_report_cache:=dsp_rc+=1)=report_cache$
50180     let fntxt(lc,35,40,256,0,'',1,'') ! fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
50200     let resp$(dsp_rc+=1)=os_filename$('Q:\Report Cache')
50220     let fnbutton(lc,30,'Open',12) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50222     let lc+=1
50224     let fnlbl(lc+=1,1,"PrintAce Max Pages:",col1_width,1)
50226     let fntxt(lc,col2_pos,3,3,0,'30',0,'Use to break up large PrintAce type print jobs into smaller batches. 0 disables feature.')
50228     let resp$(resp_pa_max_pages:=dsp_rc+=1)=pa_max_pages$
50240     let lc+=1
50260     let fnlbl(lc+=1,1,"** User Settings **",win_width,2)
50280 !   let lc+=1
50290 !   let fnlbl(lc+=1,1,"Client's path to Report Cache:",col1_width,1)
50292 !   let fntxt(lc,col2_pos,40,256,0,'',0,'client path to '&os_filename$('Q:\Report Cache')&'\nOnly necessary if using Client/Server.') ! fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
50293 !   let resp$(resp_client_report_cache:=dsp_rc+=1)=client_report_cache$
50294     let lc+=1
50300     let fnlbl(lc+=1,1,"Word Processor Executable:",col1_width,1)
50320     let fntxt(lc,col2_pos,42,80,0,'70',0,'Select an executable to display and print your RTF reports.')
50340     let resp$(resp_word_processor:=dsp_rc+=1)=wp_exe$ ! os_filename$(wp_exe$)
50360     let fnbutton(lc,col2_pos+42+5,'Atlantis',13) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50362     let fnbutton(lc,col2_pos+42+5+10,'Word',15) ! fnbutton(lyne,ps,txt$*200,comkey;tt$*200,height,width,container,tabcon,default,cancel)
50370 ! 
50372     let lc+=1
50374     let fnchk(lc+=1,55,"Wait for word processor to close before continuing",1)
50375     let fnlbl(lc,56.5,"Only uncheck when Report Cahceing Enabled.")
50376     let resp$(resp_wait_wp_close:=dsp_rc+=1)=wait_wp_close$
50377     let lc+=1
50378 ! 
50400     let fnlbl(lc+=1,1,"Receipt Printer:",col1_width,1)
50420     let fncomboa('printer',lc,col2_pos,mat printer_list$,'Select a printer to be used to print receipts.',42) ! 42,80,0,'70',0,'Select a printer to be used when printing receipts.')
50440     let resp$(resp_receipt_printer:=dsp_rc+=1)=receipt_printer$
50450     let fnbutton(lc,col2_pos+42+5,'Test',14)
50460     let fncmdkey("&Save",1,1)
50480     let fncmdkey("Apply",2,0)
50500     let fncmdkey("&Cancel",5,0,1)
50520     let fnacs(sn$,0,mat resp$,ck)
50540     if ck=5 then 
50560       goto XIT
50580     else 
50600       let report_cache$=resp$(resp_report_cache)
50610       let wait_wp_close$=resp$(resp_wait_wp_close)
50612       let pa_max_pages$=resp$(resp_pa_max_pages)
50620       let wp_exe$=resp$(resp_word_processor)
50630       let receipt_printer$=resp$(resp_receipt_printer)
50632 !     let client_report_cache$=resp$(resp_client_report_cache)
50640     end if 
50660 ! 
50680 ! 
50700     if ck=1001 then 
50720       let screen=screen_main : goto DO_SCREEN_MAIN
50740     else if ck=1002 then 
50760       let screen=screen_theme : goto DO_SCREEN_THEME
50780     else if ck=1003 then 
50800       let screen=screen_print : goto DO_SCREEN_PRINTER
50802     else if ck=14 then 
50804       let fnureg_write('Printer.Receipt',receipt_printer$)
50806       let fnopen_receipt_printer
50808       print #255: 'ACS Receipt Printer Test'
50810       print #255: ''
50812       print #255: ''
50814       print #255: ''
50816       let fnclose_receipt_printer
50820     else if ck=13 then 
50840       let fnureg_write('Atlantis Path','')
50860       let fn_get_atlantis(wp_exe$) : let wp_exe$=trim$(wp_exe$,'"')
50862     else if ck=15 then 
50864       dim office_word_exe$*256
50866       if fn_get_office_word(office_word_exe$) then 
50868         let wp_exe$=office_word_exe$
50870       end if 
50880     else if ck=12 then 
50900       execute 'sy -c -w explorer "'&os_filename$('Q:\Report Cache')&'"'
50920     else ! Save and Apply
50940       let fn_save
50960       if ck<>2 then goto XIT
50980     end if 
51000   loop  ! /r
66000   def fn_save
66020     let fnreg_write('Report_Cache',report_cache$)
66030     let fnreg_write('PrintAce.Max Pages',pa_max_pages$)
66032 !   let fnureg_write('CS Client Report Cache',client_report_cache$)
66040     let fnureg_write('Background_Picture',br_filename$(background_picture$))
66050     let fnureg_write('Min_FontSize_Height',min_fontsize_height$)
66052     let fnureg_write('Min_FontSize_Width',min_fontsize_width$)
66060     let fn_apply_theme
66070     let fnureg_write('wait_wp_close',wait_wp_close$)
66080     let fnureg_write('Atlantis Path',wp_exe$)
66100     let fnureg_write('Text_Editor',text_editor$)
66120     let fnureg_write('Decimal',decimal_assumed$)
66130     let fnureg_write('Disable_MultiSession',disable_multisession$)
66140     let fnureg_write('Save As Path',save_path$)
66150     let fnureg_write('Printer.Receipt',receipt_printer$)
66160   fnend 
68000   def fn_nav_buttons
68020     if ~setup_nav_buttons then 
68040       let setup_nav_buttons=1
68060       let screen_main=1
68080       let screen_theme=2
68100       let screen_print=3
68120     end if 
68140     if screen=0 then let screen=screen_main
68160     let nb_lc=0 : let nb_pos=95 : let nb_len=15
68180     let fnlbl(win_height,nb_len,'') ! forces all the windows for each screen to be at least the hight specified by win_height (set toward the top of this program)
68220     let fnbutton_or_disabled(screen<>screen_main,nb_lc+=1,nb_pos,'Main',1001, '',nb_len)
68380     let fnbutton_or_disabled(screen<>screen_theme,nb_lc+=1,nb_pos,'Theme',1002, '',nb_len)
68540     let fnbutton_or_disabled(screen<>screen_print,nb_lc+=1,nb_pos,'Printer',1003, '',nb_len)
68660   fnend 
70000   def fn_setup
70020     if ~setup then 
70040       let setup=1
70080       library 'R:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt ,fnerror,fntos,fnchk,fnreg_read,fnreg_write,fnbutton,fncmdkey,fnureg_read,fnureg_write,fncomboa,fnbutton_or_disabled,fnopen_receipt_printer,fnclose_receipt_printer
70100       on error goto ERTN
70120       dim text_editor$*256
70130       let default_min_fontsize_height$='14'
70132       let default_min_fontsize_width$='6'
70140     end if 
70990   fnend 
72000 IGNORE: continue 
74000 ! <Updateable Region: ERTN>
74040 ERTN: let fnerror(cap$,err,line,act$,"xit")
74060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
74080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
74100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
74120 ERTN_EXEC_ACT: execute act$ : goto ERTN
74140 ! /region
74160 XIT: let fnxit
76000   def library fnapply_theme
76020     if ~setup then let fn_setup
76040     let fnapply_theme=fn_apply_theme
76060   fnend 
78000   def fn_apply_theme
78020     dim background_picture$*256
78040     let fnureg_read('Background_Picture',background_picture$)
78042     let fnureg_read('Min_FontSize_Height',min_fontsize_height$)
78044     let fnureg_read('Min_FontSize_Width',min_fontsize_width$)
78046     execute 'config Min_FontSize '&min_fontsize_height$&'x'&min_fontsize_width$ error ERR_MIN_FONTSIZE
78060     if background_picture$='' then let background_picture$='R:\Core\background.png'
78080     let setenv('background_picture',background_picture$)
78100     let fn_set_color('[screen]','#000000','#E7EDF5')
78120     let fn_set_color('[screenheader]','#000000','#FFFFFF')
78140     let fn_set_color('[textboxes]','#000000','#FFFFFF')
78160     let fn_set_color('[gridheaders]','#000000','#FFFFFF')
78180     let fn_set_color('[labels]','#000000','#B0C4DE')
78200     let fn_set_color('[buttons]','#000000','#74DF00') ! '#F0F8FF')
78220     let fn_set_color('[buttoncancel]','#000000','#CD5C5C')
78240     execute 'Config Screen OpenDflt "Rows=35, Cols=115, Picture='&env$('background_picture')&',border=S:[screen],N=[screen]"'
78260   fnend 
78270 ERR_MIN_FONTSIZE: ! 
78280   execute 'config Min_FontSize '&default_min_fontsize_height$&'x'&default_min_fontsize_width$ ! pr 'ERR_MIN_FONTSIZE' : pause
78290   continue 
78320   def fn_set_color(attribute$,foreground_default$,background_default$)
78340     let fnureg_read('color.'&attribute$&'.foreground',foreground$) : if foreground$='' then let foreground$=foreground_default$
78360     let fnureg_read('color.'&attribute$&'.background',background$) : if background$='' then let background$=background_default$
78380     execute 'Config Attribute '&attribute$&' /'&foreground$&':'&background$ error ignore ! pr 'config attribute '&attribute$&' /'&foreground$&':'&background$ : pause
78400   fnend 
80000   def library fntext_editor(te_text_file$*256; te_options$)
80020     if ~setup then let fn_setup
80040     let fntext_editor=fn_text_editor(te_text_file$, te_options$)
80060   fnend 
82000   def fn_text_editor(te_text_file$*256; te_options$)
82040     let fnureg_read('Text_Editor',text_editor$)
82050     if text_editor$='' then let fn_text_editor_default(text_editor$)
82060     execute 'SY -w -C "'&text_editor$&'" "'&os_filename$(te_text_file$)&'"'
82080   fnend 
84000   def fn_text_editor_default(&text_editor$)
84080     dim atlantis_path$*256
84100     if exists(":C:\Windows\Notepad.exe") then 
84120       let text_editor$='C:\Windows\Notepad.exe'
84140     else if exists(":C:\Program Files (x86)\Atlantis\Atlantis.exe") then 
84160       let text_editor$='C:\Program Files (x86)\Atlantis\Atlantis.exe'
84180     else if exists(":C:\Program Files\Atlantis\Atlantis.exe") then 
84200       let text_editor$='C:\Program Files\Atlantis\Atlantis.exe'
84220     else 
84240       let text_editor$=os_filename$('R:\Core\Atlantis Nova\Atlantis.exe')
84260     end if 
84280     if ~exists(br_filename$(trim$(text_editor$,'"'))) then 
84300       let text_editor$='"'&os_filename$('R:\Core\Atlantis Nova\Atlantis.exe')&'"'
84320     end if 
84340     let text_editor$=trim$(text_editor$,'"')
84360   fnend 
85000   def library fnget_wordprocessor_exe(&wp_exe$)
85020     if ~setup then let fn_setup
85040     let fnget_wordprocessor_exe=fn_get_atlantis(wp_exe$)
85060   fnend 
86000   def fn_get_atlantis(&wp_exe$)
86020     dim atlantis_path$*256
86040     let fnureg_read('Atlantis Path',atlantis_path$)
86060     if trim$(atlantis_path$)<>'' then 
86080       let wp_exe$=atlantis_path$
86100     else 
86120       dim atl_check_path$(5)*256
86140       let atl_check_path$(1)="C:\Program Files (x86)\Atlantis\Atlantis.exe"
86160       let atl_check_path$(2)="C:\Program Files\Atlantis\Atlantis.exe"
86180 !     atl_check_path$(3)=''&os_filename$('R:\Core\Atlantis Nova\Atlantis.exe')
86200       let atl_check_path$(3)="C:\Program Files (x86)\Atlantis\awp.exe"
86220       let atl_check_path$(4)="C:\Program Files\Atlantis\awp.exe"
86240       let atl_check_path$(5)=env$('local_program_dir')&('\Core\Atlantis Nova\Atlantis.exe')
86260       if atl_check_path$(5)(1:2)='@:' then let atl_check_path$(5)(1:2)=''
86280       let atl_which=fn_first_exists_in_list(mat atl_check_path$)
86300       if atl_which>0 then 
86320         let wp_exe$=atl_check_path$(atl_which)
86340         let fnureg_write('Atlantis Path',wp_exe$)
86360       else 
86380         let wp_exe$='' ! atl_check_path$(3)
86400       end if 
86420     end if 
86440   fnend 
87000   def fn_get_office_word(&office_word_exe$)
87020     dim tmp_folder$(4)*256
87040     let tmp_folder$(1)='C:\Program Files (x86)\Microsoft Office\root\Office16\WINWORD.EXE' !  office 365
87042     let tmp_folder$(2)='c:\program files\microsoft office\office1x\winword.exe' ! ??? found on internet - seemed likely
87044     let tmp_folder$(3)='c:\program files\microsoft office\office11\winword.exe' ! ??? found on internet - seemed likely
87046     let tmp_folder$(4)='C:\Program Files\Microsoft Office\Office10\WINWORD.EXE' ! ??? found on internet - seemed likely
87048     let office_word_exe$=''
87080     let gow_which=fn_first_exists_in_list(mat tmp_folder$)
87100     if gow_which>0 then 
87120       let office_word_exe$=tmp_folder$(gow_which)
87140     end if 
87160     let fn_get_office_word=gow_which
87180   fnend 
88000   def fn_first_exists_in_list(mat feil_list$)
88020     let feil_return=0
88040     let feil_item=0
88060     let feil_count=udim(mat feil_list$)
88080     do until feil_return or feil_item=>feil_count
88100       let feil_item+=1
88120       dim feil_tmp_test$*1024
88140       let feil_tmp_test$=feil_list$(feil_item)
88160       if feil_tmp_test$(1:2)<>'@:' then let feil_tmp_test$(0:0)='@:'
88180       if exists(feil_tmp_test$) then 
88200         let feil_return=feil_item
88220       end if 
88240     loop 
88260     let fn_first_exists_in_list=feil_return
88280   fnend 
89000   def library fndecimal_assumed
89020     if ~setup then let fn_setup
89040     let fndecimal_assumed=fn_decimal_assumed
89060   fnend 
90000   def fn_decimal_assumed
90020     let fnureg_read('Decimal',tmp$)
90040     if tmp$='False' then 
90060       execute 'Config Decimal Required'
90080       let setenv('decimal_assumed','Decimal Required')
90100     else ! if tmp$='True' then ! default
90120       execute 'Config Decimal Assumed'
90140       let setenv('decimal_assumed','Decimal Assumed')
90160     end if 
90180   fnend 
92000   def library fnsave_as_path$*256
92020     if ~setup then let fn_setup
92040     let fnsave_as_path$=fn_save_as_path$
92060   fnend 
94000   def fn_save_as_path$*256
94020     dim sap_return$*256
94040     let fnureg_read('Save As Path',sap_return$)
94060     if sap_return$(1:2)='@:' then let sap_return$(1:2)=''
94080     if sap_return$='' or ~exists(env$('at')&br_filename$(sap_return$)) then 
94100         let sap_return$=os_filename$(env$('userprofile')&'\Desktop')
94120     end if 
94140     let fn_save_as_path$=env$('at')&sap_return$
94160   fnend 
