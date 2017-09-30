20000 ! r: S:\Core\Start.br
20052   def library fnSetQ(setQ$*256)
20054     library 'S:\Core\Start.br': fnSetQ
20056     fnSetQ=fnSetQ(setQ$)
20058   fnend 
20062   def library fngethandle
20064     library 'S:\Core\Start.br': fngethandle 
20066     fngethandle=fngethandle
20068   fnend 
20072   def library fnMapToVirturalDrive(path_to_map$*256,drive_id$*2)
20074     library 'S:\Core\start.br': fnMapToVirturalDrive
20076     fnMapToVirturalDrive=fnMapToVirturalDrive(path_to_map$,drive_id$)
20078   fnend 
20082   def library fnAcsSystemInitialize(; isScreenIOtest)
20084     library 'S:\Core\start.br': fnAcsSystemInitialize
20086     fnAcsSystemInitialize=fnAcsSystemInitialize( isScreenIOtest)
20088   fnend 
20100   def library fnrights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
20120     library 'S:\Core\start.br': fnrights_test
20140     fnrights_test=fnrights_test(rt_folder$,rt_how_to_fix$,folder_name$, additional_text_for_failure$)
20160   fnend 
20180   def library fnSpoolPath$*256(; initialize)
20200     library 'S:\Core\start.br': fnSpoolPath$
20220     fnSpoolPath$=fnSpoolPath$(initialize)
20240   fnend 
20260 ! /r
32000 ! r: ScreenIO
32002 def library fnfm(screenname$; keyval$*255,srow,scol,parent_key$*255,parent_window,display_only,dontRedoListView,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting,savedontask)
32004   library 'S:\Core\ScreenIO\screenio.br': fnfm
32006   fnfm=fnfm(screenname$, keyval$,srow,scol,parent_key$,parent_window,display_only,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$,selecting,savedontask)
32008 fnend
32012 def library fnfm$(screenname$; keyval$*255,srow,scol,parent_key$*255,parent_window,display_only,dontRedoListView,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting,savedontask)
32014   library 'S:\Core\ScreenIO\screenio.br': fnfm$
32016   fnfm$=fnfm$(screenname$, keyval$,srow,scol,parent_key$,parent_window,display_only,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$,selecting,savedontask)
32018 fnend
32100 ! /r
33000 ! r: FileIO
33010 def library fnopenfile(filename$*64,mat f$,mat fn,mat form$; inputonly,keynum,dont_sort_subs,&path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$*255,suppresslog)
33011 ! fnopenfile(&filename$,mat f$,mat fn,mat form$;inputonly,keynum,dont_sort_subs,&path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$*255,suppresslog)
33013   library 'S:\Core\FileIO\fileio.br': fnopenfile
33014   fnopenfile=fnopenfile(filename$,mat f$,mat fn,mat form$, inputonly,keynum,dont_sort_subs,path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$,suppresslog)
33016 fnend
33020 def library fnMakeSurePathExists(Filename$*255; Path$*255) ! mkdir funciton from fileio.brs - except fileio version is not a library 2/7/2017
33022   library 'S:\Core\fnMakeSurePathExists.br':fnMakeSurePathExists
33024   fnMakeSurePathExists=fnMakeSurePathExists(Filename$, Path$) 
33026 fnend
33032  def library fnBuildKey$*255(layout$*30,mat bkf$,mat bkf; keynum)
33034    library 'S:\Core\FileIO\fileio.br': fnBuildKey$
33036    fnBuildKey$=fnBuildKey$(layout$,mat bkf$,mat bkf, keynum)
33038  fnend
33042  def library fnCloseFile(filenumber, filelay$*255; path$*255,out)
33044    library 'S:\Core\FileIO\fileio.br': fnCloseFile
33046    fnCloseFile=fnCloseFile(filenumber,filelay$, path$,out)
33048  fnend
33052  def library fnClearLayoutCache
33054    library 'S:\Core\FileIO\fileio.br': fnClearLayoutCache
33056    fnClearLayoutCache=fnClearLayoutCache
33058  fnend
36990 ! /r
38000 def library Fn_Encodebase64(&Content$)
38020   library 'S:\Core\base64_l.br': Fn_Encodebase64
38040   Fn_Encodebase64=Fn_Encodebase64(Content$)
38060 fnend 
38080 def library Fn_Decodebase64(&Content$)
38100   library 'S:\Core\base64_l.br': Fn_Decodebase64
38120   Fn_Decodebase64=Fn_Decodebase64(Content$)
38140 fnend 
40000 def library fnkey_change(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
40010   library 'S:\Core\fnkey_change.br': fnkey_change
40020   fnkey_change=fnkey_change(h_filehandle,f_fileform_key_only$,key_from$,key_to$)
40030 fnend 
40080 def library fndate_picker$ (;_date$,format$,row,column,___, window,days_in_week,gridspec$*255,usermonth,save_date$*8,baseyear)
40090   library 'S:\Core\date_picker.br': fndate_picker$
40100   fndate_picker$=fndate_picker$ ( _date$,format$,row,column,___, window,days_in_week,gridspec$,usermonth,save_date$,baseyear)
40110 fnend 
40160 def library fnfkey(scrline,mat fkey$,mat disfk,&em$,es)
40170   library 'S:\Core\ace\win3b.br': fnfkey
40180   fnfkey(scrline,mat fkey$,mat disfk,em$,es,0)
40190 fnend 
40200 def library fnwin3(win,&cap$,wh,ww,dc,bo,win_align)
40210   library 'S:\Core\ace\Win3b.br': fnwin3b
40220   fnwin3b(win, cap$,wh,ww,dc,bo,win_align,0)
40230 fnend 
40240 def library fnopenwin(win,sr,sc,er,ec,&cap$)
40250   library 'S:\Core\OpenWin.br': fnopenwin
40260   fnopenwin(win,sr,sc,er,ec, cap$)
40270 fnend 
40280 def library fnCopy(from$*256,to$*256; new_record_length,options$*256)
40290   library 'S:\Core\copy.br': fnCopy
40300   fnCopy=fnCopy(from$,to$, new_record_length,options$)
40310 fnend 
40320 def library fnFree(fileToDelete$*256)
40330   library 'S:\Core\copy.br': fnFree
40340   fnFree=fnFree(fileToDelete$)
40350 fnend 
40360 def library fnRename(from$*256,to$*256)
40370   library 'S:\Core\copy.br': fnRename
40380   fnRename=fnRename(from$,to$)
40390 fnend 
41000 ! r: fnSnap
41020 def library fnlistprint(winno,spec$*100;header$*200,footer$*200,title$*200,mat selected,nolines,nosort,nototals$*200,nosubtotal,_print)
41040   library 'S:\Core\fnsnap\rtflib_dll.br': fnlistprint
41060   fnlistprint=fnlistprint(winno,spec$, header$,footer$,title$,mat selected,nolines,nosort,nototals$,nosubtotal,_print)
41080 fnend
41100 def library fnMsExe$*100(l$)
41120   library 'S:\Core\fnSnap\fnMsExe.br': fnMsExe$
41140   fnMsExe$=fnMsExe$(l$)
41160 fnend
41180 ! /r
42000 ! r: Client
42031   def library fnClientSelect
42032     library 'S:\Core\Client.br': fnClientSelect
42033     fnClientSelect=fnClientSelect
42034   fnend 
42041   def library fnsystem_code_standardize$(st_code$*2)
42042     library 'S:\Core\Client.br': fnsystem_code_standardize$
42043     fnsystem_code_standardize$=fnsystem_code_standardize$(st_code$)
42044   fnend 
42050   def library fnacs_version$
42060     library 'S:\Core\Client.br': fnacs_version$
42070     fnacs_version$=fnacs_version$
42080   fnend 
42090   def library fnclient$
42100     library 'S:\Core\Client.br': fnclient$
42110     fnclient$=fnclient$
42120   fnend 
42130   def library fnclient_has_mat(mat c_has$)
42140     library 'S:\Core\Client.br': fnclient_has_mat
42150     fnclient_has_mat=fnclient_has_mat(mat c_has$)
42160   fnend 
42170   def library fnclient_has(ch_sys$*2)
42180     library 'S:\Core\Client.br': fnclient_has
42190     fnclient_has=fnclient_has(ch_sys$)
42200   fnend 
42210   def library fnregistered_for_hh
42220     library 'S:\Core\Client.br': fnregistered_for_hh
42230     fnregistered_for_hh=fnregistered_for_hh
42240   fnend 
42250   def library fnregistered_for_job_cost_pr
42260     library 'S:\Core\Client.br': fnregistered_for_job_cost_pr
42270     fnregistered_for_job_cost_pr=fnregistered_for_job_cost_pr
42280   fnend 
42290   def library fnub_printbill_program$*256
42300     library 'S:\Core\Client.br': fnub_printbill_program$
42310     fnub_printbill_program$=fnub_printbill_program$
42320   fnend 
42370   def library fnpayroll_client_state$
42380     library 'S:\Core\Client.br': fnpayroll_client_state$
42390     fnpayroll_client_state$=fnpayroll_client_state$
42400   fnend 
42410   def library fnclient_is_converting
42420     library 'S:\Core\Client.br': fnclient_is_converting
42430     fnclient_is_converting=fnclient_is_converting
42440   fnend 
42450   def library fnclient_has_on_support_item(chosi_item$*2; grace_days)
42460     library 'S:\Core\Client.br': fnclient_has_on_support_item
42470     fnclient_has_on_support_item=fnclient_has_on_support_item(chosi_item$, grace_days)
42480   fnend 
42490   def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
42500     library 'S:\Core\Client.br': fnclient_has_on_support_list
42510     fnclient_has_on_support_list=fnclient_has_on_support_list(mat chosl_list$, chosl_grace_days)
42520   fnend 
42530   def library fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
42540     library 'S:\Core\Client.br': fnclient_support
42550     fnclient_support=fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
42560   fnend 
42990 ! /r
43000 ! r: core W-2, W-3, 1099 stuff
43020   def library fnask_w2_info(&taxYear$,&beg_date,&end_date,&empStart$,&empEnd$,&ssrate,&ssmax,&mcrate,&mcmax,mat w2destinationOpt$,&enableW3$,&enableBackground$,&w2Copy,&w2Copy$,&exportFormatID,&w2laser_output_filename$,&pn1,&dc1,&topmargin,&bottom,&state$,enableAskCLocality,&cLocality$)
43040     library 'S:\Core\Print\w2.br': fnask_w2_info
43060     fnask_w2_info=fnask_w2_info(taxYear$,beg_date,end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,state$,enableAskCLocality,cLocality$)
43080   fnend 
43100   def library fnFormCopyAwithBackgroundWarn
43120     library 'S:\Core\Print\w2.br': fnFormCopyAwithBackgroundWarn
43140     fnFormCopyAwithBackgroundWarn=fnFormCopyAwithBackgroundWarn
43160   fnend
43180   def library fnw2_text(w2Yoffset,maskSsn,mat a$,empId$*12,ss$,controlNumber$,mat w,dcb$,nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$*6)
43200     library 'S:\Core\Print\w2.br': fnw2_text
43220     fnw2_text=fnw2_text(w2Yoffset,maskSsn,mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$)
43240   fnend 
43260   def library fnw3(b$,mat a$,mat w,dcb,state$,stcode$)
43280     library 'S:\Core\Print\w3.br': fnw3
43300     fnw3=fnw3(b$,mat a$,mat w,dcb,state$,stcode$)
43320   fnend 
43340   def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
43360     library 'S:\Core\Print\w2.br': fnNameParse
43380     fnNameParse=fnNameParse(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
43400   fnend
43420   def library fn_FormCopyAwithBackgroundWarn
43440     library 'S:\Core\Print\w2.br': fn_FormCopyAwithBackgroundWarn
43460   fn_FormCopyAwithBackgroundWarn=fn_FormCopyAwithBackgroundWarn
43480   fnend
43500   def library fn1099print(vn$*8,nam$*30,mat ad$,ss$*11,mat box)
43520     library 'S:\Core\Programs\1099.br': fn1099print
43540     fn1099print=fn1099print(vn$,nam$,mat ad$,ss$,mat box)
43560   fnend 
43580   def library fn1099print_close
43600     library 'S:\Core\Programs\1099.br': fn1099print_close
43620     fn1099print_close=fn1099print_close
43640   fnend
43660   def library fnask_1099_info(&seltp,&type,&min1,&beg_date,&end_date)
43680     library 'S:\Core\Programs\1099.br': fnask_1099_info
43700     fnask_1099_info=fnask_1099_info(seltp,type,min1,beg_date,end_date)
43720   fnend
43999 ! /r
44000 ! r: Favorites
44002   def library fnFavoriteAdd(programCaption$*256)
44004     library 'S:\Core\Favorites.br': fnFavoriteAdd
44006     fnFavoriteAdd=fnFavoriteAdd(programCaption$)
44008   fnend 
44010   def library fnFavoriteDel(programCaption$*256)
44012     library 'S:\Core\Favorites.br': fnFavoriteDel
44014     fnFavoriteDel=fnFavoriteDel(programCaption$)
44016   fnend 
44020   def library fnFavoriteList(mat favorite$)
44022     library 'S:\Core\Favorites.br': fnFavoriteList
44024     fnFavoriteList=fnFavoriteList(mat favorite$)
44026   fnend 
44098 ! /r
44100 ! r: File Open and Save
44102   def library fnOpenPartial
44104     library 'S:\Core\File Open and Save.br': fnOpenPartial
44106     fnOpenPartial=fnOpenPartial
44108   fnend 
44110   def library fnFileSaveAs(save_what$)
44112     library 'S:\Core\File Open and Save.br': fnFileSaveAs
44114     fnFileSaveAs=fnFileSaveAs(save_what$)
44116   fnend 
44120   def library fnFileOpen
44122     library 'S:\Core\File Open and Save.br': fnFileOpen
44124     fnFileOpen=fnFileOpen
44126   fnend 
44130   def library fnAutomatedSavePoint(fileNameAddition$*128)
44132     library 'S:\Core\File Open and Save.br': fnAutomatedSavePoint
44134     fnAutomatedSavePoint=fnAutomatedSavePoint(fileNameAddition$)
44136   fnend 
44198 ! /r
45000 ! r: core   all the libraries that aren't filed anywhere else 
45010   def library fnWindowsStart(wsFile$*1024)
45020     library 'S:\Core\fnWindowsStart.br': fnWindowsStart
45030     fnWindowsStart=fnWindowsStart(wsFile$)
45040   fnend 
45050   def library fnSystemName$*40(; systemAbbreviation$*2)
45060     library 'S:\Core\CNo.br': fnSystemName$
45070     fnSystemName$=fnSystemName$( systemAbbreviation$)
45080   fnend 
45090   def library fncheckcompiled
45100     library 'S:\Core\checkcompiled.br': fncheckcompiled
45110     fncheckcompiled=fncheckcompiled
45120   fnend 
45130   def library fnbooktitle$*256(x$*256)
45140     library 'S:\Core\booktitle.br': fnbooktitle$
45150     fnbooktitle$=fnbooktitle$(x$)
45160   fnend 
45170   def library fnsave_as_path$*256
45180     library 'S:\Core\Programs\Preferences.br': fnsave_as_path$
45190     fnsave_as_path$=fnsave_as_path$
45200   fnend 
45210   def library fndecimal_assumed
45220     library 'S:\Core\Programs\Preferences.br': fndecimal_assumed
45230     fndecimal_assumed=fndecimal_assumed
45240   fnend 
45250   def library fnget_wordprocessor_exe(&wp_exe$; force$)
45260     library 'S:\Core\Programs\Preferences.br': fnget_wordprocessor_exe
45270     fnget_wordprocessor_exe=fnget_wordprocessor_exe(wp_exe$, force$)
45280   fnend 
45290   def library fnget_atlantis(&atlantis$)
45300     library 'S:\Core\Programs\Preferences.br': fnget_atlantis
45310     fnget_atlantis=fnget_atlantis(atlantis$)
45320   fnend 
45330   def library fntext_editor(te_text_file$*256; te_options$)
45340     library 'S:\Core\Programs\Preferences.br': fntext_editor
45350     fntext_editor=fntext_editor(te_text_file$, te_options$)
45360   fnend 
45410   def library fnapply_theme(; disableConScreenOpenDflt)
45420     library 'S:\Core\Programs\Preferences.br': fnapply_theme
45430     fnapply_theme=fnapply_theme( disableConScreenOpenDflt)
45440   fnend 
45450   def library fnprogram_properties(; forceProgramCaption$*256)
45460     library 'S:\Core\program_properties.br': fnprogram_properties
45470     fnprogram_properties=fnprogram_properties( forceProgramCaption$)
45480   fnend 
45490   def library fnprogram_ini_filename$*256(a$*256; doNotCreate)
45500     library 'S:\Core\Program_Properties.br': fnprogram_ini_filename$
45510     fnprogram_ini_filename$=fnprogram_ini_filename$(a$, doNotCreate)
45520   fnend 
45890   def library fncd(x)
45900     library 'S:\Core\fncd.br': fncd
45910     fncd=fncd(x)
45920   fnend 
45930   def library fnindex_it(data_file$*256,index_statement$*512; index_parameters$*256)
45940     library 'S:\Core\Index.br': fnindex_it
45950     fnindex_it=fnindex_it(data_file$,index_statement$, index_parameters$)
45960   fnend 
45970   def library fnindex_sys(; only_cno,system_id$*2)
45980     library 'S:\Core\Index.br': fnindex_sys
45990     fnindex_sys=fnindex_sys( only_cno,system_id$)
46000   fnend 
46020   def library fnub_index_customer
46040     library 'S:\Core\Index.br': fnub_index_customer
46060     fnub_index_customer=fnub_index_customer
46080   fnend 
46100   def library fnAcsInstallationPath$*256(; longFileName)
46120     library 'S:\Core\Programs\Update.br': fnAcsInstallationPath$
46140     fnAcsInstallationPath$=fnAcsInstallationPath$( longFileName)
46160   fnend 
46180   def library fnstatus(text$*512)
46200     library 'S:\Core\Programs\Update.br': fnstatus
46220     fnstatus=fnstatus(text$)
46240   fnend 
46260   def library fnstatus_pause
46280     library 'S:\Core\Programs\Update.br': fnstatus_pause
46300     fnstatus_pause=fnstatus_pause
46320   fnend 
46340   def library fnstatus_close
46360     library 'S:\Core\Programs\Update.br': fnstatus_close
46380     fnstatus_close=fnstatus_close
46400   fnend 
46420   def library fnqgl(myline,mypos; con,x,use_or_replace,qgllength)
46440     library 'S:\Core\ACS_Component.br': fnqgl
46460     fnqgl=fnqgl(myline,mypos,con,x,use_or_replace,qgllength)
46480   fnend 
46500   def library fnqglbig(myline,mypos; con,x,use_or_replace)
46520     ! library 'S:\Core\ACS_Component.br': fnqglbig
46540     ! fnqglbig(myline,mypos,con,x,use_or_replace)
46560     library 'S:\Core\ACS_Component.br': fnqgl
46580     fnqglbig=fnqgl(myline,mypos,con,x,use_or_replace,60)
46600   fnend 
46620   def library fnqgl25(myline,mypos; con,x,use_or_replace)
46640     ! library 'S:\Core\ACS_Component.br': fnqgl25
46660     ! fnqgl25=fnqgl25(myline,mypos,con,x,use_or_replace)
46680     library 'S:\Core\ACS_Component.br': fnqgl
46700     fnqgl=fnqgl(myline,mypos,con,x,use_or_replace,25)
46720   fnend 
46740   def library fnagl$*12(&x$)
46760     library 'S:\Core\fnAGL$.br': fnagl$
46780     fnagl$=fnagl$(x$)
46800   fnend 
46820   def library fnrgl$*60(x$; returnmaxlength)
46840     library 'S:\Core\fnRGL$.br': fnrgl$
46860     fnrgl$=fnrgl$(x$, returnmaxlength)
46880   fnend 
46900   def library fnrglbig$*60(x$)
46920     ! library 'S:\Core\fnRGLbig$.br': fnrglbig$
46940     ! fnrglbig$=fnrglbig$(x$)
46960     library 'S:\Core\fnRGL$.br': fnrgl$
46980     fnrglbig$=fnrgl$(x$, 60)
47000   fnend 
47020   def library fnsetmonth(mat mo$)
47040     library 'S:\Core\SetMonth.br': fnsetmonth
47060     fnsetmonth=fnsetmonth(mat mo$)
47080   fnend 
47100   def library fnosver(&osver$;get_or_put)
47120     library 'S:\Core\OSVer.br': fnosver
47140     fnosver=fnosver(osver$,get_or_put)
47160   fnend 
47180   def library fndec2hex(input_dec,&output_hex$)
47200     library 'S:\Core\Dec2Hex.br': fndec2hex
47220     fndec2hex=fndec2hex(input_dec, output_hex$)
47240   fnend 
47260   def library fnhex2dec(input_hex$,&output_dec)
47280     library 'S:\Core\hex2dec.br': fnhex2dec
47300     fnhex2dec=fnhex2dec(input_hex$, output_dec)
47320   fnend 
47340   def library fnwin3b(win,&cap$,win_height,win_width; display_cnam,button_option,win_align,pr_newpg)
47360     library 'S:\Core\Ace\Win3B.br': fnwin3b
47380     fnwin3b=fnwin3b(win,cap$,win_height,win_width, display_cnam,button_option,win_align,pr_newpg)
47400   fnend 
47420   def library fngetcd(&mcd$)
47440     library 'S:\Core\Ace\GetCD.br': fngetcd
47460     fngetcd=fngetcd(mcd$)
47480   fnend 
47500   def library fndate_mmddyy_to_ccyymmdd(x)
47520     library 'S:\Core\2000.br': fndate_mmddyy_to_ccyymmdd
47540     fndate_mmddyy_to_ccyymmdd=fndate_mmddyy_to_ccyymmdd(x)
47560   fnend 
47580   def library fnxit(;cursys$)
47600     library 'S:\Core\xit.br': fnxit
47620     fnxit=fnxit(cursys$)
47640   fnend 
47660   def library fninch2twip(&x)
47680     library 'S:\Core\Inch2Twip.br': fninch2twip
47700     fninch2twip=fninch2twip(x)
47720   fnend 
47740   def library fntwip2inch(&x)
47760     library 'S:\Core\Twip2Inch.br': fntwip2inch
47780     fntwip2inch=fntwip2inch(x)
47800   fnend 
47820   def library fnchain(prg$*255; no_fnprg_setting,noLog)
47840     library 'S:\Core\Chain.br': fnchain
47860     fnchain=fnchain(prg$, no_fnprg_setting,noLog)
47880   fnend 
47900   def library fnerror(callingProgram$*256,errornumber,linenumber,&act$,stopable$)
47920     library 'S:\Core\fnerror': fnerror
47940     fnerror=fnerror(callingProgram$,errornumber,linenumber,act$,stopable$)
47960   fnend 
47980   def library fnlog(log$*190;x)
48000     library 'S:\Core\Log.br': fnlog
48020     fnlog=fnlog(log$,x)
48040   fnend 
48060   def library fngetdir(&dir$,mat filename$;option$,filter$*40)
48080     library 'S:\Core\GetDir.br': fngetdir
48100     fngetdir=fngetdir(dir$,mat filename$,option$,filter$)
48120   fnend 
48140   def library fngetdir2(dir$*256,mat filename$; option$,filter$*40,mat fileDate$,mat fileTime$,forceFullPath,mat fileSize)
48160     library 'S:\Core\GetDir2.br': fngetdir2
48180     fngetdir2=fngetdir2(dir$,mat filename$, option$,filter$,mat fileDate$,mat fileTime$,forceFullPath,mat fileSize)
48200   fnend 
48220   ! 
48240   def library fnwait(; win,&cap$,message$*40,stopable)
48260     library 'S:\Core\Wait.br': fnwait
48280     fnwait(win,cap$,message$,stopable)
48300   fnend 
48320   def library fnadd1099(mat cinfo$, mat einfo$, mat box)
48340     library 'S:\Core\Print1099.br': fnadd1099
48360     fnadd1099(mat cinfo$, mat einfo$, mat box)
48380   fnend 
48400   def library fnprint1099(; lz1$)
48420     library 'S:\Core\Print1099.br': fnprint1099
48440     fnprint1099(lz1$)
48460   fnend 
48480   def library fncheckfileversion
48500     library 'S:\Core\Check File Versions.br': fncheckfileversion
48520     fncheckfileversion=fncheckfileversion
48540   fnend 
48560   def library fndemo
48580     library 'S:\Core\Demo.br': fndemo
48600     fndemo=fndemo
48620   fnend 
48640   def library fnconsole(;on_off)
48660     library 'S:\Core\Ace\Console.br': fnconsole
48680     fnconsole(on_off)
48700   fnend 
48720   def library fncmbcno(myline,mypos; mysys$)
48740     library 'S:\Core\CmbCNo.br': fncmbcno
48760     fncmbcno(myline,mypos,mysys$)
48780   fnend 
48800   def library fnpause(;unused)
48820     library 'S:\Core\Pause.br': fnpause
48840     fnpause(unused) : fnend 
48860   def library fnformnumb$(numb,decimals,size)
48880     library 'S:\Core\FormNumb.br': fnformnumb$
48900     fnformnumb$=fnformnumb$(numb,decimals,size)
48920   fnend 
48940   def library fncombo1(win,n$,mat l$,mat s,mat f,mat c$,filter$)
48960     library 'S:\Core\combo1.br': fncombo1
48980     fncombo1(win,n$,mat l$,mat s,mat f, mat c$,filter$)
49000   fnend 
49020   def library fnselect(win,&cap$,&q$,mat o$,&r$,d_cnam,bo,wa,pr_newpg)
49040     library 'S:\Core\Select.br': fnselect
49060     fnselect=fnselect(win,cap$,q$,mat o$,r$,d_cnam,bo,wa,pr_newpg)
49080   fnend 
49100   def library fnprocess(; chgpro)
49120     library 'S:\Core\Process.br': fnprocess
49140     fnprocess=fnprocess(chgpro)
49160   fnend 
49180   def library fnkillauto
49200     library 'S:\Core\KillAuto.br': fnkillauto
49220     fnkillauto=fnkillauto
49240   fnend 
49260   def library fnoldmsgbox(mat response$,&cap$,mat msgline$,mtype)
49280     library 'S:\Core\OldMsgBox.br': fnoldmsgbox
49300     fnoldmsgbox=fnoldmsgbox(mat response$,cap$,mat msgline$,mtype)
49320   fnend 
49340   def library fnsearch(&cap$,fum,&hea$,&form$,nformat$,&sel$,klength)
49360     library 'S:\Core\Search.br': fnsearch
49380     fnsearch=fnsearch(cap$,fum,hea$,form$,nformat$,sel$,klength)
49400   fnend 
49411   def library fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
49412     library 'S:\Core\Menu.br': fnGetProgramList
49413     fnGetProgramList=fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
49414   fnend 
49580 ! /r
50000 ! r: cno   S:\Core\CNo\   -   use cno or similar
50020   def library fncno(&cno;&cnam$)
50040     library 'S:\Core\CNo.br': fncno
50060     fncno=fncno(cno,cnam$)
50080   fnend 
50100   def library fnget_company_number_list(mat cno_list; sysid$*2)
50120     library 'S:\Core\CNo.br': fnget_company_number_list
50140     fnget_company_number_list=fnget_company_number_list(mat cno_list, sysid$)
50160   fnend 
50180   def library fnCnoLegacyNtoCReg(legacyFilename$*256,legacyForm$*64,registryKey$*128; valuePassedIn)
50200     library 'S:\Core\CNo.br': fnCnoLegacyNtoCReg
50220     fnCnoLegacyNtoCReg=fnCnoLegacyNtoCReg(legacyFilename$,legacyForm$,registryKey$, valuePassedIn)
50240   fnend 
50260   def library fnpgnum(;pgnum)
50280     library 'S:\Core\CNo.br': fnpgnum
50300     fnpgnum=fnpgnum(pgnum)
50320   fnend 
50340   def library fnrx(;rx)
50360     library 'S:\Core\CNo.br': fnrx
50380     fnrx=fnrx(rx)
50400   fnend 
50420   def library fnstyp(;styp)
50440     library 'S:\Core\CNo.br': fnstyp
50460     fnstyp=fnstyp(styp)
50480   fnend 
50500   def library fnps(;ps)
50520     library 'S:\Core\CNo.br': fnps
50540     fnps=fnps(ps)
50560   fnend 
50580   def library fnfscode(;a)
50600     library 'S:\Core\CNo.br': fnfscode
50620     fnfscode=fnfscode(a)
50640   fnend 
50660   def library fnpedat$*20(;a$*20)
50680     library 'S:\Core\CNo.br': fnpedat$
50700     fnpedat$=fnpedat$(a$)
50720   fnend 
50740   def library fnpriorcd(;a)
50760     library 'S:\Core\CNo.br': fnpriorcd
50780     fnpriorcd=fnpriorcd(a)
50800   fnend 
50820   def library fnputcno(cno)
50840     library 'S:\Core\CNo.br': fnputcno
50860     fnputcno=fnputcno(cno)
50880   fnend 
50900   def library fndat(&dat$; get_or_put)
50920     library 'S:\Core\CNo.br': fndat
50940     fndat=fndat(dat$,get_or_put)
50960   fnend 
50980   def library fncursys$(; cs$*2)
51000     library 'S:\Core\CNo.br': fncursys$
51020     fncursys$=fncursys$(cs$)
51040   fnend 
51060   def library fnprg(&prg$; g_p)
51080     library 'S:\Core\CNo.br': fnprg
51100     fnprg=fnprg(prg$,g_p)
51120   fnend 
51140 ! /r
52000 ! r: parse   S:\Core\parse\
52010   def library fnremove(and$,&word$)
52020     library 'S:\Core\parse\remove.br': fnremove
52030     fnremove=fnremove(and$,word$)
52040   fnend 
52050   def library fnremove2(&and$,&word$)
52060     library 'S:\Core\parse\remove2.br': fnremove2
52070     fnremove2=fnremove2(and$,word$)
52080   fnend 
52090   def library fncsz(&csz$,&city$,&state$,&zip$)
52100     library 'S:\Core\Parse\csz.br': fncsz
52110     fncsz=fncsz(csz$,city$,state$,zip$)
52120   fnend 
52130   def library fngetpp(&input$,&path$,&prog$,&ext$)
52140     library 'S:\Core\Parse\GetPP.br': fngetpp
52150     fngetpp=fngetpp(input$,path$,prog$,ext$)
52160   fnend 
52170 ! /r
54000 ! r: label   S:\Core\label\
54010   def library fnlabel(win,&cap$,mat linestyle$,cp,nw)
54020     library 'S:\Core\Label\fnLabel.br': fnlabel
54030     fnlabel=fnlabel(win,cap$,mat linestyle$,cp,nw)
54040   fnend 
54050   def library fnaddlabel(mat in_labeltext$)
54060     library 'S:\Core\Label\fnAddLabel.br': fnaddlabel
54070     fnaddlabel=fnaddlabel(mat in_labeltext$)
54080   fnend 
54090 ! /r
56000 ! r: PrintAce   S:\Core\printAce
56001   def library fnpa_background(background_pdf$*256)
56002     library 'S:\Core\PrintAce.br': fnpa_background
56003     fnpa_background=fnpa_background(background_pdf$)
56004   fnend       
56010   def library fnpa_line(pl_left_pos,pl_top_pos,pl_width; pl_height,pl_line_instead_of_box,h_printace)
56020     library 'S:\Core\PrintAce.br': fnpa_line
56030     fnpa_line=fnpa_line(pl_left_pos,pl_top_pos,pl_width, pl_height,pl_line_instead_of_box,h_printace)
56040   fnend 
56050   def library fnpa_elipse(pe_a,pe_b,pe_c,pe_d; h_printace)
56060     library 'S:\Core\PrintAce.br': fnpa_elipse
56070     fnpa_elipse=fnpa_elipse(pe_a,pe_b,pe_c,pe_d, h_printace)
56080   fnend 
56090   def library fnpa_pic(pp_pic$*1024,pp_x,pp_y; imgWidth,imgHeight,style$)
56100     library 'S:\Core\PrintAce.br': fnpa_pic
56110     fnpa_pic=fnpa_pic(pp_pic$,pp_x,pp_y, imgWidth,imgHeight,style$)
56120   fnend 
56130   def library fnpa_text(pt_h,pt_text$*128,pt_x,pt_y)
56140     library 'S:\Core\PrintAce.br': fnpa_text
56150     fnpa_text=fnpa_text(pt_h,pt_text$,pt_x,pt_y)
56160   fnend 
56170   def library fnpa_txt(pt_text$*128,pt_x; pt_y,pt_h)
56180     library 'S:\Core\PrintAce.br': fnpa_txt
56190     fnpa_txt=fnpa_txt(pt_text$,pt_x,pt_y, pt_h)
56200   fnend 
56210   def library fnpa_barcode(pb_a,pb_b,pb_bc$*256; h_printace)
56220     library 'S:\Core\PrintAce.br': fnpa_barcode
56230     fnpa_barcode=fnpa_barcode(pb_a,pb_b,pb_bc$, h_printace)
56240   fnend 
56250   def library fnpa_finis(; pt_h)
56260     library 'S:\Core\PrintAce.br': fnpa_finis
56270     fnpa_finis=fnpa_finis(pt_h)
56280   fnend  ! fn_pa_finis
56290   def library fnpa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$)
56300     library 'S:\Core\PrintAce.br': fnpa_open
56310     fnpa_open=fnpa_open( pa_orientation$,pa_sendto_base_name_addition$,formsFormatForce$)
56320   fnend 
56330   def library fnpa_fontsize(; pfs_fontsize,h_printace)
56340     library 'S:\Core\PrintAce.br': fnpa_fontsize
56350     fnpa_fontsize=fnpa_fontsize( pfs_fontsize,h_printace)
56360   fnend 
56370   def library fnpa_font(; pf_fontname$*256,h_printace)
56380     library 'S:\Core\PrintAce.br': fnpa_font
56390     fnpa_font=fnpa_font( pf_fontname$,h_printace)
56400   fnend 
56410   def library fnpa_fontbold(; pfb_off_or_on)
56420     library 'S:\Core\PrintAce.br': fnpa_fontbold
56430     fnpa_fontbold=fnpa_fontbold( pfb_off_or_on)
56440   fnend 
56441   def library fnpa_fontitalic(; pfb_off_or_on)
56442     library 'S:\Core\PrintAce.br': fnpa_fontitalic
56443     fnpa_fontitalic=fnpa_fontitalic( pfb_off_or_on)
56444   fnend 
56450   def library fnpa_newpage(;h_printace)
56460     library 'S:\Core\PrintAce.br': fnpa_newpage
56470     fnpa_newpage=fnpa_newpage( h_printace)
56480   fnend 
56490   def library fnbarcode(barcode$,rightleft,updown)
56500     library 'S:\Core\PrintAce.br': fnbarcode
56510     fnbarcode=fnbarcode(barcode$,rightleft,updown)
56520   fnend 
56530   def library fnbarcodewide(barcode$,rightleft,updown)
56540     library 'S:\Core\PrintAce.br': fnbarcodewide
56550     fnbarcodewide=fnbarcodewide(barcode$,rightleft,updown)
56560   fnend 
56570 ! /r
58000 ! r: print   S:\Core\Print.br and S:\Core\Print\*
58012   def library fnsafe_filename$*256(sf_in$*256)
58014     library 'S:\Core\Print.br': fnsafe_filename$
58016     fnsafe_filename$=fnsafe_filename$(sf_in$)
58018   fnend 
58020   def library fnprint_file_name$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
58040     library 'S:\Core\Print.br': fnprint_file_name$
58060     fnprint_file_name$=fnprint_file_name$( pfn_sendto_base_name_addition$,pfn_extension$,programCaptionOverride$)
58080   fnend 
58100   def library fnreport_cache_folder_current$*512
58120     library 'S:\Core\Print.br': fnreport_cache_folder_current$
58140     fnreport_cache_folder_current$=fnreport_cache_folder_current$
58160   fnend 
58180   def library fnopenprn(; xx,xxxxx,xxxxxxx,process,sendto_base_name_addition$*128,programNameOverride$*256,programCaptionOverride$*256)
58200     library 'S:\Core\Print.br': fnopenprn
58220     fnopenprn=fnopenprn( xx,xxxxx,xxxxxxx,process,sendto_base_name_addition$,programNameOverride$,programCaptionOverride$)
58240   fnend 
58260   def library fncloseprn(;forceWordProcessor$)
58280     library 'S:\Core\Print.br': fncloseprn
58300     fncloseprn=fncloseprn(forceWordProcessor$)
58320   fnend 
58420   def library fnpglen(&pglen)
58440     library 'S:\Core\Print\PgLen.br': fnpglen
58460     fnpglen=fnpglen(pglen)
58480   fnend 
58500   def library fnsavetoasstart(a$*400)
58520     library 'S:\Core\Print\SaveTo.br': fnsavetoasstart
58540     fnsavetoasstart=fnsavetoasstart(a$)
58560   fnend 
58580   def library fnopen_receipt_printer(; orp_only_if_it_is_assigned)
58600     library 'S:\Core\Print.br': fnopen_receipt_printer
58620     fnopen_receipt_printer=fnopen_receipt_printer( orp_only_if_it_is_assigned)
58640   fnend 
58660   def library fnclose_receipt_printer
58680     library 'S:\Core\Print.br': fnclose_receipt_printer
58700     fnclose_receipt_printer=fnclose_receipt_printer
58720   fnend 
58740   def library fnopen_cash_drawer
58760     library 'S:\Core\Print.br': fnopen_cash_drawer
58780     fnopen_cash_drawer=fnopen_cash_drawer
58800   fnend 
58820 ! /r
60000 ! r: hamster
60002   def library fnHamsterFio(fileid$*64)
60004     library 'S:\Core\HamsterFio.br': fnHamsterFio
60006     fnHamsterFio=fnHamsterFio(fileid$)
60008   fnend
60010   def library fnhamster(a$*20,mat b$,mat l,c,mat e$; mat f$,mat d,mat g,mat h,mat j$,mat k)
60020     library 'S:\Core\Hamster.br': fnhamster
60030     fnhamster=fnhamster(a$,mat b$,mat l,c,mat e$,mat f$,mat d,mat g,mat h,mat j$,mat k)
60040   fnend 
60050   def library fnhamster_add_combof(hac_screen_item,hac_data_file$*256,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$*256,hac_limit_to_list)
60060     library 'S:\Core\Hamster_Setup.br': fnhamster_add_combof
60070     fnhamster_add_combof=fnhamster_add_combof(hac_screen_item,hac_data_file$,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$,hac_limit_to_list)
60080   fnend 
60090   def library fnhamster_add_comboa(hac_screen_item,mat hac_option$)
60100     library 'S:\Core\Hamster_Setup.br': fnhamster_add_comboa
60110     fnhamster_add_comboa=fnhamster_add_comboa(hac_screen_item,mat hac_option$)
60120   fnend 
60130   def library fnhamster_2(a$*20; h_file)
60140     library 'S:\Core\Hamster_Setup.br': fnhamster_2
60150     fnhamster_2=fnhamster_2(a$, h_file)
60160   fnend 
60170   def library fnhamster_print(a$*20,mat b$,mat l,c,mat e$; mat f$,mat d,mat g,mat h,mat j$,mat k)
60180     library 'S:\Core\Hamster_print.br': fnhamster_print
60190     fnhamster_print=fnhamster_print(a$,mat b$,mat l,c,mat e$,mat f$,mat d,mat g,mat h,mat j$,mat k)
60200   fnend 
60210   def library fnhamster_field_reset
60220     library 'S:\Core\Hamster_Setup.br': fnhamster_field_reset
60230     fnhamster_field_reset=fnhamster_field_reset
60240   fnend 
60250   def library fnhamster_field_add(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
60260     library 'S:\Core\Hamster_Setup.br': fnhamster_field_add
60270     fnhamster_field_add=fnhamster_field_add(label$,textbox_len, field_type$,storage_length,ar_mask,storage_position)
60280   fnend 
60290   def library fnhamster_add_combo(mat c$)
60300     library 'S:\Core\Hamster_Setup.br': fnhamster_add_combo
60310     fnhamster_add_combo=fnhamster_add_combo(mat c$)
60320   fnend 
60330 ! /r
62000 ! r: Screen Ace
62010   def library fntop(;prg$*256,cap$*128)
62020     library 'S:\Core\ACS_Component.br': fntop
62030     fntop=fntop( prg$,cap$)
62040   fnend 
62042   def library fncompany_name(window,win_cols)
62044     library 'S:\Core\ACS_Component.br': fncompany_name
62046     fncompany_name=fncompany_name(window,win_cols)
62048   fnend 
62050   def library fncmdkey(caption$*200,returnkey; default,cancel,tt$*200)
62060     library 'S:\Core\ACS_Component.br': fncmdkey
62070     fncmdkey=fncmdkey(caption$,returnkey, default,cancel,tt$)
62080   fnend 
62090   def library fnflexadd1(mat item$)
62100     library 'S:\Core\ACS_Component.br': fnflexadd1
62110     fnflexadd1=fnflexadd1(mat item$)
62120   fnend 
62130   def library fntos(sn$*100)
62140     library 'S:\Core\ACS_Component.br': fntos
62150     fntos=fntos(sn$)
62160   fnend 
62170   def library fnlbl(myline,mypos,t$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
62180     library 'S:\Core\ACS_Component.br': fnlbl
62190     fnlbl=fnlbl(myline,mypos,t$,mylen,myalign,font_mod,container,tabcon,lbl_tooltip$)
62200   fnend 
62210   def library fntxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
62220     library 'S:\Core\ACS_Component.br': fntxt
62230     fntxt=fntxt(lyne,ps,width, maxlen,ali,mask$,disable,tooltip$,contain,tabcon,addtomask$)
62240   fnend 
62250   def library fnopt(lyne,ps,txt$*196; align,contain,tabcon)
62260     library 'S:\Core\ACS_Component.br': fnopt
62270     fnopt=fnopt(lyne,ps,txt$, align,contain,tabcon)
62280   fnend 
62290   def library fnchk(lyne,ps,txt$*196; align,contain,tabcon,chk_disable)
62300     library 'S:\Core\ACS_Component.br': fnchk
62310     fnchk=fnchk(lyne,ps,txt$, align,contain,tabcon,chk_disable)
62320   fnend 
62330   def library fnflexinit1(sfn$*256,lyne,ps,height,width,mat ch$;mat cm$,seltype,usr,con,tabcon)
62340     library 'S:\Core\ACS_Component.br': fnflexinit1
62350     fnflexinit1=fnflexinit1(sfn$,lyne,ps,height,width,mat ch$,mat cm$,seltype,usr,con,tabcon)
62360   fnend 
62370   def library fncomboa(sfn$*256,lyne,ps,mat opt$;ttt$*200,width,contain,tabcon)
62380     library 'S:\Core\ACS_Component.br': fncomboa
62390     fncomboa=fncomboa(sfn$,lyne,ps,mat opt$, ttt$,width,contain,tabcon)
62400   fnend 
62410   def library fncombof(sfn$*100,lyne,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
62420     library 'S:\Core\ACS_Component.br': fncombof
62430     fncombof=fncombof(sfn$,lyne,ps,width,df$,psk,lnk,psd,lnd, if$,limlis,urep,ttt$,contain,tabcon)
62440   fnend 
62450   def library fnbutton(lyne,ps,txt$*200,comkey; tt$*200,height,width,container,tabcon,default,cancel)
62460     library 'S:\Core\ACS_Component.br': fnbutton
62470     fnbutton=fnbutton(lyne,ps,txt$,comkey, tt$,height,width,container,tabcon,default,cancel)
62480   fnend 
62490   def library fnbutton_or_disabled(enable,lyne,ps,txt$*200,comkey; tt$*200,width,container,tabcon,default,cancel)
62500     library 'S:\Core\fnbutton_or_disabled.br': fnbutton_or_disabled
62510     fnbutton_or_disabled=fnbutton_or_disabled(enable,lyne,ps,txt$,comkey, tt$,width,container,tabcon,default,cancel)
62520   fnend 
62530   def library fnpicbut(lyne,mypos,txt$*40,comkey,pic1$*150,btnh,btnw; pic2$*150,tt$*150,container,tabcon,default,cancel)
62540     library 'S:\Core\ACS_Component.br': fnpicbut
62550     fnpicbut=fnpicbut(lyne,mypos,txt$,comkey,pic1$,btnh,btnw, pic2$,tt$,container,tabcon,default,cancel)
62560   fnend 
62570   def library fndisplay_menu(mat _menu$,mat _program$,mat _status$)
62580     library 'S:\Core\ACS_Component.br': fndisplay_menu
62590     fndisplay_menu=fndisplay_menu(mat _menu$,mat _program$,mat _status$)
62600   fnend 
62610   def library fnclear_menu
62620     library 'S:\Core\ACS_Component.br': fnclear_menu
62630     fnclear_menu=fnclear_menu
62640   fnend 
62650   def library fnacs(sn$*100,win,mat resp$,&ckey; startfield,close_on_exit,parent_none,disabled_background)
62660     library 'S:\Core\ACS_Component.br': fnacs
62670     fnacs=fnacs(sn$,win,mat resp$,ckey, startfield,close_on_exit,parent_none,disabled_background) : fnend  ! fnend should be on the same line as fn call so that f12 program pause will work properly
62680   def library fnpic(lyne,ps,hi,wd,picture$*300; x,y)
62690     library 'S:\Core\ACS_Component.br': fnpic
62700     fnpic=fnpic(lyne,ps,hi,wd,picture$, x,y)
62710   fnend 
62720   def library fnfra(lyne,ps,hi,wd; cap$*128,tooltip$*300,contain,tabcon)
62730     library 'S:\Core\ACS_Component.br': fnfra
62740     fnfra=fnfra(lyne,ps,hi,wd, cap$,tooltip$,contain,tabcon)
62750   fnend 
62760   def library fntab(lyne,mypos,height,width,mat cap$)
62770     library 'S:\Core\ACS_Component.br': fntab
62780     fntab=fntab(lyne,mypos,height,width,mat cap$)
62790   fnend 
62800   def library fnmultiline(lyne,ps,height,width;contain,tabcon,tt$*200)
62810     library 'S:\Core\ACS_Component.br': fnmultiline
62820     fnmultiline=fnmultiline(lyne,ps,height,width, contain,tabcon,tt$)
62830   fnend 
62840   def library fncmdset(a)
62850     library 'S:\Core\ACS_Component.br': fncmdset
62860     fncmdset=fncmdset(a)
62870   fnend 
62880   def library fnmsgbox(mat message$; &response$,cap$*128,mtype)
62890     library 'S:\Core\Ace\MsgBox.br': fnmsgbox
62900     fnmsgbox=fnmsgbox(mat message$, response$,cap$,mtype)
62910   fnend 
62920   def library fnBackgroundDisable(; Activate)
62922     library 'S:\Core\ACS_Component.br': fnBackgroundDisable
62924     fnBackgroundDisable=fnBackgroundDisable( Activate)
62926   fnend 
62990 ! /r
63000 !  r: registry stuff 
63010   def library fnsreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
63020     library 'S:\Core\Reg.br': fnsreg_read
63030     fnsreg_read=fnsreg_read(reg_field_name$,reg_field_value$, reg_field_default$)
63040   fnend 
63050   def library fnsreg_write(reg_field_name$*128,reg_field_value$*256)
63060     library 'S:\Core\Reg.br': fnsreg_write
63070     fnsreg_write=fnsreg_write(reg_field_name$,reg_field_value$)
63080   fnend 
63090   def library fncreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
63100     library 'S:\Core\Reg.br': fncreg_read
63110     fncreg_read=fncreg_read(reg_field_name$,reg_field_value$, reg_field_default$)
63120   fnend 
63130   def library fncreg_write(reg_field_name$*128,reg_field_value$*256)
63140     library 'S:\Core\Reg.br': fncreg_write
63150     fncreg_write=fncreg_write(reg_field_name$,reg_field_value$)
63160   fnend 
63170   def library fnreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
63180     library 'S:\Core\Reg.br': fnreg_read
63190     fnreg_read=fnreg_read(reg_field_name$,reg_field_value$, reg_field_default$)
63200   fnend 
63210   def library fnreg_write(reg_field_name$*128,reg_field_value$*256)
63220     library 'S:\Core\Reg.br': fnreg_write
63230     fnreg_write=fnreg_write(reg_field_name$,reg_field_value$)
63240   fnend 
63250   def library fnureg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
63260     library 'S:\Core\Reg.br': fnureg_read
63270     fnureg_read=fnureg_read(reg_field_name$,reg_field_value$, reg_field_default$)
63280   fnend 
63290   def library fnureg_write(reg_field_name$*128,reg_field_value$*256)
63300     library 'S:\Core\Reg.br': fnureg_write
63310     fnureg_write=fnureg_write(reg_field_name$,reg_field_value$)
63320   fnend 
63330   def library fnreg_close
63340     library 'S:\Core\Reg.br': fnreg_close
63350     fnreg_close=fnreg_close
63360   fnend 
63362   def library fnsreg_rename(field_name_old$*128,fieldNameNew$*128)
63364     library 'S:\Core\Reg.br': fnsreg_rename
63366     fnsreg_rename=fnsreg_rename(field_name_old$,fieldNameNew$)
63368   fnend 
63372   def library fnreg_rename(field_name_old$*128,fieldNameNew$*128)
63374     library 'S:\Core\Reg.br': fnreg_rename
63376     fnreg_rename=fnreg_rename(field_name_old$,fieldNameNew$)
63378   fnend 
63382   def library fnIniToReg
63384     library 'S:\Core\Reg.br': fnIniToReg
63386     fnIniToReg=fnIniToReg
63388   fnend 
63392   def library fnread_program_print_property(key$*80,&value$; programFileOverride$*256)
63394     library 'S:\Core\Reg.br': fnread_program_print_property
63396     fnread_program_print_property=fnread_program_print_property(key$,value$, programFileOverride$)
63398   fnend 
63412   def library fnwrite_program_print_property(key$*80,value$*256; programFileOverride$*256)
63414     library 'S:\Core\Reg.br': fnwrite_program_print_property
63416     fnwrite_program_print_property=fnwrite_program_print_property(key$,value$, programFileOverride$)
63418   fnend 
63990 ! /r
64000 ! r: array stuff
64010   def library fnarray_item_insert$(mat array$, insert_item$*1024, insert_item_number)
64020     library 'S:\Core\Array.br': fnarray_item_insert$
64030     fnarray_item_insert$=fnarray_item_insert$(mat array$, insert_item$, insert_item_number)
64040   fnend 
64050   def library fnarray_item_insert(mat array, insert_item, insert_item_number)
64060     library 'S:\Core\Array.br': fnarray_item_insert
64070     fnarray_item_insert=fnarray_item_insert(mat array, insert_item, insert_item_number)
64080   fnend 
64090   def library fnsrch_case_insensitive(mat srch_array$,srch_for$*80; srch_start_ele)
64100     library 'S:\Core\Array.br': fnsrch_case_insensitive
64110     fnsrch_case_insensitive=fnsrch_case_insensitive(mat srch_array$,srch_for$, srch_start_ele)
64120   fnend 
64130   def library fnAddOneN(mat add_to,one; skip_zeros,skip_dupes)
64140     library 'S:\Core\Array.br': fnAddOneN
64150     fnAddOneN=fnAddOneN(mat add_to,one, skip_zeros,skip_dupes)
64160   fnend 
64170   def library fnAddOneC(mat add_to$,one$*2048; skip_blanks,skip_dupes)
64180     library 'S:\Core\Array.br': fnAddOneC
64190     fnAddOneC=fnAddOneC(mat add_to$,one$, skip_blanks,skip_dupes)
64200   fnend 
64210 ! /r
66000 ! r: ini functions and quick calls
66020   def library fnIniOpen(ii_file$*256)
66030     library 'S:\Core\ini.br': fnIniOpen
66040     fnIniOpen=fnIniOpen(ii_file$)
66050   fnend 
66060   def library fnIniRead$*256(il_section$*256,il_field$*256)
66070     library 'S:\Core\ini.br': fnIniRead$
66080     fnIniRead$=fnIniRead$(il_section$,il_field$)
66090   fnend 
66100   def library fnIniSet(inis_section$*256,inis_field$*256,inis_value$*256)
66110     library 'S:\Core\ini.br': fnIniSet
66120     fnIniSet=fnIniSet(inis_section$,inis_field$,inis_value$)
66130   fnend 
66140   def library fnIniWrite
66150     library 'S:\Core\ini.br': fnIniWrite
66160     fnIniWrite=fnIniWrite
66170   fnend 
66290 ! /r
68090   def library fnshortpath$*256(longpath$*256)
68100     library 'S:\Core\Ace\fnShortPath.br': fnshortpath$
68110     fnshortpath$=fnshortpath$(longpath$)
68120   fnend 
68140 ! r: S:\Core\CNo.br
68170   def library fnUseDeptNo
68180     library 'S:\Core\CNo.br': fnUseDeptNo
68190     fnUseDeptNo=fnUseDeptNo
68200   fnend 
68250 ! /r
70000 ! r: UB   utility billing
70011   def library fnMeterAddressUpdate(meterAddressBefore$*30,&meterAddressAfter$)
70012     library 'S:\Utility Billing\Meter Address.br': fnMeterAddressUpdate
70013     fnMeterAddressUpdate=fnMeterAddressUpdate(meterAddressBefore$,meterAddressAfter$)
70014   fnend
70021   def library fnDepositChangeLog(z$*10,odp,ndp,chgDate,comment$*32)
70022     library 'S:\Utility Billing\Customer.br': fnDepositChangeLog
70023     fnDepositChangeLog=fnDepositChangeLog(z$,odp,ndp,chgDate,comment$)
70024   fnend
70031   def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen)
70032     library 'S:\Utility Billing\Meter Address.br': fnMeterAddressLocationID
70033     fnMeterAddressLocationID=fnMeterAddressLocationID(meterAddress$, leaveFileOpen)
70034   fnend
70041   def library fnMeterAddressName$*30(locationID; leaveFileOpen)
70042     library 'S:\Utility Billing\Meter Address.br': fnMeterAddressName$
70043     fnMeterAddressName$=fnMeterAddressName$(locationID, leaveFileOpen)
70044   fnend
70051   def library fnNoteDir$*256
70052     library 'S:\Utility Billing\Customer.br': fnNoteDir$
70053     fnNoteDir$=fnNoteDir$
70054   fnend
70061   def library fnWorkOrderAdd(z$*10)
70062     library 'S:\Utility Billing\Work Order Add.br': fnWorkOrderAdd
70063     fnWorkOrderAdd=fnWorkOrderAdd(z$)
70064   fnend
70090   def library fnWorkOrderList(; z$*10)
70100     library 'S:\Utility Billing\Work Order List.br': fnWorkOrderList
70110     fnWorkOrderList=fnWorkOrderList(z$)
70120   fnend
70130   def library fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
70140     library 'S:\Utility Billing\Work Order Print.br': fnWorkOrderPrint
70150     fnWorkOrderPrint=fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
70160   fnend
70170   def library fncustomer_address(z$,mat addr$; ca_address_type)
70180     library 'S:\Utility Billing\Labels.br': fncustomer_address
70190     fncustomer_address=fncustomer_address(z$,mat addr$, ca_address_type)
70200   fnend 
70202   def library fnCustomerNotes(z$)
70204     library 'S:\Utility Billing\Customer.br': fnCustomerNotes
70206     fnCustomerNotes=fnCustomerNotes(z$)
70208   fnend 
70210   def library fnapply_default_rates(mat extra, mat a)
70220     library 'S:\Utility Billing\Customer.br': fnapply_default_rates
70230     fnapply_default_rates=fnapply_default_rates(mat extra, mat a)
70240   fnend 
70250   def library fnget_services(mat servicename$; mat servicecode$, mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
70260     library 'S:\acsUB\TypeOfService.br': fnget_services
70270     fnget_services=fnget_services(mat servicename$, mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
70280   fnend 
70290   def library fncmbact(lyne,mypos; addall,container,indexfile$*256)
70300     library 'S:\acsUB\CmbAct.br': fncmbact
70310     fncmbact=fncmbact(lyne,mypos, addall,container,indexfile$)
70320   fnend 
70330   def library fncmbroute(lyne,mypos; addall,c,a$*25)
70340     library 'S:\acsUB\CmbRoute.br': fncmbroute
70350     fncmbroute=fncmbroute(lyne,mypos,addall,c,a$)
70360   fnend 
70370   def library fnd1(&d1;get_or_put)
70380     library 'S:\acsUB\D1.br': fnd1
70390     fnd1=fnd1(d1,get_or_put)
70400   fnend 
70410   def library fncustomer_search(&x$;fixgrid)
70420     library 'S:\acsUB\customer_search.br': fncustomer_search
70430     fncustomer_search=fncustomer_search(x$,fixgrid)
70440   fnend 
70450   def library fncustomer(x)
70460     library 'S:\Utility Billing\Customer.br': fncustomer
70470     fncustomer=fncustomer(x)
70480   fnend 
70490   def library fnask_account(prev_list_id$,&x$,h_customer; select_button_text$,aa_button_enable_add)
70500     library 'S:\Utility Billing\Customer.br': fnask_account
70510     fnask_account=fnask_account(prev_list_id$,x$,h_customer, select_button_text$,aa_button_enable_add)
70520   fnend 
70610   def library fnopen_meter
70620     library 'S:\Utility Billing\Hand Held\Meter Information': fnopen_meter
70630     fnopen_meter=fnopen_meter
70640   fnend 
70650   def library fncmbrt2(lyne,mypos;all)
70660     library 'S:\acsUB\CmbRt2.br': fncmbrt2
70670     fncmbrt2=fncmbrt2(lyne,mypos,all)
70680   fnend 
70770   def library fntransfile(hact$*81)
70780     library 'S:\Utility Billing\Transactions.br': fntransfile
70790     fntransfile=fntransfile(hact$)
70800   fnend 
70810   def library fntrans_total_as_of(; customer_key$,date_ccyymmdd,trans_type)
70820     library 'S:\Utility Billing\Transactions.br': fntrans_total_as_of
70830     fntrans_total_as_of=fntrans_total_as_of( customer_key$,date_ccyymmdd,trans_type)
70840   fnend 
70850   def library fnub_cnv_build_transactions
70860     library 'S:\acsUB\conversion\bld_trans.br': fnub_cnv_build_transactions
70870     fnub_cnv_build_transactions=fnub_cnv_build_transactions
70880   fnend 
70890   def library fnub_cnv_ubmstr_vb
70900     library 'S:\acsUB\conversion\ubmstr-vb.br': fnub_cnv_ubmstr_vb
70910     fnub_cnv_ubmstr_vb=fnub_cnv_ubmstr_vb
70920   fnend 
70930   def library fnub_cnv_note
70940     library 'S:\acsUB\conversion\note-cnv.br': fnub_cnv_note
70950     fnub_cnv_note=fnub_cnv_note
70960   fnend 
70970   def library fnub_cnv_note_phase_1
70980     library 'S:\acsUB\conversion\note-cnv-c7.br': fnub_cnv_note_phase_1
70990     fnub_cnv_note_phase_1=fnub_cnv_note_phase_1
71000   fnend 
71010   def library fnub_cnv_adrbil
71020     library 'S:\acsUB\conversion\ubadrbil-cnv.br': fnub_cnv_adrbil
71030     fnub_cnv_adrbil=fnub_cnv_adrbil
71040   fnend 
71050   def library fntotal_ar
71060     library 'S:\acsUB\TotalBal.br': fntotal_ar
71070     fntotal_ar=fntotal_ar
71080   fnend 
71085   def library fnfix_trans_breakdowns(do_fix,do_report)
71086     library 'S:\acsUB\check_balance_breakdowns.br': fnfix_trans_breakdowns
71087     fnfix_trans_breakdowns=fnfix_trans_breakdowns(do_fix,do_report)
71088   fnend 
74000   ! r: Hand Held
74020     def library fnhand_held_device$*20
74040       library 'S:\Core\Client.br': fnhand_held_device$
74060       fnhand_held_device$=fnhand_held_device$
74080     fnend 
74100     def library fnHand_Held_Device_list(mat device$)
74120       library 'S:\Utility Billing\Hand Held\Create Hand Held File.br': fnHand_Held_Device_list
74140       fnHand_Held_Device_list=fnHand_Held_Device_list(mat device$)
74160     fnend
74180     def library fnretrieve_hand_held_file
74200       library 'S:\Utility Billing\Hand Held\Import from Hand Held to Book.br': fnretrieve_hand_held_file
74220       fnretrieve_hand_held_file=fnretrieve_hand_held_file
74240     fnend 
74260   ! /r
74280 ! /r
82000 ! r: GL   general ledger
82012   def library fnGetFundList(mat fund_list)
82014     library 'S:\General Ledger\Fix Period Accumulators from History.br': fnGetFundList
82016     fnGetFundList=fnGetFundList(mat fund_list)
82018   fnend 
82020   def library fnW2supEdit(;empNo$)
82040     library 'S:\acsGL\w2box16.br': fnW2supEdit
82060     fnW2supEdit=fnW2supEdit( empNo$)
82080   fnend 
82100   def library fncmbbud(&indexfile$)
82120     library 'S:\acsGL\CmbBud.br': fncmbbud
82140     fncmbbud=fncmbbud(indexfile$)
82160   fnend 
82180   def library fnactpd(;a)
82200     library 'S:\acsGL\fnActPd.br': fnactpd
82220     fnactpd=fnactpd(a)
82240   fnend 
82260   def library fnaddglpayee
82280     library 'S:\General Ledger\Payee.br': fnaddglpayee
82300     fnaddglpayee=fnaddglpayee
82320   fnend 
82340   def library fnactpd$(;a$)
82360     library 'S:\acsGL\fnActPd$.br': fnactpd$
82380     fnactpd$=fnactpd$(a$)
82400   fnend 
82420   def library fncch$*20(;a$*20)
82440     library 'S:\acsGL\fnCCH$.br': fncch$
82460     fncch$=fncch$(a$)
82480   fnend 
82500   def library fnglfs
82520     library 'S:\acsGL\fnglFS.br': fnglfs
82540     fnglfs=fnglfs
82560   fnend 
82580   def library fnglmerge
82600     library 'S:\acsGL\fnGLmerge.br': fnglmerge
82620     fnglmerge=fnglmerge
82640   fnend 
82660   def library fnaccount_search(&x$;fixgrid)
82680     library 'S:\acsGL\account_search.br': fnaccount_search
82700     fnaccount_search=fnaccount_search(x$,fixgrid)
82720   fnend 
82740   def library fnacglblds
82760     library 'S:\acsGL\fnacglblds.br': fnacglblds
82780     fnacglblds=fnacglblds
82800   fnend 
82820   def library fnemployee_search(&x$;fixgrid)
82840     library 'S:\acsGL\employee_search.br': fnemployee_search
82860     fnemployee_search=fnemployee_search(x$,fixgrid)
82880   fnend 
82900   def library fnacprscr
82920     library 'S:\acsGL\fnacprscr.br': fnacprscr
82940     fnacprscr=fnacprscr
82960   fnend 
82980   def library fnfinstmt_v0_to_v1
83000     library 'S:\acsGL\Conversion\FinStmt_v0_to_v1.br': fnfinstmt_v0_to_v1
83020     fnfinstmt_v0_to_v1=fnfinstmt_v0_to_v1
83040   fnend 
83060   def library fnglmstr_338_416
83080     library 'S:\acsGL\Conversion\glMstr-338-416.br': fnglmstr_338_416
83100     fnglmstr_338_416=fnglmstr_338_416
83120   fnend 
83140   def library fnglpayee_v0_to_v1
83160     library 'S:\acsGL\Conversion\glPayee_v0_to_v1.br': fnglpayee_v0_to_v1
83180     fnglpayee_v0_to_v1=fnglpayee_v0_to_v1
83200   fnend 
83220   def library fnrepr(x$)
83240     library 'S:\Core\repr.br': fnrepr
83260     fnrepr=fnrepr(x$)
83280   fnend 
83300 ! /r
84000 ! r: CL   Checkbook
84020   def library fnApMstrConversion
84040   library 'S:\acsCL\Conversion\apmstr-cnv.br': fnApMstrConversion
84060   fnApMstrConversion=fnApMstrConversion
84080   fnend
84100   def library fnpostgl2(glt)
84120     library 'S:\acsCL\PostGL2.br': fnpostgl2
84140     fnpostgl2(glt)
84160   fnend 
84180   def library fntrmstr_v1_to_v2
84200     library 'S:\acsCL\Conversion\TrMstr-v1-to-v2.br': fntrmstr_v1_to_v2
84220     fntrmstr_v1_to_v2=fntrmstr_v1_to_v2
84240   fnend 
84260   def library fntralloc_v1_to_v2
84280     library 'S:\acsCL\Conversion\TrAlloc-v1-to-v2.br': fntralloc_v1_to_v2
84300     fntralloc_v1_to_v2=fntralloc_v1_to_v2
84320   fnend 
84340   def library fnpaytrans_v1_to_v2
84360     library 'S:\acsCL\Conversion\PayTrans-v1-to-v2.br': fnpaytrans_v1_to_v2
84380     fnpaytrans_v1_to_v2=fnpaytrans_v1_to_v2
84400   fnend 
84420   def library fnunpdaloc_v1_to_v2
84440     library 'S:\acsCL\Conversion\UnPdAloc-v1-to-v2.br': fnunpdaloc_v1_to_v2
84460     fnunpdaloc_v1_to_v2=fnunpdaloc_v1_to_v2
84480   fnend 
84500   def library fnpaymstr_v0_to_v1
84520     library 'S:\acsCL\Conversion\PayMstr-v0-to-v1.br': fnpaymstr_v0_to_v1
84540     fnpaymstr_v0_to_v1=fnpaymstr_v0_to_v1
84560   fnend 
84580   def library fnglmstrtorecl62
84600     library 'S:\acsCL\Conversion\GLMstr-to-RecL62.br': fnglmstrtorecl62
84620     fnglmstrtorecl62=fnglmstrtorecl62
84640   fnend 
84660   def library fnglcontrol
84680     library 'S:\acsCL\Conversion\fundmstr-RecL75.br': fnglcontrol
84700     fnglcontrol=fnglcontrol
84720   fnend 
84740   def library fntrmstr_v0_to_v1
84760     library 'S:\acsCL\Conversion\fnTrMstr_v0_to_v1.br': fntrmstr_v0_to_v1
84780     fntrmstr_v0_to_v1=fntrmstr_v0_to_v1
84800   fnend 
84820   def library fnaddpayee
84840     library 'S:\Checkbook\Payee.br': fnaddpayee
84860     fnaddpayee=fnaddpayee
84880   fnend 
84900   def library fnaddreceipt
84920     library 'S:\acsCL\fnReceipt.br': fnaddreceipt
84940     fnaddreceipt=fnaddreceipt
84960   fnend 
84980   def library fnbankbal(x)
85000     library 'S:\acsCL\fnBankBal.br': fnbankbal
85020     fnbankbal=fnbankbal(x)
85040   fnend 
85060   def library fnupdatebankbal(bank_code,modification)
85080     library 'S:\acsCL\fnUpdateBankBal.br': fnupdatebankbal
85100     fnupdatebankbal=fnupdatebankbal(bank_code,modification)
85120   fnend 
85140 ! /r
86000 ! r: PR   payroll
86021   def library fnGetPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4,&d1,&d1$)
86022     library 'S:\Payroll\Change Payroll Dates.br': fnGetPayrollDates
86023     fnGetPayrollDates=fnGetPayrollDates(beg_date,end_date, qtr1,qtr2,qtr3,qtr4,d1,d1$)
86024   fnend 
86031   def library fnDedNames(mat fullname$; mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
86032     library 'S:\acsPR\fnDedNames.br': fnDedNames
86033     fnDedNames=fnDedNames(mat fullname$, mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
86034   fnend 
86041   def library fnprint_designed_report(rptn)
86042     library 'S:\acsPR\newprRptS1.br': fnprint_designed_report
86043     fnprint_designed_report=fnprint_designed_report(rptn)
86044   fnend 
86050   def library fnpayroll_register_2(; det,include_tips_in_other_wh,append_reg1)
86060     library 'S:\acsPR\newprreg2.br': fnpayroll_register_2
86070     fnpayroll_register_2=fnpayroll_register_2( det,include_tips_in_other_wh,append_reg1)
86080   fnend 
86090   def library fnss_employee
86100     library 'S:\acsPR\ss_emp.br': fnss_employee
86110     fnss_employee=fnss_employee
86120   fnend 
86130   def library fnss_employer
86140     library 'S:\acsPR\ss_emp.br': fnss_employer
86150     fnss_employer=fnss_employer
86160   fnend 
86210   def library fnemployee_srch(&x$;fixgrid)
86220     library 'S:\acsPR\Employee_srch.br': fnemployee_srch
86230     fnemployee_srch(x$,fixgrid)
86240   fnend 
86250   def library fnburden_srch(&x$;fixgrid)
86260     library 'S:\acsPR\Burden_srch.br': fnburden_srch
86270     fnburden_srch(x$,fixgrid)
86280   fnend 
86290   def library fnjob_srch(&x$;fixgrid)
86300     library 'S:\acsPR\Job_srch.br': fnjob_srch
86310     fnjob_srch(x$,fixgrid)
86320   fnend 
86330   def library fncat_srch(&x$;fixgrid)
86340     library 'S:\acsPR\CAT_srch.br': fncat_srch
86350     fncat_srch(x$,fixgrid)
86360   fnend 
86370   def library fncat_srch2(&x$,&ckey;fixgrid)
86380     library 'S:\acsPR\CAT_srch2.br': fncat_srch2
86390     fncat_srch2(x$,ckey,fixgrid)
86400   fnend 
86410   def library fncategory_srch(&cn$;fixgrid)
86420     library 'S:\acsPR\CATegory_srch.br': fncategory_srch
86430     fncategory_srch(x$,fixgrid)
86440   fnend 
86450   def library fnsubcat_srch(&cde$,&ckey;fixgrid)
86460     library 'S:\acsPR\SubCat_srch.br': fnsubcat_srch
86470     fnsubcat_srch(cde$,ckey,fixgrid)
86480   fnend 
86490   def library fncmbemp(lyne,mypos;addall,c,a$*30)
86500     library 'S:\acsPR\CmbEmp.br': fncmbemp
86510     fncmbemp(lyne,mypos,addall,c,a$)
86520   fnend 
86530   def library fncmbburden(lyne,mypos;addall,c,a$*30)
86540     library 'S:\acsPR\CmbBurden.br': fncmbburden
86550     fncmbburden(lyne,mypos,addall,c,a$)
86560   fnend 
86570   def library fncmbcategory(lyne,mypos;addall,c,a$*30)
86580     library 'S:\acsPR\CmbCategory.br': fncmbcategory
86590     fncmbcategory(lyne,mypos,addall,c,a$)
86600   fnend 
86610   def library fncheckfile(hact$,filnum)
86620     library 'S:\acsPR\checkfile.br': fncheckfile
86630     fncheckfile=fncheckfile(hact$,filnum)
86640   fnend 
86690   def library fnhours(eno)
86700     library 'S:\acsPR\hours_lib.br': fnhours
86710     fnhours(eno)
86720   fnend 
86730   def library fncmbjob(lyne,mypos;addall,c,a$*30)
86740     library 'S:\acsPR\Cmbjob.br': fncmbjob
86750     fncmbjob(lyne,mypos,addall,c,a$)
86760   fnend 
86770   def library fncmbcat(lyne,mypos;addall,c,a$*30)
86780     library 'S:\acsPR\CmbCat.br': fncmbcat
86790     fncmbcat(lyne,mypos,addall,c,a$)
86800   fnend 
86810   def library fncmbsubcat(lyne,mypos;addall,c,a$*30)
86820     library 'S:\acsPR\CmbSubCat.br': fncmbsubcat
86830     fncmbsubcat(lyne,mypos,addall,c,a$)
86840   fnend 
86850   def library fnpr_conversion_department(cno; medicare_is_seperated)
86860     library 'S:\acsPR\Conversion\v4_cnv.br': fnpr_conversion_department
86870     fnpr_conversion_department=fnpr_conversion_department(cno, medicare_is_seperated)
86880   fnend 
86890   def library fnpr_conversion_add_missing(cno)
86900     library 'S:\acsPR\Conversion\v4_part2.br': fnpr_conversion_add_missing
86910     fnpr_conversion_add_missing=fnpr_conversion_add_missing(cno)
86920   fnend 
86930 ! /r


