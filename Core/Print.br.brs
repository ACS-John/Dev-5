! Replace S:\Core\Print.br
! open the report, printer, etc...
!
def fn_setup
	if ~setup then 
		setup=1
		autoLibrary
		! report_cache$='True' ! fnreg_read('Report_Cache',report_cache$,'True')
		! if report_cache$='True' then print_report_caching=1 else print_report_caching=0
		! fnureg_read('wait_wp_close',wait_wp_close$,'True')
		! if wait_wp_close$='False' then print_report_nowait=1 else print_report_nowait=0
	end if 
	on error goto Ertn
fnend 

def library fnprint_file_name$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
	fn_setup
	fnprint_file_name$=fn_print_file_name$( pfn_sendto_base_name_addition$,pfn_extension$,programCaptionOverride$)
fnend 
def fn_print_file_name$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
	dim pfnReturn$*1024
	pfn_extension$=trim$(pfn_extension$,'.')
	if pfn_extension$='' then pfn_extension$='rtf'
	! if print_report_caching then 
		if programCaptionOverride$='' then programCaptionOverride$=env$('Program_Caption')
		pfnReturn$=fn_report_cache_folder_current$&'\'&fn_safe_filename$(programCaptionOverride$)
		pfn_sendto_base_name_addition$=trim$(fn_safe_filename$(pfn_sendto_base_name_addition$))
		if pfn_sendto_base_name_addition$<>'' then 
			pfnReturn$=pfnReturn$&' '&pfn_sendto_base_name_addition$
		end if 
		pfnReturn$=pfnReturn$&' - '&date$('ccyy-mm-dd')&' '&fn_safe_filename$(time$)
		pfnReturn$=pfnReturn$&'.'&pfn_extension$
	! else 
	!   pfnReturn$=env$('temp')&'\acs-'&session$&'.'&pfn_extension$
	! end if 
	pfnReturn$=fnSrepEnv$(pfnReturn$)
	fn_print_file_name$=pfnReturn$
fnend 

def library fnreport_cache_folder_current$*512
	fn_setup
	fnreport_cache_folder_current$=fn_report_cache_folder_current$
fnend
def fn_report_cache_folder_current$*512(; ___,return$*512)
	dim report_cache_base$*256
	! dim client_report_cache$*256
	if env$('BR_MODEL')='CLIENT/SERVER' then
		report_cache_base$=rtrm$(fnProgramDataDir$,'\')&'\Report Cache'
	else
		report_cache_base$=os_filename$('[Q]\Report Cache')
	end if
	if report_cache_base$='' then report_cache_base$=os_filename$('[Q]\Report Cache')
	return$=rtrm$(report_cache_base$,'\')&'\'&fnSystemNameFromAbbr$
	return$=rtrm$(return$,'\')&'\'&fn_safe_filename$(env$('cnam'))&' ([cno])'
	return$=fnSrepEnv$(return$)
	fnmakesurepathexists(env$('at')&return$&'\')
	fn_report_cache_folder_current$=return$
fnend

def library fnopen_receipt_printer(; orp_only_if_it_is_assigned)
	fn_setup
	fnopen_receipt_printer=fn_open_receipt_printer( orp_only_if_it_is_assigned)
fnend 
def fn_open_receipt_printer(; orp_only_if_it_is_assigned)
	dim orp_receipt_printer$*256
	fnureg_read('Printer.Receipt',orp_receipt_printer$)
	orp_did_open=0
	if orp_receipt_printer$='' then 
		if ~orp_only_if_it_is_assigned then 
			fn_openprn
			orp_did_open=1
		end if
	else 
		open #255: 'Name=Prn:/'&orp_receipt_printer$,display,output ioerr ORP_FINIS
		orp_did_open=1
	end if 
	ORP_FINIS: !
	fn_open_receipt_printer=orp_did_open
fnend 
def library fnclose_receipt_printer
	fn_setup
	fnclose_receipt_printer=fn_close_receipt_printer
fnend 
def fn_close_receipt_printer
	close #255: ioerr ignore
fnend 
def library fnopen_cash_drawer
	fn_setup
	if fn_open_receipt_printer(1) then
		pr #255,using 'form pos 1,c 9,skip 0': hex$("1B70302828") ioerr ignore
		fn_close_receipt_printer
	end if
fnend 

def library fnopenprn(;xx,xxxxx,xxxxxxx,process,sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
	fn_setup
	fnopenprn=fn_openprn( process,sendto_base_name_addition$,prgCapForSettingsOverride$,programCaptionOverride$)
fnend 
def fn_openprn(;process, sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
	if file(255)<>-1 then goto Xit
	dim g_prgCapForSettingsOverride$*256
	g_prgCapForSettingsOverride$=prgCapForSettingsOverride$
	dim op_printFileName$*1024
	op_printFileName$=fn_print_file_name$( sendto_base_name_addition$,'',programCaptionOverride$) ! ,pfn_extension$)
	! 
	fnread_program_print_property("Lines",lpp$, g_prgCapForSettingsOverride$) ! lpp=val(lpp$)
	if lpp$='' then gosub SET_DEFAULTS
	dim g_prn_destination_name$*1024
	g_prn_destination_name$=op_printFileName$
	open #255: 'Name=[Q]\tmp_'&session$&'.prn,PageOFlow='&lpp$&',RecL=512,Replace',display,output 
	goto Xit
	SET_DEFAULTS: ! r:
		pr "Lines settings for this program were not found."
		pr "Default Settings will be used."
		pr "  Lines: 54"
		lpp$='54'
	return  ! /r
Xit: fnend 

def library fncloseprn(;forceWordProcessor$)
	fn_setup
	dim cp_destinationFileName$*1024
	if file(255)<>-1 then ! if the printer file is open.
		cp_destinationFileName$=g_prn_destination_name$ ! trim$(file$(255)(1:1024))
		close #255: 
		if fnCopy('[Q]\tmp_'&session$&'.prn',g_prn_destination_name$) then 
			fnfree('[Q]\tmp_'&session$&'.prn')
		else
			pr 'copy failed.  report lost at temp file: "[Q]\tmp_'&session$&'.prn"'
			pause 
		end if 
		fnStatusClose
		fn_start(cp_destinationFileName$, 0,forceWordProcessor$)
	end if 
	g_prgCapForSettingsOverride$=''
fnend 
def fn_start(start_destinationFilename$*1024; nodrop,forceWordProcessor$,___,isRtf,saveToAsStart$*2048)
	on error goto START_ERTN
	!
	! NoDrop    = 1 = Do not delete the file when your done with it.
	!
	dim winxp$*20,win2k$*22,osver$*80,temp$*120,winnt2kxp$*28
	dim landscape$*1
	dim marg(4)
	dim wordprocessor_exe$*512 ! full path and executable for wordprocessor_exe
	!
	start_destinationFilename$=trim$(start_destinationFilename$)
	winxp$="Microsoft Windows XP"
	win2k$="Microsoft Windows 2000"
	winnt2kxp$="Microsoft Windows NT/2000/XP"
	fnosver(osver$,1)
	! if  start_destinationFilename$='CANCELED' then goto START_XIT

	if lwrc$(start_destinationFilename$(len(start_destinationFilename$)-3:len(start_destinationFilename$)))=".rtf" then 
		isRtf=1
	end if
	if env$('saveToAsStart')<>'' then
		saveToAsStart$=env$('saveToAsStart')
		saveToAsStart$=fnSrepEnv$(saveToAsStart$)
		if ~isRtf then
			fnCopy(start_destinationFilename$,env$('at')&saveToAsStart$)
		end if
		setenv('saveToAsStart','')
	end if
	
	if isRtf then 
		fn_start_rtf(start_destinationFilename$, forceWordProcessor$,saveToAsStart$)
	else if osver$=winxp$ or osver$=win2k$ or osver$=winnt2kxp$ then 
		! r: start_winxp
		temp$='Sy -w NotePad "'&serverSendto$&'"'
		execute temp$
		! /r
	else 
		pr 'win 98 no longer supported.' : pause ! fn_start_win9x
	end if 
 DROPIT: ! 
	goto START_XIT
	!
	START_ERTN: ! r:
	if err=4591 then   ! added for time-outs
		pr newpage
		pr f "10,10,Cc 60,N": "Press ENTER to continue"
		input fields "11,10,C 1,N": pause$
		continue
	end if
	fnerror(program$,err,line,act$,"start_xit")
	if lwrc$(act$)<>"pause" then goto START_ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto START_ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto START_ERTN_EXEC_ACT
	START_ERTN_EXEC_ACT: execute act$ : goto START_ERTN
	! /r
	START_XIT: ! 
	on error goto Ertn
fnend 
def fn_start_rtf(startRtf_destinationFileName$*1024; forceWordProcessor$,saveToAsStart$*2048)
	dim line$*32000
	fn_start_read_properties
	! r:  make the temp rtf file
	dim clientSendto$*1024
	dim serverSendto$*1024
	clientSendto$=env$('at')&startRtf_destinationFileName$
	serverSendto$=startRtf_destinationFileName$
	!   open #20: "Name="&clientSendto$,display,input
	! else
	open #20: "Name="&serverSendto$,display,input
	! end if
	lrec20=lrec(20)
	y=0
	open #21: "Name="&env$('temp')&"\acs_print_tmp"&session$&".rtf,Size=0,RecL=800,Replace",display,output 
	pr #21: "{\rtf1\ansi\deflang1033";
	pr #21: "{\fonttbl";
	pr #21: "{\f8\fswiss\fcharset0\fprq2 Lucida Console;}";
	pr #21: "{\f181\froman\fcharset0\fprq2 Times New Roman;}";
	pr #21: "{\f128\fnil\fcharset0\fprq2 iQs Code 128;}";
	pr #21: "}"
	pr #21: "{\stylesheet";
	pr #21: "{\snext0\f8\fs22\fi0\li0\ri0\ql";
	pr #21: "{\*\stloverrides\f8\fs22\widctlpar} Normal;}"
	pr #21: "{\s1\sbasedon0\snext0\f8\fs28\b\kerning28\fi0\li0\ri0";
	pr #21: "\ql\keepn\sb240\sa60{\*\stloverrides\fs28\b\kerning28";
	pr #21: "\keepn\sb240\sa60} heading 1;}" ! ;
	pr #21: "{\s2\sbasedon0\snext0\f8\fs24\b\i\fi0\li0\ri0\ql\keepn";
	pr #21: "\sb240\sa60{\*\stloverrides\fs24\b\i\keepn\sb240\sa60}";
	pr #21: " heading 2;}" ! ;
	pr #21: "{\s3\sbasedon0\snext0\f8\fs22\b\fi0\li0\ri0\ql\keepn";
	pr #21: "\sb240\sa60{\*\stloverrides\b\keepn\sb240\sa60} ";
	pr #21: "heading 3;}" ;
	pr #21: "}"
	pr #21: "{\info";
	pr #21: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm");
	pr #21: "\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5);
	pr #21: "\sec"&time$(7:8)&"}";
	pr #21: "{\author ACS 5 - "&login_name$&"}";
	pr #21: "}"
 ! 
	pr #21: "\paperw"&str$(pgw)&"\paperh"&str$(pgh);
	pr #21: "\margl"&str$(marg(3))&"\margr"&str$(marg(4));
	pr #21: "\margt"&str$(marg(1))&"\margb"&str$(marg(2));
	if uprc$(landscape$)=uprc$("Y") then 
		pr #21: "\lndscpsxn";
	end if 
	pr #21: "\widowctrl\useltbaln\plain"
	pr #21: "\f181\fs24\pard\f8\fs"&str$(fsize*2)&" "
 L640: linput #20: line$ eof END_OF_FILE
	if uprc$(line$(1:13))=uprc$("*INSERT FILE:") then 
		line$=line$(14:len(line$))
		goto L660
	else if line$(1:12)="S:\Core\images\" then 
		goto L660
	else 
		goto L700
	end if 
	L660: ! 
	close #21: 
	execute 'Type "'&trim$(line$)&'" >>"'&env$('temp')&'\acs_print_tmp'&session$&'.rtf"'
	open #21: "Name="&env$('temp')&"\acs_print_tmp"&session$&".rtf,RecL=800,use",display,output 
	goto L640
	L700: ! 
	! 
	! 
	L730: ! 
	z=pos(line$,"\",z)
	if z=>1 and line$(z-1:z-1)<>"{" and uprc$(line$(z+1:z+1))<>"Q" then 
		line$(z:z)="\\" : z=z+2
		goto L730
	else 
	L18700: !
		z=pos(line$,"/fcode/",z)
		if z=>1 then 
			line$(z:z+6)="\" : z=z+1
			goto L18700
		else 
			z=0
		end if 
	 !  look for \s and replace with double \\s
	 !  so they will display correctly in rtf
	 !  "{" was added to allow support for bold, italic, underline, etc.
	 !  "q" was added to allow support for alignment.
	end if 
	y=y+len(line$)+2
	if line$(1:1)=chr$(12) then ! and y<lrec20 then   !  shifted this on 1/13/2017 due to strange _ showing up in ms word
		if y<lrec20 then  
			pr #21: "\page"
		end if
		line$(1:1)=''
	end if 
	pr #21: line$&"\par"
	goto L640
	!
	END_OF_FILE: ! 
	pr #21: "}"
	close #21: 
	close #20: 
	! /r
	if env$('BR_MODEL')='CLIENT/SERVER' then
		fnCopy(env$('temp')&"\acs_print_tmp"&session$&".rtf",clientSendto$)
	end if
	fnCopy(env$('temp')&"\acs_print_tmp"&session$&".rtf",serverSendto$)
	! pr 'BR copied to: '&clientSendto$ ! 
	if env$('BR_MODEL')='CLIENT/SERVER' then
		if ~setup_cs then
			setup_cs=1
			dim cache_sendto_path$*512
			dim cache_sendto_file_base$*256
			dim cache_sendto_file_ext$*128
			fnGetPp(serverSendto$,cache_sendto_path$,cache_sendto_file_base$,cache_sendto_file_ext$)
		!       pause
			serverSendto$=fn_report_cache_folder_current$&'\'&cache_sendto_file_base$&cache_sendto_file_ext$
		!       pr 'CS set destination to: '&serverSendto$
		end if
	end if
	 !
	fnget_wordprocessor_exe(wordprocessor_exe$, forceWordProcessor$) 
	wordprocessor_exe$=trim$(wordprocessor_exe$,'"')
	if saveToAsStart$<>'' then
		fnCopy(serverSendto$,env$('at')&saveToAsStart$)
	end if
	if fnprocess=1 and pos(lwrc$(wordprocessor_exe$),'atlantis')>0 then 
		execute 'Sy -w "'&wordprocessor_exe$&'" -st /p /npd "'&os_filename$(serverSendto$)&'"' ! automatic processing  ! kj 53107
	else ! if print_report_nowait or fnprocess=1 then 
		execute 'Sy -w -C "'&wordprocessor_exe$&'" "'&os_filename$(fnSrepEnv$(serverSendto$))&'"'
	! else 
	!   fn_waitForWpToCloseStart('Word Processor')
	!   execute 'Sy -w '&wordprocessor_exe$&' "'&os_filename$(serverSendto$)&'"'
	!   fn_waitForWpToCloseEnd
	end if 
 ! pause
fnend 

def library fnWaitForShellCloseStart(whatsRunning$*256)
	if ~setup then fn_setup
	fnWaitForShellCloseStart=fn_waitForWpToCloseStart(whatsRunning$)
fnend
def fn_waitForWpToCloseStart(whatsRunning$*256)
	open #h_win_wait=fngethandle: "srow=1,scol=1,rows="&env$('Session_Rows')&",cols="&env$('Session_Cols')&",border=none,picture=S:\Core\disable.png:TILE",display,output 
	pr #h_win_wait,fields str$(val(env$('Session_Rows'))/2)&',1,Cc '&env$('Session_Cols')&',[Screen]': 'Close your '&whatsRunning$&' to continue.'
fnend 

def library fnWaitForShellCloseEnd
	if ~setup then fn_setup
	fnWaitForShellCloseEnd=fn_waitForWpToCloseEnd
fnend
def fn_waitForWpToCloseEnd
	close #h_win_wait: ioerr ignore
fnend 

def fn_start_read_properties
	fnread_program_print_property('Orientation',orientation$, g_prgCapForSettingsOverride$)
	if orientation$="Landscape" then landscape$="Y" else landscape$="N"
	fnread_program_print_property('Height',pgh$, g_prgCapForSettingsOverride$) : pgh=val(pgh$)
	if pgh=0 then pgh=11
	fninch2twip(pgh)
	fnread_program_print_property('Width',pgw$, g_prgCapForSettingsOverride$) : pgw=val(pgw$)
	if pgw=0 then pgw=8.5
	fninch2twip(pgw)
	fnread_program_print_property('TopMargin',temp$, g_prgCapForSettingsOverride$) : marg(1)=val(temp$)
	fninch2twip(marg(1))
	fnread_program_print_property('BottomMargin',temp$, g_prgCapForSettingsOverride$) : marg(2)=val(temp$)
	fninch2twip(marg(2))
	fnread_program_print_property('LeftMargin',temp$, g_prgCapForSettingsOverride$) : marg(3)=val(temp$)
	fninch2twip(marg(3))
	fnread_program_print_property('RightMargin',temp$, g_prgCapForSettingsOverride$) : marg(4)=val(temp$)
	fninch2twip(marg(4))
	fnread_program_print_property('FontSize',temp$, g_prgCapForSettingsOverride$) : fsize=val(temp$)
fnend 

def library fnsafe_filename$*256(sf_in$*256)
	fnsafe_filename$=fn_safe_filename$(sf_in$)
fnend
def fn_safe_filename$*256(sf_in$*256)
	if ~sf_setup then ! r:
		sf_setup=1
		sf_symbol_count=0
		dim sf_symbol$(999)*1
		dim sf_replace_with$(999)*1
		mat sf_symbol$(999)
		mat sf_replace_with$(999)
		! alloe #    sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='#' : sf_replace_with$(sf_symbol_count)=' ' ! pound
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='<' : sf_replace_with$(sf_symbol_count)='(' ! left angle bracket
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='$' : sf_replace_with$(sf_symbol_count)=' ' ! dollar sign
		! allow plus   sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='+' : sf_replace_with$(sf_symbol_count)='_' ! plus sign
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='%' : sf_replace_with$(sf_symbol_count)=' ' ! percent
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='>' : sf_replace_with$(sf_symbol_count)=')' ! right angle bracket
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='!' : sf_replace_with$(sf_symbol_count)=' ' ! exclamation point
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='`' : sf_replace_with$(sf_symbol_count)=' ' ! backtick
		! allow ampersands     sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='&' : sf_replace_with$(sf_symbol_count)=' ' ! ampersand
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='*' : sf_replace_with$(sf_symbol_count)=' ' ! asterisk
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)="'" : sf_replace_with$(sf_symbol_count)='' ! single quotes
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='|' : sf_replace_with$(sf_symbol_count)=' ' ! pipe
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='{' : sf_replace_with$(sf_symbol_count)='(' ! left bracket
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='?' : sf_replace_with$(sf_symbol_count)=' ' ! question mark
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='"' : sf_replace_with$(sf_symbol_count)='' ! double quotes
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='=' : sf_replace_with$(sf_symbol_count)=' ' ! equal sign
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='}' : sf_replace_with$(sf_symbol_count)=')' ! right bracket
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='/' : sf_replace_with$(sf_symbol_count)='-' ! forward slash
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)=':' : sf_replace_with$(sf_symbol_count)='-' ! colon
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='\' : sf_replace_with$(sf_symbol_count)='-' ! back slash
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='@' : sf_replace_with$(sf_symbol_count)=' ' ! at sign
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)=',' : sf_replace_with$(sf_symbol_count)='' ! comma  (BR! can not handle them.)
		sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='.' : sf_replace_with$(sf_symbol_count)='' ! period / dot
		mat sf_symbol$(sf_symbol_count)
		mat sf_replace_with$(sf_symbol_count)
	end if  ! /r
	for sf_item=1 to sf_symbol_count
		sf_in$=srep$(sf_in$,sf_symbol$(sf_item),sf_replace_with$(sf_item))
	next sf_item
	do 
		sf_in$=srep$(sf_in$,'  ',' ')
	loop until pos(sf_in$,'  ')<=0
	fn_safe_filename$=trim$(sf_in$)
fnend 
include: Ertn
