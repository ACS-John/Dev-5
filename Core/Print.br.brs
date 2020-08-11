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

def library fnPrintFileName$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
	fn_setup
	fnPrintFileName$=fn_printFileName$( pfn_sendto_base_name_addition$,pfn_extension$,programCaptionOverride$)
fnend
def fn_printFileName$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
	dim pfnReturn$*1024
	pfn_extension$=trim$(pfn_extension$,'.')
	if pfn_extension$='' then pfn_extension$='rtf'
	if programCaptionOverride$='' then
		programCaptionOverride$=env$('Program_Caption')
	else if pos(programCaptionOverride$,'*')>0 then
		programCaptionOverride$=srep$(programCaptionOverride$,'*',env$('Program_Caption'))
	end if
	pfnReturn$=fn_reportCacheFolderCurrent$&'\'&fn_safeFilename$(programCaptionOverride$,1)
	pfn_sendto_base_name_addition$=trim$(fn_safeFilename$(pfn_sendto_base_name_addition$))
	if pfn_sendto_base_name_addition$<>'' then
		pfnReturn$=pfnReturn$&' '&pfn_sendto_base_name_addition$
	end if
	pfnReturn$=pfnReturn$&' - '&date$('ccyy-mm-dd')&' '&fn_safeFilename$(time$)
	pfnReturn$=pfnReturn$&'.'&pfn_extension$
	pfnReturn$=fnSrepEnv$(pfnReturn$)
	if pos(programCaptionOverride$,'\')>0 then fnmakesurepathexists(pfnReturn$)
	fn_printFileName$=pfnReturn$
fnend

def library fnReportCacheFolderCurrent$*512
	fn_setup
	fnReportCacheFolderCurrent$=fn_reportCacheFolderCurrent$
fnend
def fn_reportCacheFolderCurrent$*512(; ___,return$*512)
	dim report_cache_base$*256
	! dim client_report_cache$*256
	if env$('BR_MODEL')='CLIENT/SERVER' then
		report_cache_base$=rtrm$(fnProgramDataDir$,'\')&'\Report Cache'
	else
		report_cache_base$=os_filename$('[Q]\Report Cache')
	end if
	if report_cache_base$='' then report_cache_base$=os_filename$('[Q]\Report Cache')
	return$=rtrm$(report_cache_base$,'\')&'\'&fnSystemNameFromAbbr$
	return$=rtrm$(return$,'\')&'\'&fn_safeFilename$(env$('cnam'))&' ([cno])'
	return$=fnSrepEnv$(return$)
	fnmakesurepathexists(env$('at')&return$&'\')
	fn_reportCacheFolderCurrent$=return$
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

def library fnopenprn(;sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
	fn_setup
	fnopenprn=fn_openprn( sendto_base_name_addition$,prgCapForSettingsOverride$,programCaptionOverride$)
fnend
def fn_openprn(; sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
	if file(255)<>-1 then goto Xit
	dim g_prgCapForSettingsOverride$*256
	g_prgCapForSettingsOverride$=prgCapForSettingsOverride$
	dim op_printFileName$*1024
	op_printFileName$=fn_printFileName$( sendto_base_name_addition$,'',programCaptionOverride$) ! ,pfn_extension$)

	fnReadProgramPrintProperty("Lines",lpp$, g_prgCapForSettingsOverride$) ! lpp=val(lpp$)
	if lpp$='' then gosub SET_DEFAULTS
	dim g_prn_destination_name$*1024
	g_prn_destination_name$=op_printFileName$
	open #255: 'Name=[Q]\tmp_[session].prn,PageOFlow='&lpp$&',RecL=512,Replace',display,output
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
		if fnCopy('[Q]\tmp_[session].prn',g_prn_destination_name$) then
			fnfree('[Q]\tmp_[session].prn')
		else
			pr 'copy failed.  report lost at temp file: "[Q]\tmp_[session].prn"'
			pause
		end if
		fnStatusClose
		fn_start(cp_destinationFileName$, 0,forceWordProcessor$)
	end if
	g_prgCapForSettingsOverride$=''
fnend
def fn_start(start_destinationFilename$*1024; unused,forceWordProcessor$,___,isRtf,saveToAsStart$*2048)
	on error goto START_ERTN

	dim winxp$*20,win2k$*22,osver$*80,temp$*120,winnt2kxp$*28
	dim landscape$*1
	dim marg(4)
	dim wordprocessor_exe$*512 ! full path and executable for wordprocessor_exe

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
		fn_startRtf(start_destinationFilename$, forceWordProcessor$,saveToAsStart$)
	else if osver$=winxp$ or osver$=win2k$ or osver$=winnt2kxp$ then
		! r: start_winxp
		temp$='Sy -w NotePad "'&startRtf_destinationFileName$&'"'
		execute temp$
		! /r
	else
		pr 'win 98 no longer supported.' : pause ! fn_start_win9x
	end if
 DROPIT: !
	goto START_XIT

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
def fn_startRtf(startRtf_destinationFileName$*1024; forceWordProcessor$,saveToAsStart$*2048, ___,hOut,y,z)

	fn_startReadProperties
	! r:  make the temp rtf file
	dim startRtf_destinationFileName$*1024

	!   open #20: "Name="&'[at]'&startRtf_destinationFileName$,display,input
	! else
	open #20: "Name="&startRtf_destinationFileName$,display,input
	! end if
	lrec20=lrec(20)
	open #hOut=fngethandle: "Name=[Temp]\acs_print_tmp[session].rtf,Size=0,RecL=800,Replace",display,output
	pr #hOut: "{\rtf1\ansi\deflang1033";
	pr #hOut: "{\fonttbl";
	pr #hOut: "{\f8\fswiss\fcharset0\fprq2 Lucida Console;}";
	pr #hOut: "{\f181\froman\fcharset0\fprq2 Times New Roman;}";
	pr #hOut: "{\f128\fnil\fcharset0\fprq2 iQs Code 128;}";
	pr #hOut: "}"
	pr #hOut: "{\stylesheet";

	pr #hOut: "{\snext0\f8\fs22\fi0\li0\ri0\ql{\*\stloverrides\f8\fs22\widctlpar} Normal;}"
	pr #hOut: "{\s1\sbasedon0\snext0\f8\fs28\b\kerning28\fi0\li0\ri0\ql\keepn\sb240\sa60{\*\stloverrides\fs28\b\kerning28\keepn\sb240\sa60} heading 1;}" ! ;
	pr #hOut: "{\s2\sbasedon0\snext0\f8\fs24\b\i\fi0\li0\ri0\ql\keepn\sb240\sa60{\*\stloverrides\fs24\b\i\keepn\sb240\sa60} heading 2;}" ! ;
	pr #hOut: "{\s3\sbasedon0\snext0\f8\fs22\b\fi0\li0\ri0\ql\keepn\sb240\sa60{\*\stloverrides\b\keepn\sb240\sa60} heading 3;}" ;
	pr #hOut: "}"

	pr #hOut: "{\*\generator ACS "&env$('acsVersion')&";}"
	pr #hOut: "{\info";
	pr #hOut: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm");
	pr #hOut: "\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5)&"\sec"&time$(7:8)&"}";
	pr #hOut: "{\author ACS 5 - "&login_name$&"}";
	pr #hOut: "}"

	pr #hOut: "\paperw"&str$(pgw)&"\paperh"&str$(pgh);
	pr #hOut: "\margl"&str$(marg(3))&"\margr"&str$(marg(4));
	pr #hOut: "\margt"&str$(marg(1))&"\margb"&str$(marg(2));
	if uprc$(landscape$)=uprc$("Y") then
		pr #hOut: "\lndscpsxn";
	end if
	pr #hOut: "\widowctrl\useltbaln\plain"
	pr #hOut: "\f181\fs24\pard\f8\fs"&str$(fsize*2)&" "
	dim line$*32000
	do
		linput #20: line$ eof SrEoInput
		if uprc$(line$(1:13))=uprc$("*INSERT FILE:") then
			line$=line$(14:len(line$))
			goto SpInsertThatFileHere
		else if line$(1:12)="S:\Core\images\" then
			goto SpInsertThatFileHere
		else
			! r: process the line
			!  look for \s and replace with double \\s
			!  so they will display correctly in rtf
			!  "{" was added to allow support for bold, italic, underline, etc.
			!  "q" was added to allow support for alignment.
			do
				z=max(0,pos(line$,"\",z))
				if pos(uprc$(line$),'{\Q')>0 then pause
				if z and uprc$(line$(z-1:z+1))<>"{\Q" then
					line$(z:z)="\\"
					z+=2
				end if
			loop while z
			do
				z=max(0,pos(line$,"/fcode/",z))
				if z then
					line$(z:z+6)="\"
					z+=1
				end if
			loop while z

			y+=len(line$)+2
			if line$(1:1)=chr$(12) then ! and y<lrec20 then   !  shifted this on 1/13/2017 due to strange _ showing up in ms word
				if y<lrec20 then
					pr #hOut: "\page"
				end if
				line$(1:1)=''
			end if
			pr #hOut: line$&"\par"
		goto SrRead ! /r

		end if
		SrRead: !
	loop
	SpInsertThatFileHere: ! r:
		close #hOut:
		execute 'Type "'&trim$(line$)&'" >>"'&env$('temp')&'\acs_print_tmp'&session$&'.rtf"'
		open #hOut=fngethandle: "Name=[Temp]\acs_print_tmp[session].rtf,RecL=800,use",display,output
	goto SrRead ! /r

	SrEoInput: !
	pr #hOut: "}"
	close #hOut:
	hOut=0
	close #20:
	! /r
	if env$('BR_MODEL')='CLIENT/SERVER' then
		fnCopy('[temp]\acs_print_tmp[session].rtf','[at]'&startRtf_destinationFileName$)
	end if
	fnCopy('[temp]\acs_print_tmp[session].rtf',startRtf_destinationFileName$)
	! pr 'BR copied to: '&'[at]'&startRtf_destinationFileName$ !
	if env$('BR_MODEL')='CLIENT/SERVER' then
		if ~setup_cs then
			setup_cs=1
			dim cache_sendto_path$*512
			dim cache_sendto_file_base$*256
			dim cache_sendto_file_ext$*128
			fnGetPp(startRtf_destinationFileName$,cache_sendto_path$,cache_sendto_file_base$,cache_sendto_file_ext$)
		!       pause
			startRtf_destinationFileName$=fn_reportCacheFolderCurrent$&'\'&cache_sendto_file_base$&cache_sendto_file_ext$
		!       pr 'CS set destination to: '&startRtf_destinationFileName$
		end if
	end if

	fnget_wordprocessor_exe(wordprocessor_exe$, forceWordProcessor$)
	wordprocessor_exe$=trim$(wordprocessor_exe$,'"')
	if saveToAsStart$<>'' then
		fnCopy(startRtf_destinationFileName$,env$('at')&saveToAsStart$)
	end if
	if fnprocess=1 and pos(lwrc$(wordprocessor_exe$),'atlantis')>0 then
		execute 'Sy -w "'&wordprocessor_exe$&'" -st /p /npd "'&os_filename$(startRtf_destinationFileName$)&'"' ! automatic processing  ! kj 53107
	else ! if print_report_nowait or fnprocess=1 then
		execute 'Sy -w -C "'&wordprocessor_exe$&'" "'&os_filename$(fnSrepEnv$(startRtf_destinationFileName$))&'"'
	! else
	!   fn_waitForWpToCloseStart('Word Processor')
	!   execute 'Sy -w '&wordprocessor_exe$&' "'&os_filename$(startRtf_destinationFileName$)&'"'
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

def fn_startReadProperties
	fnReadProgramPrintProperty('Orientation',orientation$, g_prgCapForSettingsOverride$)
	if orientation$="Landscape" then landscape$="Y" else landscape$="N"
	fnReadProgramPrintProperty('Height',pgh$, g_prgCapForSettingsOverride$) : pgh=val(pgh$)
	if pgh=0 then pgh=11
	fninch2twip(pgh)
	fnReadProgramPrintProperty('Width',pgw$, g_prgCapForSettingsOverride$) : pgw=val(pgw$)
	if pgw=0 then pgw=8.5
	fninch2twip(pgw)
	fnReadProgramPrintProperty('TopMargin',temp$, g_prgCapForSettingsOverride$) : marg(1)=val(temp$)
	fninch2twip(marg(1))
	fnReadProgramPrintProperty('BottomMargin',temp$, g_prgCapForSettingsOverride$) : marg(2)=val(temp$)
	fninch2twip(marg(2))
	fnReadProgramPrintProperty('LeftMargin',temp$, g_prgCapForSettingsOverride$) : marg(3)=val(temp$)
	fninch2twip(marg(3))
	fnReadProgramPrintProperty('RightMargin',temp$, g_prgCapForSettingsOverride$) : marg(4)=val(temp$)
	fninch2twip(marg(4))
	fnReadProgramPrintProperty('FontSize',temp$, g_prgCapForSettingsOverride$) : fsize=val(temp$)
fnend

def library fnSafeFilename$*256(sf_in$*256)
	fnSafeFilename$=fn_safeFilename$(sf_in$)
fnend
def fn_safeFilename$*256(sf_in$*256; enableBackSlash)
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

		if ~enableBackSlash then
			sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='\' : sf_replace_with$(sf_symbol_count)='-' ! back slash
		end if
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
	fn_safeFilename$=trim$(sf_in$)
fnend
include: Ertn
