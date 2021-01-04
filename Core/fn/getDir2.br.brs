! r: test area
	library program$: fngetdir2


		dim fl$(0)*256
		dim test_date$(1)*32
		dim test_time$(1)*32
		pr 'function returns ';fngetdir2('S:\Core\FileIO\Layout\',mat fl$, '','*.fio',mat test_date$,mat test_time$)


	! dim tmp_file_list$(1)*256
	! fngetdir2('C:\ACS\Dev-5\ACSUB\Grid\',mat tmp_file_list$, '/s /b','*.*')
	! pr mat tmp_file_list$(1:10) : pause
	! !
	! dim fl$(1)*99
	! pr 'function returns ';fngetdir2(env$('userprofile')&"\Desktop", mat fl$,'/s /tc','*.rtf',mat test_date$,mat test_time$,enable_full_path=1)
	for x=1 to udim(mat fl$) : pr '| '&test_date$(x)&' | '&test_time$(x)&' | '&fl$(x) : next x
	end  ! /r

	! reads a directory into an array
	!
	! Dir$=Directory to Read
	!              does not require but will accept a \ on the end
	! filename$(x)=file names (includes path if /s option is used)
	! option$: /s or /o-g or what ever you want use "dir /?"
	!          at dos prompt for complete list of options.
	! r: DIR /?  (from Windows 7 Home Premium)
	! Displays a list of files and subdirectories in a directory.
	!
	! DIR [drive:][path][filename] [/A[[:]attributes]] [/B] [/C] [/D] [/L] [/N]
	!   [/O[[:]sortorder]] [/P] [/Q] [/R] [/S] [/T[[:]timefield]] [/W] [/X] [/4]
	!
	!   [drive:][path][filename]
	!               Specifies drive, directory, and/or files to list.
	!
	!   /A          Displays files with specified attributes.
	!   attributes   D  Directories                R  Read-only files
	!                H  Hidden files               A  Files ready for archiving
	!                S  System files               I  Not content indexed files
	!                L  Reparse Points             -  Prefix meaning not
	!   /L          Uses lowercase.
	!  THE DEFAULT   /N          New long list format where filenames are on the far right.
	!   /O          List by files in sorted order.
	!   sortorder    N  By name (alphabetic)       S  By size (smallest first)
	!                E  By extension (alphabetic)  D  By date/time (oldest first)
	!                G  Group directories first    -  Prefix to reverse order
	!   /P          Pauses after each screenful of information.
	!   /S          Displays files in specified directory and all subdirectories.
	!   /T          Controls which time field displayed or used for sorting
	!   timefield   C  Creation
	!               A  Last Access
	!               W  Last Written
	!   /X          This displays the short names generated for non-8dot3 file
	!               names.  The format is that of /N with the short name inserted
	!               before the long name. If no short name is present, blanks are
	!               displayed in its place.
	!   /4          Displays four-digit years
	! Switches may be preset in the DIRCMD environment variable.  Override
	! preset switches by prefixing any switch with - (hyphen)--for example, /-W.
! /r
dim tmp$*512
dim directory_of$*256
def library fnGetDir2(dir$*256,mat filename$; option$,filter$*40,mat gd2_date$,mat gd2_time$,gd2_full_path,mat gd2_size)
		fn_setup
		on error goto Ertn
		dir$=fnSrepEnv$(dir$)
		filter$=fnSrepEnv$(filter$)
		if pos(lwrc$(option$),'/s')>0 then gd2_full_path=1
		if pos(lwrc$(option$),'/b')>0 then slash_b=1 else slash_b=0
		mat filename$(0)
		gd2_return=0
		filter$=trim$(filter$) : if filter$="" then filter$="*.*"
		option$=trim$(option$)
		dir$=trim$(dir$)
		if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
		mat filename$(0)
		gd2_date_requested=fnArrayWasPassedC(mat gd2_date$)
		if gd2_date_requested then
			directory_of$=os_filename$(dir$)
			if slash_b then
				pr 'DIR /B does not return dates - either enhance fngetdir2 or change your call'
				pause
			end if
		end if
		! if udim(mat gd2_time$)>0 then gd2_time_requested=1 else gd2_time_requested=0
		gd2_time_requested=fnArrayWasPassedC(mat gd2_time$)
		! if udim(mat gd2_size)>0 then gd2_size_requested=1 else gd2_size_requested=0
		gd2_size_requested=fnArrayWasPassedN(mat gd2_size)

		dim fileList_br$*256
		dim fileList_os$*256
		if lwrc$(dir$(1:2))=lwrc$('s:') or lwrc$(dir$(1:len(env$('Q'))))=lwrc$(env$('Q')) then
			if env$('cursys')='CM' then
				clientOrServer$='Server'
				csat$=''
				csExeOption$=' -s' ! server
				fileList_br$='F:\clsinc\temp\GetDir'&session$&'.tmp'
				fileList_os$=os_filename$('F:\')&'\clsinc\temp\GetDir'&session$&'.tmp'
			else
				clientOrServer$='Server'
				csat$=''
				csExeOption$=' -s' ! server
				fileList_br$=env$('temp')&'\GetDir'&session$&'.tmp'
				fileList_os$=env$('temp')&'\GetDir'&session$&'.tmp'
			end if
		else
			clientOrServer$='Client'
			csat$=env$('at')
			csExeOption$=' -@' ! client
			fileList_br$=env$('client_temp')&'\GetDir'&session$&'.tmp'
			fileList_os$=env$('client_temp')&'\GetDir'&session$&'.tmp'
		end if

		fnFree(csat$&fileList_br$)
		tmp$='Sy'&csExeOption$&' -M Dir '&option$&' "'&rtrm$(os_filename$(dir$),'\')&'\'&filter$&'" >"'&fileList_os$&'"'
		execute tmp$ ioerr Xit

		open #tf1=fnH: "Name="&csat$&fileList_br$,display,input  ioerr Gd2_openReturnFailure
		filename_count=line_count=0
		do
			linput #tf1: tmp$ eof EO_TF1
			line_count+=1
			tmp$=rtrm$(tmp$)
			len_tmp=len(tmp$)

			if tmp$(3:3)='/' and tmp$(6:6)='/' then
				if pos(tmp$(7:10),' ')>0 then
					date_format_len=8
				else
					date_format_len=10
				end if
			end if
			pos_filename=date_format_len+30
			if slash_b then
				mat filename$(filename_count+=1)
				filename$(filename_count)=rtrm$(tmp$)
			else if tmp$(1:14)=' Directory of ' then
				directory_of$=tmp$(15:len_tmp)
			else if len_tmp and trim$(tmp$(1:1))<>'' and tmp$(pos_filename:pos_filename)<>'.' then ! if not blank (and does not start with a space) then add it to the list of files.
				mat filename$(filename_count+=1)
				filename$(filename_count)=rtrm$(tmp$(pos_filename:len_tmp))
				if gd2_date_requested then
					mat gd2_date$(filename_count)
					gd2_date$(filename_count)=rtrm$(tmp$(1:date_format_len))
					if date_format_len=8 then
						gd2_date$(filename_count)=date$(days(gd2_date$(filename_count),'mm/dd/yy'),'mm/dd/ccyy')
					end if
				end if
				if gd2_time_requested then
					mat gd2_time$(filename_count)
					gd2_time$(filename_count)=rtrm$(tmp$(date_format_len+3:date_format_len+10))
				end if

				if gd2_size_requested then
					mat gd2_size(filename_count)
					gd2_size(filename_count)=val(srep$(tmp$(date_format_len+11:date_format_len+28),',',''))
				end if
				filename$(filename_count)=rtrm$(tmp$(pos_filename:len_tmp))
				if filename$(filename_count)=uprc$(filename$(filename_count)) then ! never all caps-anything
					filename$(filename_count)=lwrc$(filename$(filename_count))
				end if
				if gd2_full_path then filename$(filename_count)=directory_of$&'\'&filename$(filename_count)
			end if
		loop
	Gd2_openReturnFailure: !
		pr 'line executed:'
		pr 'tmp$=';tmp$
		pr 'failed to open dir file: error '&str$(err)&' on line '&str$(line)&' in '&program$
		if env$('debug')<>'' or env$('acsDeveloper')<>'' then pause
	goto EO_TF1
	EO_TF1: !
	gd2_return=filename_count
	Xit: !
	if env$('acsDebug')='Yes' then
		close #tf1: ioerr ignore
	else
		close #tf1,free: ioerr ignore
	end if

	fngetdir2=gd2_return
fnend

def library fnGetDirClient(dir$*256,mat filename$; filter$*40, ___,returnN,hGdc,gdcFileList$*256)
	fn_setup
	on error goto Ertn
	dir$=fnSrepEnv$(dir$)
	mat filename$(0)
	filter$=trim$(filter$) : if filter$='' then filter$='*.*'
	dir$=trim$(dir$)
	if dir$(len(dir$):len(dir$))<>'\' then dir$=dir$&'\'
	mat filename$(0)
	gdcFileList$=env$('client_temp')&'\GetDir'&session$&'.tmp'
	fnFree(env$('at')&gdcFileList$)
	tmp$='Dir "'&env$('at')&rtrm$(dir$,'\')&'\'&filter$&'" >"'&gdcFileList$&'" -B'
	execute tmp$ ioerr Gdc_Finis
	open #hGdc=fnH: "Name="&gdcFileList$,display,input ioerr Gdc_openReturnFailure
	filename_count=line_count=0
	linput #hGdc: tmp$ eof Gdc_EO_hGdc ! consume Directory of ...
	linput #hGdc: tmp$ eof Gdc_EO_hGdc ! consume .
	linput #hGdc: tmp$ eof Gdc_EO_hGdc ! consume ..
	do
		linput #hGdc: tmp$ eof Gdc_EO_hGdc
		line_count+=1
		tmp$=rtrm$(tmp$)
		mat filename$(filename_count+=1)
		filename$(filename_count)=rtrm$(tmp$(1:len(tmp$)))
	loop
	Gdc_openReturnFailure: !
		pr 'line executed:'
		pr 'tmp$=';tmp$
		pr 'fnGetDirClient failed to open dir file: error '&str$(err)&' on line '&str$(line) ! &' in '&program$
		pause ! if env$('debug')<>'' or env$('acsDeveloper')<>'' then pause
	goto Gdc_EO_hGdc
	Gdc_EO_hGdc: !
	if filename_count>0 and pos(filename$(filename_count),'Kilobytes Used,')>0 then ! remove the "### Files, ### Kilobytes..." line from the end
		filename_count-=1
		mat filename$(filename_count)
	end if
	returnN=filename_count
	! pause
	Gdc_Finis: !
	close #hGdc,free: ioerr ignore
	fnGetDirClient=returnN
fnend

include: fn_setup
