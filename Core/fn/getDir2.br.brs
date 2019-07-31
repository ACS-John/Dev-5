! r: test area
	library program$: fngetdir2
	dim tmp_file_list$(1)*256
	fngetdir2('C:\ACS\Dev-5\ACSUB\Grid\',mat tmp_file_list$, '/s /b','*.*')
	pr mat tmp_file_list$(1:10) : pause 
	!
	dim fl$(1)*99
	pr 'function returns ';fngetdir2(env$('userprofile')&"\Desktop", mat fl$,'/s /tc','*.rtf',mat test_date$,mat test_time$,enable_full_path=1)
	for x=1 to udim(mat fl$) : pr '| '&test_date$(x)&' | '&test_time$(x)&' | '&fl$(x) : next x
	end  ! /r
! r: originally S:\Core\GetDir2.br
	! reads a directory into an array
	! ______________________________________________________________________
	! Dir$=Directory to Read
	! .            does not require but will accept a \ on the end
	! filename$(x)=file names (includes path if /s option is used)
	! option$: /s or /o-g or what ever you want use "dir /?"
	! .        at dos prompt for complete list of options.
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
def library fnGetDir2(dir$*256,mat filename$; option$,filter$*40,mat gd2_date$,mat gd2_time$,gd2_full_path,mat gd2_size)
	! r: library, on error, constants, initialize variables
		library 'S:\Core\Library': fnFree
		library 'S:\Core\Library': fnSrepEnv$
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnArrayWasPassedC
		library 'S:\Core\Library': fnArrayWasPassedN
		on error goto ERTN
		dim tmp$*512,directory_of$*256
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

	! /r
		dim fileList$*256
		if lwrc$(dir$(1:2))=lwrc$('s:') or lwrc$(dir$(1:len(env$('Q'))))=lwrc$(env$('Q')) then
			clientOrServer$='Server' ! server
			csat$=''
			csExeOption$=' -s' ! server
			fileList$=env$('temp')&'\GetDir'&session$&'.tmp'
		else
			clientOrServer$='Client' ! server
			csat$=env$('at')
			csExeOption$=' -@' ! client
			fileList$=env$('client_temp')&'\GetDir'&session$&'.tmp'
		end if
	! r: create temp text file by redirecting a shell called DIR command to it
		fnFree(csat$&fileList$)
		tmp$='Sy'&csExeOption$&' -M Dir '&option$&' "'&rtrm$(os_filename$(dir$),'\')&'\'&filter$&'" >"'&fileList$&'"'
		execute tmp$ ioerr XIT
	! /r
	! r: read the temp file into the dynamic-ly sizing array mat filename$
		open #tf1:=fngethandle: "Name="&csat$&fileList$,display,input  ioerr EO_TF1
		filename_count=line_count=0
		do 
			linput #tf1: tmp$ eof EO_TF1
			line_count+=1
			tmp$=rtrm$(tmp$)
			len_tmp=len(tmp$)
			! if line_count=1 then ! " Volume in drive C is TI106348W0B"
			! else if line_count=2 then ! " Volume Serial Number is D2FE-B614"
			!  else if trim$(tmp$(1:1))='' then ! one of the Totals lines at the end or once of the volume things at the top or the directory of line... pretty much anything besides a file hmmm
			! else if tmp$(1:5)='     ' then ! one of the Totals lines at the end
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
				!    else 
				!     filename$(filename_count)=filename$(filename_count)
				end if 
				if gd2_full_path then filename$(filename_count)=directory_of$&'\'&filename$(filename_count)
				!  else 
				!     pr tmp$ ! pause
			end if 
		loop 
	EO_TF1: ! /r
	gd2_return=filename_count
	! r: close and delete the temporary text file.  Return the number of files found
	! ______________________________________________________________________
	! if filename$(filename_count)='' then
	!   filename_count=filename_count-1
	!   mat filename$(filename_count)
	! end if
	XIT: ! 
	if env$('acsDebug')='Yes' then
		close #tf1: ioerr ignore
	else
		close #tf1,free: ioerr ignore
	end if
	 
	fngetdir2=gd2_return
fnend
include: ertn
! /r
