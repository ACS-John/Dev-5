00010 ! r: test area
00020   library program$: fngetdir2
00022   dim tmp_file_list$(1)*256
00023   fngetdir2('C:\ACS\Dev-5\ACSUB\Grid\',mat tmp_file_list$, '/s /b','*.*')
00024   print mat tmp_file_list$(1:10) : pause 
00025 ! 
00030   dim fl$(1)*99
00050   print 'function returns ';fngetdir2(env$('userprofile')&"\Desktop", mat fl$,'/s /tc','*.rtf',mat test_date$,mat test_time$,enable_full_path=1)
00060   for x=1 to udim(mat fl$) : print '| '&test_date$(x)&' | '&test_time$(x)&' | '&fl$(x) : next x
00070   end  ! /r
08010 ! r: originally S:\Core\GetDir2.br
08020 ! reads a directory into an array
08030 ! ______________________________________________________________________
08100 ! Dir$=Directory to Read
08120 ! .            does not require but will accept a \ on the end
08140 ! filename$(x)=file names (includes path if /s option is used)
08160 ! option$: /s or /o-g or what ever you want use "dir /?"
08180 ! .        at dos prompt for complete list of options.
08200 ! r: DIR /?  (from Windows 7 Home Premium)
08202 ! Displays a list of files and subdirectories in a directory.
08204 ! 
08206 ! DIR [drive:][path][filename] [/A[[:]attributes]] [/B] [/C] [/D] [/L] [/N]
08208 !   [/O[[:]sortorder]] [/P] [/Q] [/R] [/S] [/T[[:]timefield]] [/W] [/X] [/4]
08210 ! 
08212 !   [drive:][path][filename]
08214 !               Specifies drive, directory, and/or files to list.
08216 ! 
08218 !   /A          Displays files with specified attributes.
08220 !   attributes   D  Directories                R  Read-only files
08222 !                H  Hidden files               A  Files ready for archiving
08224 !                S  System files               I  Not content indexed files
08226 !                L  Reparse Points             -  Prefix meaning not
08236 !   /L          Uses lowercase.
08238 !  THE DEFAULT   /N          New long list format where filenames are on the far right.
08240 !   /O          List by files in sorted order.
08242 !   sortorder    N  By name (alphabetic)       S  By size (smallest first)
08244 !                E  By extension (alphabetic)  D  By date/time (oldest first)
08246 !                G  Group directories first    -  Prefix to reverse order
08248 !   /P          Pauses after each screenful of information.
08254 !   /S          Displays files in specified directory and all subdirectories.
08256 !   /T          Controls which time field displayed or used for sorting
08258 !   timefield   C  Creation
08260 !               A  Last Access
08262 !               W  Last Written
08266 !   /X          This displays the short names generated for non-8dot3 file
08268 !               names.  The format is that of /N with the short name inserted
08270 !               before the long name. If no short name is present, blanks are
08272 !               displayed in its place.
08274 !   /4          Displays four-digit years
08276 ! Switches may be preset in the DIRCMD environment variable.  Override
08278 ! preset switches by prefixing any switch with - (hyphen)--for example, /-W.
08280 ! /r
09000   def library fngetdir2(dir$*256,mat filename$; option$,filter$*40,mat gd2_date$,mat gd2_time$,gd2_full_path,mat gd2_size)
09020 ! r: library, on error, constants, initialize variables
09040     library 'S:\Core\Library': fngethandle,fnerror,fnFree
10060     on error goto ERTN
10220     dim tmp$*512,directory_of$*256
10230     if pos(lwrc$(option$),'/s')>0 then gd2_full_path=1
10232     if pos(lwrc$(option$),'/b')>0 then slash_b=1 else slash_b=0
10240     mat filename$(0)
12000     gd2_return=0
12020     filter$=trim$(filter$) : if filter$="" then filter$="*.*"
12040     option$=trim$(option$)
12060     dir$=trim$(dir$)
12080     if dir$(len(dir$):len(dir$))<>"\" then dir$=dir$&"\"
12090     mat filename$(0)
13000     if udim(mat gd2_date$)>0 then 
13020       gd2_date_requested=1
13040       directory_of$=os_filename$(dir$)
13060     else 
13080       gd2_date_requested=0
13100     end if 
13110     if gd2_date_requested and slash_b then print 'DIR /B does not return dates - either enhance fngetdir2 or change your call' : pause 
13120     if udim(mat gd2_time$)>0 then gd2_time_requested=1 else gd2_time_requested=0
13130     if udim(mat gd2_size)>0 then gd2_size_requested=1 else gd2_size_requested=0
13140 ! /r
13500     dim fileList$*256
13520     if lwrc$(dir$(1:2))=lwrc$('s:') or lwrc$(dir$(1:len(env$('Q'))))=lwrc$(env$('Q')) then
13540       serverOrClient$=' -s' ! server
13560       fileList$=env$('temp')&'\GetDir'&session$&'.tmp'
13580     else
13600       serverOrClient$=' -@' ! client
13620       fileList$=env$('at')&env$('client_temp')&'\GetDir'&session$&'.tmp'
13640     end if
14000 ! r: create temp text file by redirecting a shell called DIR command to it
14020     fnFree(fileList$)
14140     tmp$='Sy'&serverOrClient$&' -M Dir '&option$&' "'&rtrm$(os_filename$(dir$),'\')&'\'&filter$&'" >"'&fileList$&'"'
14160     execute tmp$ ioerr XIT
14180 ! /r
16000 ! r: read the temp file into the dynamic-ly sizing array mat filename$
16040     open #tf1:=fngethandle: "Name="&fileList$,display,input
16140     filename_count=line_count=0
18000     do 
18020       linput #tf1: tmp$ eof EO_TF1
18040       line_count+=1
18060       tmp$=rtrm$(tmp$)
18080       len_tmp=len(tmp$)
18100 ! if line_count=1 then ! " Volume in drive C is TI106348W0B"
18120 ! else if line_count=2 then ! " Volume Serial Number is D2FE-B614"
18140 !  else if trim$(tmp$(1:1))='' then ! one of the Totals lines at the end or once of the volume things at the top or the directory of line... pretty much anything besides a file hmmm
18160 ! else if tmp$(1:5)='     ' then ! one of the Totals lines at the end
18500       if tmp$(3:3)='/' and tmp$(6:6)='/' then 
18520         if pos(tmp$(7:10),' ')>0 then 
18540           date_format_len=8
18560         else 
18580           date_format_len=10
18600         end if 
18620       end if 
18700       pos_filename=date_format_len+30
19000       if slash_b then 
19020         mat filename$(filename_count+=1)
19040         filename$(filename_count)=rtrm$(tmp$)
19060       else if tmp$(1:14)=' Directory of ' then 
19080         directory_of$=tmp$(15:len_tmp)
20000       else if len_tmp and trim$(tmp$(1:1))<>'' and tmp$(pos_filename:pos_filename)<>'.' then ! if not blank (and does not start with a space) then add it to the list of files.
20020         mat filename$(filename_count+=1)
20040         filename$(filename_count)=rtrm$(tmp$(pos_filename:len_tmp))
20100         if gd2_date_requested then 
20120           mat gd2_date$(filename_count)
20140           gd2_date$(filename_count)=rtrm$(tmp$(1:date_format_len))
20160           if date_format_len=8 then 
20180             gd2_date$(filename_count)=date$(days(gd2_date$(filename_count),'mm/dd/yy'),'mm/dd/ccyy')
20200           end if 
20220         end if 
30000         if gd2_time_requested then 
30020           mat gd2_time$(filename_count)
30040           gd2_time$(filename_count)=rtrm$(tmp$(date_format_len+3:date_format_len+10))
30060         end if 

30070         if gd2_size_requested then 
30072           mat gd2_size(filename_count)
30075           gd2_size(filename_count)=val(srep$(tmp$(date_format_len+11:date_format_len+28),',',''))
30076         end if 
31000         filename$(filename_count)=rtrm$(tmp$(pos_filename:len_tmp))
31020         if filename$(filename_count)=uprc$(filename$(filename_count)) then ! never all caps-anything
31040           filename$(filename_count)=lwrc$(filename$(filename_count))
31060 !    else 
31080 !     filename$(filename_count)=filename$(filename_count)
31100         end if 
31120         if gd2_full_path then filename$(filename_count)=directory_of$&'\'&filename$(filename_count)
31500 !  else 
31520 !     print tmp$ ! pause
31540       end if 
31560     loop 
32000 EO_TF1: ! /r
32020     gd2_return=filename_count
32040 ! r: close and delete the temporary text file.  Return the number of files found
32060 ! ______________________________________________________________________
32080 ! if filename$(filename_count)='' then
32100 !   filename_count=filename_count-1
32120 !   mat filename$(filename_count)
32140 ! end if
40000 XIT: ! 
40020     if env$('acsDebug')='Yes' then
40040       close #tf1: ioerr ignore
40060     else
40080       close #tf1,free: ioerr ignore
40100     end if
40120      
40140     fngetdir2=gd2_return
40160   fnend 
60000 IGNORE: continue 
60020 ! <Updateable Region: ERTN>
60040 ERTN: fnerror(program$,err,line,act$,"xit")
60060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
60080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
60120 ERTN_EXEC_ACT: execute act$ : goto ERTN
60140 ! /region
90140 ! /r
