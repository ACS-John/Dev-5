10000 ! Replace S:\Core\Print.br
10010 ! open the report, printer, etc...
10020 ! ______________________________________________________________________
12000 def fn_setup
12020   on error goto ERTN
12040   if ~setup then 
12060     let setup=1
12080     library 'S:\Core\Library': fnerror,fnread_program_print_property,fnCopy,fnreg_read,fnureg_read,fnSystemName$,fngetpp
12100     library 'S:\Core\Library': fnosver,fnget_wordprocessor_exe,fninch2twip,fnprocess,fngethandle,fnstatus_close
12110     library 'S:\Core\Library': fnmakesurepathexists
12112     ! library 'S:\Core\Library': fntos,fnlbl,fntxt,fnacs,fnopt,fncmdset
12120     ! 
12140     let fnreg_read('Report_Cache',report_cache$,'True')
12160     if report_cache$='True' then let print_report_caching=1 else let print_report_caching=0
12180     let fnureg_read('wait_wp_close',wait_wp_close$,'True')
12200     if wait_wp_close$='False' then let print_report_nowait=1 else let print_report_nowait=0
12260   end if 
12340 fnend 
22000 def library fnprint_file_name$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
22020   let fn_setup
22040   let fnprint_file_name$=fn_print_file_name$( pfn_sendto_base_name_addition$,pfn_extension$,programCaptionOverride$)
22060 fnend 
24000 def fn_print_file_name$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
24010   dim pfnReturn$*1024
24020   let pfn_extension$=trim$(pfn_extension$,'.')
24040   if pfn_extension$='' then let pfn_extension$='rtf'
24060   if print_report_caching then 
24070     if programCaptionOverride$='' then let programCaptionOverride$=env$('Program_Caption')
24200     let pfnReturn$=fn_report_cache_folder_current$&'\'&fn_safe_filename$(programCaptionOverride$)
24210     pfn_sendto_base_name_addition$=trim$(fn_safe_filename$(pfn_sendto_base_name_addition$))
24220     if pfn_sendto_base_name_addition$<>'' then 
24240       let pfnReturn$=pfnReturn$&' '&pfn_sendto_base_name_addition$
24260     end if 
24280     let pfnReturn$=pfnReturn$&' - '&date$('ccyy-mm-dd')&' '&fn_safe_filename$(time$)
24300     let pfnReturn$=pfnReturn$&'.'&pfn_extension$
24320   else 
24340     let pfnReturn$=env$('temp')&'\ACS-'&session$&'.'&pfn_extension$
24360   end if 
24380   let fn_print_file_name$=pfnReturn$
24400 fnend 
25000 def library fnreport_cache_folder_current$*512
25020   let fn_setup
25040   fnreport_cache_folder_current$=fn_report_cache_folder_current$
25060 fnend
25080 def fn_report_cache_folder_current$*512
25100   dim report_cache_base$*256
25120   ! dim client_report_cache$*256
25140   if env$('BR_MODEL')='CLIENT/SERVER' then
25160     if env$('enableReportCacheOnClient')='Yes' then
25180       report_cache_base$='C:\ProgramData\ACS\Report Cache'
25200     else
25220       fnureg_read('CS Client Report Cache',report_cache_base$)
25240     end if
25260   else
25280     report_cache_base$=os_filename$(env$('Q')&'\Report Cache')
25300   end if
25320   if report_cache_base$='' then let report_cache_base$=os_filename$(env$('Q')&'\Report Cache')
25340   dim tmp_dir$*512
25360   let tmp_dir$=report_cache_base$&'\'&fnSystemName$
25380   let tmp_dir$=tmp_dir$&'\'&fn_safe_filename$(env$('cnam'))&' ('&env$('cno')&')'
25400   ! pr 'report_cache_base$='&report_cache_base$ : pr 'tmp_dir$='&tmp_dir$ : pause
25420   if env$('enableReportCacheOnClient')='Yes' then
25440     fnmakesurepathexists(env$('at')&tmp_dir$&'\tmp.txt')
25460   else
25480     fnmakesurepathexists(tmp_dir$&'\tmp.txt')
25500   end if
25520   fn_report_cache_folder_current$=tmp_dir$
25540 fnend
26000 def library fnopen_receipt_printer(; orp_only_if_it_is_assigned)
26020   let fn_setup
26040   fnopen_receipt_printer=fn_open_receipt_printer( orp_only_if_it_is_assigned)
26060 fnend 
26080 def fn_open_receipt_printer(; orp_only_if_it_is_assigned)
26100   dim orp_receipt_printer$*256
26120   let fnureg_read('Printer.Receipt',orp_receipt_printer$)
26140   orp_did_open=0
26160   if orp_receipt_printer$='' then 
26180     if ~orp_only_if_it_is_assigned then 
26200       let fn_openprn
26220       orp_did_open=1
26240     end if
26260   else 
26280     open #255: 'Name=Prn:/'&orp_receipt_printer$,display,output ioerr ORP_FINIS
26300     orp_did_open=1
26320   end if 
26330   ORP_FINIS: !
26340   fn_open_receipt_printer=orp_did_open
26360 fnend 
27000 def library fnclose_receipt_printer
27020   let fn_setup
27040   fnclose_receipt_printer=fn_close_receipt_printer
27060 fnend 
27080 def fn_close_receipt_printer
27100   close #255: ioerr ignore
27120 fnend 
28000 def library fnopen_cash_drawer
28020   let fn_setup
28040   if fn_open_receipt_printer(1) then
28060     print #255,using 'form pos 1,c 9,skip 0': hex$("1B70302828") ioerr ignore
28080     let fn_close_receipt_printer
28100   end if
28120 fnend 

29000 def library fnopenprn(;xx,xxxxx,xxxxxxx,process,sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
29020   let fn_setup
29040   let fnopenprn=fn_openprn( process,sendto_base_name_addition$,prgCapForSettingsOverride$,programCaptionOverride$)
29060 fnend 
30000 def fn_openprn(;process, sendto_base_name_addition$*128,prgCapForSettingsOverride$*256,programCaptionOverride$*256)
30020   if file(255)<>-1 then goto XIT
30040   dim g_prgCapForSettingsOverride$*256
30060   g_prgCapForSettingsOverride$=prgCapForSettingsOverride$
30080   dim op_printFileName$*1024
30090   let op_printFileName$=fn_print_file_name$( sendto_base_name_addition$,'',programCaptionOverride$) ! ,pfn_extension$)
30100   ! 
30120   let fnread_program_print_property("Lines",lpp$, g_prgCapForSettingsOverride$) ! let lpp=val(lpp$)
30140   if lpp$='' then gosub SET_DEFAULTS
30180   dim g_prn_destination_name$*1024
30200   let g_prn_destination_name$=op_printFileName$
30220   open #255: 'Name='&env$('Q')&'\tmp_'&session$&'.prn,PageOFlow='&lpp$&',RecL=512,Replace',display,output 
30240   goto XIT
30260   SET_DEFAULTS: ! r:
30280     print "Lines settings for this program were not found."
30300     print "Default Settings will be used."
30320     print "  Lines: 54"
30340     let lpp$='54'
30360   return  ! /r
30400 XIT: fnend 
38000   def library fncloseprn(;forceWordProcessor$)
38020     let fn_setup
38030     dim cp_destinationFileName$*1024
38040     if file(255)<>-1 then ! if the printer file is open.
38060       let cp_destinationFileName$=g_prn_destination_name$ ! trim$(file$(255)(1:1024))
38080       close #255: 
38100       if fnCopy(env$('Q')&'\tmp_'&session$&'.prn',g_prn_destination_name$) then 
38110         execute '*free '&env$('Q')&'\tmp_'&session$&'.prn' ! pr g_prn_destination_name$ : pause
38120       else
38130         print 'copy failed.'
38140         pause 
38160       end if 
38180       let fnstatus_close
38200       let fn_start(cp_destinationFileName$, 0,forceWordProcessor$)
38220     end if 
38400     g_prgCapForSettingsOverride$=''
38420   fnend 
42000   def fn_start(start_destinationFilename$*1024; nodrop,forceWordProcessor$)
42040     on error goto START_ERTN
42060 ! ______________________________________________________________________
42080 ! NoDrop    = 1 = Do not delete the file when your done with it.
42100 ! ______________________________________________________________________
42120     dim winxp$*20,win2k$*22,osver$*80,temp$*120,winnt2kxp$*28
42140     dim landscape$*1
42160     dim marg(4)
42180     dim wordprocessor_exe$*512 ! full path and executable for wordprocessor_exe
42200 ! ______________________________________________________________________
42260     let start_destinationFilename$=trim$(start_destinationFilename$)
42280     let winxp$="Microsoft Windows XP"
42300     let win2k$="Microsoft Windows 2000"
42320     let winnt2kxp$="Microsoft Windows NT/2000/XP"
42340     let fnosver(osver$,1)
42360 ! if  start_destinationFilename$='CANCELED' then goto START_XIT
42380     if lwrc$(start_destinationFilename$(len(start_destinationFilename$)-3:len(start_destinationFilename$)))=".rtf" then 
42400       let fn_start_rtf(start_destinationFilename$, forceWordProcessor$)
42420     else if osver$=winxp$ or osver$=win2k$ or osver$=winnt2kxp$ then 
42440       let fn_start_winxp
42460     else 
42480       pr 'win 98 no longer supported.' : pause ! let fn_start_win9x
42500     end if 
42520 DROPIT: ! 
42540     if ~print_report_nowait and ~print_report_caching and nodrop<>1 then 
42560       execute 'Drop "'&clientSendto$&'" -N' ioerr DROPIT ! empties the contents of clientSendto$
42580     end if 
42600     goto START_XIT ! _______
42620 ! ______________________________________________________________________
42640 START_ERTN: ! 
42660     if err=4591 then let fn_start_workaround_4591 : continue  ! line added for time-outs
42680     let fnerror(program$,err,line,act$,"start_xit")
42700     if lwrc$(act$)<>"pause" then goto START_ERTN_EXEC_ACT
42720     execute "List -"&str$(line) : pause : goto START_ERTN_EXEC_ACT
42740     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto START_ERTN_EXEC_ACT
42760 START_ERTN_EXEC_ACT: execute act$ : goto START_ERTN
42780 ! ______________________________________________________________________
42800 START_XIT: ! 
42810     on error goto ERTN
42820   fnend 
46000   def fn_start_rtf(startRtf_destinationFileName$*1024; forceWordProcessor$)
46020     dim line$*32000
46040     let fn_start_read_properties
46060     ! r:  make the temp rtf file
46070     dim clientSendto$*1024
46072     dim serverSendto$*1024
46080     clientSendto$=env$('at')&startRtf_destinationFileName$
46090     serverSendto$=startRtf_destinationFileName$
46100     !   open #20: "Name="&clientSendto$,display,input
46120     ! else
46140     open #20: "Name="&serverSendto$,display,input
46160     ! end if
46180     let lrec20=lrec(20)
46200     let y=0
46220     open #21: "Name="&env$('temp')&"\acs_print_tmp"&session$&".rtf,Size=0,RecL=800,Replace",display,output 
46240     print #21: "{\rtf1\ansi\deflang1033";
46260     print #21: "{\fonttbl";
46280     print #21: "{\f8\fswiss\fcharset0\fprq2 Lucida Console;}";
46300     print #21: "{\f181\froman\fcharset0\fprq2 Times New Roman;}";
46320     print #21: "{\f128\fnil\fcharset0\fprq2 iQs Code 128;}";
46340     print #21: "}"
46360     print #21: "{\stylesheet";
46380     print #21: "{\snext0\f8\fs22\fi0\li0\ri0\ql";
46400     print #21: "{\*\stloverrides\f8\fs22\widctlpar} Normal;}"
46420     print #21: "{\s1\sbasedon0\snext0\f8\fs28\b\kerning28\fi0\li0\ri0";
46440     print #21: "\ql\keepn\sb240\sa60{\*\stloverrides\fs28\b\kerning28";
46460     print #21: "\keepn\sb240\sa60} heading 1;}" ! ;
46480     print #21: "{\s2\sbasedon0\snext0\f8\fs24\b\i\fi0\li0\ri0\ql\keepn";
46500     print #21: "\sb240\sa60{\*\stloverrides\fs24\b\i\keepn\sb240\sa60}";
46520     print #21: " heading 2;}" ! ;
46540     print #21: "{\s3\sbasedon0\snext0\f8\fs22\b\fi0\li0\ri0\ql\keepn";
46560     print #21: "\sb240\sa60{\*\stloverrides\b\keepn\sb240\sa60} ";
46580     print #21: "heading 3;}" ;
46600     print #21: "}"
46620     print #21: "{\info";
46640     print #21: "{\creatim\yr"&date$("ccyy")&"\mo"&date$("mm");
46660     print #21: "\dy"&date$("dd")&"\hr"&time$(1:2)&"\min"&time$(4:5);
46680     print #21: "\sec"&time$(7:8)&"}";
46700     print #21: "{\author ACS 5 - "&login_name$&"}";
46720     print #21: "}"
46740 ! _______
46760     print #21: "\paperw"&str$(pgw)&"\paperh"&str$(pgh);
46780     print #21: "\margl"&str$(marg(3))&"\margr"&str$(marg(4));
46800     print #21: "\margt"&str$(marg(1))&"\margb"&str$(marg(2));
46820     if uprc$(landscape$)=uprc$("Y") then 
46840       print #21: "\lndscpsxn";
46860     end if 
46880     print #21: "\widowctrl\useltbaln\plain"
46900     print #21: "\f181\fs24\pard\f8\fs"&str$(fsize*2)&" "
46920 L640: linput #20: line$ eof END_OF_FILE
46940     if uprc$(line$(1:13))=uprc$("*INSERT FILE:") then 
46960       let line$=line$(14:len(line$))
46980       goto L660
47000     else if line$(1:12)="S:\Core\images\" then 
47020       goto L660
47040     else 
47060       goto L700
47080     end if 
47100     L660: ! 
47120     close #21: 
47140     execute "Type '"&trim$(line$)&"' >>"&env$('temp')&"\acs_print_tmp"&session$&".rtf"
47160     open #21: "Name="&env$('temp')&"\acs_print_tmp"&session$&".rtf,RecL=800,use",display,output 
47180     goto L640
47200     L700: ! 
47220     ! 
47240     ! 
47260     L730: ! 
47280     let z=pos(line$,"\",z)
47300     if z=>1 and line$(z-1:z-1)<>"{" and uprc$(line$(z+1:z+1))<>"Q" then 
47320       let line$(z:z)="\\" : let z=z+2
47340       goto L730
47360     else 
47380     L18700: !
47390       let z=pos(line$,"/fcode/",z)
47400       if z=>1 then 
47420         let line$(z:z+6)="\" : let z=z+1
47440         goto L18700
47460       else 
47480         let z=0
47500       end if 
47520      !  look for \s and replace with double \\s
47540      !  so they will display correctly in rtf
47560      !  "{" was added to allow support for bold, italic, underline, etc.
47580      !  "q" was added to allow support for alignment.
47600     end if 
47620     let y=y+len(line$)+2
47640     if line$(1:1)=chr$(12) then ! and y<lrec20 then   !  shifted this on 1/13/2017 due to strange _ showing up in ms word
47660       if y<lrec20 then  
47680         print #21: "\page"
47700       end if
47720       let line$(1:1)=''
47740     end if 
47760     print #21: line$&"\par"
47780     goto L640
47800     ! ______________________________________________________________________
49000     END_OF_FILE: ! 
49020     print #21: "}"
49040     close #21: 
49060     close #20: 
49080     ! /r
49100     if env$('enableReportCacheOnClient')='Yes' then
49120       let fnCopy(env$('temp')&"\acs_print_tmp"&session$&".rtf",clientSendto$)
49130     end if
49140     let fnCopy(env$('temp')&"\acs_print_tmp"&session$&".rtf",serverSendto$)
49200     ! pr 'BR copied to: '&clientSendto$ ! 
49220     if env$('BR_MODEL')='CLIENT/SERVER' then
49240       if ~setup_cs then
49260         setup_cs=1
49280         dim cache_sendto_path$*512
49300         dim cache_sendto_file_base$*256
49320         dim cache_sendto_file_ext$*128
49340         fngetpp(serverSendto$,cache_sendto_path$,cache_sendto_file_base$,cache_sendto_file_ext$)
49360 !       pause
49380         serverSendto$=fn_report_cache_folder_current$&'\'&cache_sendto_file_base$&cache_sendto_file_ext$
49400 !       pr 'CS set destination to: '&serverSendto$
49420       end if
49440     end if
49460 !
50000     fnget_wordprocessor_exe(wordprocessor_exe$, forceWordProcessor$) 
50020     if fnprocess=1 and pos(lwrc$(wordprocessor_exe$),'atlantis')>0 then 
50040       execute 'Sy -w '&wordprocessor_exe$&' -st /p /npd "'&os_filename$(serverSendto$)&'"' ! automatic processing  ! kj 53107
50060     else if print_report_nowait or fnprocess=1 then 
50080       execute 'Sy -w -C '&wordprocessor_exe$&' "'&os_filename$(serverSendto$)&'"'
50100     else 
50120       let acs_win_rows=val(env$('acs_win_rows'))
50140       let acs_win_cols=val(env$('acs_win_cols'))
50160       open #h_win_wait=fngethandle: "srow=1,scol=1,rows="&str$(acs_win_rows)&",cols="&str$(acs_win_cols)&",border=none,picture=S:\Core\disable.png:TILE",display,output 
50180       print #h_win_wait,fields str$(acs_win_rows/2)&',1,Cc '&str$(acs_win_cols)&',[Screen]': 'Close your word processor to continue.'
50200       execute 'Sy -w '&wordprocessor_exe$&' "'&os_filename$(serverSendto$)&'"'
50220       close #h_win_wait: 
50240     end if 
50260 ! pause
50280   fnend 
52000   def fn_start_read_properties
52020     let fnread_program_print_property('Orientation',orientation$, g_prgCapForSettingsOverride$)
52040     if orientation$="Landscape" then let landscape$="Y" else let landscape$="N"
52060     let fnread_program_print_property('Height',pgh$, g_prgCapForSettingsOverride$) : let pgh=val(pgh$)
52080     if pgh=0 then let pgh=11
52100     let fninch2twip(pgh)
52120     let fnread_program_print_property('Width',pgw$, g_prgCapForSettingsOverride$) : let pgw=val(pgw$)
52140     if pgw=0 then let pgw=8.5
52160     let fninch2twip(pgw)
52180     let fnread_program_print_property('TopMargin',temp$, g_prgCapForSettingsOverride$) : let marg(1)=val(temp$)
52200     let fninch2twip(marg(1))
52220     let fnread_program_print_property('BottomMargin',temp$, g_prgCapForSettingsOverride$) : let marg(2)=val(temp$)
52240     let fninch2twip(marg(2))
52260     let fnread_program_print_property('LeftMargin',temp$, g_prgCapForSettingsOverride$) : let marg(3)=val(temp$)
52280     let fninch2twip(marg(3))
52300     let fnread_program_print_property('RightMargin',temp$, g_prgCapForSettingsOverride$) : let marg(4)=val(temp$)
52320     let fninch2twip(marg(4))
52340     let fnread_program_print_property('FontSize',temp$, g_prgCapForSettingsOverride$) : let fsize=val(temp$)
52360   fnend 
54000   def fn_start_workaround_4591
54020     print newpage
54040     print fields "10,10,Cc 60,N": "Press ENTER to continue"
54060     input fields "11,10,C 1,N": pause$
54080   fnend 
58000 def fn_start_winxp
58020   let temp$='Sy -w NotePad "'&serverSendto$&'"'
58040   execute temp$
58060 fnend 
60000 IGNORE: continue 
60020 ! <Updateable Region: ERTN>
60040 ERTN: let fnerror(program$,err,line,act$,"xit")
60060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
60080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60120 ERTN_EXEC_ACT: execute act$ : goto ERTN
60140 ! /region
61000 def library fnsafe_filename$*256(sf_in$*256)
61020   fnsafe_filename$=fn_safe_filename$(sf_in$)
61040 fnend
62000 def fn_safe_filename$*256(sf_in$*256)
62020   if ~sf_setup then ! r:
62040     let sf_setup=1
62060     let sf_symbol_count=0
62080     dim sf_symbol$(999)*1
62100     dim sf_replace_with$(999)*1
62120     mat sf_symbol$(999)
62140     mat sf_replace_with$(999)
62160     ! alloe #    sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='#' : sf_replace_with$(sf_symbol_count)=' ' ! pound
62180     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='<' : let sf_replace_with$(sf_symbol_count)='(' ! left angle bracket
62200     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='$' : let sf_replace_with$(sf_symbol_count)=' ' ! dollar sign
62220     ! allow plus   sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='+' : sf_replace_with$(sf_symbol_count)='_' ! plus sign
62240     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='%' : let sf_replace_with$(sf_symbol_count)=' ' ! percent
62260     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='>' : let sf_replace_with$(sf_symbol_count)=')' ! right angle bracket
62280     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='!' : let sf_replace_with$(sf_symbol_count)=' ' ! exclamation point
62300     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='`' : let sf_replace_with$(sf_symbol_count)=' ' ! backtick
62320     ! allow ampersands     sf_symbol_count+=1 : sf_symbol$(sf_symbol_count)='&' : sf_replace_with$(sf_symbol_count)=' ' ! ampersand
62340     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='*' : let sf_replace_with$(sf_symbol_count)=' ' ! asterisk
62360     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)="'" : let sf_replace_with$(sf_symbol_count)='' ! single quotes
62380     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='|' : let sf_replace_with$(sf_symbol_count)=' ' ! pipe
62400     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='{' : let sf_replace_with$(sf_symbol_count)='(' ! left bracket
62420     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='?' : let sf_replace_with$(sf_symbol_count)=' ' ! question mark
62440     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='"' : let sf_replace_with$(sf_symbol_count)='' ! double quotes
62460     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='=' : let sf_replace_with$(sf_symbol_count)=' ' ! equal sign
62480     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='}' : let sf_replace_with$(sf_symbol_count)=')' ! right bracket
62500     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='/' : let sf_replace_with$(sf_symbol_count)='-' ! forward slash
62520     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)=':' : let sf_replace_with$(sf_symbol_count)='-' ! colon
62540     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='\' : let sf_replace_with$(sf_symbol_count)='-' ! back slash
62560     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='@' : let sf_replace_with$(sf_symbol_count)=' ' ! at sign
62570     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)=',' : let sf_replace_with$(sf_symbol_count)='' ! comma  (BR! can not handle them.)
62580     let sf_symbol_count+=1 : let sf_symbol$(sf_symbol_count)='.' : let sf_replace_with$(sf_symbol_count)='' ! period / dot
62600     mat sf_symbol$(sf_symbol_count)
62620     mat sf_replace_with$(sf_symbol_count)
62640   end if  ! /r
62660   for sf_item=1 to sf_symbol_count
62680     let sf_in$=srep$(sf_in$,sf_symbol$(sf_item),sf_replace_with$(sf_item))
62700   next sf_item
62720   do 
62740     let sf_in$=srep$(sf_in$,'  ',' ')
62760   loop until pos(sf_in$,'  ')<=0
62780   let fn_safe_filename$=trim$(sf_in$)
62800 fnend 
