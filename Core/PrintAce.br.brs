12000 def fn_pa_setup
12020   on error goto ERTN
12040   if ~pa_setup then 
12060     let pa_setup=1
12080     !
12100     debug_pdf=0  ! turn on to pause everytime a unhandled pdf call occurs
12120     !
12140     library 'S:\Core\Library': fnerror,fnstatus,fnstatus_close,fnstatus_pause,fnprint_file_name$,fnreg_read
12160     library 'S:\Core\PrintPdf': fnpdf_open
12180     library 'S:\Core\PrintPdf': fnpdf_Close
12200     library 'S:\Core\PrintPdf': fnpdf_Newpage
12220     library 'S:\Core\PrintPdf': fnpdf_text
12240     library 'S:\Core\PrintPdf': fnpdf_font
12260     library 'S:\Core\PrintPdf': fnpdf_fontbold
12280     library 'S:\Core\PrintPdf': fnpdf_fontitalic
12300     library 'S:\Core\PrintPdf': fnpdf_lineh
12320     library 'S:\Core\PrintPdf': fnpdf_linev
12340     library 'S:\Core\PrintPdf': fnpdf_pic
12360     library 'S:\Core\PrintPdf': fnpdf_fontsize
12380     library 'S:\Core\PrintPdf': fnpdf_background
12400     let fnreg_read('Report_Cache',report_cache$)
12420     if report_cache$='True' then let print_report_caching=1 else let print_report_caching=0
12440     let fnreg_read('PrintAce.Max Pages',max_pages$)
12460     let fnreg_read('formsFormat',formsFormat$)
12480     let g_pa_max_pages=val(max_pages$) conv ignore
12500   end if 
12520   let fn_pa_setup=pa_setup
12540 XIT: ! 
12560 fnend  ! fn_pa_setup
16000 IGNORE: continue 
16020 ! <updateable region: ertn>
16040 ERTN: let fnerror(program$,err,line,act$,"xit")
16060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
16080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
16100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
16120 ERTN_EXEC_ACT: execute act$ : goto ERTN
16140 ! </updateable region: ertn>
18000 def library fnpa_finis(; h_printace)
18020   let fn_pa_setup
18040   if formsFormat$="PDF" then
18060     fnpa_finis=fnpdf_Close
18080   else
18100     let fnpa_finis=fn_pa_finis( h_printace,1)
18120     !   fnstatus_pause
18140     !   fnstatus_close
18160   end if
18180   if formsFormatForce$<>'' then
18200     formsFormatForce$=''
18220     formsFormat$=formsFormatPrior$
18240     formsFormatPrior$=''
18260   end if
18280 fnend 
22000 def fn_pa_finis(; h_printace, pf_final_batch)
22010   !   fnstatus('pf_final_batch='&str$(pf_final_batch))
22020   let fnstatus('Sending PrintAce Batch '&str$(g_pa_batch)&' to the printer.')
22030   let fnstatus('When the messagebox (labeled Print) says "Sending to Printer" click "Okay" to continue.')
22040   dim pa_filename$*256
22060   if h_printace=0 then let h_printace=20
22080   print #h_printace: "Print.EndDoc" ioerr ignore
22100   if pa_filename$='' then let pa_filename$=file$(h_printace) ! this is now set in fnpa_open, but it may not be called.
22120    !   fnstatus('            pa_filename$='&pa_filename$)
22140   close #h_printace: ioerr ignore
22160   if pa_filename$<>'' then 
22180     if pf_final_batch then 
22200       execute 'System -W -C "'&os_filename$("S:\Core\PrAce.exe")&'" '&os_filename$(pa_filename$)
22210       let fnstatus_close
22220     else 
22240       execute 'System -W "'&os_filename$("S:\Core\PrAce.exe")&'" '&os_filename$(pa_filename$)
22260     end if 
22280   end if 
22300   let pa_filename$=''
22320 fnend 
24000 def library fnpa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$)
24020   let fn_pa_setup
24040   if formsFormatForce$<>'' then
24060     formsFormatPrior$=formsFormat$
24080     formsFormat$=formsFormatForce$
24100   end if
24120   if formsFormat$="PDF" then 
24140     fnpa_open=fnpdf_open( pa_orientation$,pa_sendto_base_name_addition$)
24160   else
24180     ! fnstatus('fnpa_open')
24200     let g_pa_batch=0
24220     let fnpa_open=fn_pa_open( pa_orientation$,pa_sendto_base_name_addition$)
24240   end if
24250   setenv('FormsFormatCurrent',formsFormat$)
24260 fnend 
26000 def fn_pa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$)
26040   let g_pa_batch+=1
26060   let fnstatus('Iniating a PrintAce Batch '&str$(g_pa_batch))
26070   if g_pa_max_pages then let fnstatus('     (up to '&str$(g_pa_max_pages)&' pages per batch)')
26080   let h_printace=20
26100   if file(h_printace)=-1 then 
26120     dim pa_o_filename$*1024
26140     if print_report_caching then 
26160       let pa_o_filename$=fnprint_file_name$(pa_sendto_base_name_addition$,'PrintAce')
26170       let fnstatus('  Report Cache Name: '&pa_o_filename$)
26180     else 
26200       let pa_o_filename$=env$('temp')&'\PA_Tmp_'&session$&'_batch_'&str$(g_pa_batch)&pa_sendto_base_name_addition$&'.PrintAce'
26220     end if 
26260     if pa_orientation$='' then let pa_orientation$='Portrait'
26280     open #h_printace: "Name="&pa_o_filename$&",Replace,RecL=5000",display,output 
26300     let pa_filename$=pa_o_filename$
26320     print #h_printace: 'Call Print.MyOrientation("'&pa_orientation$&'")'
26340    !  Print #h_printace: 'Call Print.NewPaperBin(1)'
26360     let g_pa_pagecount=1
26400     let g_pa_orientation$=pa_orientation$
26420     dim g_pa_filename$*256
26440     let g_pa_filename$=pa_o_filename$
26460     let g_pa_handle=h_printace
26480   end if 
26500 fnend 
27000 def library fnpa_background(background_pdf$*256)
27020   let fn_pa_setup
27040   if formsFormat$="PDF" then
27060     fnpdf_background(background_pdf$)
27080   else
27100     pr bell;'background will only print in PDF, not printAce'
27120     if debug_pdf then pause          
27140   end if
27160 fnend
28000 def library fnpa_newpage(;h_printace)
28010   ! fnstatus('fnpa_newpage    g_pa_pagecount='&str$(g_pa_pagecount))
28020   let fn_pa_setup
28022   if formsFormat$="PDF" then
28024     fnpa_newpage=fnpdf_Newpage
28026   else
28040     if h_printace=0 then let h_printace=20
28060     if g_pa_max_pages=0 then 
28080       print #h_printace: 'Call Print.NewPage'
28100     else ! if g_pa_max_pages<>0 then
28120       let g_pa_pagecount+=1
28140       if g_pa_pagecount>g_pa_max_pages then 
28150         let fnstatus('Maximum number of pages ('&str$(g_pa_max_pages)&') reached.')
28180         let fn_pa_finis( h_printace,0)
28200         let fn_pa_open( g_pa_handle, g_pa_orientation$, '') ! do not pass g_pa_filename$ or it will not set it again and the new batch number will not append
28220       else 
28240         print #h_printace: 'Call Print.NewPage'
28260       end if 
28280     end if 
28290   end if
28300 fnend 
32000 def library fnpa_line(pl_left_pos,pl_top_pos,pl_width; pl_height, pl_box_instead_of_line,h_printace)
32020   ! pl_box_instead_of_line=0 means it's a box
32040   ! pl_box_instead_of_line=1 means it's a line ! same as True
32060   let fn_pa_setup
32080   if formsFormat$="PDF" then
32100     if pl_width=0 then
32120       pl_return=fnpdf_linev(pl_left_pos,pl_top_pos,pl_height)
32140     else if pl_height=0 then
32160       pl_return=fnpdf_lineh(pl_left_pos,pl_top_pos,pl_width)
32180     else ! else if pl_box_instead_of_line then
32200       fnpdf_linev(pl_left_pos,pl_top_pos,pl_height) ! left side
32220       fnpdf_linev(pl_left_pos+pl_width,pl_top_pos,pl_height) ! right side
32240       fnpdf_lineh(pl_left_pos,pl_top_pos,pl_width) ! top
32260       fnpdf_lineh(pl_left_pos,pl_top_pos+pl_height,pl_width) ! bottom
32280     ! else
32300     !   pr 'add fnpa_line - PDF line logic for a SOLID box here'
32320     !   pr 'pl_height=';pl_height
32340     !   pr 'pl_width=';pl_width
32360     !   pr 'pl_box_instead_of_line=';pl_box_instead_of_line;' (means it is not a solid block i think)'
32380     !   if debug_pdf then pause
32400     end if
32420   else
32440     if pl_box_instead_of_line=1 then let pl_box_instead_of_line_text$=',1' else let pl_box_instead_of_line_text$=''
32460     if h_printace=0 then let h_printace=20
32480     print #h_printace: 'Call Print.AddLine('&str$(pl_left_pos)&','&str$(pl_top_pos)&','&str$(pl_width)&','&str$(pl_height)&pl_box_instead_of_line_text$&')'
32500   end if
32520   fnpa_line=pl_return
32540 fnend  ! fnpa_line
34000 def library fnpa_txt(pt_text$*128,pt_x; pt_y,h_printace)
34020   let fn_pa_setup
34040   if formsFormat$="PDF" then
34060     fnpa_txt=fnpdf_text(pt_text$,pt_x, pt_y)
34080   else
34100     if h_printace=0 then let h_printace=20
34120     if trim$(pt_text$)<>'' then 
34140       let pt_text$=srep$(pt_text$,'"',"'")
34160       print #h_printace: 'Call Print.AddText("'&pt_text$&'",'&str$(pt_x)&','&str$(pt_y)&')'
34180     end if  ! trim$(pt_text$)<>''
34200   end if
34220 fnend  ! fnpa_txt
36000 def library fnpa_text(h_printace,pt_text$*128,pt_x,pt_y) ! older - demands handle as first parameter - try fnpa_txt for cleaner code
36020   let fn_pa_setup
36022   if formsFormat$="PDF" then
36024     fnpa_text=fnpdf_text(pt_text$,pt_x, pt_y)
36026   else
36040     if h_printace=0 then let h_printace=20
36060     if trim$(pt_text$)<>'' then 
36080       let pt_text$=srep$(pt_text$,'"',"'")
36100       print #h_printace: 'Call Print.AddText("'&pt_text$&'",'&str$(pt_x)&','&str$(pt_y)&')'
36120     end if  ! trim$(pt_text$)<>''
36130   end if
36140 fnend  ! fnpa_text
38000 def library fnpa_elipse(pe_a,pe_b,pe_c,pe_d; h_printace)
38020   let fn_pa_setup
38022   if formsFormat$="PDF" then
38024     pr 'add PDF elipse here'
38025     if debug_pdf then pause
38026   else
38040     if h_printace=0 then let h_printace=20
38060     print #h_printace: 'Call Print.AddElipse('&str$(pe_a)&','&str$(pe_b)&','&str$(pe_c)&','&str$(pe_d)&')'
38070   end if
38080 fnend 
39000 def library fnpa_pic(pp_pic$*1024,pp_x,pp_y; imgWidth,imgHeight,style$)
39020   let fn_pa_setup
39040   if formsFormat$="PDF" then
39060     if imgHeight=0 or imgWidth=0 then
39080       if pp_x=1 and pp_y=1 then
39100         fnpdf_background(pp_pic$)
39120       else
39140         pr 'image width passed is '&str$(imgWidth)
39160         pr 'image height passed is '&str$(imgHeight)
39180         pr bell;' Both Image Height and Image Width are required for PDF'
39200         if env$('ACSDeveloper')<>'' then pause
39220       end if
39240     else
39260       fnpa_pic=fnpdf_pic(pp_pic$,pp_x,pp_y,imgWidth,imgHeight, style$)
39280     end if
39300   else
39320     if h_printace=0 then let h_printace=20
39340     print #h_printace: 'Call Print.AddPicture("'&os_filename$(pp_pic$)&'",'&str$(pp_x)&','&str$(pp_y)&')'
39360   end if
39380 fnend 
40000 def library fnpa_font(; pf_fontname$*256,h_printace)
40020   let fn_pa_setup
40040   if formsFormat$="PDF" then
40060     fnpa_font=fnpdf_font( pf_fontname$)
40080   else
40100     if h_printace=0 then let h_printace=20
40120     if pf_fontname$='' then let pf_fontname$='Courier New'
40140     print #h_printace: 'Call Print.MyFont("'&pf_fontname$&'")'
40160   end if
40180 fnend 
42000 def library fnpa_fontsize(; pfs_fontsize,h_printace)
42020   let fn_pa_setup
42040   if formsFormat$="PDF" then
42060     fnpa_fontsize=fnpdf_fontsize( pfs_fontsize)
42080   else
42100     if h_printace=0 then let h_printace=20
42120     if pfs_fontsize=0 then let pfs_fontsize=10
42140     print #h_printace: 'Call Print.MyFontSize('&str$(pfs_fontsize)&')'
42160   end if
42180 fnend 
44000 def library fnpa_fontbold(; pfb_off_or_on)
44020   let fn_pa_setup
44040   if formsFormat$="PDF" then
44060     fnpa_fontbold=fnpdf_fontbold( pfb_off_or_on)
44080   else
44100     if h_printace=0 then let h_printace=20
44120     if pfb_off_or_on<0 then let pfb_off_or_on=0
44140     if pfb_off_or_on>1 then let pfb_off_or_on=1
44160     print #h_printace: 'Call Print.MyFontBold('&str$(pfb_off_or_on)&')'
44180   end if
44200 fnend 
45000 def library fnpa_fontitalic(; pfi_off_or_on)
45020   let fn_pa_setup
45040   if formsFormat$="PDF" then
45060     fnpa_fontitalic=fnpdf_fontitalic( pfi_off_or_on)
45080   else
45100     if h_printace=0 then let h_printace=20
45120     if pfi_off_or_on<0 then let pfi_off_or_on=0
45140     if pfi_off_or_on>1 then let pfi_off_or_on=1
45160     print #h_printace: 'Call Print.MyFontItalic('&str$(pfi_off_or_on)&')'
45180   end if
45200 fnend 
46000 def library fnpa_barcode(pb_a,pb_b,pb_bc$*256; h_printace)
46020   ! see also:   fnbarcode(barcode$,rightleft,updown)      from S:\Core\barcode
46040   ! and         fnbarcodewide(barcode$,rightleft,updown)  from S:\Core\barcodewide
46060   let fn_pa_setup
46080   if formsFormat$="PDF" then
46100     pr 'add pdf fnpa_barcode logic'
46120     if debug_pdf then pause
46140   else
46160     if h_printace=0 then let h_printace=20
46180     if trim$(pb_bc$)<>'' then 
46200       let pb_bc$=srep$(pb_bc$,'"',"'")
46220       print #h_printace: 'Call Print.DisplayBarCode('&str$(pb_a)&','&str$(pb_b)&',"'&pb_bc$&'")'
46240     end if 
46260   end if
46280 fnend 
48000 def library fnbarcodewide(barcode$,rightleft,updown)
48010   !  produces code 39 barcodes for any system
48020   ! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
48030   ! PrintAce work file number must be 20
48040   if formsFormat$="PDF" then
48050     pr 'add pdf fnbarcodewide logic'
48060     if debug_pdf then pause
48070   else
48080     ! r: SET_VARIABLES: !
48090     let w=rightleft ! 30 ! right or left
48100     let x=updown ! 25 ! up and down position of top of line
48110     let y=w ! width of line
48120     let z=6 ! height of line
48130     let blankline=2.0
48140     let q=p=0
48150     let double=.12
48160     ! /r
48170     ! Let BARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
48180     gosub BCW_QUIET
48190     for a=1 to 10
48200       let barcode=val(barcode$(a:a))
48210       let p=pos(barcode$,".",a) : if p=a then goto BCW_PERIOD ! searching for period
48220       let q=pos(barcode$,"0",a) : if q=a then goto BCW_0 ! searching for BCW_0
48230       on barcode goto BCW_1,BCW_2,BCW_3,BCW_4,BCW_5,BCW_6,BCW_7,BCW_8,BCW_9,BCW_0 none BCW_NEXT_A
48240     BCW_NEXT_A: ! 
48250     next a
48260     gosub BCW_QUIET
48270     goto BCW_XIT
48280     BCW_1: ! r:
48290     let w+=double*2 ! blank space
48300     ! first line, first character (wide line)
48310     for j=1 to 6
48320       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
48330     next j
48340     ! second line of number 1 (narrow line)
48350     let w+=double*2 ! blank space
48360     for j=1 to 4
48370       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48380     next j
48390     let w+=double*blankline ! extra blank line
48400     ! third line of character 1 (narrow) (has a blank line in front of it)
48410     let w+=double*2 ! blank space
48420     for j=1 to 4
48430       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
48440     next j
48450     ! fourth line of number 1 (narrow)
48460     let w+=double*2 ! blank space
48470     for j=1 to 4
48480       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48490     next j
48500     ! fifth line of number one (wide)
48510     let w+=double*2 ! blank space
48520     for j=1 to 6
48530       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48540     next j
48550     goto BCW_NEXT_A ! /r
48560     BCW_2: ! r:
48570     let w+=double*2 ! blank space
48580     ! first line (narrow line)
48590     for j=1 to 2
48600       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
48610     next j
48620     ! second line  (wide line)
48630     let w+=double*2 ! blank space
48640     for j=1 to 6
48650       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48660     next j
48670     let w+=double*blankline ! extra blank line
48680     ! third line (narrow) (has a blank line in front of it)
48690     let w+=double*2 ! blank space
48700     for j=1 to 4
48710       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
48720     next j
48730 ! fourth line  (narrow)
48740     let w+=double*2 ! blank space
48750     for j=1 to 4
48760       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48770     next j
48780 ! fifth line (wide)
48790     let w+=double*2 ! blank space
48800     for j=1 to 6
48810       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48820     next j
48830     goto BCW_NEXT_A ! /r
48840     BCW_3: ! r:
48850     let w+=double*2 ! blank space
48860     ! first line (wide line)
48870     for j=1 to 6
48880       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
48890     next j
48900     ! second line  (wide line)
48910     let w+=double*2 ! blank space
48920     for j=1 to 6
48930       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
48940     next j
48950     let w+=double*blankline ! extra blank line
48960     ! third line (narrow) (has a blank line in front of it)
48970     let w+=double*2 ! blank space
48980     for j=1 to 4
48990       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
49000     next j
49010     ! fourth line  (narrow)
49020     let w+=double*2 ! blank space
49030     for j=1 to 4
49040     ! 
49050       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49060     next j
49070     ! fifth line (narrow)
49080     let w+=double*2 ! blank space
49090     for j=1 to 4
49100       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49110     next j
49120     goto BCW_NEXT_A ! /r    
49130     BCW_4: ! r:
49140     let w+=double*2 ! blank space
49150     ! first line (narrow line)
49160     for j=1 to 4
49170       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
49180     next j
49190     ! second line  (narrow line)
49200     let w+=double*2 ! blank space
49210     for j=1 to 4
49220       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49230     next j
49240     let w+=double*blankline ! extra blank line
49250     ! third line (wide) (has a blank line in front of it)
49260     let w+=double*2 ! blank space
49270     for j=1 to 6
49280       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
49290     next j
49300     ! fourth line  (narrow)
49310     let w+=double*2 ! blank space
49320     for j=1 to 4
49330     ! 
49340       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49350     next j
49360     ! fifth line (wide)
49370     let w+=double*2 ! blank space
49380     for j=1 to 6
49390       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49400     next j
49410     goto BCW_NEXT_A ! /r
49420     BCW_5: ! r:
49430     let w+=double*2 ! blank space
49440     ! first line (wide line)
49450     for j=1 to 6
49460       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49470     next j
49480     ! second line  (narrow line)
49490     let w+=double*2 ! blank space
49500     for j=1 to 4
49510       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49520     next j
49530     let w+=double*blankline ! extra blank line
49540     ! third line (wide) (has a blank line in front of it)
49550     let w+=double*2 ! blank space
49560     for j=1 to 6
49570       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
49580     next j
49590     ! fourth line  (narrow)
49600     let w+=double*2 ! blank space
49610     for j=1 to 4
49620       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49630     next j
49640     ! fifth line (narrow)
49650     let w+=double*2 ! blank space
49660     for j=1 to 4
49670       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49680     next j
49690     goto BCW_NEXT_A
49700 BCW_6: ! 
49710     let w+=double*2 ! blank space
49720     ! first line (narrow line)
49730     for j=1 to 4
49740       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
49750     next j
49760     ! second line  (wide)
49770     let w+=double*2 ! blank space
49780     for j=1 to 6
49790       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49800     next j
49810     let w+=double*blankline ! extra blank line
49820     ! third line (wide) (has a blank line in front of it)
49830     let w+=double*2 ! blank space
49840     for j=1 to 6
49850       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
49860     next j
49870     ! fourth line  (narrow)
49880     let w+=double*2 ! blank space
49890     for j=1 to 4
49900       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49910     ! 
49920     next j
49930     ! fifth line (narrow)
49940     let w+=double*2 ! blank space
49950     for j=1 to 4
49960       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
49970     next j
49980     goto BCW_NEXT_A ! /r
49990     BCW_7: ! r:
50000     let w+=double*2 ! blank space
50010     ! first line (narrow line)
50020     ! 
50030     for j=1 to 4
50040       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
50050     next j
50060     ! second line  (narrow)
50070     let w+=double*2 ! blank space
50080     for j=1 to 4
50090       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50100     next j
50110     let w+=double*blankline ! extra blank line
50120     ! third line (narrow) (has a blank line in front of it)
50130     let w+=double*2 ! blank space
50140     for j=1 to 4
50150       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
50160     next j
50170     ! fourth line  (wide)
50180     ! 
50190     let w+=double*2 ! blank space
50200     for j=1 to 6
50210       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50220     ! 
50230     next j
50240     ! fifth line (wide)
50250     let w+=double*2 ! blank space
50260     for j=1 to 6
50270       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50280     next j
50290     goto BCW_NEXT_A ! /r
50300     BCW_8: ! r:
50310     let w+=double*2 ! blank space
50320     ! first line (wide line)
50330     for j=1 to 6
50340       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50350     next j
50360     ! second line  (narrow)
50370     let w+=double*2 ! blank space
50380     for j=1 to 4
50390       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50400     next j
50410     let w+=double*blankline ! extra blank line
50420     ! third line (narrow) (has a blank line in front of it)
50430     let w+=double*2 ! blank space
50440     for j=1 to 4
50450       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
50460     next j
50470     ! fourth line  (wide)
50480     let w+=double*2 ! blank space
50490     for j=1 to 6
50500       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50510     ! 
50520     next j
50530     ! fifth line (narrow)
50540     let w+=double*2 ! blank space
50550     for j=1 to 4
50560       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50570     next j
50580     goto BCW_NEXT_A ! /r
50590     BCW_9: ! r:
50600     let w+=double*2 ! blank space
50610     ! first line (narrow line)
50620     ! 
50630     for j=1 to 4
50640       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
50650     next j
50660     ! second line  (wide)
50670     let w+=double*2 ! blank space
50680     for j=1 to 6
50690       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50700     ! 
50710     next j
50720     let w+=double*blankline ! extra blank line
50730     ! third line (narrow) (has a blank line in front of it)
50740     let w+=double*2 ! blank space
50750     for j=1 to 4
50760       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
50770     next j
50780     ! fourth line  (wide)
50790     let w+=double*2 ! blank space
50800     for j=1 to 6
50810       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50820     ! 
50830     next j
50840     ! fifth line (narrow)
50850     let w+=double*2 ! blank space
50860     for j=1 to 4
50870       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
50880     next j
50890     goto BCW_NEXT_A ! /r
50900     BCW_0: ! r:
50910     let w+=double*2 ! blank space
50920     ! first line (narrow line)
50930     for j=1 to 4
50940       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
50950     next j
50960     ! second line  (narrow)
50970     let w+=double*2 ! blank space
50980     for j=1 to 4
50990       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51000     next j
51010     let w+=double*blankline ! extra blank line
51020     ! third line (wide) (has a blank line in front of it)
51030     let w+=double*2 ! blank space
51040     for j=1 to 6
51050       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
51060     next j
51070     ! fourth line  (wide)
51080     let w+=double*2 ! blank space
51090     for j=1 to 6
51100       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51110     ! 
51120     next j
51130     ! fifth line (narrow)
51140     let w+=double*2 ! blank space
51150     for j=1 to 4
51160       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51170     next j
51180     goto BCW_NEXT_A ! /r
51190     BCW_QUIET: ! r:
51200     let w+=double*2 ! blank line
51210     ! first line, quiet zone (narrow line)
51220     for j=1 to 4
51230       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
51240     next j
51250     let w+=double*blankline ! double blank line
51260     ! second line, quiet zone (narrow line)
51270     let w+=double*2 !  blank line
51280     for j=1 to 4
51290       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
51300     next j
51310     ! third  line, quiet zone (wide line)
51320     let w+=double*2 ! blank line
51330     for j=1 to 6
51340       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
51350     next j
51360     ! 4th line, quiet zone (wide line)
51370     let w+=double*2 ! blank line
51380     for j=1 to 6
51390       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
51400     next j
51410     ! 5th line, quiet zone (narrow line)
51420     let w+=double*2 ! blank line
51430     for j=1 to 4
51440       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
51450     next j
51460     return ! /r
51470     BCW_PERIOD: ! r:
51480     let w+=double*2 ! blank space
51490     ! first line (big line)
51500     ! 
51510     for j=1 to 6
51520       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
51530     next j
51540     let w+=double*blankline ! extra blank line
51550     ! second line  (narrow)
51560     let w+=double*2 ! blank space
51570     for j=1 to 2
51580       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51590     next j
51600     ! third line (narrow) (has a blank line in front of it)
51610     let w+=double*2 ! blank space
51620     for j=1 to 2
51630       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
51640     next j
51650     ! fourth line  (wide)
51660     let w+=double*2 ! blank space
51670     for j=1 to 6
51680       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51690     next j
51700     ! fifth line (narrow)
51710     let w+=double*2 ! blank space
51720     for j=1 to 2
51730       print #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
51740     next j
51750     goto BCW_NEXT_A ! /r
51760     BCW_XIT: ! 
51770   end if
51780 fnend 
58000 def library fnbarcode(barcode$,rightleft,updown)
58010   !  produces code 39 barcodes for any system
58020   ! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
58030   ! PrintAce work file number must be 20
58040   if formsFormat$="PDF" then
58050     pr 'add pdf fnbarcode logic'
58060     if debug_pdf then pause
58070   else
58080     ! r: SET_VARIABLES: !
58090     let w=rightleft ! 30 ! right or left
58100     let x=updown ! 25 ! up and down position of top of line
58110     let y=w ! width of line
58120     let z=6 ! height of line
58130     let blankline=2.0
58140     let q=p=0
58150     let double=.12
58160     ! /r
58170     ! Let BARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
58180     print #20: 'Call Print.MyFontBold(1)'
58190     gosub BC_QUIET
58200     for a=1 to 10
58210       let barcode=val(barcode$(a:a))
58220       let p=pos(barcode$,".",a) : if p=a then goto BC_PERIOD ! searching for period
58230       let q=pos(barcode$,"0",a) : if q=a then goto BC_0 ! searching for BC_0
58240       on barcode goto BC_1,BC_2,BC_3,BC_4,BC_5,BC_6,BC_7,BC_8,BC_9,BC_0 none BC_NEXT_A
58250     BC_NEXT_A: ! 
58260     next a
58270     gosub BC_QUIET
58280     print #20: 'Call Print.MyFontBold(0)'
58290     goto BC_XIT
58300     BC_1: ! r:
58310     let w+=double*2 ! blank space
58320     ! first line, first character (wide line)
58330     for j=1 to 12
58340       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
58350     next j
58360     ! second line of number 1 (narrow line)
58370     let w+=double*2 ! blank space
58380     for j=1 to 4
58390       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58400     next j
58410     let w+=double*blankline ! extra blank line
58420     ! third line of character 1 (narrow) (has a blank line in front of it)
58430     let w+=double*2 ! blank space
58440     for j=1 to 4
58450       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
58460     next j
58470     ! fourth line of number 1 (narrow)
58480     let w+=double*2 ! blank space
58490     for j=1 to 4
58500       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58510     next j
58520     ! fifth line of number one (wide)
58530     let w+=double*2 ! blank space
58540     for j=1 to 12
58550       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58560     next j
58570     goto BC_NEXT_A ! /r
58580     BC_2: ! r:
58590     let w+=double*2 ! blank space
58600     ! first line (narrow line)
58610     for j=1 to 4
58620       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
58630     next j
58640     ! second line  (wide line)
58650     let w+=double*2 ! blank space
58660     for j=1 to 12
58670       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58680     next j
58690     let w+=double*blankline ! extra blank line
58700     ! third line (narrow) (has a blank line in front of it)
58710     let w+=double*2 ! blank space
58720     for j=1 to 4
58730       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
58740     next j
58750     ! fourth line  (narrow)
58760     let w+=double*2 ! blank space
58770     for j=1 to 4
58780       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58790     next j
58800     ! fifth line (wide)
58810     let w+=double*2 ! blank space
58820     for j=1 to 12
58830       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58840     next j
58850     goto BC_NEXT_A ! /r
58860     BC_3: ! r:
58870     let w+=double*2 ! blank space
58880     ! first line (wide line)
58890     for j=1 to 12
58900       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
58910     next j
58920     ! second line  (wide line)
58930     let w+=double*2 ! blank space
58940     for j=1 to 12
58950       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
58960     next j
58970     let w+=double*blankline ! extra blank line
58980     ! third line (narrow) (has a blank line in front of it)
58990     let w+=double*2 ! blank space
59000     for j=1 to 4
59010       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
59020     next j
59030     ! fourth line  (narrow)
59040     let w+=double*2 ! blank space
59050     for j=1 to 4
59060     ! 
59070       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59080     next j
59090     ! fifth line (narrow)
59100     let w+=double*2 ! blank space
59110     for j=1 to 4
59120       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59130     next j
59140     goto BC_NEXT_A ! /r
59150     BC_4: ! r:
59160     let w+=double*2 ! blank space
59170     ! first line (narrow line)
59180     for j=1 to 4
59190       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
59200     next j
59210     ! second line  (narrow line)
59220     let w+=double*2 ! blank space
59230     for j=1 to 4
59240       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59250     next j
59260     let w+=double*blankline ! extra blank line
59270     ! third line (wide) (has a blank line in front of it)
59280     let w+=double*2 ! blank space
59290     for j=1 to 12
59300       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
59310     next j
59320     ! fourth line  (narrow)
59330     let w+=double*2 ! blank space
59340     for j=1 to 4
59350     ! 
59360       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59370     next j
59380     ! fifth line (wide)
59390     let w+=double*2 ! blank space
59400     for j=1 to 12
59410       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59420     next j
59430     goto BC_NEXT_A ! /r
59440     BC_5: ! r:
59450     let w+=double*2 ! blank space
59460     ! first line (wide line)
59470     for j=1 to 12
59480       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59490     next j
59500     ! second line  (narrow line)
59510     let w+=double*2 ! blank space
59520     for j=1 to 4
59530       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59540     next j
59550     let w+=double*blankline ! extra blank line
59560     ! third line (wide) (has a blank line in front of it)
59570     let w+=double*2 ! blank space
59580     for j=1 to 12
59590       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
59600     next j
59610     ! fourth line  (narrow)
59620     let w+=double*2 ! blank space
59630     for j=1 to 4
59640       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59650     next j
59660     ! fifth line (narrow)
59670     let w+=double*2 ! blank space
59680     for j=1 to 4
59690       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59700     next j
59710     goto BC_NEXT_A ! /r
59720     BC_6: ! r:
59730     let w+=double*2 ! blank space
59740     ! first line (narrow line)
59750     for j=1 to 4
59760       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
59770     next j
59780     ! second line  (wide)
59790     let w+=double*2 ! blank space
59800     for j=1 to 12
59810       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59820     next j
59830     let w+=double*blankline ! extra blank line
59840     ! third line (wide) (has a blank line in front of it)
59850     let w+=double*2 ! blank space
59860     for j=1 to 12
59870       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
59880     next j
59890     ! fourth line  (narrow)
59900     let w+=double*2 ! blank space
59910     for j=1 to 4
59920       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59930     ! 
59940     next j
59950     ! fifth line (narrow)
59960     let w+=double*2 ! blank space
59970     for j=1 to 4
59980       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
59990     next j
60000     goto BC_NEXT_A ! /r
60010     BC_7: ! r:
60020     let w+=double*2 ! blank space
60030     ! first line (narrow line)
60040     ! 
60050     for j=1 to 4
60060       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
60070     next j
60080     ! second line  (narrow)
60090     let w+=double*2 ! blank space
60100     for j=1 to 4
60110       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60120     next j
60130     let w+=double*blankline ! extra blank line
60140     ! third line (narrow) (has a blank line in front of it)
60150     let w+=double*2 ! blank space
60160     for j=1 to 4
60170       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
60180     next j
60190     ! fourth line  (wide)
60200     ! 
60210     let w+=double*2 ! blank space
60220     for j=1 to 12
60230       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60240     ! 
60250     next j
60260     ! fifth line (wide)
60270     let w+=double*2 ! blank space
60280     for j=1 to 12
60290       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60300     next j
60310     goto BC_NEXT_A ! /r
60320     BC_8: ! r:
60330     let w+=double*2 ! blank space
60340     ! first line (wide line)
60350     for j=1 to 12
60360       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60370     next j
60380     ! second line  (narrow)
60390     let w+=double*2 ! blank space
60400     for j=1 to 4
60410       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60420     next j
60430     let w+=double*blankline ! extra blank line
60440     ! third line (narrow) (has a blank line in front of it)
60450     let w+=double*2 ! blank space
60460     for j=1 to 4
60470       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
60480     next j
60490     ! fourth line  (wide)
60500     let w+=double*2 ! blank space
60510     for j=1 to 12
60520       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60530     ! 
60540     next j
60550     ! fifth line (narrow)
60560     let w+=double*2 ! blank space
60570     for j=1 to 4
60580       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60590     next j
60600     goto BC_NEXT_A ! /r
60610     BC_9: ! r:
60620     let w+=double*2 ! blank space
60630     ! first line (narrow line)
60640     ! 
60650     for j=1 to 4
60660       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
60670     next j
60680     ! second line  (wide)
60690     let w+=double*2 ! blank space
60700     for j=1 to 12
60710       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60720     ! 
60730     next j
60740     let w+=double*blankline ! extra blank line
60750     ! third line (narrow) (has a blank line in front of it)
60760     let w+=double*2 ! blank space
60770     for j=1 to 4
60780       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
60790     next j
60800     ! fourth line  (wide)
60810     let w+=double*2 ! blank space
60820     for j=1 to 12
60830       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60840     ! 
60850     next j
60860     ! fifth line (narrow)
60870     let w+=double*2 ! blank space
60880     for j=1 to 4
60890       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
60900     next j
60910     goto BC_NEXT_A ! /r
60920     BC_0: ! r:
60930     let w+=double*2 ! blank space
60940     ! first line (narrow line)
60950     for j=1 to 4
60960       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
60970     next j
60980     ! second line  (narrow)
60990     let w+=double*2 ! blank space
61000     for j=1 to 4
61010       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61020     next j
61030     let w+=double*blankline ! extra blank line
61040     ! third line (wide) (has a blank line in front of it)
61050     let w+=double*2 ! blank space
61060     for j=1 to 12
61070       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
61080     next j
61090     ! fourth line  (wide)
61100     let w+=double*2 ! blank space
61110     for j=1 to 12
61120       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61130     ! 
61140     next j
61150     ! fifth line (narrow)
61160     let w+=double*2 ! blank space
61170     for j=1 to 4
61180       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61190     next j
61200     goto BC_NEXT_A ! /r
61210     BC_QUIET: !  r:
61220     let w+=double*2 ! blank line
61230     ! first line, quiet zone (narrow line)
61240     for j=1 to 4
61250       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
61260     next j
61270     let w+=double*blankline ! double blank line
61280     ! second line, quiet zone (narrow line)
61290     let w+=double*2 !  blank line
61300     for j=1 to 4
61310       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
61320     next j
61330     ! third  line, quiet zone (wide line)
61340     let w+=double*2 ! blank line
61350     for j=1 to 12
61360       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
61370     next j
61380     ! 4th line, quiet zone (wide line)
61390     let w+=double*2 ! blank line
61400     for j=1 to 12
61410       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
61420     next j
61430     ! 5th line, quiet zone (narrow line)
61440     let w+=double*2 ! blank line
61450     for j=1 to 4
61460       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
61470     next j
61480     return ! /r
61490     BC_PERIOD: ! r:
61500     let w+=double*2 ! blank space
61510     ! first line (big line)
61520     ! 
61530     for j=1 to 12
61540       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
61550     next j
61560     let w+=double*blankline ! extra blank line
61570     ! second line  (narrow)
61580     let w+=double*2 ! blank space
61590     for j=1 to 4
61600       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61610     next j
61620     ! third line (narrow) (has a blank line in front of it)
61630     let w+=double*2 ! blank space
61640     for j=1 to 4
61650       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
61660     next j
61670     ! fourth line  (wide)
61680     let w+=double*2 ! blank space
61690     for j=1 to 12
61700       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61710     next j
61720     ! fifth line (narrow)
61730     let w+=double*2 ! blank space
61740     for j=1 to 4
61750       print #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
61760     next j
61770     goto BC_NEXT_A ! /r
61780     BC_XIT: ! 
61790   end if
61800 fnend 
