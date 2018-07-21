def fn_pa_setup
	on error goto ERTN
	if ~pa_setup then 
		pa_setup=1
		!
		debug_pdf=0  ! turn on to pause everytime a unhandled pdf call occurs
		!
		library 'S:\Core\Library': fnerror,fnStatus,fnStatusClose,fnStatusPause,fnprint_file_name$,fnreg_read
		library 'S:\Core\PrintPdf': fnpdf_open
		library 'S:\Core\PrintPdf': fnpdf_Close
		library 'S:\Core\PrintPdf': fnpdf_Newpage
		library 'S:\Core\PrintPdf': fnpdf_text
		library 'S:\Core\PrintPdf': fnpdf_font
		library 'S:\Core\PrintPdf': fnpdf_fontbold
		library 'S:\Core\PrintPdf': fnpdf_fontitalic
		library 'S:\Core\PrintPdf': fnpdf_lineh
		library 'S:\Core\PrintPdf': fnpdf_linev
		library 'S:\Core\PrintPdf': fnpdf_pic
		library 'S:\Core\PrintPdf': fnpdf_fontsize
		library 'S:\Core\PrintPdf': fnpdf_background
		library 'S:\Core\Library': fnCopy,fnSrepEnv$
		! fnreg_read('Report_Cache',report_cache$)
		! if report_cache$='True' then print_report_caching=1 else print_report_caching=0
		fnreg_read('PrintAce.Max Pages',max_pages$)
		fnreg_read('formsFormat',formsFormat$)
		g_pa_max_pages=val(max_pages$) conv ignore
	end if 
	fn_pa_setup=pa_setup
XIT: ! 
fnend  ! fn_pa_setup
def library fnpa_finis(; h_printace)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_finis=fnpdf_Close
	else
		fnpa_finis=fn_pa_finis( h_printace,1)
		!   fnStatusPause
		!   fnStatusClose
	end if
	if formsFormatForce$<>'' then
		formsFormatForce$=''
		formsFormat$=formsFormatPrior$
		formsFormatPrior$=''
	end if
fnend 
def fn_pa_finis(; h_printace, pf_final_batch)
	!   fnStatus('pf_final_batch='&str$(pf_final_batch))
	fnStatus('Sending PrintAce Batch '&str$(g_pa_batch)&' to the printer.')
	fnStatus('When the message box (labeled Print) says "Sending to Printer" click "Okay" to continue.')
	if h_printace=0 then h_printace=20
	pr #h_printace: "Print.EndDoc" ioerr ignore
	if g_pa_filename$='' then g_pa_filename$=file$(h_printace) ! this is now set in fnpa_open, but it may not be called.
	 !   fnStatus('            g_pa_filename$='&g_pa_filename$)
	close #h_printace: ioerr ignore
	if g_pa_filename$<>'' then 
		if g_finial_filename$='' the
			g_finial_filename$=g_pa_filename$
		else
			fnCopy(g_pa_filename$,env$('at')&g_finial_filename$)
		end if
		dim prAceExeAndOsPath$*256
		if env$('br_model')='CLIENT/SERVER' then
			prAceExeAndOsPath$=(os_filename$(env$('local_program_dir'))&'\Core\PrAce.exe')
		else
			prAceExeAndOsPath$=(env$('local_program_dir')&'\Core\PrAce.exe')
		end if
		if pf_final_batch then 
			prAceSystemFlag$='-W -C '
		else
			prAceSystemFlag$='-W '
		end if
		execute 'System '&prAceSystemFlag$&'"'&prAceExeAndOsPath$&'" '&os_filename$(env$('at')&fnSrepEnv$(g_finial_filename$))
		if pf_final_batch then 
			fnStatusClose
		end if
	end if 
	g_pa_filename$=g_finial_filename$=''
fnend 
def library fnpa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$)
	fn_pa_setup
	if formsFormatForce$<>'' then
		formsFormatPrior$=formsFormat$
		formsFormat$=formsFormatForce$
	end if
	if formsFormat$="PDF" then 
		fnpa_open=fnpdf_open( pa_orientation$,pa_sendto_base_name_addition$)
	else
		! fnStatus('fnpa_open')
		g_pa_batch=0
		fnpa_open=fn_pa_open( pa_orientation$,pa_sendto_base_name_addition$)
	end if
	setenv('FormsFormatCurrent',formsFormat$)
fnend 
def fn_pa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$)
	g_pa_batch+=1
	fnStatus('Initiating a PrintAce Batch '&str$(g_pa_batch))
	if g_pa_max_pages then let fnStatus('     (up to '&str$(g_pa_max_pages)&' pages per batch)')
	h_printace=20
	if file(h_printace)=-1 then 
		dim g_pa_filename$*1024
		! if print_report_caching then 
		g_pa_filename$='[Q]\tmp_'&session$&'.prn' ! fnprint_file_name$(pa_sendto_base_name_addition$,'PrintAce')
		dim g_finial_filename$*256
		g_finial_filename$=fnprint_file_name$(pa_sendto_base_name_addition$,'PrintAce')
		fnStatus('  Report Cache Name: '&g_finial_filename$)
		! else 
		!   g_pa_filename$=env$('client_temp')&'\PA_Tmp_'&session$&'_batch_'&str$(g_pa_batch)&pa_sendto_base_name_addition$&'.PrintAce'
		! end if 
		if pa_orientation$='' then pa_orientation$='Portrait'
		open #h_printace: "Name="&g_pa_filename$&",Replace,RecL=5000",display,output 
		pr #h_printace: 'Call Print.MyOrientation("'&pa_orientation$&'")'
	 !  pr #h_printace: 'Call Print.NewPaperBin(1)'
		g_pa_pagecount=1
		g_pa_orientation$=pa_orientation$
		g_pa_handle=h_printace
	end if 
fnend 
def library fnpa_background(background_pdf$*256)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpdf_background(background_pdf$)
	else
		pr bell;'background will only pr in PDF, not printAce'
		if debug_pdf then pause          
	end if
fnend
def library fnpa_newpage(;h_printace)
	! fnStatus('fnpa_newpage    g_pa_pagecount='&str$(g_pa_pagecount))
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_newpage=fnpdf_Newpage
	else
		if h_printace=0 then h_printace=20
		if g_pa_max_pages=0 then 
			pr #h_printace: 'Call Print.NewPage'
		else ! if g_pa_max_pages<>0 then
			g_pa_pagecount+=1
			if g_pa_pagecount>g_pa_max_pages then 
				fnStatus('Maximum number of pages ('&str$(g_pa_max_pages)&') reached.')
				fn_pa_finis( h_printace,0)
				fn_pa_open( g_pa_handle, g_pa_orientation$, '') ! do not pass g_pa_filename$ or it will not set it again and the new batch number will not append
			else 
				pr #h_printace: 'Call Print.NewPage'
			end if 
		end if 
	end if
fnend 
def library fnpa_line(pl_left_pos,pl_top_pos,pl_width; pl_height, pl_box_instead_of_line,h_printace)
	! pl_box_instead_of_line=0 means it's a box
	! pl_box_instead_of_line=1 means it's a line ! same as True
	fn_pa_setup
	if formsFormat$="PDF" then
		if pl_width=0 then
			pl_return=fnpdf_linev(pl_left_pos,pl_top_pos,pl_height)
		else if pl_height=0 then
			pl_return=fnpdf_lineh(pl_left_pos,pl_top_pos,pl_width)
		else ! else if pl_box_instead_of_line then
			fnpdf_linev(pl_left_pos,pl_top_pos,pl_height) ! left side
			fnpdf_linev(pl_left_pos+pl_width,pl_top_pos,pl_height) ! right side
			fnpdf_lineh(pl_left_pos,pl_top_pos,pl_width) ! top
			fnpdf_lineh(pl_left_pos,pl_top_pos+pl_height,pl_width) ! bottom
		! else
		!   pr 'add fnpa_line - PDF line logic for a SOLID box here'
		!   pr 'pl_height=';pl_height
		!   pr 'pl_width=';pl_width
		!   pr 'pl_box_instead_of_line=';pl_box_instead_of_line;' (means it is not a solid block i think)'
		!   if debug_pdf then pause
		end if
	else
		if pl_box_instead_of_line=1 then pl_box_instead_of_line_text$=',1' else pl_box_instead_of_line_text$=''
		if h_printace=0 then h_printace=20
		pr #h_printace: 'Call Print.AddLine('&str$(pl_left_pos)&','&str$(pl_top_pos)&','&str$(pl_width)&','&str$(pl_height)&pl_box_instead_of_line_text$&')'
	end if
	fnpa_line=pl_return
fnend  ! fnpa_line
def library fnpa_txt(pt_text$*128,pt_x; pt_y,h_printace)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_txt=fnpdf_text(pt_text$,pt_x, pt_y)
	else
		if h_printace=0 then h_printace=20
		if trim$(pt_text$)<>'' then 
			pt_text$=srep$(pt_text$,'"',"'")
			pr #h_printace: 'Call Print.AddText("'&pt_text$&'",'&str$(pt_x)&','&str$(pt_y)&')'
		end if  ! trim$(pt_text$)<>''
	end if
fnend  ! fnpa_txt
def library fnpa_elipse(pe_a,pe_b,pe_c,pe_d; h_printace)
	fn_pa_setup
	if formsFormat$="PDF" then
		pr 'add PDF elipse here'
		if debug_pdf then pause
	else
		if h_printace=0 then h_printace=20
		pr #h_printace: 'Call Print.AddElipse('&str$(pe_a)&','&str$(pe_b)&','&str$(pe_c)&','&str$(pe_d)&')'
	end if
fnend 
def library fnpa_pic(pp_pic$*1024,pp_x,pp_y; imgWidth,imgHeight,style$)
	fn_pa_setup
	if formsFormat$="PDF" then
		if imgHeight=0 or imgWidth=0 then
			if pp_x=1 and pp_y=1 then
				fnpdf_background(pp_pic$)
			else
				pr 'image width passed is '&str$(imgWidth)
				pr 'image height passed is '&str$(imgHeight)
				pr bell;' Both Image Height and Image Width are required for PDF'
				if env$('ACSDeveloper')<>'' then pause
			end if
		else
			fnpa_pic=fnpdf_pic(pp_pic$,pp_x,pp_y,imgWidth,imgHeight, style$)
		end if
	else
		if h_printace=0 then h_printace=20
		pr #h_printace: 'Call Print.AddPicture("'&os_filename$(pp_pic$)&'",'&str$(pp_x)&','&str$(pp_y)&')'
	end if
fnend 
! r: font and barcode
def library fnpa_font(; pf_fontname$*256,h_printace)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_font=fnpdf_font( pf_fontname$)
	else
		if h_printace=0 then h_printace=20
		if pf_fontname$='' then pf_fontname$='Courier New'
		pr #h_printace: 'Call Print.MyFont("'&pf_fontname$&'")'
	end if
fnend 
def library fnpa_fontsize(; pfs_fontsize,h_printace)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_fontsize=fnpdf_fontsize( pfs_fontsize)
	else
		if h_printace=0 then h_printace=20
		if pfs_fontsize=0 then pfs_fontsize=10
		pr #h_printace: 'Call Print.MyFontSize('&str$(pfs_fontsize)&')'
	end if
fnend 
def library fnpa_fontbold(; pfb_off_or_on)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_fontbold=fnpdf_fontbold( pfb_off_or_on)
	else
		if h_printace=0 then h_printace=20
		if pfb_off_or_on<0 then pfb_off_or_on=0
		if pfb_off_or_on>1 then pfb_off_or_on=1
		pr #h_printace: 'Call Print.MyFontBold('&str$(pfb_off_or_on)&')'
	end if
fnend 
def library fnpa_fontitalic(; pfi_off_or_on)
	fn_pa_setup
	if formsFormat$="PDF" then
		fnpa_fontitalic=fnpdf_fontitalic( pfi_off_or_on)
	else
		if h_printace=0 then h_printace=20
		if pfi_off_or_on<0 then pfi_off_or_on=0
		if pfi_off_or_on>1 then pfi_off_or_on=1
		pr #h_printace: 'Call Print.MyFontItalic('&str$(pfi_off_or_on)&')'
	end if
fnend 
def library fnpa_barcode(pb_a,pb_b,pb_bc$*256; h_printace)
	! see also:   fnbarcode(barcode$,rightleft,updown)      from S:\Core\barcode
	! and         fnbarcodewide(barcode$,rightleft,updown)  from S:\Core\barcodewide
	fn_pa_setup
	if formsFormat$="PDF" then
		pr 'add pdf fnpa_barcode logic'
		if debug_pdf then pause
	else
		if h_printace=0 then h_printace=20
		if trim$(pb_bc$)<>'' then 
			pb_bc$=srep$(pb_bc$,'"',"'")
			pr #h_printace: 'Call Print.DisplayBarCode('&str$(pb_a)&','&str$(pb_b)&',"'&pb_bc$&'")'
		end if 
	end if
fnend 
def library fnbarcodewide(barcode$,rightleft,updown)
	!  produces code 39 barcodes for any system
	! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
	! PrintAce work file number must be 20
	if formsFormat$="PDF" then
		pr 'add pdf fnbarcodewide logic'
		if debug_pdf then pause
	else
		! r: SET_VARIABLES: !
		w=rightleft ! 30 ! right or left
		x=updown ! 25 ! up and down position of top of line
		y=w ! width of line
		z=6 ! height of line
		blankline=2.0
		q=p=0
		double=.12
		! /r
		! bARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
		gosub BCW_QUIET
		for a=1 to 10
			barcode=val(barcode$(a:a))
			p=pos(barcode$,".",a) : if p=a then goto BCW_PERIOD ! searching for period
			q=pos(barcode$,"0",a) : if q=a then goto BCW_0 ! searching for BCW_0
			on barcode goto BCW_1,BCW_2,BCW_3,BCW_4,BCW_5,BCW_6,BCW_7,BCW_8,BCW_9,BCW_0 none BCW_NEXT_A
		BCW_NEXT_A: ! 
		next a
		gosub BCW_QUIET
		goto BCW_XIT
		BCW_1: ! r:
		w+=double*2 ! blank space
		! first line, first character (wide line)
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
		! second line of number 1 (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line of character 1 (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line of number 1 (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line of number one (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_2: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 2
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_3: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
		next j
		! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
		! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r    
		BCW_4: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
		! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_5: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A
BCW_6: ! 
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_7: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		! 
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_8: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_9: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_0: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_QUIET: ! r:
		w+=double*2 ! blank line
		! first line, quiet zone (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		w+=double*blankline ! double blank line
		! second line, quiet zone (narrow line)
		w+=double*2 !  blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		! third  line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
		! 4th line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
		next j
		! 5th line, quiet zone (narrow line)
		w+=double*2 ! blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		return ! /r
		BCW_PERIOD: ! r:
		w+=double*2 ! blank space
		! first line (big line)
		! 
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		w+=double*blankline ! extra blank line
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 2
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 2
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 6
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 2
			pr #20: 'Call Print.AddLine('&str$(w+=double)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BCW_NEXT_A ! /r
		BCW_XIT: ! 
	end if
fnend 
def library fnbarcode(barcode$,rightleft,updown)
	!  produces code 39 barcodes for any system
	! must set these variables: rightleft  (how far from the right side)                                        updown (how fare down you want the barcode                                      barcode$ (the customer#, amount etc to be coded
	! PrintAce work file number must be 20
	if formsFormat$="PDF" then
		pr 'add pdf fnbarcode logic'
		if debug_pdf then pause
	else
		! r: SET_VARIABLES: !
		w=rightleft ! 30 ! right or left
		x=updown ! 25 ! up and down position of top of line
		y=w ! width of line
		z=6 ! height of line
		blankline=2.0
		q=p=0
		double=.12
		! /r
		! bARCODE$="123567890" ! 345" !67890" ! =z$  kj ! 1,2,3,4,5,6,7,8,9,0 ok
		pr #20: 'Call Print.MyFontBold(1)'
		gosub BC_QUIET
		for a=1 to 10
			barcode=val(barcode$(a:a))
			p=pos(barcode$,".",a) : if p=a then goto BC_PERIOD ! searching for period
			q=pos(barcode$,"0",a) : if q=a then goto BC_0 ! searching for BC_0
			on barcode goto BC_1,BC_2,BC_3,BC_4,BC_5,BC_6,BC_7,BC_8,BC_9,BC_0 none BC_NEXT_A
		BC_NEXT_A: ! 
		next a
		gosub BC_QUIET
		pr #20: 'Call Print.MyFontBold(0)'
		goto BC_XIT
		BC_1: ! r:
		w+=double*2 ! blank space
		! first line, first character (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
		! second line of number 1 (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line of character 1 (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line of number 1 (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line of number one (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_2: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_3: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide
		next j
		! second line  (wide line)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
		! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_4: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
		! 
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_5: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! second line  (narrow line)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_6: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_7: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		! 
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_8: ! r:
		w+=double*2 ! blank space
		! first line (wide line)
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_9: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		! 
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		w+=double*blankline ! extra blank line
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_0: ! r:
		w+=double*2 ! blank space
		! first line (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		w+=double*blankline ! extra blank line
		! third line (wide) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		! 
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_QUIET: !  r:
		w+=double*2 ! blank line
		! first line, quiet zone (narrow line)
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		w+=double*blankline ! double blank line
		! second line, quiet zone (narrow line)
		w+=double*2 !  blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		! third  line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide line
		next j
		! 4th line, quiet zone (wide line)
		w+=double*2 ! blank line
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! wide   line
		next j
		! 5th line, quiet zone (narrow line)
		w+=double*2 ! blank line
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow line
		next j
		return ! /r
		BC_PERIOD: ! r:
		w+=double*2 ! blank space
		! first line (big line)
		! 
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! narrow
		next j
		w+=double*blankline ! extra blank line
		! second line  (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! third line (narrow) (has a blank line in front of it)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")" ! contains the blank space before  and a blank line
		next j
		! fourth line  (wide)
		w+=double*2 ! blank space
		for j=1 to 12
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		! fifth line (narrow)
		w+=double*2 ! blank space
		for j=1 to 4
			pr #20: 'Call Print.AddLine('&str$(w+=double/2)&','&str$(x)&','&str$(0)&','&str$(z)&")"
		next j
		goto BC_NEXT_A ! /r
		BC_XIT: ! 
	end if
fnend 
! /r
include: ertn