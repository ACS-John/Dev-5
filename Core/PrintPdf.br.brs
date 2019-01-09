def fn_Setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fngethandle,fnprint_file_name$,fnWindowsStart,fnCopy
!   exec 'Config Option 68 On' ! Prevent stretching printed fonts. (unfortunately it also turns off all font size respect)
		esc$=chr$(27)
		gFontItalic=0
		gFontBold=0
		gFontSize=10
		gFontName$='Courier New'
		debug=1 ! debug=1
	end if
fnend
def library fnpdf_open(; pdfOrientation$*9,pdf_sendto_base_name_addition$*128)
	pdfDecipos=0 ! no longer passed - no longer needed
	if ~setup then let fn_Setup
	dim g_filename_final$*1024 ! PDF must be made on Client side.  BR won't work otherwise
	g_filename_final$=env$('at')&fnprint_file_name$( pdf_sendto_base_name_addition$,'pdf')
	open #hPdfOut:=fngethandle: 'Name='&env$('at')&'PDF:,PrintFile='&g_filename_final$&',Replace,RecL=5000',Display,Output ioerr poFAIL
	poReturn=hPdfOut
	gPdfDecipos=pdfDecipos ! if gPdfDecipos then skip translation from mm to gPdfDecipont, expect all input as Decipoints
	if lwrc$(pdfOrientation$)='landscape' then
		fn_phe('&l1O')
	else ! if pdfOrientation$='Portrait' then
		fn_phe('&l0O')
	end if
	! *****  Top Margin
	! fn_phe('&l0E') 
	fn_phe('&l2E') 
	! PRINTER PDF [TOP0], "\E&l0E"  ! 19 mm ! is apx 1.8 (10cpi) lines higher than PrintAce  (landscape test)
	! PRINTER PDF [TOP1], "\E&l1E"
	! PRINTER PDF [TOP2], "\E&l2E"  ! 26 mm  ! almost exactly what PrintAce is (landscape test)
	! PRINTER PDF [TOP3], "\E&l3E"
	! PRINTER PDF [TOP4], "\E&l4E"
	! PRINTER PDF [TOP5], "\E&l5E"
	! PRINTER PDF [TOP(XX)], "\E&lXXE"
	!
	fn_phe('&a200L')  ! Left Margin: PRINTER NWP [LEFTMARGIN(XXX)], "\E&aXXXL"
	fn_pdf_font
	fn_pdf_fontsize(10) 
	goto poFINIS
	poFAIL: !
		poReturn=min(-1,-err)
	goto poFINIS
	poFINIS: !
	fnpdf_open=poReturn
fnend
def library fnpdf_Close
	if ~setup then let fn_Setup
	close #hPdfOut: error PDFCLOSEERR
	hPdfOut=0
	fnWindowsStart(os_filename$(g_filename_final$))
goto pdfCloseFinit
PDFCLOSEERR: !
if debug then 
	if env$('acsDeveloper')<>'' then pr 'PDFCLOSEERR - debugging logic follows ' : pause
	pr #hDebugLog: 'Error '&str$(err)&' on Close PDF File/during PDF file creation'
	close #hDebugLog:
	debugLogSetup=0
	exe 'sy -M -C "'&os_filename$('acsBrPdf.txt')&'"'
	if debugScriptSetup then
		debugScriptSetup=0
		close #hDebugScript:
		exe 'sy -M -C "'&os_filename$('acsBrPdfScript.txt')&'"'
	end if
end if
goto pdfCloseFinit
pdfCloseFinit: !
fnend
def library fnpdf_newpage
	if ~setup then let fn_Setup
	fn_print('[newpage]')
fnend
def fn_pdf_mmpos(mmX,mmY) ! position cursor on page in millimeters 
	deciposX=fn_mm2decipoints(mmX)
	deciposY=fn_mm2decipoints(mmY)
	fn_debug('fn_pdf_mmpos to '&str$(mmX)&'mm,'&str$(mmY)&'mm ( '&str$(deciposX)&','&str$(deciposY)&' decipoints)')
	fn_pdf_decipos(deciposX,deciposY)
fnend
def fn_mm2decipoints(mmIn)
	if gPdfDecipos  then
		mm2return=mmIn
	else
		! 1 millimeter = 2.834645669291 point (DTP/PostScript)
		mm2return=mmIn*28.346456692 ! round(mmIn*28.346456692,0) ! 2.834645669,0)
	end if
	fn_mm2decipoints=mm2return
fnend
def fn_pdf_decipos(xxx,yyy) ! position cursor on page in decipoints 
	fn_phe('&a'&str$(yyy)&'v'&str$(xxx)&'H') ! notice Vertical (Y) is before Horizontal (X) in this syntax.  Everywhere else it should be (x,y)
fnend
def fn_phe(pheText$*2048; disableDebug) ! [p]rint #[h]PdfOut: [e]sc$&
	if ~disableDebug then let fn_debug('\E'&pheText$)
	fn_print(esc$&pheText$, 1)
fnend
def fn_print(text$*2048; disable_return)
	if text$='[newpage]' then
		pr #hPdfOut: newpage
	else if disable_return then
		pr #hPdfOut: text$;
	else
		pr #hPdfOut: text$
	end if
	if debug then
		if ~debugScriptSetup then
			debugScriptSetup=1
			open #hDebugScript:=fngethandle: 'name=acsBrPdfScript.txt,recl=2048,replace',d,o
		end if
		text$=srep$(text$,esc$,'\E')
		if disable_return then
			pr #hDebugScript: text$;
		else
			pr #hDebugScript: text$
		end if
	end if
fnend
def fn_pdf_push
	fn_phe('&f0S',1) ! Push
	! consider adding logic for things like gFontSize
fnend
def fn_pdf_pop
	fn_phe('&f1S',1) ! Pop
fnend
def library fnpdf_text(text$*256,mmX; mmY)
	if ~setup then let fn_Setup
	fn_pdf_push
	fn_pdf_mmpos(mmX,mmY)
	fn_print(text$)
	fn_pdf_pop
fnend
def library fnpdf_background(background_pdf$*256)
	if ~setup then let fn_Setup
	! background_pdf$ requres a br_filename$
	fn_phe("pdf='1,"&background_pdf$&"'")
fnend
def library fnpdf_lineh(mmX,mmY,length)
	if ~setup then let fn_Setup
	length=length/72*30/19*30
	fn_debug('fnpdf_lineH('&str$(mmX)&'mm,'&str$(mmy)&'mm,'&str$(length)&')')
	fn_pdf_push
	fn_pdf_mmpos(mmX,mmY)
	fn_phe("begin_boxtop"&rpt$(" ",length))
	fn_phe("end_box")
	fn_pdf_pop
fnend
def library fnpdf_linev(mmX,mmY,height) 
	if ~setup then let fn_Setup
	fn_debug('fnpdf_lineV('&str$(mmX)&'mm,'&str$(mmy)&'mm,'&str$(height)&')')
	fn_pdf_push
	!
	plvFontNamePrior$=gFontName$
	plvFontSizePrior=gFontSize
	fn_pdf_font('Courier New')
	fn_pdf_fontsize(1)
	fn_pdf_mmpos(mmX,mmY) ! this seems to be required in situations where no text is printed first.
	fn_print('')  ! this seems to be required in situations where no text is printed first.
	!
	! pr 'height before=';height
	height=height/34*30
	! pr 'height after=';height
	for ynow=mmY to (mmY+height), step 1
		fn_pdf_mmpos(mmX,ynow)
		fn_phe("begin_verticals|")
		fn_phe("end_box")
	nex ynow
	fn_pdf_font(plvFontNamePrior$)
	fn_pdf_fontsize(plvFontSizePrior)
	fn_pdf_pop
fnend
def library fnpdf_font(; fontname$)
	if ~setup then let fn_Setup
	fnpdf_font=fn_pdf_font( fontname$)
fnend
def fn_pdf_font(; fontname$)
	if fontname$='' then fontname$='Courier New'
	if ~setup_font_list then
		setup_font_list=1
		dim fontSupported$(3) ! contains a list of supported font face names
		dim fontProportional(3) ! contains parallel list with 1 if it is a proportional font of 0 if it is a fixed width font
		fontSupported$(1)='Courier New'     : fontProportional(1)=0
		fontSupported$(2)='Lucidia Console' : fontProportional(2)=0
		fontSupported$(3)='Times New Roman' : fontProportional(3)=1
	end if
	tmpWhichFont=srch(mat fontSupported$,fontname$)
	if tmpWhichFont>0 then
		fn_phe("font='"&fontname$&"'")
		if gFontProportional=0 and fontProportional(tmpWhichFont)=1 then ! switching to proportional
			fn_phe('(s1P') ! Printer PCL [PS], "\E(s1P" ! Set Proportional Spacing
		else if gFontProportional=1 and fontProportional(tmpWhichFont)=0 then ! switching to fixed width font
			fn_phe('(s0P') !  Printer PCL [/PS], "\E(s0P" ! Cancel Proportional Spacing
		end if
		fn_pdf_fontsize(gFontSize)
		gFontName$=fontname$
		gFontProportional=fontProportional(tmpWhichFont)
	else ! unsupported font
		pr 
	end if
fnend
def library fnpdf_fontsize(; fontsize)
	if ~setup then let fn_Setup
	fn_debug('fnpdf_fontsize('&str$(fontsize)&')')
	fnpdf_fontsize=fn_pdf_fontsize( fontsize)
fnend
def fn_pdf_fontsize(; fontsize)
	if fontsize=0 then fontsize=10
	if gFontName$='Courier New' or gFontName$='Lucida Console' then ! is a Fixed Width Font
		cpi=24-fontsize
		fn_phe('(s'&str$(cpi)&'H') ! pr 'fixed width font (height) CPI=';cpi 
		fontsizeHeight=fontsize/1.11 ! 1.05 is a little too tall
		fn_phe('(s'&str$(fontsizeHeight)&'V') ! pr 'fixed width font fontsizeHeight=';fontsizeHeight
	else ! 'Times New Roman'  ! is a Proportional Font
		! fontsize=fontsize
		fn_phe('(s'&str$(fontsize)&'V') ! pr 'proportional fontsize=';fontsize ! set only V for proportional fonts
	end if
	gFontSize=fontsize
fnend
def library fnpdf_fontbold(; off_or_on)
	if ~setup then let fn_Setup
	fnpdf_fontbold=fn_pdf_fontbold( off_or_on)
fnend
def fn_pdf_fontbold(; off_or_on)
	if off_or_on then
		fn_phe("(s1B") ! [ITALICS]
	else
		fn_phe("(s0B") ! [/ITALICS]
	end if
	gFontBold=off_or_on
fnend
def library fnpdf_fontitalic(; off_or_on)
	if ~setup then let fn_Setup
	fnpdf_fontitalic=fn_pdf_fontitalic( off_or_on)
fnend
def fn_pdf_fontitalic(; off_or_on)
	if off_or_on then
		fn_phe("(s1S") ! [ITALICS]
	else
		fn_phe("(s0S") ! [/ITALICS]
	end if
	gFontItalic=off_or_on
fnend
def library fnpdf_pic(imgname$*1024,mmX,mmY,imgWidth,imgHeight; style$)
	! PRINTER PDF [PICTURE], "\Epicture=" ! Use like [PICTURE]'2,2,logo.jpg'
	! PRINTER PDF [PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME'"
	! PRINTER PDF [ISO PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME: ISOTROPIC'"
	! PRINTER PDF [TILE PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME: TILE'"
	if ~setup then let fn_Setup
	! background_pdf$ requres a br_filename$
! if style$='' then style$='Isotropic'
	if style$<>'' then style$=': '&style$
	fn_pdf_push
	fn_pdf_mmpos(mmX,mmY) 
	if imgWidth=0 and imgHeight=0 then
		fn_phe("picture='"&imgname$&style$&"'")
	else
		imgWidth=imgWidth/720*28.346456692
		imgHeight=imgHeight/720*28.346456692
		fn_phe("picture='"&str$(imgWidth)&','&str$(imgHeight)&','&imgname$&style$&"'")
	end if
	fn_pdf_pop
fnend
def fn_debug(debugText$*2048)
	if debug then
		if ~debugLogSetup then
			debugLogSetup=1
			open #hDebugLog:=fngethandle: 'name=acsBrPdf.txt,recl=2048,replace',d,o
		end if
		if debugText$(1:2)<>'\E' then 
			pr #hDebugLog: debugText$&'  ';
		else
			pr #hDebugLog: debugText$
		end if
	end if
fnend
