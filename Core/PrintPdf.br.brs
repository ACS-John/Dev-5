06000 def fn_Setup
06020   if ~setup then
06040     setup=1
06060     library 'S:\Core\Library': fngethandle,fnprint_file_name$,fnWindowsStart,fnCopy
06070 !   exec 'Config Option 68 On' ! Prevent stretching printed fonts. (unfortunately it also turns off all font size respect)
06080     esc$=chr$(27)
06100     gFontItalic=0
06120     gFontBold=0
06140     gFontSize=10
06160     gFontName$='Courier New'
06180     debug=1 ! debug=1
06200   end if
06220 fnend
08000 def library fnpdf_open(; pdfOrientation$*9,pdf_sendto_base_name_addition$*128)
08020   pdfDecipos=0 ! no longer passed - no longer needed
08040   if ~setup then let fn_Setup
08080   dim g_filename_work$*1024
08100   dim g_filename_final$*1024
08120   g_filename_work$=env$('Q')&'\tmp_'&session$&'.prn'
08140   g_filename_final$=fnprint_file_name$( pdf_sendto_base_name_addition$,'pdf')
08160   open #hPdfOut:=fngethandle: 'Name=PDF:,PrintFile='&g_filename_work$&',Replace,RecL=5000',Display,Output ioerr poFAIL
08180   poReturn=hPdfOut
08200   gPdfDecipos=pdfDecipos ! if gPdfDecipos then skip translation from mm to gPdfDecipont, expect all input as Decipoints
08220   if lwrc$(pdfOrientation$)='landscape' then
08240     fn_phe('&l1O')
08260   else ! if pdfOrientation$='Portrait' then
08280     fn_phe('&l0O')
08300   end if
08500   ! *****  Top Margin
08520   ! fn_phe('&l0E') 
08540   fn_phe('&l2E') 
08560   ! PRINTER PDF [TOP0], "\E&l0E"  ! 19 mm ! is apx 1.8 (10cpi) lines higher than PrintAce  (landscape test)
08580   ! PRINTER PDF [TOP1], "\E&l1E"
08600   ! PRINTER PDF [TOP2], "\E&l2E"  ! 26 mm  ! almost exactly what PrintAce is (landscape test)
08620   ! PRINTER PDF [TOP3], "\E&l3E"
08640   ! PRINTER PDF [TOP4], "\E&l4E"
08660   ! PRINTER PDF [TOP5], "\E&l5E"
08680   ! PRINTER PDF [TOP(XX)], "\E&lXXE"
08700   !
08710   fn_phe('&a200L')  ! Left Margin: PRINTER NWP [LEFTMARGIN(XXX)], "\E&aXXXL"
08720   fn_pdf_font
08740   fn_pdf_fontsize(10) 
08760   goto poFINIS
09000   poFAIL: !
09020     poReturn=min(-1,-err)
09040   goto poFINIS
09060   poFINIS: !
09080   fnpdf_open=poReturn
09100 fnend
12000 def library fnpdf_Close
12020   if ~setup then let fn_Setup
12040   close #hPdfOut: error PDFCLOSEERR
12060   if g_filename_work$<>'' then 
12080     if g_filename_final$='' the
12100       g_filename_final$=g_filename_work$
12120     else
12140       fnCopy(g_filename_work$,env$('at')&g_filename_final$)
12160     end if
12200   end if 
12220   fnWindowsStart(os_filename$(env$('at')&g_filename_final$))
12240 goto pdfCloseFinit
13000 PDFCLOSEERR: !
13020 if debug then 
13040   pr #hDebugLog: 'Error '&str$(err)&' on Close PDF File/during PDF file creation'
13060   close #hDebugLog:
13080   debugLogSetup=0
13100   exe 'sy -M -C "'&os_filename$('acsBrPdf.txt')&'"'
13120   if debugScriptSetup then
13140     debugScriptSetup=0
13160     close #hDebugScript:
13180     exe 'sy -M -C "'&os_filename$('acsBrPdfScript.txt')&'"'
13200   end if
13220 end if
13240 goto pdfCloseFinit
13260 pdfCloseFinit: !
13280 fnend
14000 def library fnpdf_newpage
14020   if ~setup then let fn_Setup
14040   fn_print('[newpage]')
14160 fnend
16000 def fn_pdf_mmpos(mmX,mmY) ! position cursor on page in millimeters 
16020   deciposX=fn_mm2decipoints(mmX)
16040   deciposY=fn_mm2decipoints(mmY)
16050   fn_debug('fn_pdf_mmpos to '&str$(mmX)&'mm,'&str$(mmY)&'mm ( '&str$(deciposX)&','&str$(deciposY)&' decipoints)')
16060   fn_pdf_decipos(deciposX,deciposY)
16080 fnend
18000 def fn_mm2decipoints(mmIn)
18020   if gPdfDecipos  then
18040     mm2return=mmIn
18060   else
18080     ! 1 millimeter = 2.834645669291 point (DTP/PostScript)
18100     mm2return=mmIn*28.346456692 ! round(mmIn*28.346456692,0) ! 2.834645669,0)
18120   end if
18140   fn_mm2decipoints=mm2return
18160 fnend
22000 def fn_pdf_decipos(xxx,yyy) ! position cursor on page in decipoints 
22020   fn_phe('&a'&str$(yyy)&'v'&str$(xxx)&'H') ! notice Vertical (Y) is before Horizontal (X) in this syntax.  Everywhere else it should be (x,y)
22040 fnend
24000 def fn_phe(pheText$*2048; disableDebug) ! [p]rint #[h]PdfOut: [e]sc$&
24120   if ~disableDebug then let fn_debug('\E'&pheText$)
24160   fn_print(esc$&pheText$, 1)
24180 fnend
25000 def fn_print(text$*2048; disable_return)
25020   if text$='[newpage]' then
25040     pr #hPdfOut: newpage
25060   else if disable_return then
25080     pr #hPdfOut: text$;
25100   else
25120     pr #hPdfOut: text$
25140   end if
25160   if debug then
25180     if ~debugScriptSetup then
25200       debugScriptSetup=1
25220       open #hDebugScript:=fngethandle: 'name=acsBrPdfScript.txt,recl=2048,replace',d,o
25240     end if
25260     text$=srep$(text$,esc$,'\E')
25280     if disable_return then
25300       pr #hDebugScript: text$;
25320     else
25340       pr #hDebugScript: text$
25360     end if
25380   end if
25400 fnend
26000 def fn_pdf_push
26640   fn_phe('&f0S',1) ! Push
26660   ! consider adding logic for things like gFontSize
26680 fnend
28000 def fn_pdf_pop
28020   fn_phe('&f1S',1) ! Pop
28040 fnend
38000 def library fnpdf_text(text$*256,mmX; mmY)
38020   if ~setup then let fn_Setup
38040   fn_pdf_push
38120   fn_pdf_mmpos(mmX,mmY)
38140   fn_print(text$)
38160   fn_pdf_pop
38180 fnend
42000 def library fnpdf_background(background_pdf$*256)
42020   if ~setup then let fn_Setup
42040   ! background_pdf$ requres a br_filename$
42060   fn_phe("pdf='1,"&background_pdf$&"'")
42080 fnend
44000 def library fnpdf_lineh(mmX,mmY,length)
44120   if ~setup then let fn_Setup
44122   length=length/72*30/19*30
44130   fn_debug('fnpdf_lineH('&str$(mmX)&'mm,'&str$(mmy)&'mm,'&str$(length)&')')
44140   fn_pdf_push
44160   fn_pdf_mmpos(mmX,mmY)
44180   fn_phe("begin_boxtop"&rpt$(" ",length))
44190   fn_phe("end_box")
44200   fn_pdf_pop
44220 fnend
52000 def library fnpdf_linev(mmX,mmY,height) 
52020   if ~setup then let fn_Setup
52040   fn_debug('fnpdf_lineV('&str$(mmX)&'mm,'&str$(mmy)&'mm,'&str$(height)&')')
52060   fn_pdf_push
52080   !
52100   plvFontNamePrior$=gFontName$
52120   plvFontSizePrior=gFontSize
52140   fn_pdf_font('Courier New')
52160   fn_pdf_fontsize(1)
52180   fn_pdf_mmpos(mmX,mmY) ! this seems to be required in situations where no text is printed first.
52200   fn_print('')  ! this seems to be required in situations where no text is printed first.
52220   !
52240   ! pr 'height before=';height
52260   height=height/34*30
52280   ! pr 'height after=';height
52300   for ynow=mmY to (mmY+height), step 1
52320     fn_pdf_mmpos(mmX,ynow)
52340     fn_phe("begin_verticals|")
52360     fn_phe("end_box")
52380   nex ynow
52400   fn_pdf_font(plvFontNamePrior$)
52420   fn_pdf_fontsize(plvFontSizePrior)
52440   fn_pdf_pop
52460 fnend
56000 def library fnpdf_font(; fontname$)
56020   if ~setup then let fn_Setup
56040   fnpdf_font=fn_pdf_font( fontname$)
56060 fnend
57000 def fn_pdf_font(; fontname$)
57020   if fontname$='' then fontname$='Courier New'
57040   if ~setup_font_list then
57060     setup_font_list=1
57080     dim fontSupported$(3) ! contains a list of supported font face names
57100     dim fontProportional(3) ! contains parallel list with 1 if it is a proportional font of 0 if it is a fixed width font
57120     fontSupported$(1)='Courier New'     : fontProportional(1)=0
57140     fontSupported$(2)='Lucidia Console' : fontProportional(2)=0
57160     fontSupported$(3)='Times New Roman' : fontProportional(3)=1
57180   end if
57200   tmpWhichFont=srch(mat fontSupported$,fontname$)
57220   if tmpWhichFont>0 then
57240     fn_phe("font='"&fontname$&"'")
57260     if gFontProportional=0 and fontProportional(tmpWhichFont)=1 then ! switching to proportional
57280       fn_phe('(s1P') ! Printer PCL [PS], "\E(s1P" ! Set Proportional Spacing
57300     else if gFontProportional=1 and fontProportional(tmpWhichFont)=0 then ! switching to fixed width font
57320       fn_phe('(s0P') !  Printer PCL [/PS], "\E(s0P" ! Cancel Proportional Spacing
57340     end if
57360     fn_pdf_fontsize(gFontSize)
57380     gFontName$=fontname$
57400     gFontProportional=fontProportional(tmpWhichFont)
57420   else ! unsupported font
57440     pr 
57460   end if
57480 fnend
58000 def library fnpdf_fontsize(; fontsize)
58040   if ~setup then let fn_Setup
58050   fn_debug('fnpdf_fontsize('&str$(fontsize)&')')
58060   fnpdf_fontsize=fn_pdf_fontsize( fontsize)
58080 fnend
59000 def fn_pdf_fontsize(; fontsize)
59020   if fontsize=0 then fontsize=10
59060   if gFontName$='Courier New' or gFontName$='Lucida Console' then ! is a Fixed Width Font
59080     cpi=24-fontsize
59120     fn_phe('(s'&str$(cpi)&'H') ! pr 'fixed width font (height) CPI=';cpi 
59130     fontsizeHeight=fontsize/1.11 ! 1.05 is a little too tall
59140     fn_phe('(s'&str$(fontsizeHeight)&'V') ! pr 'fixed width font fontsizeHeight=';fontsizeHeight
59160   else ! 'Times New Roman'  ! is a Proportional Font
59170     ! fontsize=fontsize
59180     fn_phe('(s'&str$(fontsize)&'V') ! pr 'proportional fontsize=';fontsize ! set only V for proportional fonts
59200   end if
59300   gFontSize=fontsize
59320 fnend
60200 def library fnpdf_fontbold(; off_or_on)
60220   if ~setup then let fn_Setup
60240   fnpdf_fontbold=fn_pdf_fontbold( off_or_on)
60260 fnend
60280 def fn_pdf_fontbold(; off_or_on)
60300   if off_or_on then
60320     fn_phe("(s1B") ! [ITALICS]
60340   else
60360     fn_phe("(s0B") ! [/ITALICS]
60380   end if
60400   gFontBold=off_or_on
60420 fnend
62440 def library fnpdf_fontitalic(; off_or_on)
62460   if ~setup then let fn_Setup
62480   fnpdf_fontitalic=fn_pdf_fontitalic( off_or_on)
62500 fnend
62520 def fn_pdf_fontitalic(; off_or_on)
62540   if off_or_on then
62560     fn_phe("(s1S") ! [ITALICS]
62580   else
62600     fn_phe("(s0S") ! [/ITALICS]
62620   end if
62640   gFontItalic=off_or_on
62660 fnend
64000 def library fnpdf_pic(imgname$*1024,mmX,mmY,imgWidth,imgHeight; style$)
64020   ! PRINTER PDF [PICTURE], "\Epicture=" ! Use like [PICTURE]'2,2,logo.jpg'
64040   ! PRINTER PDF [PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME'"
64060   ! PRINTER PDF [ISO PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME: ISOTROPIC'"
64080   ! PRINTER PDF [TILE PIC(XX,YY,IMGNAME)], "\Epicture='XX,YY,IMGNAME: TILE'"
64100   if ~setup then let fn_Setup
64120   ! background_pdf$ requres a br_filename$
64140 ! if style$='' then style$='Isotropic'
64160   if style$<>'' then style$=': '&style$
64180   fn_pdf_push
64200   fn_pdf_mmpos(mmX,mmY) 
64220   if imgWidth=0 and imgHeight=0 then
64240     fn_phe("picture='"&imgname$&style$&"'")
64260   else
64280     imgWidth=imgWidth/720*28.346456692
64300     imgHeight=imgHeight/720*28.346456692
64320     fn_phe("picture='"&str$(imgWidth)&','&str$(imgHeight)&','&imgname$&style$&"'")
64340   end if
64360   fn_pdf_pop
64380 fnend
94000 def fn_debug(debugText$*2048)
94020   if debug then
94040     if ~debugLogSetup then
94060       debugLogSetup=1
94080       open #hDebugLog:=fngethandle: 'name=acsBrPdf.txt,recl=2048,replace',d,o
94100     end if
94120     if debugText$(1:2)<>'\E' then 
94140       pr #hDebugLog: debugText$&'  ';
94160     else
94180       pr #hDebugLog: debugText$
94200     end if
94220   end if
94240 fnend
