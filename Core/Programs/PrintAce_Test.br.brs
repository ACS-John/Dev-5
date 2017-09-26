08000 library 'S:\Core\Library': fntop,fnxit
08020 library program$: fnPrintAceTest
08040 let fntop(program$,'PrintAce Test')
08060 fnPrintAceTest
08080 fnxit
10020 def library fnPrintAceTest(;format$)
10040 ! r: setup stuff
12000   library 'S:\Core\Library': fnerror
12020   library 'S:\Core\Library': fnpa_line,fnpa_txt,fnpa_text,fnpa_finis,fnpa_open,fnpa_barcode,fnpa_elipse,fnpa_font,fnpa_fontsize,fnpa_fontbold,fnbarcode,fnbarcodewide,fnpa_pic,fnreg_read
14060   ! ** the only fonts currently used in ACS are **
14080   !    Courier New
14100   !    Lucida Console
14120   !    Times New Roman
14140   ! **
14150   enableText=1
14152   enableLine=1
14160   enableBarCodes=0
14162   enableRulerV=1
14164   enableRulerh=1
14180   enableCircles=0
14200   enablePieChart=0
14220   enablePicture=1
14240   enableBox=1
22000   on error goto ERTN
22020 ! /r
22042   fnPa_open('landscape','PrintAce Test ('&format$&')',format$)
32020   if enablePicture then let fnpa_pic('S:\startup.bmp',120,35, 55,55)
32080   if enableText then ! r:
32100     fnpa_font("Times New Roman")
32120     fnpa_txt('forms Format='&env$('FormsFormatCurrent')&' (at 220,20)',220,20)
32140     fnpa_txt("Font Times New Roman size 10 (at 10,20)",10,20)
32160     fnpa_fontsize(12)
32180     fnpa_txt("Font Times New Roman (fontsize 12)(at 10,24)",10,24)
32200     fnpa_fontsize(6)
32220     fnpa_txt("Font Times New Roman (fontsize 6)(at 10,28)",10,28)
32240     fnpa_font ! print #20: 'Call Print.MyFont("Courier New")'
32260     fnpa_fontsize
32280     fnpa_txt("Courier New size 10 (at 10,40)",10,40)
32300     fnpa_fontbold(1) ! turns bold on
32320     fnpa_txt("Courier New BOLD size 10 (at 10,60)",10,60)
32340     fnpa_fontbold ! turns bold back off
32360     fnpa_fontsize(12)
32380     fnpa_txt("This is Font Size 12 (at 10,80)",10,80)
32400     fnpa_fontsize(11)
32420     fnpa_txt("This is Font Size 11 (at 10,90)",10,90)
32440     fnpa_fontsize(8)
32460     fnpa_txt("This is Font Size 8 (at 10,100)",10,100)
32480     fnpa_fontsize(7)
32500     fnpa_txt("This is Font Size 7 (at 10,110)",10,110)
32520     fnpa_fontsize(6)
32540     fnpa_txt("This is Font Size 6 (at 10,120)",10,120)
32560     fnpa_fontsize(5)
32580     fnpa_txt("This is Font Size 5 (at 10,130)",10,130)
32600     fnpa_fontsize(4)
32620     fnpa_txt("This is Font Size 4 (at 10,140)",10,140)
32640   end if ! /r
36000   if enableLine then ! r:
36020     fnpa_line(50,50,50,50,1)
36040     if enableText then let fnpa_txt("50mm Square Box",51,95)
42020     !
42040     lyne=3 : let xmargin=139 : let ymargin=108
42060     fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
42080     !
42100     let fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
42120     let fnpa_line(xmargin+90,ymargin+0,7,0)
42140     let fnpa_line(xmargin+90,ymargin+2.8,7,0)
42160     let fnpa_line(xmargin+90,ymargin+5.6,7,0)
42180     let fnpa_line(xmargin+90,ymargin+8.4,7,0)
42200     let fnpa_line(xmargin+90,ymargin+11.2,7,0)
42220     let fnpa_line(xmargin+90,ymargin+14,7,0)
42240     let fnpa_line(xmargin+90,ymargin+17,7,0)
42260     !
42280     if enableText then let fnpa_txt("30mm Horizontal Line",105,105)
42300     fnpa_line(80,110,30,0)
42320     fnpa_line(80,112,25,0)
42340     fnpa_line(80,114,20,0)
42360     fnpa_line(80,116,15,0)
42380     fnpa_line(80,118,10,0)
42400     fnpa_line(80,120,5,0)
42420     fnpa_line(80,122,4,0)
42440     fnpa_line(80,124,3,0)
42460     fnpa_line(80,126,2,0)
42480     fnpa_line(80,128,1,0)
42680     !
42700     if enableText then let fnpa_txt("30mm Vertical Line",205,105)
42720     fnpa_line(210,160,0,30)
42740     fnpa_line(212,160,0,25)
42760     fnpa_line(214,160,0,20)
42780     fnpa_line(216,160,0,15)
42800     fnpa_line(218,160,0,10)
42820     fnpa_line(220,160,0,5)
42840     fnpa_line(222,160,0,4)
42860     fnpa_line(224,160,0,3)
42880     fnpa_line(226,160,0,2)
42900     fnpa_line(228,160,0,1)
43990   end if ! /r
45000   if enableBarCodes then  ! r: Easy PostNet Barcodes
45020     fnpa_txt("This is a PostNet Barcode for area code 72602-0758",125,156)
45040     fnpa_barcode(5,6,"726020758")
45060     fnbarcodewide('032487897123',125,200)
45080     fnbarcode('726020758',125,220)
45100   end if ! /r
46000   if enableRulerV then let fn_pa_rulerv
46060   if enableRulerH then let fn_pa_rulerh
47000   if enableCircles then ! r: Circles and Elipses make us Happy!
47020     print #20: 'Call Print.AddCircle(220,55,45)'
47040     print #20: 'Call Print.AddCircle(200,30,10)'
47060     print #20: 'Call Print.AddCircle(240,30,10)'
47080     print #20: 'Call Print.AddElipse(220,78,15,0.2)'
47100     print #20: 'Call Print.AddElipse(220,50,15,5)'
47120   end if ! /r
48000   if enablePieChart then gosub PIECHARTTEST
52080   fnpa_finis
55000 XIT: ! 
55020 fnend !  fnxit ! if env$('ACSDeveloper')<>'' then stop else let fnxit
64000 PIECHARTTEST: ! r: Pie Chart
64020 ! ______________________
64040 ! X=Random percentage, Y=Remainder, Z=Max(X,Y)/2
64060 ! ______________________
64080 L690: ! 
64100   let x=int((100-1+1)*rnd+1)
64120   let y=abs(x-100)
64140   let z=int(max(x,y)/2)
64160   if x=z or y=z then goto L690
64180   if (min(x,y,z))=0 then goto L690
64200   print #20: 'Call Print.DrawPieSection(0,'&str$(min(x,y,z))&',50,156,40,0,2)'
64220   if x<>max(x,y,z) and x<>min(x,y,z) then 
64240     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(x)&',50,156,40,0,3)'
64260     print #20: 'Call Print.DrawPieSection('&str$(x)&','&str$(max(x,y,z))&',50,156,40,0,6)'
64280   end if 
64300   if y<>max(x,y,z) and y<>min(x,y,z) then 
64320     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(y)&',50,156,40,0,3)'
64340     print #20: 'Call Print.DrawPieSection('&str$(y)&','&str$(max(x,y,z))&',50,156,40,0,6)'
64360   end if 
64380   if z<>max(x,y,z) and z<>min(x,y,z) then 
64400     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(z)&',50,156,40,0,3)'
64420     print #20: 'Call Print.DrawPieSection('&str$(z)&','&str$(max(x,y,z))&',50,156,40,0,6)'
64440   end if 
64460 ! Print #20: 'Call Print.Print.DrawPieSection('&STR$(Z)&','&STR$(MAX(X,Y,Z))&',50,156,40, ,2)'Print "MaxBound="&STR$(MAX(X,Y,Z))
64480 return ! /r
66000 def fn_pa_rulerh ! --- (along the bottom of the page)
66020   x=210
66040   y=0
66060   for yp=y to y+270
66080     if yp/10=int(yp/10) then 
66100       let prLength=6 
66120     else if yp/5=int(yp/5) then 
66140       let prLength=4 
66160     else 
66180       let prLength=2
66200     end if
66220     fnpa_line(yp,x-prLength,0,prLength)
66240   nex yp
66260 fnend
68000 def fn_pa_rulerv !|  (along the left side of the page)
68020   x=0
68040   y=0
68060   for xp=x to x+194 ! (194 is max for PDF, 207 for PrintAce) (going over max for PDF makes extra pages, but for PrintAce it is ignored)
68080     if xp/10=int(xp/10) then 
68100       let prLength=6 
68120     else if xp/5=int(xp/5) then 
68140       let prLength=4 
68160     else 
68180       let prLength=2
68200     end if
68220     fnpa_line(y,xp,prLength,0)
68240   nex xp
68260 fnend
70000 ! <Updateable Region: ERTN>
70020 ERTN: let fnerror(program$,err,line,act$,"xit")
70040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
70060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
70080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
70100 ERTN_EXEC_ACT: execute act$ : goto ERTN
70120 ! /region
