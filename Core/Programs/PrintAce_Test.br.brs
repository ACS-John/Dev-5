00040 ! r: setup stuff
14000   library 'R:\Core\Library': fnerror,fntop,fnxit
14020   library 'R:\Core\Library': fnpa_line,fnpa_txt,fnpa_text,fnpa_finis,fnpa_open,fnpa_barcode,fnpa_elipse,fnpa_font,fnpa_fontsize,fnpa_fontbold,fnbarcode,fnbarcodewide,fnpa_pic
18000 ! fnpa_line(pl_left_pos,pl_top_pos,pl_width,pl_height; pl_box_instead_of_line,h_printace)
18020 ! fnpa_txt(pt_text$*128,pt_x,pt_y; h_printace)
18040 ! fnpa_text(h_printace,pt_text$*128,pt_x,pt_y) ! older - demands handle as first parameter - try fnpa_txt for cleaner code
18060 ! fnpa_finis(; h_printace)
18080 ! fnpa_open(; h_printace, pa_orientation$,pa_o_filename$*256)
18120 ! fnpa_elipse(pe_a,pe_b,pe_c,pe_d; h_printace)
18140 ! fnpa_font(fontface$; h_printace)
18160 ! fnpa_fontsize(; size,h_printace)
18180 ! fnpa_newpage(; h_printace)
18200 ! fnbarcodewide(barcode$,rightleft,updown)
18220 ! fnbarcode(barcode$,rightleft,updown)
18240 ! fnlabel (from C:\ACS\Dev-5\Core\Label\fnlabel.br.brs seems to have barcode stuff too)
20000   let fntop(program$,'PrintAce Test')
22000   on error goto ERTN
22020 ! /r
22040   let fnpa_open
22060   gosub VBPRINT
22070 ! pr file$(20) : pause
22080   let fnpa_finis
25000 XIT: ! 
25020   let fnxit
28000 ! <Updateable Region: ERTN>
28020 ERTN: let fnerror(cap$,err,line,act$,"xit")
28040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
28060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
28080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
28100 ERTN_EXEC_ACT: execute act$ : goto ERTN
28120 ! /region
42000 VBPRINT: ! r:
42020 ! _______________________________
42040 ! _________ Font Modifications
42060 ! _______________________________
42080   let fnpa_font("Lucida Console")
42100   let fnpa_txt("This is Font Lucida Console",20)
42120   let fnpa_font ! print #20: 'Call Print.MyFont("Courier New")'
42140   let fnpa_txt("This is Font Courier New",20)
42160   let fnpa_fontbold(1) ! turns bold on
42180   let fnpa_txt("This is Font Courier New BOLD",20)
42190   let fnpa_fontbold ! turns bold back off
42200   let fnpa_fontsize(12)
42220   let fnpa_txt("This is Font Size 12",20)
42240   let fnpa_fontsize(8)
42260   let fnpa_txt("This is Font Size 8",20)
42280   print #20: 'Call Print.AddLine(50,50,50,50,True)'
42300   let fnpa_txt("This is a 50mm Square Box",51,95)
42320   print #20: 'Call Print.AddLine(110,110,30,0)'
42340   let fnpa_txt("This is a 30mm Horizontal Line",105,105)
42360   print #20: 'Call Print.AddLine(210,110,0,30)'
42380   let fnpa_txt("This is a 30mm Vertical Line",205,105)
42400 ! __________________________________
42420 ! _________ Easy PostNet Barcodes
42440 ! __________________________________
42460   let fnpa_txt("This is a PostNet Barcode for area code 72602-0758",125,156)
42480   let fnpa_barcode(5,6,"726020758")
42490   let fnbarcodewide('032487897123',125,200)
42492   let fnbarcode('726020758',125,220)
42500 ! _______________________________
42520 ! _________ Pictures in a Snap
42540 ! _______________________________
42560   let fnpa_txt("This is your StartUp.bmp Picture",120,10)
42580   fnpa_pic('r:\startup.bmp',120,20)
42600 ! _______________________________________________
42620 ! _________ Circles and Elipses make us Happy!
42640 ! _______________________________________________
42660   print #20: 'Call Print.AddCircle(220,55,45)'
42680   print #20: 'Call Print.AddCircle(200,30,10)'
42700   print #20: 'Call Print.AddCircle(240,30,10)'
42720   print #20: 'Call Print.AddElipse(220,78,15,0.2)'
42740   print #20: 'Call Print.AddElipse(220,50,15,5)'
42760 ! ______________________________________________________________________
42780 ! ______________________
42800 ! _________ Pie Chart
42820 ! ______________________
42840 ! X=Random percentage, Y=Remainder, Z=Max(X,Y)/2
42860 ! ______________________
42880 L690: ! 
42900   let x=int((100-1+1)*rnd+1)
42920   let y=abs(x-100)
42940   let z=int(max(x,y)/2)
42960   if x=z or y=z then goto L690
42980   if (min(x,y,z))=0 then goto L690
43000   print #20: 'Call Print.DrawPieSection(0,'&str$(min(x,y,z))&',50,156,40,0,2)'
43020   if x<>max(x,y,z) and x<>min(x,y,z) then 
43040     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(x)&',50,156,40,0,3)'
43060     print #20: 'Call Print.DrawPieSection('&str$(x)&','&str$(max(x,y,z))&',50,156,40,0,6)'
43080   end if 
43100   if y<>max(x,y,z) and y<>min(x,y,z) then 
43120     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(y)&',50,156,40,0,3)'
43140     print #20: 'Call Print.DrawPieSection('&str$(y)&','&str$(max(x,y,z))&',50,156,40,0,6)'
43160   end if 
43180   if z<>max(x,y,z) and z<>min(x,y,z) then 
43200     print #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(z)&',50,156,40,0,3)'
43220     print #20: 'Call Print.DrawPieSection('&str$(z)&','&str$(max(x,y,z))&',50,156,40,0,6)'
43240   end if 
43260 ! Print #20: 'Call Print.Print.DrawPieSection('&STR$(Z)&','&STR$(MAX(X,Y,Z))&',50,156,40, ,2)'Print "MaxBound="&STR$(MAX(X,Y,Z))
43280 ! ______________________________________________________________________
43300   return  ! /r
