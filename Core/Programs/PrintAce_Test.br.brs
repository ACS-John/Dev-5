library 'S:\Core\Library': fnTop,fnXit
library program$: fnPrintAceTest
fnTop(program$,'PrintAce Test')
fnPrintAceTest
fnXit
def library fnPrintAceTest(;format$)
! r: setup stuff
  autoLibrary
  ! ** the only fonts currently used in ACS are **
  !    Courier New
  !    Lucida Console
  !    Times New Roman
  ! **
  enableText=1
  enableLine=1
  enableBarCodes=0
  enableRulerV=1
  enableRulerh=1
  enableCircles=0
  enablePieChart=0
  enablePicture=1
  enableBox=1
  on error goto Ertn
! /r
  fnPa_open('landscape','PrintAce Test ('&format$&')',format$)
  if enablePicture then fnpa_pic('S:\startup.bmp',120,35, 55,55)
  if enableText then ! r:
    fnpa_font("Times New Roman")
    fnpa_txt('forms Format='&env$('FormsFormatCurrent')&' (at 220,20)',220,20)
    fnpa_txt("Font Times New Roman size 10 (at 10,20)",10,20)
    fnpa_fontsize(12)
    fnpa_txt("Font Times New Roman (fontsize 12)(at 10,24)",10,24)
    fnpa_fontsize(6)
    fnpa_txt("Font Times New Roman (fontsize 6)(at 10,28)",10,28)
    fnpa_font ! pr #20: 'Call Print.MyFont("Courier New")'
    fnpa_fontsize
    fnpa_txt("Courier New size 10 (at 10,40)",10,40)
    fnpa_fontbold(1) ! turns bold on
    fnpa_txt("Courier New BOLD size 10 (at 10,60)",10,60)
    fnpa_fontbold ! turns bold back off
    fnpa_fontsize(12)
    fnpa_txt("This is Font Size 12 (at 10,80)",10,80)
    fnpa_fontsize(11)
    fnpa_txt("This is Font Size 11 (at 10,90)",10,90)
    fnpa_fontsize(8)
    fnpa_txt("This is Font Size 8 (at 10,100)",10,100)
    fnpa_fontsize(7)
    fnpa_txt("This is Font Size 7 (at 10,110)",10,110)
    fnpa_fontsize(6)
    fnpa_txt("This is Font Size 6 (at 10,120)",10,120)
    fnpa_fontsize(5)
    fnpa_txt("This is Font Size 5 (at 10,130)",10,130)
    fnpa_fontsize(4)
    fnpa_txt("This is Font Size 4 (at 10,140)",10,140)
  end if ! /r
  if enableLine then ! r:
    fnpa_line(50,50,50,50,1)
    if enableText then fnpa_txt("50mm Square Box",51,95)

    lyne=3 : xmargin=139 : ymargin=108
    fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)

    fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
    fnpa_line(xmargin+90,ymargin+0,7,0)
    fnpa_line(xmargin+90,ymargin+2.8,7,0)
    fnpa_line(xmargin+90,ymargin+5.6,7,0)
    fnpa_line(xmargin+90,ymargin+8.4,7,0)
    fnpa_line(xmargin+90,ymargin+11.2,7,0)
    fnpa_line(xmargin+90,ymargin+14,7,0)
    fnpa_line(xmargin+90,ymargin+17,7,0)

    if enableText then fnpa_txt("30mm Horizontal Line",105,105)
    fnpa_line(80,110,30,0)
    fnpa_line(80,112,25,0)
    fnpa_line(80,114,20,0)
    fnpa_line(80,116,15,0)
    fnpa_line(80,118,10,0)
    fnpa_line(80,120,5,0)
    fnpa_line(80,122,4,0)
    fnpa_line(80,124,3,0)
    fnpa_line(80,126,2,0)
    fnpa_line(80,128,1,0)

    if enableText then fnpa_txt("30mm Vertical Line",205,105)
    fnpa_line(210,160,0,30)
    fnpa_line(212,160,0,25)
    fnpa_line(214,160,0,20)
    fnpa_line(216,160,0,15)
    fnpa_line(218,160,0,10)
    fnpa_line(220,160,0,5)
    fnpa_line(222,160,0,4)
    fnpa_line(224,160,0,3)
    fnpa_line(226,160,0,2)
    fnpa_line(228,160,0,1)
  end if ! /r
  if enableBarCodes then  ! r: Easy PostNet Barcodes
    fnpa_txt("This is a PostNet Barcode for area code 72602-0758",125,156)
    fnpa_barcode(5,6,"726020758")
    fnbarcodewide('032487897123',125,200)
    fnbarcode('726020758',125,220)
  end if ! /r
  if enableRulerV then fn_pa_rulerv
  if enableRulerH then fn_pa_rulerh
  if enableCircles then ! r: Circles and Elipses make us Happy!
    pr #20: 'Call Print.AddCircle(220,55,45)'
    pr #20: 'Call Print.AddCircle(200,30,10)'
    pr #20: 'Call Print.AddCircle(240,30,10)'
    pr #20: 'Call Print.AddElipse(220,78,15,0.2)'
    pr #20: 'Call Print.AddElipse(220,50,15,5)'
  end if ! /r
  if enablePieChart then gosub PIECHARTTEST
  fnpa_finis
Xit: !
fnend !  fnXit ! if env$('ACSDeveloper')<>'' then stop else fnXit
PIECHARTTEST: ! r: Pie Chart

! X=Random percentage, Y=Remainder, Z=Max(X,Y)/2

L690: !
  x=int((100-1+1)*rnd+1)
  y=abs(x-100)
  z=int(max(x,y)/2)
  if x=z or y=z then goto L690
  if (min(x,y,z))=0 then goto L690
  pr #20: 'Call Print.DrawPieSection(0,'&str$(min(x,y,z))&',50,156,40,0,2)'
  if x<>max(x,y,z) and x<>min(x,y,z) then
    pr #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(x)&',50,156,40,0,3)'
    pr #20: 'Call Print.DrawPieSection('&str$(x)&','&str$(max(x,y,z))&',50,156,40,0,6)'
  end if
  if y<>max(x,y,z) and y<>min(x,y,z) then
    pr #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(y)&',50,156,40,0,3)'
    pr #20: 'Call Print.DrawPieSection('&str$(y)&','&str$(max(x,y,z))&',50,156,40,0,6)'
  end if
  if z<>max(x,y,z) and z<>min(x,y,z) then
    pr #20: 'Call Print.DrawPieSection('&str$(min(x,y,z))&','&str$(z)&',50,156,40,0,3)'
    pr #20: 'Call Print.DrawPieSection('&str$(z)&','&str$(max(x,y,z))&',50,156,40,0,6)'
  end if
! pr #20: 'Call Print.Print.DrawPieSection('&STR$(Z)&','&STR$(MAX(X,Y,Z))&',50,156,40, ,2)'Print "MaxBound="&STR$(MAX(X,Y,Z))
return ! /r
def fn_pa_rulerh ! --- (along the bottom of the page)
  x=210
  y=0
  for yp=y to y+270
    if yp/10=int(yp/10) then
      prLength=6
    else if yp/5=int(yp/5) then
      prLength=4
    else
      prLength=2
    end if
    fnpa_line(yp,x-prLength,0,prLength)
  nex yp
fnend
def fn_pa_rulerv !|  (along the left side of the page)
  x=0
  y=0
  for xp=x to x+194 ! (194 is max for PDF, 207 for PrintAce) (going over max for PDF makes extra pages, but for PrintAce it is ignored)
    if xp/10=int(xp/10) then
      prLength=6
    else if xp/5=int(xp/5) then
      prLength=4
    else
      prLength=2
    end if
    fnpa_line(y,xp,prLength,0)
  nex xp
fnend
include: ertn